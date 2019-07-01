// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit MixerSources;

interface

uses Classes, SysUtils, BaseUnix, Sockets,
     Daemon, Common, Tokenizer, TimeFixer, MAD, SZCodeBaseX,
     MixerFilters, MixerSchedules;

const
 PrebufferSize = 100;

type
 TMixerSourceStatus = (msStop, msPlay, msSeeking);

 TMixerSource = class(TDaemonThread)
 private
  Lock, StreamLock: TRTLCriticalSection;
  OnFuckup, OnDone, OnDrop: String;
  Position, Duration, Volume, Panning: Double;
  Status: TMixerSourceStatus;
  Buffers: packed array of TMixerBuffer;
  Started, Finished: Boolean;
  DurationDetect: Boolean;
  Rebooting, Rebooted: Boolean;
  RebootPosition: Boolean;
  RebootPositionValue: Double;
  RebootStatus: Boolean;
  RebootStatusValue: TMixerSourceStatus;
  SeekingTo: Double;
  RecreateSchedules: Boolean;
  SourceStreamOver: Boolean;
  SourceStreamError: Boolean;
  SourceStreamInvalidFormat: Boolean;
  OutputStream: TSocketStream;
  Schedules, SchedulesPending: packed array of TMixerSchedule;
  function Reset: Boolean; virtual; abstract;
  function Remains: Double; virtual;
  procedure DurationDetectorReset; virtual;
 public
  ID, Title: String;
  Filters: TMixerFiltersPool;
  DumbPurposes: Boolean;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure SetVolume(const AVolume: Longint);
  procedure SetPanning(const APanning: Longint);
  function Startup: Boolean; virtual;
  procedure Execute; override; abstract;
  function FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean; virtual; abstract;
  procedure Play; virtual;
  procedure Stop; virtual;
  procedure Rewind; virtual;
  function Seek(APosition: Double): Boolean; virtual;
  function Seekable: Boolean; virtual; abstract;
  procedure Shutdown; virtual;
  function ExplainStatus: String; virtual;
  function HaveParameter(const Parameter: String): Boolean; virtual;
  function GetParameter(const Parameter: String): Double; virtual;
  procedure SetParameter(const Parameter: String; const AValue: Double); virtual;
  procedure Schedule(const APosition: Double; const Command: String; const Debugging, Respawn: Boolean; const Group: String); virtual;
  procedure SchedulePending(Schedule: TMixerSchedule); virtual;
  function FindScheduleGroup(const Group: String; const Lock: Boolean = True): TMixerSchedule; virtual;
  function RemoveSchedule(Schedule: TMixerSchedule; const Lock: Boolean = True): Boolean; virtual;
  procedure AddBuffer(const Buffer: TMixerBuffer);
  function FetchBuffer(var Buffer: TMixerBuffer; var Underrunned: Boolean): Boolean;
  procedure Idle; virtual;
  procedure Reboot; virtual; overload;
  procedure Reboot(const APosition: Double; const AStatus: TMixerSourceStatus); virtual; overload;
  procedure Reboot(const AStatus: TMixerSourceStatus); virtual; overload;
  function GetPosition: Double;
  function GetStatus: TMixerSourceStatus;
  destructor Destroy; override;
 end;

 TMixerSourceMP3 = class(TMixerSource)
 private
  InputBuffer: PByte;
  InputBufferSize: Longint;
  MadDecoder: mad_decoder;
  function DoInput(Stream: p_mad_stream): Integer;
  function DoOutput(Header: p_mad_header; PCM: p_mad_pcm): Integer;
  function DoError(Stream: p_mad_stream; Frame: p_mad_frame): Integer; 
  function DoHeader(Header: p_mad_header): Integer;
  function DoInputCalc(Stream: p_mad_stream): Integer;
  function DoOutputCalc(Header: p_mad_header; PCM: p_mad_pcm): Integer;
  function DoErrorCalc(Stream: p_mad_stream; Frame: p_mad_frame): Integer; 
  function DoHeaderCalc(Header: p_mad_header): Integer;
 public
  function Startup: Boolean; override;
  procedure Execute; override;
  function Seekable: Boolean; override;
  procedure Shutdown; override;
 end;

 TMixerSourceMP3Local = class(TMixerSourceMP3)
 public
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream); 
  constructor CreateFName(const AFName, AID: String; var Command, rc: String; Output: TSocketStream; const APrefetched: Boolean = False);
  function FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean; override;
  destructor Destroy; override;
 private
  Prefetched: Boolean;
  Stream: TFileStream;
  FName: String;
  function Reset: Boolean; override;
 end;

 TMixerSourceMP3Remote = class(TMixerSourceMP3)
 public
  PrefetchTempFName, PrefetchFName: String;
  MirrorFailed: Boolean;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  constructor CreateURL(AURL: String; const AID: String; var Command, rc: String; Output: TSocketStream; const APrefetching: Boolean = False; const APrefetchFName: String = '');
  function FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean; override;
  function Seekable: Boolean; override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 private
  Prefetching: Boolean;
  Stream: TSocketStream;
  Mirror: TStream;
  TotalSize, BytesReaded: Int64;
  CheckingSeekable: Boolean;
  URL, URLHost, URLPort, URLSuffix: String;
  function Reset: Boolean; override;
  procedure DurationDetectorReset; override;
 end;

 TMixerSourceMP3Bind = class(TMixerSourceMP3)
 public
  Host, Port, Password: String;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  function FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean; override;
  procedure Stop; override;
  function Seekable: Boolean; override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 private
  Stream: TSocketStream;
  Socket: Integer;
  function Reset: Boolean; override;
  procedure HereCanBeOnlyOne;
 end;

function ExplainMixerSourceStatus(const Status: TMixerSourceStatus): String;

implementation

uses Executor,
     Mixer,
     Prefetcher;

// TMixerSource

constructor TMixerSource.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  AOnFuckup, AOnDone, AOnDrop, ACommand, S: String;
 begin
  AOnFuckup:='';
  AOnDone:='';
  AOnDrop:='';

  while True do
   begin
    ACommand:=Command;

    if TokenizeToken(Command, 'ON') then
     if TokenizeTokenVar(Command, S) then
      if S = 'FUCKUP' then
       begin
        if not TokenizeValue(Command, AOnFuckup) then
         begin
          Output.Report(400, Command, rc, 'Something wrong, ON FUCKUP "command" expected');
          Fail;
         end;

        ACommand:=Command;
       end
      else if S = 'DONE' then
       begin
        if not TokenizeValue(Command, AOnDone) then
         begin
          Output.Report(400, Command, rc, 'Something wrong, ON DONE "command" expected');
          Fail;
         end;

        ACommand:=Command;
       end
      else if S = 'DROP' then
       begin
        if not TokenizeValue(Command, AOnDrop) then
         begin
          Output.Report(400, Command, rc, 'Something wrong, ON DROP "command" expected');
          Fail;
         end;

        ACommand:=Command;
       end
      else
       begin
        Output.Report(400, Command, rc, 'Something wrong, ON FUCKUP|DONE|DROP expected');
        Fail;
       end
     else
      begin
       Output.Report(400, Command, rc, 'Something wrong, ON FUCKUP|DONE|DROP expected');
       Fail;
      end
    else
     Break;
   end;

  Command:=ACommand;

  inherited Create(True);

  InitCriticalSection(Lock, 'TMixerSource.Create');
  InitCriticalSection(StreamLock, 'TMixerSource.Create');

  ID:=AID;

  Title:='untitled source "' + ID + '"';

  Status:=msStop;

  Position:=0.0;
  Duration:=0.0;

  Volume:=100.0;
  Panning:=0.0;

  Started:=False;
  Finished:=False;

  DurationDetect:=False;

  RebootPosition:=False;
  RebootStatus:=False;

  RecreateSchedules:=False;

  SourceStreamOver:=False;
  SourceStreamError:=False;
  SourceStreamInvalidFormat:=False;

  SeekingTo:=0.0;

  OutputStream:=nil;

  Filters:=TMixerFiltersPool.Create;

  SetLength(Schedules, 0);
  SetLength(SchedulesPending, 0);

  SetLength(Buffers, 0);

  OnFuckup:=AOnFuckup;
  OnDone:=AOnDone;
  OnDrop:=AOnDrop;

  DumbPurposes:=False;

  Install('source:' + ID + ':creating');

  DebugLog('TMixerSource.Create: OnFuckup="' + OnFuckup + '" OnDone="' + OnDone + '" OnDrop="' + OnDrop + '"');
 end;

procedure TMixerSource.SetVolume(const AVolume: Longint);
 begin
  Volume:=AVolume;
 end;

procedure TMixerSource.SetPanning(const APanning: Longint);
 begin
  Panning:=APanning;
 end;

function TMixerSource.Startup: Boolean;
 begin
  Result:=True;
 end;

procedure TMixerSource.Play;
 begin
  while (GetStatus = msSeeking) and (not Terminated) do
   Idle;

  if (not Terminated) then
   begin
    if (not Seekable) and (GetStatus = msStop) then
     begin
      Reset;

      EnterCriticalSection(Lock, 'TMixerSource.Play');

      SetLength(Buffers, 0);
     end
    else
     EnterCriticalSection(Lock, 'TMixerSource.Play');

    DebugLog('TMixerSource.Play: status=' + ExplainMixerSourceStatus(Status) + ', buffers ready=' + IntToStr(Length(Buffers)));

    Status:=msPlay;

    LeaveCriticalSection(Lock, 'TMixerSource.Play');
   end;
 end;

procedure TMixerSource.Stop;
 begin
  while (GetStatus = msSeeking) and (not Terminated) do
   Idle;

  if (not Terminated) then
   begin
    EnterCriticalSection(Lock, 'TMixerSource.Stop');

    Status:=msStop;

    SetLength(Buffers, 0);

    LeaveCriticalSection(Lock, 'TMixerSource.Stop');
   end;
 end;

procedure TMixerSource.Rewind;
 begin
  Seek(0.0);
 end;

function TMixerSource.Seek(APosition: Double): Boolean;
 begin
  DebugLog('TMixerSource.Seek cp1');

  if not Seekable then
   Exit(False);

  DebugLog('TMixerSource.Seek cp2');

  while (GetStatus = msSeeking) and (not Terminated) do
   Idle;

  DebugLog('TMixerSource.Seek cp3');

  if Terminated then
   Exit(False);

  if APosition < 0.0 then
   APosition:=Duration + APosition;

  // prepare seeking

  DebugLog('TMixerSource.Seek cp4');

  EnterCriticalSection(Lock, 'TMixerSource.Seek1');

  Status:=msStop;

  SeekingTo:=APosition;
   
  Reboot(msSeeking);

  LeaveCriticalSection(Lock, 'TMixerSource.Seek1');

  DebugLog('TMixerSource.Seek cp5');

  while (not Rebooted) and (not Terminated) do
   Idle;

  DebugLog('TMixerSource.Seek cp6');

  while (GetStatus = msSeeking) and (not Terminated) and (not GlobalStop) do
   begin
    DebugLog(ExplainMixerSourceStatus(GetStatus) + ExplainStatus);

    Idle;
   end;

  DebugLog('!!! AFTER SEEK = ' + ExplainMixerSourceStatus(Status) + ExplainStatus);

  Result:=True;
 end;

procedure TMixerSource.Shutdown;
 begin
  Finished:=True;
 end;

function TMixerSource.ExplainStatus: String;
 begin
  EnterCriticalSection(Self.Lock, 'TMixerSource.ExplainStatus');

  Result:=', dur=' + FloatToStrF(Duration, ffFixed, 15, 2) + 
          ', rem=' + FloatToStrF(Remains, ffFixed, 15, 2) +
          ', vol=' + FloatToStrF(Volume, ffFixed, 15, 2) + 
          ', pan=' + FloatToStrF(Panning, ffFixed, 15, 2) + 
          ', buf=' + IntToStr(Length(Buffers)) + '/' + IntToStr(PrebufferSize);

  if not Filters.Empty then
   Result:=Result + ', filters=' + Filters.List;

  if SourceStreamInvalidFormat then
   Result:=Result + ', FATAL: INVALID FORMAT';

  LeaveCriticalSection(Self.Lock, 'TMixerSource.ExplainStatus');
 end;

function TMixerSource.Remains: Double;
 begin
  if Duration > 0.0001 then
   Result:=Duration - Position
  else
   REsult:=0.0;
 end;

function TMixerSource.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'VOLUME' then Exit(True)
  else if Parameter = 'PANNING' then Exit(True);

  Exit(False);
 end;

function TMixerSource.GetParameter(const Parameter: String): Double; 
 begin
  if Parameter = 'VOLUME' then Exit(Volume)
  else if Parameter = 'PANNING' then Exit(Panning)
  else Exit(0.0);
 end;

procedure TMixerSource.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'VOLUME' then Volume:=AValue
  else if Parameter = 'PANNING' then Panning:=AValue;
 end;

procedure TMixerSource.Schedule(const APosition: Double; const Command: String; const Debugging, Respawn: Boolean; const Group: String);
 var
  S: TMixerSchedule;
 begin
  EnterCriticalSection(Lock, 'TMixerSource.Schedule');

  try
   S:=TMixerSchedule.Create(APosition, Command, ID, Debugging, Respawn, Group);

   SetLength(Schedules, Length(Schedules) + 1);
   Schedules[Length(Schedules) - 1]:=S;
  except
   on E: Exception do DebugLog('TMixerSource.Schedule: exception ' + E.Message);
  end;

  LeaveCriticalSection(Lock, 'TMixerSource.Schedule');
 end;

procedure TMixerSource.SchedulePending(Schedule: TMixerSchedule);
 begin
  EnterCriticalSection(Lock, 'TMixerSource.SchedulePending');

  SetLength(SchedulesPending, Length(SchedulesPending) + 1);
  SchedulesPending[Length(SchedulesPending) - 1]:=Schedule;

  LeaveCriticalSection(Lock, 'TMixerSource.SchedulePending');
 end;

function TMixerSource.FindScheduleGroup(const Group: String; const Lock: Boolean = True): TMixerSchedule;
 var
  K: Longint;
 begin
  if Lock then
   EnterCriticalSection(Self.Lock, 'TMixerSource.RemoveSchedule');

  Result:=nil;

  for K:=0 to Length(Schedules) - 1 do
   if Schedules[K].Group = Group then
    begin
     Result:=Schedules[K];
     Break;
    end;

  if Lock then
   LeaveCriticalSection(Self.Lock, 'TMixerSource.RemoveSchedule');
 end;

function TMixerSource.RemoveSchedule(Schedule: TMixerSchedule; const Lock: Boolean = True): Boolean;
 var
  Got, K: Longint;
 begin
  if Lock then
   EnterCriticalSection(Self.Lock, 'TMixerSource.RemoveSchedule');

  Got:=-1;

  for K:=0 to Length(Schedules) - 1 do
   if Schedules[K] = Schedule then
    begin
     Got:=K;
     Break;
    end;

  if Got = -1 then
   Result:=False
  else
   begin
    for K:=Got + 1 to Length(Schedules) - 1 do
     Schedules[K - 1]:=Schedules[K];

    SetLength(Schedules, Length(Schedules) - 1);

    Result:=True;
   end;

  if Lock then
   LeaveCriticalSection(Self.Lock, 'TMixerSource.RemoveSchedule');
 end;

procedure TMixerSource.AddBuffer(const Buffer: TMixerBuffer);
 var
  Size: Integer;
 begin
  while (not Terminated) and (not Rebooting) do
   begin
    EnterCriticalSection(Self.Lock, 'TMixerSource.AddBuffer1');

    Size:=Length(Buffers);

    LeaveCriticalSection(Self.Lock, 'TMixerSource.AddBuffer1');

    if Size >= PrebufferSize then
     begin
      Idle;

      Continue;
     end;

    Break;
   end;

  if Terminated or Rebooting then
   Exit;

  EnterCriticalSection(Self.Lock, 'TMixerSource.AddBuffer2');

  SetLength(Buffers, Length(Buffers) + 1);

  Buffers[Length(Buffers) - 1]:=Buffer;

  LeaveCriticalSection(Self.Lock, 'TMixerSource.AddBuffer2');
 end;

function TMixerSource.FetchBuffer(var Buffer: TMixerBuffer; var Underrunned: Boolean): Boolean;
 var
  K, L, Samples: LongInt;
  Remaining: Double;
 begin
  EnterCriticalSection(Self.Lock, 'TMixerSource.FetchBuffer');

  if Status <> msPlay then
   begin
    LeaveCriticalSection(Self.Lock, 'TMixerSource.FetchBuffer');

    Exit(False);
   end;

  if Length(Buffers) > 0 then
   begin
    // fetch buffer

    Result:=True;

    Move(Buffers[0], Buffer, SizeOf(Buffer));

    if Length(Buffers) > 1 then
     Move(Buffers[1], Buffers[0], SizeOf(Buffer) * (Length(Buffers) - 1));

    SetLength(Buffers, Length(Buffers) - 1);

    // process buffer

    Buffer.Volume:=Volume;
    Buffer.Panning:=Panning;

    Samples:=Buffer.Samples;

    try
     Filters.Perform(Buffer);
    except
     on E: Exception do DebugLog('TMixerSource.FetchBuffer: exception while Filters.Perform ' + E.Message);
    end;

    // going forward

    Position:=Position + (Samples / 44100);

    // perform setters

    try
     Remaining:=Remains - (Samples / 44100);
{
     Log('position=' + FloatToStrF(Position, ffFixed, 15, 2) + ', ' +
         'remaining=' + FloatToStrF(Remaining, ffFixed, 15, 2));
}
     for K:=Length(Schedules) - 1 downto 0 do
      if Schedules[K].Position >= 0.0 then
       if Position >= Schedules[K].Position then
        begin
{
         Log('run ' + IntToStr(K) + ' of ' + IntToStr(Length(Schedules)) + ': ' +
             'position=' + FloatToStrF(Position, ffFixed, 15, 2) + ', ' +
             'schedposition=' + FloatToStrF(Schedules[K].Position, ffFixed, 15, 2));
}
         Schedules[K].Resume;
         Schedules[K].Free;

         for L:=K + 1 to Length(Schedules) - 1 do
          Schedules[L - 1]:=Schedules[L];

         SetLength(Schedules, Length(Schedules) - 1);
        end
       else
      else if Remaining <= Abs(Schedules[K].Position) then
       begin
{
        Log('-run ' + IntToStr(K) + ' of ' + IntToStr(Length(Schedules)) + ': ' +
            'remaining=' + FloatToStrF(Remaining, ffFixed, 15, 2) + ', ' +
            'schedposition=' + FloatToStrF(Schedules[K].Position, ffFixed, 15, 2));
}
        Schedules[K].Resume;
        Schedules[K].Free;

        for L:=K + 1 to Length(Schedules) - 1 do
         Schedules[L - 1]:=Schedules[L];

        SetLength(Schedules, Length(Schedules) - 1);
       end;
    except
     on E: Exception do DebugLog('TMixerSource.Flush: exception ' + E.Message);
    end;

    // game over?
{
    if (Length(Buffers) = 0) and SourceStreamOver then
     begin
      Log('!!!!!!!!!!! GAME OVER');

      Status:=msStop;
      Position:=0.0;
      RecreateSchedules:=True;

      Reboot;
     end;
}
   end
  else
   begin
    Result:=False;
    Underrunned:=True;
   end;

  LeaveCriticalSection(Self.Lock, 'TMixerSource.FetchBuffer');
 end;

procedure TMixerSource.Idle;
 begin
  Wait(100);
 end;

procedure TMixerSource.Reboot;
 begin
  Rebooted:=False;
  Rebooting:=True;
 end;

procedure TMixerSource.Reboot(const APosition: Double; const AStatus: TMixerSourceStatus);
 begin
  Rebooted:=False;

  RebootPosition:=True;
  RebootPositionValue:=APosition;

  RebootStatus:=True;
  RebootStatusValue:=AStatus;

  Rebooting:=True;
 end;

procedure TMixerSource.Reboot(const AStatus: TMixerSourceStatus);
 begin
  Rebooted:=False;

  RebootStatus:=True;
  RebootStatusValue:=AStatus;

  Rebooting:=True;
 end;

function TMixerSource.GetPosition: Double;
 begin
  EnterCriticalSection(Self.Lock, 'TMixerSource.GetPosition');

  Result:=Position;

  LeaveCriticalSection(Self.Lock, 'TMixerSource.GetPosition');
 end;

function TMixerSource.GetStatus: TMixerSourceStatus;
 begin
  EnterCriticalSection(Self.Lock, 'TMixerSource.GetStatus');

  Result:=Status;

  LeaveCriticalSection(Self.Lock, 'TMixerSource.GetStatus');
 end;

procedure TMixerSource.DurationDetectorReset;
 begin
 end;

destructor TMixerSource.Destroy;
 var
  K: Integer;
 begin
  if Started and (not Finished) then
   begin
    DebugLog('TMixerSource.Destroy: started and now finished');

    EnterCriticalSection(Self.Lock, 'TMixerSource.Destroy');

    Terminate;

    LeaveCriticalSection(Self.Lock, 'TMixerSource.Destroy');

    DebugLog('TMixerSource.Destroy: teminate()d');

    while not Finished do
     Idle;

    DebugLog('TMixerSource.Destroy: now finished');

    Idle;
   end;

  DebugLog('TMixerSource.Destroy: freeing cp1');

  try
   Filters.Free;
  except
   on E: Exception do DebugLog('TMixerSource.Destroy1: exception ' + E.Message);
  end;

  DebugLog('TMixerSource.Destroy: freeing cp2');

  try
   for K:=0 to Length(Schedules) - 1 do
    Schedules[K].Free;

   SetLength(Schedules, 0);
  except
   on E: Exception do DebugLog('TMixerSource.Destroy2: exception ' + E.Message);
  end;

  DebugLog('TMixerSource.Destroy: freeing cp3');

  try
   DoneCriticalSection(StreamLock, 'TMixerSource.Destroy');
  except
   on E: Exception do DebugLog('TMixerSource.Destroy4 "' + ID + '": exception ' + E.Message);
  end;

  DebugLog('TMixerSource.Destroy: freeing cp4');

  try
   DoneCriticalSection(Lock, 'TMixerSource.Destroy');
  except
   on E: Exception do DebugLog('TMixerSource.Destroy3 "' + ID + '": exception ' + E.Message);
  end;

  DebugLog('TMixerSource.Destroy: freeing cp5');

  SetLength(Buffers, 0);

  SetLength(ID, 0);
  SetLength(Title, 0);
  SetLength(OnDrop, 0);
  SetLength(OnFuckup, 0);
  SetLength(OnDone, 0);

  DebugLog('TMixerSource.Destroy: freeing cp6');

  inherited Destroy;
 end;

// TMixerSourceMP3

function TMixerSourceMP3_InputWrapper(CData: Pointer; Stream: p_mad_stream): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoInput(Stream));
 end;

function TMixerSourceMP3_InputWrapperCalc(CData: Pointer; Stream: p_mad_stream): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoInputCalc(Stream));
 end;

function TMixerSourceMP3_OutputWrapper(CData: Pointer; Header: p_mad_header; PCM: p_mad_pcm): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoOutput(Header, PCM));
 end;

function TMixerSourceMP3_OutputWrapperCalc(CData: Pointer; Header: p_mad_header; PCM: p_mad_pcm): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoOutputCalc(Header, PCM));
 end;

function TMixerSourceMP3_ErrorWrapper(CData: Pointer; Stream: p_mad_stream; Frame: p_mad_frame): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoError(Stream, Frame));
 end;

function TMixerSourceMP3_ErrorWrapperCalc(CData: Pointer; Stream: p_mad_stream; Frame: p_mad_frame): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoErrorCalc(Stream, Frame));
 end;

function TMixerSourceMP3_HeaderWrapper(CData: Pointer; Header: p_mad_header): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoHeader(Header));
 end;

function TMixerSourceMP3_HeaderWrapperCalc(CData: Pointer; Header: p_mad_header): Integer; cdecl;
 begin
  Exit(TMixerSourceMP3(CData).DoHeaderCalc(Header));
 end;

function TMixerSourceMP3.Startup: Boolean;
 var
  Anchor: TTimeFixer;
  Tries: Longint;
  rc, S: String;
 begin
  InputBufferSize:=512 * 1024;

  GetMem(InputBuffer, InputBufferSize + 128 * 1024);

  Result:=inherited Startup;

  if not Result then
   Exit;

  Tries:=0;

  if Seekable and DurationDetect then
   while (not Terminated) do
    begin
     Anchor:=TimeAnchor;

     Inc(Tries);

     DebugLog('scanning, try ' + IntToStr(Tries));

     if Reset then
      begin
       SourceStreamOver:=False;
       SourceStreamError:=False;

       Duration:=0.0;

       DurationDetectorReset;

       Priority:=tpLower;

       mad_decoder_init(@MadDecoder, Self, TMixerSourceMP3_InputWrapperCalc, TMixerSourceMP3_HeaderWrapperCalc, nil, TMixerSourceMP3_OutputWrapperCalc, TMixerSourceMP3_ErrorWrapperCalc, nil);
       mad_decoder_run(@MadDecoder, MAD_DECODER_MODE_SYNC);
       mad_decoder_finish(@MadDecoder);

       Priority:=tpNormal;

       if SourceStreamError then
        begin
         Log('scanning failed in ' + FormatTimeAnchor(TimeAnchor - Anchor) + 's (failed to read source)');

         Result:=False;
        end
       else if SourceStreamInvalidFormat then
        begin
         Log('scanning failed in ' + FormatTimeAnchor(TimeAnchor - Anchor) + 's (invalid format)');

         Result:=False;
        end
       else
        begin
         Log('scanned in ' + FormatTimeAnchor(TimeAnchor - Anchor) + 's, duration=' + FloatToStrF(Duration, ffFixed, 20, 6));

         if OutputStream <> nil then
          begin
           S:='';
           OutputStream.Report(111, S, rc, FloatToStrF(Duration, ffFixed, 20, 6));
          end;

         Result:=True;

         Break;
        end;
      end
     else
      begin
       Log('scanning failed in ' + FormatTimeAnchor(TimeAnchor - Anchor) + 's (failed to reset)');

       Result:=False;
      end;

     DurationDetect:=False;

     if (not Result) and (Tries >= 3) then
      begin
       Log('failed to scan, source creation failed');

       Break;
      end;
    end;
 end;

procedure TMixerSourceMP3.Execute;
 var
  Running: Boolean;
  K: Longint;
 begin
  Install('source:' + ID);

  DebugLog('TMixerSourceMP3.Execute: before loop, terminated=' + IntToStr(Integer(Terminated)));

  while (not Terminated) do
   begin
    DebugLog('TMixerSourceMP3.Execute: entering loop, resetting, terminated=' + IntToStr(Integer(Terminated)));

    // resetting

    if not Reset then
     begin
      DebugLog('TMixerSourceMP3.Execute: reset failed, terminated=' + IntToStr(Integer(Terminated)));

      Log('reset failed, so good bye dude');

      TheMixer.RemoveSource(Self);

      Terminate;
{
      DebugLog('TMixerSourceMP3.Execute: reset failed, terminated=' + IntToStr(Integer(Terminated)));

      while (not Reset) and (not Terminated) do
       begin
        DebugLog('TMixerSourceMP3.Execute: reset loop, terminated=' + IntToStr(Integer(Terminated)));

        Idle;
       end;
}
     end;

    // terminated?

    if Terminated then
     Break;

    // respawn schedules

    DebugLog('TMixerSourceMP3.Execute: recreate schedules=' + IntToStr(Integer(RecreateSchedules)) + ', terminated=' + IntToStr(Integer(Terminated)));

    if RecreateSchedules then
     begin
      DebugLog('TMixerSourceMP3.Execute: recreating schedules');

      RecreateSchedules:=False;

      EnterCriticalSection(Lock, 'TMixerSourceMP3.Execute1');

      for K:=0 to Length(SchedulesPending) - 1 do
       begin
        SetLength(Schedules, Length(Schedules) + 1);
        Schedules[Length(Schedules) - 1]:=SchedulesPending[K];
       end;

      SetLength(SchedulesPending, 0);

      LeaveCriticalSection(Lock, 'TMixerSourceMP3.Execute1');
     end;

    // init decoder

    DebugLog('TMixerSourceMP3.Execute: initing decoder, terminated=' + IntToStr(Integer(Terminated)));

    EnterCriticalSection(Lock, 'TMixerSourceMP3.Execute1a');

    Position:=0.0;

    SourceStreamOver:=False;
    SourceStreamError:=False;

    if RebootStatus then
     begin
      DebugLog('TMixerSourceMP3.Execute: setting rebooted status=' + ExplainMixerSourceStatus(RebootStatusValue) + ', terminated=' + IntToStr(Integer(Terminated)));

      RebootStatus:=False;
      Status:=RebootStatusValue;
     end
    else
     Status:=msStop;

    SetLength(Buffers, 0);

    LeaveCriticalSection(Lock, 'TMixerSourceMP3.Execute1a');

    DebugLog('TMixerSourceMP3.Execute: decoder init');

    mad_decoder_init(@MadDecoder, Self, TMixerSourceMP3_InputWrapper, TMixerSourceMP3_HeaderWrapper, nil, TMixerSourceMP3_OutputWrapper, TMixerSourceMP3_ErrorWrapper, nil);

    if not Started then Started:=True;
    if not Rebooted then Rebooted:=True;

    DebugLog('TMixerSourceMP3.Execute: decoder run, status=' + ExplainMixerSourceStatus(GetStatus));

    if not Terminated then
     begin
      mad_decoder_run(@MadDecoder, MAD_DECODER_MODE_SYNC);

      if SourceStreamError and (not Rebooting) then
       begin
        DebugLog('!!! got stream error, so now we have to reboot and seek to this point, status=' + ExplainMixerSourceStatus(GetStatus) + ExplainStatus);

        Reboot(Position, GetStatus);
       end;

      // maybe we just playing and have to wait a bit for buffer flush?

      while (not Terminated) and (not Rebooting) do
       begin
        EnterCriticalSection(Self.Lock, 'TMixerSourceMP3.Execute');

        Running:=(Status = msPlay) and (Length(Buffers) > 0);

        LeaveCriticalSection(Self.Lock, 'TMixerSourceMP3.Execute');

        if not Running then
         Break;

        DebugLog('TMixerSourceMP3.Execute: running a bit after decoding, terminated=' + IntToStr(Integer(Terminated)));

        Idle;
       end;
     end;

    DebugLog('TMixerSourceMP3.Execute: decoder finish');

    mad_decoder_finish(@MadDecoder);

    DebugLog('TMixerSourceMP3.Execute: done, rebooting=' + IntToStr(Integer(Rebooting)) + ', srcstatus=' + ExplainMixerSourceStatus(Status) + '/' + IntToStr(Integer(Status)) + ExplainStatus);

    // check for reboot

    if Rebooting then
     begin
      DebugLog('TMixerSourceMP3.Execute: going to reboot, terminated=' + IntToStr(Integer(Terminated)));

      Rebooting:=False;

      Continue;
     end;

    Idle;

    DebugLog('TMixerSourceMP3.Execute: main loop restart, terminated=' + IntToStr(Integer(Terminated)) + ', dumb purposes=' + IntToStr(Integer(DumbPurposes)) + ', source stream over=' + IntToStr(Integer(SourceStreamOver)));

    if (not DumbPurposes) and SourceStreamOver then
     begin
      DebugLog('TMixerSourceMP3.Execute: setting up recreate schedules flag');

      RecreateSchedules:=True;

      DebugLog('TMixerSourceMP3.Execute: OnDone: ' + OnDone);

      if OnDone <> '' then
       TheScheduler.Put(ID, OnDone, False);
     end;
   end;

  DebugLog('TMixerSourceMP3.Execute: while breaked, and now..?');

  // on drop?

  if (OnDrop <> '') and (not GlobalStop) then
   begin
    DebugLog('TMixerSourceMP3.Execute: executing OnDrop "' + OnDrop + '"');

    TheScheduler.Put(ID, OnDrop, False);
   end;

  DebugLog('TMixerSourceMP3.Execute: shutting down, terminated=' + IntToStr(Integer(Terminated)));

  Shutdown;

  Deinstall;
 end;

function TMixerSourceMP3.DoInput(Stream: p_mad_stream): Integer;
 var
  Size: Integer;
  Skip: Longint;
 begin
  if Terminated or Rebooting then
   Exit(MAD_FLOW_STOP);

  Skip:=0;

  try
   if LongWord(Stream^.next_frame) <> 0 then
    begin
     Skip:=LongWord(Stream^.bufend) - LongWord(Stream^.next_frame);
     Move(Stream^.next_frame^, InputBuffer^, Skip);
    end
  except
   on E: Exception do Log('TMixerSourceMP3.DoInput: exception while shifting buffer: ' + E.Message);
  end;

  if not FillInputBuffer(Size, Skip) then
   begin
    DebugLog('TMixerSourceMP3.DoInput: FillInputBuffer returned FALSE');

    SourceStreamOver:=True;
    SourceStreamError:=True;

    Exit(MAD_FLOW_STOP);
   end;

  if Size = 0 then
   begin
    EnterCriticalSection(Self.Lock, 'TMixerSourceMP3.DoInput');

    Size:=Length(Buffers);

    LeaveCriticalSection(Self.Lock, 'TMixerSourceMP3.DoInput');

    if Size = 0 then
     begin
      DebugLog('TMixerSourceMP3.DoInput: SOURCE STREAM OVER: ' + ExplainMixerSourceStatus(Status) + ExplainStatus);

      SourceStreamOver:=True;

      // non-seekable remote stream just banged out, so we have to reboot and play again

      if (not Seekable) and (GetStatus = msPlay) then
       Reboot(msPlay);

      Exit(MAD_FLOW_STOP);
     end;

    Idle;

    Exit(MAD_FLOW_CONTINUE);
   end;

  if Terminated or Rebooting then
   Exit(MAD_FLOW_STOP);

  mad_stream_buffer(Stream, InputBuffer, Size + Skip);

  Exit(MAD_FLOW_CONTINUE);
 end;

function TMixerSourceMP3.DoOutput(Header: p_mad_header; PCM: p_mad_pcm): Integer;
 var
  ThisBuffer: TMixerBuffer;
  AStatus: TMixerSourceStatus;
  K: Longint;
 begin
  if Terminated or Rebooting then
   Exit(MAD_FLOW_STOP);

  if PCM.Length > 0 then
   begin
    if PCM.SampleRate = 44100 then
     begin
      if PCM.Channels = 2 then
       begin
        for K:=0 to PCM.Length - 1 do
         begin
          if PCM.Samples[0][K] >= MAD_F_ONE then 
           PCM.Samples[0][K]:=MAD_F_ONE - 1;

          if PCM.Samples[0][K] < -MAD_F_ONE then 
           PCM.Samples[0][K]:=-MAD_F_ONE;

          ThisBuffer.Left[K]:=PCM.Samples[0][K] shr (MAD_F_FRACBITS + 1 - 16);

          if PCM.Samples[1][K] >= MAD_F_ONE then 
           PCM.Samples[1][K]:=MAD_F_ONE - 1;

          if PCM.Samples[1][K] < -MAD_F_ONE then 
           PCM.Samples[1][K]:=-MAD_F_ONE;
          
          ThisBuffer.Right[K]:=PCM.Samples[1][K] shr (MAD_F_FRACBITS + 1 - 16);
         end;
       end
      else
       begin
        for K:=0 to PCM.Length - 1 do
         begin
          if PCM.Samples[0][K] >= MAD_F_ONE then 
           PCM.Samples[0][K]:=MAD_F_ONE - 1;

          if PCM.Samples[0][K] < -MAD_F_ONE then 
           PCM.Samples[0][K]:=-MAD_F_ONE;

          ThisBuffer.Left[K]:=PCM.Samples[0][K] shr (MAD_F_FRACBITS + 1 - 16);
          ThisBuffer.Right[K]:=ThisBuffer.Left[K];
         end;
       end;

      ThisBuffer.Samples:=PCM.Length;
     end
    else
     begin
      ThisBuffer.Samples:=0;
      SourceStreamInvalidFormat:=True;
     end;
   end
  else
   ThisBuffer.Samples:=0;

  // seeking?

  if GetStatus = msSeeking then
   begin
    //Log('buf=' + IntToStr(ThisBuffer.Samples));

    EnterCriticalSection(Lock, 'TMixerSourceMP3.DoOutput');

    if Position >= SeekingTo then // special case for Seek(0.0)
     begin
      Status:=msStop;

      SetLength(Buffers, 0);
     end
    else
     begin
      Position:=Position + (ThisBuffer.Samples / 44100);

      if Position >= SeekingTo then
       begin
        Status:=msStop;

        SetLength(Buffers, 0);
       end;
     end;

    LeaveCriticalSection(Lock, 'TMixerSourceMP3.DoOutput');
    
    if GetStatus = msSeeking then
     Exit(MAD_FLOW_CONTINUE);
   end;

  // rebooting position?

  if RebootPosition then
   if GetPosition >= RebootPositionValue then
    begin
     EnterCriticalSection(Lock, 'TMixerSourceMP3.DoOutput');

     RebootPosition:=False;

     SetLength(Buffers, 0);

     LeaveCriticalSection(Lock, 'TMixerSourceMP3.DoOutput');
    end
   else
    begin
     EnterCriticalSection(Lock, 'TMixerSourceMP3.DoOutput');

     Position:=Position + (ThisBuffer.Samples / 44100);

     LeaveCriticalSection(Lock, 'TMixerSourceMP3.DoOutput');
     
     Exit(MAD_FLOW_CONTINUE);
    end;

  // have to put to queue on unseekable streams only if playing (no prebuffering on start)

  if not Seekable then
   repeat
    AStatus:=GetStatus;

    if AStatus = msPlay then
     Break;

    if AStatus = msSeeking then
     Exit(MAD_FLOW_CONTINUE);

    if Terminated or Rebooting then
     Exit(MAD_FLOW_STOP);

    Idle;
   until False;

  // putting buffer to queue

  if ThisBuffer.Samples > 0 then
   AddBuffer(ThisBuffer);

  // continue

  Exit(MAD_FLOW_CONTINUE);
 end;

function TMixerSourceMP3.DoError(Stream: p_mad_stream; Frame: p_mad_frame): Integer; 
 begin
  if Terminated or Rebooting then
   Exit(MAD_FLOW_STOP)
  else
   Exit(MAD_FLOW_CONTINUE);
 end;

function TMixerSourceMP3.DoHeader(Header: p_mad_header): Integer;
 begin
  Exit(MAD_FLOW_CONTINUE);
 end;

// calc wrappers

function TMixerSourceMP3.DoInputCalc(Stream: p_mad_stream): Integer;
 var
  Size: Integer;
  Skip: Longint;
 begin
  if Terminated then
   Exit(MAD_FLOW_STOP);

  if LongWord(Stream^.next_frame) <> 0 then
   begin
    Skip:=LongWord(Stream^.bufend) - LongWord(Stream^.next_frame);
    Move(Stream^.next_frame^, InputBuffer^, Skip);
   end
  else
   Skip:=0;

  if not FillInputBuffer(Size, Skip) then
   begin
    DebugLog('TMixerSourceMP3.DoInput: FillInputBuffer returned FALSE');

    SourceStreamOver:=True;
    SourceStreamError:=True;

    Exit(MAD_FLOW_STOP);
   end;

  if Size = 0 then
   begin
    DebugLog('TMixerSourceMP3.DoInput: FillInputBuffer returned SIZE=0');

    SourceStreamOver:=True;

    Exit(MAD_FLOW_STOP);
   end;

  if Terminated then
   Exit(MAD_FLOW_STOP);

  mad_stream_buffer(Stream, InputBuffer, Size + Skip);

  Exit(MAD_FLOW_CONTINUE);
 end;

function TMixerSourceMP3.DoOutputCalc(Header: p_mad_header; PCM: p_mad_pcm): Integer;
 begin
  if Terminated then
   Exit(MAD_FLOW_STOP);

  Duration:=Duration + (PCM.Length / 44100);

  Exit(MAD_FLOW_CONTINUE);
 end;

function TMixerSourceMP3.DoErrorCalc(Stream: p_mad_stream; Frame: p_mad_frame): Integer; 
 begin
  if Terminated then
   Exit(MAD_FLOW_STOP)
  else
   Exit(MAD_FLOW_CONTINUE);
 end;

function TMixerSourceMP3.DoHeaderCalc(Header: p_mad_header): Integer;
 begin
  Exit(MAD_FLOW_CONTINUE);
 end;

procedure TMixerSourceMP3.Shutdown;
 begin
  FreeMem(InputBuffer);

  inherited Shutdown;
 end;

function TMixerSourceMP3.Seekable: Boolean;
 begin
  Exit(True);
 end;

// TMixerSourceMP3Local

constructor TMixerSourceMP3Local.Create(const AID: String; var Command, rc: String; Output: TSocketStream); 
 var
  AFName: String;
 begin
  if not TokenizeValue(Command, AFName) then
   begin
    Output.Report(400, Command, rc, 'Local filename expected');
    Fail;
   end;

  CreateFName(AFName, AID, Command, rc, Output);
 end;

constructor TMixerSourceMP3Local.CreateFName(const AFName, AID: String; var Command, rc: String; Output: TSocketStream; const APrefetched: Boolean = False);
 var
  AStream: TFileStream;
  ADurationDetect: Boolean;
  ADuration: Double;
  Value: String;
 begin
  FName:=AFName;

  if APrefetched then
   Prefetcher.Prefetch.Touch(FName);

  try
   AStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyNone);
  except
   AStream:=nil;
  end;

  if AStream = nil then
   begin
    Output.Report(404, Command, rc, 'Local file not found');
    Fail;
   end;

  if (not TokenizeTokenVar(Command, Value)) or ((Value <> 'SCAN') and (Value <> 'NOSCAN') and (Value <> 'SETUP')) then
   begin
    Output.Report(400, Command, rc, 'SCAN/NOSCAN/SETUP expected');
    Fail;
   end;

  if Value = 'SCAN' then
   ADurationDetect:=True
  else if Value = 'NOSCAN' then
   ADurationDetect:=False
  else if Value = 'SETUP' then
   begin
    if not TokenizeToken(Command, 'DURATION') then
     begin
      Output.Report(400, Command, rc, 'SETUP DURATION "x.xx" expected');
      Fail;
     end;

    if not TokenizeValue(Command, Value) then
     begin
      Output.Report(400, Command, rc, 'DURATION value expected');
      Fail;
     end;

    ADuration:=StrToFloatDef(Value, 0.0);
    ADurationDetect:=False;
   end;

  inherited Create(AID, Command, rc, Output);

  Prefetched:=APrefetched;

  Stream:=AStream;
  OutputStream:=Output;
  Title:=FName;

  DurationDetect:=ADurationDetect;
  Duration:=ADuration;

  if not Startup then
   begin
    Output.Report(400, Command, rc, 'Failed to startup source');
    Shutdown;
    Destroy;
    Fail;
   end;

  Resume;

  while (not Started) and (not Terminated) do
   Idle;
 end;

function TMixerSourceMP3Local.FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean;
 var
  Error: Boolean;
 begin
  if Terminated then
   Exit(False);

  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Local.FillInputBuffer(' + FName + ')');

  Error:=False;

  try
   if Stream = nil then
    Size:=-1
   else
    begin
     try
      Size:=Stream.Read(Pointer(LongWord(InputBuffer) + LongWord(Skip))^, InputBufferSize - Skip);
     except
      on E: Exception do
       begin
        Size:=-1;
        Log('TMixerSourceMP3Local.FillInputBuffer: exception while filling buffer cp1: ' + E.Message);
       end;
     end;

     Error:=Stream.Position <> Stream.Size;
    end;
  except
   on E: Exception do Log('TMixerSourceMP3Local.FillInputBuffer: exception while filling buffer cp2: ' + E.Message);
  end;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Local.FillInputBuffer(' + FName + ')');

  if (Size = 0) and Error then
   Exit(False);

  Exit(Size >= 0);
 end;

function TMixerSourceMP3Local.Reset: Boolean;
 begin
  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Local.Reset');

  try
   Stream.Position:=0;
  except
   on E: Exception do Log('TMixerSourceMP3Local.Reset: exception while resetting stream: ' + E.Message);
  end;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Local.Reset');

  Exit(True);
 end;

destructor TMixerSourceMP3Local.Destroy;
 var
  AStream: TStream;
 begin
  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Local.Destroy');

  AStream:=Stream;
  Stream:=nil;
  AStream.Free;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Local.Destroy');

  if Prefetched then
   Prefetcher.Prefetch.Touch(FName);

  inherited;
 end;

// TMixerSourceMP3Remote

constructor TMixerSourceMP3Remote.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  AURL: String;
 begin
  if not TokenizeValue(Command, AURL) then
   begin
    Output.Report(400, Command, rc, 'URL expected');
    Fail;
   end;

  CreateURL(AURL, AID, Command, rc, Output);
 end;

constructor TMixerSourceMP3Remote.CreateURL(AURL: String; const AID: String; var Command, rc: String; Output: TSocketStream; const APrefetching: Boolean = False; const APrefetchFName: String = '');
 var
  ADurationDetect: Boolean;
  ADuration: Double;
  Value: String;
  Matches: TStringList;
 begin
  Matches:=TStringList.Create;

  if not preg_match('^http\:\/\/(.*?)(\/.*)$', AURL, Matches) then
   begin
    Matches.Free;
    Output.Report(400, Command, rc, 'URL WTF? "http://domain.tld/someurl" expected');
    Fail;
   end;

  URLHost:=Matches[1];
  URLSuffix:=Matches[2];

  Matches.Clear;

  if preg_match('^(.*)\:([0-9]+)$', URLHost, Matches) then
   begin
    URLHost:=Matches[1];
    URLPort:=Matches[2];
   end
  else
   URLPort:='80';

  Matches.Free;

  if (not TokenizeTokenVar(Command, Value)) or ((Value <> 'SCAN') and (Value <> 'NOSCAN') and (Value <> 'SETUP')) then
   begin
    Output.Report(400, Command, rc, 'SCAN/NOSCAN/SETUP expected');
    Fail;
   end;

  if Value = 'SCAN' then
   ADurationDetect:=True
  else if Value = 'NOSCAN' then
   ADurationDetect:=False
  else if Value = 'SETUP' then
   begin
    if not TokenizeToken(Command, 'DURATION') then
     begin
      Output.Report(400, Command, rc, 'SETUP DURATION "x.xx" expected');
      Fail;
     end;

    if not TokenizeValue(Command, Value) then
     begin
      Output.Report(400, Command, rc, 'DURATION value expected');
      Fail;
     end;

    ADuration:=StrToFloatDef(Value, 0.0);
    ADurationDetect:=False;
   end;

  inherited Create(AID, Command, rc, Output);

  Stream:=nil;
  OutputStream:=Output;
  Title:=AURL;
  URL:=AURL;
  TotalSize:=0;
  BytesReaded:=0;

  Mirror:=nil;
  MirrorFailed:=False;

  Prefetching:=APrefetching;
  PrefetchTempFName:='';
  PrefetchFName:=APrefetchFName;

  if Prefetching then
   begin
    DurationDetect:=True;
    Duration:=0.0;

    DumbPurposes:=True;
   end
  else
   begin
    DurationDetect:=ADurationDetect;
    Duration:=ADuration;
   end;

  // checking remote seekable

  CheckingSeekable:=True;

  if not Reset then
   begin
    Output.Report(400, Command, rc, 'Failed to reset source while checking seekable');
    Destroy;
    Fail;
   end;

  CheckingSeekable:=False;

  // maybe we're prefetching here?

  if Prefetching then
   begin
    PrefetchTempFName:=Prefetch.TempFName(APrefetchFName);

    try
     Mirror:=TFileStream.Create(PrefetchTempFName, fmCreate);
    except
     Mirror:=nil;
    end;

    if Mirror = nil then
     Log('failed to create prefetch temp fname "' + PrefetchTempFName + '"');
   end;

  // startup

  if not Startup then
   begin
    Output.Report(400, Command, rc, 'Failed to startup source');
    Shutdown;
    Destroy;
    Fail;
   end;

  // ok, maybe everything is prefetched right now?

  if Prefetching then
   begin
    if Mirror <> nil then
     begin
      Mirror.Free;
      Mirror:=nil;
     end;
   end;

  // fire!

  Resume;

  while (not Started) and (not Terminated) do
   Idle;
 end;

function TMixerSourceMP3Remote.FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean;
 var
  Anchor: TTimeFixer;
 begin
  if Terminated then
   Exit(False);

  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Remote.FillInputBuffer');

  if Stream = nil then
   begin
    DebugLog('TMixerSourceMP3Remote.FillInputBuffer: got nil stream, restart?');

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Remote.FillInputBuffer');

    Exit(False);
   end;

  Anchor:=TimeAnchor;

  if Stream = nil then
   Size:=-1
  else
   try
    Size:=Stream.Read(Pointer(LongWord(InputBuffer) + LongWord(Skip))^, InputBufferSize - Skip);
   except
    on E: Exception do
     begin
      Size:=-1;

      Log('TMixerSourceMP3Remote.FillInputBuffer: exception ' + E.Message + ' ' +
          '(inputbuffer: ' + IntToStr(LongWord(InputBuffer)) + ', ' +
          'skip: ' + IntToStr(LongWord(Skip)) + ', ' + 
          'inputbuffersize: ' + IntToStr(InputBufferSize) + ')');
     end;
   end;

  if (TotalSize > 0) and (Size <= 0) and (BytesReaded < TotalSize) then
   begin
    Size:=-1;

    if Mirror <> nil then
     begin
      Log('mirroring failed (BytesReaded=' + IntToStr(BytesReaded) + ', TotalSize=' + IntToStr(TotalSize) + ')');

      MirrorFailed:=True;
     end;
   end;

  if (Size > 0) and (Mirror <> nil) then
   try
    Mirror.Write(Pointer(LongWord(InputBuffer) + LongWord(Skip))^, Size);
   except
    on E: Exception do
     begin
      Log('TMixerSourceMP3Remote.FillInputBuffer: exception while writing to mirror ' + E.Message);

      Mirror.Free;
      Mirror:=nil;

      MirrorFailed:=True;
     end;
   end;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Remote.FillInputBuffer');

  Anchor:=TimeAnchor - Anchor;

  if Size >= 0 then
   begin
    BytesReaded:=BytesReaded + Size;

    Exit(True);
   end;

  Log('TMixerSourceMP3Remote.FillInputBuffer: size=' + IntToStr(Size));

  Exit(False);
 end;

function TMixerSourceMP3Remote.Reset: Boolean;
 var
  AStream: TStream;
  Bytes: Longint;
  Got: Boolean;
  S: String;
 begin
  Result:=False;

  DebugLog('TMixerSourceMP3Remote.Reset: resetting: host=' + URLHost + ', port=' + URLPort + ', suffix=' + URLSuffix);

  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Remote.Reset');

  try
   DebugLog('TMixerSourceMP3Remote.Reset: resetting cp1');

   try
    if Stream <> nil then
     begin
      AStream:=Stream;
      Stream:=nil;
      AStream.Free;
     end;
   except
    on E: Exception do DebugLog('TMixerSourceMP3Remote.Reset/FreePrevious: exception ' + E.Message);
   end;

   Stream:=nil;

   BytesReaded:=0;

   DebugLog('TMixerSourceMP3Remote.Reset: resetting cp2');

   try
    Stream:=TSocketStream.Connect(URLHost, StrToIntDef(URLPort, 80), 60);
   except
    on E: Exception do 
     begin
      DebugLog('TMixerSourceMP3Remote.Reset/Connect: exception ' + E.Message);
      Stream:=nil;
     end;
   end;

   if Stream = nil then
    DebugLog('TMixerSourceMP3Remote.Reset/Connect AfterException: got nil stream');

   if Stream <> nil then
    begin
     DebugLog('TMixerSourceMP3Remote.Reset: resetting cp3');

     if CheckingSeekable then
      S:='HEAD ' + URLSuffix + ' HTTP/1.0' + #10
     else
      S:='GET ' + URLSuffix + ' HTTP/1.0' + #10;

     S:=S + 'Accept: */*' + #10 +
            'Connection: close' + #10 +
            'Host: ' + URLHost + #10 +
            'User-Agent: ' + AudinxTitle + #10 +
             #10;

     try
      Bytes:=Stream.Write(S[1], Length(S));
     except
      Bytes:=0;
     end;

     if Bytes > 0 then
      begin
       DebugLog('TMixerSourceMP3Remote.Reset: resetting cp4');

       Got:=Stream.Gets(S, True);

       DebugLog('TMixerSourceMP3Remote.Reset: resetting cp4 got=' + IntToStr(Integer(Got)) + ', RC: ' + Trim(S));

       if Got and ((Trim(S) = 'HTTP/1.0 200 OK') or (Trim(S) = 'HTTP/1.1 200 OK')) then
        begin
         Result:=True;

         while Stream.Gets(S, True) do
          begin
           if Trim(S) = '' then
            Break;

           DebugLog('TMixerSourceMP3Remote.Reset: resetting cp4: ' + Trim(S));

           if Copy(S, 1, 15) = 'Content-Length:' then 
            try
             TotalSize:=StrToIntDef(Trim(Copy(S, 16, Length(S))), 0);
            except
             on E: Exception do
              begin
               TotalSize:=0;
               DebugLog('TMixerSourceMP3Remote.Reset: StrToIntDef raised exception on "' + S + '": ' + E.Message);
              end;
            end;
          end;

         DebugLog('TMixerSourceMP3Remote.Reset: resetting cp5');

         if CheckingSeekable then
          begin
           AStream:=Stream;
           Stream:=nil;

           AStream.Free;
          end;
        end
       else if CheckingSeekable and (Trim(S) = 'HTTP/1.0 400 Bad Request') then
        begin
         Result:=True; // icecast on other side doesnt support HEAD

         AStream:=Stream;
         Stream:=nil;

         AStream.Free;
        end
       else
        begin
         Log('"' + URL + '" failure, reply was "' + Trim(S) + '"');

         try
          AStream:=Stream;
          Stream:=nil;

          AStream.Free;
         except
          on E: Exception do DebugLog('TMixerSourceMP3Remote.Reset: exception2 ' + E.Message);
         end;
        end;
      end
     else
      begin
       Log('"' + URL + '" failure');

       AStream:=Stream;
       Stream:=nil;

       AStream.Free;
      end;
    end
   else
    Log('"' + URL + '" connect failure');
  except
   on E: Exception do DebugLog('TMixerSourceMP3Remote.Reset: exception ' + E.Message);
  end;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Remote.Reset');

  DebugLog('TMixerSourceMP3Remote.Reset: resetting cp6');

  if (not CheckingSeekable) and (Stream = nil) and (OnFuckup <> '') then
   begin
    TheScheduler.Put(ID, OnFuckup, False);
    OnFuckup:='';
   end;

  DebugLog('TMixerSourceMP3Remote.Reset: finished, stream=' + IntToStr(Longint(Stream)));

  if CheckingSeekable then
   Exit;

  Exit(Stream <> nil);
 end;

function TMixerSourceMP3Remote.Seekable: Boolean;
 begin
  Result:=TotalSize > 0;
 end;

function TMixerSourceMP3Remote.ExplainStatus: String;
 begin
  Exit(inherited ExplainStatus + ', size=' + IntToStr(TotalSize) + 
                                 ', read=' + IntToStr(BytesReaded));
 end;

procedure TMixerSourceMP3Remote.DurationDetectorReset;
 begin
  inherited;

  if Mirror <> nil then
   begin
    Mirror.Size:=0;
    Mirror.Position:=0;
   end;
 end;

destructor TMixerSourceMP3Remote.Destroy;
 var
  AStream: TStream;
 begin
  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Remote.Destroy');

  if Stream <> nil then
   begin
    AStream:=Stream;
    Stream:=nil;
    AStream.Free;
   end;

  if Mirror <> nil then
   begin
    Mirror.Free;
    Mirror:=nil;
   end;

  if Prefetching and (PrefetchTempFName <> '') then
   Prefetcher.Prefetch.RejectTempFName(PrefetchTempFName);

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Remote.Destroy');

  inherited Destroy;
 end;

// TMixerSourceMP3Bind

constructor TMixerSourceMP3Bind.Create(const AID: String; var Command, rc: String; Output: TSocketStream); 
 var
  AHost, APort, APassword: String;
  ASocket, AddressSize: Integer;
  Address: TInetSockAddr;
  Yes: Integer;
 begin
  // parsing here

  if (not TokenizeToken(Command, 'HOST')) or
     (not TokenizeValue(Command, AHost)) or
     (not TokenizeToken(Command, 'PORT')) or
     (not TokenizeValue(Command, APort)) or
     (not TokenizeToken(Command, 'PASSWORD')) or
     (not TokenizeValue(Command, APassword)) then 
   begin
    Output.Report(400, Command, rc, 'HOST "ip" PORT "port" PASSWORD "jaja" expected');
    Fail;
   end;

  // trying to bind

  AddressSize:=SizeOf(Address);

  SetupSocket(ASocket, Address, AHost, StrToIntDef(APort, 0));

  Yes:=1;
  fpSetSockOpt(ASocket, SOL_SOCKET, SO_REUSEADDR, @Yes, Sizeof(Yes));

  if (fpBind(ASocket, @Address, AddressSize) < 0) or
     (fpListen(ASocket, 1) < 0) then
   begin
    Output.Report(400, Command, rc, 'Failed to bind to ' + AHost + ':' + APort + ' (invalid host address, port or port already in use?)');
    Fail;
   end;

{$IFNDEF WIN32}
  fpfcntl(ASocket, F_SETFL, O_NONBLOCK);
{$ENDIF}

  inherited Create(AID, Command, rc, Output);

  Stream:=nil;
  OutputStream:=Output;
  Title:='bind ' + AHost + ':' + APort + '/' + APassword;

  Host:=AHost;
  Port:=APort;
  Password:=APassword;

  DurationDetect:=False;
  Duration:=0.0;
  Socket:=ASocket;

  if not Startup then
   begin
    Output.Report(400, Command, rc, 'Failed to reset startup source');
    Shutdown;
    Destroy;
    Fail;
   end;

  Resume;

  while (not Started) and (not Terminated) do
   Idle;
 end;

function TMixerSourceMP3Bind.FillInputBuffer(var Size: Integer; const Skip: Integer): Boolean;
 var
  AStream: TStream;
 begin
  HereCanBeOnlyOne;

  if Terminated then
   Exit(False);

  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Bind.FillInputBuffer');

  if Stream = nil then
   begin
    Size:=0;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.FillInputBuffer');

    Exit(True);
   end;

  try
   Size:=Stream.Read(Pointer(LongWord(InputBuffer) + LongWord(Skip))^, InputBufferSize - Skip);
  except
   Size:=-1;
  end;

  if Size <= 0 then
   begin
    if Stream <> nil then
     begin
      AStream:=Stream;
      Stream:=nil;
      AStream.Free;

      Reboot;
     end
    else
     Reboot;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.FillInputBuffer');

    Exit(False);
   end;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.FillInputBuffer');

  Exit(True);
 end;

function TMixerSourceMP3Bind.Reset: Boolean;
 var
  AcceptedSocket: Integer;
  AddressSize: Integer;
  Address: TInetSockAddr;
  Authorization, S: String;
  AStream: TStream;
 begin
  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

  if Stream <> nil then
   begin
    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(True);
   end;

  AddressSize:=SizeOf(Address);
  AcceptedSocket:=fpAccept(Socket, @Address, @AddressSize);

  if AcceptedSocket <= 0 then
   begin
    Status:=msStop;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(False);
   end;

{$IFNDEF WIN32}
  fpfcntl(AcceptedSocket, F_SETFL, 0);
{$ENDIF}

  Stream:=TSocketStream.Accept(AcceptedSocket, 10);

  if Stream = nil then
   begin
    Status:=msStop;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(False);
   end;

  if not Stream.Gets(S, True) then
   begin
    Status:=msStop;

    Stream.Free;
    Stream:=nil;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(False);
   end;

  if not preg_match('^SOURCE (.*) ICE\/1\.0$', S) then
   begin
    Log('Failed to accept incoming connection from ' + GetSocketPeer(AcceptedSocket) + ' (request was "' + S + '")');

    Status:=msStop;

    AStream:=Stream;
    Stream:=nil;
    AStream.Free;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(False);
   end;

  Authorization:='';

  while Stream.Gets(S, True) do
   if (Trim(S) = '') or (Copy(S, 1, 15) = 'ice-audio-info:') then
    Break
   else if Copy(S, 1, 21) = 'Authorization: Basic ' then
    Authorization:=Copy(S, 22, Length(S));

  if SZDecodeBase64(Authorization) = ('source:' + Password) then
   begin
    Log('Got connection from ' + GetSocketPeer(AcceptedSocket));

    Reboot(msPlay);

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(True);
   end
  else
   begin
    Status:=msStop;

    AStream:=Stream;
    Stream:=nil;
    AStream.Free;

    LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Reset');

    Exit(False);
   end;
 end;

procedure TMixerSourceMP3Bind.Stop;
 begin
 end;

function TMixerSourceMP3Bind.Seekable: Boolean;
 begin
  Result:=False;
 end;

function TMixerSourceMP3Bind.ExplainStatus: String;
 begin
  Exit(inherited ExplainStatus);
 end;

procedure TMixerSourceMP3Bind.HereCanBeOnlyOne;
 var
  AddressSize, AcceptedSocket: Integer;
  Address: TInetSockAddr;
 begin
  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Bind.HereCanBeOnlyOne');

  if Stream <> nil then
   begin
    AddressSize:=SizeOf(Address);
    AcceptedSocket:=fpAccept(Socket, @Address, @AddressSize);

    if AcceptedSocket > 0 then
     begin
      Log('Incoming connection from ' + GetSocketPeer(AcceptedSocket) + ' when ' + GetSocketPeer(Stream.Handle) + ' already connected');

      fpShutdown(AcceptedSocket, 2);
      fpClose(AcceptedSocket);
     end;
   end;

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.HereCanBeOnlyOne');
 end;

destructor TMixerSourceMP3Bind.Destroy;
 begin
  EnterCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Destroy');

  if Stream <> nil then
   Stream.Free;

  fpShutdown(Socket, 2);
  fpClose(Socket);

  LeaveCriticalSection(StreamLock, 'TMixerSourceMP3Bind.Destroy');

  inherited Destroy;
 end;

// ExplainMixerSourceStatus

function ExplainMixerSourceStatus(const Status: TMixerSourceStatus): String;
 begin
  if Status = msStop then Exit('stop')
  else if Status = msPlay then Exit('play')
  else if Status = msSeeking then Exit('seeking')
  else Exit('wtf');
 end;

end.