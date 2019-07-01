// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Mixer;

interface

uses Common, SysUtils, Daemon, TimeFixer, Classes,
     MixerSources, MixerTargets, MixerSetters, MixerFilters, MixerSchedules;

type
 TMixer = class(TDaemonThread)
 private
  Buffers: packed array of TMixerBuffer;
  Buffer: TMixerBuffer;
  procedure Mixdown;
  procedure MixdownSilence;
 protected
  procedure Execute; override;
 public
  Sources, SourcesToFree: packed array of TMixerSource;
  Targets: packed array of TMixerTarget;
  Setters: packed array of TMixerSetter;
  Filters: packed array of TMixerFilter;
  MasterFilters: TMixerFiltersPool;
  Title: String;
  Stalled: Longint;
  constructor Create;
  destructor Destroy; override;
  // sources
  procedure AddSource(Source: TMixerSource);
  function RemoveSource(Source: TMixerSource; const Lock: Boolean = True): Boolean;
  function FindSource(const ID: String; const Lock: Boolean = True): TMixerSource;
  function FindSourceLike(const ID: String; const Lock: Boolean = True): TMixerSource;
  // targets
  procedure AddTarget(Target: TMixerTarget);
  function RemoveTarget(Target: TMixerTarget; const Lock: Boolean = True): Boolean;
  function FindTarget(const ID: String; const Lock: Boolean = True): TMixerTarget;
  // targets
  procedure AddSetter(Setter: TMixerSetter);
  function RemoveSetter(Setter: TMixerSetter; const Lock: Boolean = True): Boolean;
  function FindSetter(const SourceKind: TMixerSetterSourceKind; const ID: String; const Lock: Boolean = True): TMixerSetter;
  function FindSetterGroup(const Group: String; const Lock: Boolean = True): TMixerSetter;
  // filters
  procedure AddFilter(Filter: TMixerFilter);
  function RemoveFilter(Filter: TMixerFilter; const Lock: Boolean = True): Boolean;
  function FindFilter(const ID: String; const Lock: Boolean = True): TMixerFilter;
 end;

const
 TheMixer                              : TMixer                      = nil;

implementation

uses Executor;

constructor TMixer.Create;
 begin
  inherited Create(True);

  SetLength(Sources, 0);
  SetLength(SourcesToFree, 0);
  SetLength(Targets, 0);
  SetLength(Setters, 0);
  SetLength(Buffers, 0);
  SetLength(Filters, 0);

  MasterFilters:=TMixerFiltersPool.Create;

  Title:='';
  Stalled:=0;

  Priority:=tpHighest;

  Resume;
 end;

procedure TMixer.Mixdown;
 var
  K, B, MaxSamples, BuffersCount: Longint;
  L, R: Longint;
  LVolume, RVolume: Double;
 begin
  FillChar(Buffer, SizeOf(Buffer), 0);

  BuffersCount:=Length(Buffers);
  MaxSamples:=0;

  for K:=0 to BuffersCount - 1 do
   if Buffers[K].Samples > MaxSamples then
    MaxSamples:=Buffers[K].Samples;

  Buffer.Samples:=MaxSamples;
  Buffer.Volume:=100;
  Buffer.Panning:=0;

  for K:=0 to MaxSamples - 1 do
   begin
    L:=0;
    R:=0;

    for B:=0 to BuffersCount - 1 do
     if (Buffers[B].Volume = 100.0) and (Buffers[B].Panning = 0.0) then
      begin
       L:=L + Buffers[B].Left[K];
       R:=R + Buffers[B].Right[K];
      end
     else if Buffers[B].Panning = 0.0 then
      begin
       L:=L + Round(Buffers[B].Left[K] / 100 * Buffers[B].Volume);
       R:=R + Round(Buffers[B].Right[K] / 100 * Buffers[B].Volume);
      end
     else
      begin
       if Buffers[B].Panning < 0.0 then LVolume:=100.0 else LVolume:=100 - Buffers[B].Panning;
       if Buffers[B].Panning > 0.0 then RVolume:=100.0 else RVolume:=100 - Abs(Buffers[B].Panning);

       L:=L + Round(Buffers[B].Left[K] / 100 * Buffers[B].Volume / 100 * LVolume);
       R:=R + Round(Buffers[B].Right[K] / 100 * Buffers[B].Volume / 100 * RVolume);
      end;

    if L < -32767 then L:=-32767;
    if R < -32767 then R:=-32767;

    if L > 32767 then L:=32767;
    if R > 32767 then R:=32767;

    Buffer.Left[K]:=L;
    Buffer.Right[K]:=R;
   end;
 end;

procedure TMixer.MixdownSilence;
 begin
  FillChar(Buffer.Left, SizeOf(Buffer.Left), 0);
  FillChar(Buffer.Right, SizeOf(Buffer.Right), 0);
  Buffer.Samples:=1152;
  Buffer.Volume:=100;
  Buffer.Panning:=0;
 end;

procedure TMixer.Execute;
 var
  TempBuffer: TMixerBuffer;
  Underrunned: Boolean;
  Compensation, CompensationLong, CompensationLong2: Double;
  DidSamples, DidSamplesLong, LongTicks: LongInt;
  DidAnchor, DidAnchorLong, DidTime, DidTimeLong, LocalAnchor: TTimeFixer;
  LogSocketStream: TLogSocketStream;
  Now, RealNow, RealNowLong: TTimeFixer;
  K, MixerInternalFineTune: LongInt;
  S, rc: String;
 begin
  Install('mixer');

  DidSamples:=0;
  DidAnchor:=TimeAnchor;

  DidSamplesLong:=0;
  DidAnchorLong:=TimeAnchor;

  MixerInternalFineTune:=0;
  LongTicks:=0;

  LogSocketStream:=TLogSocketStream.Create;

  LocalAnchor:=TimeAnchor;
  SetLocalTimeAnchor(LocalAnchor);
  Now:=LocalAnchor - Uptime;

  while (not Terminated) and (not GlobalStop) do
   begin
    // 1. grab buffers from sources

    EnterCriticalSection(GiantSources, 'TMixer.GrabSources');

    try
     SetLength(Buffers, 0);

     for K:=0 to Length(Sources) - 1 do
      //if Sources[K].GetStatus = msPlay then
       begin
        Underrunned:=False;

        if Sources[K].FetchBuffer(TempBuffer, Underrunned) then
         begin
          SetLength(Buffers, Length(Buffers) + 1);
          Buffers[Length(Buffers) - 1]:=TempBuffer;
         end;

        if Underrunned then
         Inc(MixerUnderruns);
       end;
    except
     on E: Exception do
      Log('TMixer.Execute: GrabSources exception ' + E.Message);
    end;

    LeaveCriticalSection(GiantSources, 'TMixer.GrabSources');

    // 2. mixdown buffers

    try
     if Length(Buffers) = 0 then
      MixdownSilence
     else
      Mixdown;
    except
     on E: Exception do
      Log('TMixer.Execute: Mixdown exception ' + E.Message);
    end;

    // 3. do master filters

    try
     MasterFilters.Perform(Buffer);
    except
     on E: Exception do
      Log('TMixer.Execute: MasterFilters exception ' + E.Message);
    end;

    // 4. write mixed buffers to targets

    EnterCriticalSection(GiantTargets, 'TMixer.SendToTargets');

    try
     for K:=0 to Length(Targets) - 1 do
      Targets[K].Encode(Buffer);
    except
     on E: Exception do
      Log('TMixes.Execute: SendToTargets exception ' + E.Message);
    end;

    LeaveCriticalSection(GiantTargets, 'TMixer.SendToTargets');

    // 5. tick setters

    EnterCriticalSection(GiantSetters, 'TMixer.TickSetters');

    try
     for K:=0 to Length(Setters) - 1 do
      Setters[K].Tick(Now);

     for K:=Length(Setters) - 1 downto 0 do
      if Setters[K].Finished then
       RemoveSetter(Setters[K], False);
    except
     on E: Exception do
      Log('TMixes.Execute: TickSetters exception ' + E.Message);
    end;

    LeaveCriticalSection(GiantSetters, 'TMixer.TickSetters');

    // 5a. calc, calc, calc

    Inc(DidSamples, Buffer.Samples);
    Inc(DidSamplesLong, Buffer.Samples);

    Now:=Now + Round(Buffer.Samples / 44100 * 1000);

    LocalAnchor:=LocalAnchor + Round(Buffer.Samples / 44100 * 1000);

    SetLocalTimeAnchor(LocalAnchor);

    // 6. cleanup

    if TimeAnchor > MixerCleanupAt then
     begin
      MixerCleanupAt:=TimeAnchor + 5000;

      EnterCriticalSection(GiantSources, 'TMixer.CleanupSources');

      if Length(SourcesToFree) > 0 then
       begin
        try
         for K:=0 to Length(SourcesToFree) - 1 do
          SourcesToFree[K].Free;
        except
         on E: Exception do Log('TMixer.Execute: exception while freeing freed sources: ' + E.Message);
        end;

        SetLength(SourcesToFree, 0);
       end;

      LeaveCriticalSection(GiantSources, 'TMixer.CleanupSources');

      // stats

      if MixerDumpStatus then
       TBackgroundExecutor.Create('STATUS', 'status');
     end;

    // 7. sleep

    Inc(LongTicks);

    if DidSamples >= MixerSamplesInterval then
     begin
      DidTime:=TimeAnchor - DidAnchor;
      RealNow:=TimeAnchor - Uptime;

      // general compensation

      Compensation:=((DidSamples / 44100) - (DidTime / 1000)) * 1000 + MixerFineTune - MixerInternalFineTune;

      MixerIdle:=Round(Compensation);

      if MixerIdle > 0 then
       Wait(MixerIdle);

      // local compensation

      RealNow:=TimeAnchor - Uptime;

      MixerInternalFineTune:=Round(RealNow - Now);

      if MixerInternalFineTune < 0 then
       MixerInternalFineTune:=0
      else
       MixerInternalFineTune:=MixerInternalFineTune div 3;

      if MixerDumpIdle then
       begin
        Log('idle ' + IntToStr(MixerIdle) + 'ms, ' + 
            IntToStr(DidSamples) + ' samples mixed (' + IntToStr(Round(DidSamples / 44100 * 1000)) + 'ms) ' +
            'in ' + IntToStr(Round(DidTime)) + 'ms, ' +
            IntToStr(Round(Now)) + '-' + IntToStr(Round(RealNow)) + '=' + IntToStr(Round(Now - RealNow)) + ' internal-real time diff, ' +
            'fine-tuned to ' + IntToStr(MixerInternalFineTune) + 'ms');
       end;

      // global compensation

      if LongTicks > 1000 then
       begin
        DidTimeLong:=TimeAnchor - DidAnchorLong;
        RealNowLong:=TimeAnchor - Uptime;
        LongTicks:=0;

        CompensationLong2:=((DidSamplesLong / 44100) - (DidTimeLong / 1000)) * 1000;

        if CompensationLong2 > 0 then
         Wait(Round(CompensationLong2));

        CompensationLong:=RealNowLong - Now;

        if MixerDumpTimeCorrection then
         Log('TimeCorrection: ' +
             'samples - time = ' + IntToStr(Round(CompensationLong2)) + 'ms, ' +
             'realtime - audinxtime = ' + IntToStr(Round(CompensationLong)) + 'ms');

        DidSamplesLong:=0;
        DidAnchorLong:=TimeAnchor;
       end;

      // finally

      DidSamples:=0;
      DidAnchor:=TimeAnchor;

      if LongTicks mod 300 = 0 then
       begin
        if MixerDumpTimeCorrection then
         Log('TimeCorrection: now = ' + IntToStr(Round(Now)) + ', new now = ' + IntToStr(Round(DidAnchor - Uptime)) + ', diff = ' + IntToStr(Round((DidAnchor - Uptime) - Now)));

        LocalAnchor:=DidAnchor;
        SetLocalTimeAnchor(DidAnchor);
        Now:=DidAnchor - Uptime;
       end;
     end;
   end;

  DebugLog('TMixer.Execute: execute is over');

  S:='STATUS';
  rc:='';

  Executor.Execute(S, rc, LogSocketStream);

  LogSocketStream.Free;

  Deinstall;
 end;

// sources

procedure TMixer.AddSource(Source: TMixerSource);
 begin
  EnterCriticalSection(GiantSources, 'TMixer.AddSource');

  SetLength(Sources, Length(Sources) + 1);
  Sources[Length(Sources) - 1]:=Source;

  LeaveCriticalSection(GiantSources, 'TMixer.AddSource');
 end;

function TMixer.RemoveSource(Source: TMixerSource; const Lock: Boolean = True): Boolean;
 var
  Got, K: Longint;
  Setter: TMixerSetter;
 begin
  if Lock then 
   EnterCriticalSection(GiantSources, 'TMixer.RemoveSource');

  Got:=-1;

  for K:=0 to Length(Sources) - 1 do
   if Sources[K] = Source then
    begin
     Got:=K;
     Break;
    end;

  if Got = -1 then
   begin
    if Lock then 
     LeaveCriticalSection(GiantSources, 'TMixer.RemoveSource');

    Result:=False;
   end
  else
   begin
    // shift array

    for K:=Got + 1 to Length(Sources) - 1 do
     Sources[K - 1]:=Sources[K];

    SetLength(Sources, Length(Sources) - 1);

    // terminate

    Source.Terminate;

    // unlock everything

    if Lock then
     LeaveCriticalSection(GiantSources, 'TMixer.RemoveSource');

    // kill attached setters

    EnterCriticalSection(GiantSetters, 'TMixer.RemoveSource2a');

    repeat
     Setter:=FindSetter(sskSource, Source.ID, False);

     if Setter <> nil then
      begin
       RemoveSetter(Setter, False);

       Log('removing setter of ' + Setter.ID);
      end;
    until Setter = nil;

    LeaveCriticalSection(GiantSetters, 'TMixer.RemoveSource2a');

    // kill source

    if Lock then 
     EnterCriticalSection(GiantSources, 'TMixer.RemoveSource3');

    SetLength(SourcesToFree, Length(SourcesToFree) + 1);

    SourcesToFree[Length(SourcesToFree) - 1]:=Source;

    MixerCleanupAt:=TimeAnchor + 5000;

    if Lock then
     LeaveCriticalSection(GiantSources, 'TMixer.RemoveSource3');

    Result:=True;
   end;
 end;

function TMixer.FindSource(const ID: String; const Lock: Boolean = True): TMixerSource;
 var
  K: Longint;
 begin
  if Lock then 
   EnterCriticalSection(GiantSources, 'TMixer.FindSource');

  for K:=0 to Length(Sources) - 1 do
   if Sources[K].ID = ID then
    begin
     if Lock then 
      LeaveCriticalSection(GiantSources, 'TMixer.FindSource');

     Exit(Sources[K]);
    end;

  Result:=nil;

  if Lock then 
   LeaveCriticalSection(GiantSources, 'TMixer.FindSource');
 end;

function TMixer.FindSourceLike(const ID: String; const Lock: Boolean = True): TMixerSource;
 var
  K, L: Longint;
 begin
  if Lock then 
   EnterCriticalSection(GiantSources, 'TMixer.FindSourceLike');

  L:=Length(ID);

  for K:=0 to Length(Sources) - 1 do
   if Copy(Sources[K].ID, 1, L) = ID then
    begin
     if Lock then 
      LeaveCriticalSection(GiantSources, 'TMixer.FindSourceLike');

     Exit(Sources[K]);
    end;

  Result:=nil;

  if Lock then 
   LeaveCriticalSection(GiantSources, 'TMixer.FindSourceLike');
 end;

// targets

procedure TMixer.AddTarget(Target: TMixerTarget);
 begin
  EnterCriticalSection(GiantTargets, 'TMixer.AddTarget');

  SetLength(Targets, Length(Targets) + 1);
  Targets[Length(Targets) - 1]:=Target;

  LeaveCriticalSection(GiantTargets, 'TMixer.AddTarget');
 end;

function TMixer.RemoveTarget(Target: TMixerTarget; const Lock: Boolean = True): Boolean;
 var
  Got, K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantTargets, 'TMixer.RemoveTarget');

  Got:=-1;

  for K:=0 to Length(Targets) - 1 do
   if Targets[K] = Target then
    begin
     Got:=K;
     Break;
    end;

  if Got = -1 then
   Result:=False
  else
   begin
    Targets[Got].Free;

    for K:=Got + 1 to Length(Targets) - 1 do
     Targets[K - 1]:=Targets[K];

    SetLength(Targets, Length(Targets) - 1);

    Result:=True;
   end;

  if Lock then
   LeaveCriticalSection(GiantTargets, 'TMixer.RemoveTarget');
 end;

function TMixer.FindTarget(const ID: String; const Lock: Boolean = True): TMixerTarget;
 var
  K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantTargets, 'TMixer.FindTarget');

  for K:=0 to Length(Targets) - 1 do
   if Targets[K].ID = ID then
    begin
     if Lock then
      LeaveCriticalSection(GiantTargets, 'TMixer.FindTarget');

     Exit(Targets[K]);
    end;

  Result:=nil;

  if Lock then
   LeaveCriticalSection(GiantTargets, 'TMixer.FindTarget');
 end;

// setters

procedure TMixer.AddSetter(Setter: TMixerSetter);
 begin
  EnterCriticalSection(GiantSetters, 'TMixer.AddSetter');

  SetLength(Setters, Length(Setters) + 1);
  Setters[Length(Setters) - 1]:=Setter;

  LeaveCriticalSection(GiantSetters, 'TMixer.AddSetter');
 end;

function TMixer.RemoveSetter(Setter: TMixerSetter; const Lock: Boolean = True): Boolean;
 var
  Got, K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantSetters, 'TMixer.RemoveSetter');

  Got:=-1;

  for K:=0 to Length(Setters) - 1 do
   if Setters[K] = Setter then
    begin
     Got:=K;
     Break;
    end;

  if Got = -1 then
   Result:=False
  else
   begin
    Setters[Got].Free;

    for K:=Got + 1 to Length(Setters) - 1 do
     Setters[K - 1]:=Setters[K];

    SetLength(Setters, Length(Setters) - 1);

    Result:=True;
   end;

  if Lock then
   LeaveCriticalSection(GiantSetters, 'TMixer.RemoveSetter');
 end;

function TMixer.FindSetter(const SourceKind: TMixerSetterSourceKind; const ID: String; const Lock: Boolean = True): TMixerSetter;
 var
  K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantSetters, 'TMixer.FindSetter');

  for K:=0 to Length(Setters) - 1 do
   if (Setters[K].SourceKind = SourceKind) and (Setters[K].ID = ID) then
    begin
     if Lock then
      LeaveCriticalSection(GiantSetters, 'TMixer.FindSetter');

     Exit(Setters[K]);
    end;

  Result:=nil;

  if Lock then
   LeaveCriticalSection(GiantSetters, 'TMixer.FindSetter');
 end;

function TMixer.FindSetterGroup(const Group: String; const Lock: Boolean = True): TMixerSetter;
 var
  K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantSetters, 'TMixer.FindSetterGroup');

  for K:=0 to Length(Setters) - 1 do
   if Setters[K].Group = Group then
    begin
     if Lock then
      LeaveCriticalSection(GiantSetters, 'TMixer.FindSetterGroup');

     Exit(Setters[K]);
    end;

  Result:=nil;

  if Lock then
   LeaveCriticalSection(GiantSetters, 'TMixer.FindSetterGroup');
 end;

// filters

procedure TMixer.AddFilter(Filter: TMixerFilter);
 begin
  EnterCriticalSection(GiantFilters, 'TMixer.AddFilter');

  SetLength(Filters, Length(Filters) + 1);
  Filters[Length(Filters) - 1]:=Filter;

  LeaveCriticalSection(GiantFilters, 'TMixer.AddFilter');
 end;

function TMixer.RemoveFilter(Filter: TMixerFilter; const Lock: Boolean = True): Boolean;
 var
  Got, K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantFilters, 'TMixer.RemoveFilter');

  Got:=-1;

  for K:=0 to Length(Filters) - 1 do
   if Filters[K] = Filter then
    begin
     Got:=K;
     Break;
    end;

  if Got = -1 then
   Result:=False
  else
   begin
    Filters[Got].Free;

    for K:=Got + 1 to Length(Filters) - 1 do
     Filters[K - 1]:=Filters[K];

    SetLength(Filters, Length(Filters) - 1);

    Result:=True;
   end;

  if Lock then
   LeaveCriticalSection(GiantFilters, 'TMixer.RemoveFilter');
 end;

function TMixer.FindFilter(const ID: String; const Lock: Boolean = True): TMixerFilter;
 var
  K: Longint;
 begin
  if Lock then
   EnterCriticalSection(GiantFilters, 'TMixer.FindFilter');

  for K:=0 to Length(Filters) - 1 do
   if Filters[K].ID = ID then
    begin
     if Lock then
      LeaveCriticalSection(GiantFilters, 'TMixer.FindFilter');

     Exit(Filters[K]);
    end;

  Result:=nil;

  if Lock then
   LeaveCriticalSection(GiantFilters, 'TMixer.FindFilter');
 end;

destructor TMixer.Destroy;
 var
  K: Longint;
 begin
  DebugLog('TMixer.Execute: execute Destroy in');

  Terminate;
  WaitFor;

  for K:=Length(Filters) - 1 downto 0 do 
   Filters[K].Free;

  for K:=0 to Length(Setters) - 1 do 
   Setters[K].Free;

  for K:=0 to Length(Sources) - 1 do 
   Sources[K].Free;

  for K:=0 to Length(SourcesToFree) - 1 do 
   SourcesToFree[K].Free;

  for K:=0 to Length(Targets) - 1 do 
   Targets[K].Free;

  MasterFilters.Free;

  SetLength(Buffers, 0);
  SetLength(Title, 0);

  DebugLog('TMixer.Execute: execute Destroy done');

  inherited Destroy;
 end;

end.