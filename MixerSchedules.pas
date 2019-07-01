// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit MixerSchedules;

interface

uses Common, Daemon, SysUtils;

type
 TMixerSchedulerRecord = record
  ID, Command: String;
  Debugging: Boolean;
 end;

 TMixerScheduler = class(TDaemonThread)
 public
  Queue: packed array of TMixerSchedulerRecord;
  Lock: TRTLCriticalSection;
  constructor Create;
  procedure Put(const ID, Command: String; const Debugging: Boolean);
  procedure Execute; override;
  procedure Run(R: TMixerSchedulerRecord);
  destructor Destroy; override;
 end;

 TMixerSchedule = class
 public
  Position: Double;
  Debugging, Respawn: Boolean;
  Command, ID, Group: String;
  constructor Create(const APosition: Double; const ACommand, AID: String; const ADebugging, ARespawn: Boolean; const AGroup: String);
  constructor Clone(Schedule: TMixerSchedule);
  procedure Resume; virtual;
  destructor Destroy; override;
 end;

const
 TheScheduler                                    : TMixerScheduler                       = nil;

implementation

uses MixerSources, Mixer,
     Executor;

// TMixerScheduler

constructor TMixerScheduler.Create;
 begin
  inherited Create(True);

  SetLength(Queue, 0);

  InitCriticalSection(Lock, 'TMixerScheduler.Create');

  Resume;
 end;

procedure TMixerScheduler.Put(const ID, Command: String; const Debugging: Boolean);
 var
  R: TMixerSchedulerRecord;
 begin
  DebugLog('TMixerScheduler.Put: in ID "' + ID + '", command "' + Command + '", debugging ' + IntToStr(Integer(Debugging)));

  FillChar(R, SizeOf(R), 0);

  R.ID:=ID;
  R.Command:=Command;
  R.Debugging:=Debugging;

  EnterCriticalSection(Lock, 'TMixerScheduler.Put');

  SetLength(Queue, Length(Queue) + 1);
  Queue[Length(Queue) - 1]:=R;

  LeaveCriticalSection(Lock, 'TMixerScheduler.Put');

  DebugLog('TMixerScheduler.Put: out ID "' + ID + '", command "' + Command + '", debugging ' + IntToStr(Integer(Debugging)));
 end;

procedure TMixerScheduler.Execute;
 var
  K: Longint;
  R: TMixerSchedulerRecord;
 begin
  Install('scheduler');

  try
   while not Terminated do
    begin
     EnterCriticalSection(Lock, 'TMixerScheduler.Execute');

     if Length(Queue) > 0 then
      begin
       R:=Queue[0];

       SetLength(Queue[0].ID, 0);
       SetLength(Queue[0].Command, 0);

       for K:=1 to Length(Queue) - 1 do
        Queue[K - 1]:=Queue[K];

       SetLength(Queue, Length(Queue) - 1);

       LeaveCriticalSection(Lock, 'TMixerScheduler.Execute');

       DebugLog('TMixerScheduler.Execute: running ID "' + R.ID + '", command "' + R.Command + '"');

       try
        Run(R);
       except
        on E: Exception do Log('TMixerScheduler.Execute: exception during execution of "' + R.Command + '": ' + E.Message);
       end;

       DebugLog('TMixerScheduler.Execute: done ID "' + R.ID + '", command "' + R.Command + '"');

       SetLength(R.ID, 0);
       SetLength(R.Command, 0);
      end
     else
      begin
       LeaveCriticalSection(Lock, 'TMixerScheduler.Execute');

       Wait(50);
      end;
    end;
  except
   on E: Exception do Log('TMixerScheduler.Execute: global scheduler exception ' + E.Message);
  end;

  Deinstall;
 end;

{
procedure TMixerScheduler.Run(R: TMixerSchedulerRecord);
 begin
  if R.Debugging then
   TBackgroundExecutor.Create(R.Command, R.ID)
  else
   TBackgroundExecutor.Create(R.Command, '');
 end;
}

procedure TMixerScheduler.Run(R: TMixerSchedulerRecord);
 var
  Cmd, rc: String;
  Output: TSocketStream;
 begin
  if R.Debugging then
   Output:=TLogSocketStream.Create(R.ID + ': ')
  else
   Output:=TNullSocketStream.Create;

  Cmd:=R.Command;
  Executor.Execute(Cmd, rc, Output);

  Output.Free;
 end;

destructor TMixerScheduler.Destroy;
 begin
  Terminate;
  WaitFor;

  SetLength(Queue, 0);

  DoneCriticalSection(Lock, 'TMixerScheduler.Destroy');

  inherited Destroy;
 end;

// TMixerSchedule

constructor TMixerSchedule.Create(const APosition: Double; const ACommand, AID: String; const ADebugging, ARespawn: Boolean; const AGroup: String);
 begin
  inherited Create;

  Debugging:=ADebugging;
  Respawn:=ARespawn;
  Position:=APosition;
  Command:=ACommand;
  ID:=AID;
  Group:=AGroup;

  Inc(MixerSchedulesActive);
 end;

constructor TMixerSchedule.Clone(Schedule: TMixerSchedule);
 begin
  inherited Create;

  Debugging:=Schedule.Debugging;
  Respawn:=Schedule.Respawn;
  Position:=Schedule.Position;
  Command:=Schedule.Command;
  ID:=Schedule.ID;
  Group:=Schedule.Group;

  Inc(MixerSchedulesActive);
 end;

procedure TMixerSchedule.Resume;
 var
  Source: TMixerSource;
 begin
  if Respawn then
   try
    Source:=TheMixer.FindSource(ID);

    if Source <> nil then
     begin
      Source.SchedulePending(TMixerSchedule.Clone(Self));

      EnterCriticalSection(GiantCounters, 'TMixerSchedule.Resume');
      Inc(MixerSchedulesCreated);
      LeaveCriticalSection(GiantCounters, 'TMixerSchedule.Resume');
     end;
   except
    on E: Exception do Log('TMixerSchedule.Execute: exception during schedule pending for "' + ID + '": ' + E.Message);
   end;

  try
   TheScheduler.Put(ID, Command, Debugging);
  except
   on E: Exception do Log('TMixerSchedule.Execute: exception during putting command "' + Command + '" to scheduler');
  end;
 end;

destructor TMixerSchedule.Destroy;
 begin
  SetLength(ID, 0);
  SetLength(Command, 0);
  SetLength(Group, 0);

  Dec(MixerSchedulesActive);

  inherited Destroy;
 end;

end.