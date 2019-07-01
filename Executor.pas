// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Executor;

interface

uses SysUtils, Classes,
     Common, Daemon, TimeFixer, Tokenizer, MixerSetters, MixerSchedules;

type
 TBackgroundExecutor = class(TDaemonThread)
 private
  Commands: TStringList;
  FreeCommands: Boolean;
  LogPrefix: String;
 public
  constructor Create(const ACommands: TStringList; const AFreeCommands: Boolean; const ALogPrefix: String); overload;
  constructor Create(const ACommand: String; const ALogPrefix: String); overload;
  procedure Execute; override;
  destructor Destroy; override;
 end;

function Execute(var Command, rc: String; Output: TSocketStream; StopOnError: Boolean = False): Boolean;

implementation

uses Mixer,
     MixerSources, MixerTargets, MixerFilters,
     Prefetcher;

// TBackgroundExecutor

constructor TBackgroundExecutor.Create(const ACommands: TStringList; const AFreeCommands: Boolean; const ALogPrefix: String);
 begin
  inherited Create(True);

  Commands:=ACommands;
  FreeCommands:=AFreeCommands;
  LogPrefix:=ALogPrefix;

  FreeOnTerminate:=True;

  Resume;
 end;

constructor TBackgroundExecutor.Create(const ACommand: String; const ALogPrefix: String);
 var
  S: TStringList;
 begin
  S:=TStringList.Create;
  S.Add(ACommand);

  Create(S, True, ALogPrefix);
 end;

procedure TBackgroundExecutor.Execute;
 var
  Stream: TSocketStream;
  S, rc: String;
  K: LongInt;
 begin
  if LogPrefix = '' then
   Stream:=TNullSocketStream.Create
  else
   Stream:=TLogSocketStream.Create(LogPrefix);

  for K:=0 to Commands.Count - 1 do
   begin
    S:=Commands[K];

    rc:='';
    Executor.Execute(S, rc, Stream);
   end;

  Stream.Free;
 end;

destructor TBackgroundExecutor.Destroy;
 begin
  //Log('background executor "' + LogPrefix + '" done');

  LogPrefix:='';

  if FreeCommands then
   Commands.Free;

  inherited Destroy;
 end;

// STATUS

function ExecuteStatus(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Source: TMixerSource;
  Target: TMixerTarget;
  Filter: TMixerFilter;
  K: Longint;
 begin
  // critical sections debug

  DumpCriticalSectionsPendings(Output);

  // sources

  EnterCriticalSection(GiantSources, 'ExecuteStatus/Sources');

  for K:=0 to Length(TheMixer.Sources) - 1 do
   begin
    Source:=TheMixer.Sources[K];

    Output.Report(100, Command, rc,
                         'Source "' + Source.ID + '": ' +
                         'status=' + ExplainMixerSourceStatus(Source.GetStatus) + ', ' +
                         'pos=' + FloatToStrF(Source.GetPosition, ffFixed, 15, 2) + 
                         Source.ExplainStatus);
   end;

  LeaveCriticalSection(GiantSources, 'ExecuteStatus/Sources');

  // targets

  EnterCriticalSection(GiantTargets, 'ExecuteStatus/Targets');

  for K:=0 to Length(TheMixer.Targets) - 1 do
   begin
    Target:=TheMixer.Targets[K];

    Output.Report(101, Command, rc,
                         'Target "' + Target.ID + '": ' +
                         'written=' + IntToStr(Target.BytesWritten) +
                         Target.ExplainStatus);
   end;

  LeaveCriticalSection(GiantTargets, 'ExecuteStatus/Targets');

  // filters

  EnterCriticalSection(GiantFilters, 'ExecuteStatus/Filters');

  for K:=0 to Length(TheMixer.Filters) - 1 do
   begin
    Filter:=TheMixer.Filters[K];

    Output.Report(102, Command, rc,
                         'Filter "' + Filter.ID + '": ' +
                         Filter.ExplainStatus);
   end;

  LeaveCriticalSection(GiantFilters, 'ExecuteStatus/Filters');

  // setters

  EnterCriticalSection(GiantSetters, 'ExecuteStatus/Setters');

  if (Length(TheMixer.Setters) > 0) or (MixerSettersCreated > 0) then
   Output.Report(103, Command, rc,
                        'Setters: ' + IntToStr(Length(TheMixer.Setters)) + ' active, ' +
                                      IntToStr(MixerSettersCreated) + ' total');

  LeaveCriticalSection(GiantSetters, 'ExecuteStatus/Setters');

  // schedules

  EnterCriticalSection(GiantSchedules, 'ExecuteStatus/Schedules');

  if MixerSchedulesCreated > 0 then
   Output.Report(104, Command, rc,
                        'Schedules: ' + IntToStr(MixerSchedulesActive) + ' active, ' +
                                        IntToStr(MixerSchedulesCreated) + ' total');

  LeaveCriticalSection(GiantSchedules, 'ExecuteStatus/Schedules');

  // master filters (no locking need)

  if not TheMixer.MasterFilters.Empty then
   Output.Report(105, Command, rc, 'Master: filters=' + TheMixer.MasterFilters.List);

  // title

  EnterCriticalSection(GiantTitle, 'ExecuteStatus/Title');

  if TheMixer.Title <> '' then
   Output.Report(106, Command, rc, 'Title: ' + TheMixer.Title);

  LeaveCriticalSection(GiantTitle, 'ExecuteStatus/Title');

  // well done

  Output.Report(200, Command, rc,
                         'Uptime: ' + FormatTimeAnchor(LocalTimeAnchor - Uptime) + ', ' +
                         'underruns: ' + IntToStr(MixerUnderruns) + ', ' +
                         'last idle: ' + IntToStr(MixerIdle));

  Exit(True);
 end;

// CREATE SOURCE / CREATE TARGET / CREATE FILTER

function ExecuteCreate(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID, Misc, SavedCommand: String;
  PrefetchURL, PrefetchFName: String;
  Source: TMixerSource;
  Target: TMixerTarget;
  Filter: TMixerFilter;
  Shot: Boolean;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'CREATE SOURCE/SHOT SOURCE/TARGET/FILTER expected'));

  Shot:=Cmd = 'SHOT';

  if Shot then
   if not TokenizeTokenVar(Command, Cmd) then
    Exit(Output.Report(400, Command, rc, 'CREATE SOURCE/TARGET/FILTER expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" expected'));

    if TheMixer.FindSource(ID) <> nil then
     Exit(Output.Report(409, Command, rc, 'CREATE SOURCE failed: source identified by "' + ID + '" already exists'));

    if not TokenizeTokenVar(Command, Cmd) then
     Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" LOCAL/REMOTE/BIND/PREFETCH expected'));

    if Cmd = 'LOCAL' then // CREATE SOURCE "ID" LOCAL "FNAME"
     begin
      Source:=TMixerSourceMP3Local.Create(ID, Command, rc, Output);

      if Source = nil then
       Exit(False);

      //Log('Source "' + ID + '" created (local, "' + Source.Title + '")');

      Output.Report(200, Command, rc, 'Source "' + ID + '" ready');

      if Shot then
       Source.Free
      else
       TheMixer.AddSource(Source);
     end
    else if Cmd = 'REMOTE' then // CREATE SOURCE "ID" REMOTE "URL"
     begin
      Source:=TMixerSourceMP3Remote.Create(ID, Command, rc, Output);

      if Source = nil then
       Exit(False);

      //Log('Source "' + ID + '" created (remote, "' + Source.Title + '")');

      Output.Report(200, Command, rc, 'Source "' + ID + '" ready');

      if Shot then
       Source.Free
      else
       TheMixer.AddSource(Source);
     end
    else if Cmd = 'BIND' then // CREATE SOURCE "ID" BIND
     begin
      Source:=TMixerSourceMP3Bind.Create(ID, Command, rc, Output);

      if Source = nil then
       Exit(False);

      //Log('Source "' + ID + '" created (bind, ' + TMixerSourceMP3Bind(Source).Host + ':' + TMixerSourceMP3Bind(Source).Port + '/' + TMixerSourceMP3Bind(Source).Password + ')');

      Output.Report(200, Command, rc, 'Source "' + ID + '" ready');

      if Shot then
       Source.Free
      else
       TheMixer.AddSource(Source);
     end
    else if Cmd = 'PREFETCH' then // CREATE SOURCE "ID" PREFETCH "URL" TO "FNAME"
     begin
      if not TokenizeTokenVar(Command, Cmd) then
       Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" PREFETCH REMOTE'));

      if Cmd <> 'REMOTE' then
       Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" PREFETCH REMOTE'));

      if (not TokenizeValue(Command, PrefetchURL)) then
       Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" PREFETCH REMOTE "URL" expected'));

      if (not TokenizeTokenVar(Command, Cmd)) then
       Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" PREFETCH REMOTE "URL" TO expected'));

      if Cmd <> 'TO' then
       Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" PREFETCH REMOTE "URL" TO expected'));

      if (not TokenizeValue(Command, PrefetchFName)) then
       Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" PREFETCH REMOTE "URL" TO "FNAME" expected'));

      if Prefetch.Exists(PrefetchFName) then
       begin
        Source:=TMixerSourceMP3Local.CreateFName(Prefetch.FName(PrefetchFName), ID, Command, rc, Output, True);

        Misc:='local, already prefetched';
       end
      else
       begin
        SavedCommand:=Command;

        Source:=TMixerSourceMP3Remote.CreateURL(PrefetchURL, ID, Command, rc, Output, True, PrefetchFName);

        if Source <> nil then
         if not TMixerSourceMP3Remote(Source).MirrorFailed then
          begin
           if Prefetch.CommitTempFName(TMixerSourceMP3Remote(Source).PrefetchTempFName, PrefetchFName) then
            begin
             Misc:='remote, prefetched, respawned to local';

             Source.Free;

             Command:=SavedCommand;

             Source:=TMixerSourceMP3Local.CreateFName(Prefetch.FName(PrefetchFName), ID, Command, rc, Output, True);
            end
           else
            begin
             Source.DumbPurposes:=False;

             Misc:='remote, prefetched finalization failed';

             if TMixerSourceMP3Remote(Source).PrefetchTempFName <> '' then
              Prefetch.RejectTempFName(TMixerSourceMP3Remote(Source).PrefetchTempFName);

             Output.Report(199, Command, rc, 'Prefetch finalization failed');
            end;
          end
         else
          begin
           Source.DumbPurposes:=False;

           Misc:='remote, prefetch failed';

           if TMixerSourceMP3Remote(Source).PrefetchTempFName <> '' then
            Prefetch.RejectTempFName(TMixerSourceMP3Remote(Source).PrefetchTempFName);

           Output.Report(199, Command, rc, 'Prefetch failed');
          end;
       end;

      if Source = nil then
       Exit(False);

      Output.Report(200, Command, rc, 'Source "' + ID + '" ready: ' + Misc);

      if Shot then
       Source.Free
      else
       TheMixer.AddSource(Source);
     end
    else
     Exit(Output.Report(400, Command, rc, 'CREATE SOURCE "ID" LOCAL/REMOTE/BIND/PREFETCH expected, got "' + Cmd + '"'));

    Exit(True);
   end
  else if Cmd = 'TARGET' then
   begin
    if Shot then
     Exit(Output.Report(400, Command, rc, 'SHOT can''t be used in CREATE TARGET'));

    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'CREATE TARGET "ID" expected'));

    if TheMixer.FindTarget(ID) <> nil then
     Exit(Output.Report(409, Command, rc, 'CREATE TARGET failed: target identified by "' + ID + '" already exists'));

    Target:=TMixerTarget.Create(ID, Command, rc, Output);

    if Target = nil then
     Exit(False);

    Output.Report(200, Command, rc, 'Target "' + ID + '" ready');

    TheMixer.AddTarget(Target);

    Exit(True);
   end
  else if Cmd = 'FILTER' then
   begin
    if Shot then
     Exit(Output.Report(400, Command, rc, 'SHOT can''t be used in CREATE FILTER'));

    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'CREATE FILTER "ID" expected'));

    if TheMixer.FindFilter(ID) <> nil then
     Exit(Output.Report(409, Command, rc, 'CREATE FILTER failed: filter identified by "' + ID + '" already exists'));

    if not TokenizeToken(Command, 'TYPE') then
     Exit(Output.Report(400, Command, rc, 'CREATE FILTER "ID" TYPE "TYPE" expected'));

    if not TokenizeValue(Command, Cmd) then
     Exit(Output.Report(400, Command, rc, 'CREATE FILTER "ID" TYPE "TYPE" expected'));

    if not TokenizeToken(Command, 'DEFAULT') then
     Exit(Output.Report(400, Command, rc, 'CREATE FILTER "ID" TYPE "TYPE" DEFAULT'));

    if Cmd = 'EQUALIZER' then Filter:=TMixerFilterEqualizer.Create(ID, Command, rc, Output)
    else if Cmd = 'COMPRESSOR' then Filter:=TMixerFilterCompressor.Create(ID, Command, rc, Output)
    else if Cmd = 'ENHANCER' then Filter:=TMixerFilterEnhancer.Create(ID, Command, rc, Output)
    else if Cmd = 'TRUEBASS' then Filter:=TMixerFilterTrueBass.Create(ID, Command, rc, Output)
    else if Cmd = 'SOUND3D' then Filter:=TMixerFilterSound3D.Create(ID, Command, rc, Output)
    else if Cmd = 'LOWPASS' then Filter:=TMixerFilterLowPass.Create(ID, Command, rc, Output)
    else if Cmd = 'HIGHPASS' then Filter:=TMixerFilterHighPass.Create(ID, Command, rc, Output)
    else if Cmd = 'PHASER' then Filter:=TMixerFilterPhaser.Create(ID, Command, rc, Output)
    else if Cmd = 'FLANGER' then Filter:=TMixerFilterFlanger.Create(ID, Command, rc, Output)
    else if Cmd = 'DYNAMP' then Filter:=TMixerFilterDynAmp.Create(ID, Command, rc, Output)
    else Exit(Output.Report(400, Command, rc, 'Unknown CREATE FILTER TYPE "' + Cmd + '"'));

    if Filter = nil then
     Exit(False);

    //Log('Filter "' + ID + '" created');

    Output.Report(200, Command, rc, 'Filter "' + ID + '" ready');

    TheMixer.AddFilter(Filter);

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'CREATE SOURCE/SHOT SOURCE/TARGET/FILTER expected, got "' + Cmd + '"'));
 end;

// DROP SOURCE / DROP TARGET / DROP FILTER

function ExecuteDrop(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID, Group: String;
  Source: TMixerSource;
  Target: TMixerTarget;
  Filter: TMixerFilter;
  Setter: TMixerSetter;
  Schedule: TMixerSchedule;
  First: Boolean;
  K: LongInt;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'DROP SOMETHING expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DROP SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'DROP SOURCE failed: there is no source identified by "' + ID + '"'));

    TheMixer.RemoveSource(Source);

    //Log('Source "' + ID + '" dropped');

    Output.Report(200, Command, rc, 'Source "' + ID + '" was dropped');

    Exit(True);
   end
  else if Cmd = 'SOURCES' then
   begin
    if not TokenizeToken(Command, 'LIKE') then
     Exit(Output.Report(400, Command, rc, 'DROP SOURCES LIKE expected'));

    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DROP SOURCES LIKE "ID" expected'));

    First:=True;

    while True do
     begin
      Source:=TheMixer.FindSourceLike(ID);

      if Source = nil then
       if First then
        Exit(Output.Report(404, Command, rc, 'DROP SOURCES failed: there is no sources like "' + ID + '"'))
       else
        Exit(True);

      Output.Report(200, Command, rc, 'Source "' + Source.ID + '" (like "' + ID + '") was dropped');

      TheMixer.RemoveSource(Source);

      First:=False;
     end;

    Exit(True);
   end
  else if Cmd = 'SETS' then
   begin
    if not TokenizeToken(Command, 'GROUP') then
     Exit(Output.Report(400, Command, rc, 'DROP SETS GROUP expected'));

    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DROP SETS GROUP "group" expected'));

    First:=True;

    K:=0;

    while True do
     begin
      Setter:=TheMixer.FindSetterGroup(ID);

      if Setter = nil then
       if First then
        Exit(Output.Report(404, Command, rc, 'DROP SETS GROUP failed: there is no setters in group "' + ID + '"'))
       else
        begin
         Output.Report(200, Command, rc, IntToStr(K) + ' setters was dropped');

         Exit(True);
        end;

      Inc(K);

      TheMixer.RemoveSetter(Setter);

      First:=False;
     end;

    Exit(True);
   end
  else if Cmd = 'SCHEDULES' then
   begin
    if not TokenizeToken(Command, 'OF') then
     Exit(Output.Report(400, Command, rc, 'DROP SCHEDULES OF "ID" GROUP "group" expected'));

    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DROP SCHEDULES OF "ID" GROUP "group" expected'));

    if not TokenizeToken(Command, 'GROUP') then
     Exit(Output.Report(400, Command, rc, 'DROP SCHEDULES OF "ID" GROUP "group" expected'));

    if not TokenizeValue(Command, Group) then 
     Exit(Output.Report(400, Command, rc, 'DROP SCHEDULES OF "ID" GROUP "group" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'DROP SCHEDULES failed: there is no source identified by "' + ID + '"'));

    First:=True;

    K:=0;

    while True do
     begin
      Schedule:=Source.FindScheduleGroup(Group);

      if Schedule = nil then
       if First then
        Exit(Output.Report(404, Command, rc, 'DROP SCHEDULES failed: there is no schedules in group "' + Group + '" in source "' + ID + '"'))
       else
        begin
         Output.Report(200, Command, rc, IntToStr(K) + ' schedules was dropped');

         Exit(True);
        end;

      Inc(K);

      Source.RemoveSchedule(Schedule);

      Schedule.Free;

      First:=False;
     end;

    Exit(True);
   end
  else if Cmd = 'TARGET' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DROP TARGET "ID" expected'));

    Target:=TheMixer.FindTarget(ID);

    if Target = nil then
     Exit(Output.Report(404, Command, rc, 'DROP TARGET failed: there is no target identified by "' + ID + '"'));

    TheMixer.RemoveTarget(Target);

    Output.Report(200, Command, rc, 'Target "' + ID + '" was dropped');

    Exit(True);
   end
  else if Cmd = 'FILTER' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DROP FILTER "ID" expected'));

    Filter:=TheMixer.FindFilter(ID);

    if Filter = nil then
     Exit(Output.Report(404, Command, rc, 'DROP FILTER failed: there is no filter identified by "' + ID + '"'));

    TheMixer.RemoveFilter(Filter);

    //Log('Filter "' + ID + '" dropped');

    Output.Report(200, Command, rc, 'Filter "' + ID + '" was dropped');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'DROP SOURCE/TARGET expected, got "' + Cmd + '"'));
 end;

// PLAY SOURCE "ID"

function ExecutePlay(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID: String;
  Source: TMixerSource;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'PLAY SOURCE expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'PLAY SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'PLAY SOURCE failed: there is no source identified by "' + ID + '"'));

    Source.Play;

    Output.Report(200, Command, rc, 'Playing source "' + ID + '"');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'PLAY SOURCE expected, got "' + Cmd + '"'));
 end;

// PAUSE SOURCE "ID"

function ExecutePause(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID: String;
  Source: TMixerSource;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'PAUSE SOURCE expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'PAUSE SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'PAUSE SOURCE failed: there is no source identified by "' + ID + '"'));

    Source.Stop;

    Output.Report(200, Command, rc, 'Paused source "' + ID + '"');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'PAUSE SOURCE expected, got "' + Cmd + '"'));
 end;

// STOP SOURCE "ID"

function ExecuteStop(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID: String;
  Source: TMixerSource;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'STOP SOURCE expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'STOP SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'STOP SOURCE failed: there is no source identified by "' + ID + '"'));

    Source.Stop;
    Source.Seek(0.0);

    Output.Report(200, Command, rc, 'Stopped source "' + ID + '"');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'STOP SOURCE expected, got "' + Cmd + '"'));
 end;

// SEEK SOURCE "ID" TO "secs"

function ExecuteSeek(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID: String;
  Source: TMixerSource;
  SeekTo: Double;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'SEEK SOURCE expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'SEEK SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'SEEK SOURCE failed: there is no source identified by "' + ID + '"'));

    if not TokenizeToken(Command, 'TO') then
     Exit(Output.Report(400, Command, rc, 'SEEK SOURCE "ID" TO "secs" expected'));

    if not TokenizeValue(Command, Cmd) then
     Exit(Output.Report(400, Command, rc, 'SEEK SOURCE "ID" TO "secs" expected'));

    if not Source.Seekable then
     Exit(Output.Report(403, Command, rc, 'SEEK SOURCE failed: source "' + ID + '" is not seekable (streaming?)'));

    SeekTo:=StrToFloatDef(Cmd, 0.0);

    Source.Seek(SeekTo);

    Output.Report(200, Command, rc,
                           'Seeked source "' + ID + '" ' +
                           'to ' + FloatToStrF(SeekTo, ffFixed, 15, 2) + ' ' +
                           '(real ' + FloatToStrF(Source.GetPosition, ffFixed, 15, 2) + ')');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'SEEK SOURCE expected, got "' + Cmd + '"'));
 end;

// SET VOLUME OF SOURCE "file1" DELAY "10" FROM "0" TO "100" DURING "20"

const
 ExecuteSetFreezeAnchor: TTimeFixer = 0;

function ExecuteSet(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Parameter, ID, S: String;
  SourceKind: TMixerSetterSourceKind;
  TokenType: TTokenType;
  Source: TMixerSource;
  Filter: TMixerFilter;
  Setter: TMixerSetter;
  Frozen: Boolean;
 begin
  if not TokenizeTokenVar(Command, Parameter) then
   Exit(Output.Report(400, Command, rc, 'SET SOMETHING expected'));

  if Parameter = 'FREEZE' then
   begin
    ExecuteSetFreezeAnchor:=LocalTimeAnchor - Uptime;
    Output.Report(200, Command, rc, 'Relative setters anchor frozen to ' + FloatToStrF(ExecuteSetFreezeAnchor / 1000, ffFixed, 15, 4) + ', now try SET FROZEN ... to use it');
    Exit(True);
   end;

  if Parameter = 'FROZEN' then
   begin
    Frozen:=True;

    if not TokenizeTokenVar(Command, Parameter) then
     Exit(Output.Report(400, Command, rc, 'SET SOMETHING expected'));
   end
  else
   Frozen:=False;

  if not TokenizeToken(Command, 'OF') then
   Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' OF expected'));

  if (not TokenizeTokenVar(Command, S)) or ((S <> 'SOURCE') and (S <> 'FILTER')) then
   Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' OF SOURCE/FILTER expected'));

  if S = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then
     Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' OF SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);
    SourceKind:=sskSource;

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'SET ' + Parameter + ' OF SOURCE failed: there is no source identified by "' + ID + '"'));

    if not Source.HaveParameter(Parameter) then
     Exit(Output.Report(404, Command, rc, 'SET ' + Parameter + ' OF SOURCE "' + ID + '" failed: source have no parameter "' + Parameter + '"'));
   end
  else if S = 'FILTER' then
   begin
    if not TokenizeValue(Command, ID) then
     Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' OF FILTER "ID" expected'));

    Filter:=TheMixer.FindFilter(ID);
    SourceKind:=sskFilter;

    if Filter = nil then
     Exit(Output.Report(404, Command, rc, 'SET ' + Parameter + ' OF FILTER failed: there is no filter identified by "' + ID + '"'));

    if not Filter.HaveParameter(Parameter) then
     Exit(Output.Report(404, Command, rc, 'SET ' + Parameter + ' OF FILTER "' + ID + '" failed: filter have no parameter "' + Parameter + '"'));
   end
  else
   Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' OF SOURCE/FILTER expected'));

  Setter:=TMixerSetter.Create(SourceKind, ID, Parameter);

  while Command <> '' do
   begin
    if (not Tokenize(Command, S, TokenType)) or (TokenType in [ttValue, ttError]) then
     Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' failed: DELAY/FROM/TO/DURING expected (' + IntToStr(Longint(TokenType)) + ')'));

    if TokenType in [ttNone, ttSemicolon] then
     Break;

    if S = 'GROUP' then
     begin
      if not TokenizeValue(Command, S) then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' ... GROUP "NAME" expected'));

      Setter.Group:=S;
     end
    else if S = 'DELAY' then
     begin
      if Setter.ValueDelayDefined then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' failed: DELAY again?'));

      if not TokenizeValue(Command, S) then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' ... DELAY "VALUE" expected'));

      Setter.ValueDelay:=StrToFloatDef(S, 0.0);
      Setter.ValueDelayDefined:=True;
     end
    else if S = 'FROM' then 
     begin
      if Setter.ValueFromDefined then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' failed: FROM again?'));

      if not TokenizeValue(Command, S) then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' ... FROM "VALUE" expected'));

      Setter.ValueFrom:=StrToFloatDef(S, 0.0);
      Setter.ValueFromDefined:=True;
     end
    else if S = 'TO' then
     begin
      if Setter.ValueToDefined then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' failed: TO again?'));

      if not TokenizeValue(Command, S) then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' ... TO "VALUE" expected'));

      Setter.ValueTo:=StrToFloatDef(S, 0.0);
      Setter.ValueToDefined:=True;
     end
    else if S = 'DURING' then
     begin
      if Setter.ValueDuringDefined then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' failed: DURING again?'));

      if not TokenizeValue(Command, S) then
       Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' ... DURING "VALUE" expected'));

      Setter.ValueDuring:=StrToFloatDef(S, 0.0);
      Setter.ValueDuringDefined:=True;
     end
    else
     Exit(Output.Report(400, Command, rc, 'SET ' + Parameter + ' ... DELAY/FROM/TO/DURING expected, "' + S + '"'));
   end;

  if Frozen then
   Setter.Startup(ExecuteSetFreezeAnchor)
  else
   Setter.Startup(LocalTimeAnchor - Uptime);

  TheMixer.AddSetter(Setter);

  EnterCriticalSection(GiantCounters, 'ExecuteSet');
  Inc(MixerSettersCreated);
  LeaveCriticalSection(GiantCounters, 'ExecuteSet');

  Output.Report(200, Command, rc, 'Setter queued (relative to ' + FloatToStrF(Setter.StartedAt / 1000, ffFixed, 15, 4) + ')');

  Exit(True);
 end;

// SCHEDULE SOURCE "ID" EXECUTE "COMMAND"

function ExecuteSchedule(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, ID, Value, Group: String;
  Debugging, Respawn: Boolean;
  Source: TMixerSource;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE expected'));

  if Cmd = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'SCHEDULE SOURCE failed: there is no source identified by "' + ID + '"'));

    if not TokenizeToken(Command, 'AT') then
     Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT expected'));

    if not TokenizeValue(Command, Value) then
     Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" expected'));

    if not TokenizeTokenVar(Command, Cmd) then
     Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" [DEBUG] [RESPAWN] [GROUP "groupname"] DO expected'));

    if Cmd = 'DEBUG' then
     begin
      Debugging:=True;

      if not TokenizeTokenVar(Command, Cmd) then
       Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" [DEBUG] [RESPAWN] [GROUP "groupname"] DO expected'));
     end
    else
     Debugging:=False;

    if Cmd = 'RESPAWN' then
     begin
      Respawn:=True;

      if not TokenizeTokenVar(Command, Cmd) then
       Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" [DEBUG] [RESPAWN] [GROUP "groupname"] DO expected'));
     end
    else
     Respawn:=False;

    if Cmd = 'GROUP' then
     begin
      if not TokenizeValue(Command, Group) then
       Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" [DEBUG] [RESPAWN] [GROUP "groupname"] DO expected'));

      if not TokenizeTokenVar(Command, Cmd) then
       Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" [DEBUG] [RESPAWN] [GROUP "groupname"] DO expected'));
     end
    else
     Group:='';

    if Cmd <> 'DO' then
     Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" DO expected'));

    if not TokenizeValue(Command, Cmd) then
     Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE "ID" AT "secs" DO "commands" expected'));

    Source.Schedule(StrToFloatDef(Value, 0.0), Cmd, Debugging, Respawn, Group);

    EnterCriticalSection(GiantCounters, 'ExecuteSchedule');
    Inc(MixerSchedulesCreated);
    LeaveCriticalSection(GiantCounters, 'ExecuteSchedule');

    if Debugging then
     Output.Report(200, Command, rc, 'Scheduled source "' + ID + '" (with debugging)')
    else
     Output.Report(200, Command, rc, 'Scheduled source "' + ID + '"');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'SCHEDULE SOURCE expected, got "' + Cmd + '"'));
 end;

// LOG "ID"

function ExecuteLog(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  S: String;
 begin
  if not TokenizeValue(Command, S) then
   Exit(Output.Report(400, Command, rc, 'LOG "text" expected'));

  Log(S);

  Output.Report(200, Command, rc, 'Logged');
  Exit(True);
 end;

// TITLE "ID"

function ExecuteTitle(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  S: String;
  K: Integer;
 begin
  if not TokenizeValue(Command, S) then
   Exit(Output.Report(400, Command, rc, 'TITLE "text" expected'));

  for K:=0 to Length(TheMixer.Targets) - 1 do
   TheMixer.Targets[K].SetTitle(S);

  EnterCriticalSection(GiantTitle, 'ExecuteTitle');

  TheMixer.Title:=S;
  MixerTitleLength:=Length(S);

  LeaveCriticalSection(GiantTitle, 'ExecuteTitle');

  Output.Report(200, Command, rc, 'New title is "' + S + '"');
  Exit(True);
 end;

// ATTACH FILTER TO SOURCE "id" / TO MASTER

function ExecuteAttach(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  ID, S: String;
  Filter: TMixerFilter;
  Source: TMixerSource;
 begin
  if (not TokenizeToken(Command, 'FILTER')) or (not TokenizeValue(Command, ID)) then
   Exit(Output.Report(400, Command, rc, 'ATTACH FILTER "ID" TO SOURCE/MASTER expected'));

  Filter:=TheMixer.FindFilter(ID);

  if Filter = nil then
   Exit(Output.Report(404, Command, rc, 'ATTACH FILTER failed: there is no filter identified by "' + ID + '"'));

  if (not TokenizeToken(Command, 'TO')) or (not TokenizeTokenVar(Command, S)) then
   Exit(Output.Report(400, Command, rc, 'ATTACH FILTER "ID" TO SOURCE/MASTER expected'));

  if S = 'MASTER' then
   begin
    EnterCriticalSection(GiantFilters, 'ExecuteAttach1');

    Filter.Reset;

    TheMixer.MasterFilters.Add(Filter);

    LeaveCriticalSection(GiantFilters, 'ExecuteAttach1');

    Output.Report(200, Command, rc, 'Attached to master');
    Exit(True);
   end
  else if S = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'ATTACH FILTER "ID" TO SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'ATTACH FILTER failed: there is no source identified by "' + ID + '"'));

    EnterCriticalSection(GiantFilters, 'ExecuteAttach2');

    Filter.Reset;

    Source.Filters.Add(Filter);

    LeaveCriticalSection(GiantFilters, 'ExecuteAttach2');

    Output.Report(200, Command, rc, 'Attached to source');
    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'ATTACH FILTER "ID" TO SOURCE/MASTER expected'));
 end;

function ExecuteDeattach(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  ID, S: String;
  Filter: TMixerFilter;
  Source: TMixerSource;
  Did: Boolean;
 begin
  if (not TokenizeToken(Command, 'FILTER')) or (not TokenizeValue(Command, ID)) then
   Exit(Output.Report(400, Command, rc, 'DEATTACH FILTER "ID" FROM SOURCE/MASTER expected'));

  Filter:=TheMixer.FindFilter(ID);

  if Filter = nil then
   Exit(Output.Report(404, Command, rc, 'DEATTACH FILTER failed: there is no filter identified by "' + ID + '"'));

  if (not TokenizeToken(Command, 'FROM')) or (not TokenizeTokenVar(Command, S)) then
   Exit(Output.Report(400, Command, rc, 'DEATTACH FILTER "ID" FROM SOURCE/MASTER expected'));

  if S = 'MASTER' then
   begin
    EnterCriticalSection(GiantFilters, 'ExecuteDeattach1');

    Filter.Reset;

    Did:=TheMixer.MasterFilters.Remove(Filter);

    LeaveCriticalSection(GiantFilters, 'ExecuteDeattach1');

    if Did then
     Output.Report(200, Command, rc, 'Deattached from master')
    else
     Output.Report(404, Command, rc, 'Deattach from master failed (no filter was attached)');

    Exit(True);
   end
  else if S = 'SOURCE' then
   begin
    if not TokenizeValue(Command, ID) then 
     Exit(Output.Report(400, Command, rc, 'DEATTACH FILTER "ID" FROM SOURCE "ID" expected'));

    Source:=TheMixer.FindSource(ID);

    if Source = nil then
     Exit(Output.Report(404, Command, rc, 'DEATTACH FILTER failed: there is no source identified by "' + ID + '"'));

    EnterCriticalSection(GiantFilters, 'ExecuteDeattach2');

    Filter.Reset;

    Did:=Source.Filters.Remove(Filter);

    LeaveCriticalSection(GiantFilters, 'ExecuteDeattach2');

    if Did then
     Output.Report(200, Command, rc, 'Attached to source')
    else
     Output.Report(404, Command, rc, 'Deattach from source failed (no filter was attached)');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'ATTACH FILTER "ID" TO SOURCE/MASTER expected'));
 end;

// EXECUTE ONCE/ANYWAY "URL" OR EXECUTE "filename"
// EXECUTE BACK "command"

function ExecuteExecute(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  S, How, D, URLHost, URLPort, URLSuffix: String;
  Matches: TStringList;
  Stream: TSocketStream;
  Bytes: Longint;
  F: TextFile;
 begin
  if (not TokenizeTokenVar(Command, How)) or ((How <> 'ONCE') and (How <> 'ANYWAY') and (How <> 'BACK')) then
   Exit(Output.Report(400, Command, rc, 'EXECUTE ONCE|ANYWAY|BACK "URL/filename" expected'));

  if not TokenizeValue(Command, S) then
   Exit(Output.Report(400, Command, rc, 'EXECUTE "URL/filename" expected'));

  if How = 'BACK' then
   begin
    TBackgroundExecutor.Create(S, 'background');

    Output.Report(200, Command, rc, 'Done');

    Exit(True);
   end;

  try
   Matches:=TStringList.Create;

   if preg_match('^http\:\/\/(.*?)(\/.*)$', S, Matches) then
    begin
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

     while (not GlobalStop) do
      begin
       try
        Stream:=TSocketStream.Connect(URLHost, StrToIntDef(URLPort, 80), 30);
       except
        on E: Exception do Log('ExecuteExecute: EXECUTE "' + S + '" exception on TSocketStream.Connect ' + E.Message);
       end;

       if Stream <> nil then
        begin
         D:='GET ' + URLSuffix + ' HTTP/1.0' + #10 +
            'Accept: */*' + #10 +
            'Connection: close' + #10 +
            'Host: ' + URLHost + #10 +
            'User-Agent: ' + AudinxTitle + #10 +
            #10;

         try
          Bytes:=Stream.Write(D[1], Length(D));
         except
          Bytes:=0;
         end;

         if Bytes > 0 then
          if Stream.Gets(D, True) and ((Trim(D) = 'HTTP/1.0 200 OK') or (Trim(D) = 'HTTP/1.1 200 OK')) then
           begin
            while Stream.Gets(D, True) do
             begin
              if Trim(D) = '' then
               Break;
             end;

            //Output.Report(101, Command, rc, 'Execution started');

            while Stream.Gets(D, True) do
             begin
              if GlobalStop then
               Break
              else
               Execute(D, rc, Output);
             end;

            Stream.Free;

            Break;
           end
          else
           Stream.Free
         else
          Stream.Free;
        end;

       if How = 'ANYWAY' then
        begin
         Log('Failed to execute "' + S + '", retrying');
         Output.Report(0, Command, rc, 'Failed to execute "' + S + '", retrying');
         Wait(1000);
         Continue;
        end
       else
        Exit(Output.Report(404, Command, rc, 'Failed to execute "' + S + '"'));
      end;
    end
   else
    begin
     Matches.Free;

     while True do
      begin
       Assign(F, S);

       try
        System.Reset(F);
       except
        if How = 'ANYWAY' then
         begin
          Log('Failed to execute "' + S + '", retrying');
          Output.Report(0, Command, rc, 'Failed to execute "' + S + '", retrying');
          Wait(1000);
          Continue;
         end
        else
         begin
          Result:=Output.Report(404, Command, rc, 'Failed to execute "' + S + '"');
          Exit;
         end;
       end;

       //Output.Report(101, Command, rc, 'Execution started');

       while not Eof(F) do
        begin
         ReadLn(F, D);

         Execute(D, rc, Output);

         if GlobalStop then
          Break;
        end;

       CloseFile(F);

       Break;
      end;
    end;
  except
   on E: Exception do Log('EXCEPTION: EXECUTE "' + S + '" global ' + E.Message);
  end;

  //Output.Report(102, Command, rc, 'Execution finished');
  Output.Report(200, Command, rc, 'Done');

  Exit(True);
 end;

// SLEEP "ID"

function ExecuteSleep(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  S: String;
 begin
  if not TokenizeValue(Command, S) then
   Exit(Output.Report(400, Command, rc, 'SLEEP "secs" expected'));

  Wait(Trunc(StrToFloatDef(S, 0) * 1000));

  Output.Report(200, Command, rc, 'Good morning!');
  Exit(True);
 end;

// MAINTENANCE PREFETCH

function ExecuteMaintenance(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, S: String;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'MAINTENANCE PREFETCH "SECONDS" expected'));

  if Cmd = 'PREFETCH' then
   begin
    if not TokenizeValue(Command, S) then
     Exit(Output.Report(400, Command, rc, 'MAINTENANCE PREFETCH "SECONDS" expected'));

    Prefetch.Purge(StrToIntDef(S, 31 * 86400));

    Output.Report(200, Command, rc, 'Well done');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'MAINTENANCE PREFETCH "SECONDS" expected, got "' + Cmd + '"'));
 end;

// SETUP

function ExecuteSetup(var Command, rc: String; Output: TSocketStream): Boolean;
 var
  Cmd, S: String;
  K: LongInt;
 begin
  if not TokenizeTokenVar(Command, Cmd) then
   Exit(Output.Report(400, Command, rc, 'SETUP BUFFER expected'));

  if Cmd = 'BUFFER' then
   begin
    if not TokenizeValue(Command, S) then
     Exit(Output.Report(400, Command, rc, 'SETUP BUFFER "SECONDS" expected'));

    K:=Trunc(StrToFloatDef(S, 0.25) * 44100);

    if K < 4410 then K:=4410;
    if K > 441000 then K:=441000;

    MixerSamplesInterval:=K;

    Output.Report(200, Command, rc, 'Well done');

    Exit(True);
   end
  else
   Exit(Output.Report(400, Command, rc, 'SETUP BUFFER expected, got "' + Cmd + '"'));
 end;

// SHUTDOWN

function ExecuteShutdown(var Command, rc: String; Output: TSocketStream): Boolean;
 begin
  System.EnterCriticalSection(GlobalLock);

  RealGlobalStop:=True;

  System.LeaveCriticalSection(GlobalLock);

  Output.Report(200, Command, rc, 'We''re going down');
  Exit(True);
 end;

// RESTART

function ExecuteRestart(var Command, rc: String; Output: TSocketStream): Boolean;
 begin
  System.EnterCriticalSection(GlobalLock);

  RealGlobalRefresh:=True;
  RealGlobalStop:=True;

  System.LeaveCriticalSection(GlobalLock);

  Output.Report(200, Command, rc, 'We''re going to restart');
  Exit(True);
 end;

// Execute

function Execute(var Command, rc: String; Output: TSocketStream; StopOnError: Boolean = False): Boolean;
 var
  Token, ACommand: String;
  TokenType: TTokenType;
  Ok: Boolean;
 begin
  Result:=True;
  rc:='';

  ACommand:=Command;

  while Command <> '' do
   begin
    Tokenize(Command, Token, TokenType);

    if TokenType = ttToken then
     begin
      Ok:=False;

      try
       if Token = 'STATUS' then Ok:=ExecuteStatus(Command, rc, Output)
       else if Token = 'CREATE' then Ok:=ExecuteCreate(Command, rc, Output)
       else if Token = 'DROP' then Ok:=ExecuteDrop(Command, rc, Output)
       else if Token = 'PLAY' then Ok:=ExecutePlay(Command, rc, Output)
       else if Token = 'PAUSE' then Ok:=ExecutePause(Command, rc, Output)
       else if Token = 'STOP' then Ok:=ExecuteStop(Command, rc, Output)
       else if Token = 'SEEK' then Ok:=ExecuteSeek(Command, rc, Output)
       else if Token = 'SET' then Ok:=ExecuteSet(Command, rc, Output)
       else if Token = 'SCHEDULE' then Ok:=ExecuteSchedule(Command, rc, Output)
       else if Token = 'LOG' then Ok:=ExecuteLog(Command, rc, Output)
       else if Token = 'ATTACH' then Ok:=ExecuteAttach(Command, rc, Output)
       else if Token = 'DEATTACH' then Ok:=ExecuteDeattach(Command, rc, Output)
       else if Token = 'TITLE' then Ok:=ExecuteTitle(Command, rc, Output)
       else if Token = 'EXECUTE' then Ok:=ExecuteExecute(Command, rc, Output)
       else if Token = 'SLEEP' then Ok:=ExecuteSleep(Command, rc, Output)
       else if Token = 'SHUTDOWN' then Ok:=ExecuteShutdown(Command, rc, Output)
       else if Token = 'MAINTENANCE' then Ok:=ExecuteMaintenance(Command, rc, Output)
       else if Token = 'SETUP' then Ok:=ExecuteSetup(Command, rc, Output)
       else if Token = 'RESTART' then Ok:=ExecuteRestart(Command, rc, Output)
       else Exit(Output.Report(400, Command, rc, 'WTF ' + Token + '?'));
      except
       on E: Exception do
        begin
         Log('Execute: exception on ' + Token + ', cmd="' + ACommand + '" ' + E.Message);
         Ok:=False;
        end;
      end;

      if (not Ok) and StopOnError then
       Exit(False);

      if not Ok then
       Result:=False;
     end
    else if TokenType = ttNone then
     Break
    else if TokenType = ttValue then
     Exit(Output.Report(400, Command, rc, 'unexpected value "' + Token + '"'))
    else if TokenType = ttSemicolon then
     Continue
    else if Copy(Command, 1, 1) = '#' then
     Break
    else
     Exit(Output.Report(400, Command, rc, 'parse error near "' + Copy(Command, 1, 64) + '"'));

    if GlobalStop then
     Exit;
   end;
 end;

end.