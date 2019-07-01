// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014
//
// maximum respect to
//  - Andrei Borovsky <aborovsky@mtu-net.ru> for the perfect lame & mad headers
//  - Milenko Mitrovic <dcoder@dsp-worx.de> for the DSP worx

uses 
     {$IFNDEF WIN32}
     CMem,
     Unix,
     {$ENDIF}
     CThreads,
     BaseUnix, 
     SysUtils,
     Classes,
     Sockets,
     Common,
     Daemon,
     Tokenizer,
     Executor,
     Mixer,
     MixerSources,
     MixerSchedules,
     MAD,
     Lame,
     AAC,
     TimeFixer,
     Prefetcher,
     dspFastFourier;

{$H+}

{$IFNDEF WIN32}
     {$LINKLIB c}
     {$LINKLIB m}
{$ENDIF}

// some vars ;)

var
 Acceptables, ExecuteOnStart: TStringList;

// switches parser

procedure CommandLine(var K: Integer);
 var
  S: String;
  L: Integer;
 begin
  S:=ParamStr(K);

  if S = '-l' then begin Inc(K); LogMain:=ParamStr(K); end 
  else if S = '-a' then begin Inc(K); Acceptables.Add(ParamStr(K)); end 
  else if S = '-e' then begin Inc(K); ExecuteOnStart.Add(ParamStr(K)); end 
  else if S = '-P' then begin Inc(K); LogPrefix:=ParamStr(K); end
  else if S = '-T' then begin Inc(K); MixerFineTune:=StrToIntDef(ParamStr(K), MixerFineTune); end
  else if S = '-f' then begin Inc(K); Prefetch.Folder:=ParamStr(K); end
  else if S = '--execute' then 
   begin 
    S:='';

    for L:=K + 1 to ParamCount do
     S:=S + ParamStr(L) + ' ';

    for L:=1 to Length(S) do
     if S[L] = '@' then S[L]:='"'
     else if S[L] = '!' then S[L]:='''';

    ExecuteOnStart.Add(S);

    K:=ParamCount + 1;
   end
  else if S = '--debug' then Debugging:=True
  else if S = '--dump-status' then MixerDumpStatus:=True
  else if S = '--dump-idle' then MixerDumpIdle:=True
  else if S = '--dump-time-correction' then MixerDumpTimeCorrection:=True
  else if S = '--check-libs' then
   begin
    LoadLAME;

    if not LAMELoaded then
     begin
      WriteLn('NO LAME');
      Halt(255);
     end;

    if not MADLibLoaded then 
     begin
      WriteLn('NO MAD');
      Halt(255);
     end;

    if not AACLibLoaded then
     begin
      WriteLn('NO AAC');
      Halt(255);
     end;

    WriteLn('AUDINX READY');
    Halt(0);
   end
  else
   begin
    WriteLn('syntax: audinx [-h 192.168.0.1] [-p 8127] [-d] [--pid audinx.pid] [-l log] [-P audinx] [-a 192.168.0.2] [-e command] [-T 0] [-f prefetchfolder/] [--debug] [--dump-status] [--dump-idle] [--check-libs]');
    Halt(255);
   end;
 end;

// connection

function ControlConnection(Socket: Pointer): Longint;
 var
  Got, K: Longint;
  D, rc: String;
  S: TSocketStream;
 begin
  Result:=0;
  DaemonThreadID:=Longint(Socket);
  DaemonThreadName:='control/' + GetSocketPeer(Longint(Socket));

  S:=TSocketStream.Create(Longint(Socket), 10);

  // check for acceptables

  if Acceptables.Count > 0 then
   begin
    Got:=-1;
    D:=GetSocketPeer(Longint(Socket));

    for K:=0 to Acceptables.Count - 1 do
     if D = Acceptables[K] then
      begin
       Got:=K;
       Break;
      end;

    if Got = -1 then
     begin
      Log('this time is full of monsters like ' + GetSocketPeer(Longint(Socket)));
      S.Graceful;
      Exit;
     end;
   end;

  // yes!

{$IFNDEF WIN32}
  fpfcntl(Longint(Socket), F_SETFL, O_NONBLOCK);
{$ENDIF}

  S.Puts('WBC, AND WHO''S THIS?');

  if S.Gets(D) then
   if Trim(D) = 'THIS IS A BRAD' then
    begin
     S.Puts('200'#9'Right about now!');

     S.Timeout:=86400;

     while S.Gets(D) do
      if AnsiUpperCase(Trim(D)) = 'BYE' then
       begin
        S.Puts('200'#9'California is druggy druggy druggy');
        Break;
       end
      else
       try
        Execute(D, rc, S);
       except
        on E: Exception do 
         begin
          S.Puts('500'#9 + E.Message);
          Log('ControlConnection: exception ' + E.Message + ' while executing "' + D + '"');
         end;
       end;
    end
   else
    begin
     Log('Unknown access key "' + D + '" ("THIS IS A BRAD" expected)');

     S.Puts('403'#9'Check it out now');
    end
  else
   S.Puts('403'#9'Check it out now');

  S.Graceful;

  SetLength(DaemonThreadName, 0);

  Exit;
 end;

// startup

procedure Startup;
 begin
  InitializeDCFFT;

  InitCriticalSection(GiantTargets, 'GiantTargets');
  InitCriticalSection(GiantSetters, 'GiantSetters');
  InitCriticalSection(GiantSources, 'GiantSources');
  InitCriticalSection(GiantSchedules, 'GiantSchedules');
  InitCriticalSection(GiantFilters, 'GiantFilters');
  InitCriticalSection(GiantCounters, 'GiantCounters');
  InitCriticalSection(GiantTitle, 'GiantTitles');
  InitCriticalSection(GiantLocalTimeAnchor, 'GiantLocalTimeAnchor');

  LoadLAME;

  if not LAMELoaded then 
   Die('unable to startup without LAME library');

  if not MADLibLoaded then 
   Die('unable to startup without MAD library');

  Uptime:=TimeAnchor;

  SetLocalTimeAnchor(Uptime);

  TheMixer:=TMixer.Create;

  TheScheduler:=TMixerScheduler.Create;

  if ExecuteOnStart.Count > 0 then
   TBackgroundExecutor.Create(ExecuteOnStart, False, 'onstart');

  Log(AudinxTitle + ' started');
 end;

// shutdown

procedure Shutdown;
 begin
  TheScheduler.Free;

  TheMixer.Free;

  DoneCriticalSection(GiantLocalTimeAnchor, 'GiantLocalTimeAnchor');
  DoneCriticalSection(GiantTargets, 'GiantTargets');
  DoneCriticalSection(GiantSetters, 'GiantSetters');
  DoneCriticalSection(GiantSources, 'GiantSources');
  DoneCriticalSection(GiantSchedules, 'GiantSchedules');
  DoneCriticalSection(GiantFilters, 'GiantFilters');
  DoneCriticalSection(GiantCounters, 'GiantCounters');
  DoneCriticalSection(GiantTitle, 'GiantTitles');

  FinalizeDCFFT;

  Log('shutted down');
 end;

// respawn

function InitPChar(const S: String): PChar;
 begin
  Result:=StrAlloc(Length(S) + 1);

  StrPCopy(Result, S);
 end;

{$IFNDEF WIN32}
procedure Respawn;
 var
  Args: packed array of PChar;
  K: Integer;
 begin
  SetLength(Args, ParamCount + 2);

  for K:=0 to ParamCount do
   Args[K]:=InitPChar(ParamStr(K));

  Args[ParamCount + 1]:=nil;

  fpExecvp(ParamStr(0), @Args[0]);
 end;
{$ENDIF}

begin
 Acceptables:=TStringList.Create;
 ExecuteOnStart:=TStringList.Create;
 Prefetch:=TPrefetcher.Create;

 StartTheDaemon(CommandLine, ControlConnection, Startup, Shutdown);

 Prefetch.Free;
 ExecuteOnStart.Free;
 Acceptables.Free;

{$IFNDEF WIN32}
 if GlobalRefresh then
  Respawn;
{$ENDIF}
end.