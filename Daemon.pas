// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Daemon;

interface
uses 
     SysUtils,
     BaseUnix,
     Classes,
     Sockets,
     Common,
     TimeFixer;

const
 RunningAs              : (raConsole, raDaemon)           = raConsole;

 PidFile                : String                          = '';

 LogMain                : String                          = 'audinx.log';
 LogPrefix              : String                          = 'audinx';

 DaemonIP               : ShortString                     = '127.0.0.1';
 DaemonPort             : Integer                         = 11111;

 RealGlobalRefresh      : Boolean                         = False;
 RealGlobalStop         : Boolean                         = False;
 RealGlobalPipe         : Boolean                         = False;

 Debugging              : Boolean                         = False;

threadvar
 DaemonThreadID         : Integer;
 DaemonThreadName       : String;

{$IFNDEF WIN32}
type 
 PSigContext = pointer;

 TSigInfo = ^PSigInfo;
 PSigInfo = record
  si_signo: LongInt;  
  si_errno: LongInt;  
  si_code: LongInt; 
  _sifields: record 
    case LongInt of
      1: (
          _kill: record
            _pid: pid_t;
            _uid: uid_t;
          end;
        );
      2: (
          _timer: record
            _timer1: dword;
            _timer2: dword;
          end;
        );
      3: (
          _rt: record
            _pid: pid_t;
            _uid: uid_t;
            _sigval: pointer;
          end;
        );
      4: (
          _sigchld: record
            _pid: pid_t;
            _uid: uid_t;
            _status: LongInt;
            _utime: clock_t;
            _stime: clock_t;
          end;
        );
      5: (
          _sigfault: record
            _addr: pointer;
          end;
        );
      6: (
          _sigpoll: record
            _band: LongInt;
            _fd: LongInt;
          end;
        );
   end;
 end;
{$ENDIF}

type
 TSwitchParser = procedure(var K: Integer);

 TWorkerStarter = function(Socket: Pointer): Longint;

 TStartuper = procedure;

 TShutdowner = procedure;

 TDaemonThreadWorker = class(TDaemonThread)
 public
  StartWorker: TWorkerStarter;
  Finished: Boolean;
  Param: Pointer;
  constructor Create(AStartWorker: TWorkerStarter; const AParam: Pointer);
  procedure Execute; override;
 end;

var
 GlobalLock: TRTLCriticalSection;

{$IFNDEF WIN32}
procedure PerformSignalHUP(sig: LongInt; SigInfo: psiginfo; SigContext: Pointer); cdecl;
procedure PerformSignalQUIT(sig: LongInt; SigInfo: psiginfo; SigContext: Pointer); cdecl;
{$ENDIF}

procedure Log(const S: String; const ForceThreadName: String = '');
procedure DebugLog(const S: String);
procedure Die(const S: String);

procedure StartTheDaemon(ParseSwitch: TSwitchParser; StartWorker: TWorkerStarter; Startuper: TStartuper; Shutdowner: TShutdowner);

function GlobalRefresh: Boolean;
function GlobalStop: Boolean;
function GlobalPipe: Boolean;

implementation

var
 LogLock: TRTLCriticalSection;

{$IFNDEF WIN32}
procedure PerformSignalHUP(sig: LongInt; SigInfo: psiginfo; SigContext: Pointer); cdecl;
 var
  F: TextFile;
 begin
{
  System.EnterCriticalSection(GlobalLock);

  RealGlobalRefresh:=True;
  RealGlobalStop:=True;

  System.LeaveCriticalSection(GlobalLock);
}
  try
   Assign(F, 'audinx.locks.' + Format('%f', [TimeStampToMSecs(DateTimeToTimeStamp(Now))]));
   Rewrite(F);
   WriteLn(F, GetCriticalSectionsPendings);
   CloseFile(F);
  except
  end;
 end;
 
procedure PerformSignalQUIT(sig: LongInt; SigInfo: psiginfo; SigContext: Pointer); cdecl;
 begin
  System.EnterCriticalSection(GlobalLock);

  RealGlobalStop:=True;

  System.LeaveCriticalSection(GlobalLock);
 end;

procedure PerformSignalCHLD(sig: LongInt; SigInfo: psiginfo; SigContext: Pointer); cdecl;
 var
  S: cInt;
 begin
  fpWait(S);
 end;

procedure PerformSignalPIPE(sig: LongInt; SigInfo: psiginfo; SigContext: Pointer); cdecl;
 begin
  System.EnterCriticalSection(GlobalLock);

  RealGlobalPipe:=True;

  System.LeaveCriticalSection(GlobalLock);
 end;
{$ENDIF}

function LogPerform(const FName, S: String; const ForceThreadName: String = ''): Boolean;
 var
  F: Text;
 begin
  Result:=True;

  try
   Assign(F, FName);
   Append(F);
  except
   try
    Assign(F, FName);
    Rewrite(F);
   except
    Result:=False;
    Exit;
   end;
  end;

  try
   if Length(S) = 0 then
    WriteLn(F)
   else if ForceThreadName = '' then
    WriteLn(F, FormatDateTime('dd.mm.yyyy'#9'HH:nn:ss', Now), #9, LogPrefix, '/', DaemonThreadName, #9, S)
   else                                                           
    WriteLn(F, FormatDateTime('dd.mm.yyyy'#9'HH:nn:ss', Now), #9, LogPrefix, '/', ForceThreadName, #9, S);
  except
  end;

  try
   CloseFile(F);
  except
  end;
 end;
 
procedure Log(const S: String; const ForceThreadName: String = '');
 begin
  System.EnterCriticalSection(LogLock);

  try
   LogPerform(LogMain, S, ForceThreadName);

   if RunningAs = raConsole then
    begin
     if ForceThreadName = '' then
      WriteLn(LogPrefix, '/', DaemonThreadName, #9, S)
     else
      WriteLn(LogPrefix, '/', ForceThreadName, #9, S);

     Flush(Output);
    end;
  except
  end;

  System.LeaveCriticalSection(LogLock);
 end;

procedure DebugLog(const S: String);
 begin
  if Debugging then
   Log(S);
 end;

procedure Die(const S: String);
 begin
  Log(S);
  Halt(255);
 end;

// daemons

procedure StartTheDaemon(ParseSwitch: TSwitchParser; StartWorker: TWorkerStarter; Startuper: TStartuper; Shutdowner: TShutdowner);
 var
  Socket, AcceptedSocket, AddressSize: Integer;
  Address: TInetSockAddr;
  K, L, Leak: Longint;
  Pool: array of TDaemonThreadWorker;
  Got: Boolean;
  Yes: Integer;
  Pid: TextFile;
 begin
  InitCriticalSection(LogLock, 'startup');

  DaemonThreadID:=0;
  DaemonThreadName:='startup';

  // check parameters

  K:=1;

  while K <= ParamCount do
   begin
    if ParamStr(K) = '-d' then
     RunningAs:=raDaemon
    else if ParamStr(K) = '-h' then
     begin
      Inc(K);

      DaemonIP:=ParamStr(K);
     end
    else if ParamStr(K) = '--pid' then
     begin
      Inc(K);

      PidFile:=ParamStr(K);
     end
    else if ParamStr(K) = '-p' then
     begin
      Inc(K);

      DaemonPort:=StrToIntDef(ParamStr(K), 11111);
     end 
    else 
     ParseSwitch(K);

    Inc(K);
   end;

  // perform

{$IFNDEF WIN32}
  if RunningAs = raDaemon then
   begin
    DebugLog('forking');

    K:=fpFork;

    DebugLog('fork got=' + IntToStr(K));
  
    if K = -1 then
     begin
      DebugLog('failed to fork');
      Halt(255);
     end;
   
    if K <> 0 then
     begin
      //Write(K);
      Halt(0);
     end;
     
    Close(Input);
    Close(Output);
    Close(ErrOutput);
    Close(StdOut);
    Close(StdErr);

    fpSetSid;
   end;
{$ENDIF}

  DaemonThreadName:='dispatcher';

  Startuper;

  // write .pid-file

  if PidFile <> '' then
   begin
    try
     Assign(Pid, PidFile);
     Rewrite(Pid);

     WriteLn(Pid, System.GetProcessID);
    except
    end;

    try
     CloseFile(Pid);
    except
    end;
   end;

  // install signals handlers
  
{$IFDEF WIN32}
{$ELSE}
  InstallSignalHandler(SIGHUP, @PerformSignalHUP);
  InstallSignalHandler(SIGQUIT, @PerformSignalQUIT);
  InstallSignalHandler(SIGTERM, @PerformSignalQUIT);
  InstallSignalHandler(SIGABRT, @PerformSignalQUIT);
  InstallSignalHandler(SIGINT, @PerformSignalQUIT);
  InstallSignalHandler(SIGCHLD, @PerformSignalCHLD);
  InstallSignalHandler(SIGPIPE, @PerformSignalPIPE);
{$ENDIF}

  // leaks

  Leak:=GetHeapStatus.TotalAllocated;

  // init socket

  AddressSize:=SizeOf(Address);

  SetupSocket(Socket, Address, DaemonIP, DaemonPort);

  Yes:=1;
  fpSetSockOpt(Socket, SOL_SOCKET, SO_REUSEADDR, @Yes, Sizeof(Yes));

  if fpBind(Socket, @Address, AddressSize) < 0 then
   Die('failed to bind to ' + DaemonIP + ':' + IntToStr(DaemonPort));

  if fpListen(Socket, 1) < 0 then
   Die('listen error ' + IntToStr(SocketError));

{$IFNDEF WIN32}
  fpfcntl(Socket, F_SETFL, O_NONBLOCK);
{$ENDIF}

  SetLength(Pool, 0);

  while not GlobalStop do
   begin
    AcceptedSocket:=fpAccept(Socket, @Address, @AddressSize);

    if AcceptedSocket <= 0 then
     begin
      Got:=False;

      for K:=Length(Pool) - 1 downto 0 do
       if Pool[K].Finished then
        begin
         Pool[K].Free;

         for L:=K + 1 to Length(Pool) - 1 do
          Pool[L - 1]:=Pool[L];

         SetLength(Pool, Length(Pool) - 1);

         Got:=True;

         Break;
        end;

      if not Got then
       Wait(100);
     end
    else
     begin
      SetLength(Pool, Length(Pool) + 1);
      Pool[Length(Pool) - 1]:=TDaemonThreadWorker.Create(StartWorker, Pointer(AcceptedSocket));
     end;
   end;

  for K:=Length(Pool) - 1 downto 0 do
   Pool[K].Terminate;

  for K:=Length(Pool) - 1 downto 0 do
   begin
    Pool[K].WaitFor;
    Pool[K].Free;
   end;

  fpShutdown(Socket, 2);
  fpClose(Socket);

  Shutdowner;

  DoneCriticalSection(LogLock, 'shutdown');

  if PidFile <> '' then
   DeleteFile(PidFile);
  
  Leak:=Longint(GetHeapStatus.TotalAllocated) - Longint(Leak);

  if RunningAs = raConsole then
   WriteLn('memory leak: ' + IntToStr(Leak));
 end;

// TDaemonThreadWorker

constructor TDaemonThreadWorker.Create(AStartWorker: TWorkerStarter; const AParam: Pointer);
 begin
  inherited Create(True);

  StartWorker:=AStartWorker;
  Param:=AParam;
  Finished:=False;

  Resume;
 end;

procedure TDaemonThreadWorker.Execute;
 begin
  StartWorker(Param);

  Finished:=True;
 end;

// Global*

function GlobalRefresh: Boolean;
 begin
  System.EnterCriticalSection(GlobalLock);

  Result:=RealGlobalRefresh;

  System.LeaveCriticalSection(GlobalLock);
 end;

function GlobalStop: Boolean;
 begin
  System.EnterCriticalSection(GlobalLock);

  Result:=RealGlobalStop;

  System.LeaveCriticalSection(GlobalLock);
 end;

function GlobalPipe: Boolean;
 begin
  System.EnterCriticalSection(GlobalLock);

  Result:=RealGlobalPipe;

  System.LeaveCriticalSection(GlobalLock);
 end;

initialization
 System.InitCriticalSection(GlobalLock);

finalization
 System.DoneCriticalSection(GlobalLock);

end.