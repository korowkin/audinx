// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Common;

interface

{.$DEFINE CSTRACE}

uses 
     CThreads, 
     SysUtils, 
     BaseUnix, 
     Classes, 
     Sockets, 
     Errors, 
     Unix, 
     TimeFixer, 
     RegExp, 
     Resolver;

const
 ChannelBufferCapacity                                                         = 2304;
 AudinxTitle                                                                   = 'Audinx/0.4.12';

 Uptime                                : TTimeFixer                            = 0;
 LocalAnchor                           : TTimeFixer                            = 0;

 MixerUnderruns                        : LongInt                               = 0;
 MixerSettersCreated                   : LongInt                               = 0;

 MixerSchedulesActive                  : LongInt                               = 0;
 MixerSchedulesCreated                 : LongInt                               = 0;

 MixerTitleLength                      : LongInt                               = 0;

 MixerCleanupAt                        : TTimeFixer                            = 0;

 MixerIdle                             : LongInt                               = 0;

 MixerFineTune                         : LongInt                               = 0;

 MixerSamplesInterval                  : LongInt                               = 44100 div 4;

 MixerDumpStatus                       : Boolean                               = False;
 MixerDumpIdle                         : Boolean                               = False;
 MixerDumpTimeCorrection               : Boolean                               = False;

type
 TChannelBuffer = packed array[0..ChannelBufferCapacity - 1] of SmallInt;

 TMixerBuffer = record
  Left: TChannelBuffer;
  Right: TChannelBuffer;
  Samples: Longint;
  Volume: Double;
  Panning: Double;
 end;

 PUnitedMixerBuffer = ^TUnitedMixerBuffer;
 TUnitedMixerBuffer = packed array[0..(ChannelBufferCapacity * 2) - 1] of SmallInt;

 TVirtualUnitedMixerBuffer = record
  Buffer: Pointer;
  UnitedBuffer: PUnitedMixerBuffer;
 end;

 TSocketStream = class(THandleStream)
 public
  Timeout: Longint;
  Accepted: Boolean;
  constructor Create(const AHandle: Longint; const ATimeout: Longint);
  constructor Connect(const AHost: String; const APort: Integer; const ATimeout: Longint = 60);
  constructor Accept(const AHandle: Longint; const ATimeout: Longint);
  {$IFDEF WIN32}
  function Read(var Buffer; Count: Longint): Longint; override;
  function Write(const Buffer; Count: Longint): Longint; override;
  {$ENDIF}
  function Gets(var D: String; const Blocking: Boolean = False): Boolean;
  procedure Puts(S: String);
  function Report(const Code: Integer; var Command, rc: String; const S: String): Boolean;
  procedure Graceful;
  destructor Destroy; override;
 end;

 TNullSocketStream = class(TSocketStream)
 public
  constructor Create;
  function Write(const Buffer; Count: Longint): Longint; override;
 end;

 TStdOutSocketStream = class(TSocketStream)
 public
  constructor Create;
  function Write(const Buffer; Count: Longint): Longint; override;
 end;

 TLogSocketStream = class(TSocketStream)
 private
  Prefix: String;
 public
  constructor Create(const APrefix: String = '');
  function Write(const Buffer; Count: Longint): Longint; override;
 end;

 TDaemonThread = class(TThread)
 public
  procedure Install(const AName: String);
  procedure Deinstall;
 end;

var
 GiantTargets                                    : TRTLCriticalSection;
 GiantSetters                                    : TRTLCriticalSection;
 GiantSources                                    : TRTLCriticalSection;
 GiantSchedules                                  : TRTLCriticalSection;
 GiantFilters                                    : TRTLCriticalSection;
 GiantCounters                                   : TRTLCriticalSection;
 GiantTitle                                      : TRTLCriticalSection;
 GiantLocalTimeAnchor                            : TRTLCriticalSection;

procedure Wait(MSecs: Longint);

function GetSocketPeer(Socket: Longint): String;
function SetupSocket(var aSocket: Integer; var aAddress: TInetSockAddr; const aIP: string; const aPort: Word): Boolean;

function UT: Longint;
function DateTimeToUnixTime(DelphiTime: TDateTime): LongWord;

{$IFNDEF WIN32}
function InstallSignalHandler(sig: Longint; H: SigActionHandler): Boolean;
{$ENDIF}

function Trim(const S: String): String;
procedure SplitToTheWords(const What: String; Words: TStringList);
function Implode(const Divider: String; const Items: TStringList): String;
function URLEncode(const S: String): String;

function CalcValueLinear(const ValueFrom, ValueTo: Double; First, Current, Last: TTimeFixer): Double;

function PrepareVolume(Volume: Double): Double;
function PreparePanning(Panning: Double): Double;

procedure MixerBufferToUnitedBuffer(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
procedure UnitedBufferToMixerBuffer(var UnitedBuffer: TUnitedMixerBuffer; var Buffer: TMixerBuffer);

function preg_match(const Expression, Victim: String; Matches: TStringList = nil; const Modifiers: String = ''): Boolean;
function preg_replace_callback(const Expression, Victim: String; Callback: TRegExprReplaceFunction): String;

procedure DumpCriticalSectionsPendings(Output: TSocketStream);
function GetCriticalSectionsPendings: String;

procedure InitCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
procedure EnterCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
procedure LeaveCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
procedure DoneCriticalSection(var Section: TRTLCriticalSection; const Reason: String);

procedure CreateVirtualUnitedMixerBuffer(var UnitedBuffer: TVirtualUnitedMixerBuffer);
procedure DestroyVirtualUnitedMixerBuffer(var UnitedBuffer: TVirtualUnitedMixerBuffer);

function LocalTimeAnchor: TTimeFixer;
procedure SetLocalTimeAnchor(const ALocalTimeAnchor: TTimeFixer);

implementation

uses 
{$IFDEF WIN32}
     Windows,
{$ENDIF}
     Resolve,
     Daemon;

function GetSocketPeer(Socket: Longint): String;
 var
  Address: TSockAddr;
  AddressSize: Longint;
 begin
  AddressSize:=SizeOf(Address);
  fpGetPeerName(Socket, @Address, @AddressSize);

  Exit(NetAddrToStr(Address.sin_addr));
 end;

function SetupSocket(var aSocket: Integer; var aAddress: TInetSockAddr; const aIP: string; const aPort: Word): Boolean;
 begin
  aSocket:=fpSocket(AF_INET, SOCK_STREAM, 6);

  if aSocket < 0 then
   Exit(False);

  with aAddress do 
   begin
    sin_Family:=AF_INET;
    sin_Port:=htons(aPort);
    sin_Addr.s_addr:=LongInt(StrToNetAddr(aIP));
   end;

  Exit(True);
 end;

// TSocketStream

constructor TSocketStream.Create(const AHandle: Longint; const ATimeout: Longint);
 begin
  inherited Create(AHandle);

  Accepted:=False;
  Timeout:=ATimeout;
 end;

constructor TSocketStream.Connect(const AHost: String; const APort: Integer; const ATimeout: Longint = 60);
 var
{
  HostResolver: THostResolver;
  HostAddr: THostAddr;
}
  Addr: TInetSockAddr;
  Sock, I, IP, BufSize: Longint;
{$IFNDEF WIN32}
  TV: TimeVal;
{$ENDIF}
 begin
  if not ResolveHostname(AHost, IP) then
   begin
    Log('Failed to resolve "' + AHost + '"');
    Fail;
    Exit;
   end;

  {$IFDEF FREEBSD}
  //Addr.Len:=SizeOf(Addr);
  {$ENDIF}
  Addr.sin_Family:=AF_INET;
  Addr.sin_Port:=htons(APort);
  Addr.sin_Addr.s_addr:=IP;

  Sock:=fpSocket(AF_INET, SOCK_STREAM, 0);

  if Sock < 0 then
   begin
    Log('Failed to create socket while connecting to ' + AHost + ':' + IntToStr(APort) + ' (rc ' + IntToStr(Sock) + ')');
    Fail;
    Exit;
   end;

{$IFDEF WIN32}

{$ELSE}

  BufSize:=16 * 1024;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDBUF, @BufSize, SizeOf(BufSize));

  BufSize:=256 * 1024;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVBUF, @BufSize, SizeOf(BufSize));

  TV.tv_sec:=60;
  TV.tv_usec:=0;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_SNDTIMEO, @TV, SizeOf(TV));

  TV.tv_sec:=60;
  TV.tv_usec:=0;
  fpSetSockOpt(Sock, SOL_SOCKET, SO_RCVTIMEO, @TV, SizeOf(TV));

{$ENDIF}

  I:=fpConnect(Sock, @Addr, SizeOf(Addr));

  if I < 0 then
   begin
    Log('Failed to connect to ' + AHost + ':' + IntToStr(APort) + ' (rc ' + IntToStr(I) + ')');
    Fail;
    Exit;
   end;

  inherited Create(Sock);

  Timeout:=ATimeout;
  Accepted:=True;
 end;

constructor TSocketStream.Accept(const AHandle: Longint; const ATimeout: Longint);
 begin
  inherited Create(AHandle);

  Timeout:=ATimeout;
  Accepted:=True;
 end;

{$IFDEF WIN32}
function TSocketStream.Read(var Buffer; Count: Longint): Longint;
 begin
  Result:=fpRecv(Handle, @Buffer, Count, 0);

  if Result = -1 then
   Result:=0;
 end;

function TSocketStream.Write(const Buffer; Count: Longint): Longint;
 begin
  Result:=fpSend(Handle, @Buffer, Count, 0);

  if Result = -1 then
   Result:=0;
 end;
{$ENDIF}

function TSocketStream.Gets(var D: String; const Blocking: Boolean = False): Boolean;
 var
  Anchor, Bytes, Shit: Longint;
  C: Byte absolute Shit;
 begin
  Anchor:=UT + Timeout;

  D:='';

  while UT < Anchor do
   begin
    Bytes:=Read(C, 1);

    //System.Write(Ord(C), ':', Bytes, ' ');

    if (Bytes < 0) or GlobalStop then
     Exit(False);

    if Bytes = 0 then
     if Blocking then
      Exit(False)
     else
      Wait(100)
    else
     if C = 10 then
      Exit(True)
     else
      D:=D + Chr(C);
   end;

  Exit(False);
 end;

procedure TSocketStream.Puts(S: String);
 begin
  S:=S + #13#10;
  Write(S[1], Length(S));
 end;

function TSocketStream.Report(const Code: Integer; var Command, rc: String; const S: String): Boolean;
 begin
  rc:=S;

  if (Code >= 400) and (Length(Command) > 0) then
   Puts(IntToStr(Code) + #9 + S + ' (near "' + Command + '")')
  else
   Puts(IntToStr(Code) + #9 + S);
   
  Exit(False);
 end;

procedure TSocketStream.Graceful;
 begin
  fpShutdown(Longint(Handle), 2);
  fpClose(Longint(Handle));

  Free;
 end;

destructor TSocketStream.Destroy;
 begin
  if Accepted then
   begin
    fpShutdown(Longint(Handle), 2);
    fpClose(Longint(Handle));
   end;

  inherited Destroy;
 end;

// TNullSocketStream

constructor TNullSocketStream.Create;
 begin
  inherited Create(0, 60);
 end;

function TNullSocketStream.Write(const Buffer; Count: Longint): Longint;
 begin
  Exit(0);
 end;

// TStdOutSocketStream

constructor TStdOutSocketStream.Create;
 begin
  inherited Create(0, 60);
 end;

function TStdOutSocketStream.Write(const Buffer; Count: Longint): Longint;
 var
  S: String;
 begin
  if Count > 0 then
   begin
    SetLength(S, Count);

    Move(Buffer, S[1], Count);

    System.Write(S);
   end;

  Exit(0);
 end;

// TLogSocketStream

constructor TLogSocketStream.Create(const APrefix: String = '');
 begin
  inherited Create(0, 60);

  Prefix:=APrefix;
  Accepted:=False;
 end;

function TLogSocketStream.Write(const Buffer; Count: Longint): Longint;
 var
  S: String;
 begin
  if Count > 0 then
   begin
    SetLength(S, Count);

    Move(Buffer, S[1], Count);

    Log(Trim(S), Prefix);
   end;

  Exit(0);
 end;

// TDaemonThread

procedure TDaemonThread.Install(const AName: String);
 begin
  DaemonThreadID:=Longint(Handle);
  DaemonThreadName:=AName;
 end;

procedure TDaemonThread.Deinstall;
 begin
  DaemonThreadID:=0;
  SetLength(DaemonThreadName, 0);
 end;

// The Main Function (ported from Windows 95 :)))

procedure Wait(MSecs: Longint);
{$IFDEF WIN32}
 begin
  Sleep(MSecs);
 end;
{$ELSE}
 var
  A, B: TimeSpec;
 begin
  A.tv_sec:=MSecs div 1000;
  A.tv_nsec:=MSecs mod 1000 * 1000000;

  fpNanoSleep(@A, @B);
 end;
{$ENDIF}

// UnixTime ROCKS

function UT: Longint;
{$IFDEF WIN32}
 begin
  Result:=Round((Now - 25569.0) * 86400);
 end;
{$ELSE}
 begin
  fpTime(Result);
 end;
{$ENDIF}

function DateTimeToUnixTime(DelphiTime: TDateTime): LongWord;
 begin
  Result:=Round((DelphiTime - 25569.0) * 86400);
 end;

// *nix signals

{$IFNDEF WIN32}
function InstallSignalHandler(sig: Longint; H: SigActionHandler): Boolean;
 var
  NA, OA: PSigActionRec;
 begin
  New(NA);
  New(OA);
  NA^.sa_Handler:=H;
  FillChar(NA^.sa_Mask, SizeOf(NA^.sa_Mask), #0);
  NA^.sa_Flags:=SA_RESTART or SA_NOCLDSTOP;
  
  if fpSigAction(sig, NA, OA) <> 0 then
   Result:=False
  else
   Result:=True;

  Dispose(NA);
  Dispose(OA);
 end;
{$ENDIF}

// trimmer

function Trim(const S: String): String;
 begin
  Exit(SysUtils.Trim(S));
 end;

{
 var
  K: Integer;
 begin
  if Length(S) = 0 then 
   Exit('');

  K:=1;

  while (S[K] in [#13, #10, ' ', #9]) and (K <= Length(S)) do Inc(K);

  if K > 0 then
   begin
    S:=Copy(S, K, Length(S) - K + 1);

    K:=Length(S);

    if K > 0 then
     while ((S[K] in [#13, #10, ' ', #9]) and (K > 1)) do Dec(K);
   end;

  Result:=Copy(S, 1, K);
 end;
}

// splitter

procedure SplitToTheWords(const What: String; Words: TStringList);
 var
  S: String;
  K: Integer;
 begin
  Words.Sorted:=True;
  Words.Duplicates:=dupIgnore;

  SplitRegExpr('[ \%\\\.,\"\-\:\;\/\(\)\x80-\xBF\+]+', What, Words);

  for K:=Words.Count - 1 downto 0 do
   begin
    S:=Words[K];

    if (S = '') or ((Length(S) = 1) and (not (S[1] in ['0'..'9']))) then
     Words.Delete(K);
   end;
 end;

// imploder (viva PHP!)

function Implode(const Divider: String; const Items: TStringList): String;
 var
  K: Integer;
 begin
  Result:='';

  for K:=0 to Items.Count - 1 do
   Result:=Result + Items[K] + Divider;

  if Length(Result) > 0 then
   SetLength(Result, Length(Result) - Length(Divider));
 end;

// URL encoder

function URLEncode(const S: String): String;
 var
  K: Integer;
 begin
  Result:='';

  for K:=1 to Length(S) do
   if S[K] in ['a'..'z', 'A'..'Z', '0'..'9'] then
    Result:=Result + S[K]
   else
    Result:=Result + '%' + IntToHex(Ord(S[K]), 2);
 end;

// CalcValueLinear

function CalcValueLinear(const ValueFrom, ValueTo: Double; First, Current, Last: TTimeFixer): Double;
 var
  Difference: TTimeFixer;
  ValueDifference, Step: Double;
 begin
  if Current >= Last then
   Exit(ValueTo);

  if Current <= First then
   Exit(ValueFrom);

  if Last > First then
   Difference:=Last - First
  else
   Difference:=1;

  ValueDifference:=ValueTo - ValueFrom;

  Step:=ValueDifference / Difference;

  Result:=ValueFrom + Step * (Current - First);

  if ValueTo > ValueFrom then
   begin
    if Result < ValueFrom then Exit(ValueFrom);
    if Result > ValueTo then Exit(ValueTo);
   end
  else
   begin
    if Result > ValueFrom then Exit(ValueFrom);
    if Result < ValueTo then Exit(ValueTo);
   end;
 end;

// PrepareVolume

function PrepareVolume(Volume: Double): Double;
 begin
  Exit(Volume);
{
  Exit(100 - Sqrt(100 - Volume) * 10.0);
}
 end;

// PreparePanning

function PreparePanning(Panning: Double): Double;
 begin
  Exit(Panning);
{
  if Panning >= 0.0 then 
   Exit(100 - Sqrt(100 - Panning) * 10.0)
  else 
   Exit(-(100 - Sqrt(100 - Abs(Panning)) * 10.0));
}
 end;

// MixerBufferToUnitedBuffer

procedure MixerBufferToUnitedBuffer(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 var
  K: Longint;
 begin
  for K:=0 to Buffer.Samples - 1 do
   begin
    UnitedBuffer[K * 2]:=Buffer.Left[K];
    UnitedBuffer[K * 2 + 1]:=Buffer.Right[K];
   end;
 end;

// UnitedBufferToMixerBuffer

procedure UnitedBufferToMixerBuffer(var UnitedBuffer: TUnitedMixerBuffer; var Buffer: TMixerBuffer);
 var
  K: Longint;
 begin
  for K:=0 to Buffer.Samples - 1 do
   begin
    Buffer.Left[K]:=UnitedBuffer[K * 2];
    Buffer.Right[K]:=UnitedBuffer[K * 2 + 1];
   end;
 end;

// preg_match

function preg_match(const Expression, Victim: String; Matches: TStringList = nil; const Modifiers: String = ''): Boolean;
 var
  R: TRegExpr;
  K: Integer;
 begin
  if Assigned(Matches) then
   Matches.Clear;

  R:=TRegExpr.Create;

  try
   if Length(Modifiers) > 0 then
    R.ModifierStr:=Modifiers;

   R.Expression:=Expression;

   Result:=R.Exec(Victim);

   if Result and Assigned(Matches) then
    for K:=0 to R.SubExprMatchCount do
     Matches.Add(R.Match[K]);
  finally
   r.Free;
  end;
 end;

// preg_replace_callback

function preg_replace_callback(const Expression, Victim: String; Callback: TRegExprReplaceFunction): String;
 var
  r: TRegExpr;
 begin
  Result:='';
  r:=TRegExpr.Create;

  try
   R.Expression:=Expression;

   Result:=r.ReplaceEx(Victim, Callback);
  finally
   r.Free;
  end;
 end;

// virtual united buffer

procedure CreateVirtualUnitedMixerBuffer(var UnitedBuffer: TVirtualUnitedMixerBuffer);
 begin
  GetMem(UnitedBuffer.Buffer, SizeOf(TUnitedMixerBuffer) + 1024);

  UnitedBuffer.UnitedBuffer:=UnitedBuffer.Buffer;

  while LongWord(UnitedBuffer.UnitedBuffer) mod 16 = 0 do
   Inc(LongWord(UnitedBuffer.UnitedBuffer));
 end;

procedure DestroyVirtualUnitedMixerBuffer(var UnitedBuffer: TVirtualUnitedMixerBuffer);
 begin
  FreeMem(UnitedBuffer.Buffer);
 end;

// local anchors

function LocalTimeAnchor: TTimeFixer;
 begin
  EnterCriticalSection(GiantLocalTimeAnchor, 'LocalTimeAnchor');

  Result:=LocalAnchor;

  LeaveCriticalSection(GiantLocalTimeAnchor, 'LocalTimeAnchor');
 end;

procedure SetLocalTimeAnchor(const ALocalTimeAnchor: TTimeFixer);
 begin
  EnterCriticalSection(GiantLocalTimeAnchor, 'SetLocalTimeAnchor');

  LocalAnchor:=ALocalTimeAnchor;

  LeaveCriticalSection(GiantLocalTimeAnchor, 'SetLocalTimeAnchor');
 end;

// critical sections

function CSDebuggable(const Reason: String): Boolean;
 begin
  if Reason = 'TMixerScheduler.Execute' then Exit(False)
  else Exit(True);
 end;

{$IFDEF CSTRACE}
var
 CS: TStringList;
 CSLock: TRTLCriticalSection;
{$ENDIF}

procedure DumpCriticalSectionsPendings(Output: TSocketStream);
{$IFDEF CSTRACE}
 var
  rc: String;
  K: Integer;
 begin
  System.EnterCriticalSection(CSLock);

  rc:='';

  for K:=0 to CS.Count - 1 do
   Output.Report(0, rc, rc, CS[K]);

  System.LeaveCriticalSection(CSLock);
 end;
{$ELSE}
 begin
 end;
{$ENDIF}

function GetCriticalSectionsPendings: String;
{$IFDEF CSTRACE}
 var
  S: String;
  K: Integer;
 begin
  System.EnterCriticalSection(CSLock);

  S:='';

  for K:=0 to CS.Count - 1 do
   S:=S + IntToStr(K) + ': ' + CS[K] + #13#10;

  System.LeaveCriticalSection(CSLock);

  Result:=S;
 end;
{$ELSE}
 begin
  Result:='CSTRACE is not enabled';
 end;
{$ENDIF}

procedure InitCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
 begin
  System.InitCriticalSection(Section); 
 end;

procedure EnterCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
 begin
  {$IFDEF CSTRACE}
  System.EnterCriticalSection(CSLock);

  CS.Add(Reason + '@' + Format('0x%p', [@Section]));

  System.LeaveCriticalSection(CSLock);
  {$ENDIF}

  System.EnterCriticalSection(Section); 
 end;

procedure LeaveCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
 {$IFDEF CSTRACE}
 var
  I: Integer;
 {$ENDIF}
 begin
  System.LeaveCriticalSection(Section); 

  {$IFDEF CSTRACE}
  System.EnterCriticalSection(CSLock);

  I:=CS.IndexOf(Reason + '@' + Format('0x%p', [@Section]));

  if I <> -1 then
   CS.Delete(I);

  System.LeaveCriticalSection(CSLock);
  {$ENDIF}
 end;

procedure DoneCriticalSection(var Section: TRTLCriticalSection; const Reason: String);
 begin
  System.DoneCriticalSection(Section); 
 end;

initialization
 {$IFDEF CSTRACE}
 CS:=TStringList.Create;
 System.InitCriticalSection(CSLock);
 {$ENDIF}

finalization
 {$IFDEF CSTRACE}
 System.DoneCriticalSection(CSLock);
 CS.Free;
 {$ENDIF}

end.