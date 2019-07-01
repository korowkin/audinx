// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit MixerTargets;

interface

uses Classes, SysUtils,
     Common, Tokenizer, Lame, AAC, TimeFixer, Daemon, SZCodeBaseX;

type
 TMixerTargetBuffer = packed array of SmallInt;
 PMixerTargetBuffer = ^TMixerTargetBuffer;

 TMixerTransport = class
 public
  Failures: Longint;
  LastFailure: TTimeFixer;
  ID: String;
  constructor Create(var Command, rc: String; Output: TSocketStream);
  procedure Write(var Buffer; Size: Longint); virtual;
  procedure SetTitle(const S: String); virtual;
  procedure Disconnect; virtual;
  destructor Destroy; override;
 end;

 TMixerTransportIcacastUpdater = class(TDaemonThread)
 public
  ID, Host, Port, Mountpoint, Title, Password: String;
  constructor Create(const AID, AHost, APort, AMountpoint, ATitle, APassword: String);
  procedure Execute; override;
  destructor Destroy; override;
 end;

 TMixerTransportIcecast = class(TMixerTransport)
 public
  Host, Port, Mountpoint, Title, Password, URL, Genre, Bitrate, PublicStream, Description, ContentType: String;
  ConnectTimeout, ReconnectTimeout: Longint;
  Updater: TMixerTransportIcacastUpdater;
  Buffer: PByte;
  BufferFilled, BufferSize: LongInt;
  Stream: TSocketStream;
  constructor Create(var Command, rc: String; Output: TSocketStream);
  procedure Connect;
  procedure Flush;
  procedure Write(var Buffer; Size: Longint); override;
  procedure SetTitle(const S: String); override;
  procedure Disconnect; override;
  destructor Destroy; override;
 end;

 TMixerTransportFile = class(TMixerTransport)
 public
  FileName: String;
  Stream: TStream;
  RetryTimeout: Longint;
  constructor Create(var Command, rc: String; Output: TSocketStream);
  procedure Connect;
  procedure Write(var Buffer; Size: Longint); override;
  destructor Destroy; override;
 end;

 TMixerEncoder = class
 public
  Transport: TMixerTransport;
  constructor Create(var Command, rc: String; Output: TSocketStream);
  function Encode(var Buffer: TMixerBuffer): Longint; virtual; abstract;
  destructor Destroy; override;
 end;

 TMixerEncoderMP3 = class(TMixerEncoder)
 public
  L: PLame_global_flags;
  OutBufferSize, Bitrate, Ticks: Longint;
  OutBuffer: PByte;
  InBufferLeft, 
  InBufferRight: TMixerTargetBuffer;
  constructor Create(var Command, rc: String; Output: TSocketStream);
  procedure LameSetup; virtual;
  function Encode(var Buffer: TMixerBuffer): Longint; override;
  procedure Flush(Buffer: PByte; Bytes: Longint);
  destructor Destroy; override;
 end;

 TMixerEncoderOGG = class(TMixerEncoderMP3)
 public
  procedure LameSetup; override;
 end;

 TMixerEncoderAAC = class(TMixerEncoder)
 public
  AAC: HANDLE_AACENCODER;
  InBuffer: array of SmallInt;
  OutBuffer: PByte;
  OutBufferSize: Longint;
  Bitrate: Longint;
  Ticks: Longint;
  constructor Create(var Command, rc: String; Output: TSocketStream);
  function Encode(var Buffer: TMixerBuffer): Longint; override;
  procedure Flush(Buffer: PByte; Bytes: Longint);
  destructor Destroy; override;
 end;

 TMixerTarget = class(TDaemonThread)
 private
  Lock: TRTLCriticalSection;
  Queue: packed array of TMixerBuffer;
 public
  ID: String;
  Encoder: TMixerEncoder;
  Transport: TMixerTransport;
  Waiting, Ready: Boolean;
  BytesWritten: Int64;
  BytesWrittenLastSec, MaxQueueSize: Longint;
  BytesWrittenStamp: TTimeFixer;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  function GetQueueSize: Longint; virtual;
  procedure Encode(var Buffer: TMixerBuffer);
  procedure Execute; override;
  function ExplainStatus: String; virtual;
  procedure SetTitle(const S: String); virtual;
  destructor Destroy; override;
 end;

implementation

// TMixerTransport

constructor TMixerTransport.Create(var Command, rc: String; Output: TSocketStream);
 begin
  inherited Create;

  Failures:=0;
  LastFailure:=0;
 end;

procedure TMixerTransport.Write(var Buffer; Size: Longint);
 begin
 end;

procedure TMixerTransport.SetTitle(const S: String);
 begin
 end;

procedure TMixerTransport.Disconnect;
 begin
 end;

destructor TMixerTransport.Destroy;
 begin
  inherited Destroy;
 end;

// TMixerTransportIcacastUpdater

constructor TMixerTransportIcacastUpdater.Create(const AID, AHost, APort, AMountpoint, ATitle, APassword: String);
 begin
  inherited Create(True);

  ID:=AID;
  Host:=AHost;
  Port:=APort;
  Mountpoint:=AMountpoint;
  Title:=ATitle;
  Password:=APassword;

  Resume;
 end;

procedure TMixerTransportIcacastUpdater.Execute;
 var
  Stream: TSocketStream;
  Bytes: Longint;
  S: String;
 begin
  Install('target:' + ID + ':metaupdate');

  try
   Stream:=TSocketStream.Connect(Host, StrToIntDef(Port, 8000), 15);

   if Stream <> nil then
    begin
     S:='GET /admin/metadata?mount=' + Mountpoint + '&mode=updinfo&song=' + URLEncode(Title) + '&charset=UTF-8 HTTP/1.0' + #13#10 +
        'Authorization: Basic ' + SZFullEncodeBase64('source:' + Password) + #13#10 +
        'User-Agent: ' + AudinxTitle + #13#10 +
        #13#10;

     try
      Bytes:=Stream.Write(S[1], Length(S));
     except
      Bytes:=0;
     end;

     if Bytes > 0 then
      begin
       if (not Stream.Gets(S, True)) or (Trim(S) <> 'HTTP/1.0 200 OK') then
        Log('failure, reply was "' + Trim(S) + '" (while updating metadata)');
      end
     else
      Log('failure (while updating metadata)');

     Stream.Free;
    end
   else
    Log('connect failure (while updating metadata)');
  except
   on E: Exception do Log('TMixerTransportIcacastUpdater.Execute: exception ' + E.Message);
  end;

  Deinstall;

  SetLength(ID, 0);
  SetLength(Host, 0);
  SetLength(Port, 0);
  SetLength(Mountpoint, 0);
  SetLength(Title, 0);
  SetLength(Password, 0);
 end;

destructor TMixerTransportIcacastUpdater.Destroy;
 begin
  Terminate;
  WaitFor;

  SetLength(ID, 0);
  SetLength(Host, 0);
  SetLength(Port, 0);
  SetLength(Mountpoint, 0);
  SetLength(Title, 0);
  SetLength(Password, 0);

  inherited Destroy;
 end;

// TMixerTransportIcecast

constructor TMixerTransportIcecast.Create(var Command, rc: String; Output: TSocketStream);
 function Fetch(var S: String; const TokenName: String): Boolean;
  begin
   if not TokenizeToken(Command, TokenName) then
    begin
     Output.Report(400, Command, rc, TokenName + ' expected');
     Exit(True);
    end;

   if not TokenizeValue(Command, S) then 
    begin
     Output.Report(400, Command, rc, TokenName + ' "value" expected');
     Exit(True);
    end;

   Exit(False);
  end;
 begin
  inherited Create(Command, rc, Output);

  if Fetch(Host, 'HOST') or
     Fetch(Port, 'PORT') or
     Fetch(Mountpoint, 'MOUNTPOINT') or
     Fetch(ContentType, 'CONTENTTYPE') or
     Fetch(Title, 'TITLE') or
     Fetch(Password, 'PASSWORD') or
     Fetch(URL, 'URL') or
     Fetch(Genre, 'GENRE') or
     Fetch(Bitrate, 'BITRATE') or
     Fetch(PublicStream, 'PUBLIC') or
     Fetch(Description, 'DESCRIPTION') then
   begin
    Fail;
    Exit;
   end;

  BufferSize:=32 * 1024;
  BufferFilled:=0;
  GetMem(Buffer, BufferSize);

  Updater:=nil;
  Stream:=nil;

  ID:='';

  ConnectTimeout:=30;
  ReconnectTimeout:=15;

  Connect;
 end;

procedure TMixerTransportIcecast.Connect;
 var
  S, rc: String;
  Bytes: Longint;
 const
  IcecastCRLF = #13#10;
 begin
  if (Stream <> nil) or ((Failures > 0) and (TimeAnchor - LastFailure <= ReconnectTimeout * 1000)) then
   Exit;

  Stream:=TSocketStream.Connect(Host, StrToIntDef(Port, 8000), ConnectTimeout);

  if Stream = nil then
   begin
    Inc(Failures);
    LastFailure:=TimeAnchor;

    Exit;
   end;

  S:='SOURCE ' + Mountpoint + ' ICE/1.0' + IcecastCRLF +
     'Authorization: Basic ' + SZFullEncodeBase64('source:' + Password) + IcecastCRLF +
     'Content-Type: ' + ContentType + IcecastCRLF +
     'ice-name: ' + Title + IcecastCRLF +
     'ice-url: ' + URL + IcecastCRLF +
     'ice-genre: ' + Genre + IcecastCRLF +
     'ice-bitrate: ' + Bitrate + IcecastCRLF +
     'ice-public: ' + PublicStream + IcecastCRLF +
     'ice-description: ' + Description + IcecastCRLF +
     IcecastCRLF;

  rc:='';

  try
   Bytes:=Stream.Write(S[1], Length(S));
  except
   on E: Exception do
    begin
     Bytes:=0;
     rc:=E.Message;
    end;
  end;

  if Bytes <= 0 then
   begin
    Log('connect failed, going to reconnect (' + rc + ', ' + IntToStr(Bytes) + ')');

    try
     Stream.Free;
    except
    end;

    Stream:=nil;

    Inc(Failures);
    LastFailure:=TimeAnchor;

    Exit;
   end;

  if (not Stream.Gets(S, True)) or (Trim(S) <> 'HTTP/1.0 200 OK') then
   begin
    Stream.Free;
    Stream:=nil;

    Inc(Failures);
    LastFailure:=TimeAnchor;

    Log('connect failure #' + IntToStr(Failures) + ', reply was "' + Trim(S) + '" (password was "' + Password + '")');
   end
  else
   begin
    Failures:=0;
   end;

  BufferFilled:=0;
 end;

procedure TMixerTransportIcecast.Flush;
 var
  Size, Bytes: LongInt;
  RealBuffer: Pointer;
 begin
  if BufferFilled > 0 then
   begin
    //Log('flushing ' + IntToStr(BufferFilled) + ' bytes from buffer');

    Bytes:=0;
    RealBuffer:=Buffer;
    Size:=BufferFilled;

    //DebugLog('TMixerTransportIcecast.Write: ' + IntToStr(Size) + ' bytes to write');

    while Size > 0 do
     begin
      try
       if Stream <> nil then
        Bytes:=Stream.Write(RealBuffer^, Size)
       else
        Bytes:=0;
      except
       on E: Exception do
        begin
         Log('TMixerTransportIcecast.Flush: Stream.Write exception ' + E.Message);
         Bytes:=-1;
        end;
      end;

      if Bytes <= 0 then
       Break
      else
       begin
        Inc(RealBuffer, Bytes);
        Dec(Size, Bytes);
       end;
     end;

    if Bytes <= 0 then
     begin
      Log('write failed, going to reconnect');

      Disconnect;
     end;

    BufferFilled:=0;
   end;
 end;

procedure TMixerTransportIcecast.Write(var Buffer; Size: Longint);
 var
  PutTo: Pointer;
 begin
  if (Stream = nil) and (TimeAnchor - LastFailure > (ReconnectTimeout * 1000)) then
   begin
    Log('reconnecting...');
    Connect;
   end;

  if Stream <> nil then
   begin
    if (BufferFilled + Size) >= BufferSize then
     Flush;

    if Stream <> nil then
     begin
      PutTo:=Self.Buffer;
      Inc(PutTo, BufferFilled);

      Move(Buffer, PutTo^, Size);

      Inc(BufferFilled, Size);

      //Log('TMixerTransportIcecast.Write: written ' + IntToStr(Size) + ' to buffer, filled=' + IntToStr(BufferFilled));
     end;
   end
  else
   BufferFilled:=0;
 end;

procedure TMixerTransportIcecast.SetTitle(const S: String);
 begin
  if Updater <> nil then
   Updater.Free;

  Updater:=TMixerTransportIcacastUpdater.Create(ID, Host, Port, Mountpoint, S, Password);
 end;

procedure TMixerTransportIcecast.Disconnect;
 begin
  if Stream <> nil then
   begin
    try
     Stream.Free;
    except
     on E: Exception do
      Log('TMixerTransportIcecast.Disconnect: exception ' + E.Message);
    end;

    Stream:=nil;
   end;

  Log('disconnected');

  Inc(Failures);
  LastFailure:=TimeAnchor;
 end;

destructor TMixerTransportIcecast.Destroy;
 begin
  if Stream <> nil then
   try
    Stream.Free;
   except
    on E: Exception do
     Log('TMixerTransportIcecast.Destroy: exception on Stream.Free ' + E.Message);
   end;

  if Updater <> nil then
   try
    Updater.Free;
   except
    on E: Exception do
     Log('TMixerTransportIcecast.Destroy: exception on Updater.Free ' + E.Message);
   end;

  FreeMem(Buffer, BufferSize);

  inherited;
 end;

// TMixerTransportFile

constructor TMixerTransportFile.Create(var Command, rc: String; Output: TSocketStream);
 function Fetch(var S: String; const TokenName: String): Boolean;
  begin
   if not TokenizeToken(Command, TokenName) then
    begin
     Output.Report(400, Command, rc, TokenName + ' expected');
     Exit(True);
    end;

   if not TokenizeValue(Command, S) then 
    begin
     Output.Report(400, Command, rc, TokenName + ' "value" expected');
     Exit(True);
    end;

   Exit(False);
  end;
 begin
  inherited;

  if Fetch(FileName, 'FILENAME') then
   begin
    Fail;
    Exit;
   end;

  RetryTimeout:=15;

  Connect;
 end;

procedure TMixerTransportFile.Connect;
 begin
  if Stream <> nil then
   Exit;

  try
   Stream:=TFileStream.Create(FileName, fmOpenWrite or fmShareDenyNone);
  except
   Stream:=nil;
  end;

  if Stream = nil then
   begin
    try
     Stream:=TFileStream.Create(FileName, fmCreate);
    except
     Stream:=nil;
    end;

    if Stream = nil then
     begin
      Inc(Failures);
      LastFailure:=TimeAnchor;

      Exit;
     end;
   end;

  Stream.Position:=Stream.Size;
 end;

procedure TMixerTransportFile.Write(var Buffer; Size: Longint);
 var
  Bytes: Longint;
 begin
  if (Stream = nil) and (TimeAnchor - LastFailure > RetryTimeout) then
   Connect;

  if Stream <> nil then
   begin
    try
     Bytes:=Stream.Write(Buffer, Size);
    except
     Bytes:=-1;
    end;

    if Bytes < 0 then
     begin
      Stream.Free;
      Stream:=nil;

      Inc(Failures);
      LastFailure:=TimeAnchor;
     end;
   end;
 end;

destructor TMixerTransportFile.Destroy;
 begin
  if Stream <> nil then
   try
    Stream.Free;
   except
   end;

  inherited;
 end;

// TMixerEncoder

constructor TMixerEncoder.Create(var Command, rc: String; Output: TSocketStream);
 begin
  inherited Create;
 end;

destructor TMixerEncoder.Destroy; 
 begin
  inherited;
 end;

// TMixerEncoderMP3

constructor TMixerEncoderMP3.Create(var Command, rc: String; Output: TSocketStream);
 var
  Value: String;
  Quality: Integer;
 begin
  inherited Create(Command, rc, Output);

  L:=0;
  Ticks:=0;
  OutBuffer:=nil;

  // BITRATE "256"

  if not TokenizeToken(Command, 'BITRATE') then
   begin
    Output.Report(400, Command, rc, 'BITRATE expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Value) then 
   begin
    Output.Report(400, Command, rc, 'BITRATE "256" expected');
    Destroy;
    Fail;
   end;

  Bitrate:=StrToIntDef(Value, 0);

  if Bitrate < 32 then
   begin
    Output.Report(400, Command, rc, 'Too low bitrate specified');
    Destroy;
    Fail;
   end;

  Bitrate:=StrToIntDef(Value, 0);

  // QUALITY "2"

  if not TokenizeToken(Command, 'QUALITY') then
   begin
    Output.Report(400, Command, rc, 'QUALITY expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Value) then 
   begin
    Output.Report(400, Command, rc, 'QUALITY "2" expected');
    Destroy;
    Fail;
   end;

  Quality:=StrToIntDef(Value, 2);

  // fire!

  L:=lame_init;

  LameSetup;

  lame_set_in_samplerate(L, 44100);
  lame_set_num_channels(L, 2);
  lame_set_brate(L, Bitrate);
  lame_set_out_samplerate(L, 44100);
  lame_set_original(L, 0);
  lame_set_mode(L, 1); // 0,1,2,3 = stereo, jstereo, dual channel (not supported), mono
  //lame_set_padding_type(L, 0); // 0=pad no frames  1=pad all frames 2=adjust padding(default)
  lame_set_error_protection(L, 1);
  lame_set_quality(L, Quality);

  lame_init_params(L);

  OutBufferSize:=1024 * 1024;
  GetMem(OutBuffer, OutBufferSize);

  SetLength(InBufferLeft, 0);
  SetLength(InBufferRight, 0);

  Transport:=nil;
 end;

procedure TMixerEncoderMP3.LameSetup;
 begin
 end;

function TMixerEncoderMP3.Encode(var Buffer: TMixerBuffer): Longint;
 var
  Bytes, Anchor: LongInt;
 begin
  if (L = 0) or (OutBuffer = nil) or (Buffer.Samples < 1) then
   Exit(0);

  Anchor:=Length(InBufferLeft);

  SetLength(InBufferLeft, Anchor + Buffer.Samples);
  Move(Buffer.Left, InBufferLeft[Anchor], Buffer.Samples * SizeOf(SmallInt));

  SetLength(InBufferRight, Anchor + Buffer.Samples);
  Move(Buffer.Right, InBufferRight[Anchor], Buffer.Samples * SizeOf(SmallInt));

  //Log('TMixerEncoderMP3.Encode now ' + IntToStr(Length(InBufferLeft)) + ' at anchor ' + IntToStr(Anchor) + ', samples=' + IntToStr(Buffer.Samples));

  if Length(InBufferLeft) >= 11025 then
   begin
    Bytes:=lame_encode_buffer(L, @InBufferLeft[0], @InBufferRight[0], Length(InBufferLeft), OutBuffer, OutBufferSize);

    if Bytes > 0 then
     Flush(OutBuffer, Bytes)
    else
     Log('TMixerEncoderMP3.Encode: lame_encode_buffer failed with rc=' + IntToStr(Bytes));

    SetLength(InBufferLeft, 0);
    SetLength(InBufferRight, 0);

    Exit(Bytes);
   end
  else
   Exit(0);
 end;

procedure TMixerEncoderMP3.Flush(Buffer: PByte; Bytes: Longint);
 begin
  if (Transport <> nil) and (L <> 0) and (OutBuffer <> nil) then
   Transport.Write(OutBuffer^, Bytes);
 end;

destructor TMixerEncoderMP3.Destroy;
 var
  Bytes: Longint;
 begin
  if (L <> 0) and (OutBuffer <> nil) then
   begin
    Bytes:=lame_encode_flush(L, OutBuffer, OutBufferSize);

    if Bytes > 0 then
     Flush(OutBuffer, Bytes);
   end;

  if L <> 0 then
   lame_close(L);

  if OutBuffer <> nil then
   FreeMem(OutBuffer);

  inherited Destroy;
 end;

// TMixerEncoderOGG

procedure TMixerEncoderOGG.LameSetup;
 begin
  lame_set_ogg(L, 1);

  DebugLog('TMixerEncoderOGG.LameSetup: lame_set_ogg(1)');
 end;

// TMixerEncoderAAC

constructor TMixerEncoderAAC.Create(var Command, rc: String; Output: TSocketStream);
 var
  Value: String;
  Quality: Integer;
  info: AACENC_InfoStruct;
  arc: UINT;
 begin
  inherited Create(Command, rc, Output);

  AAC:=nil;
  Ticks:=0;
  SetLength(InBuffer, 0);
  OutBuffer:=nil;

  // BITRATE "256"

  if not TokenizeToken(Command, 'BITRATE') then
   begin
    Output.Report(400, Command, rc, 'BITRATE expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Value) then 
   begin
    Output.Report(400, Command, rc, 'BITRATE "256" expected');
    Destroy;
    Fail;
   end;

  Bitrate:=StrToIntDef(Value, 0);

  if Bitrate < 8 then
   begin
    Output.Report(400, Command, rc, 'Too low bitrate specified');
    Destroy;
    Fail;
   end;

  Bitrate:=StrToIntDef(Value, 0);

  // QUALITY "2"

  if not TokenizeToken(Command, 'QUALITY') then
   begin
    Output.Report(400, Command, rc, 'QUALITY expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Value) then 
   begin
    Output.Report(400, Command, rc, 'QUALITY "29" expected');
    Destroy; 
    Fail;
   end;

  Quality:=StrToIntDef(Value, 29);

  // fire!

  arc:=aacEncOpen(@AAC, 0, 2);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to init AAC encoder, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;

  arc:=aacEncoder_SetParam(AAC, AACENC_AOT, Quality);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to set AACENC_AOT, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;

  arc:=aacEncoder_SetParam(AAC, AACENC_BITRATE, Bitrate * 1000);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to set AACENC_BITRATE, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;

  arc:=aacEncoder_SetParam(AAC, AACENC_SAMPLERATE, 44100);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to set AACENC_SAMPLERATE, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;

  arc:=aacEncoder_SetParam(AAC, AACENC_CHANNELMODE, 2);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to set AACENC_CHANNELMODE, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;
{
  arc:=aacEncoder_SetParam(AAC, AACENC_AFTERBURNER, 1);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to set AACENC_AFTERBURNER, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;

  arc:=aacEncoder_SetParam(AAC, AACENC_METADATA_MODE, 0);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to set AACENC_METADATA_MODE, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;
}
  arc:=aacEncEncode(AAC, nil, nil, nil, nil);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to initialize AAC encoder, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;

  FillChar(info, 0, SizeOf(info));

  arc:=aacEncInfo(AAC, @info);

  if arc <> AACENC_OK then
   begin
    Output.Report(400, Command, rc, 'Failed to get AAC encoder info, rc=' + IntToStr(arc));
    Destroy; 
    Fail;
   end;
{
  Log('AAC encoder info: ' +
      'maxOutBufBytes=' + IntToStr(info.maxOutBufBytes) + ', ' +
      'maxAncBytes=' + IntToStr(info.maxAncBytes) + ', ' +
      'inBufFillLevel=' + IntToStr(info.inBufFillLevel) + ', ' +
      'inputChannels=' + IntToStr(info.inputChannels) + ', ' +
      'frameLength=' + IntToStr(info.frameLength) + ', ' +
      'encoderDelay=' + IntToStr(info.encoderDelay));
}
  OutBufferSize:=128 * 1024;

  if info.maxOutBufBytes * 2 > OutBufferSize then
   OutBufferSize:=info.maxOutBufBytes * 2;

  GetMem(OutBuffer, OutBufferSize);

  Transport:=nil;
 end;

function TMixerEncoderAAC.Encode(var Buffer: TMixerBuffer): Longint;
 var
  InBufferPointer: Pointer;
  InBufferIdentifier,
  InBufferSize,
  InBufferElementSize: INT;
  OutBufferIdentifier,
  OutBufferElementSize: UINT;
  rc, K, Bytes: Longint;
  in_buf, out_buf: AACENC_BufDesc;
  in_args: AACENC_InArgs;
  out_args: AACENC_OutArgs;
 begin
  if (AAC = nil) or (OutBuffer = nil) or (Buffer.Samples < 1) then
   Exit(0);

  Bytes:=0;

  // 1. add buffer to tail

  SetLength(InBuffer, Buffer.Samples * 4);

  for K:=0 to Buffer.Samples - 1 do
   begin
    InBuffer[K * 2]:=Buffer.Left[K];
    InBuffer[K * 2 + 1]:=Buffer.Right[K];
   end;

  // 2. encode buffer

  InBufferPointer:=@InBuffer[0];
  InBufferSize:=Length(InBuffer);

  while InBufferSize > 0 do
   begin
    FillChar(in_buf, SizeOf(in_buf), 0);
    FillChar(out_buf, SizeOf(out_buf), 0);
    FillChar(in_args, SizeOf(in_args), 0);
    FillChar(out_args, SizeOf(out_args), 0);

    InBufferIdentifier:=IN_AUDIO_DATA;
    InBufferElementSize:=2;

    in_args.numInSamples:=InBufferSize div 2;

    in_buf.numBufs:=1;
    in_buf.bufs:=@InBufferPointer;
    in_buf.bufferIdentifiers:=@InBufferIdentifier;
    in_buf.bufSizes:=@InBufferSize;
    in_buf.bufElSizes:=@InBufferElementSize;

    OutBufferIdentifier:=OUT_BITSTREAM_DATA;
    OutBufferElementSize:=1;

    out_buf.numBufs:=1;
    out_buf.bufs:=@OutBuffer;
    out_buf.bufferIdentifiers:=@OutBufferIdentifier;
    out_buf.bufSizes:=@OutBufferSize;
    out_buf.bufElSizes:=@OutBufferElementSize;

    rc:=aacEncEncode(AAC, @in_buf, @out_buf, @in_args, @out_args);

    K:=out_args.numInSamples * 2;

    Inc(InBufferPointer, K);
    Dec(InBufferSize, K);
{
    Log('AAC bytes=' + IntToStr(out_args.numOutBytes) + ', samples=' + IntToStr(out_args.numInSamples) + ', buffersize=' + IntToStr(InBufferSize) + ', bytes=' + IntToStr(Bytes) + ', K=' + IntToStr(K));
}
    if rc <> AACENC_OK then
     begin
      Log('aacEncEncode failed to encode, rc=' + IntToStr(rc));
      Exit(0);
     end;

    if out_args.numOutBytes > 0 then
     begin
      Flush(OutBuffer, out_args.numOutBytes);

      Bytes:=Bytes + out_args.numOutBytes;
     end;
   end;

  SetLength(InBuffer, 0);

  Exit(Bytes);
 end;

procedure TMixerEncoderAAC.Flush(Buffer: PByte; Bytes: Longint);
 begin
  if (Transport <> nil) and (AAC <> nil) and (OutBuffer <> nil) then
   Transport.Write(OutBuffer^, Bytes);
 end;

destructor TMixerEncoderAAC.Destroy;
 begin
  if AAC <> nil then
   aacEncClose(@AAC);

  if OutBuffer <> nil then
   FreeMem(OutBuffer);

  SetLength(InBuffer, 0);

  inherited Destroy;
 end;

// TMixerTarget

constructor TMixerTarget.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Value: String;
 begin
  inherited Create(True);

  ID:=AID;

  // ENCODER "MP3" ...

  if not TokenizeToken(Command, 'ENCODER') then
   begin
    Output.Report(400, Command, rc, 'ENCODER expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Value) then 
   begin
    Output.Report(400, Command, rc, 'ENCODER "MP3" expected');
    Destroy;
    Fail;
   end;

  if AnsiUpperCase(Value) = 'MP3' then Encoder:=TMixerEncoderMP3.Create(Command, rc, Output)
  else if AnsiUpperCase(Value) = 'OGG' then Encoder:=TMixerEncoderOGG.Create(Command, rc, Output)
  else if AnsiUpperCase(Value) = 'AAC' then Encoder:=TMixerEncoderAAC.Create(Command, rc, Output)
  else
   begin
    Output.Report(400, Command, rc, 'ENCODER "MP3/OGG/AAC" expected, "' + Value + '" got');
    Destroy;
    Fail;
   end;

  if Encoder = nil then 
   begin
    Destroy;
    Fail;
   end;

  // TRANSPORT "ICECAST" ...

  if not TokenizeToken(Command, 'TRANSPORT') then
   begin
    Output.Report(400, Command, rc, 'TRANSPORT expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Value) then 
   begin
    Output.Report(400, Command, rc, 'TRANSPORT "ICECAST/FILE" expected');
    Destroy;
    Fail;
   end;

  Value:=AnsiUpperCase(Value);

  if Value = 'ICECAST' then
   Transport:=TMixerTransportIcecast.Create(Command, rc, Output)
  else if Value = 'FILE' then
   Transport:=TMixerTransportFile.Create(Command, rc, Output)
  else
   begin
    Output.Report(400, Command, rc, 'TRANSPORT "ICECAST/FILE" expected, "' + Value + '" got');
    Destroy;
    Fail;
   end;

  if Transport = nil then 
   begin
    Destroy;
    Fail;
   end;

  Transport.ID:=ID;

  Encoder.Transport:=Transport;

  InitCriticalSection(Lock, 'TMixerTarget.Create');

  SetLength(Queue, 0);

  BytesWritten:=0;
  BytesWrittenLastSec:=0;
  BytesWrittenStamp:=0;

  //Priority:=tpHigher;

  Waiting:=False;
  Ready:=True;

  MaxQueueSize:=500;

  Resume;
 end;

function TMixerTarget.GetQueueSize: Longint;
 begin
  EnterCriticalSection(Lock, 'TMixerTarget.GetQueueSize');

  Result:=Length(Queue);

  LeaveCriticalSection(Lock, 'TMixerTarget.GetQueueSize');
 end;

procedure TMixerTarget.Encode(var Buffer: TMixerBuffer);
 begin
  if Terminated then
   Exit;

  if GetQueueSize >= MaxQueueSize then
   begin
    Log('queue overflow on ' + ID);

    EnterCriticalSection(Lock, 'TMixerTarget.Encode');

    SetLength(Queue, 0);

    LeaveCriticalSection(Lock, 'TMixerTarget.Encode');
   end;

  EnterCriticalSection(Lock, 'TMixerTarget.Encode');

  SetLength(Queue, Length(Queue) + 1);
  Queue[Length(Queue) - 1]:=Buffer;

  LeaveCriticalSection(Lock, 'TMixerTarget.Encode');
 end;

procedure TMixerTarget.Execute;
 var
  Buffer: TMixerBuffer;
  Bytes, K: Longint;
 begin
  Install('target:' + ID);

  while True do
   begin
    if Terminated then
     Break;

    // fetching buffer from queue

    EnterCriticalSection(Lock, 'TMixerTarget.Execute');

    K:=Length(Queue);

    if K > 0 then
     begin
      Buffer:=Queue[0];

      for K:=1 to Length(Queue) - 1 do
       Queue[K - 1]:=Queue[K];

      SetLength(Queue, Length(Queue) - 1);

      K:=Length(Queue);
     end
    else
     Buffer.Samples:=0;

    LeaveCriticalSection(Lock, 'TMixerTarget.Execute');

    if K >= MaxQueueSize then
     if Ready then
      begin
       Ready:=False;
       Log('output queue overflow, let''s wait a bit');
      end
     else
    else
     if not Ready then
      begin
       Ready:=True;
       Log('output queue good now, let''s go');
      end;

    // writing buffer

    if Buffer.Samples > 0 then
     begin
      Bytes:=0;

      if Encoder <> nil then
       Bytes:=Encoder.Encode(Buffer)
      else
       Break;

      BytesWritten:=BytesWritten + Bytes;
      BytesWrittenLastSec:=BytesWrittenLastSec + Bytes;

      if (TimeAnchor - BytesWrittenStamp) >= 1000 then
       begin
        BytesWrittenStamp:=TimeAnchor;
        BytesWrittenLastSec:=0;
       end;
     end
    else
     Wait(100);
   end;

  Deinstall;
 end;

function TMixerTarget.ExplainStatus: String;
 begin
  Result:=', lastsecwritten=' + IntToStr(BytesWrittenLastSec);

  if Transport <> nil then
   begin
    Result:=Result + ', failures=' + IntToStr(Transport.Failures);

    if Transport.LastFailure > 0 then
     Result:=Result + ', lastfailure=' + FormatTimeAnchorDate(Transport.LastFailure);
   end;

  if Waiting then
   Result:=Result + ', waiting';

  Result:=Result + ', queue=' + IntToStr(GetQueueSize);
 end;

procedure TMixerTarget.SetTitle(const S: String);
 begin
  if Transport <> nil then
   Transport.SetTitle(S);
 end;

destructor TMixerTarget.Destroy;
 begin
  if (not Suspended) and (not Terminated) then
   begin
    Terminate;
    WaitFor;
   end;

  SetLength(Queue, 0);

  if Encoder <> nil then
   Encoder.Free;

  if Transport <> nil then
   Transport.Free;

  inherited;
 end;

end.