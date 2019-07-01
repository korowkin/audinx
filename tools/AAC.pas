unit AAC;

interface

{$IFDEF WIN32}
uses Windows;
{$ENDIF}

const
{$IFDEF WIN32}
 AACLibPath = 'libfdk-aac.dll';
{$ENDIF}

{$IFDEF FREEBSD}
 AACLibPath = 'libfdk-aac.so';
{$ENDIF}

{$IFDEF LINUX}
 AACLibPath = '/usr/lib/libfdk-aac.so';
{$ENDIF}

var
 AACLibLoaded: Boolean = False;

const
 AACENCODER_LIB_VL0            = 3;
 AACENCODER_LIB_VL1            = 4;
 AACENCODER_LIB_VL2            = 12;

 // AACENC_ERROR

 AACENC_OK                     = $0000;

 AACENC_INVALID_HANDLE         = $0020;
 AACENC_MEMORY_ERROR           = $0021;
 AACENC_UNSUPPORTED_PARAMETER  = $0022;
 AACENC_INVALID_CONFIG         = $0023;

 AACENC_INIT_ERROR             = $0040;
 AACENC_INIT_AAC_ERROR         = $0041;
 AACENC_INIT_SBR_ERROR         = $0042;
 AACENC_INIT_TP_ERROR          = $0043;
 AACENC_INIT_META_ERROR        = $0044;

 AACENC_ENCODE_ERROR           = $0060;

 AACENC_ENCODE_EOF             = $0080;

 // AACENC_BufferIdentifier

 IN_AUDIO_DATA                 = 0;
 IN_ANCILLRY_DATA              = 1;
 IN_METADATA_SETUP             = 2;
 OUT_BITSTREAM_DATA            = 3;
 OUT_AU_SIZES                  = 4;

type
 INT = LongInt;
 UINT = LongWord;

 PPointer = ^Pointer;

 PAACENC_InfoStruct = ^AACENC_InfoStruct;
 AACENC_InfoStruct = record
  maxOutBufBytes: UINT;
  maxAncBytes: UINT;
  inBufFillLevel: UINT;
  inputChannels: UINT;
  frameLength: UINT;
  encoderDelay: UINT;
  confBuf: array[1..64] of Char;
  confSize: UINT;
 end;

 PAACENC_BufDesc = ^AACENC_BufDesc;
 AACENC_BufDesc = record
  numBufs: INT;
  bufs: PPointer;
  bufferIdentifiers: Pointer;
  bufSizes: Pointer;
  bufElSizes: Pointer;
 end;

 PAACENC_InArgs = ^AACENC_InArgs;
 AACENC_InArgs = record
  numInSamples: INT;
  numAncBytes: INT;
 end;

 PAACENC_OutArgs = ^AACENC_OutArgs;
 AACENC_OutArgs = record
  numOutBytes: INT;
  numInSamples: INT;
  numAncBytes: INT;
 end;

 PLIB_INFO = ^LIB_INFO;
 LIB_INFO = record
  title: PChar;
  build_date: PChar;
  build_time: PChar;
  module_id: Integer;
  version: Integer;
  flags: UINT;
  versionStr: array[1..32] of Char;
 end;

 PHANDLE_AACENCODER = ^HANDLE_AACENCODER;
 HANDLE_AACENCODER = Pointer;

 AACENC_ERROR = UINT;

const
 AACENC_METADATA_DRC_NONE          = 0;
 AACENC_METADATA_DRC_FILMSTANDARD  = 1;
 AACENC_METADATA_DRC_FILMLIGHT     = 2;
 AACENC_METADATA_DRC_MUSICSTANDARD = 3;
 AACENC_METADATA_DRC_MUSICLIGHT    = 4;
 AACENC_METADATA_DRC_SPEECH        = 5;

 AACENC_INIT_NONE                  = $0000;
 AACENC_INIT_CONFIG                = $0001;
 AACENC_INIT_STATES                = $0002;
 AACENC_INIT_TRANSPORT             = $1000;
 AACENC_RESET_INBUFFER             = $2000;
 AACENC_INIT_ALL                   = $FFFF;

 // AACENC_PARAM

 AACENC_AOT                        = $0100;
 AACENC_BITRATE                    = $0101;
 AACENC_BITRATEMODE                = $0102;
 AACENC_SAMPLERATE                 = $0103;
 AACENC_SBR_MODE                   = $0104;
 AACENC_GRANULE_LENGTH             = $0105;
 AACENC_CHANNELMODE                = $0106;
 AACENC_CHANNELORDER               = $0107;
 AACENC_SBR_RATIO                  = $0108;
 AACENC_AFTERBURNER                = $0200;
 AACENC_BANDWIDTH                  = $0203;
 AACENC_TRANSMUX                   = $0300;
 AACENC_HEADER_PERIOD              = $0301;
 AACENC_SIGNALING_MODE             = $0302;
 AACENC_TPSUBFRAMES                = $0303;
 AACENC_PROTECTION                 = $0306;
 AACENC_ANCILLARY_BITRATE          = $0500;
 AACENC_METADATA_MODE              = $0600;
 AACENC_CONTROL_STATE              = $FF00;
 AACENC_NONE                       = $FFFF;

type
 TaacEncOpen = function(phAacEncoder: PHANDLE_AACENCODER; const encModules: UINT; const maxChannels: UINT): AACENC_ERROR; cdecl;
 TaacEncClose = function(phAacEncoder: PHANDLE_AACENCODER): AACENC_ERROR; cdecl;
 TaacEncEncode = function(const hAacEncoder: HANDLE_AACENCODER; const inBufDesc: PAACENC_BufDesc; const outBufDesc: PAACENC_BufDesc; const inargs: PAACENC_InArgs; const outargs: PAACENC_OutArgs): AACENC_ERROR; cdecl;
 TaacEncInfo = function(const hAacEncoder: HANDLE_AACENCODER; pInfo: PAACENC_InfoStruct): AACENC_ERROR; cdecl;
 TaacEncoder_SetParam = function(const hAacEncoder: HANDLE_AACENCODER; const param: UINT; const value: UINT): AACENC_ERROR; cdecl;
 TaacEncoder_GetParam = function(const hAacEncoder: HANDLE_AACENCODER; const param: UINT): UINT; cdecl;
 TaacEncGetLibInfo = function(info: PLIB_INFO): AACENC_ERROR; cdecl;

var
 aacEncOpen: TaacEncOpen;
 aacEncClose: TaacEncClose;
 aacEncEncode: TaacEncEncode;
 aacEncInfo: TaacEncInfo;
 aacEncoder_SetParam: TaacEncoder_SetParam;
 aacEncoder_GetParam: TaacEncoder_GetParam;
 aacEncGetLibInfo: TaacEncGetLibInfo;

implementation

{$IFDEF WIN32}

var
 Handle: HMODULE;

initialization
 Handle:=LoadLibraryEx(AACLibPath, 0, 0);

 if Handle <> 0 then
  begin
   AACLibLoaded:=True;

   aacEncOpen:=GetProcAddress(Handle, 'aacEncOpen');
   aacEncClose:=GetProcAddress(Handle, 'aacEncClose');
   aacEncEncode:=GetProcAddress(Handle, 'aacEncEncode');
   aacEncInfo:=GetProcAddress(Handle, 'aacEncInfo');
   aacEncoder_SetParam:=GetProcAddress(Handle, 'aacEncoder_SetParam');
   aacEncoder_GetParam:=GetProcAddress(Handle, 'aacEncoder_GetParam');
   aacEncGetLibInfo:=GetProcAddress(Handle, 'aacEncGetLibInfo');
  end;

finalization
 if Handle <> 0 then 
  FreeLibrary(Handle);

{$ENDIF}

{$IFNDEF WIN32}

function dlopen(name: pchar; mode: longint): pointer; cdecl; external 'c';
function dlsym(lib: pointer; name: pchar): pointer; cdecl; external 'c';
function dlclose(lib: pointer): longint; cdecl; external 'c';

var
 Handle: Pointer;

initialization
 {$IFDEF LINUX}
 Handle:=dlopen(AACLibPath, 1);
 {$ELSE}
 Handle:=dlopen(AACLibPath, 0);
 {$ENDIF}

 if Handle <> nil then
  begin
   AACLibLoaded:=True;

   aacEncOpen:=dlsym(Handle, 'aacEncOpen');
   aacEncClose:=dlsym(Handle, 'aacEncClose');
   aacEncEncode:=dlsym(Handle, 'aacEncEncode');
   aacEncInfo:=dlsym(Handle, 'aacEncInfo');
   aacEncoder_SetParam:=dlsym(Handle, 'aacEncoder_SetParam');
   aacEncoder_GetParam:=dlsym(Handle, 'aacEncoder_GetParam');
   aacEncGetLibInfo:=dlsym(Handle, 'aacEncGetLibInfo');
  end;

finalization
 if Handle <> nil then 
  dlclose(Handle);

{$ENDIF}

end.