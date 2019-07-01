// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Prefetcher;

interface

uses Classes, SysUtils, BaseUnix, Sockets,
     Daemon, Common;

type
 TPrefetcher = class
 public
  Folder: String;
  constructor Create;
  function Exists(const FName: String): Boolean;
  procedure Touch(const FName: String);
  function FName(const FName: String): String;
  function TempFName(const FName: String): String;
  procedure RejectTempFName(const TempFName: String);
  function CommitTempFName(const TempFName, FName: String): Boolean;
  procedure Purge(const Seconds: LongInt);
  destructor Destroy; override;
 end;

var
 Prefetch: TPrefetcher;

implementation

constructor TPrefetcher.Create;
 begin
 end;

function TPrefetcher.Exists(const FName: String): Boolean;
 begin
  Result:=FileExists(Folder + FName);
 end;

procedure TPrefetcher.Touch(const FName: String);
 begin
  FileSetDate(FName, DateTimeToFileDate(Now));
 end;

function TPrefetcher.FName(const FName: String): String;
 begin
  Result:=Folder + FName;
 end;

function TPrefetcher.TempFName(const FName: String): String;
 begin
  Result:=Folder + FName + '.' + IntToStr(UT) + '.tmp';
 end;

procedure TPrefetcher.RejectTempFName(const TempFName: String);
 begin
  DeleteFile(TempFName);
 end;

function TPrefetcher.CommitTempFName(const TempFName, FName: String): Boolean;
 begin
  Result:=RenameFile(TempFName, Self.FName(FName));

  Touch(Self.FName(FName));
 end;

procedure TPrefetcher.Purge(const Seconds: LongInt);
 var
  Anchor: LongWord;
  SR: TSearchRec;
  rc: LongInt;
 begin
  rc:=FindFirst(Folder + '*', faAnyFile, SR);

  Anchor:=UT - Seconds;

  while rc = 0 do
   begin
    DebugLog('TPrefetcher.Purge: "' + SR.Name + '" attr=' + IntToStr(SR.Attr) + '/' + IntToStr(faDirectory));

    if SR.Attr and faDirectory = 0 then
     begin
      DebugLog('TPrefetcher.Purge: faDirectory ok, time=' + IntToStr(DateTimeToUnixTime(FileDateToDateTime(SR.Time))) + ', anchor=' + IntToStr(Anchor));

      if DateTimeToUnixTime(FileDateToDateTime(SR.Time)) < Anchor then
       begin
        DebugLog('TPrefetcher.Purge: killing "' + SR.Name + '"');

        DeleteFile(Folder + SR.Name);
       end;
     end;

    rc:=FindNext(SR);
   end;

  FindClose(SR);
 end;

destructor TPrefetcher.Destroy;
 begin
 end;

end.