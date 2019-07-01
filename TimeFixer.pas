// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

{$H+}
unit TimeFixer;

interface

uses SysUtils;

type
 TTimeFixer = Comp;
 TTimeFixerLogger = procedure(const S: String);

const
 TimeFixerLogger: TTimeFixerLogger = nil;

function TimeAnchor: TTimeFixer;
function FormatTimeAnchor(Anchor: TTimeFixer): String;
function FormatTimeAnchorDate(Anchor: TTimeFixer): String;

procedure TimeFix(var T: TTimeFixer);
procedure TimeUnfix(T: TTimeFixer; const S: String);

function TimeUnfixShort(T: TTimeFixer): String;

implementation

function TimeAnchor: TTimeFixer;
 begin
  Result:=TimeStampToMSecs(DateTimeToTimeStamp(Now));
 end;

function FormatTimeAnchor(Anchor: TTimeFixer): String;
 begin
  Str(Anchor / 1000:1:2, Result);
 end;

function FormatTimeAnchorDate(Anchor: TTimeFixer): String;
 begin
  Result:=FormatDateTime('dd.mm.yyyy HH:nn:ss', TimeStampToDateTime(MSecsToTimeStamp(Anchor)));
 end;

procedure TimeFix(var T: TTimeFixer);
 begin
  T:=TimeAnchor;
 end;

procedure TimeUnfix(T: TTimeFixer; const S: String);
 var
  D: String;
 begin
  T:=TimeStampToMSecs(DateTimeToTimeStamp(Now)) - T;

  if Assigned(TimeFixerLogger) then
   begin
    Str(T / 1000:1:4, D);

    TimeFixerLogger('TIME ' + S + ': ' + D + 's');
   end;
 end;

function TimeUnfixShort(T: TTimeFixer): String;
 var
  D: String;
 begin
  Str((TimeAnchor - T) / 1000:1:4, D);

  Result:=D;
 end;

end.