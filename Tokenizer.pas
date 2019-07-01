// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Tokenizer;

interface

uses Common, SysUtils;

type
 TTokenType = (ttNone, ttToken, ttValue, ttSemicolon, ttError);

function Tokenize(var S, Token: String; var TokenType: TTokenType): Boolean;
function Tokenize1(var S, Value: String; const Expected: TTokenType): Boolean;
function TokenizeToken(var S: String; const Token: String): Boolean;
function TokenizeTokenVar(var S, Token: String): Boolean;
function TokenizeValue(var S, Value: String): Boolean;

implementation

function Tokenize(var S, Token: String; var TokenType: TTokenType): Boolean;
 var
  Got: Boolean;
  Ch: Char;
 begin
  TokenType:=ttNone;
  Token:='';
  S:=Trim(S);

  if Length(S) > 0 then
   begin
    Ch:=S[1];

    if Ch = ';' then // ttSemicolon
     begin
      TokenType:=ttSemicolon;
      S:=Copy(S, 2, Length(S));
     end
    else if Ch = '"' then // ttValue
     begin
      TokenType:=ttValue;
      Got:=False;
      S:=Copy(S, 2, Length(S));

      while Length(S) > 0 do
       begin
        Ch:=S[1];

        if Ch = '\' then
         if (Length(S) > 1) and (S[2] = '"') then
          begin
           S:=Copy(S, 3, Length(S));
           Token:=Token + '"';
          end
         else
          begin
           Token:=Token + Ch;
           S:=Copy(S, 2, Length(S));
          end
        else if Ch = '"' then
         begin
          Got:=True;
          S:=Copy(S, 2, Length(S));
          Break;
         end
        else
         begin
          Token:=Token + Ch;
          S:=Copy(S, 2, Length(S));
         end;
       end;

      if not Got then
       TokenType:=ttError;
     end
    else if Ch = '''' then // ttValue
     begin
      TokenType:=ttValue;
      Got:=False;
      S:=Copy(S, 2, Length(S));

      while Length(S) > 0 do
       begin
        Ch:=S[1];

        if Ch = '\' then
         if (Length(S) > 1) and (S[2] = '''') then
          begin
           S:=Copy(S, 3, Length(S));
           Token:=Token + '''';
          end
         else
          begin
           Token:=Token + Ch;
           S:=Copy(S, 2, Length(S));
          end
        else if Ch = '''' then
         begin
          Got:=True;
          S:=Copy(S, 2, Length(S));
          Break;
         end
        else
         begin
          Token:=Token + Ch;
          S:=Copy(S, 2, Length(S));
         end;
       end;

      if not Got then
       TokenType:=ttError;
     end
    else if Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then // ttToken
     begin
      TokenType:=ttToken;

      while Length(S) > 0 do
       begin
        Ch:=S[1];

        if Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
         begin
          Token:=Token + Ch;
          S:=Copy(S, 2, Length(S));
         end
        else
         Break;
       end;

      Token:=AnsiUpperCase(Token);
     end
    else
     TokenType:=ttError;
   end;

  Result:=(TokenType <> ttNone) and (TokenType <> ttError);
 end;

function Tokenize1(var S, Value: String; const Expected: TTokenType): Boolean;
 var
  TokenType: TTokenType;
 begin
  if Tokenize(S, Value, TokenType) then
   if TokenType = Expected then
    Exit(True)
   else
    Exit(False)
  else
   Exit(False);
 end;

function TokenizeToken(var S: String; const Token: String): Boolean;
 var
  D: String;
 begin
  if Tokenize1(S, D, ttToken) then
   Exit(Token = D)
  else
   Exit(False);
 end;

function TokenizeTokenVar(var S, Token: String): Boolean;
 begin
  Exit(Tokenize1(S, Token, ttToken));
 end;

function TokenizeValue(var S, Value: String): Boolean;
 begin
  Result:=Tokenize1(S, Value, ttValue);
 end;

end.