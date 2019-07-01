// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit Resolver;

interface

function ResolveHostname(Hostname: String; var IP: Longint): Boolean;

implementation

{$IFDEF WIN32}
uses
     WinSock, SysUtils, Daemon, Common, TimeFixer;

function ResolveHostname(Hostname: String; var IP: Longint): Boolean;
 var
  HostEnt: PHostEnt;
  Addr: PLongint;
  Addr2: PChar absolute addr;
 begin
  Result:=False;

  HostEnt:=WinSock.GetHostByName(PChar(Hostname));

  if Assigned(HostEnt) then
   if Assigned(HostEnt^.h_addr_list) then
    begin
     Addr:=nil;

     Addr2:=HostEnt^.h_addr_list^;

     if Addr <> nil then
      begin
       IP:=Longint(Addr^);

       Result:=True;
      end;
    end;
 end;

{$ELSE}
uses
     NetDB,
     Sockets;

function ResolveHostname(Hostname: String; var IP: Longint): Boolean;
 var
  Addresses: array of THostAddr;
 begin
  IP:=LongInt(StrToNetAddr(Hostname));

  if IP = 0 then
   begin
    SetLength(Addresses, 1);

    Result:=ResolveName(Hostname, Addresses) = 1;

    if Result then
     IP:=LongInt(Addresses[Low(Addresses)]);
   end
  else
   Result:=True;
 end;

{$ENDIF}

end.