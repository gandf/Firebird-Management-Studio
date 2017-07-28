unit zluSockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, zluCommDiag
  {$ifdef windows}
  , winsock2
  {$endif}
  ;

type

  TibcSocket = class
  private
    { private declarations }
    FPortName: String;
    FHost: String;
    FTimeout: Integer;
    FPort: Integer;
    c_connected: boolean;
    c_socket: integer;
    inetaddr_client_remote : tsockaddr;
    function GetPortName: String;
    procedure SetHost(PHost: String);
    procedure SetTimeout(PTimeout: Integer);
    procedure SetPort(PPort: Integer);

    procedure memclr(p: pointer ; len: longint);
    procedure setsockaddr(var sa: tsockaddr; adr: string; port: longint);
    function my_connect(s: integer; var ina: tsockaddr): boolean;
    procedure setnonblockingsocket(s: integer);
    function is_writable_socket(s: integer; var error: longint): boolean;
    function ip_client_start_socket(s: string) : boolean;
    function ip_client_is_connected(var error: longint): boolean;
    procedure ip_client_close_socket;
  public
    { public declarations }
    constructor Create(aOwner: TComponent);
    procedure Connect;
    procedure Disconnect;
    function Connected: Boolean;
    property PortName: String read GetPortName;
    property Host: String read FHost write SetHost;
    property Timeout: Integer read FTimeout write SetTimeout;
    property Port: Integer read FPort write SetPort;
  published
    { published declarations }
  end;

implementation

constructor TibcSocket.Create(aOwner: TComponent);
begin
end;

function TibcSocket.GetPortName : String;
var
  Service : String;
begin
  // list of well known ports

  // determine service name based on port number
  // these are well-known ports
  case Port of
    IP_PORT_ECHO       : Service:='ECHO';
    IP_PORT_DISCARD    : Service:='DISCARD';
    IP_PORT_SYSTAT     : Service:='SYSTAT';
    IP_PORT_DAYTIME    : Service:='DAYTIME';
    IP_PORT_NETSTAT    : Service:='NETSTAT';
    IP_PORT_FTP        : Service:='FTP';
    IP_PORT_TELNET     : Service:='TELNET';
    IP_PORT_SMTP       : Service:='SMTP';
    IP_PORT_TIMESERVER : Service:='TIMESERVER';
    IP_PORT_NAMESERVER : Service:='NAMESERVER';
    IP_PORT_WHOIS      : Service:='WHOIS';
    IP_PORT_MTP        : Service:='MTP';
    IP_PORT_GDS_DB     : Service:='GDS_DB';
  else
    Service:='';
  end;

  FPortName:=Service;
  Result:=FPortName;
end;

procedure TibcSocket.SetHost(PHost: String);
begin
  FHost := PHost;
end;
procedure TibcSocket.SetTimeout(PTimeout: Integer);
begin
  FTimeout := PTimeout;
end;
procedure TibcSocket.SetPort(PPort: Integer);
begin
  FPort := PPort;
end;

procedure TibcSocket.Connect;
begin
  ip_client_start_socket(Host);
end;

procedure TibcSocket.Disconnect;
begin
  ip_client_close_socket();
end;

function TibcSocket.Connected : Boolean;
var
  VError: longint;
begin
  result := ip_client_is_connected(VError);
end;

procedure TibcSocket.memclr(p: pointer ; len: longint);
var
  x: longint;
begin
  for x := 1 to len do
  begin
    byte(p^) := 0;
    inc(p);
  end;
end;

procedure TibcSocket.setsockaddr(var sa: tsockaddr; adr: string; port: longint);
begin
 memclr(@sa, sizeof(sa));
 sa.sin_family := AF_INET;
 sa.sin_addr.S_addr := inet_addr(PChar(adr));
 sa.sin_port := htons(port);
end;

function TibcSocket.my_connect(s: integer; var ina: tsockaddr): boolean;
var
  c: longint;
begin
  c := fpconnect(s, @ina, sizeof(ina));
  {$ifdef windows}
  if c = SOCKET_ERROR then
    if wsagetlasterror = WSAEISCONN then
      c := 0;
  {$endif}
  result := (c = 0);
end;

procedure TibcSocket.setnonblockingsocket(s: integer);
var
  nb: dword;
  {$ifdef linux}
  arg: longint;
  {$endif}
begin
  // nonblocking
  {$ifdef linux}
  arg := fpfcntl(s , F_GETFL );
  if arg >= 0 then
  begin
    arg := arg or O_NONBLOCK;
    fpfcntl(s , F_SETFL , arg );
  end;
  {$endif}

  {$ifdef windows}
  nb := 1; // 1 = nonblocking, 0 = blocking
  winsock2.ioctlsocket(s, FIONBIO, @nb);
  {$endif}
end;

function TibcSocket.is_writable_socket(s: integer; var error: longint): boolean;
var
  fds: tfdset;
  tv: timeval;
  valopt: longint = 1;
  vallen: {$ifdef linux} longword {$else} longint {$endif};
begin
  result := false;
  {$ifdef linux}
  fpfd_zero(fds);
  fpfd_set(s, fds);
  {$endif}
  {$ifdef windows}
  fd_zero(fds);
  fd_set(s, fds);
  {$endif}
  tv.tv_sec := FTimeout div 1000;
  tv.tv_usec := FTimeout mod 1000;
  //               socket+1 , read , write , except , timeout
  {$ifdef linux}
  if fpselect(s + 1, nil, @fds, nil, @tv) > 0 then
  begin
    if fpfd_isset(s, fds ) = 1 then
    begin
      vallen := sizeof(valopt);
      if fpgetsockopt(s, sol_socket, so_error, @valopt, @vallen) = 0 then
      begin
        error := valopt;
        result := valopt = 0;
      end;
    end;
  end;
  {$else}
  if select(s + 1, nil, @fds, nil, @tv) > 0 then
  begin
    if fd_isset(s, fds ) then
    begin
      vallen := sizeof(valopt);
      if getsockopt(s, sol_socket, so_error, valopt, vallen) = 0 then
      begin
        error := valopt;
        result := valopt = 0;
      end;
    end;
  end;
  {$endif}
end;

function TibcSocket.ip_client_start_socket(s: string) : boolean;
begin
  result := false;
  setsockaddr(inetaddr_client_remote, s, FPort);
  c_socket := fpsocket(PF_INET, SOCK_STREAM, 0);
  if c_socket >= 0 then
  begin
    setnonblockingsocket(c_socket); // nonblocking connect call
    result := my_connect(c_socket, inetaddr_client_remote);
  end;

  //setnonblockingsocket(c_socket ); // blocking connect call, if this is executed later
  c_connected := false;
end;

function TibcSocket.ip_client_is_connected(var error: longint): boolean;
begin
  if not(c_connected) then
    c_connected := is_writable_socket(c_socket ,error);
  result := c_connected;
end;

procedure TibcSocket.ip_client_close_socket;
begin
  if c_socket >= 0 then
    closesocket(c_socket);
  c_socket := -1;
  c_connected := false;
end;

end.

