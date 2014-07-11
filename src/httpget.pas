unit httpget;

(*
    OpenAcoon - An OpenSource Internet-Search-Engine
    Copyright (C) 1999-2014 Acoon GmbH

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as
    published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
*)

interface


uses
    {$ifdef Unix}
    cthreads,
    {$endif}
    SysUtils,
    robotglobal,
    IdGlobal,
    IdTCPClient;



type
    tHttpGet = class
	strict private
	    ResponseBuffer: AnsiString;
	    ResponseBufferIsValid: boolean;
	    ReadPos: int32;

	    procedure SetDefaults;
	    procedure SetupClient;
	    procedure Connect;
	    procedure SendRequest;
	    function RequestString:AnsiString;
	    procedure ReceiveData;
	    procedure ReadChunk;
	    procedure CheckIfMaximumSizeExceeded;
	    procedure PopulateResponseBuffer;

	public
	    ErrorCode: integer;
	    UserAgent: AnsiString;
	    MaxSize: int32; // Maximum response-size to accept
	    Host: AnsiString;
	    Path: AnsiString;
	    Port: uint16;
	    AcceptLanguage : AnsiString;
	    IP: tIP4;
	    ReadDelay: int32; // milliseconds to wait between socket-reads
	    Client: tIdTCPClient;

	    constructor Create;
	    destructor Destroy; override;
	    procedure Get;

	    function GetStatusCode: integer;
	    procedure Seek(APosition: int32);
	    function FilePos: int32;
	    function ReadLine: AnsiString;
	    function GetResponse: AnsiString;
	    function Eof: boolean;
    end;




// ------------------------------------------------------------------




implementation





constructor tHttpGet.Create;
begin
    Client := tIdTCPClient.Create(nil);

    ErrorCode := 0;
    Host := '';
    Path := '';
    Port := 80;
    IP.IP := 0;
    ReadPos := 0;
    ResponseBuffer := '';
    ResponseBufferIsValid := false;

    SetDefaults;
end;





procedure tHttpGet.SetDefaults;
begin
    // Default user-agent is "httpget". In most cases you should
    // override that in the code that uses this class.
    UserAgent := 'httpget';

    // Default maximum response-size is 1mb
    MaxSize := 1024*1024;

    AcceptLanguage := '';

    // With a 64kb TCP/IP receive-window, a 50ms delay for every 64kb
    // transferred effectively rate-limits the connection to about 10mbit/s.
    // This makes the request easier on the server's internet-connection.
    // If you want maximum possible speed, then set this to zero.
    ReadDelay := 50;

    // Set connection-timeout and read-timeout to 10 seconds
    Client.ConnectTimeout := 10000;
    Client.ReadTimeout := 10000;
end;





destructor tHttpGet.Destroy;
begin
    Client.Free;
end;





procedure tHttpGet.Get;
begin
    SetupClient;
    if ErrorCode = 0 then Connect;
    if ErrorCode = 0 then SendRequest;
    if ErrorCode = 0 then ReceiveData;
end;





procedure tHttpGet.SetupClient;
begin
    if Host = '' then ErrorCode := 10;
    if IP.IP = 0 then ErrorCode := 11;
    if Path = '' then ErrorCode := 12;

    if ErrorCode = 0 then
    begin
	Client.Host := Ip2Str(IP);
	Client.Port := Port;
    end;
end;





procedure tHttpGet.Connect;
begin
    try
	Client.Connect;

	// Yes, this looks like wasteful memory-usage. But it has the
	// advantage to REALLY cut-down on memory-reallocation frequency.
	// And to cut-down on moving-around of buffer-content.
	Client.Socket.InputBuffer.GrowthFactor := 1024*1024;

    except
	ErrorCode := 20;
    end;
end;





procedure tHttpGet.SendRequest;
begin
    try
	Client.Socket.Write(RequestString, en8bit);
    except
	ErrorCode := 30;
    end;
end;





function tHttpGet.RequestString:AnsiString;
begin
    Result:=
	'GET ' + Path + ' HTTP/1.0'#13#10+
	'Host: ' + Host + #13#10+
	'Connection: close'#13#10+
	'User-Agent: ' + UserAgent + #13#10+
	'Accept: */*'#13#10;

    if AcceptLanguage <> '' then
	Result := Result + 'Accept-Language: ' + AcceptLanguage + #13#10;

    Result := Result +#13#10;
end;





procedure tHttpGet.ReceiveData;
begin
    try
	while (ErrorCode = 0) and (not Client.Socket.ClosedGracefully) do
	    ReadChunk;

	// Check for data once more. In my tests this was never
	// necessary, but it could be. Better safe than sorry.
	if ErrorCode = 0 then ReadChunk;

    except
	ErrorCode := 40;
    end;
end;





procedure tHttpGet.ReadChunk;
begin
    Client.Socket.CheckForDataOnSource;
    CheckIfMaximumSizeExceeded;

    // If ReadDelay>0 then this acts as a rate-limiter.
    // If ReadDelay=0 then this will at least yield the current
    // thread, thus avoiding excessive CPU-usage.
    Sleep(ReadDelay);
end;





procedure tHttpGet.CheckIfMaximumSizeExceeded;
begin
    if Client.Socket.InputBuffer.Size > MaxSize then
	ErrorCode := 50;
end;





function tHttpGet.GetStatusCode: integer;
var
    s: string;
    i: integer;
begin
    Seek(0);
    s := ReadLine;

    i := Pos(' ',s);
    if i > 0 then Delete(s,1,i);

    i := Pos(' ',s);
    if i > 0 then s := copy(s, 1, i-1);

    try
	Result := StrToIntDef(s, 0);
    except
	Result := 0;
    end;
end;





procedure tHttpGet.Seek(APosition: int32);
begin
    if APosition = 0 then PopulateResponseBuffer;
    ReadPos := APosition;
end;





procedure tHttpGet.PopulateResponseBuffer;
begin
    if Assigned(Client.Socket) then
	if Assigned(Client.Socket.InputBuffer) then
	begin
	    ResponseBuffer := Client.Socket.InputBuffer.AsString;
	    ResponseBufferIsValid := true;
	end;
end;





function tHttpGet.FilePos: int32;
begin
    Result := ReadPos;
end;





function tHttpGet.ReadLine: AnsiString;
var
    c: char;
begin
    Result := '';

    while not eof do
    begin
	Inc(ReadPos);
	c := ResponseBuffer[ReadPos];

	case c of
	    #10: break; // End of line. We are done.
	    #13: ; // Ignore <cr>
	    else Result := Result + c;
	end;
    end;
end;





function tHttpGet.GetResponse: AnsiString;
begin
    PopulateResponseBuffer;
    Result := ResponseBuffer;
end;





function tHttpGet.Eof: boolean;
begin
    Result := ReadPos >= Length(ResponseBuffer);
end;





end.
