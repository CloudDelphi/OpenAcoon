unit HTTPClient;

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
    Classes,
    GlobalTypes,
    ScktComp,
    DNSResolver,
    MemoryFile;

type
    tHTTPClient = class(tThread)
    strict private
        Url: AnsiString;
        Complete: boolean;
        Position: int32;
        ResponseData: tMemoryFile;
        StatusCode: integer;
        Client: tClientSocket;
        SocketStream: tWinSocketStream;
        Request: AnsiString;
        IP: tIP4;

        procedure ErrorUnknownHost;
        procedure Shutdown;
        function ReadFromStream(var s: shortstring; ErrCode: integer): integer;
        function AddToResponse(var s: shortstring): boolean;
        // function GetResponse: AnsiString;
    protected
        procedure Execute; override;
        procedure ErrorAbort(ErrCode: integer);
    public
        UserAgent: AnsiString;
        MaxSize: int32;

        constructor Create(AUrl: AnsiString; ThisIP: tIP4);
        destructor Destroy; override;
        function IsComplete: boolean;
        function GetStatusCode: integer;
        procedure Seek(APosition: int32);
        function FilePos: int32;
        function ReadLine: AnsiString;
        function GetResponse: RawByteString;
        function Eof: boolean;
        procedure WaitForCompletion(TimeOut: int32); // TimeOut in MilliSekunden
    end;

var
    HTTPClientDefaultMaxSize: int32;
    HTTPClientDefaultUserAgent: AnsiString;

procedure CountFailedConnection;
function GetAndResetFailedConnectionCount: integer;


implementation

uses
    SysUtils,
    Windows,
    Logging,
    IdGlobal,
    SyncObjs;

var
    FailedCS: tCriticalSection;
    FailedConnections: integer;


procedure CountFailedConnection;
begin
    FailedCS.Enter;
    Inc(FailedConnections);
    FailedCS.Leave;
end;



function GetAndResetFailedConnectionCount: integer;
begin
    FailedCS.Enter;
    Result := FailedConnections;
    FailedConnections := 0;
    FailedCS.Leave;
end;



function tHTTPClient.AddToResponse(var s: shortstring): boolean;
begin
    // DebugLogMsg('robot.log','AddToResponse.Length='+IntToStr(Length(s))+' "'+s+'"');
    if Length(s) > 0 then ResponseData.Write(s[1], Length(s));
    if ResponseData.Size > MaxSize then
    begin
        ErrorAbort(403); // Datei zu gross. "Access denied"
        exit(false);
    end;
    Result := true;
end;


constructor tHTTPClient.Create(AUrl: AnsiString; ThisIP: tIP4);
begin
    inherited Create(true);
    FreeOnTerminate := false;
    Complete := false;
    IP := ThisIP;
    UserAgent := HTTPClientDefaultUserAgent;
    MaxSize := HTTPClientDefaultMaxSize;
    Url := AUrl;
    try
        ResponseData := tMemoryFile.Create(16384);
    except
        DebugLogMsg('robot.log', 'Error creating MemoryFile');
    end;
    StatusCode := 0;
    Position := 1;
end;


destructor tHTTPClient.Destroy;
begin
    ResponseData.Free;
    inherited;
end;


function tHTTPClient.Eof: boolean;
begin
    Result := Position > ResponseData.Size;
end;


procedure tHTTPClient.Shutdown;
begin
    try
        DebugLogMsg('robot.log', '20');
        // if Client.Active then DebugLogMsg('robot.log','IsActive')
        // else DebugLogMsg('robot.log','NOTActive');
        if Client.Active then Client.Close;
        DebugLogMsg('robot.log', '21');
    except
    end;


    try
        DebugLogMsg('robot.log', '24');
        if Assigned(SocketStream) then FreeAndNil(SocketStream);
        DebugLogMsg('robot.log', '25');
    except
        DebugLogMsg('robot.log', '25a');
    end;


    try
        DebugLogMsg('robot.log', '22');
        FreeAndNil(Client);
        DebugLogMsg('robot.log', '23');
    except
        DebugLogMsg('robot.log', '23a');
    end;


    DebugLogMsg('robot.log', '26');
end;


procedure tHTTPClient.ErrorAbort(ErrCode: integer);
begin
    if ErrCode >= 996 then CountFailedConnection;

    StatusCode := ErrCode;
    DebugLogMsg('robot.log', 'ErrorCode=' + IntToStr(ErrCode));
    DebugLogMsg('robot.log', '41');
    Shutdown;
    DebugLogMsg('robot.log', '40');
    Complete := true;
end;


procedure tHTTPClient.ErrorUnknownHost;
begin
    ErrorAbort(404); // "Not found"
end;


procedure tHTTPClient.Execute;
var
    Host, Path: AnsiString;
    i, Port: integer;
    Le: int64;
    s: shortstring;
    s2: string;
begin
    // SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_BELOW_NORMAL);

    Host := AnsiString(LowerCase(Url));
    i := Pos('/', Host);
    if i > 0 then
    begin
        Host := copy(Host, 1, i - 1);
        Path := copy(Url, i, Length(Url));
    end
    else Path := '/';
    i := Pos(':', Host);
    Port := 80;
    if i <> 0 then
    begin
        try
            Port := StrToIntDef(copy(Host, i + 1, 5), 80);
        except
        end;
        Host := copy(Host, 1, i - 1);
        if Port < 0 then Port := 0;
        if Port > 65535 then Port := 65535;
    end;

    // Dialogs.MessageDlg('Host="' + Host + '"  Port=' + IntToStr(Port) + '  Path="' + Path + '"',
    // mtInformation, [mbOk], 0, mbOk);

    try
        Client := tClientSocket.Create(nil);
        Client.ClientType := ctBlocking;
        SocketStream := tWinSocketStream.Create(tCustomWinSocket(Client.Socket), 11000);
        Client.Port := Port;
        DebugLogMsg('robot.log', Host + ' is pre-resolved to ' + Ip2Str(IP));
        if IP.IP <> 0 then Client.Address := Ip2Str(IP)
        else Client.Host := Host;

        try
            Client.Open;
        except
            on E: eSocketError do
            begin
                s2 := E.Message;
                if Pos('10049', s2) > 0 then ErrorUnknownHost
                else ErrorAbort(999);
                exit;
            end;
            else
            begin
                ErrorAbort(998);
                exit;
            end;
        end;


        Request :=
        'GET ' + Path + ' HTTP/1.0'#13#10 +
        'Host: ' + Host + #13#10 +
        'User-Agent: ' + UserAgent + #13#10 +
        'Connection: close' + #13#10 +
        'Accept: */*'#13#10 +
        #13#10;


        try
            Le := Length(Request);
            if SocketStream.Write(Request[1], Le) <> Le then
            begin
                ErrorAbort(997);
                exit;
            end;
        except
            ErrorAbort(996);
            exit;
        end;

        for i := 1 to 100000 do
        begin
            Le := ReadFromStream(s, 995);
            DebugLogMsg('robot.log', 'Le=' + IntToStr(Le));
            if Le = -1 then exit; // Exit if there was an error

            if s = '' then break;
            if not AddToResponse(s) then exit;
        end;
        DebugLogMsg('robot.log', '3');

        Le := ReadFromStream(s, 993);
        if Le > 0 then AddToResponse(s);
        Shutdown;
    except
        ResponseData.Size := 0;
        ErrorAbort(992);
    end;

    DebugLogMsg('robot.log', 'xx1');
    if ResponseData.Size > 0 then
    begin
        s := ReadLine;
        DebugLogMsg('robot.log', s);
        Seek(0);
        i := Pos(' ', s);
        if i > 0 then
        begin
            Delete(s, 1, i);
            i := Pos(' ', s);
            if i > 0 then
            begin
                s := copy(s, 1, i - 1);
                try
                    StatusCode := StrToIntDef(s, 991);
                except
                    StatusCode := 990;
                end;
            end;
        end;
    end;

    DebugLogMsg('robot.log', 'StatusCode=' + IntToStr(StatusCode));

    // DebugLogMsg('robot.log',GetResponse);
    Complete := true;
end;


function tHTTPClient.ReadFromStream(var s: shortstring; ErrCode: integer): integer;
var
    Ti1: int64;
    Le: integer;
begin
    Ti1 := Ticks;
    Le := 0;
    try
        DebugLogMsg('robot.log', '1');
        Le := SocketStream.Read(s[1], 255);
        DebugLogMsg('robot.log', '2');
    except
        DebugLogMsg('robot.log', '4');
        ErrorAbort(ErrCode);
        DebugLogMsg('robot.log', '5');
        s := '';
        DebugLogMsg('robot.log', '6');
        exit(-1);
    end;
    DebugLogMsg('robot.log', '7');
    if (Ticks - Ti1) > 10000 then
    begin
        DebugLogMsg('robot.log', '8');
        ErrorAbort(994);
        DebugLogMsg('robot.log', '9');
        s := '';
        exit(-1);
    end;

    DebugLogMsg('robot.log', '10');
    SetLength(s, Le);
    Result := Le;
end;


function tHTTPClient.FilePos: int32;
begin
    Result := Position;
end;


(*
function tHTTPClient.GetResponse: AnsiString;
var
    s: AnsiString;
begin
    SetLength(s, ResponseData.Size);
    ResponseData.Position := 0;
    if Length(s)>0 then ResponseData.Read(s[1], Length(s));
    Result := s;
end;
*)


function tHTTPClient.GetResponse: RawByteString;
begin
    SetLength(Result, ResponseData.Size);
    ResponseData.Position := 0;
    ResponseData.Read(Result[1], Length(Result));
    // WriteLn(Result);
end;



function tHTTPClient.GetStatusCode: integer;
begin
    Result := StatusCode;
end;


function tHTTPClient.IsComplete: boolean;
begin
    Result := Complete;
end;


procedure tHTTPClient.Seek(APosition: int32);
begin
    Position := APosition + 1;
end;


procedure tHTTPClient.WaitForCompletion(TimeOut: int32);
var
    i: integer;
begin
    // while (not IsComplete) and (TimeOut > 0) do
    while (not Finished) and (TimeOut > 0) do
    begin
        i := TimeOut;
        if i > 100 then i := 100;
        Sleep(i);
        Dec(TimeOut, i);
    end;
end;


function tHTTPClient.ReadLine: AnsiString;
var
    i, j: integer;
    s: AnsiString;
begin
    i := Position;
    j := i - 1;
    while ((i - 1) < ResponseData.Size) and (ResponseData.GetByte(i - 1) <> 10) do
    begin
        // DebugLogMsg('robot.log',IntToStr(i)+'='+IntToStr(ResponseData.GetByte(i-1)));
        if ResponseData.GetByte(i - 1) <> 13 then j := i;
        Inc(i);
    end;

    s := ''; // Do NOT remove this line or the following SetLength will work on an uninitialized string!!!!
    SetLength(s, j - Position + 1);
    // DebugLogMsg('robot.log',IntToStr(Length(s)));

    ResponseData.Position := Position - 1;
    if Length(s) > 0 then ResponseData.Read(s[1], Length(s));
    Position := i + 1;
    // DebugLogMsg('robot.log',s);
    Result := s;
end;


begin
    HTTPClientDefaultMaxSize := 200 * 1024;
    HTTPClientDefaultUserAgent :=
    'Acoon-Robot ' + cShortVersion + ' (http://www.acoon.de)';
    FailedCS := tCriticalSection.Create;
    FailedConnections := 0;

end.
