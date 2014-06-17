unit RobotGetUrl;

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
    ScktComp,
    SyncObjs,
    DNSResolver,
    Classes;

type
    tMyByteArray = array [0 .. 2000 * 1000 * 1000] of byte;
    pMyByteArray = ^tMyByteArray;

    tUrlInfo = record
        Port: integer;
        Domain: shortstring;
        Path: shortstring;
        ModifiedStr: shortstring;
        UrlPos: shortstring;
        Url: shortstring;
        OrgUrl: shortstring;
    end;

    tGetUrl = class(tThread)
    private
        UrlInfo: tUrlInfo;
        Client: tClientSocket;
        SocketStream: tWinSocketStream;
        Buffer: pMyByteArray;
        FSize: int32;
        Request: array [0 .. 1030] of AnsiChar;
        ReqLen: integer;
        IP: tIP4;
        procedure ErrorAbort;
        procedure ErrorUnknownHost;
        procedure ClearBuffer;
        procedure WriteToBuffer(const s: AnsiString);
        procedure AddRequest(const s: shortstring);
    protected
        procedure Execute; override;
        procedure Cleanup;
    public
        constructor Create(Info: tUrlInfo);
    end;

var
    CritSec: tCriticalSection;
    MaxConnections, CurConnections: integer;
    TotalBytes: int64;
    OutputOpen: boolean;
    OutputFile: file;
    UrlNr: integer;
    UrlAn: integer;
    NewConns: integer;


procedure StartNewConnection;


implementation

uses
    FileLocation,
    MemoryPool,
    HTTPClient,
    Logging,
    RobotsTxt,
    idGlobal,
    SysUtils;

const
    cMaxUrls = 100000000; // 100 million

var
    OutputNumber: integer;
    Urls: array [1 .. cMaxUrls] of pShortString;


// This procedure will be called from one of the tGetUrl objects.
// CritSec must be ENTERed before (!) calling this procedure and
// LEAVEd after calling it.
procedure WriteOutput(var Buffer; Len: int32);
var
    FNam: string;
begin
    if Len > 0 then
    begin
        if not OutputOpen then
        begin
            Inc(OutputNumber);
            FNam := IntToStr(OutputNumber);
            while Length(FNam) < 8 do
                FNam := '0' + FNam;
            AssignFile(OutputFile, cInPath + FNam);
            ReWrite(OutputFile, 1);
            OutputOpen := true;
        end;

        Dec(Len, 4);
        Move(Len, Buffer, 4);
        // BlockWrite(OutputFile, Len, SizeOf(Len));
        BlockWrite(OutputFile, Buffer, Len + 4);
    end;
end;


procedure StartNewConnection;
var
    Info: tUrlInfo;
    s: shortstring;
    i: integer;
begin
    if UrlNr < UrlAn then
    begin
        // CritSec.Enter;
        // Inc(CurConnections);
        tInterlocked.Increment(CurConnections);
        Inc(UrlNr);
        s := Urls[UrlNr]^;
        FreeMem(Urls[UrlNr]);
        // CritSec.Leave;

        if Pos('/', s) = 0 then s := s + '/';
        i := Pos(#8, s);
        if i = 0 then Info.ModifiedStr := ''
        else
        begin
            Info.ModifiedStr := copy(s, 1, i - 1);
            Delete(s, 1, i);
        end;
        Info.OrgUrl := s;

        if s = '' then LogMsg('robot.log', 'Empty URL at position ' + IntToStr(UrlNr));


        i := Pos(#255, s);
        if i = 0 then Info.UrlPos := ''
        else
        begin
            Info.UrlPos := copy(s, i + 1, 255);
            s := copy(s, 1, i - 1);
        end;

        Info.Url := s;
        if RobotsTxtStartCheckingFor(s) then
            Inc(NewConns);
        (* else if not RobotsTxtOneMinuteSinceLastAccess(Info.Url) then
        begin
            tInterlocked.Decrement(CurConnections);
            exit;
        end; *)

        i := Pos('/', s);
        if i = 0 then exit;
        Info.Path := Trim(copy(s, i, 255));
        s := copy(s, 1, i - 1);

        i := Pos(':', s);
        Info.Port := 80;
        if i > 0 then
        begin
            try
                Info.Port := StrToIntDef(copy(s, i + 1, 255), 80);
            except
                exit;
            end;
            if (Info.Port < 0) or (Info.Port > 65535) then exit;
            s := copy(s, 1, i - 1);
        end;

        Info.Domain := Trim(s);

        Inc(NewConns);
        tGetUrl.Create(Info).Start;
    end; { UrlNr<UrlAn }
end;


procedure tGetUrl.Cleanup;
begin
    try
        CritSec.Enter;
        try
            if (FSize > 4) and (Buffer <> nil) then
            begin
                // if Buffer[3]<>0 then
                WriteOutput(Buffer^, FSize);
                // else LogMsg('robot.log','Debug: Prevented obviously corrupted output-buffer from being written.');
            end;
        except
        end;
    finally
        CritSec.Leave;
        tInterlocked.Decrement(CurConnections);
    end;

    try
        MemPool(HTTPClientDefaultMaxSize).Release(Buffer);
        Buffer := nil;
    except
        LogMsg('robot.log', 'Exception while releasing buffer');
    end;
end;


procedure tGetUrl.ClearBuffer;
begin
    FSize := 4;
end;


procedure tGetUrl.ErrorAbort;
begin
    try
        Client.Close;
    except
    end;
    try
        Client.Free;
    except
    end;
    try
        SocketStream.Free;
    except
    end;

    ClearBuffer;
    WriteToBuffer(UrlInfo.OrgUrl + #13#10);
    WriteToBuffer('File transfer timed-out.'#13#10);
end;


procedure tGetUrl.ErrorUnknownHost;
begin
    WriteToBuffer('#ignore'#13#10);
    try
        Client.Close;
    except
    end;
    try
        Client.Free;
    except
    end;
    try
        SocketStream.Free;
    except
    end;
end;


procedure tGetUrl.AddRequest(const s: shortstring);
begin
    if ((Length(s) + ReqLen) <= 1024) and (Length(s) > 0) then
    begin
        Move(s[1], Request[ReqLen], Length(s));
        Inc(ReqLen, Length(s));
    end;
end;


procedure tGetUrl.Execute;
var
    s: shortstring;
    s2: string;
    i: integer;
    Le: int64;
    Ti1: int64;
    CrawlDelayPassed: boolean;
    Retries: integer;
    RobotsTxtValid: boolean;
begin
    // SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_BELOW_NORMAL);

    try
        Buffer := MemPool(HTTPClientDefaultMaxSize).Alloc;
    except
        LogMsg('robot.log', 'Failed to alloc Buffer');
        exit;
    end;

    ClearBuffer;
    try
        WriteToBuffer(UrlInfo.OrgUrl + #13#10);
        if not RobotsTxtIsUrlAllowed(UrlInfo.Url) then
        begin
            WriteToBuffer('#ignore'#13#10);
            WriteToBuffer('#Because of Robots.txt'#13#10);
            // LogMsg('robot.log','Blocked because of robots.txt: "'+UrlInfo.Url+'"');
            Cleanup;
            exit;
        end;
    except
        LogMsg('robot.log', 'Exception during robots.txt check for Url "' + UrlInfo.OrgUrl + '"');
        ErrorAbort;
        Cleanup;
        exit;
    end;



    Retries := 0;
    repeat
        CrawlDelayPassed := RobotsTxtCrawlDelayPassedSinceLastAccess(UrlInfo.Url);
        if not CrawlDelayPassed then Sleep(5000);
        Inc(Retries);
    until CrawlDelayPassed or (Retries > 60);

    {
    CrawlDelayPassed:= RobotsTxtCrawlDelayPassedSinceLastAccess(UrlInfo.Url);
    if not CrawlDelayPassed then
    begin
        ErrorAbort;
        Cleanup;
        (*
        LogMsg('robot.log', 'Skipping "' + UrlInfo.Url +
        '" because even after 10 minutes there was no time-window to access this host.');
        *)
        exit;
    end;
 }


    RobotsTxtValid := RobotsTxtIsRobotsTxtValid(UrlInfo.Url);
    if not RobotsTxtValid then
    begin
        DebugLogMsg('robot.log', 'Unable to get a valid robots.txt (or a "403" or "404") for "' + UrlInfo.Url + '".');
        ErrorAbort;
        Cleanup;
        exit;
    end;


    try
        Client := tClientSocket.Create(nil);
        Client.ClientType := ctBlocking;
        SocketStream := tWinSocketStream.Create(tCustomWinSocket(Client.Socket), 11000);

        Client.Port := UrlInfo.Port;
        // Client.Host := UrlInfo.Domain;
        Client.Address := Ip2Str(RobotsTxtGetIP(UrlInfo.Domain));
        if Client.Address = '0.0.0.0' then
            LogMsg('robot.log', 'Internal Error: IP of "' + UrlInfo.Domain + '" is 0.0.0.0');
    except
        LogMsg('robot.log', 'Exception while setting up SocketStream');
        ErrorAbort;
        Cleanup;
        exit;
    end;

    try
        Client.Open;
    except
        on E: eSocketError do
        begin
            s2 := E.Message;
            if Pos('10049', s2) > 0 then ErrorUnknownHost
            else
            begin
                ErrorAbort;
                CountFailedConnection;
            end;
            Cleanup;
            exit;
        end;
        else
        begin
            CountFailedConnection;
            ErrorAbort;
            Cleanup;
            exit;
        end;
    end;

    try
        ReqLen := 0;
        AddRequest('GET ' + UrlInfo.Path + ' HTTP/1.0'#13#10);
        AddRequest('Host: ' + UrlInfo.Domain + #13#10);
        AddRequest('User-Agent: ' + HTTPClientDefaultUserAgent + #13#10);
        AddRequest('Accept: */*'#13#10);

        AddRequest('Accept-Language: de-de,de,en-us,en'#13#10);
        // AddRequest('Accept-Language: en-us,en'#13#10);

        (* if UrlInfo.ModifiedStr <> '' then
            AddRequest('If-Modified-Since: ' + UrlInfo.ModifiedStr + #13#10); *)
        AddRequest(#13#10);
    except
        LogMsg('robot.log', 'Exception while setting up Request');
        CountFailedConnection;
        ErrorAbort;
        Cleanup;
        exit;
    end;

    try
        if SocketStream.Write(Request[0], ReqLen) <> ReqLen then
        begin
            ErrorAbort;
            CountFailedConnection;
            Cleanup;
            exit;
        end;
    except
        ErrorAbort;
        CountFailedConnection;
        Cleanup;
        exit;
    end;

    try
        for i := 1 to 100000 do
        begin
            Le := 0;
            try
                Ti1 := Ticks;
                try
                    Le := SocketStream.Read(s[1], 255);
                except
                    ErrorAbort;
                    Cleanup;
                    exit;
                end;
                if (Ticks - Ti1) > 10000 then
                begin
                    ErrorAbort;
                    Cleanup;
                    exit;
                end;
            except
                LogMsg('robot.log', 'Exception #2 in receive-loop.');
                ErrorAbort;
                Cleanup;
                exit;
            end;

            try
                SetLength(s, Le);
                if s = '' then break;
                WriteToBuffer(s);
                Inc(TotalBytes, Length(s));
                if FSize > HTTPClientDefaultMaxSize then
                begin
                    ClearBuffer;
                    WriteToBuffer(UrlInfo.OrgUrl + #13#10);
                    WriteToBuffer('#ignore'#13#10);
                    try
                        Client.Close;
                    except
                    end;
                    try
                        Client.Free;
                    except
                    end;
                    try
                        SocketStream.Free;
                    except
                    end;
                    Cleanup;
                    exit;
                end;
            except
                LogMsg('robot.log', 'Exception #3 in receive-loop.');
                ErrorAbort;
                Cleanup;
                exit;
            end;
        end;
    except
        LogMsg('robot.log', 'Exception in receive-loop.');
        ErrorAbort;
        Cleanup;
        exit;
    end;

    Le := 0;
    try
        Le := SocketStream.Read(s[1], 255);
    except
        ErrorAbort;
        Cleanup;
        exit;
    end;

    try
        SetLength(s, Le);
        WriteToBuffer(s);
        if FSize > HTTPClientDefaultMaxSize then
        begin
            ClearBuffer;
            WriteToBuffer(UrlInfo.OrgUrl + #13#10);
            WriteToBuffer('#ignore'#13#10);
        end;
        TotalBytes := TotalBytes + Length(s);
    except
        LogMsg('robot.log', 'Exception at finalizing.');
        ErrorAbort;
        Cleanup;
        exit;
    end;

    try
        Client.Close;
    except
    end;
    try
        Client.Free;
    except
    end;
    try
        SocketStream.Free;
    except
    end;

    try
        Cleanup;
    except
        LogMsg('robot.log', 'Exception at final CleanUp in GetUrl.Execute');
    end;
end;


constructor tGetUrl.Create;
begin
    inherited Create(true);

    FreeOnTerminate := true;
    UrlInfo := Info;
    ClearBuffer;
end;


procedure tGetUrl.WriteToBuffer(const s: AnsiString);
begin
    if Length(s) > 0 then
    begin
        if (FSize + Length(s)) <= HTTPClientDefaultMaxSize then
            Move(s[1], Buffer^[FSize], Length(s));
        Inc(FSize, Length(s));
    end;
end;



procedure LoadUrlList;
var
    f: TextFile;
    s: AnsiString;
begin
    AssignFile(f, cUrlList);
    Reset(f);
    UrlAn := 0;
    while not eof(f) do
    begin
        ReadLn(f, s);
        if (UrlAn < cMaxUrls) and (s <> '') and (Length(s) <= 255) then
        begin
            Inc(UrlAn);
            GetMem(Urls[UrlAn], Length(s) + 1);
            Urls[UrlAn]^ := s;
        end;
    end;
    CloseFile(f);
end;

begin
    CritSec := tCriticalSection.Create;
    OutputOpen := false;
    OutputNumber := 0;
    NewConns := 0;
    UrlNr := 0;
    LoadUrlList;

end.
