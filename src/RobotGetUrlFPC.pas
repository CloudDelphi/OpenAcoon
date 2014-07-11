unit RobotGetUrlFPC;

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
    SyncObjs,
    httpget,
    robotglobal,
    DNSResolver,
    Classes;

type
    tMyByteArray = array [0 .. 2000 * 1000 * 1000] of byte;
    pMyByteArray = ^tMyByteArray;



    (*
    tGetUrl = class(tThread)
    private
        UrlInfo: tUrlInfo;
        Client: tHttpGet;
        IP: tIP4;
	FSize: int32;
        procedure ErrorAbort;
        procedure ErrorUnknownHost;
        procedure ClearBuffer;
        procedure WriteToBuffer(const s: AnsiString);
    protected
        procedure Execute; override;
        procedure Cleanup;
    public
        constructor Create(Info: tUrlInfo);
    end;
    *)


var
    CritSec: tCriticalSection;
    MaxConnections, CurConnections: integer;
    TotalBytes: int64;
    OutputOpen: boolean;
    OutputFile: file;
    UrlNr: integer;
    UrlAn: integer;
    NewConns: integer;
    UserAgent: string;



procedure StartNewConnection;





implementation

uses
    FileLocation,
    MemoryPool,
    //HTTPClientFPC,
    Logging,
    RobotsTxt,
    idGlobal,
    geturl,
    Config,
    SysUtils;



const
    cMaxUrls = 100000000; // 100 million



var
    OutputNumber: integer;
    Urls: array [1 .. cMaxUrls] of pShortString;




(*
    WriteOutput will be called from GetUrlComplete each time
    a tGetUrl completes.

    CritSec must be acquired before (!) calling this procedure and
    released after calling it.

    The purpose of this is to have a *single* write-operation.

    Back when there were two writes (the size and the actual data)
    this occasionally caused corrupted output. I have no idea why, as
    the use of a tCriticalSection should have prevented that.
*)
procedure WriteOutput(var InBuffer: array of byte; Len: int32);
var
    FNam: string;
    OutBuffer: array of byte;
begin
    if Len > 0 then
    begin
	Inc(TotalBytes, Len);
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

	SetLength(OutBuffer, Len + 4);
	Move(Len, OutBuffer[0], 4);
	Move(InBuffer[0], OutBuffer[4], Len);
        BlockWrite(OutputFile, OutBuffer[0], Len +4);
    end;
end;




procedure GetUrlComplete(var Buffer: array of byte; BufLen: int32);
begin
    try
        CritSec.Acquire;
        try
            if BufLen > 4 then
                WriteOutput(Buffer, BufLen);
        except
        end;

    finally
	Dec(CurConnections);
        CritSec.Release;

    end;
end;



procedure StartNewConnection;
var
    Info: tUrlInfo;
    ThisGetUrl: tGetUrl;
    s: shortstring;
    i: integer;
begin
    if UrlNr < UrlAn then
    begin
        CritSec.Enter;
        Inc(CurConnections);
        // tInterlocked.Increment(CurConnections);
        Inc(UrlNr);
        s := Urls[UrlNr]^;
        FreeMem(Urls[UrlNr]);
        CritSec.Leave;

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
	ThisGetUrl := tGetUrl.Create(Info);
	ThisGetUrl.OnComplete := GetUrlComplete;
	ThisGetUrl.Start;
    end; { UrlNr<UrlAn }
end;




{$ifdef DoNotCompileThisAndRemoveItAfterRefactoring}
procedure tGetUrl.Cleanup;
begin
    try
        CritSec.Enter;
        try
            if (FSize > 4) and (Buffer <> nil) then
            begin
                WriteOutput(Buffer^, FSize);
            end;
        except
        end;
    finally
	Dec(CurConnections);
        CritSec.Leave;
    end;
end;





procedure tGetUrl.ClearBuffer;
begin
    FSize := 4;
end;


procedure tGetUrl.ErrorAbort;
begin
    try
        Client.Active:=false;
    except
    end;
    try
        Client.Free;
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
        Client.Active:=false;
    except
    end;
    try
        Client.Free;
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

    RobotsTxtValid := RobotsTxtIsRobotsTxtValid(UrlInfo.Url);
    if not RobotsTxtValid then
    begin
        DebugLogMsg('robot.log', 'Unable to get a valid robots.txt (or a "403" or "404") for "' + UrlInfo.Url + '".');
        ErrorAbort;
        Cleanup;
        exit;
    end;


    try
        Client := tTCPClient.Create(nil);

        Client.Port := UrlInfo.Port;
        Client.Host := Ip2Str(RobotsTxtGetIP(UrlInfo.Domain));
        if Client.Host = '0.0.0.0' then
            LogMsg('robot.log', 'Internal Error: IP of "' + UrlInfo.Domain + '" is 0.0.0.0');
    except
        LogMsg('robot.log', 'Exception while setting up SocketStream');
        ErrorAbort;
        Cleanup;
        exit;
    end;

    try
        Client.Active:=true;
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
        AddRequest(#13#10);
    except
        LogMsg('robot.log', 'Exception while setting up Request');
        CountFailedConnection;
        ErrorAbort;
        Cleanup;
        exit;
    end;

    try
        if Client.Stream.Write(Request[0], ReqLen) <> ReqLen then
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
                    Le := Client.Stream.Read(s[1], 255);
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
                        Client.Active:=false;
                    except
                    end;
                    try
                        Client.Free;
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
        Le := Client.Stream.Read(s[1], 255);
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
        Client.Active:=false;
    except
    end;
    try
        Client.Free;
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
{$endif}




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
    UserAgent := ConfigReadString('robot.useragent');
    CritSec := tCriticalSection.Create;
    OutputOpen := false;
    OutputNumber := 0;
    NewConns := 0;
    UrlNr := 0;
    LoadUrlList;
end.
