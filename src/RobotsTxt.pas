unit RobotsTxt;

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
    robotglobal,
    DNSResolver;

const
    cMaxRobotsHash = 1024 * 1024 - 1;
    cOneMinute = 1.0 / 24.0 / 60.0;
    cFiveSeconds = cOneMinute / 60.0 * 5.0;
    cTenSeconds = cOneMinute / 60.0 * 10.0;
    cTwentySeconds = cOneMinute / 60.0 * 20.0;
    cThirtySeconds = cOneMinute / 60.0 * 30.0;

var
    DefaultCrawlDelay: double;


function RobotsTxtStartCheckingFor(AUrl: shortstring): boolean;
function RobotsTxtIsUrlAllowed(AUrl: shortstring): boolean;
function RobotsTxtCrawlDelayPassedSinceLastAccess(AUrl: shortstring): boolean;
function RobotsTxtIsRobotsTxtValid(AUrl: shortstring): boolean;
function RobotsTxtGetIP(AUrl: shortstring): tIP4;



implementation

uses
    {$ifdef Unix}
    cthreads,
    {$endif}
    SysUtils,
    {$ifdef DCC}
    Windows,
    {$endif}
    httpget,
    Logging,
    Hash,
    GlobalTypes,
    // Dialogs,
    Classes,
    SyncObjs;


type
    pCacheElement = ^tCacheElement;

    tCacheElement = record
        Next: pCacheElement;
        StatusCode: integer;
        IP: tIP4;
        LastAccess: tDateTime;
        BlockedPaths: tStringList;
        Valid: boolean;
        HostName: shortstring;
    end;

    tRobotsTxt = class(tThread)
    private
        CacheElement: pCacheElement;
    protected
        procedure Execute; override;
    public
        constructor Create(ACacheElement: pCacheElement);
    end;

var
    HashTable: array [0 .. cMaxRobotsHash] of pCacheElement;
    CritSec: tCriticalSection;


function Url2HostName(AUrl: shortstring): shortstring;
var
    i: integer;
begin
    i := Pos('/', AUrl);
    if i > 0 then AUrl := copy(AUrl, 1, i - 1);
    Result := LowerCase(AUrl);
end;


constructor tRobotsTxt.Create(ACacheElement: pCacheElement);
begin
    inherited Create(true);
    CacheElement := ACacheElement;
    FreeOnTerminate := true;
end;


procedure tRobotsTxt.Execute;
var
    Cl: tHttpGet;
    s: String;
    IsValidUA: boolean;
    Redirects: integer;
    ReadyToExit: boolean;
    RobotsTxtUrl: String;
    ThisIP: tIP4;
begin
    //SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_BELOW_NORMAL);
    DebugLogMsg('robot.log', 'Hostname=' + CacheElement.HostName);
    CacheElement.IP := GetIP4ByHostName(AnsiString(CacheElement.HostName));
    DebugLogMsg('robot.log', 'IP for ' + CacheElement.HostName + ' is ' + Ip2Str(CacheElement.IP));
    if CacheElement.IP.IP = 0 then
    begin
        CacheElement.StatusCode := 950; // This will ensure that no actual URLs will be crawled from this host
        CacheElement.Valid := true;
        exit;
    end;

    Redirects := 0;
    RobotsTxtUrl := CacheElement.HostName + '/robots.txt';
    ThisIP := CacheElement.IP;

    repeat
        DebugLogMsg('robot.log', 'RobotsTxt at "' + RobotsTxtUrl + '"');
        Cl := tHttpGet.Create;
	Cl.Host := CacheElement.HostName;
	Cl.IP := CacheElement.IP;
	Cl.Path := '/robots.txt';
	Cl.Get;

        ReadyToExit := true;
        if (Cl.ErrorCode = 0) and (Redirects < 5) then
        begin
            DebugLogMsg('robot.log', 'Actual StatusCode is ' + IntToStr(Cl.GetStatusCode));
            if (Cl.GetStatusCode = 301) or (Cl.GetStatusCode = 302) then
            begin
                Inc(Redirects);
                RobotsTxtUrl := '';
                Cl.Seek(0);
                DebugLogMsg('robot.log', 'Checking for new location...');
                while not Cl.Eof do
                begin
                    s := Cl.ReadLine;
                    if s = '' then break;
                    if LowerCase(copy(s, 1, 9)) = 'location:' then
                    begin
                        Delete(s, 1, 9);
                        s := Trim(s);
                        DebugLogMsg('robot.log', 'Found new location ' + s);
                        if LowerCase(copy(s, 1, 7)) = 'http://' then Delete(s, 1, 7);
                        if LowerCase(copy(s, 1, 8)) = 'https://' then Delete(s, 1, 8);
                        DebugLogMsg('robot.log', 'Setting robots.txt to ' + s);
                        RobotsTxtUrl := s;
                        ThisIP.IP := 0;
                        ReadyToExit := false;
                        Cl.Free;
                        Cl:=nil;
                        break;
                    end;
                end;
                if Assigned(Cl) then Cl.Seek(0);
            end;

        end;
    until ReadyToExit;


    if Cl.ErrorCode = 0 then
    begin
        CacheElement.StatusCode := Cl.GetStatusCode;

        if CacheElement.StatusCode = 200 then
        begin
            IsValidUA := false;
            while not Cl.Eof do
            begin
                s := LowerCase(Cl.ReadLine);
                if copy(s, 1, 11) = 'user-agent:' then
                begin
                    Delete(s, 1, 11);
                    s := Trim(s);
                    IsValidUA := (Pos('acoon', s) > 0) or (s = '*');
                end
                else if copy(s, 1, 9) = 'disallow:' then
                begin
                    Delete(s, 1, 9);
                    s := Trim(s);
                    if IsValidUA and (s <> '') then
                        CacheElement.BlockedPaths.Add(s);
                end;
            end;
        end;
    end
    else CacheElement.StatusCode := 989;

    try
        // DebugLogMsg('robot.log','Freeing HttpClient in tRobotsTxt');
        Cl.Free;
    except
    end;

    CacheElement.Valid := true;
end;


function FindHashEntry(AUrl: String): pCacheElement;
var
    p: pCacheElement;
    HashCode: uint32;
    HostName: shortstring;
begin
    // DebugLogMsg('robot.log','Searching for robots.txt entry for "'+AUrl+'"');
    HostName := Url2HostName(AUrl);
    HashCode := CalcCRC(HostName) and cMaxRobotsHash;
    p := HashTable[HashCode];
    while p <> nil do
    begin
        if p^.HostName = HostName then break
        else p := p^.Next;
    end;

    (*
    if p=nil then
        DebugLogMsg('robot.log','Did NOT find a robots.txt entry for "'+AUrl+'"')
    else
        DebugLogMsg('robot.log','Successfully found a robots.txt entry for "'+AUrl+'"');
 *)

    Result := p;
end;


function RobotsTxtStartCheckingFor(AUrl: shortstring): boolean;
var
    p: pCacheElement;
    HashCode: uint32;
    Ro: tRobotsTxt;
begin
    CritSec.Enter;
    p := FindHashEntry(AUrl);
    if p = nil then
    begin
        // DebugLogMsg('robot.log','Creating robots.txt entry for "'+AUrl+'"');
        AUrl := Url2HostName(AUrl);
        HashCode := CalcCRC(AUrl) and cMaxRobotsHash;
        GetMem(p, SizeOf(tCacheElement) - 255 + Length(AUrl));
        p.Next := HashTable[HashCode];
        p.Valid := false;
        p.StatusCode := 0;
        p.IP.IP := 0;
        p.BlockedPaths := tStringList.Create;
        p.BlockedPaths.Sorted := true;
        p.BlockedPaths.Duplicates := dupIgnore;
        p.HostName := AUrl;
        p.LastAccess := Now - 2 * cOneMinute;
        HashTable[HashCode] := p;
        // Dialogs.MessageDlg('RobotsTxtStartChecking for "' + AUrl + '"',
        // mtInformation, [mbOk], 0, mbOk);

        Ro := tRobotsTxt.Create(p);
        Ro.Start;
        Result := true;
    end
    else Result := false;
    CritSec.Leave;
end;


function RobotsTxtCrawlDelayPassedSinceLastAccess(AUrl: shortstring): boolean;
var
    p: pCacheElement;
    CurrentTime: tDateTime;
    HostName: shortstring;
    MinimumDelay: double;
begin
    // exit(true);
    HostName := LowerCase(AUrl);
    if copy(HostName, 1, 7) = 'http://' then Delete(HostName, 1, 7);
    if copy(HostName, 1, 8) = 'https://' then Delete(HostName, 1, 8);

    // Default Crawl-Delay is usually 60 seconds unless overridden by an application
    MinimumDelay := DefaultCrawlDelay;

    // We need LOTS of Wikipedia pages in the index, so set the
    // Crawl-Delay to 5 seconds for these hosts.
    if (HostName = 'de.wikipedia.org') or (HostName = 'en.wikipedia.org') or
    (HostName = 'www.wikipedia.de') or (HostName = 'www.wikipedia.org') then MinimumDelay := cFiveSeconds;

    // We also want as many Reddit.com pages in the index, so set the
    // Crawl-Delay to 20 seconds for that host.
    if (HostName = 'www.reddit.com') then MinimumDelay := cTwentySeconds;

    if (HostName = 'feedproxy.google.com') then MinimumDelay := cFiveSeconds;

    CritSec.Enter;
    p := FindHashEntry(AUrl);
    if p = nil then Result := false // In theory this can never happen
    else
    begin
        CurrentTime := Now;
        if CurrentTime < p.LastAccess then Result := false
            // Can only happen when changing back from Daylight-Savings-Time
        else Result := CurrentTime > (p.LastAccess + MinimumDelay);
        if Result then p.LastAccess := CurrentTime;
    end;
    CritSec.Leave;
end;



function RobotsTxtIsUrlAllowed(AUrl: shortstring): boolean;
var
    p: pCacheElement;
    i: integer;
    OriginalUrl: shortstring;
begin
    OriginalUrl := AUrl;

    p := FindHashEntry(AUrl);
    if p = nil then
    begin // Dies sollte eigentlich nie vorkommen...
        Result := true; // Geht nur, wenn RobotsTxtStartCheckingFor nicht
        exit; // für diese URL aufgerufen wurde.
    end;

    for i := 1 to 300 do // 300 * 0,1s = 30s Timeout
    begin
        if p^.Valid then break;
        Sleep(100);
    end;

    if not p^.Valid then
    begin
        Result := true; // Falls der Timeout erreicht wurde, nehmen wir an,
        exit; // dass die URL erlaubt ist.
    end;

    AUrl := LowerCase(AUrl);
    i := Pos('/', AUrl);
    if i > 0 then Delete(AUrl, 1, i - 1);

    for i := 0 to p^.BlockedPaths.Count - 1 do
    begin
        if copy(AUrl, 1, Length(p^.BlockedPaths.Strings[i])) =
        p^.BlockedPaths.Strings[i] then
        begin
            // DebugLogMsg('robot.log', OriginalUrl + ' blocked by robots.txt');

            // Dialogs.MessageDlg('Blocked for "' + AUrl + '"',
            // mtInformation, [mbOk], 0, mbOk);
            Result := false;
            exit;
        end;
    end;
    // Dialogs.MessageDlg('Ok for "' + AUrl + '"',
    // mtInformation, [mbOk], 0, mbOk);
    Result := true;
end;


function RobotsTxtIsRobotsTxtValid(AUrl: shortstring): boolean;
var
    p: pCacheElement;
begin
    CritSec.Enter;

    p := FindHashEntry(AUrl);
    if p = nil then Result := false // In theory this can never happen
    else
    begin
        // DebugLogMsg('robot.log','StatusCode='+IntTostr(p.StatusCode)+' for "'+AUrl+'"');
        if
	    (p.StatusCode = 200) or
	    (p.StatusCode = 404) or
	    (p.StatusCode = 403) then Result := true
        else Result := false;
    end;

    CritSec.Leave;
end;


function RobotsTxtGetIP(AUrl: shortstring): tIP4;
var
    p: pCacheElement;
begin
    CritSec.Enter;

    p := FindHashEntry(AUrl);
    if p = nil then Result.IP := 0 // In theory this can never happen
    else
    begin
        if p.Valid then Result := p.IP
        else Result.IP := 0;
    end;

    CritSec.Leave;
end;



begin
    FillChar(HashTable, SizeOf(HashTable), 0);
    CritSec := tCriticalSection.Create;
    DefaultCrawlDelay := cOneMinute;

end.
