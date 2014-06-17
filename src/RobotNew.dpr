program RobotNew;

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

{$APPTYPE CONSOLE}

uses
    SysUtils,
    Windows,
    RobotsTxt in 'RobotsTxt.pas',
    HTTPClient in 'HTTPClient.pas',
    DNSResolver in 'DNSResolver.pas',
    Hash in 'Hash.pas',
    Logging in 'Logging.pas',
    GlobalTypes in 'GlobalTypes.pas',
    MemoryFile in 'MemoryFile.pas',
    MemoryPool in 'MemoryPool.pas',
    RobotGetUrl in 'RobotGetUrl.pas',
    FileLocation in 'FileLocation.pas',
    Config in 'Config.pas',
    OSWrapper in 'OSWrapper.pas';

var
    AdjConn: integer;
    Bytes: array [0 .. 4] of int64;
    ByteNr: integer;
    LastBytes: int64;
    StartTi, LastTick: int64;
    ConnCounter: integer;
    LastNewConn: array [0 .. 59] of integer;
    FailedCounter: integer;
    LastFailed: array [0 .. 59] of integer;
    IdleSeconds: integer;
    DataWrittenSinceLastTimer: boolean;


procedure Init;
var
    s: string;
begin
    DefaultCrawlDelay:=cThirtySeconds;
    IdleSeconds := 0;
    MaxConnections := 10;
    CurConnections := 0;
    AdjConn := 0;
    TotalBytes := 0;
    Bytes[0] := 0;
    Bytes[1] := 0;
    Bytes[2] := 0;
    Bytes[3] := 0;
    Bytes[4] := 0;
    ByteNr := 0;
    LastBytes := 0;
    ConnCounter := 0;
    FillChar(LastNewConn, SizeOf(LastNewConn), 0);
    FillChar(LastFailed, SizeOf(LastFailed), 0);

    HTTPClientDefaultMaxSize :=
    ConfigReadIntegerDefault('index.max-page-size', 200 * 1024);
    if HTTPClientDefaultMaxSize < 10240 then
        HTTPClientDefaultMaxSize := 10240;
    if HTTPClientDefaultMaxSize > 16 * 1024 * 1024 then
        HTTPClientDefaultMaxSize := 16 * 1024 * 1024;

    s := ConfigReadString('robot.useragent');
    if s <> '' then HTTPClientDefaultUserAgent := s;

    SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);

    StartTi := GetTickCount;
    LastTick := StartTi;
end;



procedure CloseOutputFile;
begin
    CritSec.Acquire;
    if OutputOpen then
    begin
        DataWrittenSinceLastTimer := true;
        CloseFile(OutputFile);
        OutputOpen := false;
    end;
    CritSec.Release;
end;



procedure HaltRobot;
begin
    CloseOutputFile;
    halt;
end;



procedure TimerEvent;
var
    Ti: int64;
    s: shortstring;
    i: integer;
    TrueConn: integer;
    ConnSum, FailedSum: integer;
begin
    DataWrittenSinceLastTimer := false;
    try
        CloseOutputFile;

        if DataWrittenSinceLastTimer then
            Write(#13'W ')
        else
            Write(#13'- ');


        NewConns := 0;
        try
            Bytes[ByteNr] := TotalBytes - LastBytes;
            LastBytes := TotalBytes;
            Ti := GetTickCount - LastTick;
            if Ti <= 0 then Ti := 1000;
                // Label10.Caption := IntToStr(Round(1.0 * Bytes[ByteNr] * 1000 / Ti));
            Str(8.0 * Bytes[ByteNr] / 1000.0 / Ti: 4: 2, s);
            Write(s, 'mbit/s ');

            LastTick := GetTickCount;
            ByteNr := (ByteNr + 1) mod 5;

            Ti := (GetTickCount - StartTi) div 1000;
            if Ti > 0 then
            begin
                Str(8.0 * TotalBytes / 1000000.0 / Ti: 4: 2, s);
                Write('(', s, ') ');
                Str(UrlNr / Ti: 4: 2, s);
                Write(s, ' pages/s; ');
            end;
        except
        end;

        if FileExists(cSpeedTrap) then
        begin
            Inc(AdjConn, 10);
            if AdjConn > 10000 then AdjConn := 10000;
        end
        else AdjConn := 0;
        TrueConn := MaxConnections - AdjConn;
        if TrueConn < 0 then TrueConn := 0;
        if (TrueConn < 5) and (AdjConn > 0) then
        begin
            TrueConn := 5;
            if TrueConn > MaxConnections then TrueConn := MaxConnections;
        end;
        // Label4.Caption := IntToStr(TrueConn) + '/' + IntToStr(MaxConnections);

        for i := 1 to MaxConnections do
        begin
            if (CurConnections < TrueConn) and (NewConns < 4000) then
                StartNewConnection;
        end;

        Write(CurConnections:4, ' connections; ');
        // Label6.Caption := IntToStr(CurConnections);
        Write('Progress: ', UrlNr, '/', UrlAn,'   ');
        // Label14.Caption := IntToStr(UrlNr) + '/' + IntToStr(UrlAn);

        ConnCounter := (ConnCounter + 1) mod 60;
        LastNewConn[ConnCounter] := NewConns;
        ConnSum := 0;
        for i := 0 to 59 do
            Inc(ConnSum, LastNewConn[i]);
        Str(1.0 * ConnSum / 60.0: 4: 2, s);
        // Label17.Caption := IntToStr(NewConns) + '/' + s;



        FailedCounter := (FailedCounter + 1) mod 60;
        LastFailed[FailedCounter] := GetAndResetFailedConnectionCount;
        FailedSum := 0;
        for i := 0 to 59 do
            Inc(FailedSum, LastFailed[i]);
        Str(1.0 * FailedSum / 60.0: 4: 2, s);
        // Label18.Caption := IntToStr(LastFailed[FailedCounter]) + '/' + s;

    except
    end;

    if (NewConns = 0) and (UrlNr = UrlAn) then
        Inc(IdleSeconds)
    else IdleSeconds := 0;

    if (IdleSeconds > 5) and (CurConnections = 0) and (not DataWrittenSinceLastTimer) then HaltRobot;
    // if (IdleSeconds > 75) and (not DataWrittenSinceLastTimer) then HaltRobot;
    if (IdleSeconds > 300) then HaltRobot;
end;


begin
    WriteLn('RobotNew ', cVersionCopy);
    WriteLn(cGPLNotice);

    Init;

    while true do
    begin
        TimerEvent;
        Sleep(1000);
    end;

end.
