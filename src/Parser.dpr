program Parser;

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
    {$ifdef Unix}
    cthreads,
    cmem,
    {$endif}
    OSWrapper,
    SysUtils,
    Classes,
    FileLocation in 'FileLocation.pas',
    GlobalTypes in 'GlobalTypes.pas',
    Config in 'Config.pas',
    DbTypes in 'DbTypes.pas',
    ParserClass in 'ParserClass.pas',
    MemoryBuffer in 'MemoryBuffer.pas',
    Logging in 'Logging.pas',
    Words in 'Words.pas',
    CacheFile in 'CacheFile.pas';

type
    tEntry = record
        Next: integer;
        StartPo: integer;
        Len: integer;
        Url: shortstring;
    end;



var
    FollowLinks, ProcessBackLinks: boolean;
    MaxBufLen: integer;
    IndexLanguage: string;
    i: integer;
    ParserThread: array [0 .. 9] of tParserClass;
    Seconds: integer;
    StillRunning: integer;
    f: File;
    s: string;

begin
    {$ifndef Unix}
    System.NeverSleepOnMMThreadContention := true;
    {$endif}
    WriteLn('Parser ', cVersionCopy);
    WriteLn(cGPLNotice);
    WriteLn;
    WriteLn('Usage: PARSER [/nolinks] [/nobacklinks]');
    WriteLn;

    // SetThreadPriority(GetCurrentThread, Thread_Priority_Below_Normal);
    MaxBufLen := ConfigReadIntegerDefault('index.max-page-size', 200 * 1024);
    if MaxBufLen < 10240 then
        MaxBufLen := 10240;
    if MaxBufLen > 16 * 1024 * 1024 then
        MaxBufLen := 16 * 1024 * 1024;
    IndexLanguage := LowerCase(ConfigReadStringDefault('index.language', 'all'));

    FollowLinks := true;
    ProcessBackLinks := true;
    for i := 1 to ParamCount do
    begin
        s := LowerCase(ParamStr(i));
        if s = '/nolinks' then FollowLinks := false
        else if s = '/nobacklinks' then ProcessBackLinks := false;
    end;

    Randomize;

    for i := 0 to 9 do
    begin
        ParserThread[i] := tParserClass.Create('???????' + IntToStr(i), IndexLanguage, FollowLinks, ProcessBackLinks,
        MaxBufLen);
        ParserThread[i].Start;
        Sleep(10);
    end;

    Seconds := 0;
    repeat
        Sleep(1000);
        Inc(Seconds);
        StillRunning := 0;
        for i := 0 to 9 do
            if not ParserThread[i].Finished then Inc(StillRunning);
        WriteLn(#13, Seconds, ' ', StillRunning, ' ');
    until StillRunning = 0;
    WriteLn;

    if Seconds >= 60 then
    begin
        try
            AssignFile(f, cSpeedTrap);
            ReWrite(f, 1);
            CloseFile(f);
        except
        end;
    end
    else DeleteFile(cSpeedTrap);

    for i := 0 to 9 do
        ParserThread[i].Free;

end.
