program Parser;

(*
  OpenAcoon - An OpenSource Internet-Search-Engine
  Copyright (C) 1999-2008 Acoon GmbH

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License version 2 as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

  -------------------------------------------------------------------------------

  ChangeLog:
  23-Apr-2008 MikeS Added: Parser now supports <base href="[url]">.
  12-May-2008 MikeS BugFix: Only the first 9,999,999 pages in a crawl could be
  parsed, because only files starting with a zero were searched for.
  16-May-2008 MikeS Modified: The Parser now acts accordingly to the
  index.language and index.max-page-size settings in the config-file.
  17-Max-2008 MikeS Modified: Merged OpenOutFile and OpenCatchemFiles
  procedures into a single OpenOutputFiles procedure.
  17-May-2008 MikeS Modified: Introduced changes to reflect the changes to
  RobotMain.pas.
  21-May-2008 MikeS BugFix: Quotation-marks at the end of links are now removed.
  26-May-2008 MikeS BugFix: Sometimes files weren't deleted after being parsed.
  17-Feb-2010 MikeS BugFix: Severe crash-bug in MakeFlatUrl when a URL started
  with "www.domain.com/../". This bug has been in the code for over 10 years!
*)

{$APPTYPE CONSOLE}


uses
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
