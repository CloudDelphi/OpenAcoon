program ImportUrls;

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
    Classes,
    StrUtils,
    Hash in 'Hash.pas',
    DbTypes in 'DbTypes.pas',
    CacheFile in 'CacheFile.pas',
    GlobalTypes in 'GlobalTypes.pas',
    FileLocation in 'FileLocation.pas',
    Config in 'Config.pas',
    UrlDatabase in 'UrlDatabase.pas',
    OSWrapper in 'OSWrapper.pas';

const
    cMaxDupFilter = 4 * 1024 * 1024 - 1;
    cRoundRobinFile = 'ImportUrls.rr';

type
    tHashTable = array [0 .. cMaxUrlHash] of int32;
    pDupFilter = ^tDupFilter;

    tDupFilter = record
        Next: pDupFilter;
        Url: AnsiString;
    end;

var
    HashTable: array [0 .. cDbCount - 1] of tHashTable;
    HashTableModified: array [0 .. cDbCount - 1] of boolean;
    fOut: array [0 .. cDbCount - 1] of tPreloadedFile;
    Inf: array [0 .. cDbCount - 1] of tFileStream;
    Hosts: tStringList;
    OldUrlAn, UrlAn: array [0 .. cDbCount - 1] of int32;
    DupFilter: array [0 .. cMaxDupFilter] of pDupFilter;
    TimeStampInsteadOfBacklinks: boolean;


function IsDe(Url: shortstring): boolean;
var
    s2: string[3];
begin
    while Pos('/', Url) > 0 do
        Url := copy(Url, 1, Pos('/', Url) - 1);
    s2 := shortstring(LowerCase(copy(Url, Length(Url) - 2, 3)));
    Result := (s2 = '.de') or (s2 = '.at') or (s2 = '.ch');
end;


procedure AddUrlToDb(var Url: shortstring; Priority: tPriority);
var
    UrlData: tUrlData;
    HashCode: uint32;
    Po: integer;
    DoWrite: boolean;
    OldPr: tPriority;
    UrlDbNr: integer;
    b: byte;
    i: integer;
    LowerUrl: string;
begin
    if Url = '' then exit;

    i := Pos('&amp;', LowerCase(Url));
    if i > 0 then
    begin
        repeat
            Delete(Url, i + 1, 4);
            i := Pos('&amp;', LowerCase(Url));
        until i <= 0;
    end;


    b := 0;
    UrlDbNr := DbNrOfUrl(Url);
    HashCode := HashOfUrl(Url);
    // UrlDbNr := HashCode and (cDbCount - 1);
    // HashCode := (HashCode shr cDbBits) and cMaxUrlHash;

    LowerUrl := LowerCase(Url);
    Po := HashTable[UrlDbNr][HashCode];
    while Po <> 0 do
    begin
        try
            fOut[UrlDbNr].Seek(Po);
            fOut[UrlDbNr].Read(UrlData, SizeOf(UrlData));
        except
            WriteLn(#13, 'Cannot find URL: ', Url);
            exit;
        end;
        if LowerUrl = LowerCase(UrlData.Url) then
        begin
            OldPr := UrlData.Priority;
            // UrlData.Url := Url;
            if (UrlData.Priority <> Priority) and (Priority = prIgnore) and
            (UrlData.InfPo <> -1) then
            begin
                Inf[UrlDbNr].Position := UrlData.InfPo;
                Inf[UrlDbNr].Write(b, 1);
            end;

            case Priority of
                prIgnore: UrlData.Priority := Priority;
                prNormal:
                    begin
                        if (UrlData.Priority <> prIgnore) then
                        begin
                            UrlData.Priority := prNormal;
                        end;
                    end;

            end;

            DoWrite := OldPr <> UrlData.Priority;
            if not TimeStampInsteadOfBacklinks then
            begin
                Inc(UrlData.InLinkCount);
                DoWrite := true;
            end;

            if DoWrite then
            begin
                fOut[UrlDbNr].Seek(Po);
                fOut[UrlDbNr].Write(UrlData, SizeOf(UrlData));
            end;
            exit;
        end;
        if Po = UrlData.Next then
        begin
            WriteLn(#13'Self=Next at ', Url);
            exit;
        end;
        Po := UrlData.Next;
    end;


    // URL not found in database. Add it.

    // But do *not* add the URL if its extension is on the blocklist
    if UrlHasBlockedExtension(Url) then exit;

    UrlData.Url := Url;
    UrlData.Next := HashTable[UrlDbNr][HashCode];
    UrlData.InfPo := -1;

    if TimeStampInsteadOfBacklinks then
    begin
        UrlData.InLinkCount := Trunc(Now * 1000);
        // WriteLn('New InLinkCount=',UrlData.InLinkCount);
    end
    else UrlData.InLinkCount := 1;

    UrlData.Priority := Priority;
    Po := fOut[UrlDbNr].FileSize;
    fOut[UrlDbNr].Seek(Po);
    fOut[UrlDbNr].Write(UrlData, SizeOf(UrlData));
    HashTable[UrlDbNr][HashCode] := Po;
    HashTableModified[UrlDbNr] := true;
    Inc(UrlAn[UrlDbNr]);
end;


function Right(const s: shortstring; Len: integer): shortstring;
begin
    Result := shortstring(copy(s, Length(s) - Len + 1, Len));
end;


procedure AddUrl(Url: shortstring; Priority: tPriority);
var
    i, Po: integer;
    s2, s3, s4, s5: string[10];
    sl: shortstring;
    UrlLo: shortstring;
begin
    i := Pos(#255, Url);
    if i > 0 then SetLength(Url, i - 1);
    i := Pos(#8, Url);
    if i > 0 then Delete(Url, 1, i);

    if Length(Url) > 0 then
    begin
        if (Url[1] = ' ') or (Url[Length(Url)] = ' ') then
            Url := shortstring(Trim(Url));
    end;

    while LowerCase(copy(Url, 1, 7)) = 'http://' do
        Delete(Url, 1, 7);


    i := Pos(':', Url);
    if i > 0 then
        if i < Pos('/', Url) then exit;
    // The 3 lines above discard all URLs where a ":" occurs *before* the first "/".
    // Originally all URLs containing a ":" were discarded, but Wikipedia uses ":" in some of its URLs,
    // which made it necessary to change this behavior.

    if Url <> '' then
    begin
        if Pos('/', Url) = 0 then Url := Url + '/';

        if copy(Url, Length(Url) - 1, 2) = '//' then Url := copy(Url, 1, Length(Url) - 1);

        if Length(Url) > cMaxUrlLength then exit;

        Po := -1;
        UrlLo := shortstring(LowerCase(Url)) + #0;
        for i := 0 to Hosts.Count - 1 do
        begin
            SetLength(UrlLo, Length(Hosts.Strings[i]));
            if UrlLo = Hosts.Strings[i] then
            begin
                Po := i;
                break;
            end;
        end;

        sl := shortstring(LowerCase(Url));
        s2 := RightStr(sl, 2);
        s3 := RightStr(sl, 3);
        s4 := RightStr(sl, 4);
        s5 := RightStr(sl, 5);
        if (Po = -1) and (Url <> '') and
        (Url[1] <> '/') and (Url[1] >= '1') and
        (Pos('mailto:', sl) = 0) and
        (Pos('ftp:', sl) = 0) and
        (Pos('telnet:', sl) = 0) and
        (Pos('news:', sl) = 0) and
        (Pos('gopher:', sl) = 0) and
        (Pos('.htm/', sl) = 0) and
        (Pos('.html/', sl) = 0) and
        (Pos('/cgi-map/', sl) = 0) and
        (Pos(#39, sl) = 0) and
        (s2 <> '.z') then
        begin
            // if not UrlHasBlockedExtension(Url) then
            AddUrlToDb(Url, Priority);
        end;
    end;
end;


var
    UrlDbNr: integer;
    fIn: TextFile;
    s: AnsiString;
    i: integer;
    CurrentDbNr: integer;
    Sum: int64;
    EnablePreloading: boolean;
    LastPreloadDbNr: integer;
    DoPreload: boolean;
    DoRoundRobin: boolean;
    RRFile: TextFile;
    StartDbNr, EndDbNr: integer;
    StartTi, CurrentTi: int64;
    ElapsedSeconds: int64;
    MaxRuntime: int64;
    HashCode: integer;
    p: pDupFilter;


procedure CheckParameters;
var
    i: integer;
    s: string;
begin
    DoRoundRobin := false;
    StartDbNr := 0;
    EndDbNr := cDbCount - 1;
    DoPreload := false;
    MaxRuntime := 15 * 60; // The default for MaxRuntime is 15 minutes
    TimeStampInsteadOfBacklinks := false;

    i := 1;
    while i <= ParamCount do
    begin
        s := LowerCase(ParamStr(i));

        if (s = '-r') or (s = '-timestampinsteadofbacklinks') then TimeStampInsteadOfBacklinks := true;
        if (s = '-r') or (s = '-roundrobin') then DoRoundRobin := true;
        if (s = '-p') or (s = '-preload') then DoPreload := true;

        if (s = '-s') or (s = '-startdb') then
        begin
            Inc(i);
            StartDbNr := StrToIntDef(ParamStr(i), 0);
        end;

        if (s = '-e') or (s = '-enddb') then
        begin
            Inc(i);
            EndDbNr := StrToIntDef(ParamStr(i), cDbCount - 1);
        end;

        if (s = '-m') or (s = '-maxruntime') then
        begin
            Inc(i);
            MaxRuntime := StrToIntDef(ParamStr(i), 15 * 60);
            if MaxRuntime < 1 then MaxRuntime := 1; // no less than 1 second
            if MaxRuntime > 3600 then MaxRuntime := 3600; // no more than 1 hour
        end;

        Inc(i);
    end;

    if DoRoundRobin then EndDbNr := StartDbNr - 1;

    if StartDbNr < 0 then StartDbNr := 0;
    if StartDbNr > (cDbCount - 1) then StartDbNr := cDbCount - 1;

    if EndDbNr < 0 then EndDbNr := 0;
    if EndDbNr > (cDbCount - 1) then EndDbNr := cDbCount - 1;
end;


begin
    WriteLn('ImportUrls ', cVersionCopy);
    WriteLn(cGPLNotice);
    WriteLn;

    CheckParameters;
    if DoRoundRobin then
    begin
        if FileExists(cRoundRobinFile) then
        begin
            AssignFile(RRFile, cRoundRobinFile);
            Reset(RRFile);
            ReadLn(RRFile, StartDbNr);
            CloseFile(RRFile);
            if StartDbNr < 0 then StartDbNr := 0;
            if StartDbNr > (cDbCount - 1) then StartDbNr := cDbCount - 1;
        end;
    end
    else DeleteFile(cRoundRobinFile);


    Hosts := tStringList.Create;
    Hosts.Sorted := true;
    Hosts.Duplicates := dupIgnore;
    if FileExists(cIgnoreHosts) then
    begin
        AssignFile(fIn, cIgnoreHosts);
        Reset(fIn);
        while not Eof(fIn) do
        begin
            ReadLn(fIn, s);
            if Length(s) > 255 then s := copy(s, 1, 255);
            if s <> '' then Hosts.Add(LowerCase(s));
        end;
        CloseFile(fIn);
    end;

    Write('Loading URL-Index... ');
    for i := 0 to cDbCount - 1 do
    begin
        if FileExists(cInfDb + IntToStr(i)) then
            Inf[i] := tFileStream.Create(cInfDb + IntToStr(i), fmOpenReadWrite)
        else
            Inf[i] := tFileStream.Create(cInfDb + IntToStr(i), fmCreate or fmOpenReadWrite);

        fOut[i] := tPreloadedFile.Create;
        fOut[i].Assign(cUrlDb + IntToStr(i));
        if FileExists(cUrlDb + IntToStr(i)) then
        begin
            fOut[i].OpenReadWrite;
            FillChar(HashTable[i], SizeOf(tHashTable), 0);
            fOut[i].Read(HashTable[i], SizeOf(tHashTable));
            fOut[i].Read(UrlAn[i], 4);
        end
        else
        begin
            FillChar(HashTable, SizeOf(tHashTable), 0);
            UrlAn[i] := 0;
            fOut[i].OpenReadWrite;
            fOut[i].Write(HashTable, SizeOf(tHashTable));
            fOut[i].Write(UrlAn[i], 4);
        end;
        OldUrlAn[i] := UrlAn[i];

        HashTableModified[i] := false;
    end;
    WriteLn('Done.');

    if FileExists(cIgnorePages) then
    begin
        i := 0;
        AssignFile(fIn, cIgnorePages);
        Reset(fIn);
        while not Eof(fIn) do
        begin
            ReadLn(fIn, s);
            if Length(s) > 255 then s := copy(s, 1, 255);
            if s <> '' then AddUrl(s, prIgnore);
            Inc(i);
            if (i and 15) = 0 then Write(#13, 'IgnorePages: ', i);
        end;
        WriteLn(#13, 'IgnorePages: ', i);
        CloseFile(fIn);
        DeleteFile(cIgnorePages);
    end;

    if FileExists(cAddUrl) then
    begin
        i := 0;
        AssignFile(fIn, cAddUrl);
        Reset(fIn);
        while not Eof(fIn) do
        begin
            ReadLn(fIn, s);
            if Length(s) > 255 then s := copy(s, 1, 255);
            if s <> '' then AddUrl(shortstring(s), prNormal);
            Inc(i);
            if (i and 15) = 0 then Write(#13, 'AddUrl: ', i);
        end;
        WriteLn(#13, 'AddUrl: ', i);
        CloseFile(fIn);
        DeleteFile(cAddUrl);
    end;

    EnablePreloading := DoPreload;
    LastPreloadDbNr := -1;

{$DEFINE NoDupFiltering}
    // In order for the InLinkCount to work properly there must NOT
    // be any filtering of duplicate URLs.

    i := 0;
    FillChar(DupFilter, SizeOf(DupFilter), 0);
    StartTi := GetTickCount;
    CurrentDbNr := StartDbNr;
    repeat
        if FileExists(cUrls + IntToStr(CurrentDbNr)) then
        begin
            AssignFile(fIn, cUrls + IntToStr(CurrentDbNr));
            Reset(fIn);
            while not Eof(fIn) do
            begin
                ReadLn(fIn, s);
                if Length(s) <= 255 then
                begin
{$IFNDEF NoDupFiltering}
                    HashCode := CalcCRC(s) and cMaxDupFilter;
                    p := DupFilter[HashCode];
                    while p <> nil do
                    begin
                        if p.Url = s then break
                        else p := p.Next;
                    end;

                    if p = nil then
                    begin
                        New(p);
                        p.Next := DupFilter[HashCode];
                        p.Url := s;
                        DupFilter[HashCode] := p;
{$ENDIF}
// UrlDbNr := DbNrOfUrl(s);
                        UrlDbNr := CurrentDbNr;
                        if EnablePreloading then
                        begin
                            if UrlDbNr <> LastPreloadDbNr then
                            begin
                                if LastPreloadDbNr <> -1 then fOut[LastPreloadDbNr].UnloadCache;
                                LastPreloadDbNr := UrlDbNr;
                                fOut[UrlDbNr].Preload;
                            end;
                        end;

                        if s <> '' then AddUrl(shortstring(s), prNormal);
{$IFNDEF NoDupFiltering}
                    end;
{$ENDIF}
                    Inc(i);
                    if (i and 1023) = 0 then Write(#13, 'URLs: ', i, ' (', LastPreloadDbNr, ')    ');
                end;
            end;
            CloseFile(fIn);
            DeleteFile(cUrls + IntToStr(CurrentDbNr));
        end;

        if CurrentDbNr = EndDbNr then break;
        CurrentDbNr := (CurrentDbNr + 1) mod cDbCount;

        AssignFile(RRFile, cRoundRobinFile);
        ReWrite(RRFile);
        WriteLn(RRFile, CurrentDbNr);
        CloseFile(RRFile);

        CurrentTi := GetTickCount;
        ElapsedSeconds := (CurrentTi - StartTi) div 1000;
        if ElapsedSeconds > MaxRuntime then break;
    until false;
    WriteLn(#13, 'URLs: ', i, ' (', LastPreloadDbNr, ')    ');

    Sum := 0;
    for i := 0 to cDbCount - 1 do
        Inc(Sum, UrlAn[i]);
    WriteLn('There are now ', Sum, ' URLs in the database.');
    for i := 0 to cDbCount - 1 do
        Dec(Sum, OldUrlAn[i]);
    WriteLn(Sum, ' URLs were added to the database.');

    Write('Writing URL-Index... ');
    for i := 0 to cDbCount - 1 do
    begin
        if HashTableModified[i] then
        begin
            fOut[i].Seek(0);
            fOut[i].Write(HashTable[i], SizeOf(tHashTable));
            fOut[i].Write(UrlAn[i], 4);
        end;
        fOut[i].Close;
        Inf[i].Free;
    end;
    WriteLn('Done.');

end.
