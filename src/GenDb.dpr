program GenDb;

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
    OSWrapper,
    Classes,
    CacheFile in 'CacheFile.pas',
    DbTypes in 'DbTypes.pas',
    Hash in 'Hash.pas',
    FileLocation in 'FileLocation.pas',
    GlobalTypes in 'GlobalTypes.pas',
    DomainRank in 'DomainRank.pas',
    UrlDatabase in 'UrlDatabase.pas';

type
    TProgress = record
        Pass: integer;
        Step: integer;
    end;

    pIntArray = ^tIntArray;
    tIntArray = array [1 .. 40 * 1000 * 1000] of integer;
    Str255 = ShortString;
    pStr255 = ^Str255;

    tUrlInfo = packed record
        Url: string[cMaxUrlLength];
        Title: string[cMaxTitleLength];
        Description: ShortString;
    end;


var
    Progress: TProgress;
    F: file;
    F2: TextFile;
    S: ShortString;
    IgHosts: array [1 .. 2000] of pStr255;
    IgHostAn: integer;
    IgDomains: array [1 .. 2000] of pStr255;
    IgDomainAn: integer;
    UniqueWords: int64;
    WordOccurences: int64;
    BackLinksFromAge: boolean;
    OutputDir: string;
    // FilterAn: integer;
    // FiAn: array[1..8] of integer;
    // Fi: array[1..8,1..2000] of pstr255;
    // FilterFound: array[1..8] of integer;



procedure WriteProgress;
var
    F: tFileStream;
begin
    F := tFileStream.Create('gendb.progress', fmCreate);
    F.Write(Progress, SizeOf(Progress));
    F.Free;
end;



procedure WriteStr(var fi: file; var S: ShortString);
begin
    BlockWrite(fi, S, Length(S) + 1);
end;



function Zahl(i, le: integer): ShortString;
var
    S: ShortString;
    j: integer;
begin
    S := IntToStr(i);
    while Length(S) < le do
    begin
        S := ' ' + S;
    end;

    for j := 1 to Length(S) do
    begin
        if S[j] = ' ' then
        begin
            S[j] := '0';
        end;
    end;
    Result := S;
end;


{ ---------------------------------------------------------- }


procedure DoPass1;
var
    Inf: tCacheFile;
    UrlDat: tBufWriteFile;
    An: integer;
    PageInfo: tPageInfo;
    TempKey: array [0 .. 63] of tBufWriteFile;
    DbNr, i, j, k, Po: integer;
    IgTmp: ShortString;
    DoAdd: boolean;
    ThisTime, StartTime, Sec: int64;
    UrlPtr: integer;
    b: byte;
    // p: pstr255;
    FilterOut: tBufWriteFile;
    ThisKeyword: ShortString;
    RankingFlags: byte;
    MaxAllowedDocId: uint32;
begin
    WriteLn('Pass 1 - Preparing URL-data');

    if FileExists(OutputDir + 'urls.dat0') then
    begin
        WriteLn(
        'Fatal Error: There are already files in the destination directory!'
        );
        halt;
    end;

    // FillChar(FiAn,SizeOf(FiAn),0);
    // FilterAn:=0;

    for i := 0 to 63 do
    begin
        TempKey[i] := tBufWriteFile.Create;
        TempKey[i].Assign(cTempPath + 'keys.' + IntToStr(i));
        TempKey[i].ReWrite;
    end;

    // Calculate the highest DocId that will still fit into the RWI data-structures
    MaxAllowedDocId := (512 * 1024 * 1024 div cDbCount) - 1;

    for DbNr := 0 to (cDbCount - 1) do
    begin
        UrlDat := tBufWriteFile.Create;
        UrlDat.Assign(OutputDir + 'urls.dat' + IntToStr(DbNr));
        UrlDat.ReWrite;
        Inf := tCacheFile.Create;
        Inf.Assign(cInfDb + IntToStr(DbNr));
        Inf.Reset;
        FilterOut := tBufWriteFile.Create;
        FilterOut.Assign(OutputDir + 'filter.dat' + IntToStr(DbNr));
        FilterOut.ReWrite;

        An := 0;
        StartTime := GetTickCount;

        while not Inf.Eof do
        begin
            Inf.Read(PageInfo, SizeOf(PageInfo));
            j := -1;
            if PageInfo.Url <> '' then
            begin
                IgTmp := LowerCase(PageInfo.Url);
                Po := Pos('/', IgTmp);
                if Po > 0 then
                    IgTmp := copy(IgTmp, 1, Po - 1);

                repeat { Den Hostnamen auf die eigentliche Domain reduzieren }
                    k := 0;
                    for i := 1 to Length(IgTmp) do
                        if IgTmp[i] = '.' then
                            Inc(k);
                    if k > 1 then
                        delete(IgTmp, 1, Pos('.', IgTmp));
                until k <= 1;

                for i := 1 to IgDomainAn do
                    if IgDomains[i]^ = IgTmp then
                    begin
                        j := i;
                        break;
                    end;

                if j = -1 then
                begin
                    IgTmp := LowerCase(PageInfo.Url);

                    for i := 1 to IgHostAn do
                    begin
                        SetLength(IgTmp, Length(IgHosts[i]^));
                        if IgTmp = IgHosts[i]^ then
                        begin
                            j := i;
                            break;
                        end;
                    end;
                end;

                DoAdd := (j = -1) (* and (Pos('?', PageInfo.Url) = 0) *) and
                (Pos(#39, PageInfo.Url) = 0);
                if DoAdd then
                begin
                    UrlPtr := (An shl cDbBits) or DbNr;
                    for i := 1 to PageInfo.WordCount do
                    begin
                        Inf.Read(RankingFlags, 1);
                        Inf.Read(ThisKeyword[0], 1);
                        if Length(ThisKeyword) <> 0 then Inf.Read(ThisKeyword[1], Length(ThisKeyword));
                        j := CalcCRC(ThisKeyword) and 63;
                        TempKey[j].Write(RankingFlags, 1);
                        TempKey[j].Write(ThisKeyword[0], Length(ThisKeyword) + 1);
                        TempKey[j].Write(UrlPtr, 4);
                    end;

                    PageInfo.Title := Trim(PageInfo.Title);
                    PageInfo.Description := Trim(PageInfo.Description);
                    UrlDat.Write(PageInfo.Url, SizeOf(PageInfo.Url));
                    UrlDat.Write(PageInfo.Title, SizeOf(PageInfo.Title));
                    UrlDat.Write(PageInfo.Description,
                    SizeOf(PageInfo.Description));

                    S := LowerCase(PageInfo.Url);
                    S[Length(S) + 1] := #0;
                    b := 0;
                    i := Pos('/', PageInfo.Url);
                    if i = Length(PageInfo.Url) then
                        b := b or 128;
                    if (LowerCase(copy(PageInfo.Url, 1, 4)) = 'www.') and
                    (i > 1) and (PageInfo.Url[i - 1] <> '.') then
                        b := b or 64;
                    if PageInfo.Language = 0 then
                        b := b or 32;

                    if LowerCase(copy(PageInfo.Url, 1, 4)) = 'www.' then
                        delete(PageInfo.Url, 1, 4);
                    i := Length(PageInfo.Url) - 1;
                    if i > 31 then
                        i := 31;
                    if i < 0 then
                        i := 0;
                    b := b or i;

                    FilterOut.Write(b, SizeOf(b));

                    Inc(An);
                    if (An and 8191) = 0 then
                    begin
                        Write(#13, DbNr, ' ',
                        100.0 * Inf.FilePos / Inf.FileSize:5:1,
                        '% complete. (', An, ' URLs)');
                        if Inf.FilePos < Inf.FileSize then
                        begin
                            ThisTime := GetTickCount;
                            Sec := Round(0.001 * (ThisTime - StartTime));
                            Write(' ', Sec div 60, ':', Zahl(Sec mod 60, 2),
                            ' elapsed -');
                            Sec := Round(0.001 * (ThisTime - StartTime)
                            / Inf.FilePos *
                            (Inf.FileSize - Inf.FilePos));
                            Write(' ', Sec div 60, ':', Zahl(Sec mod 60, 2),
                            ' remaining       ');
                        end;
                    end;
                end
                else
                begin // Do NOT add
                    for i := 1 to PageInfo.WordCount do
                    begin
                        Inf.Read(RankingFlags, 1);
                        Inf.Read(ThisKeyword[0], 1);
                        if Length(ThisKeyword) <> 0 then Inf.Read(ThisKeyword[1], Length(ThisKeyword));
                    end;
                end;

            end
            else
            begin // Url=''
                for i := 1 to PageInfo.WordCount do
                begin
                    Inf.Read(RankingFlags, 1);
                    Inf.Read(ThisKeyword[0], 1);
                    if Length(ThisKeyword) <> 0 then Inf.Read(ThisKeyword[1], Length(ThisKeyword));
                end;
            end;

            // Make sure that there will not be DocIDs that will overflow the RWI data-structures
            if An >= MaxAllowedDocId then break;
        end;

        FilterOut.Close;
        FilterOut.Free;
        UrlDat.Close;
        UrlDat.Free;
        Inf.Close;
        Inf.Free;
        WriteLn(#13, DbNr, ' ', '100.0% complete. (', An,
        ' URLs)                                            ');
    end;

    for i := 0 to 63 do
    begin
        TempKey[i].Close;
        TempKey[i].Free;
    end;

    Progress.Pass := 1;
    WriteLn;
end;

{ ---------------------------------------------------------- }

type
    tKeyData = array [0 .. 100 * 1000 * 1000] of integer;
    pKeyData = ^tKeyData;

    pKeyEntry = ^tKeyEntry;

    tKeyEntry = record
        Next: pKeyEntry;
        An: integer;
        KeyData: pKeyData;
        KeyTxt: ShortString;
    end;

var
    HashTable: array [0 .. cMaxIndexHash] of pKeyEntry;
    HashOut: array [0 .. cMaxIndexHash] of int64;


procedure IncKeywordReferenceCount(const ThisKey: ShortString);
var
    HashCode: integer;
    p: pKeyEntry;
begin
    HashCode := (CalcCRC(ThisKey) shr 6) and cMaxIndexHash;
    p := HashTable[HashCode];
    while p <> nil do
    begin
        if p^.KeyTxt = ThisKey then
        begin
            Inc(p^.An);
            exit;
        end;
        p := p^.Next;
    end;

    // Write('Adding Ref to "',ThisKey,'"   ');
    GetMem(p, SizeOf(tKeyEntry) - 255 + Length(ThisKey));
    p^.Next := HashTable[HashCode];
    p^.An := 1;
    p^.KeyData := nil;
    p^.KeyTxt := ThisKey;
    HashTable[HashCode] := p;
end;


procedure DoPass2;
var
    Step: integer;
    fIn: tCacheFile;
    fOut: tBufWriteFile;
    Po, Nr, KeyPo: integer;
    i, l, h: integer;
    Info: TextFile;
    p, p2: pKeyEntry;
    HashCode: integer;
    ThisKeyword: ShortString;
    RankingFlags: byte;
    TopHitsPointer: int64;
begin
    WriteLn('Starting Pass 2');

    TopHitsPointer := -1;

    for Step := Progress.Step to 63 do
    begin
        FillChar(HashTable, SizeOf(HashTable), 0);
        Write('Step ', Step, '   Analyzing... ');

        fIn := tCacheFile.Create;
        fIn.Assign(cTempPath + 'keys.' + IntToStr(Step));
        fIn.Reset;

        while (not fIn.Eof) do
        begin
            fIn.Read(RankingFlags, 1);
            fIn.Read(ThisKeyword[0], 1);
            if Length(ThisKeyword) <> 0 then fIn.Read(ThisKeyword[1], Length(ThisKeyword));
            fIn.Read(Nr, 4);
            if ThisKeyword <> '' then IncKeywordReferenceCount(ThisKeyword);
        end;

        fIn.Close;
        fIn.Free;



        for i := 0 to cMaxIndexHash do
        begin
            p := HashTable[i];
            while p <> nil do
            begin
                if p^.An > 0 then
                begin
                    GetMem(p^.KeyData, p^.An * 4);
                    p^.An := 0;
                end;
                p := p^.Next;
            end;
        end;



        Write('Reading... ');
        fIn := tCacheFile.Create;
        fIn.Assign(cTempPath + 'keys.' + IntToStr(Step));
        fIn.Reset;

        while (not fIn.Eof) do
        begin
            fIn.Read(RankingFlags, 1);
            fIn.Read(ThisKeyword[0], 1);
            if Length(ThisKeyword) <> 0 then fIn.Read(ThisKeyword[1], Length(ThisKeyword));
            fIn.Read(Nr, 4);
            if ThisKeyword <> '' then
            begin
                HashCode := (CalcCRC(ThisKeyword) shr 6) and cMaxIndexHash;
                p := HashTable[HashCode];
                while p <> nil do
                begin
                    if p^.KeyTxt = ThisKeyword then
                    begin
                        p^.KeyData^[p^.An] := (Nr shl 3) or (RankingFlags and 7);
                        Inc(p^.An);
                        break;
                    end;
                    p := p^.Next;
                    if p = nil then
                    begin
                        WriteLn('Keyword "', ThisKeyword, '" could not be found in the hashtable!');
                        halt;
                    end;
                end;
            end;
        end;

        fIn.Close;
        fIn.Free;

        Write('Writing... ');
        FillChar(HashOut, SizeOf(HashOut), 0);
        fOut := tBufWriteFile.Create;
        fOut.Assign(OutputDir + 'keys' + Zahl(Step, 2) + '.idx');
        fOut.ReWrite;
        fOut.Write(HashOut, SizeOf(HashOut));

        AssignFile(Info, OutputDir + 'info.txt');
        if FileExists(OutputDir + 'info.txt') then
            Append(Info)
        else
            ReWrite(Info);
        for i := 0 to cMaxIndexHash do
        begin
            p := HashTable[i];
            if p = nil then
                HashOut[i] := 0
            else HashOut[i] := fOut.FileSize;

            while p <> nil do
            begin
                if p^.An = 0 then
                begin
                    WriteLn('p.An=0 for "', p^.KeyTxt, '"');
                    halt;
                end;
                if p^.An >= 100000 then
                    WriteLn(Info, p^.An:10, ' ', p^.KeyTxt);

                Inc(UniqueWords);
                Inc(WordOccurences, p^.An);

                try
                    fOut.Write(p^.KeyTxt, Length(p^.KeyTxt) + 1);
                    fOut.Write(p^.An, 4);
                    fOut.Write(TopHitsPointer, SizeOf(TopHitsPointer));
                except
                    WriteLn('Exception #1');
                    halt;
                end;


                try
                    if p^.An <> 0 then
                        fOut.Write(p^.KeyData^[0], p^.An * 4);
                except
                    WriteLn('Exception #2');
                    WriteLn(p^.KeyTxt);
                    WriteLn(p^.An);
                    // WriteLn(integer(p^.KeyData));
                    halt;
                end;
                p2 := p^.Next;
                FreeMem(p^.KeyData);
                FreeMem(p);
                p := p2;
            end;
            Nr := 0;
            fOut.Write(Nr, 1);
        end;
        WriteLn(Info, 'UniqueWords=', UniqueWords);
        WriteLn(Info, 'WordOccurences=', WordOccurences);
        CloseFile(Info);

        fOut.Close;
        fOut.Free;
        fOut := tBufWriteFile.Create;
        fOut.Assign(OutputDir + 'keys' + Zahl(Step, 2) + '.idx');
        fOut.Reset;
        fOut.Write(HashOut, SizeOf(HashOut));
        fOut.Close;
        fOut.Free;

        WriteLn(' Done.');

        Progress.Step := Step + 1;
        WriteProgress;
    end;
    Progress.Pass := 2;
end;

{ ---------------------------------------------------------- }



procedure DoPass3;
var
    Step, Nr, i, Last: integer;
    fIn: tCacheFile;
    PageInfo: tPageInfo;
    Url: ShortString;
    HashCode: integer;
    p, p2: pKeyEntry;
    fOut: tBufWriteFile;
    Info: TextFile;
begin
    FillChar(HashTable, SizeOf(HashTable), 0);

    for Step := 0 to (cDbCount - 1) do
    begin
        Write('Host-Step ', Step, '   Analyzing... ');
        fIn := tCacheFile.Create;
        fIn.Assign(OutputDir + 'urls.dat' + IntToStr(Step));
        fIn.Reset;
        Nr := 0;

        while (not fIn.Eof) do
        begin
            fIn.Read(PageInfo.Url, SizeOf(PageInfo.Url));
            fIn.Read(PageInfo.Title, SizeOf(PageInfo.Title));
            fIn.Read(PageInfo.Description, SizeOf(PageInfo.Description));
            Url := PageInfo.Url;
            i := Pos('/', Url);
            if i > 0 then
                SetLength(Url, i - 1);
            i := Pos(':', Url);
            if i > 0 then
                SetLength(Url, i - 1);
            Url := LowerCase(Url);
            HashCode := CalcCRC(Url) and cMaxIndexHash;

            p := HashTable[HashCode];
            while p <> nil do
            begin
                if Url = p^.KeyTxt then
                    break
                else
                    p := p^.Next;
            end;

            if p = nil then
            begin
                GetMem(p, SizeOf(tKeyEntry) - 255 + Length(Url));
                p^.Next := HashTable[HashCode];
                p^.An := 0;
                p^.KeyData := nil;
                p^.KeyTxt := Url;
                HashTable[HashCode] := p;
            end;
            Inc(p^.An);

            Inc(Nr);
            // if (Nr and 1023) = 0 then write(#13, Nr);
        end;
        WriteLn(Nr);

        fIn.Close;
        fIn.Free;
    end;

    Write('Allocating memory...');
    for i := 0 to cMaxIndexHash do
    begin
        p := HashTable[i];
        while p <> nil do
        begin
            if p^.An > 0 then
                GetMem(p^.KeyData, p^.An * 4);
            p^.An := 0;
            p := p^.Next;
        end;
    end;
    WriteLn(' Done.');

    for Step := 0 to (cDbCount - 1) do
    begin
        Write('Host-Step ', Step, '   Reading... ');
        fIn := tCacheFile.Create;
        fIn.Assign(OutputDir + 'urls.dat' + IntToStr(Step));
        fIn.Reset;
        Nr := 0;

        while (not fIn.Eof) do
        begin
            fIn.Read(PageInfo.Url, SizeOf(PageInfo.Url));
            fIn.Read(PageInfo.Title, SizeOf(PageInfo.Title));
            fIn.Read(PageInfo.Description, SizeOf(PageInfo.Description));
            Url := PageInfo.Url;
            i := Pos('/', Url);
            if i > 0 then
                SetLength(Url, i - 1);
            i := Pos(':', Url);
            if i > 0 then
                SetLength(Url, i - 1);
            Url := LowerCase(Url);
            HashCode := CalcCRC(Url) and cMaxIndexHash;

            p := HashTable[HashCode];
            while p <> nil do
            begin
                if Url = p^.KeyTxt then
                    break
                else
                    p := p^.Next;
            end;

            if p = nil then
            begin
                WriteLn(
                #13'Internal fatal error in Pass 4: Host not found in host-list');
                halt;
            end;
            p^.KeyData^[p^.An] := (Nr shl cDbBits) or Step;
            Inc(p^.An);

            Inc(Nr);
            // if (Nr and 1023) = 0 then write(#13, Nr);
        end;
        WriteLn(Nr);

        fIn.Close;
        fIn.Free;
    end;

    WriteLn('Writing host-list...');
    FillChar(HashOut, SizeOf(HashOut), 255);
    fOut := tBufWriteFile.Create;
    fOut.Assign(OutputDir + 'hosts.idx');
    fOut.ReWrite;
    fOut.Write(HashOut, SizeOf(HashOut));
    AssignFile(Info, OutputDir + 'info.txt');
    if FileExists(OutputDir + 'info.txt') then
        Append(Info)
    else
        ReWrite(Info);
    WriteLn(Info, 'Host-list follows:');

    for i := 0 to cMaxIndexHash do
    begin
        p := HashTable[i];
        Last := -1;
        while p <> nil do
        begin
            if p^.An > 0 then
            begin
                HashOut[i] := fOut.FileSize;
                fOut.Write(Last, 4);
                fOut.Write(p^.KeyTxt, Length(p^.KeyTxt) + 1);
                fOut.Write(p^.An, 4);
                fOut.Write(p^.KeyData^[0], p^.An * 4);
                if p^.An > 100 then
                    WriteLn(Info, p^.An:10, ' ', p^.KeyTxt);
                Last := HashOut[i];
            end;
            p := p^.Next;
        end;
    end;
    fOut.Close;
    fOut.Free;

    fOut := tBufWriteFile.Create;
    fOut.Assign(OutputDir + 'hosts.idx');
    fOut.Reset;
    fOut.Write(HashOut, SizeOf(HashOut));
    fOut.Close;
    fOut.Free;

    CloseFile(Info);
    WriteLn(' Done.');

    for i := 0 to cMaxIndexHash do
    begin
        p := HashTable[i];
        while p <> nil do
        begin
            p2 := p^.Next;
            if p^.An > 0 then
                FreeMem(p^.KeyData, p^.An * 4);
            FreeMem(p, SizeOf(tKeyEntry) - 255 + Length(p^.KeyTxt));
            p := p2;
        end;
    end;

    Progress.Pass := 3;
end;

{ ---------------------------------------------------------- }


procedure DoPass4;
var
    DomainRanks: tDomainRank;
    DbNr: integer;
    Rank: int32;
    UrlInfo: tUrlInfo;
    fIn: tCacheFile;
    fOut, fOut2: tBufWriteFile;
    b: byte;

    function CalcDomainData(const Url: ShortString): byte;
    var
        S: ShortString;
        Slashes, i: integer;
        PathElements, HostElements: byte;
    begin
        S := Url;
        Slashes := 0;
        for i := 1 to Length(S) do
            if S[i] = '/' then
                Inc(Slashes);

        // if (Slashes = 1) and (s[Length(s)] = '/') then
        // SetLength(s, Length(s) - 1);

        PathElements := Slashes + 1;
        if S[Length(S)] = '/' then
            Dec(PathElements);

        for i := 1 to Length(S) do
            if S[i] = '/' then
            begin
                SetLength(S, i - 1);
                break;
            end;

        HostElements := 1;
        for i := 1 to Length(S) do
            if S[i] = '.' then
                Inc(HostElements);


        if PathElements > 15 then
            PathElements := 15;
        if HostElements > 15 then
            HostElements := 15;

        (* WriteLn;
          WriteLn(Url);
          WriteLn(Slashes);
          WriteLn('PathElements=',PathElements);
          WriteLn('HostElements=',HostElements); *)

        Result := (PathElements shl 4) or HostElements;
    end;


begin
    DomainRanks := tDomainRank.Create;

    for DbNr := 0 to cDbCount - 1 do
    begin
        Write(#13'DbNr: ', DbNr);
        fIn := tCacheFile.Create;
        fIn.Assign(OutputDir + 'urls.dat' + IntToStr(DbNr));
        fIn.Reset;

        fOut := tBufWriteFile.Create;
        fOut.Assign(OutputDir + 'rank.dat' + IntToStr(DbNr));
        fOut.ReWrite;

        fOut2 := tBufWriteFile.Create;
        fOut2.Assign(OutputDir + 'rank2.dat' + IntToStr(DbNr));
        fOut2.ReWrite;

        while not fIn.Eof do
        begin
            fIn.Read(UrlInfo, SizeOf(UrlInfo));
            Rank := DomainRanks.GetDomainRanking(UrlInfo.Url);
            fOut.Write(Rank, SizeOf(Rank));

            if LowerCase(string(copy(UrlInfo.Url, 1, 4))) = 'www.' then
                delete(UrlInfo.Url, 1, 4);
            b := CalcDomainData(UrlInfo.Url);
            fOut2.Write(b, 1);


            // For debugging only
            (* if Rank<>-1 then
              begin
              WriteLn;
              WriteLn(Rank,': ',UrlInfo.Url);
              halt;
              end; *)
        end;

        fIn.Close;
        fIn.Free;

        fOut.Close;
        fOut.Free;

        fOut2.Close;
        fOut2.Free;
    end;
    WriteLn;
    Progress.Pass := 4;
end;

{ ---------------------------------------------------------- }

procedure DoPass5;
// Generate the "fancy-hits" accelerator index
type
    uint32Array = array of uint32;
var
    DbNr: integer;
    fDomainRank: file;
    DomainRankData: array [0 .. cDbCount - 1] of uint32Array;
    fIn: tCacheFile;
    fOut: tBufWriteFile;
    fRWI: tFileStream;
    OutBuf: uint32Array;
    Keyword: ShortString;
    RWICount: int32;
    FancyPos, FancyPointer: int64;
    FancyCount: int32;
    NextPos: int64;
    RWIData: uint32;
    RWIDbNr, RWIIndex: uint32;
    InCount, OutCount: int32;
begin
    // First, we need to load all the domain-rank data into memory.
    // We need *fast* access to that, as it is used to determine which RWI-data
    // gets put into the fancy-hits index.

    Write('Reading Rank-Data...');
    for DbNr := 0 to cDbCount - 1 do
    begin
        // Write('1');
        AssignFile(fDomainRank, OutputDir + 'rank.dat' + IntToStr(DbNr));
        Reset(fDomainRank, 4);
        // WriteLn('2');
        // WriteLn('FileSize=',FileSize(fDomainRank));
        SetLength(DomainRankData[DbNr], FileSize(fDomainRank));
        BlockRead(fDomainRank, DomainRankData[DbNr][0], FileSize(fDomainRank));
        CloseFile(fDomainRank);
        // Write('3');
    end;
    WriteLn(' Done.');

    // Create the outbuf buffer
    SetLength(OutBuf, 500 * 1000 * 1000); // Almost 2gb !

    for DbNr := 0 to 63 do
    // Note that 63 is correct here at all times. This is *not* the cDbCount-1 value!
    // There are *always* 64 shards for the RWI data.
    begin
        Write(#13'Creating accelerator-index #', DbNr);
        // Open one shard of the RWI data
        fIn := tCacheFile.Create;
        fIn.Assign(OutputDir + 'keys' + Zahl(DbNr, 2) + '.idx');
        fIn.Reset;

        // Create the fancy-hits file
        fOut := tBufWriteFile.Create;
        fOut.Assign(OutputDir + 'fancy' + Zahl(DbNr, 2) + '.idx');
        fOut.ReWrite;

        fRWI := tFileStream.Create(OutputDir + 'keys' + Zahl(DbNr, 2) + '.idx', fmOpenReadWrite or fmShareDenyNone);

        // Skip the hash-table at the beginning of the RWI-file
        fIn.Seek(8 * (cMaxIndexHash + 1));

        // Now read all the RWI-data...

        while not fIn.Eof do
        begin
            fIn.Read(Keyword[0], 1);
            if Keyword[0] <> #0 then
            begin
                fIn.Read(Keyword[1], Length(Keyword));
                fIn.Read(RWICount, SizeOf(RWICount));
                FancyPos := fIn.FilePos;
                fIn.Read(FancyPointer, SizeOf(FancyPointer));

                InCount := 0;
                OutCount := 0;

                if RWICount > 100000 then
                begin
                    FancyCount := 0;
                    while RWICount > 0 do
                    begin
                        Dec(RWICount);
                        Inc(InCount);
                        fIn.Read(RWIData, SizeOf(RWIData));
                        RWIDbNr := (RWIData shr 3) and (cDbCount - 1);
                        RWIIndex := RWIData shr (3 + cDbBits);
                        if (DomainRankData[RWIDbNr][RWIIndex] > 0) and (DomainRankData[RWIDbNr][RWIIndex] < 200000) then
                        begin
                            if FancyCount <= High(OutBuf) then
                            begin
                                OutBuf[FancyCount] := RWIData;
                                Inc(FancyCount);
                                Inc(OutCount);
                            end;
                        end;
                    end;

                    // Update FancyPointer in input file
                    FancyPointer := fOut.FileSize;
                    fRWI.Position := FancyPos;
                    fRWI.Write(FancyPointer, SizeOf(FancyPointer));

                    fOut.Write(FancyCount, SizeOf(FancyCount));
                    fOut.Write(OutBuf[0], FancyCount * 4);
                    // Warning! If the RWI-data is changed to something other than 32-bit,
                    // then the line above must be changed accordingly.

                    // WriteLn('InCount=', InCount, ' OutCount=', OutCount);
                    // halt;
                end
                else
                begin
                    NextPos := fIn.FilePos;
                    Inc(NextPos, 4 * RWICount);
                    fIn.Seek(NextPos);
                end;
            end;
        end;

        fIn.Close;
        fIn.Free;
        fOut.Close;
        fOut.Free;
        fRWI.Free;
    end;

    for DbNr := 0 to cDbCount - 1 do
    begin
        SetLength(DomainRankData[DbNr], 0);
    end;
    WriteLn;
    Progress.Pass := 5;
end;


procedure DoPass6; // Backlink-Counts
var
    DbNr: integer;
    fIn: tCacheFile;
    fOut: tBufWriteFile;
    fUrls: tPreloadedFile;
    UrlInfo: tUrlInfo;
    HashCode, Po: int64;
    UrlData: tUrlData;
    BackLinks: int64;
    LowerUrl: string;
    CurrentTime: double;
begin
    CurrentTime:=Now;

    for DbNr := 0 to cDbCount - 1 do
    begin
        Write('Backlink-counts Db #', DbNr);
        fIn := tCacheFile.Create;
        fIn.Assign(OutputDir + 'urls.dat' + IntToStr(DbNr));
        fIn.Reset;

        fOut := tBufWriteFile.Create;
        fOut.Assign(OutputDir + 'backlink.dat' + IntToStr(DbNr));
        fOut.ReWrite;

        fUrls := tPreloadedFile.Create;
        fUrls.Assign(cUrlDb + IntToStr(DbNr));
        fUrls.OpenRead;
        Write(' Loading Url-Data... ');
        fUrls.Preload;
        Write('Loaded. Processing... ');

        while not fIn.Eof do
        begin
            fIn.Read(UrlInfo, SizeOf(UrlInfo));

            BackLinks := 1;
            LowerUrl := LowerCase(UrlInfo.Url);
            HashCode := HashOfUrl(UrlInfo.Url);
            Po := HashCode * 4;
            while Po <> 0 do
            begin
                try
                    fUrls.Seek(Po);
                    fUrls.Read(UrlData, SizeOf(UrlData));
                except
                    WriteLn(#13, 'Cannot find URL: ', UrlData.Url);
                    Po := 0;
                    break;
                end;

                if LowerUrl = LowerCase(UrlData.Url) then
                begin
                    BackLinks := UrlData.InLinkCount;
                    break;
                end;
                if Po = UrlData.Next then
                begin
                    WriteLn(#13'Self=Next at ', UrlData.Url);
                    Po := 0;
                    break;
                end;
                Po := UrlData.Next;
            end;
            if BackLinksFromAge then BackLinks:=Trunc(1000.0/(CurrentTime+0.1-(0.001*BackLinks)));
            if BackLinks<1 then BackLinks:=1;

            fOut.Write(BackLinks, SizeOf(BackLinks));
        end;

        fIn.Close;
        fIn.Free;

        fOut.Close;
        fOut.Free;

        fUrls.Close;
        fUrls.Free;

        WriteLn('Done.');
    end;

    // Progress.Pass := 6;
end;


{ ---------------------------------------------------------- }


procedure CheckParameters;
var
    i: integer;
    S: string;
begin
    BackLinksFromAge := false;
    OutputDir := cSDataPath;

    i := 1;
    while i <= ParamCount do
    begin
        S := LowerCase(ParamStr(i));
        if (s='-b') or (S = '-backlinksfromage') then BackLinksFromAge := true;

        if (S = '-outputdir') or (S = '-o') then
        begin
            Inc(i);
            OutputDir := ParamStr(i);
            if OutputDir = '' then OutputDir := cSDataPath;
        end;

        Inc(i);
    end;
end;


begin
    WriteLn;
    WriteLn('GenDb ', cVersionCopy);
    WriteLn(cGPLNotice);
    WriteLn;

    UniqueWords := 0;
    WordOccurences := 0;

    CheckParameters;



    Write('Reading IgnoreHosts... ');
    IgHostAn := 0;
    IgDomainAn := 0;
    if FileExists(cIgnoreHosts) then
    begin
        AssignFile(F2, cIgnoreHosts);
        Reset(F2);
        while not Eof(F2) do
        begin
            ReadLn(F2, S);
            if S <> '' then
            begin
                S := LowerCase(S);
                if copy(S, 1, 1) = '@' then
                begin
                    Inc(IgDomainAn);
                    delete(S, 1, 1);
                    GetMem(IgDomains[IgDomainAn], Length(S) + 1);
                    IgDomains[IgDomainAn]^ := S;
                end
                else
                begin
                    Inc(IgHostAn);
                    GetMem(IgHosts[IgHostAn], Length(S) + 1);
                    IgHosts[IgHostAn]^ := S;
                end;
            end;
        end;
        CloseFile(F2);
    end;
    WriteLn(IgHostAn, ' IgnoreHosts found');
    WriteLn(IgDomainAn, ' IgnoreDomains found');

    if FileExists('gendb.progress') then
    begin
        AssignFile(F, 'gendb.progress');
        Reset(F, 1);
        BlockRead(F, Progress, SizeOf(Progress));
        CloseFile(F);
        // if Progress.Pass > 3 then Progress.Pass := 3;
    end
    else
    begin
        Progress.Pass := 0;
        Progress.Step := 0;
    end;
    WriteProgress;

    if Progress.Pass = 0 then
        DoPass1;
    WriteProgress;

    if Progress.Pass = 1 then
        DoPass2;
    WriteProgress;

    if Progress.Pass = 2 then
        DoPass3;
    WriteProgress;

    if Progress.Pass = 3 then
        DoPass4;
    WriteProgress;

    if Progress.Pass = 4 then
        DoPass5;
    WriteProgress;

    if Progress.Pass = 5 then
        DoPass6; // Backlink-Counts
    WriteProgress;

    AssignFile(F, OutputDir + 'ready2.dat');
    ReWrite(F, 1);
    BlockWrite(F, Progress, SizeOf(Progress));
    CloseFile(F);

    WriteLn('UniqueWords=', UniqueWords);
    WriteLn('WordOccurences=', WordOccurences);

end.
