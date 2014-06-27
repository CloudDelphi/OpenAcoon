program searchservernew;

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

uses
    OSWrapper,
    SysUtils,
    Classes,
    //IdBaseComponent,
    //IdComponent,
    //IdCustomTCPServer,
    IdCustomHTTPServer,
    IdHTTPServer,
    IdContext,
    //DomainRank,
    Hash,
    CacheFile,
    GlobalTypes,
    FileLocation,
    DbTypes,
    SyncObjs,
    Words;

const
    cMaxKeywords = 10; // This defines the maximum number of keywords per query
    cMaxTempPages = 32768; // Size of read-buffer used during query-processing

    cMaxPagesPerShard = 10 * 1000 * 1000;
    // Number of shards is defined in dbtypes.pas -> cDbCount
    // 10 million is enough for 1.28 billion pages when used with the default
    // number of 128 shards. You can set this pretty high without adverse
    // consequences, as the actual memory-allocation is only the amount that
    // is really needed.

    cMaxCachedResults = 64 * 1024 - 1;

type
    tFilter = array [0 .. cMaxPagesPerShard] of byte;
    pFilter = ^tFilter;
    tBitField = array [0 .. 10 * 1000 * 1000] of int32; { Reicht für ca. 2,5 Mrd. Seiten }
    pBitField = ^tBitField;
    tValues = array [0 .. 10 * 1000 * 1000] of uint16; { Reicht für ca. 640 Mio. Seiten }
    pValues = ^tValues;
    tAction = (acSet, acAnd, acNot);

    tCachedResult = record
        Pages: array [1 .. 1000] of integer;
        Values: array [1 .. 1000] of uint16;
        MaxValue, Count: integer;
        Query: shortstring;
    end;

    tRankDataArray = array of int32;
    tBackLinkDataArray = array of int64;
    tUrlDataArray = array of byte;

    tRWIWorkChunk = record
        RWIData: array of uint32;
        Action: tAction;
    end;

    pRWIWorkChunk = ^tRWIWorkChunk;


var
    RankData: array [0 .. cDbCount - 1] of tRankDataArray;
    BackLinkData: array [0 .. cDbCount - 1] of tBackLinkDataArray;
    MaxBackLinkCount: int64;
    UrlData: array [0 .. cDbCount - 1] of tUrlDataArray;
    OrgRbs: RawByteString;
    CachedResults: array [0 .. cMaxCachedResults] of tCachedResult;
    CacheUsed: array [0 .. cMaxCachedResults] of boolean;
    ThisQuery: AnsiString;
    ThisIsCachedResult: boolean;
    Ti4, Ti5, Ti6: int64;
    cSData: string;
    MainQS: array [0 .. 2048] of AnsiChar;
    f: TextFile;
    Begriff: AnsiString;
    StartWithNr: integer;
    PreferDe, PreferEn, GerOnly: boolean;
    KeyWordCount: integer;
    KeyWords: array [1 .. cMaxKeywords] of shortstring;
    KeyWordIsHostName: array [1 .. cMaxKeywords] of boolean;
    KeyWordResultCount: array [1 .. cMaxKeywords] of int32;
    KeyWordAction: array [1 .. cMaxKeywords] of tAction;

    BitField, TempBitField: array [0 .. cDbCount - 1] of pBitField;
    BitFieldSize: array [0 .. cDbCount - 1] of integer;
    Values: array [0 .. cDbCount - 1] of pValues;
    ValueSize: array [0 .. cDbCount - 1] of integer;

    Html: array [0 .. cDbCount - 1] of tFileStream;
    TempBuf: array [1 .. cMaxTempPages] of int32;
    FilterData: array [0 .. cDbCount - 1] of pFilter;
    FilterDataSize: array [0 .. cDbCount - 1] of integer;
    ShowCount: integer;
    MaxValue: integer;
    ResultCount: integer;
    FilterMask: integer;
    b1, b2, b3, b4, b5, b6, b7: integer;
    Searchs: integer;
    NoResults: integer;
    ValueTable: array [0 .. 65535] of integer;
    ValueData: array [0 .. 65535, 1 .. 1024] of integer;
    KeyDbs, FancyDbs: array [0 .. 63] of tPreloadedFile;
    BitFieldInitialized: boolean;
    Counter: integer;
    Count: integer;
    RefreshCachesCountdown: integer;
    Ticks: int64;
    MinTicks, MaxTicks: integer;
    CritSec: tCriticalSection;
    QueryPass: integer;
    EarlyAbort: boolean;
    IdHTTPServer1: TIdHTTPServer;



function LookupDomainRank(DocID: integer): integer; inline;
begin
    Result := RankData[DocID and (cDbCount - 1)][DocID shr cDbBits];
end;


function LookupUrlData(DocID: integer): integer; inline;
begin
    Result := UrlData[DocID and (cDbCount - 1)][DocID shr cDbBits];
end;



function ReferenceRAMCaches: integer;
// Accesses internal RAM-Caches. This is done to prevent the OS from
// swapping-out the caches. This could happen if there are relatively
// few searches being done.
var
    i, j: integer;
    Sum: int64;
begin
    Sum := 0;
    for i := 0 to cDbCount - 1 do
    begin
        for j := 0 to FilterDataSize[i] - 1 do
            Inc(Sum, FilterData[i]^[j]);

        for j := 0 to High(RankData[i]) do
            Inc(Sum, RankData[i][j]);

        for j := 0 to High(UrlData[i]) do
            Inc(Sum, UrlData[i][j]);

        for j := 0 to High(BackLinkData[i]) do
            Inc(Sum, BackLinkData[i][j]);
    end;

    // We need to do something with "Sum" in order to prevent
    // the compiler warning "Value assigned to Sum never used".
    Result := Sum;
end;


function GetBackLinkValue(DbNr, Index: int32): double;
var
    BackLinkCount: int64;
begin
    BackLinkCount := BackLinkData[DbNr][Index];
    if (BackLinkCount = 0) or (MaxBackLinkCount <= 1) then Result := 1.0
    else Result := 2500.0 * ln(BackLinkCount) / ln(MaxBackLinkCount);
end;



procedure AppendToLog(Txt: string);
var
    f: TextFile;
begin
    FileMode := 2;
    try
        AssignFile(f, cSearchLogFile);
        if FileExists(cSearchLogFile) then
            Append(f)
        else
            ReWrite(f);

        WriteLn(f, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ' + Txt);
        CloseFile(f);
    except
    end;
end;


procedure ResetStatistics;
begin
    Searchs := 0;
    Counter := 0;
    Ticks := 0;
    MinTicks := 3600000;
    MaxTicks := 0;
    NoResults := 0;
end;


procedure AddToMemo(Txt: string);
begin
    WriteLn(Txt);
end;


function FindParam(const Name: AnsiString): string;
const
    MaxLen = 255;
var
    ThisName: AnsiString;
    i, j: integer;
    Code1, Code3: integer;
    qs: array [0 .. 2048] of AnsiChar;
    Data, Data2: AnsiString;
begin
    StrCopy(qs, MainQS);
    StrLower(qs);

    i := 0;
    Data := '';
    while qs[i] <> #0 do
    begin
        ThisName := '';
        if qs[i] = '&' then Inc(i);
        while ((qs[i] <> #0) and (qs[i] <> '=')) do
        begin
            ThisName := ThisName + qs[i];
            Inc(i);
        end;

        if Name = ThisName then
        begin
            j := 0;
            if (qs[i] = '=') then Inc(i);
            while ((qs[i] <> #0) and (qs[i] <> '&') and (j < MaxLen)) do
            begin
                Data := Data + qs[i];
                Inc(i);
                Inc(j);
            end;


            Data2 := '';
            while Data <> '' do
            begin
                case Data[1] of
                    'ä', 'Ä':
                        begin
                            Data2 := Data2 + 'ae';
                            Delete(Data, 1, 1);
                        end;
                    'ö', 'Ö':
                        begin
                            Data2 := Data2 + 'oe';
                            Delete(Data, 1, 1);
                        end;
                    'ü', 'Ü':
                        begin
                            Data2 := Data2 + 'ue';
                            Delete(Data, 1, 1);
                        end;
                    'ß':
                        begin
                            Data2 := Data2 + 'ss';
                            Delete(Data, 1, 1);
                        end;
                    '%': if Length(Data) >= 3 then
                        begin
                            Code1 := StrToIntDef('$' + copy(Data, 2, 2), 0);
                            Delete(Data, 1, 3);
                            case Code1 of
                                $C3:
                                    begin
                                        Code3 := StrToIntDef('$' + copy(Data, 2, 2), 0);
                                        Delete(Data, 1, 2);
                                        case Code3 of
                                            $A4, $84: Data2 := Data2 + 'ae';
                                            $B6, $96: Data2 := Data2 + 'oe';
                                            $AC, $9C: Data2 := Data2 + 'ue';
                                            $9F: Data2 := Data2 + 'ss';
                                        else Data2 := Data2 + Chr(Code1) + Chr(Code3);
                                        end;
                                    end;
                                $C4, $E4: Data2 := Data2 + 'ae';
                                $D6, $F6: Data2 := Data2 + 'oe';
                                $DC, $FC: Data2 := Data2 + 'ue';
                                $DF: Data2 := Data2 + 'ss';
                            else Data2 := Data2 + Chr(Code1);
                            end;
                        end
                        else
                        begin
                            Data2 := Data2 + Data[1];
                            Delete(Data, 1, 1);
                        end;
                else
                    begin
                        Data2 := Data2 + Data[1];
                        Delete(Data, 1, 1);
                    end;
                end;
            end;

            Result := Data2;
            exit;
        end;

        while ((qs[i] <> #0) and (qs[i] <> '&')) do
            Inc(i);
    end;
end;


procedure ExtractKeywords;
var
    i: integer;
begin
    for i := 1 to Length(Begriff) do
    begin
        if Begriff[i] in ['"', '/', '&', '*', ',', '+'] then Begriff[i] := ' ';
    end;

    while Pos('  ', Begriff) > 0 do
        Delete(Begriff, Pos('  ', Begriff), 1);
    Begriff := Trim(Begriff);

    KeyWordCount := 0;
    i := 1;

    while i <= Length(Begriff) do
    begin
        while ((i <= Length(Begriff)) and (Begriff[i] = ' ')) do
            Inc(i);

        Inc(KeyWordCount);
        KeyWords[KeyWordCount] := '';

        while ((i <= Length(Begriff)) and (Begriff[i] <> ' ')) do
        begin
            if ((Begriff[i] <> '+') or (KeyWordCount > 1) or
            (KeyWords[KeyWordCount] <> '')) then
                KeyWords[KeyWordCount] := KeyWords[KeyWordCount] + Begriff[i];
            Inc(i);
        end;
        if KeyWordCount >= cMaxKeywords then exit;
    end;
end;


procedure ReadString(var Fi: tFileStream; var s: shortstring);
begin
    Fi.Read(s, 1);
    if Length(s) > 0 then
        Fi.Read(s[1], Length(s));
end;


procedure FindThisHost(const ThisHost: shortstring; Action: tAction);
var
    Hosts: tFileStream;
    HashCode: int32;
    DbNr, Index: integer;
    i, Data, ThisValue, NewValue: integer;
    s: shortstring;
    Po, An: int32;
    ThisAn, fd: integer;
begin
    case Action of
        acSet:
            begin
                BitFieldInitialized := true;
                for i := 0 to cDbCount - 1 do
                begin
                    FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
                end;
                FillChar(ValueTable, SizeOf(ValueTable), 0);
            end;
        acAnd:
            begin
                for i := 0 to cDbCount - 1 do
                    FillChar(TempBitField[i]^, BitFieldSize[i] * 4, 0);
            end;
        acNot:
            begin
                for i := 0 to cDbCount - 1 do
                    FillChar(TempBitField[i]^, BitFieldSize[i] * 4, 255);
            end;
    end;

    HashCode := CalcCRC(ThisHost) and cMaxIndexHash;
    Hosts := tFileStream.Create(cSData + 'hosts.idx', fmOpenRead or fmShareDenyNone);

    Hosts.Position := HashCode * 8;
    Hosts.Read(Po, 4);

    while Po <> -1 do
    begin
        Hosts.Position := Po;
        Hosts.Read(Po, 4);
        ReadString(Hosts, s);
        if s = ThisHost then
        begin
            Hosts.Read(An, 4);

            while An > 0 do
            begin
                ThisAn := An;
                if ThisAn > cMaxTempPages then ThisAn := cMaxTempPages;
                Hosts.Read(TempBuf, 4 * ThisAn);
                for i := 1 to ThisAn do
                begin
                    Data := TempBuf[i];
                    DbNr := Data and (cDbCount - 1);
                    Index := Data shr cDbBits;
                    fd := FilterData[DbNr]^[Index];
                    ThisValue := b1;
                    Inc(ThisValue, (31 - (fd and 31)) * b7);
                    if Action = acSet then
                        Inc(ThisValue, Round(GetBackLinkValue(DbNr, Index)));
                    if ThisValue < 1 then ThisValue := 1;
                    if ThisValue > 65535 then ThisValue := 65535;

                    case Action of
                        acSet:
                            begin
                                Inc(ValueTable[ThisValue]);
                                Values[DbNr]^[Index] := ThisValue;
                                Inc(BitField[DbNr]^[Index shr 5], 1 shl (Index and 31));
                            end;
                        acAnd:
                            begin
                                if (BitField[DbNr]^[Index shr 5] and (1 shl (Index and 31))) > 0 then
                                    NewValue := Values[DbNr]^[Index]
                                else NewValue := 0;

                                Dec(ValueTable[NewValue]);
                                Inc(NewValue, ThisValue);

                                if NewValue > 65535 then NewValue := 65535;


                                Inc(ValueTable[NewValue]);
                                Values[DbNr]^[Index] := NewValue;
                                TempBitField[DbNr]^[Index shr 5] :=
                                TempBitField[DbNr]^[Index shr 5] + (1 shl (Index and 31));

                            end;
                        acNot:
                            begin
                                Dec(TempBitField[DbNr]^[Index shr 5], 1 shl (Index and 31));
                            end;
                    end; { case Action of }
                end; { For-Schleife }

                Dec(An, ThisAn);
            end; { while An>0 }

            case Action of
                acAnd, acNot:
                    begin
                        for DbNr := 0 to cDbCount - 1 do
                            for i := 0 to BitFieldSize[DbNr] - 1 do
                                BitField[DbNr]^[i] := BitField[DbNr]^[i] and TempBitField[DbNr]^[i];
                    end; { acAnd, acNot }
            end;

            Hosts.Free;
            exit;
        end
        else
        begin
            if Po = -1 then
            begin
                Hosts.Free;
                if Action = acAnd then
                    for i := 0 to cDbCount - 1 do
                        FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
                exit;
            end;
        end;
    end;

    Hosts.Free;
    if Action = acAnd then
        for i := 0 to cDbCount - 1 do
            FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
end;


procedure ProcessThisKeyViaAnd(ThisAn: integer);
var
    i: integer;
    DbNr, Index: integer;
    Data: integer;
    IndexShl: array [0 .. 31] of integer;
    ThisValue, NewValue, ThisRankValue: integer;
    fd: integer;
    TempBit: integer;
    ThisUrlData: byte;
    HostElements: byte;
    BaseValue: array [0 .. 31] of integer;
begin
    for i := 0 to 31 do
        IndexShl[i] := 1 shl i;

    for i := 0 to 31 do
    begin
        ThisValue := b1;
        if (i and 1) <> 0 then Inc(ThisValue, b2);
        if (i and 2) <> 0 then Inc(ThisValue, b3);
        if (i and 4) <> 0 then Inc(ThisValue, b4);
        if (i and 16) <> 0 then Inc(ThisValue, b5);
        if (i and 8) <> 0 then Inc(ThisValue, b6);
        BaseValue[i] := ThisValue;
    end;

    for i := 1 to ThisAn do
    begin
        Data := TempBuf[i];
        DbNr := (Data shr 3) and (cDbCount - 1);
        Index := Data shr (3 + cDbBits);
        if ((BitField[DbNr]^[Index shr 5] and IndexShl[Index and 31]) > 0) then
        begin
            fd := FilterData[DbNr]^[Index];
            ThisValue := BaseValue[(Data and 7) or ((fd shr 3) and 24)];

            if PreferDe and ((fd and 32) <> 0) then Inc(ThisValue, 10);
            if PreferEn and ((fd and 32) = 0) then Inc(ThisValue, 10);
            Inc(ThisValue, (31 - (fd and 31)) * b7);

            ThisRankValue := LookupDomainRank((Index shl cDbBits) or DbNr) + 1;
            ThisUrlData := LookupUrlData((Index shl cDbBits) or DbNr) + 1;
            HostElements := ThisUrlData and 15;

            if ThisRankValue = 0 then ThisRankValue := 1000001;

            ThisValue := Round((1.0 - ThisRankValue * 0.000000027) * ThisValue / HostElements);



            if ThisValue > 65535 then ThisValue := 65535;

            TempBit := TempBitField[DbNr]^[Index shr 5];
            NewValue := Values[DbNr]^[Index] + ThisValue;
            if NewValue > 65535 then NewValue := 65535;
            Values[DbNr]^[Index] := NewValue;
            TempBitField[DbNr]^[Index shr 5] := TempBit + IndexShl[Index and 31];
        end;

    end; { For }
end;



function FindKeyWordResultCountForHost(ThisKey: shortstring): integer;
begin
    Result := 0;
end;



function FindKeyWordResultCount(ThisKey: shortstring; KeywordNr: integer): integer;
var
    Keys: ^tPreloadedFile;
    HashCode: integer;
    s: shortstring;
    Po: int64;
    An: int32;
begin
    Result := 0;

    if (LowerCase(copy(ThisKey, 1, 5)) = 'host:') or
    (LowerCase(copy(ThisKey, 1, 5)) = 'site:') then
    begin
        Result := FindKeyWordResultCountForHost(copy(ThisKey, 6, 255));
        exit;
    end;

    if LowerCase(copy(ThisKey, 1, 4)) = 'www.' then
    begin
        Result := FindKeyWordResultCountForHost(ThisKey);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = '.de' then
    begin
        Result := FindKeyWordResultCountForHost('www.' + ThisKey);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 3, 4)) = '.com' then
    begin
        Result := FindKeyWordResultCountForHost('www.' + ThisKey);
        exit;
    end;

    HashCode := CalcCRC(ThisKey);
    Str(HashCode and 63, s);
    Keys := @KeyDbs[HashCode and 63];
    // AppendToLog('Optimizer: Searching for keyword "' + ThisKey + '" in database #' + IntToStr(HashCode and 63));
    HashCode := (HashCode shr 6) and cMaxIndexHash;
    if Length(s) < 2 then s := '0' + s;
    FileMode := 0;

    Keys.Seek(HashCode * 8);
    Keys.Read(Po, 8);
    Keys.Seek(Po);

    while Po <> 0 do
    begin
        Keys.Read(s, 1);
        if s = '' then
        begin
            break;
        end;
        Keys.Read(s[1], Length(s));
        if s = ThisKey then
        begin
            Keys.Read(An, 4);
            Result := An;
            exit;
        end;
        Keys.Read(An, 4);
        Keys.Seek(Keys.FilePos + uint32(An) * 4 + 8);
    end;

    Result := 0;
    if Pos(':', ThisKey) > 0 then exit;

    if (LowerCase(copy(ThisKey, 1, 3)) = 'www') or
    (LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = 'com') or
    (LowerCase(copy(ThisKey, Length(ThisKey) - 1, 2)) = 'de') then
    begin
        if LowerCase(copy(ThisKey, 1, 3)) = 'www' then Insert('.', ThisKey, 4);
        if LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = 'com' then Insert('.', ThisKey, Length(ThisKey) - 2);
        if LowerCase(copy(ThisKey, Length(ThisKey) - 1, 2)) = 'de' then Insert('.', ThisKey, Length(ThisKey) - 1);
        begin
            KeyWordIsHostName[KeywordNr] := true;
            KeyWords[KeywordNr] := 'host:' + ThisKey;
        end;
    end;
end;



procedure FindThisKey(ThisKey: shortstring; Action: tAction);
var
    Keys, Fancy: ^tPreloadedFile;
    HashCode: integer;
    s: shortstring;
    An: int32;
    Po: int64;
    i, An2, ThisAn: integer;
    ThisValue, NewValue: integer;
    DbNr, Index: integer;
    Data: integer;
    IndexShl: array [0 .. 31] of integer;
    TempBit: integer;
    fd: integer;
    AllLocations, TitleOnly, UrlOnly: boolean;
    ThisRankValue: integer;
    //ThisRankFactor: double;
    ThisUrlData: byte;
    PathElements, HostElements: byte;
    TopHitsPointer: int64;
begin
    Ti4 := GetTickCount;
    Ti5 := 0;
    Ti6 := 0;

    if (LowerCase(copy(ThisKey, 1, 5)) = 'host:') or
    (LowerCase(copy(ThisKey, 1, 5)) = 'site:') then
    begin
        FindThisHost(copy(ThisKey, 6, 255), Action);
        exit;
    end;

    if LowerCase(copy(ThisKey, 1, 4)) = 'www.' then
    begin
        FindThisHost(ThisKey, Action);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = '.de' then
    begin
        FindThisHost('www.' + ThisKey, Action);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 3, 4)) = '.com' then
    begin
        FindThisHost('www.' + ThisKey, Action);
        exit;
    end;

    AllLocations := true;
    TitleOnly := false;
    UrlOnly := false;
    if LowerCase(copy(ThisKey, 1, 6)) = 'inurl:' then
    begin
        AllLocations := false;
        UrlOnly := true;
        Delete(ThisKey, 1, 6);
    end
    else
    if LowerCase(copy(ThisKey, 1, 8)) = 'intitle:' then
    begin
        AllLocations := false;
        TitleOnly := true;
        Delete(ThisKey, 1, 8);
    end;


    for i := 0 to 31 do
        IndexShl[i] := 1 shl i;

    case Action of
        acSet:
            begin
                BitFieldInitialized := false;
                FillChar(ValueTable, SizeOf(ValueTable), 0);
                if KeyWordCount > 1 then
                begin
                    for i := 0 to cDbCount - 1 do
                        FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
                    BitFieldInitialized := true;
                end;
            end;
        acAnd:
            begin
                for i := 0 to cDbCount - 1 do
                    FillChar(TempBitField[i]^, BitFieldSize[i] * 4, 0);
            end;
    end;

    Ti5 := GetTickCount - Ti4;
    HashCode := CalcCRC(ThisKey);;
    Str(HashCode and 63, s);
    Keys := @KeyDbs[HashCode and 63];
    Fancy := @FancyDbs[HashCode and 63];
    HashCode := (HashCode shr 6) and cMaxIndexHash;
    if Length(s) < 2 then s := '0' + s;
    FileMode := 0;

    Keys.Seek(HashCode * 8);
    Keys.Read(Po, 8);
    Keys.Seek(Po);

    while Po <> 0 do
    begin
        Keys.Read(s, 1);
        if s = '' then
        begin
            if Action = acAnd then
                for i := 0 to cDbCount - 1 do
                    FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
            Ti6 := GetTickCount - Ti5 - Ti4;
            exit;
        end;
        Keys.Read(s[1], Length(s));
        if s = ThisKey then
        begin
            Keys.Read(An, 4);
            Keys.Read(TopHitsPointer, SizeOf(TopHitsPointer));

            if (QueryPass = 1) and (TopHitsPointer <> -1) then
            begin
                Fancy.Seek(TopHitsPointer);
                Fancy.Read(An, 4);
            end;

            if (Action = acSet) and (not BitFieldInitialized) and (An > 0) then
            begin
                for i := 0 to cDbCount - 1 do
                    FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
                BitFieldInitialized := true;
            end;

            if KeyWordCount = 1 then
            begin
                MaxValue := 0;
                BitFieldInitialized := false;
            end;

            while An > 0 do
            begin
                ThisAn := An;
                if ThisAn > cMaxTempPages then ThisAn := cMaxTempPages;
                if (QueryPass = 2) or (TopHitsPointer = -1) then
                    Keys.Read(TempBuf, 4 * ThisAn)
                else
                    Fancy.Read(TempBuf, 4 * ThisAn);


                if (Action = acAnd) and AllLocations and (FilterMask = 0) then ProcessThisKeyViaAnd(ThisAn)
                else
                begin
                    for i := 1 to ThisAn do
                    begin
                        Data := TempBuf[i];
                        DbNr := (Data shr 3) and (cDbCount - 1);
                        Index := Data shr (3 + cDbBits);
                        if (AllLocations or (UrlOnly and ((Data and 4) <> 0)) or
                        (TitleOnly and ((Data and 2) <> 0))) and
                        ((FilterMask = 0) or
                        ((FilterMask <> 0) and ((FilterData[DbNr]^[Index] and FilterMask) <> 0)))
                        then
                        begin

                            if ((Action = acAnd) and ((BitField[DbNr]^[Index shr 5] and
                            IndexShl[Index and 31]) > 0)) or (Action <> acAnd) then
                            begin
                                ThisValue := b1;

                                // Keyword exists in description
                                if (Data and 1) <> 0 then Inc(ThisValue, b2);

                                // Keyword is in title
                                if (Data and 2) <> 0 then Inc(ThisValue, b3);

                                // Keyword is in URL
                                if (Data and 4) <> 0 then Inc(ThisValue, b4);

                                fd := FilterData[DbNr]^[Index];

                                // URL is domain-root ( = www.somedomain.com/ )
                                if (fd and 128) <> 0 then Inc(ThisValue, b5);

                                if (fd and 64) <> 0 then Inc(ThisValue, b6);
                                if PreferDe and ((fd and 32) <> 0) then Inc(ThisValue, 10);
                                if PreferEn and ((fd and 32) = 0) then Inc(ThisValue, 10);
                                Inc(ThisValue, (31 - (fd and 31)) * b7);

                                ThisRankValue := LookupDomainRank((Index shl cDbBits) or DbNr) + 1;
                                ThisUrlData := LookupUrlData((Index shl cDbBits) or DbNr) + 1;
                                HostElements := ThisUrlData and 15;

                                if ThisRankValue = 0 then ThisRankValue := 1000001;

                                Inc(ThisValue, Round(GetBackLinkValue(DbNr, Index)));
                                ThisValue :=
                                Round((1.0 - ThisRankValue * 0.000000027) * ThisValue / HostElements);



                                if ThisValue > 65535 then ThisValue := 65535;
                            end;

                            case Action of
                                acSet:
                                    begin
                                        if KeyWordCount > 1 then
                                        begin
                                            Values[DbNr]^[Index] := ThisValue;
                                            Inc(BitField[DbNr]^[Index shr 5], IndexShl[Index and 31]);
                                        end
                                        else
                                        begin
                                            Inc(Count);
                                            if ThisValue > MaxValue then MaxValue := ThisValue;
                                            Inc(ValueTable[ThisValue]);
                                            if ValueTable[ThisValue] <= 1024 then
                                            begin
                                                ValueData[ThisValue, ValueTable[ThisValue]] :=
                                                (Index shl cDbBits) or DbNr;
                                            end;
                                        end;
                                    end;
                                acAnd:
                                    begin
                                        if (BitField[DbNr]^[Index shr 5] and IndexShl[Index and 31]) > 0 then
                                        begin
                                            TempBit := TempBitField[DbNr]^[Index shr 5];
                                            NewValue := Values[DbNr]^[Index] + ThisValue;
                                            if NewValue > 65535 then NewValue := 65535;
                                            Values[DbNr]^[Index] := NewValue;
                                            TempBitField[DbNr]^[Index shr 5] :=
                                            TempBit + IndexShl[Index and 31];
                                        end;
                                    end;
                                acNot:
                                    begin
                                        BitField[DbNr]^[Index shr 5] := BitField[DbNr]^[Index shr 5] and
                                        (not IndexShl[Index and 31]);
                                    end;
                            end; { case Action of }
                        end; { Is in Filtermask }
                    end; { For-Loop }
                end;

                Dec(An, ThisAn);
            end; { while An>0 }

            Ti5 := GetTickCount - Ti4;
            if Action = acAnd then
            begin
                TempBit := 0;
                for DbNr := 0 to cDbCount - 1 do
                    for i := 0 to BitFieldSize[DbNr] - 1 do
                    begin
                        BitField[DbNr]^[i] :=
                        BitField[DbNr]^[i] and TempBitField[DbNr]^[i];
                        TempBit := TempBit or BitField[DbNr]^[i];
                    end;
                if TempBit = 0 then
                begin
                    EarlyAbort := true;
                end;
            end;

            Ti6 := GetTickCount - Ti5 - Ti4;
            exit;
        end;
        Keys.Read(An, 4);
        Keys.Read(TopHitsPointer, SizeOf(TopHitsPointer));
        Keys.Seek(Keys.FilePos + uint32(An) * 4);
    end;

    if Action = acAnd then
        for i := 0 to cDbCount - 1 do
            FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);

    Ti6 := GetTickCount - Ti5 - Ti4;
end;



procedure OptimizeQuery;
var
    i: integer;
    Method: (logAnd, logNot);
    ThisKey: shortstring;
    ChangesMade: boolean;

    procedure SwapKeywords(Nr1, Nr2: integer);
    var
        s: string;
        Action: tAction;
        Count: integer;
    begin
        s := KeyWords[Nr1];
        KeyWords[Nr1] := KeyWords[Nr2];
        KeyWords[Nr2] := s;

        Action := KeyWordAction[Nr1];
        KeyWordAction[Nr1] := KeyWordAction[Nr2];
        KeyWordAction[Nr2] := Action;

        Count := KeyWordResultCount[Nr1];
        KeyWordResultCount[Nr1] := KeyWordResultCount[Nr2];
        KeyWordResultCount[Nr2] := Count;

        ChangesMade := true;
    end;


begin
    for i := 1 to KeyWordCount do
    begin
        KeyWordIsHostName[i] := false;
        ThisKey := KeyWords[i];
        Method := logAnd;
        if ThisKey <> '' then
        begin
            case ThisKey[1] of
                '+':
                    begin
                        Method := logAnd;
                        Delete(ThisKey, 1, 1);
                    end;
                '-':
                    begin
                        Method := logNot;
                        Delete(ThisKey, 1, 1);
                    end;
            end;
        end;
        if Method = logAnd then KeyWordAction[i] := acAnd
        else KeyWordAction[i] := acNot;

        KeyWordResultCount[i] := FindKeyWordResultCount(ThisKey, i);
    end;

    repeat
        ChangesMade := false;
        for i := 1 to KeyWordCount - 1 do
        begin
            if (KeyWordAction[i] <> acAnd) and (KeyWordAction[i + 1] = acAnd) then SwapKeywords(i, i + 1);

            if (KeyWordAction[i] = acAnd) and (KeyWordAction[i + 1] = acAnd) and
            (KeyWordResultCount[i + 1] < KeyWordResultCount[i]) then SwapKeywords(i, i + 1);
        end;
    until not ChangesMade;
end;


procedure FindKeys;
var
    i: integer;
    ThisKey: shortstring;
    Method: (logAnd, logNot);
    s: string;
    ThisMax, ThisValue: integer;
    HashCode: integer;
begin
    MaxValue := 1;
    Count := 0;
    EarlyAbort := false;

    ThisQuery := '';
    ThisIsCachedResult := false;
    for i := 1 to KeyWordCount do
        if i = 1 then ThisQuery := KeyWords[i]
        else ThisQuery := ThisQuery + ' ' + KeyWords[i];
    ThisQuery := ThisQuery + #255 + IntToStr(b1) + #255 + IntToStr(b2) + #255 + IntToStr(b3) +
    #255 + IntToStr(b4) + #255 + IntToStr(b5) + #255 + IntToStr(b6) + #255 + IntToStr(b7);

    if FindParam('preferde') <> '' then ThisQuery := ThisQuery + #255 + 'preferde=on';
    if FindParam('preferen') <> '' then ThisQuery := ThisQuery + #255 + 'preferen=on';
    if FindParam('geronly') <> '' then ThisQuery := ThisQuery + #255 + 'geronly=on';

  (* Disable this code because there is a chance that something may be wrong with the cache.
     Not necessarily with this code, but maybe with the adding to cache. *)
    HashCode := CalcCRC(ThisQuery) and cMaxCachedResults;
    if CacheUsed[HashCode] then
    begin
        if CachedResults[HashCode].Query = ThisQuery then
        begin
            Inc(Counter);
            ThisIsCachedResult := true;
            BitFieldInitialized := false;
            Count := CachedResults[HashCode].Count;
            MaxValue := CachedResults[HashCode].MaxValue;
            FillChar(ValueTable, SizeOf(ValueTable), 0);
            ThisMax := Count;
            if ThisMax > 1000 then ThisMax := 1000;
            for i := 1 to ThisMax do
            begin
                ThisValue := CachedResults[HashCode].Values[i];
                Inc(ValueTable[ThisValue]);
                ValueData[ThisValue, ValueTable[ThisValue]] := CachedResults[HashCode].Pages[i];
            end;
            exit;
        end;
    end;

    if KeyWordCount = 0 then
    begin
        for i := 0 to cDbCount - 1 do
        begin
            FillChar(BitField[i]^, BitFieldSize[i] * 4, 0);
            BitField[i]^[BitFieldSize[i] - 1] := 0;
        end;
    end;

    OptimizeQuery;

    for i := 1 to KeyWordCount do
    begin
        ThisKey := KeyWords[i];
        Method := logAnd;
        if ThisKey <> '' then
        begin
            case ThisKey[1] of
                '+':
                    begin
                        Method := logAnd;
                        Delete(ThisKey, 1, 1);
                    end;
                '-':
                    begin
                        Method := logNot;
                        Delete(ThisKey, 1, 1);
                    end;
            end;
        end;

        if i = 1 then FindThisKey(ThisKey, acSet)
        else
        begin
            case Method of
                logAnd: FindThisKey(ThisKey, acAnd);
                logNot: FindThisKey(ThisKey, acNot);
            end;
        end;
        if EarlyAbort then break;
    end;
end;


procedure ShowLink(Nr, Value, LfdNr: integer; Li: tStringList);
var
    Url: shortstring;
    s: shortstring;
    DbNr: integer;
    DocID: integer;
    UrlPo: int64;
    PageInfoSize: integer;
begin
    PageInfoSize := 258 + cMaxUrlLength + cMaxTitleLength;
    DocID := Nr;

    DbNr := Nr and (cDbCount - 1);
    UrlPo := Nr shr cDbBits;
    UrlPo := UrlPo * PageInfoSize;

    Html[DbNr].Position := UrlPo;
    Html[DbNr].Read(Url, 1 + cMaxUrlLength);
    Li.Add('url=http://' + Url);

    Html[DbNr].Read(s, 1 + cMaxTitleLength);
    if Trim(s) = '' then s := '(Ohne Titel)'; // "Ohne Titel" is German for "Without a title"
    Li.Add('title=' + s);

    Html[DbNr].Read(s, 256);
    s := Trim(s);
    Li.Add('text=' + s);
    Li.Add('rel=' + IntToStr(Value));

    Li.Add('domainrank=' + IntToStr(LookupDomainRank(DocID)));
    Li.Add('backlinks=' + IntToStr(BackLinkData[DbNr][Nr shr cDbBits]));
end;


function GetURLbyDocumentID(DocID: integer): shortstring;
var
    DbNr, Posi: integer;
    Url: shortstring;
begin
    DbNr := DocID and (cDbCount - 1);
    Posi := (DocID shr cDbBits) * 378;
    Html[DbNr].Position := Posi;
    Html[DbNr].Read(Url, 61);
    Result := Url;
end;


procedure SwapInt(var i1, i2: integer);
var
    i: integer;
begin
    i := i1;
    i1 := i2;
    i2 := i;
end;


procedure GenResults(Li: tStringList);
var
    Nr: integer;
    i, j, i32: integer;
    Data: integer;
    EndWithNr: integer;
    ThisValue: integer;
    DbNr, HashCode: integer;
    BitFieldPtr: pBitField;
    ValuesPtr: pValues;
    f: TextFile;
begin
    if BitFieldInitialized then
    begin
        MaxValue := 1;
        FillChar(ValueTable, SizeOf(ValueTable), 0);

        for DbNr := 0 to cDbCount - 1 do
        begin
            BitFieldPtr := BitField[DbNr];
            ValuesPtr := Values[DbNr];

            for i := 0 to BitFieldSize[DbNr] - 1 do
            begin
                if BitFieldPtr^[i] <> 0 then
                begin
                    Data := BitFieldPtr^[i];
                    i32 := i * 32;

                    if (Data and 255) <> 0 then
                    begin
                        for j := 0 to 7 do
                        begin
                            if (Data and 1) > 0 then
                            begin
                                Inc(Count);
                                ThisValue := ValuesPtr^[i32];
                                if ThisValue > MaxValue then MaxValue := ThisValue;
                                Inc(ValueTable[ThisValue]);
                                if ValueTable[ThisValue] <= 1001 then
                                    ValueData[ThisValue, ValueTable[ThisValue]] := ((i32) shl cDbBits) or DbNr;
                            end;
                            Data := Data shr 1;
                            Inc(i32);
                        end;
                    end
                    else
                    begin
                        Data := Data shr 8;
                        Inc(i32, 8);
                    end;



                    if (Data and 255) <> 0 then
                    begin
                        for j := 0 to 7 do
                        begin
                            if (Data and 1) > 0 then
                            begin
                                Inc(Count);
                                ThisValue := ValuesPtr^[i32];
                                if ThisValue > MaxValue then MaxValue := ThisValue;
                                Inc(ValueTable[ThisValue]);
                                if ValueTable[ThisValue] <= 1001 then
                                    ValueData[ThisValue, ValueTable[ThisValue]] := ((i32) shl cDbBits) or DbNr;
                            end;
                            Data := Data shr 1;
                            Inc(i32);
                        end;
                    end
                    else
                    begin
                        Data := Data shr 8;
                        Inc(i32, 8);
                    end;



                    if (Data and 255) <> 0 then
                    begin
                        for j := 0 to 7 do
                        begin
                            if (Data and 1) > 0 then
                            begin
                                Inc(Count);
                                ThisValue := ValuesPtr^[i32];
                                if ThisValue > MaxValue then MaxValue := ThisValue;
                                Inc(ValueTable[ThisValue]);
                                if ValueTable[ThisValue] <= 1001 then
                                    ValueData[ThisValue, ValueTable[ThisValue]] := ((i32) shl cDbBits) or DbNr;
                            end;
                            Data := Data shr 1;
                            Inc(i32);
                        end;
                    end
                    else
                    begin
                        Data := Data shr 8;
                        Inc(i32, 8);
                    end;



                    if (Data and 255) <> 0 then
                    begin
                        for j := 0 to 7 do
                        begin
                            if (Data and 1) > 0 then
                            begin
                                Inc(Count);
                                ThisValue := ValuesPtr^[i32];
                                if ThisValue > MaxValue then MaxValue := ThisValue;
                                Inc(ValueTable[ThisValue]);
                                if ValueTable[ThisValue] <= 1001 then
                                    ValueData[ThisValue, ValueTable[ThisValue]] := ((i32) shl cDbBits) or DbNr;
                            end;
                            Data := Data shr 1;
                            Inc(i32);
                        end;
                    end;

                end;
            end;
        end;
    end;


    if StartWithNr > 991 then Count := 0;
    ResultCount := Count;
    Nr := 0;


    Li.Add('TotalCount=' + IntToStr(Count));
    if (Count = 0) and (QueryPass = 2) then
    begin
        Inc(NoResults);

        try
            AssignFile(f, 'noresults.txt');
            if FileExists('noresults.txt') then Append(f)
            else ReWrite(f);
            WriteLn(f, OrgRbs);
            CloseFile(f);
        except
        end;
    end;

    EndWithNr := StartWithNr + ShowCount - 1;
    if EndWithNr > Count then EndWithNr := Count;

    Li.Add('StartWith=' + IntToStr(StartWithNr));
    Li.Add('EndWith=' + IntToStr(EndWithNr));



    for ThisValue := MaxValue downto 0 do
    begin
        for i := 1 to ValueTable[ThisValue] do
        begin
            Inc(Nr);
            if (Nr >= StartWithNr) and (Nr <= EndWithNr) then
            begin
                j := ValueData[ThisValue, i];
                if (QueryPass = 2) or (ResultCount >= 1000) then ShowLink(j, ThisValue, Nr, Li);
            end; { Show this link }
            if Nr >= EndWithNr then break;
        end;
        if Nr >= EndWithNr then break;
    end;

    if (ResultCount >= 1000) or (QueryPass = 2) then
    begin
        if (not ThisIsCachedResult) and (Length(ThisQuery) <= 255) then
        begin
            HashCode := CalcCRC(ThisQuery) and cMaxCachedResults;
            CachedResults[HashCode].Query := ThisQuery;
            CachedResults[HashCode].Count := Count;
            CachedResults[HashCode].MaxValue := MaxValue;
            CacheUsed[HashCode] := true;
            Nr := 0;
            for ThisValue := MaxValue downto 0 do
            begin
                for i := 1 to ValueTable[ThisValue] do
                begin
                    Inc(Nr);
                    CachedResults[HashCode].Pages[Nr] := ValueData[ThisValue, i];
                    CachedResults[HashCode].Values[Nr] := ThisValue;
                    if Nr >= 1000 then break;
                end;
                if Nr >= 1000 then break;
            end;
        end;
    end;
end;


procedure RefineSearch;
var
    i, j: integer;
    s: shortstring;
begin
    i := 1;
    while i < KeyWordCount do
    begin
        if (KeyWords[i] = 'und') or (KeyWords[i] = 'and') then
        begin
            s := KeyWords[i + 1];
            if copy(s, 1, 1) = '+' then
                Delete(s, 1, 1);
            if copy(s, 1, 1) = '-' then
                Delete(s, 1, 1);
            s := '+' + s;
            KeyWords[i + 1] := s;
            for j := i + 1 to KeyWordCount do
                KeyWords[j - 1] := KeyWords[j];
            Dec(KeyWordCount);
        end
        else if (KeyWords[i] = 'nicht') or (KeyWords[i] = 'not') then
        begin
            s := KeyWords[i + 1];
            if copy(s, 1, 1) = '+' then
                Delete(s, 1, 1);
            if copy(s, 1, 1) = '-' then
                Delete(s, 1, 1);
            s := '-' + s;
            KeyWords[i + 1] := s;
            for j := i + 1 to KeyWordCount do
                KeyWords[j - 1] := KeyWords[j];
            Dec(KeyWordCount);
        end
        else
            Inc(i);
    end;


    i := 1;
    while i <= KeyWordCount do
    begin
        if IsFillWord(KeyWords[i]) then
        begin
            for j := i + 1 to KeyWordCount do
                KeyWords[j - 1] := KeyWords[j];
            Dec(KeyWordCount);
        end
        else
            Inc(i);
    end;
end;


procedure LoadCacheData;
var
    f: tCacheFile;
    i, j: integer;
    NonMinusOne: integer;
begin
    for i := 0 to 63 do
    begin
        KeyDbs[i] := tPreloadedFile.Create;
        if i < 10 then
            KeyDbs[i].Assign(cSData + 'keys0' + IntToStr(i) + '.idx')
        else
            KeyDbs[i].Assign(cSData + 'keys' + IntToStr(i) + '.idx');
        KeyDbs[i].OpenRead;

        FancyDbs[i] := tPreloadedFile.Create;
        if i < 10 then
            FancyDbs[i].Assign(cSData + 'fancy0' + IntToStr(i) + '.idx')
        else
            FancyDbs[i].Assign(cSData + 'fancy' + IntToStr(i) + '.idx');
        FancyDbs[i].OpenRead;
    end;

    NonMinusOne := 0;
    MaxBackLinkCount := 0;
    for i := 0 to cDbCount - 1 do
    begin
        f := tCacheFile.Create;
        f.Assign(cSData + 'filter.dat' + IntToStr(i));
        f.Reset;
        FilterDataSize[i] := f.FileSize;
        if FilterDataSize[i] < 8 then
            FilterDataSize[i] := 8;
        GetMem(FilterData[i], FilterDataSize[i]);
        f.Read(FilterData[i]^, f.FileSize);

        ValueSize[i] := f.FileSize;
        if ValueSize[i] < 8 then
            ValueSize[i] := 8;
        GetMem(Values[i], ValueSize[i] * 2);
        BitFieldSize[i] := (f.FileSize + 31) div 32;
        if BitFieldSize[i] < 8 then
            BitFieldSize[i] := 8;
        GetMem(BitField[i], BitFieldSize[i] * 4);
        GetMem(TempBitField[i], BitFieldSize[i] * 4);

        f.Close;
        f.Free;


        f := tCacheFile.Create;
        f.Assign(cSData + 'rank.dat' + IntToStr(i));
        f.Reset;
        SetLength(RankData[i], f.FileSize div 4);
        f.Read(RankData[i][0], f.FileSize);
        for j := 0 to High(RankData[i]) do
        begin
            if RankData[i][j] <> -1 then Inc(NonMinusOne);
        end;
        f.Close;
        f.Free;


        f := tCacheFile.Create;
        f.Assign(cSData + 'rank2.dat' + IntToStr(i));
        f.Reset;
        SetLength(UrlData[i], f.FileSize);
        f.Read(UrlData[i][0], f.FileSize);
        f.Close;
        f.Free;



        f := tCacheFile.Create;
        f.Assign(cSData + 'backlink.dat' + IntToStr(i));
        f.Reset;
        SetLength(BackLinkData[i], f.FileSize div 8);
        f.Read(BackLinkData[i][0], f.FileSize);
        for j := 0 to High(BackLinkData[i]) do
        begin
            if BackLinkData[i][j] > MaxBackLinkCount then MaxBackLinkCount := BackLinkData[i][j];
        end;
        f.Close;
        f.Free;

    end;
end;


procedure CheckDataPath;
var
    NewPath: string;
    i: integer;
    Sr: tSearchRec;
    Ti1: integer;
begin
    NewPath := '';
    if FileExists(cSearchFirstPath + 'ready2.dat') and FileExists(cSearchFirstPath + 'keys63.idx') and
    (not FileExists(cSearchSecondPath + 'ready2.dat')) then NewPath := cSearchFirstPath;

    if FileExists(cSearchSecondPath + 'ready2.dat') and FileExists(cSearchSecondPath + 'keys63.idx') and
    (not FileExists(cSearchFirstPath + 'ready2.dat')) then NewPath := cSearchSecondPath;

    if FileExists(cSearchFirstPath + 'ready2.dat') and FileExists(cSearchFirstPath + 'keys63.idx') and
    FileExists(cSearchSecondPath + 'ready2.dat') and FileExists(cSearchSecondPath + 'keys63.idx') then
    begin
        i := FindFirst(cSearchFirstPath + 'ready2.dat', faAnyFile, Sr);
        if i = 0 then
        begin
            Ti1 := Sr.Time;
            FindClose(Sr);
            i := FindFirst(cSearchSecondPath + 'ready2.dat', faAnyFile, Sr);
            if i = 0 then
            begin
                if Ti1 > Sr.Time then NewPath := cSearchFirstPath
                else NewPath := cSearchSecondPath;
            end;
            FindClose(Sr);
        end
        else FindClose(Sr);
    end;

    if NewPath = '' then exit;

    if NewPath <> cSData then
    begin
	WriteLn('Switching to new index in: ', NewPath);
        if cSData <> '' then
        begin
            for i := 0 to cDbCount - 1 do
            begin
                FreeMem(FilterData[i], FilterDataSize[i]);
                FreeMem(Values[i], ValueSize[i] * 2);
                FreeMem(BitField[i], BitFieldSize[i] * 4);
                FreeMem(TempBitField[i], BitFieldSize[i] * 4);
            end;

            for i := 0 to 63 do
            begin
                KeyDbs[i].Close;
                KeyDbs[i].Free;
                FancyDbs[i].Close;
                FancyDbs[i].Free;
            end;
        end;

        for i := 0 to cMaxCachedResults do
            CacheUsed[i] := false;

        Searchs := 0;
        NoResults := 0;
        Counter := 0;

        cSData := NewPath;
        LoadCacheData;
    end;
end;


procedure ShowQueryStatistics;
var
    s: string;
begin
    WriteLn('Stats: ', Searchs, ' of which ', NoResults, ' are without result');
    WriteLn('Stats: ', 0.001 * Ticks / Searchs: 5: 3, 'ms/query');
    WriteLn('Stats: ', 0.001 * MaxTicks: 5: 3, 'ms max/query');
    WriteLn('Stats: ', 0.001 * MinTicks: 5: 3, 'ms min/query');
    WriteLn('Stats: ', Counter, ' queries (', 100 * Counter div Searchs, '% cache-hits');
end;


procedure HandleCacheRefresh;
var
    Ti: dword;
begin
    Inc(RefreshCachesCountdown);
    if RefreshCachesCountdown >= 4000 then
    begin
        Ti := GetTickCount;
        ReferenceRAMCaches;
        WriteLn('RAM-Caches refreshed ', GetTickCount-Ti, 'ms');
        RefreshCachesCountdown := 0;
    end;
end;


function ParseParameter(Parameter: string; Default, Min, Max: integer): integer;
var
    Value: integer;
begin
    if not TryStrToInt(FindParam(Parameter), Value) then
        Value := Default;

    if Value < Min then
        Value := Min;

    if Value > Max then
        Value := Max;

    Result := Value;
end;


function ParseThisParameter(s: string; Default, Min, Max: integer): integer;
var
    Value: integer;
begin
    if s <> '' then
    begin
        if not TryStrToInt(s, Value) then
            Value := Default;
    end
    else Value := Default;

    if Value < Min then
        Value := Min;

    if Value > Max then
        Value := Max;

    Result := Value;
end;


procedure ProcessQueryParameters;
begin
    b1 := ParseParameter('b1', 8, 0, 32) * 256;
    b2 := ParseParameter('b2', 2, 0, 32) * 256;
    b3 := ParseParameter('b3', 2, 0, 32) * 256;
    b4 := ParseParameter('b4', 1, 0, 32) * 256;
    b5 := ParseParameter('b5', 1, 0, 32) * 256;
    b6 := ParseParameter('b6', 1, 0, 32) * 256;

    b7 := ParseParameter('b7', 1, 0, 32 * 256);
    // yes, for b7 it's really "32*256" and not "32)*256"

    // if all ranking parameters have NO weight, then give at least b1 some
    if (b1 = 0) and (b2 = 0) and (b3 = 0) and (b4 = 0) and (b5 = 0) and
    (b6 = 0) and (b7 = 0) then
        b1 := 256;

    FilterMask := 0;
    Begriff := FindParam('begriff');
    StartWithNr := ParseParameter('startwith', 1, 1, 201);
    ShowCount := 10;
    PreferDe := FindParam('preferde') <> '';
    PreferEn := FindParam('preferen') <> '';
    GerOnly := FindParam('geronly') <> '';
    if GerOnly then
        FilterMask := FilterMask or 32;
    ExtractKeywords;
    RefineSearch;
end;


procedure OpenSnippetDatabases;
var
    i: integer;
begin
    for i := 0 to cDbCount - 1 do
    begin
        Html[i] := tFileStream.Create(cSData + 'urls.dat' + IntToStr(i), fmOpenRead or fmShareDenyNone);
    end;
end;


procedure CloseSnippetDatabases;
var
    i: integer;
begin
    for i := 0 to cDbCount - 1 do
    begin
        Html[i].Free;
    end;
end;


procedure ReadQueryFromFile(FNam: string);
var
    Tries: integer;
begin
    FileMode := 0;
    Tries := 0;
    repeat
        AssignFile(f, cSearchTempDir + 'a' + FNam);
        try
            Reset(f);
            Tries := -1;
        except
            AddToMemo(DateToStr(Date) + ' ' + TimeToStr(Time) + ': ' + IntToStr
            (Tries) + ': Sharing violation');
            Inc(Tries);
            Sleep(250);
        end;
    until (Tries = -1) or (Tries = 10);

    if Tries = -1 then
    begin // this means we had success in opening the file containing the query
        ReadLn(f, MainQS);
        CloseFile(f);
    end
    else
    begin
        MainQS := '';
        AddToMemo('Fatal error');
    end;
end;


procedure CreateOutputFile(FNam: string);
begin
    FileMode := 2;
    AssignFile(f, cSearchTempDir + 'b' + FNam);
    try
        ReWrite(f);
    except
        // MessageBeep(0);
        halt;
    end;
end;


(*
procedure CheckForQueries;
var
    Sr: tSearchRec;
    Code: integer;
    FNam: string;
    s: string;
    Ti, Ti2, Ti3: int64;
    Li: tStringList;
    i: integer;
begin
    Li := tStringList.Create;
    Li.Sorted := false;
    Li.Duplicates := dupAccept;

    HandleCacheRefresh;

    Code := FindFirst(cSearchTempDir + 'a*', faAnyFile, Sr);
    while Code = 0 do
    begin
        try
            CritSec.Enter;

            FNam := Sr.Name;
            Delete(FNam, 1, 1);

            ReadQueryFromFile(FNam);
            CreateOutputFile(FNam);

            Ti := GetTickCount;

            ProcessQueryParameters;
            PreferEn := true;
            OpenSnippetDatabases;

            Ti2 := GetTickCount;
            QueryPass := 2;
            FindKeys;
            Ti3 := GetTickCount;
            Li.Clear;
            GenResults(Li);
            for i := 0 to Li.Count - 1 do
                WriteLn(f, Li.Strings[i]);
            CloseSnippetDatabases;

            Inc(Searchs);
            Ti := GetTickCount - Ti;
            if Ti < 0 then
                Ti := 0;
            Ticks := Ticks + Ti;
            if Ti < MinTicks then
                MinTicks := Ti;
            if Ti > MaxTicks then
                MaxTicks := Ti;
            FormatSettings.LongTimeFormat := 'hh:nn:ss';
            Str(Ti: 5, s);
            if Ti >= 0 then
            begin
                AddToMemo(s + 'ms ' + '(' + IntToStr(ResultCount)
                + ') ' + Begriff + '(' + IntToStr(KeyWordCount)
                + ')' + ' - ' + IntToStr(Ti3 - Ti2) + '/' + IntToStr
                (GetTickCount - Ti3) + ' (' + IntToStr(Ti5)
                + '/' + IntToStr(Ti6) + ')');
            end;

            SetMaxMemoLines(15);
            CloseFile(f);
            ShowQueryStatistics;

            if Ti > 2000 then
                AppendToLog(IntToStr(Ti) + 'ms   Query=' + Begriff);

            DeleteFile(cSearchTempDir + 'a' + FNam);
            Code := FindNext(Sr);
        finally
            CritSec.Leave;
        end;
    end;
    FindClose(Sr);

    Sleep(10);

    Li.Free;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
    Timer1.Enabled := false;
    if not IdHTTPServer1.Active then IdHTTPServer1.Active := true;
    // CheckForQueries;
    Timer1.Enabled := true;
end;
*)


procedure EmptyCache;
var
    i: integer;
begin
    for i := 0 to cMaxCachedResults do
        CacheUsed[i] := false;
end;


procedure InitServer;
var
    i: integer;
    s: string;
begin
    IdHTTPServer1:=TIdHTTPServer.Create;
    IdHTTPServer1.DefaultPort := 8081;
    i := 1;
    while i <= ParamCount do
    begin
        s := LowerCase(ParamStr(i));
        if (s = '-p') or (s = '-port') then
        begin
            Inc(i);
            IdHTTPServer1.DefaultPort := StrToIntDef(ParamStr(i), 8081);
        end;

        Inc(i);
    end;
    Randomize;
    RefreshCachesCountdown := 0;
    WriteLn('SearchServer ', cVersion);
    WriteLn(cCopyright);

    EmptyCache;
    ResetStatistics;

    cSData := '';
    CheckDataPath;
    if not IdHTTPServer1.Active then IdHTTPServer1.Active := true;
end;



procedure SetupQuery(Req: TIdHTTPRequestInfo; Res: TIdHTTPResponseInfo);
var
    Ti, Ti2, Ti3: int64;
    s: string;
    Li: tStringList;
    rbs: RawByteString;
    u8: UTF8String;
    s2: string;
begin
    try
        CritSec.Enter;
        CheckDataPath;


        Ti := GetTickCount;

        b1 := 8 * 256;
        b2 := 2 * 256;
        b3 := 2 * 256;
        b4 := 1 * 256;
        b5 := 1 * 256;
        b6 := 1 * 256;
        b7 := 1; // 1

        b1 := ParseThisParameter(Req.Params.Values['b1'], 8, 0, 32) * 256;
        b2 := ParseThisParameter(Req.Params.Values['b2'], 2, 0, 32) * 256;
        b3 := ParseThisParameter(Req.Params.Values['b3'], 2, 0, 32) * 256;
        b4 := ParseThisParameter(Req.Params.Values['b4'], 1, 0, 32) * 256;
        b5 := ParseThisParameter(Req.Params.Values['b5'], 1, 0, 32) * 256;
        b6 := ParseThisParameter(Req.Params.Values['b6'], 1, 0, 32) * 256;

        b7 := ParseThisParameter(Req.Params.Values['b7'], 1, 0, 32 * 256);
        // Yes, the above line IS CORRECT!


        rbs := Req.Params.Values['q'];
        OrgRbs := rbs;
        u8 := rbs;
        s := AnsiLowerCase(u8);
        Begriff := '';

        while s <> '' do
        begin
            s2 := copy(s, 1, 2);
            if (Length(s2) = 2) and (Ord(s[1]) = 227) then
            begin
                case Ord(s2[2]) of
                    164: Begriff := Begriff + 'ae';
                    182: Begriff := Begriff + 'oe';
                    188: Begriff := Begriff + 'ue';
                    132: Begriff := Begriff + 'ae';
                    150: Begriff := Begriff + 'oe';
                    156: Begriff := Begriff + 'ue';
                    159: Begriff := Begriff + 'ss';
                end;
                Delete(s, 1, 2);
            end
            else
            begin
                Begriff := Begriff + AnsiString(s[1]);
                Delete(s, 1, 1);
            end;
        end;

        FilterMask := 0;
        StartWithNr := ParseThisParameter(Req.Params.Values['startwith'], 1, 1, 991);
        ShowCount := ParseThisParameter(Req.Params.Values['showcount'], 10, 10, 1000);
        PreferDe := true;
        PreferEn := false;
        GerOnly := false;
        if GerOnly then
            FilterMask := FilterMask or 32;


        ExtractKeywords;
        RefineSearch;

        OpenSnippetDatabases;

        Ti2 := GetTickCount;
        QueryPass := 1;
        FindKeys;
        Ti3 := GetTickCount;
        Li := tStringList.Create;
        Li.Sorted := false;
        Li.Duplicates := dupAccept;
        GenResults(Li);
        if ResultCount < 1000 then
        begin
            QueryPass := 2;
            FindKeys;
            Li.Clear;
            GenResults(Li);
        end;

        //Res.CharSet := 'text/plain';
        Res.ContentText := Li.Text;

        Li.Free;
        CloseSnippetDatabases;

        Inc(Searchs);
        Ti := GetTickCount - Ti;
        if Ti < 0 then
            Ti := 0;
        Ticks := Ticks + Ti;
        if Ti < MinTicks then
            MinTicks := Ti;
        if Ti > MaxTicks then
            MaxTicks := Ti;
        FormatSettings.LongTimeFormat := 'hh:nn:ss';
        Str(Ti: 5, s);
        if Ti >= 0 then
        begin
            AddToMemo(s + 'ms ' + '(' + IntToStr(ResultCount)
            + ') ' + Begriff + '(' + IntToStr(KeyWordCount)
            + ')' + ' - ' + IntToStr(Ti3 - Ti2) + '/' + IntToStr
            (GetTickCount - Ti3) + ' (' + IntToStr(Ti5)
            + '/' + IntToStr(Ti6) + ')');
        end;

        ShowQueryStatistics;

        if Ti > 2000 then
            AppendToLog(IntToStr(Ti) + 'ms   Query=' + Begriff);

    finally
        CritSec.Leave;
    end;
end;


procedure IdHTTPServer1CommandGet(AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo;
    AResponseInfo: TIdHTTPResponseInfo);
begin
    if AnsiLowerCase(ARequestInfo.Document) = '/query.html' then
        SetupQuery(ARequestInfo, AResponseInfo);
end;


begin
    CritSec := tCriticalSection.Create;
    InitServer;
end.
