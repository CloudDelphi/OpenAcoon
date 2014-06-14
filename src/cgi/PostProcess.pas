unit PostProcess;

interface

uses
    SearchResults;

const
    rpPhraseInDomain = 0;
    rpPhraseInUrl = 1;
    rpPhraseInTitle = 2;
    rpPhraseInSnippet = 3;
    rpNoSpacesPhraseInDomain = 4;
    rpNoSpacesPhraseInUrl = 5;
    rpKeywordInDomain = 6;
    rpKeywordInUrl = 7;
    rpKeywordInTitle = 8;
    rpKeywordInSnippet = 9;
    rpKeywordHiddenInDomain = 10;
    rpKeywordHiddenInUrl = 11;
    rpKeywordHiddenInTitle = 12;
    rpKeywordHiddenInSnippet = 13;
    rpKeywordPosInUrl = 14;
    rpKeywordPosInTitle = 15;
    rpKeywordPosInSnippet = 16;
    rpUrlLength = 17;
    rpPathElements = 18;
    rpBackLinks = 19;
    rpDomainRank = 20;
    rpIsDomainRoot = 21;
    rpUrlStartsWithWWW = 22;

type
    tRankingParameters = record
        d: array [0 .. 22] of double;
    end;

var
    DefaultRP: tRankingParameters;
    cDefaultBaseDir: string;


procedure AdjustRanking(var SrIn: tSearchResults; const rp: tRankingParameters; const BaseDir: string);
procedure DoPostProcess(var SrIn, SrOut: tSearchResults);
procedure LoadRankingParametersFromIni(BaseDir: string; var rp: tRankingParameters);


implementation

uses
    SysUtils,
    Ini,
    CgiTools,
    Classes;

const
    ppCombinePerHost = 1;

type
    tResultInfo = record
        ResultCountForThisHostName: integer;
        ShownResultsForThisHostName: integer;
        TooMuchUnicode: boolean;
    end;

    tResultInfoArray = array of tResultInfo;

var
    fs: tFormatSettings;


procedure InitRankingParameters(var rp: tRankingParameters);
var
    i: integer;
begin
    for i := low(rp.d) to high(rp.d) do
        rp.d[i] := 0.0;
end;


function RankingFactor(BaseDir, s: string): double;
begin
    if not TryStrToFloat(GetIni('RankingFactors', s, '0.0', BaseDir + 'ranking.ini'), Result, fs) then
        Result := 0.0;
end;


procedure LoadRankingParametersFromIni(BaseDir: string; var rp: tRankingParameters);
begin
    InitRankingParameters(rp);

    rp.d[rpPhraseInDomain] := RankingFactor(BaseDir, 'PhraseInDomain');
    rp.d[rpPhraseInUrl] := RankingFactor(BaseDir, 'PhraseInUrl');
    rp.d[rpPhraseInTitle] := RankingFactor(BaseDir, 'PhraseInTitle');
    rp.d[rpPhraseInSnippet] := RankingFactor(BaseDir, 'PhraseInSnippet');
    rp.d[rpNoSpacesPhraseInDomain] := RankingFactor(BaseDir, 'NoSpacesPhraseInDomain');
    rp.d[rpNoSpacesPhraseInUrl] := RankingFactor(BaseDir, 'NoSpacesPhraseInUrl');
    rp.d[rpKeywordInDomain] := RankingFactor(BaseDir, 'KeywordInDomain');
    rp.d[rpKeywordInUrl] := RankingFactor(BaseDir, 'KeywordInUrl');
    rp.d[rpKeywordInTitle] := RankingFactor(BaseDir, 'KeywordInTitle');
    rp.d[rpKeywordInSnippet] := RankingFactor(BaseDir, 'KeywordInSnippet');
    rp.d[rpKeywordHiddenInDomain] := RankingFactor(BaseDir, 'KeywordHiddenInDomain');
    rp.d[rpKeywordHiddenInUrl] := RankingFactor(BaseDir, 'KeywordHiddenInUrl');
    rp.d[rpKeywordHiddenInTitle] := RankingFactor(BaseDir, 'KeywordHiddenInTitle');
    rp.d[rpKeywordHiddenInSnippet] := RankingFactor(BaseDir, 'KeywordHiddenInSnippet');
    rp.d[rpKeywordPosInUrl] := RankingFactor(BaseDir, 'KeywordPosInUrl');
    rp.d[rpKeywordPosInTitle] := RankingFactor(BaseDir, 'KeywordPosInTitle');
    rp.d[rpKeywordPosInSnippet] := RankingFactor(BaseDir, 'KeywordPosInSnippet');
    rp.d[rpUrlLength] := RankingFactor(BaseDir, 'UrlLength');
    rp.d[rpPathElements] := RankingFactor(BaseDir, 'PathElements');
    rp.d[rpBackLinks] := RankingFactor(BaseDir, 'BackLinks');
    rp.d[rpDomainRank] := RankingFactor(BaseDir, 'DomainRank');
    rp.d[rpIsDomainRoot] := RankingFactor(BaseDir, 'IsDomainRoot');
    rp.d[rpUrlStartsWithWWW] := RankingFactor(BaseDir, 'UrlStartsWithWWW');
end;


function FileNameFromKeyword(Keyword: string): string;
var
    i: integer;
begin
    Result := '';
    for i := 1 to Length(Keyword) do
    begin
        case Keyword[i] of
            'a' .. 'z', '0' .. '9', '-':
                Result := Result + Keyword[i];
        else
            Result := Result + '_';
        end;
    end;
    if Length(Result) > 100 then
        Result := copy(Result, 1, 100);
end;


function UnicodePercentage(s: string): integer;
var
    i: integer;
    IsUnicode: int32;
begin
    IsUnicode := 0;
    for i := 1 to Length(s) do
    begin
        case s[i] of
            'a' .. 'z', 'A' .. 'Z', '0' .. '9', ' ', '-', '_', '#', '&', #124:
                ;
        else
            Inc(IsUnicode);
        end;
    end;
    if Length(s) = 0 then
        Result := 0
    else
        Result := Trunc(100 * IsUnicode div Length(s));
end;


function HostFromUrl(s: string): string;
var
    i: integer;
begin
    if LowerCase(copy(s, 1, 7)) = 'http://' then
        Delete(s, 1, 7);
    if LowerCase(copy(s, 1, 8)) = 'https://' then
        Delete(s, 1, 8);
    i := Pos('/', s);
    if i = 0 then
        i := Length(s) + 1;
    Result := LowerCase(copy(s, 1, i - 1));
end;


procedure AddResult(var SrIn, SrOut: tSearchResults; ResultNr: integer; var ResultInfo: tResultInfoArray;
Options: integer);
var
    i, HostNameIndex: integer;
begin
    HostNameIndex := ResultNr;

    // We only need to do a hostname-lookup if this hostname occurs more than once
    if (ResultInfo[ResultNr].ResultCountForThisHostName > 1) and ((Options and ppCombinePerHost) > 0) then
    begin
        for i := 1 to ResultNr - 1 do
        begin
            if SrIn.Result[ResultNr].HostName = SrIn.Result[i].HostName then
            begin
                HostNameIndex := i;
                break;
            end;
        end;
    end;

    // Do not add this result if we already have 3 from this host
    if (ResultInfo[HostNameIndex].ShownResultsForThisHostName >= 3) and ((Options and ppCombinePerHost) > 0) then
        exit;

    // Do not add this result if it contains a backslash
    if Pos('\', SrIn.Result[ResultNr].Url) > 0 then
        exit;

    // Now check for duplicate content.
    for i := 1 to SrOut.EndWith do
    begin
        // It is duplicate content if the exact same Snippet has already been added
        // to the result-list.
        if SrOut.Result[i].Snippet = SrIn.Result[ResultNr].Snippet then
            exit;
    end;


    Inc(ResultInfo[HostNameIndex].ShownResultsForThisHostName);
    Inc(SrOut.EndWith);
    Inc(SrOut.Count);
    SrOut.Result[SrOut.EndWith] := SrIn.Result[ResultNr];
    if (Options and ppCombinePerHost) > 0 then
    begin
        if ResultInfo[HostNameIndex].ShownResultsForThisHostName > 1 then
            SrOut.Result[SrOut.EndWith].Indent := true;
        if ResultInfo[HostNameIndex].ShownResultsForThisHostName = 3 then
            SrOut.Result[SrOut.EndWith].ShowMore := true;
    end;
end;


procedure AddResults(var SrIn, SrOut: tSearchResults; var ResultInfo: tResultInfoArray; TooMuchUnicode: boolean;
Options: integer);
var
    i: integer;
begin
    for i := 1 to SrIn.EndWith do
    begin
        if (ResultInfo[i].TooMuchUnicode = TooMuchUnicode)  and (SrIn.Result[i].Url <> '') then
        begin
            AddResult(SrIn, SrOut, i, ResultInfo, Options);
        end;
    end;
end;


procedure CleanUpIndenting(var SrOut: tSearchResults; Options: integer);
var
    Page, i, j, FoundAt: integer;
    Temp: pSearchResult;
    MovedResults: boolean;
    Iterations: integer;
begin
    for Page := 0 to 99 do
    begin
        SrOut.Result[Page * 10 + 1].Indent := false;
        Iterations := 0;
        repeat
            MovedResults := false;

            for i := 3 to 10 do
            begin
                // if the Hostname is different than the one of the previous result, then
                // the result MAY need to be moved up.
                if SrOut.Result[Page * 10 + i].HostName <> SrOut.Result[Page * 10 + i - 1].HostName then
                begin
                    FoundAt := -1;
                    for j := i - 1 downto 1 do
                    begin
                        if SrOut.Result[Page * 10 + i].HostName = SrOut.Result[Page * 10 + j].HostName then
                        begin
                            FoundAt := j;
                            break;
                        end;
                    end;


                    if FoundAt >= 0 then
                    // There is another result with the same hostname on this result-page.
                    // So we need to move-up the current result to be right below the
                    // already existing one.
                    begin
                        MovedResults := true;
                        for j := i - 1 downto FoundAt + 1 do
                        begin
                            Temp := SrOut.Result[Page * 10 + j];
                            SrOut.Result[Page * 10 + j] := SrOut.Result[Page * 10 + j + 1];
                            SrOut.Result[Page * 10 + j + 1] := Temp;
                        end;
                    end;


                end;
            end;

            // This is kind of a watchdog, so that we can get out of the loop in case a bug
            // would otherwise keep us going through it again and again and again...
            // Yes, this is "bad" programming. But better bad programming than buggy programming.
            Inc(Iterations);
        until (not MovedResults) or (Iterations > 1000);


        // And now make sure that we only "Indent" a result if the one
        // above it on the *same* page has the same hostname.
        for i := 2 to 10 do
        begin
            if SrOut.Result[Page * 10 + i].HostName <> SrOut.Result[Page * 10 + i - 1].HostName then
                SrOut.Result[Page * 10 + i].Indent := false;
        end;
    end;
end;


procedure DeleteUpTo(const FindThis: string; var FindIn: string);
var
    i: integer;
begin
    i := Pos(FindThis, FindIn);
    if i > 0 then
        Delete(FindIn, 1, i + Length(FindThis) - 1);
end;


function AllBefore(const FindThis, FindIn: string): string;
var
    i: integer;
begin
    i := Pos(FindThis, FindIn);
    Result := copy(FindIn, 1, i - 1);
end;


procedure ProcessRankingAdjustment(var SrIn: tSearchResults; var LiRanking: tStringList);
var
    Li: tStringList;
    i, j, k: integer;
    s, TLD, Url, s1, s2, s3: string;
    d: double;
    BestRankValue, BestBacklinkCount: integer;
begin
    Li := tStringList.Create;
    Li.Sorted := false;
    Li.Duplicates := dupAccept;
    Li.Delimiter := ' ';

    BestRankValue := 0;
    BestBacklinkCount := 0;
    for k := 1 to SrIn.EndWith do
    begin
        if SrIn.Result[k].RankingValueInt > BestRankValue then
            BestRankValue := SrIn.Result[k].RankingValueInt;
        if SrIn.Result[k].BackLinksInt > BestBacklinkCount then
            BestBacklinkCount := SrIn.Result[k].BackLinksInt;
    end;


    for j := 0 to LiRanking.Count - 1 do
    begin
        s := LiRanking.Strings[j];
        if s <> '' then
        begin
            Li.DelimitedText := s;

            for i := 1 to SrIn.EndWith do
            begin
                case Li.Count of
                    2:
                        begin
                            s1 := LowerCase(Li.Strings[0]);
                            s2 := Li.Strings[1];
                            if s1 = 'all' then
                            begin
                                if TryStrToFloat(s2, d, fs) then
                                begin
                                    SrIn.Result[i].RankingValueInt := Round(d * SrIn.Result[i].RankingValueInt);
                                    SrIn.Result[i].BackLinksInt := Round(d * SrIn.Result[i].BackLinksInt);
                                end;
                            end
                            else if s1 = 'deleteurl' then
                            begin
                                Url := LowerCase(SrIn.Result[i].Url);
                                if copy(Url, 1, 7) = 'http://' then
                                    Delete(Url, 1, 7);
                                if copy(Url, 1, 8) = 'https://' then
                                    Delete(Url, 1, 8);
                                if Url = LowerCase(s2) then
                                begin
                                    SrIn.Result[i].Url := '';
                                    SrIn.Result[i].ShowUrl := '';
                                    SrIn.Result[i].RankingValueInt := 0;
                                end;
                            end;
                        end; // 2
                    3:
                        begin
                            s1 := LowerCase(Li.Strings[0]);
                            s2 := LowerCase(Li.Strings[1]);
                            s3 := Li.Strings[2];
                            if s1 = 'host' then
                            begin
                                if SrIn.Result[i].HostName = s2 then
                                begin
                                    if TryStrToFloat(s3, d, fs) then
                                    begin
                                        SrIn.Result[i].RankingValueInt := Round(d * SrIn.Result[i].RankingValueInt);
                                        SrIn.Result[i].BackLinksInt := Round(d * SrIn.Result[i].BackLinksInt);
                                    end;
                                end;
                            end // host
                            else if s1 = 'url' then
                            begin
                                Url := LowerCase(SrIn.Result[i].Url);
                                if copy(Url, 1, 7) = 'http://' then
                                    Delete(Url, 1, 7);
                                if copy(Url, 1, 8) = 'https://' then
                                    Delete(Url, 1, 8);
                                if Url = LowerCase(s2) then
                                begin
                                    if TryStrToFloat(s3, d, fs) then
                                    begin
                                        SrIn.Result[i].RankingValueInt := Round(d * SrIn.Result[i].RankingValueInt);
                                        SrIn.Result[i].BackLinksInt := Round(d * SrIn.Result[i].BackLinksInt);
                                    end;
                                end;
                            end // url
                            else if s1 = 'tld' then
                            begin
                                TLD := SrIn.Result[i].HostName;
                                while Pos('.', TLD) > 0 do
                                    Delete(TLD, 1, Pos('.', TLD));
                                if TLD = s2 then
                                begin
                                    if TryStrToFloat(s3, d, fs) then
                                    begin
                                        SrIn.Result[i].RankingValueInt := Round(d * SrIn.Result[i].RankingValueInt);
                                        SrIn.Result[i].BackLinksInt := Round(d * SrIn.Result[i].BackLinksInt);
                                    end;
                                end;
                            end; // host
                        end; // 3
                end; // case

                if Li.Count > 0 then // Now catch all cases that may have a variable number of arguments
                begin
                    s1 := LowerCase(Li.Strings[0]);
                    // Only process the "add" statement ONCE. Thus the "i=1"
                    if (s1 = 'add') and (SrIn.EndWith < high(SrIn.Result)) and (i = 1) and (Li.Count >= 3) then
                    begin
                        s3 := Li.Strings[2];
                        if TryStrToFloat(Li.Strings[1], d, fs) then
                        begin
                            Inc(SrIn.EndWith);
                            k := SrIn.EndWith;
                            if k > SrIn.TotalCount then
                                SrIn.TotalCount := k;
                            SrIn.Result[k].Url := s3;
                            SrIn.Result[k].ShowUrl := s3;
                            SrIn.Result[k].HostName := HostFromUrl(s3);
                            SrIn.Result[k].RankingValueInt := Round(d * BestRankValue);
                            SrIn.Result[k].BackLinksInt := Round(d * BestBacklinkCount);
                            DeleteUpTo(' ', s);
                            DeleteUpTo(' ', s);
                            DeleteUpTo(' ', s);
                            SrIn.Result[k].Title := AllBefore('^', s);
                            DeleteUpTo('^', s);
                            SrIn.Result[k].Snippet := s;
                        end;
                    end; // add

                end; // Li.Count>0
            end;
        end;
    end;
end;


function NormalizeString(s: string): string;
var
    i: integer;
begin
    Result := '';
    s := LowerCase(Trim(s));
    while s <> '' do
    begin
        case s[1] of
            'a' .. 'z', '0' .. '9', ' ':
                begin
                    Result := Result + s[1];
                end;
            'ä', 'Ä':
                Result := Result + 'ae';
            'ö', 'Ö':
                Result := Result + 'oe';
            'ü', 'Ü':
                Result := Result + 'ue';
            'ß':
                Result := Result + 'ss';
            #195:
                begin
                    Delete(s, 1, 1);
                    if Length(s) >= 1 then
                    begin
                        case Ord(s[1]) of
                            132, 164:
                                Result := Result + 'ae';
                            150, 182:
                                Result := Result + 'oe';
                            156, 188:
                                Result := Result + 'ue';
                            159:
                                Result := Result + 'ss';
                        else
                            Result := Result + ' ';
                        end;
                    end;
                end
        else
            Result := Result + ' ';
        end;
        Delete(s, 1, 1);
    end;

    repeat
        i := Pos('  ', Result);
        if i > 0 then
            Delete(Result, i, 1);
    until i = 0;
end;


procedure ReRank(var SrIn: tSearchResults; const rp: tRankingParameters);
var
    i, j, Position: integer;
    Domain, TLD, Url, Title, Snippet: string;
    PhraseInDomain, PhraseInUrl, PhraseInTitle, PhraseInSnippet: boolean;
    NoSpacesPhraseInDomain, NoSpacesPhraseInUrl: boolean;
    KeywordInDomain, KeywordInUrl, KeywordInTitle, KeywordInSnippet: boolean;
    KeywordHiddenInDomain, KeywordHiddenInUrl, KeywordHiddenInTitle, KeywordHiddenInSnippet: boolean;
    IsDomainRoot, UrlStartsWithWWW: boolean;
    Value: double;
    QueryWithoutSpaces: string;
    PathElements, UrlLength: integer;
    Keyword, Keyword2: tStringList;
    s, s2: string;
    Li: tStringList;
begin
    Keyword := tStringList.Create;
    Keyword.Sorted := true;
    Keyword.Duplicates := dupIgnore;
    Keyword.Delimiter := ' ';
    Keyword.DelimitedText := SrIn.Query;
    Keyword2 := tStringList.Create;
    Keyword2.Sorted := false;
    Keyword2.Duplicates := dupAccept;
    for i := 0 to Keyword.Count - 1 do
        Keyword2.Add(' ' + Keyword.Strings[i] + ' ');

    Li := tStringList.Create;
    Li.Sorted := false;
    Li.Duplicates := dupIgnore;
    Li.Delimiter := ' ';

    QueryWithoutSpaces := SrIn.Query;
    while Pos(' ', QueryWithoutSpaces) > 0 do
        Delete(QueryWithoutSpaces, Pos(' ', QueryWithoutSpaces), 1);

    for i := 1 to SrIn.EndWith do
    begin
        Url := LowerCase(SrIn.Result[i].Url);
        if copy(Url, 1, 7) = 'http://' then
            Delete(Url, 1, 7);
        if copy(Url, 1, 8) = 'https://' then
            Delete(Url, 1, 8);
        UrlLength := Length(Url);
        // if the URL is more than 100 characters long, then the actual length doesn't matter anymore
        if UrlLength > 100 then
            UrlLength := 100;


        Domain := Url;
        UrlStartsWithWWW := copy(Domain, 1, 4) = 'www.';
        if UrlStartsWithWWW then
            Delete(Domain, 1, 4);

        j := Pos('/', Domain);
        IsDomainRoot := j = Length(Domain);
        if j > 0 then
            Domain := copy(Domain, 1, j - 1);
        TLD := Domain;
        while Pos('.', TLD) > 0 do
            Delete(TLD, 1, Pos('.', TLD));
        Domain := copy(Domain, 1, Length(Domain) - Length(TLD));
        if copy(Domain, Length(Domain), 1) = '.' then
            Delete(Domain, Length(Domain), 1);


        PathElements := 0;
        while Url <> '' do
        begin
            Inc(PathElements);
            if Pos('/', Url) > 0 then
                Delete(Url, 1, Pos('/', Url))
            else
                Url := '';
        end;
        // if there are more than 10 path-elements, then it doesn't matter anymore
        if PathElements > 10 then
            PathElements := 10;

        Url := SrIn.Result[i].Url;
        if LowerCase(copy(Url, 1, 7)) = 'http://' then
            Delete(Url, 1, 7);
        if LowerCase(copy(Url, 1, 8)) = 'https://' then
            Delete(Url, 1, 8);
        if LowerCase(copy(Url, 1, 4)) = 'www.' then
            Delete(Url, 1, 4);
        Url := ' ' + NormalizeString(Url) + ' ';
        Domain := ' ' + NormalizeString(Domain) + ' ';
        Title := ' ' + NormalizeString(SrIn.Result[i].Title) + ' ';
        Snippet := ' ' + NormalizeString(SrIn.Result[i].Snippet) + ' ';

        PhraseInUrl := Pos(' ' + SrIn.Query + ' ', Url) > 0;
        PhraseInDomain := Pos(' ' + SrIn.Query + ' ', Domain) > 0;
        NoSpacesPhraseInUrl := Pos(QueryWithoutSpaces, Url) > 0;
        NoSpacesPhraseInDomain := Pos(QueryWithoutSpaces, Domain) > 0;
        PhraseInTitle := Pos(' ' + SrIn.Query + ' ', Title) > 0;
        PhraseInSnippet := Pos(' ' + SrIn.Query + ' ', Snippet) > 0;


        Value := 0;

        for j := 0 to Keyword.Count - 1 do
        begin
            s := Keyword[j];
            s2 := Keyword2[j];

            KeywordHiddenInUrl := Pos(s, Url) > 0;
            KeywordInUrl := Pos(s2, Url) > 0;
            if KeywordInUrl then
                KeywordHiddenInUrl := false;

            KeywordHiddenInDomain := Pos(s, Domain) > 0;
            KeywordInDomain := Pos(s2, Domain) > 0;
            if KeywordInDomain then
                KeywordHiddenInDomain := false;

            KeywordHiddenInTitle := Pos(s, Title) > 0;
            KeywordInTitle := Pos(s2, Title) > 0;
            if KeywordInTitle then
                KeywordHiddenInTitle := false;

            KeywordHiddenInSnippet := Pos(s, Snippet) > 0;
            KeywordInSnippet := Pos(s2, Snippet) > 0;
            if KeywordInSnippet then
                KeywordHiddenInSnippet := false;


            if KeywordInUrl then
                Value := Value + rp.d[rpKeywordInUrl];
            if KeywordHiddenInUrl then
                Value := Value + rp.d[rpKeywordHiddenInUrl];

            Li.DelimitedText := Trim(Url);
            Position := Li.IndexOf(s);
            if (Position >= 0) and (Position < 20) then
                Value := Value + rp.d[rpKeywordPosInUrl] * (20 - Position);

            if KeywordInDomain then
                Value := Value + rp.d[rpKeywordInDomain];
            if KeywordHiddenInDomain then
                Value := Value + rp.d[rpKeywordHiddenInDomain];

            if KeywordInTitle then
                Value := Value + rp.d[rpKeywordInTitle];
            if KeywordHiddenInTitle then
                Value := Value + rp.d[rpKeywordHiddenInTitle];

            Li.DelimitedText := Trim(Title);
            Position := Li.IndexOf(s);
            if (Position >= 0) and (Position < 20) then
                Value := Value + rp.d[rpKeywordPosInTitle] * (20 - Position);

            if KeywordInSnippet then
                Value := Value + rp.d[rpKeywordInSnippet];
            if KeywordHiddenInSnippet then
                Value := Value + rp.d[rpKeywordHiddenInSnippet];

            Li.DelimitedText := Trim(Snippet);
            Position := Li.IndexOf(s);
            if (Position >= 0) and (Position < 100) then
                Value := Value + rp.d[rpKeywordPosInSnippet] * (100 - Position);
        end;


        if PhraseInUrl then
            Value := Value + rp.d[rpPhraseInUrl];
        if NoSpacesPhraseInUrl then
            Value := Value + rp.d[rpNoSpacesPhraseInUrl];
        if PhraseInDomain then
            Value := Value + rp.d[rpPhraseInDomain];
        if NoSpacesPhraseInDomain then
            Value := Value + rp.d[rpNoSpacesPhraseInDomain];
        if PhraseInTitle then
            Value := Value + rp.d[rpPhraseInTitle];
        if PhraseInSnippet then
            Value := Value + rp.d[rpPhraseInSnippet];
        if IsDomainRoot then
            Value := Value + rp.d[rpIsDomainRoot];
        if UrlStartsWithWWW then
            Value := Value + rp.d[rpUrlStartsWithWWW];

        Value := Value + rp.d[rpUrlLength] * (100 - UrlLength);
        Value := Value + rp.d[rpPathElements] * (10 - PathElements);
        Value := Value + rp.d[rpBackLinks] * SrIn.Result[i].BackLinksInt;
        if SrIn.Result[i].DomainRankInt <> -1 then
            Value := Value + rp.d[rpDomainRank] * (1000000 - SrIn.Result[i].DomainRankInt);

        SrIn.Result[i].RankingValueInt := Round(Value * 10.0);
    end;
end;


procedure AdjustRanking(var SrIn: tSearchResults; const rp: tRankingParameters; const BaseDir: string);
var
    i: integer;
    Temp: pSearchResult;
    Swapped: boolean;
    LowestRanking: int64;
    LiRanking, Li: tStringList;
    s, FNam: string;
    OriginalQuery: string;
begin
    Li := tStringList.Create;
    Li.Sorted := false;
    Li.Duplicates := dupIgnore;
    Li.Delimiter := ' ';
    OriginalQuery := SrIn.Query;
    SrIn.Query := NormalizeString(SrIn.Query);


    ReRank(SrIn, rp);

    // Normalize the ranking-value, so that the lowest value will be 0
    LowestRanking := SrIn.Result[1].RankingValueInt;
    for i := 2 to SrIn.EndWith do
    begin
        if SrIn.Result[i].RankingValueInt < LowestRanking then
            LowestRanking := SrIn.Result[i].RankingValueInt;
    end;


    LiRanking := tStringList.Create;
    LiRanking.Sorted := false;
    LiRanking.Duplicates := dupIgnore;


    LiRanking.LoadFromFile(BaseDir + 'general.txt');
    ProcessRankingAdjustment(SrIn, LiRanking);


    Li.DelimitedText := SrIn.Query;

    LiRanking.Clear;
    for i := 0 to Li.Count - 1 do
    begin
        s := Li.Strings[i];
        if (s <> '') and (copy(s, 1, 1) <> '-') and (Pos(':', s) = 0) and (Pos('.', s) = 0) then
        begin
            FNam := BaseDir + 'Keywords\' + FileNameFromKeyword(s) + '.txt';
            if FileExists(FNam) then
                LiRanking.LoadFromFile(FNam);

            LiRanking.Add('host www.' + s + '.com 1.6');
            LiRanking.Add('host ' + s + '.com 1.5');
            LiRanking.Add('host www.' + s + '.gov 1.6');
            LiRanking.Add('host ' + s + '.gov 1.5');
            LiRanking.Add('host www.' + s + '.edu 1.6');
            LiRanking.Add('host ' + s + '.edu 1.5');
            LiRanking.Add('host www.' + s + '.de 1.6');
            LiRanking.Add('host ' + s + '.de 1.5');
            LiRanking.Add('host www.' + s + '.at 1.6');
            LiRanking.Add('host ' + s + '.at 1.5');
            LiRanking.Add('host www.' + s + '.ch 1.6');
            LiRanking.Add('host ' + s + '.ch 1.5');

            LiRanking.Add('url www.' + s + '.com/ 1.6');
            LiRanking.Add('url ' + s + '.com/ 1.5');
            LiRanking.Add('url www.' + s + '.gov/ 1.6');
            LiRanking.Add('url ' + s + '.gov/ 1.5');
            LiRanking.Add('url www.' + s + '.edu/ 1.6');
            LiRanking.Add('url ' + s + '.edu/ 1.5');
            LiRanking.Add('url www.' + s + '.de/ 1.6');
            LiRanking.Add('url ' + s + '.de/ 1.5');
            LiRanking.Add('url www.' + s + '.at/ 1.6');
            LiRanking.Add('url ' + s + '.at/ 1.5');
            LiRanking.Add('url www.' + s + '.ch/ 1.6');
            LiRanking.Add('url ' + s + '.ch/ 1.5');

        end;
    end;
    ProcessRankingAdjustment(SrIn, LiRanking);

    FNam := BaseDir + 'Queries\' + FileNameFromKeyword(SrIn.Query) + '.txt';
    if FileExists(FNam) then
    begin
        LiRanking.LoadFromFile(FNam);
        ProcessRankingAdjustment(SrIn, LiRanking);
    end;


    if SrIn.Language = 'en' then
        LiRanking.LoadFromFile(BaseDir + 'english.txt')
    else
        LiRanking.LoadFromFile(BaseDir + 'german.txt');
    ProcessRankingAdjustment(SrIn, LiRanking);


    repeat
        Swapped := false;
        for i := 1 to SrIn.EndWith - 1 do
        begin
            if SrIn.Result[i + 1].RankingValueInt > SrIn.Result[i].RankingValueInt then
            begin
                Swapped := true;
                Temp := SrIn.Result[i];
                SrIn.Result[i] := SrIn.Result[i + 1];
                SrIn.Result[i + 1] := Temp;
            end;
        end;
    until (not Swapped);

    SrIn.Query := OriginalQuery;
end;


procedure DoPostProcess(var SrIn, SrOut: tSearchResults);
var
    ResultInfo: tResultInfoArray;
    i, j: integer;
    Options: integer;
begin
    Options := 0;
    if (Pos('host:', LowerCase(SrIn.Query)) = 0) and (Pos('site:', LowerCase(SrIn.Query)) = 0) then
        Options := Options or ppCombinePerHost;

    SetLength(ResultInfo, SrIn.EndWith + 1);
    InitSearchResults(SrOut);
    SrOut.Language := SrIn.Language;
    SrOut.Count := SrIn.Count;
    SrOut.StartWith := SrIn.StartWith;
    SrOut.EndWith := 0;
    SrOut.TotalCount := SrIn.TotalCount;

    for i := 1 to SrIn.EndWith do
    begin
        // Check if the result has too many Unicode characters.
        // If yes, then this result will me moved to the end of the result-list.
        ResultInfo[i].TooMuchUnicode := false;
        if UnicodePercentage(SrIn.Result[i].Title) > 40 then
            ResultInfo[i].TooMuchUnicode := true;
        if UnicodePercentage(SrIn.Result[i].Snippet) > 20 then
            ResultInfo[i].TooMuchUnicode := true;
        ResultInfo[i].TooMuchUnicode := false;
    end;

    for i := 1 to SrIn.EndWith do
    begin
        // We need this initialized to 0 later, so just do it now and we'll have it done.
        ResultInfo[i].ShownResultsForThisHostName := 0;

        ResultInfo[i].ResultCountForThisHostName := 0;
        for j := 1 to SrIn.EndWith do
        begin
            if SrIn.Result[i].HostName = SrIn.Result[j].HostName then
                Inc(ResultInfo[i].ResultCountForThisHostName);
        end;
    end;


    // First add all results without too many Unicode characters
    AddResults(SrIn, SrOut, ResultInfo, false, Options);

    // Then add all results with too many Unicode characters
    AddResults(SrIn, SrOut, ResultInfo, true, Options);


    CleanUpIndenting(SrOut, Options);
end;


function UrlAlreadyInList(var Sr: tSearchResults; Url: string): boolean;
var
    i: integer;
begin
    Url := LowerCase(Url);
    for i := 1 to Sr.EndWith do
    begin
        if LowerCase(Sr.Result[i].Url) = Url then
        begin
            Result := true;
            exit;
        end;
    end;
    Result := false;
end;


begin
    // You should probably change this to an absolute path to where the ranking-data is stored
    cDefaultBaseDir := '..\..\ranking\';

    fs.ThousandSeparator := ',';
    fs.DecimalSeparator := '.';
    LoadRankingParametersFromIni(cDefaultBaseDir, DefaultRP);

end.
