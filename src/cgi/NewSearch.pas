unit NewSearch;

interface

uses
    Classes,
    SearchResults;

type
    tNewSearchThread = class(tThread)
    public
        Complete: boolean;
        Sr: tSearchResults;
        QueryUrl: string;
        constructor Create(AQuery, ALanguage: string);
    protected
        OriginalQuery, OriginalLanguage: string;
        procedure Execute; override;
    end;

implementation

uses
    //Windows,
    SysUtils,
    CgiTools,
    Tools,
    IdHTTP,
    OSWrapper;


const
    cDefaultQueryUrl = 'http://127.0.0.1:8081/query.html';


procedure NormalizeSearch(var s: string);
var
    Li: tStringList;
    i: integer;
    s2: string;
begin
    UnWebify(s);
    s := LowerCase(Trim(s));
    Li := tStringList.Create;
    Li.Sorted := true;
    Li.Duplicates := dupIgnore;

    for i := 1 to Length(s) do
    begin
        case s[i] of
            ',', '"', '#', #13, #10:
                s[i] := ' ';
        end;
    end;
    s := LowerCase(Trim(s));
    while Pos('  ', s) > 0 do
        Delete(s, Pos('  ', s), 1);

    while s <> '' do
    begin
        i := Pos(' ', s);
        if i = 0 then
            i := Length(s) + 1;
        s2 := copy(s, 1, i - 1);
        Delete(s, 1, i);
        if s2 <> 'and' then
            Li.Add(s2);
    end;

    s := '';
    // For the first pass ignore all keywords starting with a minus-sign
    for i := 0 to Li.Count - 1 do
    begin
        if copy(Li.Strings[i], 1, 1) <> '-' then
        begin
            if s = '' then
                s := Li.Strings[i]
            else
                s := s + ' ' + Li.Strings[i];
        end;
    end;

    // And now add all the keywords starting with a minus-sign
    for i := 0 to Li.Count - 1 do
    begin
        if copy(Li.Strings[i], 1, 1) = '-' then
        begin
            if s = '' then
                s := Li.Strings[i]
            else
                s := s + ' ' + Li.Strings[i];
        end;
    end;
end;


constructor tNewSearchThread.Create;
begin
    inherited Create(true);
    Complete := false;
    OriginalQuery := AQuery;
    OriginalLanguage := ALanguage;
    QueryUrl := cDefaultQueryUrl;
end;


procedure tNewSearchThread.Execute;
var
    i, j, LastSpace: integer;
    s, s2: string;
    LineNr: integer;
    QueryParamaters: RawByteString;
    Http: tidHttp;
    Response: RawByteString;
    Li: tStringList;
    NormalizedQuery: string;
begin
    InitSearchResults(Sr);
    Li := tStringList.Create;
    Li.Sorted := false;
    Li.Duplicates := dupAccept;

    if QueryUrl = '' then QueryUrl := cDefaultQueryUrl;

    s := OriginalQuery;
    NormalizeSearch(s);
    Sr.Query := OriginalQuery;
    UnWebify(Sr.Query);
    Sr.Language := LowerCase(OriginalLanguage);


    s2 := '';
    while s <> '' do
    begin
        if (Length(s) >= 2) and (Ord(s[1]) = 195) then
        begin
            case Ord(s[2]) of
                164:
                    s2 := s2 + 'ae';
                182:
                    s2 := s2 + 'oe';
                188:
                    s2 := s2 + 'ue';
                132:
                    s2 := s2 + 'ae';
                150:
                    s2 := s2 + 'oe';
                156:
                    s2 := s2 + 'ue';
                159:
                    s2 := s2 + 'ss';
            end;
            Delete(s, 1, 2);
        end
        else
        begin
            case s[1] of
                #228:
                    s2 := s2 + 'ae';
                #246:
                    s2 := s2 + 'oe';
                #252:
                    s2 := s2 + 'ue';
                #196:
                    s2 := s2 + 'ae';
                #214:
                    s2 := s2 + 'oe';
                #220:
                    s2 := s2 + 'ue';
                #223:
                    s2 := s2 + 'ss';
            else
                s2 := s2 + s[1];
            end;
            Delete(s, 1, 1);
        end;
    end;

    NormalizedQuery := s2;

    QueryParamaters := 'q=' + NormalizedQuery;
    QueryParamaters := QueryParamaters + '&startwith=1';
    QueryParamaters := QueryParamaters + '&showcount=1000';
    QueryParamaters := QueryParamaters + '&b1=8&b2=2&b3=2&b4=1&b5=1&b6=1&b7=1';

    Http := tidHttp.Create;

    try
        Response := Http.Get(QueryUrl + '?' + QueryParamaters);
        Li.Text := Response;
    except
        Response := '';
    end;


    for LineNr := 0 to Li.Count - 1 do
    begin
        s := Li.Strings[LineNr];
        s2 := AnsiLowerCase(s);

        if BeginsWithAndDeleteIfTrue('totalcount=', s2) then
        begin
            Sr.TotalCount := SafeStrToIntDef(s2, 0);
        end;

        if BeginsWithAndDeleteIfTrue('startwith=', s2) then
        begin
            Sr.StartWith := SafeStrToIntDef(s2, 0);
        end;

        if BeginsWithAndDeleteIfTrue('endwith=', s2) then
        begin
            Sr.EndWith := SafeStrToIntDef(s2, 0);
        end;

        if copy(s2, 1, 4) = 'url=' then
        begin
            Inc(Sr.Count);
            Delete(s, 1, 4);
            Sr.Result[Sr.Count].Url := s;
            Sr.Result[Sr.Count].ShowUrl := s;
            if LowerCase(copy(s, 1, 7)) = 'http://' then
                Delete(s, 1, 7);
            if LowerCase(copy(s, 1, 8)) = 'https://' then
                Delete(s, 1, 8);
            j := Pos('/', s);
            if j = 0 then
                j := Length(s) + 1;
            Sr.Result[Sr.Count].HostName := LowerCase(copy(s, 1, j - 1));

        end;

        if copy(s2, 1, 6) = 'title=' then
        begin
            Delete(s, 1, 6);
            Sr.Result[Sr.Count].Title := shortstring(copy(s, 1, 255));
        end;

        if copy(s2, 1, 5) = 'text=' then
        begin
            Delete(s, 1, 5);

            LastSpace := 0;
            for i := 1 to Length(s) do
            begin
                if s[i] = ' ' then
                    LastSpace := i;
                if (s[i] = ',') and ((i - LastSpace) > 60) then
                begin
                    s[i] := ' ';
                    LastSpace := i;
                end;
            end;

            Sr.Result[Sr.Count].Snippet := s;
        end;

        if BeginsWith('rel=', s2) then
        begin
            Delete(s, 1, 4);
            Sr.Result[Sr.Count].RankingValue := shortstring(copy(s, 1, 255));
            try
                Sr.Result[Sr.Count].RankingValueInt := StrToIntDef(Sr.Result[Sr.Count].RankingValue, 0);
            except
                Sr.Result[Sr.Count].RankingValueInt := 0;
            end;
        end;

        if BeginsWith('domainrank=', s2) then
        begin
            Delete(s, 1, 11);
            Sr.Result[Sr.Count].DomainRank := shortstring(copy(s, 1, 255));
            try
                Sr.Result[Sr.Count].DomainRankInt := StrToIntDef(Sr.Result[Sr.Count].DomainRank, 0);
            except
                Sr.Result[Sr.Count].DomainRankInt := 0;
            end;
        end;

        if BeginsWith('backlinks=', s2) then
        begin
            Delete(s, 1, 10);
            Sr.Result[Sr.Count].BackLinks := shortstring(copy(s, 1, 255));
            try
                Sr.Result[Sr.Count].BackLinksInt := StrToIntDef(Sr.Result[Sr.Count].BackLinks, 0);
            except
                Sr.Result[Sr.Count].BackLinksInt := 0;
            end;
        end;

    end;

    Complete := true;
end;

end.
