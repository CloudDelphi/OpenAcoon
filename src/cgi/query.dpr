{$IfDef DCC}
    {$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
    {$MINSTACKSIZE $00004000}
    {$MAXSTACKSIZE $00100000}
    {$IMAGEBASE $00400000}
{$EndIf}
{$APPTYPE CONSOLE}
program query;

uses
    SysUtils,
    Classes,
    CgiTools,
    Highlighter,
    Ini,
    Tools,
    KillThread,
    SearchResults,
    NewSearch,
    OSWrapper,
    PostProcess;

const
    CSearch = '/cgi/query.exe';

    // Set the maximum number of keywords for a query to 10
    CMaxKeywords = 10;

type
    pExEntry = ^tExEntry;

    tExEntry = record
        Kw: shortstring;
        Count: integer;
        Next: pExEntry;
    end;



procedure DoLayoutJSON(Sr: tSearchResults);
var
    i: integer;
begin
    WriteLn('{');
    WriteLn('"TotalCount": ', Sr.TotalCount, ',');
    WriteLn('"Count": ', Sr.Count, ',');

    WriteLn('"Results": [');
    for i := 1 to Sr.Count do
    begin
        WriteLn('{');

        WriteLn('"Url": "', MakeEntities(Sr.Result[i].Url), '",');
        WriteLn('"Title": "', MakeEntities(Sr.Result[i].Title), '",');
        WriteLn('"Snippet": "', MakeEntities(Sr.Result[i].Snippet), '",');
        WriteLn('"ShowUrl": "', MakeEntities(Sr.Result[i].ShowUrl), '"');

        if i = Sr.Count then
            WriteLn('}')
        else
            WriteLn('},');
    end;
    WriteLn(']');

    WriteLn('}');
end;


procedure DoHtmlLayout(Sr: tSearchResults);

    procedure ReplaceText(var s: string; const StrFrom, StrTo: string);
    var
        i: integer;
    begin
        i := Pos(LowerCase(StrFrom), LowerCase(s));
        if i > 0 then
        begin
            Delete(s, i, length(StrFrom));
            Insert(StrTo, s, i);
        end;
    end;


var
    Li: tStringList;
    FNam: string;
    i, j: integer;
    StrResults: string;
    StrIndent: string;
    StrQuery: string;
    s, QueryString: string;
    PageNr, LastPage, MinPage, MaxPage: integer;
    StartWithNr, EndWithNr: integer;
begin
    SetHighlightSearch(GetItem('q'));
    FNam := ExtractFilePath(ParamStr(0)) + 'query_template.html';

    Li := tStringList.Create;
    Li.Sorted := false;
    Li.Duplicates := dupAccept;
    i := 0;
    repeat
        try
            Li.LoadFromFile(FNam);
            break;
        except
            Inc(i);
            Sleep(75);
        end;
    until i >= 10;

    if Li.Count = 0 then
        exit;

    // Re-construct the keyword for the form

    s := GetItem('q');
    StrQuery := '';
    while s <> '' do
    begin
        if (length(s) >= 2) and (s[1] = #195) then
        begin
            case Ord(s[2]) of
                164:
                    StrQuery := StrQuery + 'ae';
                182:
                    StrQuery := StrQuery + 'oe';
                188:
                    StrQuery := StrQuery + 'ue';
                132:
                    StrQuery := StrQuery + 'ae';
                150:
                    StrQuery := StrQuery + 'oe';
                156:
                    StrQuery := StrQuery + 'ue';
                159:
                    StrQuery := StrQuery + 'ss';
            end;
            Delete(s, 1, 2);
        end
        else
        begin
            StrQuery := StrQuery + s[1];
            Delete(s, 1, 1);
        end;
    end;

    while Pos('"', StrQuery) > 0 do
        Delete(StrQuery, Pos('"', StrQuery), 1);

    // Construct the search-results HTML including the navigation to
    // further results.

    StrResults := '<div class="results">';
    if Sr.Count > 1000 then
        Sr.Count := 1000;

    for i := 1 to Sr.Count do
        if Sr.Result[i].ShowUrl = '' then
            Sr.Result[i].ShowUrl := Sr.Result[i].Url;

    if Sr.Count = 0 then
        StrResults := StrResults + '<p>Sorry, but we couldn''t find any results for your query.</p>'
    else if Sr.Count > 1 then
        StrResults := StrResults + '<p>We found a total of ' + IntToStr(Sr.TotalCount) + ' results:</p>'
    else // Sr.Count=1
        StrResults := StrResults + '<p>We found exactly one result:</p>';


    StartWithNr := 1;
    EndWithNr := 10;
    PageNr := 0;
    s := GetItem('startwith');
    if s <> '' then
    begin
        try
            StartWithNr := StrToIntDef(s, 1);
            PageNr := (StartWithNr - 1) div 10; // First page is PageNr=0 !

            if PageNr < 0 then
                PageNr := 0;

            if PageNr > 99 then
                PageNr := 99;

            StartWithNr := PageNr * 10 + 1;
            EndWithNr := StartWithNr + 9;
        except
            StartWithNr := 1;
        end;
    end;


    if EndWithNr > Sr.EndWith then
        EndWithNr := Sr.EndWith;

    for i := StartWithNr to EndWithNr do
    begin
        // Replace all "&nbsp;" with <space> in the snippet
        repeat
            j := Pos('&nbsp;', LowerCase(Sr.Result[i].Snippet));
            if j > 0 then
            begin
                Sr.Result[i].Snippet[j] := ' ';
                Delete(Sr.Result[i].Snippet, j + 1, 5);
            end;
        until j = 0;

        // Set a default title if none exists
        if Trim(Sr.Result[i].Title) = '' then
            Sr.Result[i].Title := '(No Title)';

        if copy(Sr.Result[i].ShowUrl, 1, 7) = 'http://' then
            Delete(Sr.Result[i].ShowUrl, 1, 7);
        if copy(Sr.Result[i].ShowUrl, 1, 8) = 'https://' then
            Delete(Sr.Result[i].ShowUrl, 1, 8);
        if Sr.Result[i].Indent then
            StrIndent := ' result_indent'
        else
            StrIndent := '';
        StrResults := StrResults + '<div class="result' + StrIndent + '" id="result' + IntToStr(i) + '">';



        StrResults := StrResults + '<a href="/cgi/out.exe?pos=' + IntToStr(i + Sr.StartWith - 1)
        + '&amp;url=' + EncodeUrl(Sr.Result[i].Url) + '" class="result_link">' + IntToStr(i + Sr.StartWith - 1)
        + ') ' + HighlightSearchWords(Sr.Result[i].Title) + '</a>' + '<br/>' + '<span class="result_url">' +
        HighlightSearchWords(Sr.Result[i].ShowUrl) + '</span><br/><span class="result_snippet">';


        if Pos(' ', Sr.Result[i].Snippet) = 0 then
        begin
            while Sr.Result[i].Snippet <> '' do
            begin
                StrResults := StrResults + HighlightSearchWords(copy(Sr.Result[i].Snippet, 1, 55)) + ' ';
                Delete(Sr.Result[i].Snippet, 1, 55);
            end;
        end
        else
            StrResults := StrResults + HighlightSearchWords(Sr.Result[i].Snippet);

        if Sr.Result[i].Age <> -1 then
        begin
            if Sr.Result[i].Age >= 172800 then
                s := 'Found about ' + IntToStr(Sr.Result[i].Age div 86400) + ' days ago'
            else if Sr.Result[i].Age >= 86400 then
                s := 'Found about 1 day ago'
            else if Sr.Result[i].Age >= 7200 then
                s := 'Found about ' + IntToStr(Sr.Result[i].Age div 3600) + ' hours ago'
            else if Sr.Result[i].Age >= 3600 then
                s := 'Found about 1 hour ago'
            else if Sr.Result[i].Age >= 120 then
                s := 'Found about ' + IntToStr(Sr.Result[i].Age div 60) + ' minutes ago'
            else
                s := 'Found about 1 minute ago';

            StrResults := StrResults + '<br/>' + '(' + s + ')';
        end;

        StrResults := StrResults + '</span>';


        if Sr.Result[i].ShowMore then
        begin
            StrResults := StrResults + '<p>&nbsp;</p><p class="more_results"><a href="' + CSearch + '?q=' +
            'host:' + EncodeUrl(Sr.Result[i].HostName) + '+' + GetItem2('q') + '">';
            StrResults := StrResults + 'More results from ';
            StrResults := StrResults + Sr.Result[i].HostName + '...</a></p>';
        end;

        StrResults := StrResults + '</div>'#13#10;
    end;

    if (StartWithNr + 10) > Sr.EndWith then
    begin
        i := Sr.Count;
        if i > 1000 then
            i := 1000;
        if i <> Sr.EndWith then
        begin
            StrResults := StrResults + '<p>&nbsp;</p><p>&nbsp;</p><p><strong>';
            StrResults := StrResults + 'NOTE: To improve readability of the search-results, some' +
            ' (almost) identical results were removed from the list.';

            StrResults := StrResults + '</strong></p>';
        end;
    end;

    // Add the navigation bar to StrResults
    if Sr.TotalCount > 0 then
    begin
        QueryString := GetItem2('q');
        StrResults := StrResults + '<div class="results_navbar"><strong>' + 'Result-pages:</strong> ';

        i := StartWithNr - 10; { !!!10 }
        if i > 0 then
        begin
            StrResults := StrResults + '<a href="' + CSearch + '?lang=en&amp;q=' + QueryString +
            '&amp;startwith=' + IntToStr(i) + '">&lt;&lt; Back</a> '
        end;

        StrResults := StrResults + '[ ';

        Inc(PageNr); // Convert to first page is 1 instead of 0
        LastPage := (Sr.EndWith + 9) div 10;
        if LastPage > 100 then
            LastPage := 100;
        MinPage := PageNr - 5;
        if MinPage < 1 then
            MinPage := 1;
        MaxPage := MinPage + 9;
        if MaxPage > LastPage then
        begin
            MaxPage := LastPage;
            MinPage := LastPage - 9;
            if MinPage < 1 then
                MinPage := 1;
        end;

        for i := MinPage to MaxPage do
        begin
            if (i * 10 - 9
                { !!! *10-9 } ) <= Sr.TotalCount then
            begin
                if (i * 10 - 9
                    { !!! 10-9 } ) = StartWithNr then
                    StrResults := StrResults + '<strong>' + IntToStr(i) + '</strong> '
                else
                    StrResults := StrResults + '<a href="' + CSearch + '?q=' + QueryString +
                    '&amp;startwith=' + IntToStr(i * 10 - 9) + '">' + IntToStr(i) + '</a> ';
            end;
        end;
        StrResults := StrResults + '] ';

        i := StartWithNr + 10; { !!! +10 }
        if (i <= Sr.EndWith) and (i <= 991) then
        begin
            StrResults := StrResults + '<a href="' + CSearch + '?q=' + QueryString +
            '&amp;startwith=' + IntToStr(i) + '">Next &gt;&gt;</a>'
        end;

        StrResults := StrResults + '</div>';
    end;


    StrResults := StrResults + '</div>';


    // And then replace the placeholders in the template with the actual text

    for i := 0 to Li.Count - 1 do
    begin
        s := Li.Strings[i];
        ReplaceText(s, '###Query###', StrQuery);
        ReplaceText(s, '###Results###', StrResults);
        Li.Strings[i] := s;
    end;

    WriteLn(Li.Text);
    Li.Free;
end;


procedure DoMain;
var
    StartTick: uint32;
    SearchThread: tNewSearchThread;
    SrOut: tSearchResults;
begin
    TKillThread.Create(35); // End this process after 35 seconds no matter what


    Randomize;
    WriteLn('Content-Type: text/html; charset=UTF-8');

    SearchThread := tNewSearchThread.Create(GetItem2('q'), GetItem('lang'));
    SearchThread.QueryUrl := 'http://127.0.0.1:8081/query.html';
    SearchThread.Start;

    StartTick := GetTickCount;
    repeat
        if SearchThread.Complete then
            break;
        Sleep(10);
    until (GetTickCount - StartTick) > 10000; // Timeout after 10 seconds;

    WriteLn;

    AdjustRanking(SearchThread.Sr, DefaultRP, cDefaultBaseDir);
    DoPostProcess(SearchThread.Sr, SrOut);
    if SrOut.TotalCount < SrOut.EndWith then
        SrOut.TotalCount := SrOut.EndWith;
    if SrOut.Count < SrOut.EndWith then
        SrOut.Count := SrOut.EndWith;

    if LowerCase(GetItem('format')) = 'json' then
        DoLayoutJSON(SrOut)
    else
        DoHtmlLayout(SrOut);
end;


begin
    DoMain;

end.
