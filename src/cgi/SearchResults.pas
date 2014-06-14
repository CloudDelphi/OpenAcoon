unit SearchResults;

interface

type
    tSearchResult = record
        Title, Snippet, Url, ShowUrl, RankingValue, BackLinks, DomainRank, HostName: string;
        RankingValueInt, BackLinksInt, DomainRankInt: int64;
        Indent, ShowMore: boolean;
        Age: int64; // Age of the page in seconds
    end;

    pSearchResult = ^tSearchResult;

    tSearchResults = record
        Count: integer;
        StartWith: integer;
        EndWith: integer;
        TotalCount: integer;
        Query, Language: string;
        Result: array [1 .. 1010] of pSearchResult;
    end;

procedure InitSearchResults(var Sr: tSearchResults);

implementation


procedure InitSearchResults(var Sr: tSearchResults);
var
    i: integer;
begin
    Sr.Count := 0;
    Sr.StartWith := 0;
    Sr.EndWith := 0;
    Sr.TotalCount := 0;
    Sr.Query := '';
    Sr.Language := '';
    for i := low(Sr.Result) to high(Sr.Result) do
    begin
        New(Sr.Result[i]);
        Sr.Result[i].Title := '';
        Sr.Result[i].Snippet := '';
        Sr.Result[i].Url := '';
        Sr.Result[i].ShowUrl := '';
        Sr.Result[i].RankingValue := '';
        Sr.Result[i].BackLinks := '';
        Sr.Result[i].DomainRank := '';
        Sr.Result[i].HostName := '';
        Sr.Result[i].RankingValueInt := 0;
        Sr.Result[i].BackLinksInt := 0;
        Sr.Result[i].DomainRankInt := 0;
        Sr.Result[i].Indent := false;
        Sr.Result[i].ShowMore := false;
        Sr.Result[i].Age := -1; // -1 means that there is no valid age available
    end;

end;

end.
