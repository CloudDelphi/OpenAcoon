unit Tools;

interface

function SafeStrToIntDef(const s: string; default: integer): integer;
function BeginsWith(const toFind, findIn: string): boolean;
function BeginsWithAndDeleteIfTrue(const toFind: string;
var findIn: string): boolean;

implementation

uses
    SysUtils, Windows;



function BeginsWith(const toFind, findIn: string): boolean;
begin
    Result := toFind = copy(findIn, 1, Length(toFind));
end;


function BeginsWithAndDeleteIfTrue(const toFind: string; var findIn: string): boolean;
var
    tmpResult: boolean;
begin
    tmpResult := BeginsWith(toFind, findIn);
    if tmpResult then
    begin
        Delete(findIn, 1, Length(toFind));
    end;
    Result := tmpResult;
end;


function SafeStrToIntDef(const s: string; default: integer): integer;
begin
    try
        Result := StrToIntDef(s, default);
    except
        Result := default;
    end;
end;


end.
