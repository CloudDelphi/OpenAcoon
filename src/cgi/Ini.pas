unit Ini;

interface

function GetIni(section, name, def, fileName: shortstring): string;
procedure SetIni(section, name: shortstring; value: string;
    fileName: shortstring);

implementation

uses
    Windows, SysUtils;

const
    CharArrLen = 255;

type
    TCharArr = array [0 .. CharArrLen] of Char;



function GetIni(section, name, def, fileName: shortstring): string;
var
    se: TCharArr;
    na: TCharArr;
    de: TCharArr;
    fn: TCharArr;
    res: array [0 .. 2048] of Char;
begin
    StrPCopy(se, string(section));
    StrPCopy(na, string(Name));
    StrPCopy(de, string(def));
    StrPCopy(fn, string(fileName));
    GetPrivateProfileString(se, na, de, res, 2047, fn);
    Result := StrPas(res);
end;



procedure SetIni(section, name: shortstring; value: string;
    fileName: shortstring);
var
    se: TCharArr;
    na: TCharArr;
    fn: TCharArr;
    va: array [0 .. 2048] of Char;
begin
    StrPCopy(se, string(section));
    StrPCopy(na, string(Name));
    StrPCopy(va, string(value));
    StrPCopy(fn, string(fileName));
    WritePrivateProfileString(se, na, va, fn);
end;

end.
