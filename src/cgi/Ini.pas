unit Ini;

interface

function GetIni(section, name, def, fileName: shortstring): string;
procedure SetIni(section, name: shortstring; value: string;
    fileName: shortstring);

implementation

uses
    {$IfDef DCC}
    Windows,
    {$Else}
    IniFiles,
    {$EndIf}
    SysUtils;

const
    CharArrLen = 255;

type
    TCharArr = array [0 .. CharArrLen] of Char;



function GetIni(section, name, def, fileName: shortstring): string;
var
{$IfDef DCC}
    se: TCharArr;
    na: TCharArr;
    de: TCharArr;
    fn: TCharArr;
    res: array [0 .. 2048] of Char;
{$Else}
    ThisIni: TIniFile;
{$EndIf}
begin
{$IfDef DCC}
    StrPCopy(se, string(section));
    StrPCopy(na, string(Name));
    StrPCopy(de, string(def));
    StrPCopy(fn, string(fileName));
    GetPrivateProfileString(se, na, de, res, 2047, fn);
    Result := StrPas(res);
{$Else}
    ThisIni := TIniFile.Create(fileName);
    Result := ThisIni.ReadString(section, name, def);
    ThisIni.Free;
{$EndIf}
end;



procedure SetIni(section, name: shortstring; value: string;
    fileName: shortstring);
var
{$IfDef DCC}
    se: TCharArr;
    na: TCharArr;
    fn: TCharArr;
    va: array [0 .. 2048] of Char;
{$Else}
    ThisIni: TIniFile;
{$EndIf}
begin
{$IfDef DCC}
    StrPCopy(se, string(section));
    StrPCopy(na, string(Name));
    StrPCopy(va, string(value));
    StrPCopy(fn, string(fileName));
    WritePrivateProfileString(se, na, va, fn);
{$Else}
    ThisIni := TIniFile.Create(fileName);
    ThisIni.WriteString(section, name, value);
    ThisIni.Free;
{$EndIf}
end;

end.
