unit OSWrapper;

interface

uses
    Types;

function GetTickCount:int64;

implementation

uses
{$ifdef UNIX}
    SysUtils;
{$else}
    Windows;
{$endif}


function GetTickCount:int64;
begin
    {$ifdef UNIX}
    Result:=Trunc(Now*24*3600*1000);
    {$else}
    Result:=Windows.GetTickCount;
    {$endif}
end;



end.
