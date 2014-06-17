unit OSWrapper;

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
