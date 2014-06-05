program Sleep;

(*
    OpenAcoon - An OpenSource Internet-Search-Engine
    Copyright (C) 1999-2008 Acoon GmbH

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as
    published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*)

{$APPTYPE CONSOLE}

uses
  SysUtils,
  GlobalTypes in 'GlobalTypes.pas';

var
    Seconds,i: integer;
begin
    WriteLn('SLEEP ',cVersionCopy);
    WriteLn(cGPLNotice);
    Seconds:=StrToIntDef(ParamStr(1),1);
    for i:=Seconds downto 1 do
    begin
        Write(#13,i,' ');
        SysUtils.Sleep(1000);
    end;
    WriteLn(#13,'    ');
end.
