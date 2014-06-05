unit UrlDatabase;

(*
    OpenAcoon - An OpenSource Internet-Search-Engine
    Copyright (C) 1999-2013 Acoon GmbH

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


interface

uses
    GlobalTypes;


function HostOfUrl(Url: shortstring): shortstring;
function DbNrOfUrl(const Url: shortstring): int32;
function HashOfUrl(const Url: shortstring): int32;
function UrlHasBlockedExtension(Url: string):boolean;


implementation

uses
    DbTypes,
    Hash,
    SysUtils,
    Classes;

var
    BlockedExtensions: tStringList;



function UrlHasBlockedExtension(Url: string):boolean;
var
    i: integer;
begin
    (*
    repeat
        i:=Pos('.',Url);
        if i>0 then Delete(Url,1,i);
    until i=0;
    *)

    i:=Length(Url);
    while (i>0) do
    begin
        if Url[i]='.' then break
        else Dec(i);
    end;
    if i>0 then Delete(Url,1,i);
    Result:=BlockedExtensions.IndexOf(LowerCase(Url))<>-1;
end;



function HostOfUrl(Url: shortstring): shortstring;
var
    i: integer;
begin
    i := Pos('/', Url);
    if i > 0 then SetLength(Url, i - 1);

    Result := Url;
end;



function DbNrOfUrl(const Url: shortstring): int32;
begin
    // Result := CalcCRC(HostOfUrl(shortstring(LowerCase(Url)))) and (cDbCount - 1);
    Result := CalcCRC(shortstring(LowerCase(Url))) and (cDbCount - 1);
end;


function HashOfUrl(const Url: shortstring): int32;
begin
    // Result := CalcCRC(shortstring(LowerCase(Url))) and cMaxUrlHash;
    Result := (CalcCRC(LowerCase(Url)) shr cDbBits) and cMaxUrlHash;
end;


procedure LoadBlockedExtensions;
var
    f: TextFile;
    s: string;
begin
    BlockedExtensions := tStringList.Create;
    BlockedExtensions.Sorted := true;
    BlockedExtensions.Duplicates := dupIgnore;

    try
        AssignFile(f, 'BlockedExtensions.txt');
        Reset(f);
        while not eof(f) do
        begin
            ReadLn(f, s);
            if (s <> '') and (copy(s,1,2)<>'//') then BlockedExtensions.Add(LowerCase(s));
        end;
        CloseFile(f);
    except
        WriteLn('WARNING: "BlockedExtensions.txt" could not be found.');
    end;
end;

begin
    LoadBlockedExtensions;

end.
