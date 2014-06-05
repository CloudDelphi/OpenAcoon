unit UrlFind;

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

-------------------------------------------------------------------------------

ChangeLog:
28-May-2008 MikeS Added: New config-file option "ImportData.UseUrlPreload".

*)

interface

uses
    GlobalTypes,
    CacheFile,
    DbTypes;

var
    UrlDb: array [0 .. cDbCount - 1] of tPreloadedFile;

procedure InitUrlFind;
procedure DoneUrlFind;
procedure FindUrl(Url: shortstring; var DbNr, Po: int32; var UrlData: tUrlData);


implementation

uses
    SysUtils,
    Hash,
    FileLocation,
    Config,
    UrlDatabase;

var
    PreloadCount: integer;
    UseUrlPreload: boolean;


procedure InitUrlFind;
var
    i: integer;
begin
    UseUrlPreload := LowerCase(ConfigReadString('ImportData.UseUrlPreload')) = 'true';

    PreloadCount := 0;
    for i := 0 to cDbCount - 1 do
    begin
        UrlDb[i] := tPreloadedFile.Create;
        UrlDb[i].Assign(cUrlDb + IntToStr(i));
        UrlDb[i].OpenReadWrite;
    end;
end;


procedure DoneUrlFind;
var
    i: integer;
begin
    for i := 0 to cDbCount - 1 do
        UrlDb[i].Close;
end;


procedure FindUrl(Url: shortstring; var DbNr, Po: int32; var UrlData: tUrlData);
var
    i: integer;
    UrlPos: int64;
    HashCode: integer;
begin
    i := Pos(#255, Url);
    if i > 0 then
    begin
        try
            UrlPos := StrToIntDef(copy(Url, i + 1, 255), 0);
        except
            UrlPos := 0;
        end;
        Url := copy(Url, 1, i - 1);
    end
    else UrlPos := 0;
    Url := LowerCase(Url);

    // HashCode := CalcCRC(LowerCase(Url));
    // DbNr := HashCode and (cDbCount - 1);
    // HashCode := (HashCode shr cDbBits) and cMaxUrlHash;
    DbNr := DbNrOfUrl(Url);
    HashCode := HashOfUrl(Url);

    if UseUrlPreload and (not UrlDb[DbNr].IsPreloaded) then UrlDb[DbNr].Preload;

    if UrlPos > 0 then
    begin
        UrlDb[DbNr].Seek(UrlPos);
        FillChar(UrlData, SizeOf(UrlData), 0);
        UrlDb[DbNr].Read(UrlData, SizeOf(UrlData));
        if LowerCase(UrlData.Url) = Url then
        begin
            Po := UrlPos;
            exit;
        end;
    end;

    UrlDb[DbNr].Seek(HashCode * 4);
    UrlDb[DbNr].Read(Po, 4);
    while Po > 0 do
    begin
        UrlDb[DbNr].Seek(Po);
        UrlDb[DbNr].Read(UrlData, SizeOf(UrlData));
        if LowerCase(UrlData.Url) = Url then exit;
        if Po = UrlData.Next then
        begin
            WriteLn('Internal Error: Po=Next in FindUrl'#7);
            Po := 0;
            exit;
        end;
        Po := UrlData.Next;
    end;
    Po := 0;
end;

end.
