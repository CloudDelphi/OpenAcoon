program ImportData;

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

{$APPTYPE CONSOLE}


uses
  // Windows,
  SysUtils,
  Classes,
  DbTypes in 'DbTypes.pas',
  UrlFind in 'UrlFind.pas',
  Hash in 'Hash.pas',
  CacheFile in 'CacheFile.pas',
  FileLocation in 'FileLocation.pas',
  GlobalTypes in 'GlobalTypes.pas',
  Config in 'Config.pas';

var
    Url: shortstring;
    DbNr, Po: integer;
    UrlData: tUrlData;
    fUrls: TextFile;
    InfDb: array [0 .. cDbCount - 1] of tFileStream;
    KeyFileModified: boolean;
    OutBuffer: array [0 .. 20000] of byte;


procedure HandleNotModifieds;
var
    Sr: tSearchRec;
    An, ErrCode: integer;
    f: TextFile;
begin
    An := 0;
    ErrCode := FindFirst(cImportPath + '*.nmo', faAnyFile, Sr);
    while ErrCode = 0 do
    begin
        if (Sr.Attr and faDirectory) = 0 then
        begin
            try
                // Currently crawls do not use "If-Modified-Since" method.
                // All crawls are completely fresh. Thus we do not need this
                // code at the moment.
                (*
                AssignFile(f, cImportPath + Sr.Name);
                Reset(f);

                while not eof(f) do
                begin
                    ReadLn(f, Url);
                    FindUrl(Url, DbNr, Po, UrlData);
                    if Po <> 0 then
                    begin
                        UrlData.DoDownload := false;
                        UrlDb[DbNr].Seek(Po);
                        UrlDb[DbNr].Write(UrlData, SizeOf(UrlData));
                    end;

                    Inc(An);
                    if (An and 1023 = 0) then Write(#13, An);
                end;

                CloseFile(f);
                *)
                DeleteFile(cImportPath + Sr.Name);
            except
            end;
        end;
        ErrCode := FindNext(Sr);
    end;
    FindClose(Sr);
    WriteLn(#13, An, ' Not Modifieds');
end;


procedure HandleRedirects;
var
    Sr: tSearchRec;
    An, ErrCode: integer;
    f: TextFile;
    s: shortstring;
    b: byte;
    Url: shortstring;
begin
    An := 0;
    b := 0;
    ErrCode := FindFirst(cImportPath + '*.rel', faAnyFile, Sr);
    while ErrCode = 0 do
    begin
        if (Sr.Attr and faDirectory) = 0 then
        begin
            try
                AssignFile(f, cImportPath + Sr.Name);
                Reset(f);

                while not eof(f) do
                begin
                    ReadLn(f, Url);
                    ReadLn(f, s);
                    FindUrl(Url, DbNr, Po, UrlData);
                    if Po <> 0 then
                    begin
                        WriteLn(fUrls, s);

                        UrlData.Priority := prIgnore;
                        // UrlData.DoDownload := false;
                        UrlDb[DbNr].Seek(Po);
                        UrlDb[DbNr].Write(UrlData, SizeOf(UrlData));
                        if UrlData.InfPo <> -1 then
                        begin
                            // InfDb[DbNr].Seek(UrlData.InfPo, soFromBeginning);
                            InfDb[DbNr].Position := UrlData.InfPo;
                            InfDb[DbNr].Write(b, 1);
                        end;
                    end;

                    Inc(An);
                    if (An and 1023) = 0 then Write(#13, An);
                end;

                CloseFile(f);
                DeleteFile(cImportPath + Sr.Name);
            except
            end;
        end;
        ErrCode := FindNext(Sr);
    end;
    FindClose(Sr);
    WriteLn(#13, An, ' Redirects');
end;


procedure HandleIgnores;
var
    Sr: tSearchRec;
    An, ErrCode: integer;
    f: TextFile;
    b: byte;
    Url: shortstring;
begin
    An := 0;
    b := 0;
    ErrCode := FindFirst(cImportPath + '*.ign', faAnyFile, Sr);
    while ErrCode = 0 do
    begin
        if (Sr.Attr and faDirectory) = 0 then
        begin
            try
                AssignFile(f, cImportPath + Sr.Name);
                Reset(f);

                while not eof(f) do
                begin
                    ReadLn(f, Url);
                    FindUrl(Url, DbNr, Po, UrlData);
                    if Po <> 0 then
                    begin
                        UrlData.Priority := prIgnore;
                        // UrlData.DoDownload := false;
                        UrlDb[DbNr].Seek(Po);
                        UrlDb[DbNr].Write(UrlData, SizeOf(UrlData));
                        if UrlData.InfPo <> -1 then
                        begin
                            // InfDb[DbNr].Seek(UrlData.InfPo, soFromBeginning);
                            InfDb[DbNr].Position := UrlData.InfPo;
                            InfDb[DbNr].Write(b, 1);
                        end;
                    end;

                    Inc(An);
                    if (An and 1023) = 0 then Write(#13, An);
                end;

                CloseFile(f);
                DeleteFile(cImportPath + Sr.Name);
            except
            end;
        end;
        ErrCode := FindNext(Sr);
    end;
    FindClose(Sr);
    WriteLn(#13, An, ' Ignores');
end;


procedure HandleWWWData;
var
    Sr: tSearchRec;
    An, ErrCode: integer;
    f: TextFile;
    s: shortstring;
    PageInfo: tPageInfo;
    i: integer;
    Url2, Tit2, Des2, TempLanguage: shortstring;
    LinkCount: integer;
    b: byte;
    ThisKeyCount: int32;
    ThisKeyword: shortstring;
    Flags: byte;
    OkToWrite: boolean;
    OutBufPos: integer;
begin
    An := 0;
    b := 0;
    ErrCode := FindFirst(cImportPath + '*.www', faAnyFile, Sr);
    while ErrCode = 0 do
    begin
        if (Sr.Attr and faDirectory) = 0 then
        begin
            try
                AssignFile(f, cImportPath + Sr.Name);
                Reset(f);
                // WriteLn('Opening ',Sr.Name);

                while not eof(f) do
                begin
                    ReadLn(f, s);

                    if s = '#ignore' then
                    begin
                        ReadLn(f, Url);
                        FindUrl(Url, DbNr, Po, UrlData);
                        if Po <> 0 then
                        begin
                            UrlData.Priority := prIgnore;
                            // UrlData.DoDownload := false;
                            UrlDb[DbNr].Seek(Po);
                            UrlDb[DbNr].Write(UrlData, SizeOf(UrlData));
                            if UrlData.InfPo <> -1 then
                            begin
                                // InfDb[DbNr].Seek(UrlData.InfPo, soFromBeginning);
                                InfDb[DbNr].Position := UrlData.InfPo;
                                InfDb[DbNr].Write(b, 1);
                            end;
                        end;
                    end { #ignore }
                    else

                    if s = '#wwwdata' then
                    begin
                        ReadLn(f, Url);
                        // WriteLn('Url=',Url,' **');
                        ReadLn(f, TempLanguage);
                        TempLanguage := LowerCase(TempLanguage);
                        FindUrl(Url, DbNr, Po, UrlData);
                        // WriteLn('Po=',Po,'   DbNr=',DbNr,'   InfPo=',UrlData.InfPo,'   Url=',Url);
                        OkToWrite := Po <> 0;
                        if Po = 0 then
                        begin
                            UrlData.InfPo := -1;
                            WriteLn('Unable to locate URL "', Url, '"');
                            // halt;
                            // break;
                        end;

                        if UrlData.InfPo <> -1 then
                        // If there already is an entry for this URL, mark it
                        // as deleted by setting the URL to an empty string.
                        begin
                            // InfDb[DbNr].Seek(UrlData.InfPo, soFromBeginning);
                            InfDb[DbNr].Position := UrlData.InfPo;
                            InfDb[DbNr].Write(b, 1);
                        end;

                        UrlData.InfPo := InfDb[DbNr].Size;
                        i := Pos(#255, Url);
                        if i = 0 then PageInfo.Url := Url
                        else PageInfo.Url := copy(Url, 1, i - 1);
                        ReadLn(f, PageInfo.Title);
                        ReadLn(f, PageInfo.Description);
                        if TempLanguage = 'german' then PageInfo.Language := 0
                        else PageInfo.Language := -1;
                        Url2 := LowerCase(PageInfo.Url);
                        Tit2 := LowerCase(PageInfo.Title);
                        Des2 := LowerCase(PageInfo.Description);

                        // WriteLn('Url=',PageInfo.Url);
                        // WriteLn('Title=',PageInfo.Title);
                        // WriteLn('Snippet=',PageInfo.Description);


                        ReadLn(f, s); // This would be the Last-Modified-Date. Always "0" for now.
                        ReadLn(f, ThisKeyCount);
                        PageInfo.WordCount := ThisKeyCount;

                        OutBufPos := 0;
                        if OkToWrite then
                        begin
                            // UrlData.DoDownload := false;
                            UrlDb[DbNr].Seek(Po);
                            UrlDb[DbNr].Write(UrlData, SizeOf(UrlData));

                            InfDb[DbNr].Position := UrlData.InfPo;
                            // InfDb[DbNr].Write(PageInfo, SizeOf(PageInfo));
                            Move(PageInfo, OutBuffer[0], SizeOf(PageInfo));
                            OutBufPos := SizeOf(PageInfo);
                        end;

                        for i := 1 to ThisKeyCount do
                        begin
                            ReadLn(f, ThisKeyword);
                            // WriteLn('Keyword="',ThisKeyword,'"');
                            Flags := 0;
                            if Pos(ThisKeyword, Url2) > 0 then Inc(Flags, 4);
                            if Pos(ThisKeyword, Tit2) > 0 then Inc(Flags, 2);
                            if Pos(ThisKeyword, Des2) > 0 then Inc(Flags, 1);
                            if OkToWrite then
                            begin
                                if OutBufPos > (SizeOf(OutBuffer) - 300) then
                                begin
                                    InfDb[DbNr].Write(OutBuffer, OutBufPos);
                                    OutBufPos := 0;
                                end;

                                OutBuffer[OutBufPos] := Flags;
                                Move(ThisKeyword[0], OutBuffer[OutBufPos + 1], Length(ThisKeyword) + 1);
                                Inc(OutBufPos, Length(ThisKeyword) + 2);

                                // InfDb[DbNr].Write(Flags, 1);
                                // InfDb[DbNr].Write(ThisKeyword[0], Length(ThisKeyword) + 1);
                            end;
                        end;
                        if OutBufPos > 0 then InfDb[DbNr].Write(OutBuffer, OutBufPos);


                        ReadLn(f, LinkCount);
                        for i := 1 to LinkCount do
                        begin
                            ReadLn(f, s);
                            WriteLn(fUrls, s);
                        end;
                    end; { #wwwdata }

                    Inc(An);
                    if (An and 2047) = 0 then Write(#13, An);
                end;

                CloseFile(f);
                DeleteFile(cImportPath + Sr.Name);
            except
            end;
        end;
        ErrCode := FindNext(Sr);
    end;
    FindClose(Sr);
    WriteLn(#13, An, ' WWW-pages');
end;


var
    i: integer;

begin
    WriteLn('ImportData ', cVersionCopy);
    WriteLn(cGPLNotice);
    WriteLn;

    InitUrlFind;

    AssignFile(fUrls, cUrls);
    if FileExists(cUrls) then Append(fUrls)
    else ReWrite(fUrls);

    for i := 0 to cDbCount - 1 do
    begin
        if FileExists(cInfDb + IntToStr(i)) then
            InfDb[i] := tFileStream.Create(cInfDb + IntToStr(i), fmOpenReadWrite)
        else
            InfDb[i] := tFileStream.Create(cInfDb + IntToStr(i), fmOpenReadWrite or fmCreate);
    end;

    HandleNotModifieds;
    HandleRedirects;
    HandleIgnores;

    HandleWWWData;

    for i := 0 to cDbCount - 1 do
        InfDb[i].Free;

    CloseFile(fUrls);
    DoneUrlFind;

end.
