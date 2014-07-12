unit FileLocation;

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

var
    cUrls,cAddUrl,cIgnorePages,cUrlList,
    cIgnoreHosts,cUrlDb,cInfDb,cKeyDb, // cBitTable,
    cInPath,cParsedPath,cImportPath,cSpeedTrap,
    cSDataPath,cTempPath,
    cSearchTempDir,cSearchLogFile,
    cSearchFirstPath,cSearchSecondPath: string;


implementation

uses
    Config;

procedure CorrectPathSeparators(var Path: string);
{$ifdef UNIX}
var
    i: integer;
{$endif}
begin
    {$ifdef UNIX}
    for i:=1 to length(Path) do
	if Path[i]='\' then Path[i]:='/';
    {$endif}
end;


begin
    // I used ConfigReadString and manual checking for empty results
    // because ConfigReadStringDefault will return empty strings despite
    // having a different default if the setting exists, but has no value.
    // The file- and pathnames here MUST NOT be empty, so extra checking
    // is needed.
    // TODO: There is need for additional checking here. Pathnames should
    // already exist or otherwise be created. Also conversion between
    // slash and backslash for Windows/Linux/Mac compatibility should be
    // added.

    cUrls:=ConfigReadString('file-system.Urls');
    if cUrls='' then cUrls:='data\txt\urls.txt';
    CorrectPathSeparators(cUrls);

    cAddUrl:=ConfigReadString('file-system.AddUrl');
    if cAddUrl='' then cAddUrl:='data\txt\addurl.txt';
    CorrectPathSeparators(cAddUrl);

    cUrlList:=ConfigReadString('file-system.UrlList');
    if cUrlList='' then cUrlList:='data\txt\robot.url';
    CorrectPathSeparators(cUrlList);

    cIgnorePages:=ConfigReadString('file-system.IgnorePages');
    if cIgnorePages='' then cIgnorePages:='data\txt\ignorepages.txt';
    CorrectPathSeparators(cIgnorePages);

    cIgnoreHosts:=ConfigReadString('file-system.IgnoreHosts');
    if cIgnoreHosts='' then cIgnoreHosts:='data\txt\ignorehosts.txt';
    CorrectPathSeparators(cIgnoreHosts);



    cUrlDb:=ConfigReadString('file-system.UrlDb');
    if cUrlDb='' then cUrlDb:='data\db2.url';
    CorrectPathSeparators(cUrlDb);

    cInfDb:=ConfigReadString('file-system.InfDb');
    if cInfDb='' then cInfDb:='data\db2.inf';
    CorrectPathSeparators(cInfDb);

    cKeyDb:=ConfigReadString('file-system.KeyDb');
    if cKeyDb='' then cKeyDb:='data\db2.key';
    CorrectPathSeparators(cKeyDb);




    cInPath:=ConfigReadString('file-system.InPath');
    if cInPath='' then cInPath:='data\crawler\in\';
    CorrectPathSeparators(cInPath);

    cParsedPath:=ConfigReadString('file-system.ParsedPath');
    if cParsedPath='' then cParsedPath:='data\crawler\parsed\';
    CorrectPathSeparators(cParsedPath);

    cImportPath:=ConfigReadString('file-system.ImportPath');
    if cImportPath='' then cImportPath:='data\crawler\import\';
    CorrectPathSeparators(cImportPath);

    cSpeedTrap:=ConfigReadString('file-system.SpeedTrap');
    if cSpeedTrap='' then cSpeedTrap:='data\crawler\speedtrap.dat';
    CorrectPathSeparators(cSpeedTrap);



    cSDataPath:=ConfigReadString('file-system.SDataPath');
    if cSDataPath='' then cSDataPath:='data\sdata1\';
    CorrectPathSeparators(cSDataPath);

    cTempPath:=ConfigReadString('file-system.TempPath');
    if cTempPath='' then cTempPath:='data\tmp\';
    CorrectPathSeparators(cTempPath);



    cSearchTempDir:=ConfigReadString('file-system.Search.TempDir');
    if cSearchTempDir='' then cSearchTempDir:='data\search\';
    CorrectPathSeparators(cSearchTempDir);

    cSearchLogFile:=ConfigReadString('file-system.Search.LogFile');
    if cSearchLogFile='' then cSearchLogFile:='data\search.log';
    CorrectPathSeparators(cSearchLogFile);

    cSearchFirstPath:=ConfigReadString('file-system.Search.FirstPath');
    if cSearchFirstPath='' then cSearchFirstPath:='data\sdata1\';
    CorrectPathSeparators(cSearchFirstPath);

    cSearchSecondPath:=ConfigReadString('file-system.Search.SecondPath');
    if cSearchSecondPath='' then cSearchSecondPath:='data\sdata2\';
    CorrectPathSeparators(cSearchSecondPath);

end.
