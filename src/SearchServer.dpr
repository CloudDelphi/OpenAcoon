program SearchServer;

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

uses
    Forms,
    ServerMain in 'ServerMain.pas' {Form1} ,
    CacheFile in 'CacheFile.pas',
    Hash in 'Hash.pas',
    FileLocation in 'FileLocation.pas',
    GlobalTypes in 'GlobalTypes.pas',
    DomainRank in 'DomainRank.pas',
    Words in 'Words.pas';

{$R *.RES}


begin
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.Run;

end.
