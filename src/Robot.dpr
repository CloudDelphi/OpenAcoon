program Robot;

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

{$M 32768,1048576}


uses
  Forms,
  Windows,
  RobotMain in 'RobotMain.pas' {Form1},
  FileLocation in 'FileLocation.pas',
  GlobalTypes in 'GlobalTypes.pas',
  HTTPClient in 'HTTPClient.pas',
  RobotsTxt in 'RobotsTxt.pas',
  Config in 'Config.pas',
  Logging in 'Logging.pas',
  MemoryPool in 'MemoryPool.pas',
  Hash in 'Hash.pas',
  MemoryFile in 'MemoryFile.pas',
  DNSResolver in 'DNSResolver.pas',
  RobotGetUrl in 'RobotGetUrl.pas';

{$R *.RES}
{$SETPEFLAGS IMAGE_FILE_LARGE_ADDRESS_AWARE}


begin
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
