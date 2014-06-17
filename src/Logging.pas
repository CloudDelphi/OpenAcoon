unit Logging;

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

procedure LogMsg(const aFilename, aMessage: string);
procedure DebugLogMsg(const aFilename, aMessage: string);


implementation

uses
    SysUtils,
    SyncObjs;

var
    CritSec: TCriticalSection;


procedure DebugLogMsg(const aFilename, aMessage: string);
begin
    // LogMsg(aFilename, aMessage);
end;



procedure LogMsg(const aFilename, aMessage: string);
var
    f: TextFile;
begin
    CritSec.Enter;
    try
        AssignFile(f, aFilename);
        if FileExists(aFilename) then Append(f)
        else ReWrite(f);
        WriteLn(f, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' ', aMessage);
        CloseFile(f);
    except
    end;
    CritSec.Leave;
end;


begin
    CritSec := TCriticalSection.Create;

end.
