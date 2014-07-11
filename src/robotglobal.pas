unit robotglobal;

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


type
    tUrlInfo = record
        Port: integer;
        Domain: shortstring;
        Path: shortstring;
        ModifiedStr: shortstring;
        UrlPos: shortstring;
        Url: shortstring;
        OrgUrl: shortstring;
    end;

    tIP4 = record
        case boolean of
            false: (b: array [1 .. 4] of byte);
            true: (IP: uint32);
    end;



var
    HTTPClientDefaultMaxSize: int32;
    HTTPClientDefaultUserAgent: AnsiString;



function Ip2Str(const IP: tIP4): string;



// ------------------------------------------------------------------


implementation

uses
    SysUtils;



function Ip2Str(const IP: tIP4): string;
begin
    Result :=
	IntToStr(IP. b[1]) + '.' +
	IntToStr(IP. b[2]) + '.' +
	IntToStr(IP. b[3]) + '.' +
	IntToStr(IP. b[4]);
end;



end.
