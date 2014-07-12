unit GlobalTypes;

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

const
{$IFDEF UNIX}
    cCRLF = #10;
    cPathDelimiter = '/';
{$ELSE}
    cCRLF = #13#10;
    cPathDelimiter = '\';
{$ENDIF}


{$ifdef CPU64}
    {$define Is64Bit}
{$endif}
{$ifdef CPUX64}
    {$define Is64Bit}
{$endif}

{$ifdef Is64Bit}
    cIs64Bit = '64bit ';
{$else}
    cIs64Bit = '';
{$endif}

    cShortVersion = '5.0.1';
    cDate = '12-Jul-2014';
    cVersion = cShortVersion + ' '+cIs64Bit+' '+cDate;

    cCopyright = '(c) 1999-2014 Acoon GmbH';
    cVersionCopy = cVersion + '  ' + cCopyright;
    cGPLNotice = '';
    (*  'This program is licensed under GPL version 2 (see LICENSE.TXT)' + cCRLF +
      'and is provided AS IS WITHOUT ANY WARRANTY OF ANY KIND.'; *)

type
    int8 = shortint;
    int16 = smallint;
    int32 = integer;
    uint8 = byte;
    uint16 = word;
    uint32 = longword;


implementation


function IntSizesCorrect:boolean;
begin
    Result:=
	(SizeOf(int8) = 1) and
	(SizeOf(int16) = 2) and
	(SizeOf(int32) = 4) and
	(SizeOf(uint8) = 1) and
	(SizeOf(uint16) = 2) and
	(SizeOf(uint32) = 4) and
	(SizeOf(integer) = 4);
end;


begin
    if not IntSizesCorrect then
    begin
        WriteLn('!!! Internal Error !!!');
        WriteLn('The size of at least one integer-type does not match expectations.');
        WriteLn('Check "GlobalTypes.pas" !!!');

        WriteLn('SizeOf (int8)=',SizeOf(int8));
        WriteLn('SizeOf (int16)=',SizeOf(int16));
        WriteLn('SizeOf (int32)=',SizeOf(int32));
        WriteLn('SizeOf (uint8)=',SizeOf(int8));
        WriteLn('SizeOf (uint16)=',SizeOf(int16));
        WriteLn('SizeOf (uint32)=',SizeOf(int32));
        halt;
    end;

end.

