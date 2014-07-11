unit Hash;

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

uses
    GlobalTypes;

function CalcCRC(const s: shortstring):uint32;


implementation

const
    CRC32DivisorPolynom=$edb88320;
    {1110 1101  1011 1000  1000 0011  0010 0000}
    {   E    D     B    8     8    3     2    0}
    {X^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x^1+x^0}

    CRC32Seed=$ffffffff;

var
  CRC32Table: array[0..255] of uint32;


procedure InitializeCRC32Table;
var
    lbyte: integer;
    lbit: integer;
    llCRC32: uint32;
begin
    for lbyte:=0 to 255 do
    begin
        llCRC32:=lByte;
        for lbit:=0 to 7 do
        begin
            if odd(llCRC32) then
                llCRC32:=llCRC32 shr 1 xor CRC32DivisorPolynom
            else
                llCRC32:=llCRC32 shr 1;
        end;
        CRC32Table[lbyte]:=llCRC32;
    end;
end;


function CalcCRC(const s: shortstring):uint32;
var
  i: integer;
  llCRC: uint32;
begin
    llCRC:=CRC32Seed;
    for i:=1 to Length(s) do
        llCRC:=CRC32Table[(llCRC and 255) xor Ord(s[i])] xor (llCRC shr 8);
    llCRC:=llCRC xor CRC32Seed;
    Result:=llCRC;
end;


begin
    InitializeCRC32Table;
end.
