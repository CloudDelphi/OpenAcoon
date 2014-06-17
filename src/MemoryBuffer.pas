unit MemoryBuffer;

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
    tMemoryBuffer = class
        constructor Create; overload;
        constructor Create(Size: int64); overload;
        procedure SetSize(Size: int64);
        procedure Seek(NewPosition: int64);
        procedure Advance(AdvanceBy: int64);
        procedure Rewind(RewindBy: int64);
        function GetCurrentCharacter: AnsiChar;
        function Eof: boolean;
        procedure WriteChar(c: AnsiChar);
        function PeekAhead(Len: integer): shortstring;
        function LowerPeekAhead(Len: integer): shortstring;
        function PeekChar: AnsiChar;
        procedure CopyFrom(var Source; Len: int64);
        procedure ConvertLowAsciiToSpaces;

    private
        Buffer: array of AnsiChar;
        Position: int64;
        ActualSize: int64;
    end;



implementation


constructor tMemoryBuffer.Create;
begin
    inherited Create;
    Seek(0);
    ActualSize := 0;
end;


procedure tMemoryBuffer.Advance(AdvanceBy: int64);
begin
    Seek(Position + AdvanceBy);
end;


procedure tMemoryBuffer.ConvertLowAsciiToSpaces;
var
    i: integer;
begin
    for i := 0 to ActualSize - 1 do
        if Buffer[i] < #32 then Buffer[i] := #32;
end;


procedure tMemoryBuffer.CopyFrom(var Source; Len: int64);
begin
    if Len <= (High(Buffer) + 1) then
    begin
        Move(Source, Buffer[0], Len);
        ActualSize := Len;
        Position := 0;
    end;
end;


constructor tMemoryBuffer.Create(Size: int64);
begin
    Create;
    SetSize(Size);
end;


function tMemoryBuffer.Eof: boolean;
begin
    Result := Position >= ActualSize;
end;


function tMemoryBuffer.GetCurrentCharacter: AnsiChar;
begin
    if Eof then Result := ' '
    else Result := Buffer[Position];
end;


function tMemoryBuffer.LowerPeekAhead(Len: integer): shortstring;
var
    i: integer;
begin
    Result:=PeekAhead(Len);
    for i:=1 to Length(Result) do
        if (Result[i]>='A') and (Result[i]<='Z') then
            Inc(Result[i],32);
end;


function tMemoryBuffer.PeekAhead(Len: integer): shortstring;
var
    i: integer;
    c: AnsiChar;
    s: shortstring;
begin
    if Len > 255 then Len := 255;
    s := '';

    for i := 1 to Len do
    begin
        if (Position + i) < ActualSize then
        begin
            c := Buffer[Position + i];

            // Convert UpperCase to LowerCase for A..Z
            if (c >= 'A') and (c <= 'Z') then Inc(c, 32);

            Inc(s[0]);
            s[Length(s)] := c;
        end;
    end;

    Result:=s;
end;


function tMemoryBuffer.PeekChar: AnsiChar;
begin
    if (Position + 1) < ActualSize then Result := Buffer[Position + 1]
    else Result := ' ';
end;


procedure tMemoryBuffer.Rewind(RewindBy: int64);
begin
    Dec(Position, RewindBy);
    if Position < 0 then Position := 0;
end;


procedure tMemoryBuffer.Seek(NewPosition: int64);
begin
    Position := NewPosition;
end;


procedure tMemoryBuffer.SetSize(Size: int64);
begin
    SetLength(Buffer, Size);
end;


procedure tMemoryBuffer.WriteChar(c: AnsiChar);
begin
    if Position <= High(Buffer) then
    begin
        Buffer[Position] := c;
        Inc(Position);
        if Position > ActualSize then ActualSize := Position;
    end;
end;

end.
