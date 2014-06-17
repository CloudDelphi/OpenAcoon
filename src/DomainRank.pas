unit DomainRank;

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
    tDomainRank = class
    private
        RankData: array of pointer;
        procedure AddEntry(aRanking: integer; const aDomain: shortstring);
        function CalcHashCode(const aDomain: shortstring): integer;
        function NormalizeDomain(aDomain: shortstring): shortstring;
    public
        DomainCount: integer;
        LastDomain: shortstring;
        constructor Create;
        destructor Destroy; override;
        function GetDomainRanking(aDomain: shortstring): integer;
    end;

implementation

uses
    Hash, SysUtils;

const
    cHashElements = 1024 * 1024; // MUST (!) be a power of 2

type
    pRankData = ^tRankData;

    tRankData = record
        Next: pRankData;
        Rank: integer;
        Domain: shortstring;
    end;


procedure tDomainRank.AddEntry(aRanking: integer; const aDomain: shortstring);
var
    p: pRankData;
    HashCode: integer;
begin
    HashCode := CalcHashCode(aDomain);

    // The following checks if the domain is already in the list and just Exits if that is the case.
    p := RankData[HashCode];
    while p <> nil do
    begin
        if p^.Domain = aDomain then
            exit
        else
            p := p^.Next;
    end;

    GetMem(p, SizeOf(tRankData) - 255 + Length(aDomain));
    p^.Domain := aDomain;
    p^.Rank := aRanking;
    p^.Next := RankData[HashCode];
    RankData[HashCode] := p;
    Inc(DomainCount);
end;


constructor tDomainRank.Create;
var
    f: TextFile;
    Posi: integer;
    s: shortstring;
    i: integer;
begin
    inherited;

    // Initialize and zero the RankData array
    SetLength(RankData, cHashElements);
    for i := 0 to high(RankData) do
        RankData[i] := nil;

    DomainCount := 0;

    AssignFile(f, 'domainrank.txt');
    Reset(f);
    Posi := 0;
    while not eof(f) do
    begin
        ReadLn(f, s);
        s := NormalizeDomain(s);

        AddEntry(Posi, s);

        Inc(Posi);
    end;
    CloseFile(f);
end;


function tDomainRank.NormalizeDomain(aDomain: shortstring): shortstring;
var
    i: integer;
begin
    aDomain := shortstring(LowerCase(string(aDomain)));
    if copy(aDomain, 1, 7) = 'http://' then
        Delete(aDomain, 1, 7);

    if copy(aDomain, 1, 4) = 'www.' then
        Delete(aDomain, 1, 4);

    i := Pos('/', string(aDomain));
    if i > 0 then
        aDomain := copy(aDomain, 1, i - 1);

    Result := aDomain;
end;


destructor tDomainRank.Destroy;
var
    i: integer;
    p, Next: pRankData;
begin
    for i := 0 to high(RankData) do
    begin
        p := RankData[i];
        while p <> nil do
        begin
            Next := p^.Next;
            FreeMem(p, SizeOf(tRankData) - 255 + Length(p^.Domain));
            p := Next;
        end;
    end;

    inherited;
end;


function tDomainRank.GetDomainRanking(aDomain: shortstring): integer;
// Returns -1 if the Domain isn't found in the list.
var
    p: pRankData;
begin
    aDomain := NormalizeDomain(aDomain);
    LastDomain:=aDomain;

    p := RankData[CalcHashCode(aDomain)];
    while p <> nil do
    begin
        if p^.Domain = aDomain then
        begin
            Result := p^.Rank;
            exit;
        end;
        p := p^.Next;
    end;

    Result := -1;
end;


function tDomainRank.CalcHashCode(const aDomain: shortstring): integer;
begin
    Result := CalcCRC(aDomain) and high(RankData);
end;

end.
