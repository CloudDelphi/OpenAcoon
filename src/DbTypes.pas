unit DbTypes;

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

interface

uses
    GlobalTypes;
    
const
    // Usually use cDbBits=7 for the full-scale software and cDbBits=2 for the FreshBot
    cDbBits = 7; // Number of bits needed for the shard-number.
                 // cDbCount = 2^cDbBits !!!
    cDbCount = 1 shl cDbBits; // Number of database-shards

    // cMaxUrlLength = 200;
    // cMaxTitleLength = 100;
    cMaxUrlLength = 60;
    cMaxTitleLength = 60;
    // Above, use both 60 with the legacy full-scale software. Use UrlLength=200 and
    // TitleLength=100 for use with the FreshBot

    cMaxKeywords = 10000; // Maximum number of keywords per Web-Page

    // The following numbers all need to (2^x)-1, otherwise the code will NOT work!
    cMaxIndexHash = 4*1024*1024-1; // Size of RWI HashTable for keywords and hostnames
    // cMaxIndexHash = 16*1024-1; // Size of RWI HashTable for keywords and hostnames
    // Above, use the small number for the "FreshBot" and the higher number when compiling
    // for the regular full-scale software.

    cMaxUrlHash = ((256*1024*1024) div cDbCount)-1; // Size of the URL hashtable
    // cMaxUrlHash = ((1*1024*1024) div cDbCount)-1; // Size of the URL hashtable
    // Above, use the small number for the "FreshBot" and the higher number when compiling
    // for the regular full-scale software.

type
    tPriority = (prNormal,prIgnore);
    {
      prNormal = Jede andere Domain
      prIgnore = Diese URL ignorieren. Wird gesetzt wenn es die Seite nicht
            gibt, oder diese als nicht deutschsprachig erkannt wird. Außerdem
            wenn der Server beim Zugriff ein Relocate (HTTP-Status 301 oder 302)
            zurückgibt }
    tUrlData = packed record // URL data-structure
        Next: int32; // File-position of next entry
        InfPo: int64; // This is set to -1 while the URL has not been crawled,
                      // afterwards it points to the position in the info-database
                      // where the info about this page is stored
        InLinkCount: int64; // Counts the number of incoming links. While int64 is probably bigger than
            // necessary, it's better to be safe than have it overrun. :)
        Priority: tPriority;
        Url: string[cMaxUrlLength];
    end;
    tKey = packed record // Datenstruktur für die Keyword-Datenbank
        Next: int32; // Dateipointer auf den nächsten Listeneintrag
        Key: shortstring;
    end;
    tHit = packed record
        PageID: uint32;
        // PageID: Bit0=IsInDescription, Bit1=IsInTitle, Bit2=IsInURL
    end;
    tPageInfo = packed record // Alle Infos zu einer erfassten WWW-Seite
        Url: string[cMaxUrlLength]; // URL der Seite
        Title: string[cMaxTitleLength]; // Überschrift (<title>-Abschnitt) der Seite
        Description: shortstring; // Beschreibung der Seite. Die ersten 255
                                  // Zeichen oder der Description Meta-Tag
        WordCount: uint32; // Anzahl ALLER Wörter auf der Seite
        Language: int8; { 0=Deutsch; -1=Unbekannt }
        // Keys: array[1..cMaxKeywords] of tHit;
        // Anschließend an diese Datenstruktur folgt für jedes Keyword eine
        // THit-Datenstruktur
    end;

    
implementation

procedure AbortMsg(s: string);
begin
    WriteLn('Internal error: Size of ',s,' is incorrect.');
    halt;
end;


begin
    if SizeOf(tKey)<>260 then AbortMsg('tKey');
    if SizeOf(tHit)<>4 then AbortMsg('tHit');
    if SizeOf(tPageInfo)<>(cMaxUrlLength+cMaxTitleLength+263) then AbortMsg('tPageInfo');
end.

