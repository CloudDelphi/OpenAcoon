unit DbTypes;

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
    (*
      prNormal = Everything is fine. This URL can be crawled or has already been crawled.
      prIgnore = Ignore this URL. Will be set if the page does not exist or if
                 the server returns a 301 or 302 relocation.
    *)
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

    tHit = packed record
        PageID: uint32;
        // PageID: Bit0=IsInDescription, Bit1=IsInTitle, Bit2=IsInURL
    end;

    tPageInfo = packed record // All the info for a successfully crawled page
        Url: string[cMaxUrlLength];
        Title: string[cMaxTitleLength]; // <title>

        Description: shortstring;
        // Snippet. Limited to 255 characters.
        // Either from the page-text or from the "description" meta-tag

        WordCount: uint32;
        Language: int8; // 0=German; -1=Unknown
    end;


implementation


procedure AbortMsg(s: string);
begin
    WriteLn('Internal error: Size of ',s,' is incorrect.');
    halt;
end;


begin
    // Some data-structurs need to have fixed sizes.
    // Having the wrong sizes could damage the databases. VERY bad karma!!!
    // So make some checking to see if they are correct.
    if SizeOf(tHit)<>4 then AbortMsg('tHit');
    if SizeOf(tPageInfo)<>(cMaxUrlLength+cMaxTitleLength+263) then AbortMsg('tPageInfo');
end.

