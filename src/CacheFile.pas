unit CacheFile;

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
    Classes;

type
    TCacheFile = class
        procedure Assign(fNam: string);
        procedure Reset;
        procedure Close;
        procedure Read(var x; len: int64);
        function FileSize: int64;
        function FilePos: int64;
        procedure Seek(Po: int64);
        function Eof: boolean;
    private
        F: tFileStream;
        Posi: int64;
        Size: int64;
        BufStart: int64;
        BufLen: int64;
        Buf: array [0 .. 64 * 1024 - 1] of byte;
        FileName: string;
    end;

    TBufWriteFile = class
        procedure Assign(fNam: string);
        procedure ReWrite;
        procedure Reset;
        procedure Close;
        procedure Write(var x; len: int64);
        function FileSize: int64;
    private
        BufLen: int64;
        Buf: array [0 .. 16 * 1024 * 1024 - 1] of byte;
        FSize: int64;
        FileName: string;
        F: tFileStream;

        procedure FlushBuffer;
    end;


    TPreloadedFile = class { Klassendeklaration für die TPreloadedFile Klasse }
        constructor Create;
        procedure Assign(fNam: string); // Die folgenden Namen sprechen für sich
        procedure OpenRead;
        procedure OpenReadWrite;
        procedure Close;
        procedure Read(var x; len: int64);
        procedure Write(var x; len: int64);
        function FileSize: int64;
        function FilePos: int64;
        procedure Seek(Po: int64);
        function Eof: boolean;
        procedure Preload;
        procedure UnloadCache;
        function IsPreloaded: boolean;
    private
        F: tFileStream;
        FileName: string;
        CacheSize: int64;
        CacheData: array of byte;
        Posi: int64;
    end;


implementation

uses
    SysUtils;

type
    tBufArr = array [0 .. 1000000000] of byte;
    pBufArr = ^tBufArr;


procedure TBufWriteFile.Assign;
begin
    FileName := fNam;
end;



procedure TBufWriteFile.ReWrite;
begin
    F := tFileStream.Create(FileName, fmCreate or fmShareDenyNone);
    BufLen := 0;
    FSize := 0;
end;



procedure TBufWriteFile.Reset;
begin
    F := tFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
    BufLen := 0;
    FSize := 0;
end;


procedure TBufWriteFile.FlushBuffer;
begin
    if BufLen > 0 then
    begin
        F.Write(Buf, BufLen);
        BufLen := 0;
    end;
end;


procedure TBufWriteFile.Close;
begin
    FlushBuffer;
    F.Free;
end;



procedure TBufWriteFile.Write;
begin
    if ((BufLen + len) > SizeOf(Buf)) and (BufLen > 0) then
    begin
        FlushBuffer;
    end;

    if len <= SizeOf(Buf) then
    begin
        Move(x, Buf[BufLen], len);
        Inc(BufLen, len);
    end
    else
    begin
        F.Write(x, len);
    end;

    Inc(FSize, len);
end;



function TBufWriteFile.FileSize;
begin
    Result := FSize;
end;


{ ----------------------------------------------- }


procedure TCacheFile.Assign;
begin
    FileName := fNam;
end;



procedure TCacheFile.Reset;
begin
    F := tFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    Size := F.Size;

    Posi := 0;
    BufStart := -1;
    BufLen := -1;
end;



procedure TCacheFile.Close;
begin
    F.Free;
end;



procedure TCacheFile.Read;
var
    pBuffer: pBufArr;
    startAt: integer;
    le: int64;
begin
    if (len = 1) and (Posi >= BufStart) and (Posi < BufStart + BufLen) then
    begin
        Move(Buf[Posi - BufStart], x, 1);
        Posi := Posi + 1;
        exit;
    end;

    pBuffer := @x;
    startAt := 0;

    while len > 0 do
    begin
        if (BufStart = -1) or (Posi < BufStart) or (Posi >= (BufStart + BufLen)) then
        begin
            BufStart := Posi;
            BufLen := Size - BufStart;
            if BufLen > SizeOf(Buf) then
            begin
                BufLen := SizeOf(Buf);
            end;

            // F.Seek(BufStart, soFromBeginning);
            F.Position := BufStart;
            F.Read(Buf, BufLen);
        end;

        le := BufLen + BufStart - Posi;
        if le > len then
        begin
            le := len;
        end;

        Move(Buf[Posi - BufStart], pBuffer^[startAt], le);
        Inc(Posi, le);
        Inc(startAt, le);
        Dec(len, le);
    end;
end;



function TCacheFile.FileSize;
begin
    Result := Size;
end;



function TCacheFile.FilePos;
begin
    Result := Posi;
end;



procedure TCacheFile.Seek;
begin
    Posi := Po;
end;



function TCacheFile.Eof;
begin
    if Size > 0 then
    begin
        Result := Posi >= Size;
    end
    else
    begin
        Result := Posi = Size;
    end;
end;


{ -------------------------------------------- }



constructor TPreloadedFile.Create;
begin
    inherited Create;
    FileName := '';
end;



procedure TPreloadedFile.Assign;
begin
    FileName := fNam;
    CacheSize := 0;
    SetLength(CacheData, 1);
end;



procedure TPreloadedFile.OpenRead;
begin
    CacheSize := 0;
    SetLength(CacheData, 1);
    Posi := 0;

    if FileExists(FileName) then
        F := tFileStream.Create(FileName, fmOpenRead or fmShareDenyNone)
    else
        F := tFileStream.Create(FileName, fmCreate or fmOpenRead, fmShareDenyNone)
end;



procedure TPreloadedFile.OpenReadWrite;
begin
    CacheSize := 0;
    SetLength(CacheData, 1);
    Posi := 0;
    if FileExists(FileName) then
        F := tFileStream.Create(FileName, fmOpenReadWrite, fmShareDenyNone)
    else
        F := tFileStream.Create(FileName, fmCreate or fmOpenReadWrite, fmShareDenyNone)
end;



procedure TPreloadedFile.Close;
begin
    UnloadCache;
    F.Free;
end;



procedure TPreloadedFile.UnloadCache;
begin
    if IsPreloaded then
    begin
        if CacheSize <> 0 then
        begin
            CacheSize := 0;
            SetLength(CacheData, 1);
        end;
    end;
end;



procedure TPreloadedFile.Preload;
var
    oldFilePos: int64;
    // s: string;
    Po, len, Bytes: int64;
begin
    UnloadCache;
    { Sicherheitshalber eventuell bereits vorhandenen Cache leeren }

    CacheSize := FileSize;
    // WriteLn('CacheSize=',CacheSize);
    try
        SetLength(CacheData, CacheSize);
        // WriteLn('Cache has ',High(CacheData)+1,' bytes');
        // ReadLn(s);
    except
        CacheSize := 0;
        SetLength(CacheData, 1);
        WriteLn(#13'Memory allocation in TPreloadedFile.Preload failed.');
        exit;
    end;
    if (High(CacheData) + 1) <> CacheSize then
    begin
        CacheSize := 0;
        SetLength(CacheData, 1);
        WriteLn(#13'Memory allocation in TPreloadedFile.Preload failed.');
        exit;
    end;


    oldFilePos := FilePos; { Alte Dateiposition merken }
    try
        // F.Seek(0, soFromBeginning);
        F.Position := 0;
        Po := 0;
        Bytes := CacheSize;
        while Bytes > 0 do
        begin
            len := Bytes;
            if len > 1048576 then len := 1048576;
            // WriteLn('Po=',Po);
            try
                CacheData[Po] := 0;
            except
                WriteLn('Oopsie... #1  High(CacheData)=', High(CacheData));
            end;
            try
                F.Read(CacheData[Po], len);
            except
                WriteLn('Oopsie... #2');
            end;
            Inc(Po, len);
            Dec(Bytes, len);
        end;
    except
        WriteLn(#13'Cache Preload in TPreloadedFile.Preload failed.');
        WriteLn('ReadFile caused an exception.');
        UnloadCache;
    end;

    Seek(oldFilePos); { Zurück zur ursprünglichen Dateiposition }
end;



procedure TPreloadedFile.Read;
begin
    if IsPreloaded and ((Posi + len) <= CacheSize) then
    { Ist der Bereich im Cache ? }
    begin
        Move(CacheData[Posi], x, len) { Ja, Daten aus Cache lesen }
    end
    else
    begin
        if F.Read(x, len) <> len then System.Write('Uh-oh... Read failed...');
        { Nein, Lesezugriff an Windows weiterreichen }
    end;
    Inc(Posi, len); { Dateipointer mitbewegen }
end;



procedure TPreloadedFile.Write;
begin
    if IsPreloaded and ((Posi + len) <= CacheSize) then
    { Ist der Bereich im Cache ? }
    begin
        Move(x, CacheData[Posi], len);
        { Ja, geänderte Daten im Cache aktualisieren }
        { Achtung!! Der Fall, daß die zu schreibenden Daten teilweise das Cache-Ende
          überlappen, wird NICHT abgefangen. Dies dürfte aufgrund der Benutzung in
          ImportUrls aber auch nicht vorkommen. Damit dies passieren könnte, müßten
          die Datenstrukturen in der URL-Datenbank bereits beschädigt sein,
          und dann kommt eh jede Hilfe zu spät. }
    end;

    F.Write(x, len);
    { Schreibzugriff an Windows weiterreichen }
    Inc(Posi, len); { Dateipointer mitbewegen }
end;



function TPreloadedFile.FileSize;
begin
    Result := F.Size; { Dateigröße ermitteln }
end;



function TPreloadedFile.IsPreloaded: boolean;
begin
    Result := High(CacheData) > 0;
end;



function TPreloadedFile.FilePos;
begin
    Result := Posi; { Dateiposition ermitteln }
end;



procedure TPreloadedFile.Seek;
begin
    // F.Seek(po, soFromBeginning);
    F.Position := Po;
    Posi := Po;
end;



function TPreloadedFile.Eof;
begin
    Result := FilePos >= FileSize; { Sind wir am Ende der datei angelangt ? }
end;



end.
