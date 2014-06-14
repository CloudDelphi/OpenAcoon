unit CgiTools;

interface


function GetItem(const itemName: string): string;
function GetItem2(const itemName: string): string;
procedure UnWebify(var s: string);
procedure UnWebify2(var s: string);
function Webify(s: string): string;
function MakeEntities(s: string): string;
function MakeEntitiesNoAmp(s: string): string;
function EncodeUrl(s: string): string;
function DecodeUrl(s: string): string;
function EncodeUrl2(s: string): string;


implementation

uses
    Windows,
    SysUtils;



function GetEnv(name: pchar): string;
var
    Txt: array [0 .. 10000] of char;
begin
    Txt[0] := #0;
    GetEnvironmentVariable(name, Txt, 10000);
    Result := StrPas(Txt);
end;


function DecodeUrl(s: string): string;
var
    i: integer;
    s2: string;
begin
    s2 := '';
    i := 1;
    while i <= Length(s) do
    begin
        case s[i] of
            '+':
                s2 := s2 + ' ';
            '%':
                begin
                    if (i + 2) <= Length(s) then
                    begin
                        s2 := s2 + chr(StrToIntDef('$' + copy(s, i + 1, 2),
                                32));
                        Inc(i, 2);
                    end
                    else
                        s2 := s2 + s[i];
                end
            else
                s2 := s2 + s[i];
        end;
        Inc(i);
    end;
    Result := s2;
end;


procedure UnWebify(var s: string);
var
    i, x, y: integer;
begin
    i := 1;
    while (i + 2) <= Length(s) do
    begin
        if s[i] = '+' then
            s[i] := ' ';
        if s[i] = '%' then
        begin
            x := 0;
            y := 0;
            case s[i + 1] of
                '0' .. '9':
                    x := Ord(s[i + 1]) - 48;
                'A', 'a':
                    x := 10;
                'B', 'b':
                    x := 11;
                'C', 'c':
                    x := 12;
                'D', 'd':
                    x := 13;
                'E', 'e':
                    x := 14;
                'F', 'f':
                    x := 15;
            end;
            case s[i + 2] of
                '0' .. '9':
                    y := Ord(s[i + 2]) - 48;
                'A', 'a':
                    y := 10;
                'B', 'b':
                    y := 11;
                'C', 'c':
                    y := 12;
                'D', 'd':
                    y := 13;
                'E', 'e':
                    y := 14;
                'F', 'f':
                    y := 15;
            end;
            s[i] := chr(x * 16 + y);
            delete(s, i + 1, 2);
        end;
        Inc(i);
    end;
end;


procedure UnWebify2(var s: string);
var
    i, x, y: integer;
begin
    i := 1;
    while (i + 2) <= Length(s) do
    begin
        if s[i] = '_' then
        begin
            x := 0;
            y := 0;
            case UpCase(s[i + 1]) of
                '0' .. '9':
                    x := Ord(s[i + 1]) - 48;
                'A':
                    x := 10;
                'B':
                    x := 11;
                'C':
                    x := 12;
                'D':
                    x := 13;
                'E':
                    x := 14;
                'F':
                    x := 15;
            end;
            case UpCase(s[i + 2]) of
                '0' .. '9':
                    y := Ord(s[i + 2]) - 48;
                'A':
                    y := 10;
                'B':
                    y := 11;
                'C':
                    y := 12;
                'D':
                    y := 13;
                'E':
                    y := 14;
                'F':
                    y := 15;
            end;
            s[i] := chr(x * 16 + y);
            delete(s, i + 1, 2);
        end;
        Inc(i);
    end;
end;


function GetItem(const itemName: string): string;
var
    s: string;
    i: integer;
    qs: string;
begin
    qs := GetEnv('QUERY_STRING');

    while qs <> '' do
    begin
        if Pos('&', qs) > 0 then
        begin
            s := copy(qs, 1, Pos('&', qs) - 1);
            delete(qs, 1, Length(s) + 1);
        end
        else
        begin
            s := qs;
            qs := '';
        end;

        UnWebify(s);
        if Pos('=', s) <> 0 then
        begin
            if LowerCase(itemName) = LowerCase(copy(s, 1, Pos('=', s) - 1)) then
            begin
                delete(s, 1, Pos('=', s));

                if LowerCase(itemName) = 'begriff' then
                begin
                    for i := 1 to Length(s) do
                        case s[i] of
                            '<', '>':
                                s[i] := ' ';
                        end;
                end;

                Result := s;
                exit;
            end;
        end;

    end;
    Result := '';
end;


function GetItem2(const itemName: string): string;
var
    s: string;
    i: integer;
    qs: string;
begin
    qs := GetEnv('QUERY_STRING');

    while qs <> '' do
    begin
        if Pos('&', qs) > 0 then
        begin
            s := copy(qs, 1, Pos('&', qs) - 1);
            delete(qs, 1, Length(s) + 1);
        end
        else
        begin
            s := qs;
            qs := '';
        end;

        { UnWebify(s); }
        if Pos('=', s) <> 0 then
        begin
            if LowerCase(itemName) = LowerCase(copy(s, 1, Pos('=', s) - 1)) then
            begin
                delete(s, 1, Pos('=', s));

                if LowerCase(itemName) = 'begriff' then
                begin
                    for i := 1 to Length(s) do
                        case s[i] of
                            '<', '>':
                                s[i] := ' ';
                        end;
                end;

                Result := s;
                exit;
            end;
        end;

    end;
    Result := '';
end;


function MakeEntities(s: string): string;
var
    s2: string;
    i: integer;
begin
    s2 := '';
    for i := 1 to Length(s) do
    begin
        case s[i] of
            '0' .. '9', 'a' .. 'z', 'A' .. 'Z', ' ', ':', '-', ',', '.', '(',
                ')', '!', '?', ';', '=', '/', #13, #10:
                s2 := s2 + s[i];
            'ä':
                s2 := s2 + '&auml;';
            'ö':
                s2 := s2 + '&ouml;';
            'ü':
                s2 := s2 + '&uuml;';
            'Ä':
                s2 := s2 + '&Auml;';
            'Ö':
                s2 := s2 + '&Ouml;';
            'Ü':
                s2 := s2 + '&Uuml;';
            'ß':
                s2 := s2 + '&szlig;';
            '&':
                s2 := s2 + '&amp;';
            '<':
                s2 := s2 + '&lt;';
            '>':
                s2 := s2 + '&gt;';
            '"':
                s2 := s2 + '&quot;';
        else
            begin
                s2 := s2 + '&#' + IntToStr(Ord(s[i])) + ';';
            end;
        end;
    end;
    Result := s2;
end;


function MakeEntitiesNoAmp(s: string): string;
var
    s2: string;
    i: integer;
begin
    s2 := '';
    for i := 1 to Length(s) do
    begin
        case s[i] of
            '0' .. '9', 'a' .. 'z', 'A' .. 'Z', ' ', ':', '-', ',', '.', '(',
                ')', '!', '?', ';':
                s2 := s2 + s[i];
            'ä':
                s2 := s2 + '&auml;';
            'ö':
                s2 := s2 + '&ouml;';
            'ü':
                s2 := s2 + '&uuml;';
            'Ä':
                s2 := s2 + '&Auml;';
            'Ö':
                s2 := s2 + '&Ouml;';
            'Ü':
                s2 := s2 + '&Uuml;';
            'ß':
                s2 := s2 + '&szlig;';
            '&':
                s2 := s2 + '&amp;';
            '<':
                s2 := s2 + '&lt;';
            '>':
                s2 := s2 + '&gt;';
            '"':
                s2 := s2 + '&quot;';
        else
            begin
                s2:=s2+s[i];
            end;
        end;
    end;
    while Pos('&amp;amp;', s2) > 0 do
        delete(s2, Pos('&amp;amp;', s2) + 1, 4);
    while Pos('&amp;apos;', s2) > 0 do
        delete(s2, Pos('&amp;apos;', s2) + 1, 4);
    Result := s2;
end;


function Webify(s: string): string;
var
    s2: string;
    i: integer;
begin
    s2 := '';
    for i := 1 to Length(s) do
    begin
        case s[i] of
            'ä':
                s2 := s2 + '&auml;';
            'ö':
                s2 := s2 + '&ouml;';
            'ü':
                s2 := s2 + '&uuml;';
            'Ä':
                s2 := s2 + '&Auml;';
            'Ö':
                s2 := s2 + '&Ouml;';
            'Ü':
                s2 := s2 + '&Uuml;';
            'ß':
                s2 := s2 + '&szlig;';
            '&':
                s2 := s2 + '&amp;';
            '<':
                s2 := s2 + '&lt;';
            '>':
                s2 := s2 + '&gt;';
            '"':
                s2 := s2 + '&quot;';
            '£':
                s2 := s2 + '&pound;';
        else
            s2 := s2 + s[i];
        end;
    end;
    Result := s2;
end;


function EncodeUrl(s: string): string;
const
    HexChar = '0123456789ABCDEF';
var
    s2: string;
    i: integer;
begin
    s2 := '';
    for i := 1 to Length(s) do
    begin
        case s[i] of
            '0' .. '9', 'a' .. 'z', 'A' .. 'Z', '.':
                s2 := s2 + s[i];
            ' ':
                s2 := s2 + '+';
        else
            s2 := s2 + '%' + HexChar[Ord(s[i]) div 16 + 1] + HexChar
                [Ord(s[i]) and 15 + 1];
        end;
    end;
    Result := s2;
end;


function EncodeUrl2(s: string): string;
const
    HexChar = '0123456789ABCDEF';
var
    s2: string;
    i: integer;
begin
    s2 := '';
    for i := 1 to Length(s) do
    begin
        case s[i] of
            '0' .. '9', 'a' .. 'z', 'A' .. 'Z', '.', '-':
                s2 := s2 + s[i];
        else
            s2 := s2 + '_' + HexChar[Ord(s[i]) div 16 + 1] + HexChar
                [Ord(s[i]) and 15 + 1];
        end;
    end;
    Result := s2;
end;


end.
