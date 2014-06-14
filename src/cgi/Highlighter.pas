unit Highlighter;

interface

procedure SetHighlightSearch(inputString: string);
function HighlightSearchWords(s: string): string;

implementation

uses
    Classes,
    SysUtils,
    CgiTools;

const
    WordChars = 'abcdefghijklmnopqrstuvwxyzäöü0123456789';

var
    Keys: tStringList;


function CleanJunkFromResult(s: string): string;
var
    s2: string;
    i: integer;
begin
    s2 := '';
    for i := 1 to Length(s) do
    begin
        if (ord(s[i]) < 128) and (s[i] >= ' ') and (s[i] <> '|') then
            s2 := s2 + s[i]
        else
        begin
            case s[i] of
                'ä', 'ö', 'ü', 'Ä', 'Ö', 'Ü', 'ß':
                    s2 := s2 + s[i];
            end;
        end;
    end;

    repeat
        i := Pos(', , ', s2);
        if i > 0 then
            Delete(s2, i, 2);
    until i = 0;

    repeat
        i := Pos('??', s2);
        if i > 0 then
            Delete(s2, i, 2);
    until i = 0;

    Result := s2;
end;


procedure ReplaceAll(source, dest: string; var s: string);
var
    i: integer;
begin
    while Pos(source, s) > 0 do
    begin
        i := Pos(source, s);
        s := copy(s, 1, i - 1) + dest + copy(s, i + Length(source), Length(s));
    end;
end;


function LowerString(inputString: string): string;
var
    s: string;
begin
    s := LowerCase(inputString);
    while Pos('Ä', s) > 0 do
    begin
        s[Pos('Ä', s)] := 'ä';
    end;
    while Pos('Ö', s) > 0 do
    begin
        s[Pos('Ö', s)] := 'ö';
    end;
    while Pos('Ü', s) > 0 do
    begin
        s[Pos('Ü', s)] := 'ü';
    end;
    ReplaceAll('ß', 'ss', s);
    ReplaceAll('ä', 'ae', s);
    ReplaceAll('ö', 'oe', s);
    ReplaceAll('ü', 'ue', s);
    Result := s;
end;


procedure SetHighlightSearch(inputString: string);
var
    s: string;
    s2: string;
begin
    Keys.Clear;
    s := LowerString(inputString);
    while Pos('+', s) > 0 do
    begin
        s[Pos('+', s)] := ' ';
    end;
    while Pos('  ', s) > 0 do
    begin
        Delete(s, Pos('  ', s), 1);
    end;
    s := Trim(s);

    while s <> '' do
    begin
        if Pos(s[1], WordChars) > 0 then
        begin
            s2 := s[1];
            Delete(s, 1, 1);
            while (s <> '') and (Pos(s[1], WordChars) > 0) do
            begin
                s2 := s2 + s[1];
                Delete(s, 1, 1);
            end;
            Keys.Add(s2);
        end
        else
        begin
            Delete(s, 1, 1);
        end;
    end;
end;



function HighlightSearchWords(s: string): string;
var
    s2: string;
    s3: string;
begin
    s2 := '';

    while s <> '' do
    begin
        if Pos(LowerString(copy(s, 1, 1)), WordChars) > 0 then
        begin
            s3 := s[1];
            Delete(s, 1, 1);
            while (s <> '') and (Pos(LowerString(copy(s, 1, 1)), WordChars) > 0) do
            begin
                s3 := s3 + s[1];
                Delete(s, 1, 1);
            end;

            if Keys.IndexOf(s3) = -1 then
            begin
                while s3 <> '' do
                begin
                    s2 := s2 + s3[1];
                    Delete(s3, 1, 1);
                end;
            end
            else
            begin
                s2 := s2 + '<strong>' + s3 + '</strong>';
            end;
        end
        else
        begin
            s2 := s2 + s[1];
            Delete(s, 1, 1);
        end;
    end;

    ReplaceAll(#195#156, '&Uuml;', s2);
    ReplaceAll(#195#150, '&Ouml;', s2);
    ReplaceAll(#195#132, '&Auml;', s2);


    ReplaceAll(#195#164, '&auml;', s2);
    ReplaceAll(#195#182, '&ouml;', s2);
    ReplaceAll(#195#188, '&uuml;', s2);
    ReplaceAll(#195#63, '&szlig;', s2);

    ReplaceAll(' & ',' &amp; ',s2);


    Result := CleanJunkFromResult(s2);
end;

begin
    Keys := tStringList.Create;
    Keys.Sorted := true;
    Keys.Duplicates := dupIgnore;

end.
