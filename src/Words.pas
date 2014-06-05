unit Words;

interface

function IsFillWord(const s: shortstring): boolean;
function IsGerman(const s: shortstring): boolean;

implementation


function IsFillWord(const s: shortstring): boolean;
begin
    Result := true;
    if Length(s) < 2 then
        exit;
    case s[1] of
        'a':
            if (s = 'and') or (s = 'auf') or (s = 'an') or (s = 'als') or (s = 'auch') or
              (s = 'am') or (s = 'alle') or (s = 'aus') then
                exit;
        'b':
            if (s = 'bitte') or (s = 'bei') or (s = 'bis') then
                exit;
        'd':
            if (s = 'du') or (s = 'der') or (s = 'die') or (s = 'das') or (s = 'den') or
              (s = 'dem') or (s = 'de') or (s = 'diese') or (s = 'dr') or (s = 'durch') or (s = 'des') then
                exit;
        'e':
            if (s = 'er') or (s = 'es') or (s = 'ein') or (s = 'eine') or (s = 'eines') or
              (s = 'einer') or (s = 'einen') then
                exit;
        'f':
            if s = 'fuer' then
                exit;
        'h':
            if s = 'he' then
                exit;
        'i':
            if (s = 'ich') or (s = 'ihr') or (s = 'it') or (s = 'im') or (s = 'ist') or
              (s = 'in') then
                exit;
        'm':
            if s = 'mit' then
                exit;
        'n':
            if (s = 'not') or (s = 'nicht') or (s = 'nach') or (s = 'nbsp') then
                exit;
        'o':
            if (s = 'oder') or (s = 'or') then
                exit;
        's':
            if (s = 'sie') or (s = 'she') or (s = 'sind') or (s = 'sich') then
                exit;
        't':
            if (s = 'the') or (s = 'to') then
                exit;
        'u':
            if (s = 'und') or (s = 'ueber') or (s = 'uns') then
                exit;
        'v':
            if s = 'von' then
                exit;
        'w':
            if (s = 'wir') or (s = 'we') or (s = 'werden') then
                exit;
        'y':
            if s = 'you' then
                exit;
        'z':
            if (s = 'zu') or (s = 'zur') or (s = 'zum') then
                exit;
    end;

    Result := false;
end;


function IsGerman(const s: shortstring): boolean;
begin
    if s = '' then
    begin
        Result := false;
        exit;
    end;

    Result := true;
    case s[1] of
        'a':
            if (s = 'auf') or (s = 'auch') then
                exit;
        'b':
            if (s = 'bitte') or (s = 'beim') or (s = 'bzw') then
                exit;
        'd':
            if (s = 'dessen') or (s = 'dass') or (s = 'durch') or
              (s = 'diese') or (s = 'dieser') or (s = 'dokumente') or
              (s = 'damit') or (s = 'daran') or (s = 'darauf') or
              (s = 'denen') or (s = 'denn') or (s = 'diesem') or (s = 'deutschen') then
                exit;
        'e':
            if (s = 'eine') or (s = 'einer') or (s = 'eines') or (s = 'einen') or
              (s = 'einem') or (s = 'erhalten') then
                exit;
        'h':
            if (s = 'haben') or (s = 'heute') then
                exit;
        'i':
            if (s = 'ist') or (s = 'ihre') or (s = 'ihren') or
              (s = 'ihnen') then
                exit;
        'k':
            if (s = 'koennen') or (s = 'kann') then
                exit;
        'm':
            if s = 'mehr' then
                exit;
        'n':
            if (s = 'nicht') or (s = 'nur') or (s = 'noch') or (s = 'nachrichten') or
              (s = 'nach') then
                exit;
        'o':
            if (s = 'oder') or (s = 'offizielle') then
                exit;
        's':
            if (s = 'sich') or (s = 'sind') or (s = 'sowie') then
                exit;
        'u':
            if (s = 'ueber') or (s = 'unter') or (s = 'uns') or (s = 'und') or
              (s = 'unsere') then
                exit;
        'w':
            if (s = 'wir') or (s = 'wie') or (s = 'wird') or
              (s = 'weitere') or (s = 'wenn') or (s = 'weiter') or (s = 'wurde') then
                exit;
        'z':
            if (s = 'zu') or (s = 'zur') or (s = 'zum') or (s = 'zurueck') then
                exit;
    end;

    Result := false;
end;



end.
