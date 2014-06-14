unit ParserClass;

interface

uses
    GlobalTypes,
    Classes,
    MemoryBuffer;

type
    tCharArr = array [1 .. 10000000] of AnsiChar;
    pCharArr = ^tCharArr;

    tParserClass = class(tThread)
        {$ifdef Unix}
        Finished: boolean;
        {$endif}

        constructor Create(AFileMask, Language: string; DoFollowLinks, DoBackLinks: boolean; AMaxBufLen: integer);
        procedure Execute; override;

    strict private
        FFileMask: string;
        ThisFileSize, ThisFilePos: int32;
        ThisFileBuffer: array [0 .. 16 * 1024 * 1024] of AnsiChar; // 16mb + 1 byte
        fOutput: TextFile;
        NotModifieds: TextFile;
        Redirects: TextFile;
        Ignores: TextFile;
        Backlinks: TextFile;
        FollowLinks: boolean;
        ProcessBackLinks: boolean;
        MaxBufLen: integer;
        IndexLanguage: string;
        BaseFileName: shortstring;
        BaseUrl: AnsiString;
        TxtBuf: pCharArr;
        TiSum: int64;
        TiCount: int64;
        Buffer: tMemoryBuffer;
        ThisLink: shortstring;
        Url, OrgUrl: AnsiString;
        InTag, InTitle, InBody, InLink, InHRef, InAnchor, SomewhereInAnchor: boolean;
        InScript, InStyle: boolean;
        InMeta, InName, InContent, MetaActive, InComment: boolean;
        InBase, InBaseHRef, InBaseLink: boolean;
        liWords, liLinks: tStringList;
        MetaName, MetaContent: shortstring;
        MetaDescription, MetaRobots,
        MetaLanguage, MetaContentLanguage, MetaDCLanguage: shortstring;
        c: AnsiChar;
        ThisBaseLink: shortstring;

        function NormalizeLinkUrl(NormalizeThisLink: shortstring): shortstring;
        procedure ReadLnFromFile(var s: AnsiString);
        function DoCatchit: boolean;
        procedure OpenOutputFiles;
        procedure AddFile;
        procedure ProcessSubFiles(FNam: string);
        procedure DeleteIfEmpty(FNam: string);
        procedure WriteBackLink(FromUrl, ToUrl, LinkText: shortstring);
        function GetCharacter: shortstring;
        procedure HandleInTag;
        procedure AddNewLink(AddThisLink: shortstring);
    end;



implementation

uses
    SysUtils,
    Types,
    {$ifndef Unix}
    IOUtils,
    {$else}
    OSWrapper,
    {$endif}
    FileLocation,
    Words,
    Logging,
    CacheFile,
    DbTypes;


procedure MakeFlatUrl(var s: shortstring);
var
    Steps: array [1 .. 50] of shortstring;
    Depth: integer;
    ThisStep: shortstring;
    Po, i: integer;
    EndsWithSlash: boolean;
begin
    EndsWithSlash := copy(s, Length(s), 1) = '/';
    Depth := 0;
    while s <> '' do
    begin
        Po := Pos('/', s);
        if Po = 0 then
            Po := Length(s) + 1;
        ThisStep := copy(s, 1, Po - 1);
        if Po >= Length(s) then s := ''
        else Delete(s, 1, Po);

        if ThisStep = '..' then
        begin
            Dec(Depth);
            if Depth < 1 then
                Depth := 1;
        end
        else if ThisStep <> '.' then
        begin
            Inc(Depth);
            if Depth > 50 then
            begin
                s := '';
                exit;
            end;
            Steps[Depth] := ThisStep;
        end;
    end;

    if Depth >= 1 then s := Steps[1]
    else s := '';
    for i := 2 to Depth do
    begin
        if (Length(s) + Length(Steps[i])) < 254 then
        begin
            s[Length(s) + 1] := '/';
            Move(Steps[i][1], s[Length(s) + 2], Length(Steps[i]));
            Inc(s[0], Length(Steps[i]) + 1);
        end;
    end;
    if EndsWithSlash then s := s + '/';
end;


{ tParserClass }

constructor tParserClass.Create(AFileMask, Language: string; DoFollowLinks, DoBackLinks: boolean; AMaxBufLen: integer);
begin
    inherited Create(true);
    {$ifdef Unix}
    Finished:=false;
    {$endif}

    FFileMask := AFileMask;
    FollowLinks := DoFollowLinks;
    ProcessBackLinks := DoBackLinks;
    MaxBufLen := AMaxBufLen;
    IndexLanguage := Language;
    GetMem(TxtBuf, MaxBufLen);
    liWords := tStringList.Create;
    liWords.Sorted := true;
    liWords.Duplicates := dupIgnore;
    liLinks := tStringList.Create;
    liLinks.Sorted := true;
    liLinks.Duplicates := dupIgnore;
    Buffer := tMemoryBuffer.Create(MaxBufLen);
    TiSum := 0;
    TiCount := 0;
end;


function tParserClass.NormalizeLinkUrl(NormalizeThisLink: shortstring): shortstring;
var
    Po: integer;
    s: shortstring;
begin
    Result := '';
    Po := Pos('/', BaseUrl);
    NormalizeThisLink := Trim(NormalizeThisLink);
    if (Length(NormalizeThisLink) >= 7) and (NormalizeThisLink[5] = ':') and (LowerCase(copy(NormalizeThisLink, 1, 7)) = 'http://') then
    begin
        while LowerCase(copy(NormalizeThisLink, 1, 7)) = 'http://' do
            Delete(NormalizeThisLink, 1, 7);
    end
    else
    begin
        if (NormalizeThisLink <> '') and (NormalizeThisLink[1] = '/') then
        begin
            if (Po - 1 + Length(NormalizeThisLink)) <= 255 then
                Insert(copy(BaseUrl, 1, Po - 1), NormalizeThisLink, 1)
            else exit;
        end
        else
        begin
            s := BaseUrl;
            while (Length(s) <> 0) and (s[Length(s)] <> '/') do
                Dec(s[0]);
            if (Length(s) + Length(NormalizeThisLink)) <= 255 then
                Insert(s, NormalizeThisLink, 1)
            else exit;
        end;
    end;

    MakeFlatUrl(NormalizeThisLink);

    if NormalizeThisLink <> '' then
    begin
        if Pos('/', NormalizeThisLink) = 0 then
            NormalizeThisLink := NormalizeThisLink + '/';
    end;

    Result := NormalizeThisLink;
end;


procedure tParserClass.ReadLnFromFile(var s: AnsiString);
var
    c: AnsiChar;
    LineBuffer: array [1 .. 16384] of AnsiChar;
    BufLen: integer;
begin
    BufLen := 0;
    while (ThisFilePos < ThisFileSize) do
    begin
        c := ThisFileBuffer[ThisFilePos];
        Inc(ThisFilePos);
        case c of
            #10: break;
            #13:;
        else
            begin
                if BufLen < SizeOf(LineBuffer) then
                begin
                    Inc(BufLen);
                    LineBuffer[BufLen] := c;
                end;
            end;
        end;
    end;

    SetLength(s, BufLen);
    if BufLen > 0 then Move(LineBuffer, s[1], BufLen);
end;


procedure tParserClass.WriteBackLink(FromUrl, ToUrl, LinkText: shortstring);
var
    i: integer;
begin
    if ProcessBackLinks then
    begin
        WriteLn(Backlinks, FromUrl);
        WriteLn(Backlinks, ToUrl);
        for i := 1 to Length(LinkText) do
            if LinkText[i] < ' ' then LinkText[i] := ' ';
        WriteLn(Backlinks, LinkText);
    end;
end;


procedure tParserClass.DeleteIfEmpty(FNam: string);
var
    f: file;
    Size: integer;
begin
    try
        AssignFile(f, FNam);
        Reset(f, 1);
        Size := FileSize(f);
        CloseFile(f);
    except
    end;

    if Size = 0 then SysUtils.DeleteFile(FNam);
end;


function tParserClass.DoCatchit: boolean;
var
    i: integer;
    s, s2, Url, OrgUrl: AnsiString;
    ResCode: integer;
begin
    Result := false;
    try
        ReadLnFromFile(Url);
        OrgUrl := Url;
        ReadLnFromFile(s);
        try
            i := Pos(#255, Url);
            if i > 0 then
                Url := copy(Url, 1, i - 1);

            ResCode := 0;
            if LowerCase(copy(s, 1, 5)) = 'http/' then
            begin
                try
                    ResCode := StrToIntDef(copy(s, 10, 3), 0);
                except
                end;
            end;

            // Check for Timeouts
            if s = 'File transfer timed-out.' then
            begin
                // DeleteFile(FNam);
                exit;
            end
            else
            begin
                case ResCode of
                    304:
                        begin // Not-Modified
                            WriteLn(NotModifieds, Url);
                            exit;
                        end;
                    400, 401, 403, 404, 406, 500:
                        begin
                            WriteLn(Ignores, OrgUrl);
                            exit;
                        end;
                else
                    begin
                        // Check for Ignores
                        if (s = '#ignore') then
                        begin
                            WriteLn(Ignores, OrgUrl);
                            exit;
                        end
                        else
                        begin
                            // Check for Relocates
                            ThisFilePos := 0;
                            ReadLnFromFile(s);
                            ReadLnFromFile(s);

                            while ThisFilePos < ThisFileSize do
                            begin
                                ReadLnFromFile(s);
                                if s = '' then
                                    break;
                                if LowerCase(copy(s, 1, 10)) = 'location: ' then
                                begin
                                    Delete(s, 1, 10);
                                    s := Trim(s);
                                    if copy(s, Length(s) - 1, 2) = '//' then
                                        Delete(s, Length(s), 1);
                                    if LowerCase(copy(s, 1, 7)) = 'http://' then
                                    begin
                                        Delete(s, 1, 7);
                                        if copy(s, 1, 1) = '/' then
                                            s := copy(Url, 1, Pos('/', Url) - 1) + s;
                                        WriteLn(Redirects, OrgUrl);
                                        WriteLn(Redirects, s);
                                        exit;
                                    end
                                    else if LowerCase(copy(s, 1, 7)) = 'https://' then
                                    begin
                                        Delete(s, 1, 8);
                                        if copy(s, 1, 1) = '/' then
                                            s := copy(Url, 1, Pos('/', Url) - 1) + s;
                                        WriteLn(Redirects, OrgUrl);
                                        WriteLn(Redirects, s);
                                        exit;
                                    end
                                    else
                                    begin
                                        s2 := OrgUrl;
                                        if copy(s, 1, 1) = '/' then i := Pos('/', s2) - 1
                                        else
                                        begin
                                            i := Length(s2);
                                            while (i > 0) and (s2[i] <> '/') do
                                                Dec(i);
                                        end;
                                        s2 := copy(s2, 1, i);
                                        WriteLn(Redirects, OrgUrl);
                                        WriteLn(Redirects, s2 + s);
                                        exit;
                                    end;
                                end;
                            end;
                        end;

                    end;
                end;
            end;
        except
        end;
    except
    end;

    Result := true;
end;


procedure tParserClass.OpenOutputFiles;
var
    i: integer;
    FNam: shortstring;
begin
    Randomize;

    repeat
        FNam := '';
        for i := 1 to 8 do
            FNam := FNam + IntToStr(Random(10));
    until not FileExists(cParsedPath + FNam + '.nmo');
    BaseFileName := FNam;

    AssignFile(NotModifieds, cParsedPath + FNam + '.nmo');
    ReWrite(NotModifieds);

    AssignFile(Redirects, cParsedPath + FNam + '.rel');
    ReWrite(Redirects);

    AssignFile(Ignores, cParsedPath + FNam + '.ign');
    ReWrite(Ignores);

    AssignFile(fOutput, cParsedPath + FNam + '.www');
    ReWrite(fOutput);

    AssignFile(Backlinks, cParsedPath + FNam + '.bck');
    ReWrite(Backlinks);
end;



function tParserClass.GetCharacter: shortstring;
var
    s: shortstring;
    c: AnsiChar;
    c2: AnsiChar;
begin
    c := Buffer.GetCurrentCharacter;
    if c = '&' then
    begin
        s := Buffer.PeekAhead(5);
        c2 := s[1];
        s[1] := 'x';
        if s = 'xuml;' then
        begin
            case c2 of
                'a': c := 'ä';
                'o': c := 'ö';
                'u': c := 'ü';
                'A': c := 'Ä';
                'O': c := 'Ö';
                'U': c := 'Ü';
            end;
            Buffer.Advance(5);
        end
        else
        begin
            s := Buffer.PeekAhead(6);
            if s = 'szlig;' then
            begin
                c := 'ß';
                Buffer.Advance(6);
            end
            else
            begin
                s := Buffer.PeekAhead(4);
                if s = 'amp;' then
                begin
                    c := '&';
                    Buffer.Advance(4);
                end
                else if s = 'reg;' then
                begin
                    c := '®';
                    Buffer.Advance(4);
                end
                else
                begin
                    s := Buffer.PeekAhead(5);
                    if s = 'copy;' then
                    begin
                        c := '©';
                        Buffer.Advance(5);
                    end
                    else if s = 'nbsp;' then
                    begin
                        c := ' ';
                        Buffer.Advance(5);
                    end;
                end;
            end;
        end;
    end; // if c='&'

    Result := c;
    case c of
        'ä': Result := 'ae';
        'ö': Result := 'oe';
        'ü': Result := 'ue';
        'Ä': Result := 'Ae';
        'Ö': Result := 'Oe';
        'Ü': Result := 'Ue';
        'ß': Result := 'ss';
    end;
end;


procedure tParserClass.HandleInTag;
var
    StrPosi: integer;
begin
    if InLink and (((c = '"') and (Length(ThisLink) > 1)) or
    (c = ' ') or (c = '>')) then
    begin
        InLink := false;
        InHRef := false;
        StrPosi := Pos('#', ThisLink);
        if StrPosi > 0 then
            Delete(ThisLink, StrPosi, 255);
        ThisLink := Trim(ThisLink);
        if copy(ThisLink, 1, 1) = '"' then
            Delete(ThisLink, 1, 1);
        if copy(ThisLink, Length(ThisLink), 1) = '"' then
            Delete(ThisLink, Length(ThisLink), 1);
        if Length(ThisLink) <> 0 then
        begin
            AddNewLink(ThisLink);
        end;
    end;

    if InLink and (Length(ThisLink) < 255) then
    begin
        Inc(ThisLink[0]);
        ThisLink[Length(ThisLink)] := c;
    end;

    if InBaseLink and (((c = '"') and (Length(ThisBaseLink) > 1)) or
    (c = ' ') or (c = '>')) then
    begin
        InBaseLink := false;
        InBaseHRef := false;
        if Pos('#', ThisBaseLink) > 0 then
            Delete(ThisBaseLink, Pos('#', ThisBaseLink), 255);
        ThisBaseLink := Trim(ThisBaseLink);
        if copy(ThisBaseLink, 1, 1) = '"' then
            Delete(ThisBaseLink, 1, 1);
        if ThisBaseLink <> '' then
        begin
            if Pos('://', ThisBaseLink) > 0 then
            begin
                Delete(ThisBaseLink, 1, Pos('://', ThisBaseLink) + 2);
                if copy(ThisBaseLink, Length(ThisBaseLink), 1) <> '/' then
                    ThisBaseLink := ThisBaseLink + '/';
                BaseUrl := ThisBaseLink;
            end;
        end;
    end;
    if InBaseLink and (Length(ThisBaseLink) < 255) then
    begin
        Inc(ThisBaseLink[0]);
        ThisBaseLink[Length(ThisBaseLink)] := c;
    end;

    if InName and (c = '"') then
    begin
        if MetaActive then
        begin
            InName := false;
            if Pos('"', MetaName) > 0 then
                Delete(MetaName, 1, Pos('"', MetaName));
            MetaActive := false;
        end
        else
            MetaActive := true;
    end
    else if InName and (Length(MetaName) < 254) and MetaActive then
        MetaName := MetaName + GetCharacter;

    if InContent and (c = '"') then
    begin
        if MetaActive then
        begin
            InContent := false;
            if Pos('"', MetaContent) > 0 then
                Delete(MetaContent, 1, Pos('"', MetaContent));
            if LowerCase(copy(MetaContent, 1, 8)) = 'content=' then
                Delete(MetaContent, 1, 8);
            MetaActive := false;
        end
        else
            MetaActive := true;
    end
    else if InContent and (Length(MetaContent) < 254) then
        MetaContent := MetaContent + GetCharacter;

        // MakeLeftCopy(8, s);
        // s := Buffer.PeekAhead(8);
    if InMeta and (not InName) and (not InContent) and
    (Buffer.LowerPeekAhead((8)) = 'content=') then
    begin
        InContent := true;
        MetaActive := false;
    end;

        // MakeLeftCopy(11, s);
        // s := Buffer.PeekAhead(11);
    if InMeta and (not InName) and (not InContent) and
    (Buffer.LowerPeekAhead(11) = 'http-equiv=') then
    begin
        InName := true;
        MetaActive := false;
    end;

        // MakeLeftCopy(5, s);
        // s := Buffer.LowerPeekAhead(5);
    if InMeta and (not InName) and (not InContent) and
    (Buffer.LowerPeekAhead(5) = 'name=') then
    begin
        InName := true;
        MetaActive := false;
    end;

    if InAnchor and (not InHRef) and (Buffer.LowerPeekAhead(5) = 'href=') then InHRef := true;
    if InHRef and (not InLink) and (c = '=') then
    begin
        InLink := true;
        ThisLink := '';
    end;

    if InBase and (not InBaseHRef) and (Buffer.LowerPeekAhead(5) = 'href=') then InBaseHRef := true;
    if InBaseHRef and (not InBaseLink) and (c = '=') then
    begin
        InBaseLink := true;
        ThisBaseLink := '';
    end;
    // EndTimer;
end;


procedure tParserClass.AddNewLink(AddThisLink: shortstring);
begin
    if FollowLinks then
    begin
        AddThisLink := NormalizeLinkUrl(AddThisLink);
        if Length(AddThisLink) <> 0 then liLinks.Add(AddThisLink);
    end;
end;



procedure tParserClass.AddFile;
var
    Beschr: shortstring;
    Title: shortstring;
    i: integer;
    s, s2: shortstring;
    TrueLen: integer;
    ThisWord: shortstring;
    GermanPage: boolean;
    KeyAn: integer;
    InLinkText: boolean;
    LinkText: shortstring;
    Ti, Ti2: int64;



    (* procedure MakeLeftCopy(Len: integer; var s: shortstring);
      var
      i: integer;
      c: AnsiChar;
      begin
      s := '';
      for i := 1 to Len do
      begin
      if (Posi+i-1) <= BufLen then
      begin
      c := Buf[Posi+i-1];
      if (c >= 'A') and (c <= 'Z') then
      Inc(c, 32);
      Inc(s[0]);
      s[length(s)] := c;
      end;
      end;
      end; *)


    procedure AddToWordList(const s: shortstring; const Flags: shortstring);
    begin
        if (s <> '') and (not IsFillWord(s)) and (liWords.Count < cMaxKeywords) then
            liWords.Add( (* Flags + *) s);
    end;


    procedure InitParsing;
    var
        i: integer;
    begin
        ThisFilePos := 0;
        ReadLnFromFile(Url);
        if Length(Url) >= 4 then
        begin
            if Url[4] = #0 then LogMsg('parser.log', 'Probably corrupted URL in parser input: "' + Url + '"');
        end;
        OrgUrl := Url;
        BaseUrl := Url;
        i := Pos(#255, Url);
        if i > 0 then Url := copy(Url, 1, i - 1);

        if ThisFileSize > MaxBufLen then
        begin
            WriteLn(fOutput, '#ignore');
            WriteLn(fOutput, OrgUrl);
            exit;
        end;

        // liWords.Clear;
        // liLinks.Clear;


        Title := '';
        TrueLen := 0;

        // BufLen:=ThisFileSize;
        // GetMem(Buf, BufLen);
        // GetMem(TxtBuf, ThisFileSize);
        // Move(ThisFileBuffer, Buf^, BufLen);
        Buffer.CopyFrom(ThisFileBuffer, ThisFileSize);
        Buffer.ConvertLowAsciiToSpaces;

        // for i := 1 to BufLen do
        // if Buf[i] < #32 then Buf[i] := #32;


        InTag := false;
        InTitle := false;
        InBody := false;
        InAnchor := false;
        SomewhereInAnchor := false;
        InScript := false;
        InStyle := false;
        InLink := false;
        InHRef := false;
        GermanPage := false;
        InMeta := false;
        MetaDescription := '';
        MetaRobots := '';
        MetaLanguage := '';
        MetaContentLanguage := '';
        MetaDCLanguage := '';
        InName := false;
        InContent := false;
        MetaActive := false;
        InComment := false;
        InBase := false;
        InBaseHRef := false;
        InBaseLink := false;
        ThisLink := '';
        ThisBaseLink := '';
        LinkText := '';
    end;


    procedure HandleTagOpening;
    begin

// if (not SomewhereInAnchor) then
        begin
            Inc(TrueLen);
            TxtBuf[TrueLen] := ' ';
        end;
        if ProcessBackLinks then
        begin
            if SomewhereInAnchor and (Length(LinkText) < 255) and (Length(LinkText) > 0) and
            (LinkText[Length(LinkText)] <> ' ') then LinkText := LinkText + ' ';
        end;

        InTag := true;
        // MakeLeftCopy(2, s);
        s := Buffer.PeekAhead(2);
        if s = 'a ' then
        begin
            InAnchor := true;
            SomewhereInAnchor := true;
        end
        else
        begin
            // MakeLeftCopy(4, s);
            s := Buffer.PeekAhead(4);
            if s = 'body' then InBody := true
            else if s = 'base' then InBase := true;

            // MakeLeftCopy(5, s);
            s := Buffer.PeekAhead(5);
            if s = 'title' then InTitle := true
            else
            begin
                // MakeLeftCopy(6, s);
                s := Buffer.PeekAhead(6);
                if s = '/title' then
                begin
                    InTitle := false;
                    InBody := true;
                end
                else if s = '/style' then InStyle := false
                else if s = 'script' then InScript := true
                else
                begin
                    // MakeLeftCopy(5, s);
                    s := Buffer.PeekAhead(5);
                    if s = 'meta ' then
                    begin
                        InMeta := true;
                        InName := false;
                        InContent := false;
                        MetaName := '';
                        MetaContent := '';
                        MetaActive := false;
                    end
                    else if s = 'style' then InStyle := true
                    else
                    begin
                        // MakeLeftCopy(3, s);
                        s := Buffer.PeekAhead(3);
                        if s = '!--' then InComment := true;
                        if s = '/a>' then
                        begin
                            SomewhereInAnchor := false;
                            if ProcessBackLinks then
                            begin
                                LinkText := Trim(LinkText);
                                if LinkText <> '' then
                                begin // Store Link-URL and LinkText
                                // WriteLn('LinkUrl="', NormalizeLinkUrl(ThisLink), '"');
                                // WriteLn('LinkText="', LinkText, '"');
                                // WriteLn;

                                // TODO: Actually store the data
                                    WriteBackLink(OrgUrl, NormalizeLinkUrl(ThisLink), LinkText);


                                // Clear LinkText for future use.
                                    LinkText := '';
                                end;
                            end;
                        end;
                        // MakeLeftCopy(7, s);
                        s := Buffer.PeekAhead(7);
                        if s = '/script' then InScript := false;
                    end;
                end;
            end;
        end;
    end;


    procedure HandleTagClosing;
    begin
        if InMeta then
        begin
            if LowerCase(MetaName) = 'description' then
                MetaDescription := MetaContent;
            if LowerCase(MetaName) = 'robots' then
                MetaRobots := MetaRobots + ',' + MetaContent;
            if LowerCase(MetaName) = 'language' then
                MetaLanguage := MetaContent;
            if LowerCase(MetaName) = 'content-language' then
                MetaContentLanguage := MetaContent;
            if LowerCase(MetaName) = 'dc.language' then
                MetaDCLanguage := MetaContent;
        end;

        if InLink then
        begin
            if Pos('#', ThisLink) > 0 then
                Delete(ThisLink, Pos('#', ThisLink), 255);
            ThisLink := Trim(ThisLink);
            if copy(ThisLink, 1, 1) = '"' then
                Delete(ThisLink, 1, 1);
            if copy(ThisLink, Length(ThisLink), 1) = '"' then
                Delete(ThisLink, Length(ThisLink), 1);
            if ThisLink <> '' then
                AddNewLink(ThisLink);
        end;

        if InBaseLink then
        begin
            if Pos('#', ThisBaseLink) > 0 then
                Delete(ThisBaseLink, Pos('#', ThisBaseLink), 255);
            ThisBaseLink := Trim(ThisBaseLink);
            if copy(ThisBaseLink, 1, 1) = '"' then
                Delete(ThisBaseLink, 1, 1);
            if ThisBaseLink <> '' then
            begin
                if Pos('://', ThisBaseLink) > 0 then
                begin
                    Delete(ThisBaseLink, 1, Pos('://', ThisBaseLink) + 2);
                    if copy(ThisBaseLink, Length(ThisBaseLink), 1) <> '/' then
                        ThisBaseLink := ThisBaseLink + '/';
                    BaseUrl := ThisBaseLink;
                end;
            end;
        end;

        InTag := false;
        InLink := false;
        InHRef := false;
        InAnchor := false;
        InMeta := false;
        InName := false;
        InContent := false;
        InBase := false;
        InBaseHRef := false;
        InBaseLink := false;
    end;


    procedure HandleRegularCharacter;
    var
        s: shortstring;
        i: integer;
    begin
        s := GetCharacter;

        if not InComment then
        begin
            if InTitle and ((Length(Title) + Length(s)) <= 255) then
                Title := Title + s
            else
            if InBody and (TrueLen < ThisFileSize) then
            begin
                // WriteLn(s,' (',Length(s),')');
                if (Length(s) > 0) and (not SomewhereInAnchor) and (not InScript) and (not InStyle) then
                begin
                    Move(s[1], TxtBuf[TrueLen + 1], Length(s));
                    Inc(TrueLen, Length(s));
                end
                else if SomewhereInAnchor and (s <> '') and (not InScript) and (not InStyle) then
                begin
                    if ((Length(LinkText) + Length(s)) <= 255) and ProcessBackLinks then
                    begin
                        for i := 1 to Length(s) do
                        begin
                            Inc(LinkText[0]);
                            LinkText[Length(LinkText)] := s[i];
                        end;
                        // LinkText := LinkText + s;
                    end;
                end;
            end;
        end;
    end;


var
    StrPosi: integer;
    AllText: AnsiString;
    AllTextLen: integer;
begin
    InitParsing;

    if Trim(OrgUrl) = '' then
        LogMsg('parser.log', 'Empty URL. ThisFileSize=' + IntToStr(ThisFileSize) +
        ' ThisFilePos=' + IntToStr(ThisFilePos));

    Buffer.Seek(0);
    Inc(TiCount);
    while not Buffer.Eof do
    begin
        if InComment then
        begin
            s := Buffer.PeekAhead(3);
            if s = '-->' then
                InComment := false;
        end;

        c := Buffer.GetCurrentCharacter;
        case c of
            '<': HandleTagOpening;
            '>': HandleTagClosing;
        else
            begin
                if InTag then
                begin
                    if InLink and (((c = '"') and (Length(ThisLink) > 1)) or
                    (c = ' ') or (c = '>')) then
                    begin
                        InLink := false;
                        InHRef := false;
                        StrPosi := Pos('#', ThisLink);
                        if StrPosi > 0 then
                            Delete(ThisLink, StrPosi, 255);
                        ThisLink := Trim(ThisLink);
                        if copy(ThisLink, 1, 1) = '"' then
                            Delete(ThisLink, 1, 1);
                        if copy(ThisLink, Length(ThisLink), 1) = '"' then
                            Delete(ThisLink, Length(ThisLink), 1);
                        if Length(ThisLink) <> 0 then
                        begin
                            AddNewLink(ThisLink);
                        end;
                    end;

                    if InLink and (Length(ThisLink) < 255) then
                    begin
                        Inc(ThisLink[0]);
                        ThisLink[Length(ThisLink)] := c;
                    end;

                    if InBaseLink and (((c = '"') and (Length(ThisBaseLink) > 1)) or
                    (c = ' ') or (c = '>')) then
                    begin
                        InBaseLink := false;
                        InBaseHRef := false;
                        if Pos('#', ThisBaseLink) > 0 then
                            Delete(ThisBaseLink, Pos('#', ThisBaseLink), 255);
                        ThisBaseLink := Trim(ThisBaseLink);
                        if copy(ThisBaseLink, 1, 1) = '"' then
                            Delete(ThisBaseLink, 1, 1);
                        if ThisBaseLink <> '' then
                        begin
                            if Pos('://', ThisBaseLink) > 0 then
                            begin
                                Delete(ThisBaseLink, 1, Pos('://', ThisBaseLink) + 2);
                                if copy(ThisBaseLink, Length(ThisBaseLink), 1) <> '/' then
                                    ThisBaseLink := ThisBaseLink + '/';
                                BaseUrl := ThisBaseLink;
                            end;
                        end;
                    end;
                    if InBaseLink and (Length(ThisBaseLink) < 255) then
                    begin
                        Inc(ThisBaseLink[0]);
                        ThisBaseLink[Length(ThisBaseLink)] := c;
                    end;

                    if InName and (c = '"') then
                    begin
                        if MetaActive then
                        begin
                            InName := false;
                            if Pos('"', MetaName) > 0 then
                                Delete(MetaName, 1, Pos('"', MetaName));
                            MetaActive := false;
                        end
                        else
                            MetaActive := true;
                    end
                    else if InName and (Length(MetaName) < 254) and MetaActive then
                        MetaName := MetaName + GetCharacter;

                    if InContent and (c = '"') then
                    begin
                        if MetaActive then
                        begin
                            InContent := false;
                            if Pos('"', MetaContent) > 0 then
                                Delete(MetaContent, 1, Pos('"', MetaContent));
                            if LowerCase(copy(MetaContent, 1, 8)) = 'content=' then
                                Delete(MetaContent, 1, 8);
                            MetaActive := false;
                        end
                        else
                            MetaActive := true;
                    end
                    else if InContent and (Length(MetaContent) < 254) then
                        MetaContent := MetaContent + GetCharacter;

                    if InMeta and (not InName) and (not InContent) and
                    (Buffer.LowerPeekAhead((8)) = 'content=') then
                    begin
                        InContent := true;
                        MetaActive := false;
                    end;

                    if InMeta and (not InName) and (not InContent) and
                    (Buffer.LowerPeekAhead(11) = 'http-equiv=') then
                    begin
                        InName := true;
                        MetaActive := false;
                    end;

                    if InMeta and (not InName) and (not InContent) and
                    (Buffer.LowerPeekAhead(5) = 'name=') then
                    begin
                        InName := true;
                        MetaActive := false;
                    end;

                    if InAnchor and (not InHRef) and (Buffer.LowerPeekAhead(5) = 'href=') then InHRef := true;
                    if InHRef and (not InLink) and (c = '=') then
                    begin
                        InLink := true;
                        ThisLink := '';
                    end;

                    if InBase and (not InBaseHRef) and (Buffer.LowerPeekAhead(5) = 'href=') then InBaseHRef := true;
                    if InBaseHRef and (not InBaseLink) and (c = '=') then
                    begin
                        InBaseLink := true;
                        ThisBaseLink := '';
                    end;
                end
                else
                begin
                    HandleRegularCharacter;
                end;
            end;
        end;
        Buffer.Advance(1);
    end;


    s2 := LowerCase(Trim(copy(Title, 1, 255)));
    while s2 <> '' do
    begin
        s2 := Trim(s2);
        s := '';
        while (s2 <> '') and (s2[1] in ['a' .. 'z', '-', '_']) do
        begin
            Inc(s[0]);
            s[Length(s)] := s2[1];
            Delete(s2, 1, 1);
        end;
        Delete(s2, 1, 1);
        AddToWordList(s, '1');
    end;

    (*
    i := 1;
    AllTextLen:=0;
    SetLength(AllText,250000);
    while i <= TrueLen do
    begin
        c := TxtBuf[i];
        if (c >= 'A') and (c <= 'Z') then Inc(c, 32);
        Inc(i);

        if ((c >= 'a') and (c <= 'z')) or ((c >= '0') and (c <= '9')) or (c='+') or (c='#') then
        begin
            if AllTextLen<250000 then
            begin
                Inc(AllTextLen);
                AllText[AllTextLen]:=c;
            end;
        end;
    end;
 *)


    i := 1;
    while i <= TrueLen do
    begin
        c := TxtBuf[i];
        if (c >= 'A') and (c <= 'Z') then Inc(c, 32);
        Inc(i);


        if ((c >= 'a') and (c <= 'z')) or ((c >= '0') and (c <= '9')) then
        begin
            ThisWord := c;
            while i <= TrueLen do
            begin
                c := TxtBuf[i];
                if (c >= 'A') and (c <= 'Z') then Inc(c, 32);
                Inc(i);
                if ((c >= 'a') and (c <= 'z')) or ((c >= '0') and (c <= '9')) or (c = '-') or (c = '_') then
                begin
                    if Length(ThisWord) < 255 then
                    begin
                        Inc(ThisWord[0]);
                        ThisWord[Length(ThisWord)] := c;
                    end;
                end
                else break;
            end;
            if not GermanPage then
                if IsGerman(ThisWord) then GermanPage := true;
            AddToWordList(ThisWord, '0');
        end;
        // if liWords.Count >= cMaxKeyWords then break;
    end;

    i := 1;
    while i <= Length(MetaDescription) do
    begin
        c := MetaDescription[i];
        if (c >= 'A') and (c <= 'Z') then Inc(c, 32);
        Inc(i);
        if ((c >= 'a') and (c <= 'z')) or ((c >= '0') and (c <= '9')) then
        begin
            ThisWord := c;
            while i <= Length(MetaDescription) do
            begin
                c := MetaDescription[i];
                if (c >= 'A') and (c <= 'Z') then Inc(c, 32);
                Inc(i);
                if ((c >= 'a') and (c <= 'z')) or ((c >= '0') and (c <= '9')) or (c = '-') or (c = '_') then
                begin
                    if Length(ThisWord) < 255 then
                    begin
                        Inc(ThisWord[0]);
                        ThisWord[Length(ThisWord)] := c;
                    end;
                end
                else break;
            end;

            if not GermanPage then
                if IsGerman(ThisWord) then GermanPage := true;

            AddToWordList(ThisWord, '0');
            (* if (liWords.Count < cMaxKeyWords) and (liWords.IndexOf(ThisWord) = -1) and
              (not IsFillWord(ThisWord)) and (ThisWord <> '') then
                  liWords.Add(ThisWord); *)
        end;
    end;

    Beschr := '';
    i := 1;
    while (i <= TrueLen) and (Length(Beschr) < 255) do
    begin
        c := TxtBuf[i];
        Inc(i);
        if c <> #32 then Beschr := Beschr + c
        else
        begin
            if (Beschr <> '') and (Beschr[Length(Beschr)] <> #32) then
                Beschr := Beschr + c;
        end;
    end;

    // FreeMem(Buf, BufLen);
    // Buffer.Free;
    // FreeMem(TxtBuf, ThisFileSize);

    if (not GermanPage) and
    ((LowerCase(MetaLanguage) = 'de') or (Pos('deutsch', LowerCase(MetaLanguage)) > 0) or
    (LowerCase(MetaContentLanguage) = 'de') or (Pos('deutsch', LowerCase(MetaContentLanguage)) > 0) or
    (LowerCase(MetaDCLanguage) = 'de') or (Pos('deutsch', LowerCase(MetaDCLanguage)) > 0)) then
        GermanPage := true;

    if (GermanPage or (IndexLanguage <> 'de')) and
    (Pos('noindex', LowerCase(MetaRobots)) = 0) then
    begin
        WriteLn(fOutput, '#wwwdata');

        WriteLn(fOutput, OrgUrl);

        if GermanPage then WriteLn(fOutput, 'GERMAN')
        else WriteLn(fOutput, 'OTHER');


        // SetLength(AllText,AllTextLen);
        // WriteLn(fOutput,AllText);



        WriteLn(fOutput, copy(Trim(Title), 1, cMaxTitleLength));
        MetaDescription := Trim(MetaDescription);
        if MetaDescription <> '' then WriteLn(fOutput, copy(Trim(MetaDescription), 1, 255))
        else WriteLn(fOutput, copy(Trim(Beschr), 1, 255));

        WriteLn(fOutput, '0'); { Last-Modified Date !!! }

        KeyAn := liWords.Count;
        // if KeyAn>cMaxKeyWords then KeyAn:=cMaxKeyWords;
        WriteLn(fOutput, KeyAn);
        for i := 0 to KeyAn - 1 do
            WriteLn(fOutput, LowerCase(liWords.Strings[i]));

        if FollowLinks and (Pos('nofollow', LowerCase(MetaRobots)) = 0) then
        begin
            WriteLn(fOutput, liLinks.Count);
            for i := 0 to liLinks.Count - 1 do
            begin
                s := liLinks.Strings[i];
                WriteLn(fOutput, s);
            end;
        end
        else WriteLn(fOutput, '0');
    end
    else
    begin
        WriteLn(fOutput, '#ignore');
        WriteLn(fOutput, OrgUrl);
    end;

    liWords.Clear;
    liLinks.Clear;
end;


procedure tParserClass.ProcessSubFiles(FNam: string);
var
    f: tPreloadedFile;
    f2: TextFile;
begin
    FileMode := fmOpenRead or fmShareExclusive;
    try
        f := tPreloadedFile.Create;
        f.Assign(FNam);
        f.OpenRead;
        f.Preload;
        while not f.Eof do
        begin
            f.Read(ThisFileSize, SizeOf(ThisFileSize));
            // WriteLn('FileSize=',ThisFileSize);

            if ThisFileSize > SizeOf(ThisFileBuffer) then
            begin
                try
                    AssignFile(f2, 'parser.err');
                    if FileExists('parser.err') then
                        Append(f2)
                    else
                        ReWrite(f2);
                    WriteLn(f2, 'ThisFileSize>SizeOf(ThisFileBuffer)');
                    CloseFile(f2);
                    f.Seek(f.FileSize);
                except
                end;
            end
            else
            begin
                f.Read(ThisFileBuffer, ThisFileSize);
                ThisFilePos := 0;

                if DoCatchit then
                begin
                    AddFile;
                end;
            end;
        end;
        f.Close;
        f.Free;
        SysUtils.DeleteFile(FNam);
    except
        // WriteLn('Exception');
        try
            f.Close;
            f.Free;
        except
        end;
        try
            SysUtils.DeleteFile(FNam);
        except
        end;
    end;
end;


procedure tParserClass.Execute;
var
    Sr: tSearchRec;
    Code: integer;
    // Count: integer;
    // FileList: tStringDynArray;
    i, j: integer;
begin
    Priority:=tpIdle;
    // SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_IDLE);
    OpenOutputFiles;
    // Count := 0;

    {FileList := TDirectory.GetFiles(cInPath, FFileMask);
    j := High(FileList);
    for i := 0 to j do
        ProcessSubFiles(FileList[i]);}


    Code := FindFirst(cInPath + FFileMask, faAnyFile, Sr);
    while Code = 0 do
    begin
        if (Sr.Attr and faDirectory) = 0 then
        begin
            ProcessSubFiles(cInPath + Sr.Name);
        end;

        Code := FindNext(Sr);
    end;
    SysUtils.FindClose(Sr);



    CloseFile(NotModifieds);
    CloseFile(Redirects);
    CloseFile(Ignores);
    CloseFile(fOutput);
    CloseFile(Backlinks);

    DeleteIfEmpty(cParsedPath + BaseFileName + '.ign');
    DeleteIfEmpty(cParsedPath + BaseFileName + '.nmo');
    DeleteIfEmpty(cParsedPath + BaseFileName + '.rel');
    DeleteIfEmpty(cParsedPath + BaseFileName + '.www');
    DeleteIfEmpty(cParsedPath + BaseFileName + '.bck');

    FreeMem(TxtBuf);

    {$ifdef Unix}
    Finished:=true;
    {$endif}
end;



end.
