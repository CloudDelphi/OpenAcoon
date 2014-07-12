unit geturl;

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
    {$ifdef Unix}
    cthreads,
    {$endif}
    Classes,
    IdGlobal,
    robotglobal,
    httpget;



type
    tGetUrl = class(tThread)
    private
        UrlInfo: tUrlInfo;
        ThisHttpGet: tHttpGet;
        IP: tIP4;
	Buffer: array of byte;
	FSize: int32;
	ErrorCode: integer;

        procedure ClearBuffer;
        procedure WriteToBuffer(const s: AnsiString); overload;
	procedure WriteToBuffer(var Data: tIdBytes); overload;
	procedure WriteBogusErrorMessageToBuffer;
	function BufferSize:int32;
	procedure CheckRobotsTxt;
	procedure CheckCrawlDelay;
	procedure CheckRobotsTxtIsValid;
	procedure FetchUrl;
    protected
        procedure Execute; override;
    public
	OnComplete: procedure(var Buffer: array of byte; BufLen: int32);

        constructor Create(Info: tUrlInfo);
    end;





// ------------------------------------------------------------------


implementation



uses
    SysUtils,
    Logging,
    RobotsTxt;





constructor tGetUrl.Create(Info: tUrlInfo);
begin
    inherited Create(false);

    UrlInfo := Info;
    ThisHttpGet := nil;
    ErrorCode := 0;

    SetLength(Buffer, HTTPClientDefaultMaxSize);
    ClearBuffer;
end;





procedure tGetUrl.ClearBuffer;
begin
    FSize := 0;
    WriteToBuffer(UrlInfo.OrgUrl + #13#10);
end;





procedure tGetUrl.WriteToBuffer(const s: AnsiString);
begin
    if (FSize + Length(s)) <= BufferSize then
    begin
	Move(s[1], Buffer[FSize], Length(s));
	Inc(FSize, Length(s));
    end
    else
    begin
	ErrorCode := 1;
    end;
end;





procedure tGetUrl.WriteToBuffer(var Data: tIdBytes);
begin
    if (FSize + Length(Data)) <= BufferSize then
    begin
	Move(Data[0], Buffer[FSize], Length(Data));
	Inc(FSize, Length(Data));
    end
    else
    begin
	ErrorCode := 1;
    end;
end;





function tGetUrl.BufferSize:int32;
begin
    Result := High(Buffer)+1;
end;





procedure tGetUrl.Execute;
begin
    CheckRobotsTxt;
    if ErrorCode = 0 then CheckCrawlDelay;
    if ErrorCode = 0 then CheckRobotsTxtIsValid;
    if ErrorCode = 0 then FetchUrl;

    if Assigned(OnComplete) then
	OnComplete(Buffer, FSize);
end;





procedure tGetUrl.CheckRobotsTxt;
begin
    if not RobotsTxtIsUrlAllowed(UrlInfo.Url) then
    begin
	WriteToBuffer('#ignore'#13#10);
	WriteToBuffer('#Because of Robots.txt'#13#10);
	ErrorCode := 2;
    end;
end;





procedure tGetUrl.CheckCrawlDelay;
var
    Retries: integer;
    CrawlDelayPassed: boolean;
begin
    Retries := 0;

    repeat
        Inc(Retries);
        CrawlDelayPassed :=
	    RobotsTxtCrawlDelayPassedSinceLastAccess(UrlInfo.Url);

        if not CrawlDelayPassed then Sleep(5000);

    until CrawlDelayPassed or (Retries > 60);

    if Retries > 60 then
    begin
	ErrorCode := 5;
	WriteBogusErrorMessageToBuffer;
    end;
end;





procedure tGetUrl.WriteBogusErrorMessageToBuffer;
begin
    // Yes, the error-message is misleading. But it causes the software
    // to simply assume a temporary error and try again later.
    WriteToBuffer('File transfer timed-out.'#13#10);
end;





procedure tGetUrl.CheckRobotsTxtIsValid;
begin
    if not RobotsTxtIsRobotsTxtValid(UrlInfo.Url) then
    begin
        DebugLogMsg(
	    'robot.log',
	    'Unable to get a valid robots.txt (or a "403" or "404") for "' +
	    UrlInfo.Url + '".');

	ErrorCode := 3;
	WriteBogusErrorMessageToBuffer;
    end;
end;





procedure tGetUrl.FetchUrl;
var
    Data: tIdBytes;
begin
    ThisHttpGet := tHttpGet.Create;
    ThisHttpGet.Host := UrlInfo.Domain;
    ThisHttpGet.IP := RobotsTxtGetIP(UrlInfo.Domain);
    ThisHttpGet.Path := UrlInfo.Path;
    ThisHttpGet.Port := UrlInfo.Port;
    ThisHttpGet.UserAgent := HTTPClientDefaultUserAgent;
    ThisHttpGet.AcceptLanguage := 'de-de,de,en-us,en';

    ThisHttpGet.Get;

    if ThisHttpGet.ErrorCode = 0 then
    begin
	ThisHttpGet.Client.Socket.InputBuffer.ExtractToBytes(
	    Data,
	    ThisHttpGet.Client.Socket.InputBuffer.Size,
	    false,
	    0);

	//WriteToBuffer(ThisHttpGet.Client.Socket.InputBuffer.AsString);
	WriteToBuffer(Data);
	if ErrorCode > 0 then
	begin
	    ClearBuffer;
	    WriteToBuffer('#ignore'#13#10);
	    WriteToBuffer('#Response too big'#13#10);
	end;
    end
    else
    begin
	ErrorCode := 4;
	WriteBogusErrorMessageToBuffer;
    end;
end;





end.
