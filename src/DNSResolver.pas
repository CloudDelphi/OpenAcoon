unit DNSResolver;

interface

uses
    GlobalTypes;

type
    tIP4 = record
        case boolean of
            false: (b: array [1 .. 4] of byte);
            true: (IP: uint32);
    end;

function GetIP4ByHostName(const AHostName: AnsiString): tIP4;
function Ip2Str(const IP: tIP4): string;


implementation

uses
    WinSock,
    Windows,
    Logging,
    Hash,
    Classes,
    SyncObjs,
    SysUtils;

const
    MaxIP4s = 10;
    DNSCache = 'dnscache.dat'; // TODO: Make configurable in openacoon.config[.default] !!!!
    MaxHash = 10024 * 1024 - 1;

type
    pDNSData = ^tDNSData;
    tDNSData = record
        Next: pDNSData;
        IP: tIP4;
        HostName: shortstring;
    end;

var
    CritSec: tCriticalSection;
    HashTable: array [0 .. MaxHash] of pDNSData;


procedure LoadCacheData;
var
    f: TextFile;
    s: string;
    HostName: AnsiString;
    IP: tIP4;
    Data: pDNSData;
    i: integer;
    i64: int64;
    HashCode: integer;
begin
    FillChar(HashTable, SizeOf(HashTable), 0);
    if FileExists(DNSCache) then
    begin
        AssignFile(f, DNSCache);
        Reset(f);
        while not eof(f) do
        begin
            ReadLn(f, s);
            i := Pos('=', s);
            if i > 0 then
            begin
                HostName := AnsiString(LowerCase(copy(s, 1, i - 1)));
                if Length(HostName) <= 255 then
                begin
                    Delete(s, 1, i);
                    if TryStrToInt64(s, i64) then
                    begin
                        IP.IP := i64;
                        HashCode := CalcCRC(HostName) and MaxHash;
                        GetMem(Data, SizeOf(tDNSData) - 255 + Length(HostName));
                        Data^.IP := IP;
                        Data^.HostName := HostName;
                        Data^.Next := HashTable[HashCode];
                        HashTable[HashCode] := Data;
                    end;
                end;
            end;

        end;
        CloseFile(f);
    end;
end;



procedure AddToCache(const AHostName: AnsiString; const IP: tIP4);
var
    f: TextFile;
begin
    if Pos('=', AHostName) > 0 then exit; // should never happen, but better safe than sorry

    CritSec.Acquire;
    try
        AssignFile(f, DNSCache);
        if FileExists(DNSCache) then
            Append(f)
        else
            ReWrite(f);
        WriteLn(f, AHostName, '=', IP.IP);
        CloseFile(f);
    finally
        CritSec.Release;
    end;
end;


function GetIP4ByHostName(const AHostName: AnsiString): tIP4;
// Note: AHostName is assumed to always be lower-case!
var
    HostName: array [0 .. 255] of AnsiChar;
    HostEnt: pHostEnt;
    IP: array [0 .. MaxIP4s - 1] of tIP4;
    IpCount: integer;
    p: PAnsiChar;
    p2: pointer;
    Ti: cardinal;
    s: string;
    i64: int64;
    HashCode: integer;
    Data: pDNSData;
begin
    Result.IP := 0;
    if Length(AHostName) > 255 then exit;

    HashCode := CalcCRC(AHostName) and MaxHash;
    Data := HashTable[HashCode];
    while Data <> nil do
    begin
        if Data^.HostName = AHostName then exit(Data^.IP)
        else Data := Data^.Next;
    end;
    // DebugLogMsg('robot.log','Couldn''t find DNS cache-entry for "'+AHostName+'".');

    StrPCopy(HostName, AHostName);
    Ti := GetTickCount;
    HostEnt := GetHostByName(HostName);
    Ti := GetTickCount - Ti;
    if Ti > 20000 then LogMsg('robot.log', 'DNS lookup for "' + AHostName + '" took ' + IntToStr(Ti) + 'ms.');

    if HostEnt <> nil then
    begin
        if (HostEnt^.h_addrtype = AF_INET) and (HostEnt^.h_length = 4) then
        begin
            IpCount := 0;
            p2 := pointer(HostEnt^.h_addr_list);
            while (p2 <> nil) and (IpCount < MaxIP4s) do
            begin
                p := PAnsiChar(p2^);
                if p = nil then break;
                Move(p^, IP[IpCount], 4);
                Inc(IpCount);
                Inc(NativeInt(p2), SizeOf(p));
            end;
            Result := IP[Random(IpCount)];
        end;
    end;
    if Result.IP <> 0 then AddToCache(AHostName, Result);
end;


function Ip2Str(const IP: tIP4): string;
begin
    Result := IntToStr(IP.b[1]) + '.' + IntToStr(IP.b[2]) + '.' + IntToStr(IP.b[3]) + '.' + IntToStr(IP.b[4]);
end;


procedure Startup;
var
    ErrorCode: integer;
    WSAData: TWSAData;
begin
    ErrorCode := WSAStartup($0101, WSAData);
end;


begin
    CritSec := tCriticalSection.Create;
    LoadCacheData;
    Startup;

end.
