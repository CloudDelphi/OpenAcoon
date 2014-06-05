unit Logging;

interface

procedure LogMsg(const aFilename, aMessage: string);
procedure DebugLogMsg(const aFilename, aMessage: string);


implementation

uses
    SysUtils,
    SyncObjs;

var
    CritSec: TCriticalSection;


procedure DebugLogMsg(const aFilename, aMessage: string);
begin
    // LogMsg(aFilename, aMessage);
end;



procedure LogMsg(const aFilename, aMessage: string);
var
    f: TextFile;
begin
    CritSec.Enter;
    try
        AssignFile(f, aFilename);
        if FileExists(aFilename) then Append(f)
        else ReWrite(f);
        WriteLn(f, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' ', aMessage);
        CloseFile(f);
    except
    end;
    CritSec.Leave;
end;


begin
    CritSec := TCriticalSection.Create;

end.
