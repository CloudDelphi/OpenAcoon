unit KillThread;

interface

uses
    Classes;

(*
    The purpose of this class is to have a timed off-switch. The process
    will automatically be killed after a given number of seconds have
    elapsed. This can be useful as a hard timeout in CGI-scripts for example.
*)

type
    TKillThread = class(TThread)
    strict private
        FSeconds: integer;
    public
        constructor Create(seconds: integer);
    protected
        procedure Execute; override;
    end;


implementation

uses
    SysUtils;



constructor TKillThread.Create(seconds: integer);
begin
    inherited Create(false);
    FSeconds := seconds;
end;



procedure TKillThread.Execute;
var
    i: integer;
begin
    for i := 1 to FSeconds do
    begin
        Sleep(1000);
    end;
    halt;
end;

end.
