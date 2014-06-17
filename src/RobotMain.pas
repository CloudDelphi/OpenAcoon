unit RobotMain;

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
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Spin, ExtCtrls, StdCtrls, ScktComp, SyncObjs,
    GlobalTypes, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
    TForm1 = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Timer1: TTimer;
        SpinButton1: TSpinButton;
        Label13: TLabel;
        Label14: TLabel;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        Label18: TLabel;
        Label19: TLabel;
        Button5: TButton;
    IdHTTP1: TIdHTTP;
        procedure SpinButton1DownClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure SpinButton1UpClick(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
    private
        { Private-Deklarationen }
        TimerCS: tCriticalSection;
    public
        { Public-Deklarationen }
    end;

var
    Form1: TForm1;

implementation

uses
    FileLocation,
    RobotsTxt,
    RobotGetUrl,
    HTTPClient,
    Config,
    IdGlobal,
    Logging,
    DNSResolver,
    MemoryPool;

{$R *.DFM}

type
    pShortString = ^shortstring;


// The following variables should theoretically belong to tForm1, but as
// there will ever be only a single tForm1, this doesn't really matter in
// this case.
var
    AdjConn: integer;
    Bytes: array [0 .. 4] of int64;
    ByteNr: integer;
    LastBytes: int64;
    StartTi, LastTick: int64;
    ConnCounter: integer;
    LastNewConn: array [0 .. 59] of integer;
    FailedCounter: integer;
    LastFailed: array [0 .. 59] of integer;

procedure TForm1.FormCreate(Sender: TObject);
var
    s: AnsiString;
begin
    TimerCS := tCriticalSection.Create;
    Label1.Caption := 'Robot ' + cVersion;
    Label2.Caption := cCopyright;
    MaxConnections := 100;
    CurConnections := 0;
    AdjConn := 0;
    TotalBytes := 0;
    Bytes[0] := 0;
    Bytes[1] := 0;
    Bytes[2] := 0;
    Bytes[3] := 0;
    Bytes[4] := 0;
    ByteNr := 0;
    LastBytes := 0;
    ConnCounter := 0;
    FillChar(LastNewConn, SizeOf(LastNewConn), 0);
    FillChar(LastFailed, SizeOf(LastFailed), 0);

    HTTPClientDefaultMaxSize :=
    ConfigReadIntegerDefault('index.max-page-size', 200 * 1024);
    if HTTPClientDefaultMaxSize < 10240 then
        HTTPClientDefaultMaxSize := 10240;
    if HTTPClientDefaultMaxSize > 16 * 1024 * 1024 then
        HTTPClientDefaultMaxSize := 16 * 1024 * 1024;

    s := ConfigReadString('robot.useragent');
    if s <> '' then HTTPClientDefaultUserAgent := s;

    SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);

    StartTi := Ticks;
    LastTick := StartTi;
    Timer1.Enabled := true;
end;


procedure TForm1.SpinButton1DownClick(Sender: TObject);
begin
    Dec(MaxConnections, 10);
    if MaxConnections < 0 then MaxConnections := 0;
    Label4.Caption := IntToStr(MaxConnections);
end;


procedure TForm1.SpinButton1UpClick(Sender: TObject);
begin
    Inc(MaxConnections, 10);
    if MaxConnections > 10000 then MaxConnections := 10000;
    Label4.Caption := IntToStr(MaxConnections);
end;


procedure TForm1.Timer1Timer(Sender: TObject);
var
    Ti: int64;
    s: shortstring;
    i: integer;
    TrueConn: integer;
    ConnSum, FailedSum: integer;
begin
    TimerCS.Enter;
    try
        CritSec.Acquire;
        if OutputOpen then
        begin
            Form1.Label4.Color := clYellow;
            CloseFile(OutputFile);
            OutputOpen := false;
        end
        else Label4.Color := clBtnFace;
        CritSec.Release;

        NewConns := 0;
        try
            Bytes[ByteNr] := TotalBytes - LastBytes;
            LastBytes := TotalBytes;
            Ti := Ticks - LastTick;
            if Ti <= 0 then Ti := 1000;
                // Label10.Caption := IntToStr(Round(1.0 * Bytes[ByteNr] * 1000 / Ti));
            Str(8.0 * Bytes[ByteNr] / 1000.0 / Ti: 4: 2, s);
            Label10.Caption := s;

            LastTick := Ticks;
            ByteNr := (ByteNr + 1) mod 5;

            Ti := (Ticks - StartTi) div 1000;
            if Ti > 0 then
            begin
                    // Label11.Caption := IntToStr(Round(1.0 * TotalBytes / Ti));
                Str(8.0 * TotalBytes / 1000000.0 / Ti: 4: 2, s);
                Label11.Caption := s;
                Str(UrlNr / Ti: 4: 2, s);
                Label12.Caption := s;
            end;
        except
        end;

        if FileExists(cSpeedTrap) then
        begin
            Inc(AdjConn, 10);
            if AdjConn > 10000 then AdjConn := 10000;
        end
        else AdjConn := 0;
        TrueConn := MaxConnections - AdjConn;
        if TrueConn < 0 then TrueConn := 0;
        if (TrueConn < 5) and (AdjConn > 0) then
        begin
            TrueConn := 5;
            if TrueConn > MaxConnections then TrueConn := MaxConnections;
        end;
        Label4.Caption := IntToStr(TrueConn) + '/' + IntToStr(MaxConnections);

        for i := 1 to MaxConnections do
        begin
            if (CurConnections < TrueConn) and (NewConns < 4000) then
                StartNewConnection;
        end;

        Label6.Caption := IntToStr(CurConnections);
        Label14.Caption := IntToStr(UrlNr) + '/' + IntToStr(UrlAn);

        ConnCounter := (ConnCounter + 1) mod 60;
        LastNewConn[ConnCounter] := NewConns;
        ConnSum := 0;
        for i := 0 to 59 do
            Inc(ConnSum, LastNewConn[i]);
        Str(1.0 * ConnSum / 60.0: 4: 2, s);
        Label17.Caption := IntToStr(NewConns) + '/' + s;



        FailedCounter := (FailedCounter + 1) mod 60;
        LastFailed[FailedCounter] := GetAndResetFailedConnectionCount;
        FailedSum := 0;
        for i := 0 to 59 do
            Inc(FailedSum, LastFailed[i]);
        Str(1.0 * FailedSum / 60.0: 4: 2, s);
        Label18.Caption := IntToStr(LastFailed[FailedCounter]) + '/' + s;

    except
    end;
    TimerCS.Leave;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
    MaxConnections := 0;
    Label4.Caption := IntToStr(MaxConnections);
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
    MaxConnections := 100;
    Label4.Caption := IntToStr(MaxConnections);
end;


procedure TForm1.Button3Click(Sender: TObject);
begin
    MaxConnections := 400;
    Label4.Caption := IntToStr(MaxConnections);
end;


procedure TForm1.Button4Click(Sender: TObject);
begin
    MaxConnections := 1000;
    Label4.Caption := IntToStr(MaxConnections);
end;


procedure TForm1.Button5Click(Sender: TObject);
begin
    MaxConnections := 5000;
    Label4.Caption := IntToStr(MaxConnections);
end;

end.
