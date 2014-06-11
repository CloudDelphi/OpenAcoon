@rem The build assumes that DCC64 is within your search-path.
@rem If this is not the case you need to add the full path to dcc64
@rem to the "Set Compiler=" line below.
@rem
@rem This is also where you need to make changes if you want to change
@rem compiler-options.
@rem
@rem The sources were created for Delphi XE2. Later versions may (or may not)
@rem need changes to the source-code and/or the compiler-options.

@echo off
set Compiler=dcc64 -W-IMPLICIT_STRING_CAST -W- -H- -$O+ -B -Q -ASysUtils=System.SysUtils;Windows=WinAPI.Windows;Classes=System.Classes;StrUtils=System.StrUtils;SyncObjs=System.SyncObjs;Forms=VCL.Forms;Messages=WinAPI.Messages;Graphics=VCL.Graphics;Controls=VCL.Controls;Dialogs=VCL.Dialogs;Spin=VCL.Samples.Spin;ExtCtrls=Vcl.ExtCtrls;StdCtrls=VCL.StdCtrls;ScktComp=System.Win.ScktComp;Types=System.Types;IoUtils=System.IOUtils;WinSock=WinAPI.WinSock

@rem Make sure the "bin" directory exists
md bin

cd src
del *.dcu
del *.exe

%Compiler% ImportUrls.dpr
if errorlevel 1 goto Error

%Compiler% CleanUrlsTxt.dpr
if errorlevel 1 goto Error

%Compiler% PrepareRobot.dpr
if errorlevel 1 goto Error

%Compiler% ImportData.dpr
if errorlevel 1 goto Error

%Compiler% Sleep.dpr
if errorlevel 1 goto Error

%Compiler% Parser.dpr
if errorlevel 1 goto Error

%Compiler% Robot.dpr
if errorlevel 1 goto Error

%Compiler% RobotNew.dpr
if errorlevel 1 goto Error

%Compiler% GenDb.dpr
if errorlevel 1 goto Error

%Compiler% SearchServer.dpr
if errorlevel 1 goto Error

move /Y *.exe ..\bin
del *.dcu

echo .
echo +++++++++++++++++++++++++
echo +                       +
echo + Build was successful! +
echo +                       +
echo +++++++++++++++++++++++++

goto Done

:Error
echo -------------------------
echo -                       -
echo - Error during compile! -
echo -                       -
echo -------------------------

:Done
cd ..
