@rem The build assumes that FPC is within your search-path.
@rem If this is not the case you need to add the full path to FPC
@rem to the "Set Compiler=" line below.
@rem
@rem This is also where you need to make changes if you want to change
@rem compiler-options.


@echo off
set Compiler=fpc -Mdelphi -Twin64 -Px86_64 -O3 -Xs -XX -vew -vq -Fudep\indy10\lib\*
Set Compiler=%Compiler% -Fuc:\fpc\2.6.4\units\x86_64-win64\*


@rem Make sure the "bin" directory exists
md bin

cd src
rem echo %compiler%
rem goto Done
del *.o
del *.ppu
del *.exe

%Compiler% RobotNew.dpr
if errorlevel 1 goto Error

%Compiler% SearchServerNew.dpr
if errorlevel 1 goto Error

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

%Compiler% GenDb.dpr
if errorlevel 1 goto Error

%Compiler% cgi\query.dpr
if errorlevel 1 goto Error

move /Y *.exe ..\bin
del *.dcu
move /Y cgi\*.exe ..\bin
del cgi\*.dcu

echo .
echo ++++++++++++++++++++++++++
echo +                        +
echo + Build was successful ! +
echo +                        +
echo ++++++++++++++++++++++++++

goto Done

:Error
echo +-----------------------+
echo !                       !
echo ! Error during compile! !
echo !                       !
echo +-----------------------+

:Done
cd ..
