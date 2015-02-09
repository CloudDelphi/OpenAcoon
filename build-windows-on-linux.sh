#!/bin/bash
echo "Starting Cross-Compile for Windows on Linux of OpenAcoon..."
echo "------------------------------------"
rm src/*.o
rm src/*.ppu
rm src/cgi/*.o
rm src/cgi/*.ppu
mkdir bin
rm bin/*
set -e

compiler="fpc -Mdelphi -Twin64 -O3 -Xs -XX -vew -vq"
compiler=$compiler" -Fudep/indy10/Lib/*"
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# You will probably need to change the path in the following line to match your setup
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
compiler=$compiler" -Fu/usr/local/lib/fpc/2.6.4/units/x86_64-win64/*"
compiler=$compiler" -FE."


function compile {
    $compiler $1.dpr
    if [ -e "$1.exe" ]
    then
        mv $1.exe ../bin
    fi
    echo -e '\n'
}


cd src

compile RobotNew
compile searchservernew
compile ImportUrls
compile CleanUrlsTxt
compile PrepareRobot
compile ImportData
compile Sleep
compile Parser
compile GenDb
compile cgi/query
mv query.exe ../bin
#compile Robot
#compile SearchServer

cd ..

echo "++++++++++++++++++++++++++"
echo "+                        +"
echo "+ Build was successful ! +"
echo "+                        +"
echo "++++++++++++++++++++++++++"
