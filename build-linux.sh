#!/bin/bash
echo "Starting Linux build of OpenAcoon..."
echo "------------------------------------"
rm src/*.o
rm src/*.ppu
rm src/cgi/*.o
rm src/cgi/*.ppu
mkdir bin
rm bin/*
set -e

# You will probably need to change the -Fl parameter below to point
# to where you have indy10
compiler="fpc -Mdelphi -Tlinux -O3 -vew -Sew -vq -Fu~/sources/indy10/ -Fu/usr/lib64/lazarus/lcl/units/x86_64-linux/ -Fu/usr/lib64/lazarus/components/lazutils/lib/x86_64-linux/"


function compile {
    $compiler $1.dpr
    mv $1 ../bin
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
#compile Robot
#compile SearchServer

cd ..

echo "++++++++++++++++++++++++++"
echo "+                        +"
echo "+ Build was successful ! +"
echo "+                        +"
echo "++++++++++++++++++++++++++"
