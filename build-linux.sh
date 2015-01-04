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
compiler="fpc -Mdelphi -Tlinux -O3 -vew -vq"
compiler=$compiler" -Fu/data_b/source/indy10/Lib/*"
compiler=$compiler" -Fu/usr/share/fpcsrc/2.6.2/packages/fcl-base/src"
compiler=$compiler" -Fu/usr/share/fpcsrc/2.6.2/packages/fcl-net/src"
compiler=$compiler" -Fu/usr/share/fpcsrc/2.6.2/packages/iconvenc/src"
compiler=$compiler" -FE."
echo $compiler

function compile {
    $compiler $1.dpr
    if [ -e "$1" ]
    then
        mv $1 ../bin
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
mv query ../bin
#compile Robot
#compile SearchServer

cd ..

echo "++++++++++++++++++++++++++"
echo "+                        +"
echo "+ Build was successful ! +"
echo "+                        +"
echo "++++++++++++++++++++++++++"
