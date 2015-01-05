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

# You will probably need to change directory names to match your setup
fpc_src="/usr/share/fpcsrc/2.6.2/packages/"
compiler="fpc -Mdelphi -Tlinux -O3 -vew -vq"
compiler=$compiler" -Fu/data_b/source/indy10/Lib/*"
compiler=$compiler" -Fu${fpc_src}fcl-base/src"
compiler=$compiler" -Fu${fpc_src}fcl-net/src"
compiler=$compiler" -Fu${fpc_src}iconvenc/src"
compiler=$compiler" -FE."


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
