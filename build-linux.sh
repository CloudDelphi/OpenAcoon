#!/bin/sh
echo "Starting Linux build of OpenAcoon..."
echo "------------------------------------"
rm src/*.o
rm src/*.ppu
rm src/cgi/*.o
rm src/cgi/*.ppu
mkdir bin
rm bin/*
set -e
compiler="fpc -Mdelphi -Tlinux -O3 -vew -Sew -vq -Fl~/sources/indy10/"


function compile {
    $compiler $1.dpr
    mv $1 ../bin
    echo -e '\n'
}


cd src

compile ImportUrls
compile CleanUrlsTxt
compile PrepareRobot
compile ImportData
compile Sleep
compile Parser
compile GenDb
compile cgi/query
compile RobotNew
compile SearchServer
compile Robot

cd ..

echo "++++++++++++++++++++++++++"
echo "+                        +"
echo "+ Build was successful ! +"
echo "+                        +"
echo "++++++++++++++++++++++++++"
