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
compiler="fpc -Mdelphi -Tlinux -O3 -vew -Sew -vq"

cd src

$compiler ImportUrls.dpr
mv ImportUrls ../bin

$compiler CleanUrlsTxt.dpr
mv CleanUrlsTxt ../bin

$compiler PrepareRobot.dpr
mv PrepareRobot ../bin

$compiler ImportData.dpr
mv ImportData ../bin

$compiler Sleep.dpr
mv Sleep ../bin

$compiler Parser.dpr
mv Parser ../bin

$compiler Robot.dpr
mv Robot ../bin

$compiler RobotNew.dpr
mv RobotNew ../bin

$compiler GenDb.dpr
mv GenDb ../bin

$compiler SearchServer.dpr
mv SearchServer ../bin

$compiler cgi/query.dpr
mv /cgi/query ../bin

cd ..
