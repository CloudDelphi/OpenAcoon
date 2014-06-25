#!/bin/sh
echo "Starting Linux build of OpenAcoon..."
echo "------------------------------------"
rm *.o
rm *.ppu
set -e
compiler="fpc -Mdelphi -Tlinux -O3 -vew -Sew -vq"

cd src
$compiler Sleep.dpr
$compiler CleanUrlsTxt.dpr
$compiler ConsolidateRedirects.dpr
$compiler ImportAlexa.dpr
$compiler ImportData.dpr
$compiler PrepareRobot.dpr
$compiler Tests.dpr
$compiler ImportUrls.dpr
$compiler Parser.dpr
$compiler GenDb.dpr
$compiler RobotNew.dpr
$compiler SearchServer.dpr
cd ..
