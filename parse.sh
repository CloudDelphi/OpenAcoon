#!/bin/sh

while true
do
    bin/Parser /-olinks /nobacklinks
    bin/Sleep 05
done
