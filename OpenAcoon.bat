:start
move data\crawler\parsed\*.* data\crawler\import
ImportData
CleanUrlsTxt data\txt\urls.txt data\txt\urls.txt
del data\txt\urls.txt
ImportUrls -PreLoad -StartDb 0000 -EndDb 1023 -RoundRobin -MaxRuntime 0060
Sleep 05
goto start
