:start
move data\crawler\parsed\*.* data\crawler\import
bin\ImportData
bin\CleanUrlsTxt data\txt\urls.txt data\txt\urls.txt
del data\txt\urls.txt
bin\ImportUrls -PreLoad -StartDb 0000 -EndDb 0127 -RoundRobin -MaxRuntime 0060
bin\Sleep 05
goto start
