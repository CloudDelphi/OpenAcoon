### OpenAcoon

This is a web search-engine that can easily hold a few hundred million web-pages
in its search-index. If you want to see what it can do see
https://deusu.org/

The above website runs on an Intel i7-3770K with 32gb RAM and two 500gb SSDs.
The search-index on that site currently holds about 327 million WWW-pages.
On average a query takes about 0.6 seconds. The transfer-speed from SSD into
RAM is the limiting-factor for query-time. Even 600mb/s can be slow sometimes. :)

The software is written in Delphi (=Pascal). The code is written for Delphi XE2.
I don't know if it will compile and run in newer versions of Delphi. Please see
the note below about compiling it on Linux.

Sorry for the quality of the code. Some of it was written 15 years ago when I was
still young and stupid. :)

[Note 12-Jul-2014: I had to dive deep into the Indy-sourcecode the last few days.
I feel a LOT better about the quality of my own sourcecode now... :) ]

#### Linux

As of 12-Jul-2014 the master branch will compile with FreePascal on Linux.
I have tested it a bit and it at least *seems* to work. That of course does
*not* mean that it is bug-free. :)

To compile with FreePascal you may need to change a pathname in build-linux.sh,
so that FPC will find all the necessary files.

#### Compiling with FreePascal for Windows

As of 09-Feb-2015 the master branch will compile with FreePascal for Windows.

On Windows use "build-windows-fpc.bat" to compile.

You can even use FPC to cross-compile on Linux to Windows. In that case use
"build-windows-on-linux.sh". You will have to manually compile FPC to enable
it to cross-compile of course. This is a bit tricky... :)

In both cases you will probably have to adjust some compiler-options
in the the bat/sh file to let FPC know about the path to its libraries.
