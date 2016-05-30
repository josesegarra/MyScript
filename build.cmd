del .\main.exe
.\compiler\fpc\ppc386.exe -Mdelphi -FU.\temp -Fu".\source\headers" .\source\main.pas -o".\main.exe"
del /Q .\temp\*.*
.\main.exe tests/test1.js