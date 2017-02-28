@echo off

cd C:\Projects\CopyCat\doc
call ..\install\ftpupload.bat CopyCat.hlp
call ..\install\ftpupload.bat CHM\CopyCat.chm
call ..\install\ftpuploaddir.bat HTML\* doc/CC
pause