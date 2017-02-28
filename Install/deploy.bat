@echo off

cd C:\Projects\CopyCat\Install
call ftpupload.bat CopyCat_FullSource_%2%.exe
call ftpupload.bat CopyCatTrial_%2%.exe
pause