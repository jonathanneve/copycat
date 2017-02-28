@echo off
echo user copycat> ftpcmd.dat
echo 84KLop9p0>> ftpcmd.dat
echo bin>> ftpcmd.dat
echo hash>> ftpcmd.dat
echo prompt>> ftpcmd.dat
echo cd /httpdocs/%2 >>ftpcmd.dat
echo mput %1>> ftpcmd.dat
echo quit>> ftpcmd.dat
ftp -n -s:ftpcmd.dat ftp.copycat.fr
del ftpcmd.dat