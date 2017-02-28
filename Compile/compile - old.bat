@echo off

cd \projects\copycat

echo.
echo Compiling BCB6...
"c:\Program Files\Borland\CBuilder6\bin\dcc32" CopyCat_C6.dpk -JPHNE -b -Q -W -H -N0BCB6 -NHBCB6 -NOBCB6 -NBBCB6 -LEBCB6 -LNBCB6
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\CBuilder6\bin\dcc32" CopyCat_C6.dpk -Q -W -H -N0BCB6 -NHBCB6 -NOBCB6 -NBBCB6 -LEBCB6 -LNBCB6
IF ERRORLEVEL 1 GOTO error

rem *** on BCC32 if put -c option after filename then is ignored and tries to run ilink32 (all options should come before filename)
"c:\Program Files\Borland\CBuilder6\bin\bcc32" -c -D_RTLDLL;NO_STRICT;USEPACKAGES -O2 -Hu -Vx -Ve -r -a8 -b- -k- -vi- -tWM -nBCB6 CopyCat_C6.cpp
IF ERRORLEVEL 1 GOTO error

rem -H="%s\lib\vcl%d0.csm" [TargetConfig.Target.RootDir, TargetConfig.Target.Version]));
rem    Lines.Add(Format('-I"%s\include;%s\include\vcl"',
rem    if DebugUnits then
rem      Lines.Add('-D_DEBUG -y -v');

"c:\Program Files\Borland\CBuilder6\bin\ilink32" -x -aa -Tpp -Gn -Gl -Gi -jBCB6 -IBCB6 -LBCB6;"C:\Program Files\Borland\CBuilder6\Lib";"C:\Program Files\Borland\CBuilder6\Lib\obj";"C:\Program Files\Borland\CBuilder6\Lib\release" c0pkg32.obj vcl.bpi dbrtl.bpi rtl.bpi vcldb.bpi designide.bpi Memmgr.Lib sysinit.obj BCB6\CopyCat_C6.obj BCB6\CcConf.obj BCB6\CcConflictMgr.obj BCB6\CcConfStorage.obj BCB6\CcLog.obj BCB6\CcMemDS.obj BCB6\CcReplicator.obj BCB6\CcProviders.obj BCB6\CCat.obj BCB6\CcRplList.obj BCB6\CcInterbase.obj BCB6\CcNexusDB.obj BCB6\CcInterbaseConn.obj BCB6\CcEditors.obj BCB6\CcTransports.obj BCB6\CcKeys.obj BCB6\CcSQLServer.obj BCB6\CcDB.obj BCB6\CcMySQL.obj BCB6\CcSQLite.obj, BCB6\CopyCat_C6.bpl, , import32.lib cp32mti.lib, , CopyCat_C6.res
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling BCB6 Trial...
"c:\Program Files\Borland\CBuilder6\bin\dcc32" CopyCat_C6.dpk -JPHNE -b -Q -W -H -N0Trial\BCB6 -NHTrial\BCB6 -NOTrial\BCB6 -NBTrial\BCB6 -LETrial\BCB6 -LNTrial\BCB6 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\CBuilder6\bin\dcc32" CopyCat_C6.dpk -Q -W -H -N0Trial\BCB6 -NHTrial\BCB6 -NOTrial\BCB6 -NBTrial\BCB6 -LETrial\BCB6 -LNTrial\BCB6 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

rem *** on BCC32 if put -c option after filename then is ignored and tries to run ilink32 (all options should come before filename)
"c:\Program Files\Borland\CBuilder6\bin\bcc32" -c -D_RTLDLL;NO_STRICT;USEPACKAGES -O2 -Hu -Vx -Ve -r -a8 -b- -k- -vi- -tWM -nTrial\BCB6 CopyCat_C6.cpp
IF ERRORLEVEL 1 GOTO error

rem -H="%s\lib\vcl%d0.csm" [TargetConfig.Target.RootDir, TargetConfig.Target.Version]));
rem    Lines.Add(Format('-I"%s\include;%s\include\vcl"',
rem    if DebugUnits then
rem      Lines.Add('-D_DEBUG -y -v');

"c:\Program Files\Borland\CBuilder6\bin\ilink32" -x -aa -Tpp -Gn -Gl -Gi -jTrial\BCB6 -ITrial\BCB6 -LTrial\BCB6;"C:\Program Files\Borland\CBuilder6\Lib";"C:\Program Files\Borland\CBuilder6\Lib\obj";"C:\Program Files\Borland\CBuilder6\Lib\release" c0pkg32.obj vcl.bpi dbrtl.bpi rtl.bpi vcldb.bpi designide.bpi Memmgr.Lib sysinit.obj Trial\BCB6\CopyCat_C6.obj Trial\BCB6\CcConf.obj Trial\BCB6\CcConflictMgr.obj Trial\BCB6\CcConfStorage.obj Trial\BCB6\CcLog.obj Trial\BCB6\CcMemDS.obj Trial\BCB6\CcReplicator.obj Trial\BCB6\CcProviders.obj Trial\BCB6\CCat.obj Trial\BCB6\CcRplList.obj Trial\BCB6\CcInterbase.obj Trial\BCB6\CcInterbaseConn.obj Trial\BCB6\CcEditors.obj Trial\BCB6\CcTransports.obj Trial\BCB6\CcKeys.obj Trial\BCB6\CcSQLServer.obj Trial\BCB6\CcDB.obj Trial\BCB6\CcMySQL.obj Trial\BCB6\CcSQLite.obj Trial\BCB6\CcNexusDB.obj, Trial\BCB6\CopyCat_C6.bpl, , import32.lib cp32mti.lib, , CopyCat_C6.res
IF ERRORLEVEL 1 GOTO error



echo.
echo Compiling BCB5...
"c:\Program Files\Borland\CBuilder5\bin\dcc32" CopyCat_C5.dpk -JPHNE -b -Q -W -H -N0BCB5 -NHBCB5 -NOBCB5 -NBBCB5 -LEBCB5 -LNBCB5
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\CBuilder5\bin\dcc32" CopyCat_C5.dpk -Q -W -H -N0BCB5 -NHBCB5 -NOBCB5 -NBBCB5 -LEBCB5 -LNBCB5
IF ERRORLEVEL 1 GOTO error

rem *** on BCC32 if put -c option after filename then is ignored and tries to run ilink32 (all options should come before filename)
"c:\Program Files\Borland\CBuilder5\bin\bcc32" -c -D_RTLDLL;NO_STRICT;USEPACKAGES -O2 -Hu -Vx -Ve -r -a8 -b- -k- -vi- -tWM -nBCB5 CopyCat_C5.cpp
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\CBuilder5\bin\ilink32" -x -aa -Tpp -Gn -Gl -Gi -jBCB5 -IBCB5 -LBCB5;"C:\Program Files\Borland\CBuilder5\Lib";"C:\Program Files\Borland\CBuilder5\Lib\obj";"C:\Program Files\Borland\CBuilder5\Lib\release" c0pkg32.obj vcl50.bpi vcldb50.bpi dsnide50.bpi Memmgr.Lib sysinit.obj BCB5\CopyCat_C5.obj BCB5\CcConf.obj BCB5\CcConflictMgr.obj BCB5\CcConfStorage.obj BCB5\CcLog.obj BCB5\CcMemDS.obj BCB5\CcReplicator.obj BCB5\CcProviders.obj BCB5\CCat.obj BCB5\CcRplList.obj BCB5\CcInterbase.obj BCB5\CcInterbaseConn.obj BCB5\CcEditors.obj BCB5\CcTransports.obj BCB5\CcKeys.obj BCB5\CcOracle.obj BCB5\CcSQLServer.obj BCB5\CcDB.obj BCB5\CcMySQL.obj BCB5\CcSQLite.obj BCB5\CcNexusDB.obj , BCB5\CopyCat_C5.bpl, , import32.lib cp32mti.lib, , CopyCat_C5.res
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling BCB5 Trial...
"c:\Program Files\Borland\CBuilder5\bin\dcc32" CopyCat_C5.dpk -JPHNE -b -Q -W -H -N0Trial\BCB5 -NHTrial\BCB5 -NOTrial\BCB5 -NBTrial\BCB5 -LETrial\BCB5 -LNTrial\BCB5 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\CBuilder5\bin\dcc32" CopyCat_C5.dpk -Q -W -H -N0Trial\BCB5 -NHTrial\BCB5 -NOTrial\BCB5 -NBTrial\BCB5 -LETrial\BCB5 -LNTrial\BCB5 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

rem *** on BCC32 if put -c option after filename then is ignored and tries to run ilink32 (all options should come before filename)
"c:\Program Files\Borland\CBuilder5\bin\bcc32" -c -D_RTLDLL;NO_STRICT;USEPACKAGES -O2 -Hu -Vx -Ve -r -a8 -b- -k- -vi- -tWM -nTrial\BCB5 CopyCat_C5.cpp
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\CBuilder5\bin\ilink32" -x -aa -Tpp -Gn -Gl -Gi -jTrial\BCB5 -ITrial\BCB5 -LTrial\BCB5;"C:\Program Files\Borland\CBuilder5\Lib";"C:\Program Files\Borland\CBuilder5\Lib\obj";"C:\Program Files\Borland\CBuilder5\Lib\release" c0pkg32.obj vcl50.bpi vcldb50.bpi dsnide50.bpi Memmgr.Lib sysinit.obj Trial\BCB5\CopyCat_C5.obj Trial\BCB5\CcConf.obj Trial\BCB5\CcConflictMgr.obj Trial\BCB5\CcConfStorage.obj Trial\BCB5\CcLog.obj Trial\BCB5\CcMemDS.obj Trial\BCB5\CcReplicator.obj Trial\BCB5\CcProviders.obj Trial\BCB5\CCat.obj Trial\BCB5\CcRplList.obj Trial\BCB5\CcInterbase.obj Trial\BCB5\CcInterbaseConn.obj Trial\BCB5\CcEditors.obj Trial\BCB5\CcTransports.obj Trial\BCB5\CcKeys.obj Trial\BCB5\CcSQLServer.obj Trial\BCB5\CcDB.obj Trial\BCB5\CcMySQL.obj Trial\BCB5\CcSQLite.obj Trial\BCB5\CcNexusDB.obj, Trial\BCB5\CopyCat_C5.bpl, , import32.lib cp32mti.lib, , CopyCat_C5.res
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling D5 trial...
"c:\Program Files\Borland\Delphi5\bin\dcc32" CopyCat_D5.dpk -b -Q -W -H -NTrial\D5 -LETrial\D5 -LNTrial\D5 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D5...
"c:\Program Files\Borland\Delphi5\bin\dcc32" CopyCat_D5.dpk -b -Q -W -H -ND5 -LED5 -LND5
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D6 trial...
"c:\Program Files\Borland\Delphi6\bin\dcc32" CopyCat_D6.dpk -b -Q -W -H -NTrial\D6 -LETrial\D6 -LNTrial\D6 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D6...
"c:\Program Files\Borland\Delphi6\bin\dcc32" CopyCat_D6.dpk -b -Q -W -H -ND6 -LED6 -LND6
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D6 connectors...
rem "c:\Program Files\Borland\Delphi6\bin\dcc32" ADO\CopyCatADO_D6.dpk -b -Q -W -H -ND6\Connectors -LED6\Connectors -LND6\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D7 trial...
"c:\Program Files\Borland\Delphi7\bin\dcc32" CopyCat_D7.dpk -b -Q -W -H -NTrial\D7 -LETrial\D7 -LNTrial\D7 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D7...
"c:\Program Files\Borland\Delphi7\bin\dcc32" CopyCat_D7.dpk -b -Q -W -H -ND7 -LED7 -LND7
IF ERRORLEVEL 1 GOTO error

cd Ado
echo.
echo Compiling D7 connectors...
rem "c:\Program Files\Borland\Delphi7\bin\dcc32" CopyCatADO_D7.dpk -b -Q -W -H -ND7\Connectors -LED7\Connectors -LND7\Connectors
IF ERRORLEVEL 1 GOTO error
cd ..

echo.
echo Compiling D2005...
"c:\Program Files\Borland\BDS\3.0\bin\dcc32" CopyCat_2005.dpk -b -Q -W -H -ND2005 -LED2005 -LND2005
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2005 trial...
"c:\Program Files\Borland\BDS\3.0\bin\dcc32" CopyCat_2005.dpk -b -Q -W -H -NTrial\D2005 -LETrial\D2005 -LNTrial\D2005 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2005 connectors...
cd Ado
rem "c:\Program Files\Borland\BDS\3.0\bin\dcc32" CopyCatADO_2005.dpk -b -Q -W -H -ND2005\Connectors -LED2005\Connectors -LND2005\Connectors
IF ERRORLEVEL 1 GOTO error
cd ..


echo.
echo Compiling D2006...
"c:\Program Files\Borland\BDS\4.0\bin\dcc32" CopyCat_2006.dpk -JPHNE -b -Q -W -H -N0D2006 -NHD2006 -NOD2006 -NBD2006 -LED2006 -LND2006
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\BDS\4.0\bin\dcc32" CopyCat_2006.dpk -JL -Q -W -H -N0D2006 -NHD2006 -NOD2006 -NBD2006 -LED2006 -LND2006
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2006 trial...
"c:\Program Files\Borland\BDS\4.0\bin\dcc32" CopyCat_2006.dpk -JPHNE -b -Q -W -H -N0Trial\D2006 -NHTrial\D2006 -NOTrial\D2006 -NBTrial\D2006 -LETrial\D2006 -LNTrial\D2006 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Borland\BDS\4.0\bin\dcc32" CopyCat_2006.dpk -JL -Q -W -H -N0Trial\D2006 -NHTrial\D2006 -NOTrial\D2006 -NBTrial\D2006 -LETrial\D2006 -LNTrial\D2006 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2006 connectors...
rem "c:\Program Files\Borland\BDS\4.0\bin\dcc32" ADO\CopyCatADO_D2006.dpk -JL -b -Q -W -H -N0D2006\Connectors -NHD2006\Connectors -NOD2006\Connectors -NBD2006\Connectors -LED2006\Connectors -LND2006\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2007...
"c:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32" CopyCat_2007.dpk -JPHNE -b -Q -W -H -N0D2007 -NHD2007 -NOD2007 -NBD2007 -LED2007 -LND2007
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32" CopyCat_2007.dpk -JL -Q -W -H -N0D2007 -NHD2007 -NOD2007 -NBD2007 -LED2007 -LND2007
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2007 trial...
"c:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32" CopyCat_2007.dpk -JPHNE -b -Q -W -H -N0Trial\D2007 -NHTrial\D2007 -NOTrial\D2007 -NBTrial\D2007 -LETrial\D2007 -LNTrial\D2007 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32" CopyCat_2007.dpk -JL -Q -W -H -N0Trial\D2007 -NHTrial\D2007 -NOTrial\D2007 -NBTrial\D2007 -LETrial\D2007 -LNTrial\D2007 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2007 connectors...
cd ADO
call "c:\Program Files\Embarcadero\RAD Studio\12.0\bin\rsvars.bat"
rem msbuild CopyCatADO_2007.dpk -N0D2007\Connectors -NHD2007\Connectors -NOD2007\Connectors -NBD2007\Connectors -LED2007\Connectors -LND2007\Connectors
IF ERRORLEVEL 1 GOTO error
cd ..

echo.
echo Compiling D2009...
"c:\Program Files\CodeGear\RAD Studio\6.0\bin\dcc32" CopyCat_2009.dpk -JPHNE -b -Q -W -H -N0D2009 -NHD2009 -NOD2009 -NBD2009 -LED2009 -LND2009
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\CodeGear\RAD Studio\6.0\bin\dcc32" CopyCat_2009.dpk -JL -Q -W -H -N0D2009 -NHD2009 -NOD2009 -NBD2009 -LED2009 -LND2009
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2009 trial...
"c:\Program Files\CodeGear\RAD Studio\6.0\bin\dcc32" CopyCat_2009.dpk -JPHNE -b -Q -W -H -N0Trial\D2009 -NHTrial\D2009 -NOTrial\D2009 -NBTrial\D2009 -LETrial\D2009 -LNTrial\D2009 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\CodeGear\RAD Studio\6.0\bin\dcc32" CopyCat_2009.dpk -JL -Q -W -H -N0Trial\D2009 -NHTrial\D2009 -NOTrial\D2009 -NBTrial\D2009 -LETrial\D2009 -LNTrial\D2009 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2009 connectors...
rem "c:\Program Files\CodeGear\RAD Studio\6.0\bin\dcc32" ADO\CopyCatADO_D2009.dpk -JL -b -Q -W -H -N0D2009\Connectors -NHD2009\Connectors -NOD2009\Connectors -NBD2009\Connectors -LED2009\Connectors -LND2009\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2010...
"c:\Program Files\Embarcadero\RAD Studio\7.0\bin\dcc32" CopyCat_2010.dpk -JPHNE -b -Q -W -H -N0D2010 -NHD2010 -NOD2010 -NBD2010 -LED2010 -LND2010
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\7.0\bin\dcc32" CopyCat_2010.dpk -JL -Q -W -H -N0D2010 -NHD2010 -NOD2010 -NBD2010 -LED2010 -LND2010
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2010 trial...
"c:\Program Files\Embarcadero\RAD Studio\7.0\bin\dcc32" CopyCat_2010.dpk -JPHNE -b -Q -W -H -N0Trial\D2010 -NHTrial\D2010 -NOTrial\D2010 -NBTrial\D2010 -LETrial\D2010 -LNTrial\D2010 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\7.0\bin\dcc32" CopyCat_2010.dpk -JL -Q -W -H -N0Trial\D2010 -NHTrial\D2010 -NOTrial\D2010 -NBTrial\D2010 -LETrial\D2010 -LNTrial\D2010 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2010 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\7.0\bin\dcc32" ADO\CopyCatADO_D2010.dpk -JL -b -Q -W -H -N0D2010\Connectors -NHD2010\Connectors -NOD2010\Connectors -NBD2010\Connectors -LED2010\Connectors -LND2010\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2011...
"c:\Program Files\Embarcadero\RAD Studio\8.0\bin\dcc32" CopyCat_2011.dpk -JPHNE -b -Q -W -H -N0D2011 -NHD2011 -NOD2011 -NBD2011 -LED2011 -LND2011
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\8.0\bin\dcc32" CopyCat_2011.dpk -JL -Q -W -H -N0D2011 -NHD2011 -NOD2011 -NBD2011 -LED2011 -LND2011
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2011 trial...
"c:\Program Files\Embarcadero\RAD Studio\8.0\bin\dcc32" CopyCat_2011.dpk -JPHNE -b -Q -W -H -N0Trial\D2011 -NHTrial\D2011 -NOTrial\D2011 -NBTrial\D2011 -LETrial\D2011 -LNTrial\D2011 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\8.0\bin\dcc32" CopyCat_2011.dpk -JL -Q -W -H -N0Trial\D2011 -NHTrial\D2011 -NOTrial\D2011 -NBTrial\D2011 -LETrial\D2011 -LNTrial\D2011 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2011 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\8.0\bin\dcc32" ADO\CopyCatADO_D2011.dpk -JL -b -Q -W -H -N0D2011\Connectors -NHD2011\Connectors -NOD2011\Connectors -NBD2011\Connectors -LED2011\Connectors -LND2011\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2012...
"c:\Program Files\Embarcadero\RAD Studio\9.0\bin\dcc32" CopyCat_2012.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0D2012 -NHD2012 -NOD2012 -NBD2012 -LED2012 -LND2012
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\9.0\bin\dcc32" CopyCat_2012.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0D2012 -NHD2012 -NOD2012 -NBD2012 -LED2012 -LND2012
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2012 trial...
"c:\Program Files\Embarcadero\RAD Studio\9.0\bin\dcc32" CopyCat_2012.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0Trial\D2012 -NHTrial\D2012 -NOTrial\D2012 -NBTrial\D2012 -LETrial\D2012 -LNTrial\D2012 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\9.0\bin\dcc32" CopyCat_2012.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0Trial\D2012 -NHTrial\D2012 -NOTrial\D2012 -NBTrial\D2012 -LETrial\D2012 -LNTrial\D2012 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2012 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\9.0\bin\dcc32" ADO\CopyCatADO_D2012.dpk -JL -b -Q -W -H -N0D2012\Connectors -NHD2012\Connectors -NOD2012\Connectors -NBD2012\Connectors -LED2012\Connectors -LND2012\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2013...
"c:\Program Files\Embarcadero\RAD Studio\10.0\bin\dcc32" CopyCat_2013.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0D2013 -NHD2013 -NOD2013 -NBD2013 -LED2013 -LND2013
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\10.0\bin\dcc32" CopyCat_2013.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0D2013 -NHD2013 -NOD2013 -NBD2013 -LED2013 -LND2013
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2013 trial...
"c:\Program Files\Embarcadero\RAD Studio\10.0\bin\dcc32" CopyCat_2013.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0Trial\D2013 -NHTrial\D2013 -NOTrial\D2013 -NBTrial\D2013 -LETrial\D2013 -LNTrial\D2013 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\10.0\bin\dcc32" CopyCat_2013.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0Trial\D2013 -NHTrial\D2013 -NOTrial\D2013 -NBTrial\D2013 -LETrial\D2013 -LNTrial\D2013 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2013 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\10.0\bin\dcc32" ADO\CopyCatADO_D2013.dpk -JL -b -Q -W -H -N0D2013\Connectors -NHD2013\Connectors -NOD2013\Connectors -NBD2013\Connectors -LED2013\Connectors -LND2013\Connectors
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling D2014...
"c:\Program Files\Embarcadero\RAD Studio\11.0\bin\dcc32" CopyCat_2014.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0D2014 -NHD2014 -NOD2014 -NBD2014 -LED2014 -LND2014
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\11.0\bin\dcc32" CopyCat_2014.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0D2014 -NHD2014 -NOD2014 -NBD2014 -LED2014 -LND2014
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2014 trial...
"c:\Program Files\Embarcadero\RAD Studio\11.0\bin\dcc32" CopyCat_2014.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0Trial\D2014 -NHTrial\D2014 -NOTrial\D2014 -NBTrial\D2014 -LETrial\D2014 -LNTrial\D2014 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\11.0\bin\dcc32" CopyCat_2014.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0Trial\D2014 -NHTrial\D2014 -NOTrial\D2014 -NBTrial\D2014 -LETrial\D2014 -LNTrial\D2014 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2014 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\11.0\bin\dcc32" ADO\CopyCatADO_D2014.dpk -JL -b -Q -W -H -N0D2014\Connectors -NHD2014\Connectors -NOD2014\Connectors -NBD2014\Connectors -LED2014\Connectors -LND2014\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2015...
"c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" CopyCat_XE5.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0D2015 -NHD2015 -NOD2015 -NBD2015 -LED2015 -LND2015
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" CopyCat_XE5.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0D2015 -NHD2015 -NOD2015 -NBD2015 -LED2015 -LND2015
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2015 trial...
"c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" CopyCat_XE5.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0Trial\D2015 -NHTrial\D2015 -NOTrial\D2015 -NBTrial\D2015 -LETrial\D2015 -LNTrial\D2015 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" CopyCat_XE5.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0Trial\D2015 -NHTrial\D2015 -NOTrial\D2015 -NBTrial\D2015 -LETrial\D2015 -LNTrial\D2015 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling D2015 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" ADO\CopyCatADO_XE5.dpk -JL -b -Q -W -H -N0D2015\Connectors -NHD2015\Connectors -NOD2015\Connectors -NBD2015\Connectors -LED2015\Connectors -LND2015\Connectors
IF ERRORLEVEL 1 GOTO error
rem "c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" FireDAC\CopyCatFireDAC_XE5.dpk -JL -b -Q -W -H -N0D2015\Connectors -NHD2015\Connectors -NOD2015\Connectors -NBD2015\Connectors -LED2015\Connectors -LND2015\Connectors
IF ERRORLEVEL 1 GOTO error
rem "c:\Program Files\Embarcadero\RAD Studio\12.0\bin\dcc32" DataSnap\CopyCatDataSnap_XE5.dpk -JL -b -Q -W -H -N0D2015\Connectors -NHD2015\Connectors -NOD2015\Connectors -NBD2015\Connectors -LED2015\Connectors -LND2015\Connectors
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling XE6...
"c:\Program Files\Embarcadero\Studio\14.0\bin\dcc32" CopyCat_XE6.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0XE6 -NHXE6 -NOXE6 -NBXE6 -LEXE6 -LNXE6
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\Studio\14.0\bin\dcc32" CopyCat_XE6.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0XE6 -NHXE6 -NOXE6 -NBXE6 -LEXE6 -LNXE6
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling XE6 trial...
 "c:\Program Files\Embarcadero\Studio\14.0\bin\dcc32" CopyCat_XE6.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0Trial\XE6 -NHTrial\XE6 -NOTrial\XE6 -NBTrial\XE6 -LETrial\XE6 -LNTrial\XE6 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
 "c:\Program Files\Embarcadero\Studio\14.0\bin\dcc32" CopyCat_XE6.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0Trial\XE6 -NHTrial\XE6 -NOTrial\XE6 -NBTrial\XE6 -LETrial\XE6 -LNTrial\XE6 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling DXE6 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\14.0\bin\dcc32" ADO\CopyCatADO_XE6.dpk -JL -b -Q -W -H -N0XE6\Connectors -NHXE6\Connectors -NOXE6\Connectors -NBXE6\Connectors -LEXE6\Connectors -LNXE6\Connectors
IF ERRORLEVEL 1 GOTO error
rem "c:\Program Files\Embarcadero\RAD Studio\14.0\bin\dcc32" FireDAC\CopyCatFireDAC_XE6.dpk -JL -b -Q -W -H -N0XE6\Connectors -NHXE6\Connectors -NOXE6\Connectors -NBXE6\Connectors -LEXE6\Connectors -LNXE6\Connectors
IF ERRORLEVEL 1 GOTO error
rem "c:\Program Files\Embarcadero\RAD Studio\14.0\bin\dcc32" DataSnap\CopyCatDataSnap_XE6.dpk -JL -b -Q -W -H -N0XE6\Connectors -NHXE6\Connectors -NOXE6\Connectors -NBXE6\Connectors -LEXE6\Connectors -LNXE6\Connectors
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling XE7...
"c:\Program Files\Embarcadero\Studio\15.0\bin\dcc32" CopyCat_XE7.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0XE7 -NHXE7 -NOXE7 -NBXE7 -LEXE7 -LNXE7
IF ERRORLEVEL 1 GOTO error
"c:\Program Files\Embarcadero\Studio\15.0\bin\dcc32" CopyCat_XE7.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0XE7 -NHXE7 -NOXE7 -NBXE7 -LEXE7 -LNXE7
IF ERRORLEVEL 1 GOTO error


echo.
echo Compiling XE6 trial...
 "c:\Program Files\Embarcadero\Studio\15.0\bin\dcc32" CopyCat_XE7.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JPHNE -b -Q -W -H -N0Trial\XE7 -NHTrial\XE7 -NOTrial\XE7 -NBTrial\XE7 -LETrial\XE7 -LNTrial\XE7 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error
 "c:\Program Files\Embarcadero\Studio\15.0\bin\dcc32" CopyCat_XE7.dpk -nsSystem;System.Win;Winapi;Vcl;Data;Xml -JL -Q -W -H -N0Trial\XE7 -NHTrial\XE7 -NOTrial\XE7 -NBTrial\XE7 -LETrial\XE7 -LNTrial\XE7 -DCC_TRIAL
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling DXE7 connectors...
rem "c:\Program Files\Embarcadero\RAD Studio\14.0\bin\dcc32" ADO\CopyCatADO_XE6.dpk -JL -b -Q -W -H -N0XE6\Connectors -NHXE6\Connectors -NOXE6\Connectors -NBXE6\Connectors -LEXE6\Connectors -LNXE6\Connectors
IF ERRORLEVEL 1 GOTO error
rem "c:\Program Files\Embarcadero\RAD Studio\14.0\bin\dcc32" FireDAC\CopyCatFireDAC_XE6.dpk -JL -b -Q -W -H -N0XE6\Connectors -NHXE6\Connectors -NOXE6\Connectors -NBXE6\Connectors -LEXE6\Connectors -LNXE6\Connectors
IF ERRORLEVEL 1 GOTO error
rem "c:\Program Files\Embarcadero\RAD Studio\14.0\bin\dcc32" DataSnap\CopyCatDataSnap_XE6.dpk -JL -b -Q -W -H -N0XE6\Connectors -NHXE6\Connectors -NOXE6\Connectors -NBXE6\Connectors -LEXE6\Connectors -LNXE6\Connectors
IF ERRORLEVEL 1 GOTO error

cd Install

echo.
echo Compiling Trial setup...
iscc trial.iss 
IF ERRORLEVEL 1 GOTO error

echo.
echo Compiling Pro setup...
iscc fullsource.iss 
IF ERRORLEVEL 1 GOTO error

goto end


:error
echo.
echo *** There is an error! ***
echo.

:end
pause