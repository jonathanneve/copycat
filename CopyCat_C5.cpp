//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CopyCat_C5.res");
USERES("CopyCat.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("VCLDB50.bpi");
USEUNIT("CcProviders.pas");
USEUNIT("CCat.pas");
USEUNIT("CcConf.pas");
USEUNIT("CcConflictMgr.pas");
USEUNIT("CcLog.pas");
USEUNIT("CcMemDS.pas");
USEUNIT("CcReplicator.pas");
USEUNIT("CcTransports.pas");
USEUNIT("CcInterbase.pas");
USEUNIT("CcKeys.pas");
USEUNIT("CcEditors.pas");
USEUNIT("CcInterbaseConn.pas");
USEUNIT("CcDB.pas");
USEUNIT("CcSQLServer.pas");
USEUNIT("CcMySQL.pas");
USEUNIT("CcOracle.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
