//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CopyCatUIB_C5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("VCLDB50.bpi");
USEPACKAGE("CopyCat_C5.bpi");
USEUNIT("CcProvUIB.pas");
USEPACKAGE("JvUIBC5R.bpi");
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
