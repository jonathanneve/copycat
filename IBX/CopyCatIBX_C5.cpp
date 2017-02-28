//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CopyCatIBX_C5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("VCLDB50.bpi");
USEUNIT("CcProvIBX.pas");
USEPACKAGE("CopyCat_C5.bpi");
USEPACKAGE("vclib50.bpi");
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
