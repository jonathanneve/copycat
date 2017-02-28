//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CopyCatZeosD_C5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("CcProvZeos.pas");
USEPACKAGE("VCLDB50.bpi");
USEPACKAGE("CopyCat_C5.bpi");
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
