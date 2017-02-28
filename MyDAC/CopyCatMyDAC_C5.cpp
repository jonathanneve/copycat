//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//{$R '..\CopyCat.dcr'}
USERES("CopyCatMyDAC_C5.res");
//USERES("..\CopyCat.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CopyCat_C5.bpi");
USEUNIT("CcProvMyDAC.pas");
USEPACKAGE("vcldb50.bpi");
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
