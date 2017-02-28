//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CopyCatRTC_C5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("CcRTCTransport.pas");
USEPACKAGE("rtcSDK_C5.bpi");
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
