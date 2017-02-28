//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//{$R '..\CopyCat.dcr'}
USERES("CopyCatXMLRPC_C5.res");
//USERES("..\CopyCat.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CopyCat_C5.bpi");
USEUNIT("CcXMLRPCTransport.pas");
USEUNIT("CcXmlRpcServer.pas");
USEUNIT("CcXmlRpcTypes.pas");
USEUNIT("CcDIMime.pas");
USEUNIT("CcLibXmlParser.pas");
USEUNIT("CcXmlRpcClient.pas");
USEUNIT("CcXmlRpcCommon.pas");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("Indy50.bpi");
USEUNIT("htmlentities.pas");
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
