//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORMNS("..\..\Common\IBX\fConnectParams.pas", Fconnectparams, frConnectParams); /* TFrame: File Type */
USEFORMNS("..\main.pas", Main, MainForm);
USEFORMNS("..\genproc.pas", Genproc, fmGenProc);
USEFORMNS("..\subnode.pas", Subnode, fmSubNode);
USEFORMNS("..\tableconfig.pas", Tableconfig, fmTables);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TMainForm), &MainForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        catch (...)
        {
                 try
                 {
                         throw Exception("");
                 }
                 catch (Exception &exception)
                 {
                         Application->ShowException(&exception);
                 }
        }
        return 0;
}
//---------------------------------------------------------------------------
