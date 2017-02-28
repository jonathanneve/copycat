//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("replicator.res");
USEFORM("Unit1.cpp", Form2);
USEUNIT("C:\Program Files\GExperts\DbugIntf.pas");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm2), &Form2);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
