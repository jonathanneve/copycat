//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("Unit1.cpp", Form2);
USEFORM("Unit2.cpp", fmDBLogin);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->HelpFile = "CopyCatDemo.hlp";
                 Application->Title = "Microtec CopyCat Demo";
                 Application->CreateForm(__classid(TForm2), &Form2);
                 Application->CreateForm(__classid(TfmDBLogin), &fmDBLogin);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
