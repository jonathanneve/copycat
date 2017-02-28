//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CopyCat.res");
USEFORM("Unit1.cpp", Form2);
USEFORM("C:\Program Files\Borland\CBuilder5\Projects\Unit3.cpp", Form3);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->HelpFile = "CopyCatDemo.hlp";
                 Application->Title = "Microtec CopyCat Demo";
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
