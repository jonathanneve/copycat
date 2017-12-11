//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
USEFORMNS("..\..\Common\IBX\fConnectParams.pas", Fconnectparams, frConnectParams); /* TFrame: File Type */
USEFORMNS("..\genproc.pas", Genproc, fmGenProc);
USEFORMNS("..\main.pas", Main, MainForm);
USEFORMNS("..\subnode.pas", Subnode, fmSubNode);
USEFORMNS("..\tableconfig.pas", Tableconfig, fmTables);
//---------------------------------------------------------------------------
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
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
