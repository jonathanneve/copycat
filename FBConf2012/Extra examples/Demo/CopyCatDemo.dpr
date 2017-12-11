program CopyCatDemo;

uses
//WinHelpViewer, // Needed for D2006 & XP.  *** Won't work for Vista.
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {fmDBLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := 'CopyCatDemo.hlp';
  Application.Title := 'CopyCat Demo';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfmDBLogin, fmDBLogin);
  Application.Run;
end.
