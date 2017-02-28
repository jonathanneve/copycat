program CopyCatDemo;

uses
  ExceptionLog,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {fmDBLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := 'C:\Projects\Replicator\Demo\CopyCatDemo.hlp';
  Application.Title := 'CopyCat Demo';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfmDBLogin, fmDBLogin);
  Application.Run;
end.
