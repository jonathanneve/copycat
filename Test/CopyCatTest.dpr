program CopyCatTest;

uses
  ExceptionLog,
  Forms,
  CcTest in 'CcTest.pas',
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
