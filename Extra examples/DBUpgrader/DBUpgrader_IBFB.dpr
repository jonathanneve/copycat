program DBUpgrader_IBFB;

uses
  ExceptionLog,
  Forms,
  fConnectParams in '..\..\Examples\Common\FIBPlus\fConnectParams.pas' {frConnectParams: TFrame},
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
