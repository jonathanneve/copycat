program Project6;

uses
  Vcl.Forms,
  Unit6 in 'Unit6.pas' {Form6},
  ServerMethodsUnit1 in 'ServerMethodsUnit1.pas',
  ServerContainerUnit1 in 'ServerContainerUnit1.pas' {ServerContainer1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TServerContainer1, ServerContainer1);
  Application.Run;
end.

