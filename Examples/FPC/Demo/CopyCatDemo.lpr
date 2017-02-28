program CopyCatDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
