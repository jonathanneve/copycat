program Agent;

uses
  EMemLeaks,
  Vcl.Forms,
  agentmain in 'agentmain.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
