program server;

uses
  EMemLeaks,
  Vcl.Forms,
  server_main in 'server_main.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
