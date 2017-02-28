program TransportServer;

uses
  Forms,
  TrnSrvUnit1 in '..\..\TrnSrvUnit1.pas' {Form1},
  fServerOptions in '..\fServerOptions.pas' {frServerOptions: TFrame},
  fConnectParams in '..\..\..\Common\FireDAC\fConnectParams.pas' {frConnectParams: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CopyCat TransportServer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
