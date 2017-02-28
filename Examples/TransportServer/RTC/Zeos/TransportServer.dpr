program TransportServer;

uses
  Forms,
  TrnSrvUnit1 in '..\..\TrnSrvUnit1.pas' {Form1},
  fConnectParams in '..\..\..\Common\Zeos\fConnectParams.pas' {frConnectParams: TFrame},
  fServerOptions in '..\fServerOptions.pas' {frServerOptions: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CopyCat TransportServer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
