program replicator;

uses
  Forms,
  Unit1 in '..\Unit1.pas' {Form1},
  conflict in '..\conflict.pas' {fmConflict},
  fConnectParams in '..\..\Common\ADO\fConnectParams.pas' {frConnectParams: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CopyCat Replicator';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
