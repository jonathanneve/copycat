program replicator;

uses
  Forms,
  Unit1 in '..\Unit1.pas' {fmMain},
  conflict in '..\conflict.pas' {fmConflict},
  fConnectParams in '..\..\Common\ZEOS\fConnectParams.pas' {frConnectParams: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CopyCat Replicator';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
