program UnitTests;

uses
  EMemLeaks,
  Vcl.Forms,
  DUnitTestRunner,
  dtConnector in 'dtConnector.pas' {dmtConnector: TDataModule},
  CctConnection in 'CctConnection.pas',
  CctConnectors in 'CctConnectors.pas',
  CctReplicator in 'CctReplicator.pas',
  dtConnectorADO in 'dtConnectorADO.pas' {dmtConnectorADO: TDataModule},
  dtconnectorFireDAC in 'dtconnectorFireDAC.pas' {dmtConnectorFireDAC: TDataModule};

//  dtConnectorNexusDB in 'dtConnectorNexusDB.pas' {dmtConnectorNexusDB: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;
end.
