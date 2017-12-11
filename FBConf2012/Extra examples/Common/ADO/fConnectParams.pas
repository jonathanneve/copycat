unit fConnectParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CcConnADO, CcProviders;

type
  TfrConnectParams = class(TFrame)
    Label11: TLabel;
    Label12: TLabel;
    Label1: TLabel;
    memMSConnectionStr: TMemo;
    cbVersions: TComboBox;
    cbDatabaseTypes: TComboBox;
    Connection: TCcConnectionADO;
    procedure cbDatabaseTypesChange(Sender: TObject);
  public
    { Public declarations }
    procedure Init;
    procedure SetConnectParams;
  end;

implementation

{$R *.DFM}

procedure TfrConnectParams.Init;
begin
  Connection.DatabaseTypes( cbDatabaseTypes.Items);
end;

procedure TfrConnectParams.SetConnectParams;
begin
  with Connection do begin
    ConnectionString := memMSConnectionStr.Text;
    DBType := cbDatabaseTypes.Text;
    DBVersion := cbVersions.Text;
  end;
end;

procedure TfrConnectParams.cbDatabaseTypesChange(Sender: TObject);
begin
  Connection.DBType := cbDatabaseTypes.Text;
  if Connection.DBAdaptor <> nil then
    cbVersions.Items.Assign(Connection.DBAdaptor.SupportedVersions);
end;

end.
