unit fConnectParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcProviders, CcInterbaseConn, DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Phys.IB, FireDAC.Phys.PG, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  FireDAC.Phys.ADS, FireDAC.Phys.ASA, FireDAC.Phys.MySQL, FireDAC.Phys.MSAcc,
  FireDAC.Phys.ODBC, FireDAC.Phys.MSSQL, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.DB2, FireDAC.Phys.Oracle, FireDAC.Comp.UI, FireDAC.Comp.Client,
  CcProvFireDAC;

type
  TfrConnectParams = class(TFrame)
    Label10: TLabel;
    cbVersions: TComboBox;
    Connection: TCcConnectionFireDAC;
    FDConnection: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    FDPhysODBCDriverLink1: TFDPhysODBCDriverLink;
    FDPhysMSAccessDriverLink1: TFDPhysMSAccessDriverLink;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysASADriverLink1: TFDPhysASADriverLink;
    FDPhysADSDriverLink1: TFDPhysADSDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    cbDBType: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    FDTransaction1: TFDTransaction;
    procedure cbDBTypeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  public
    procedure Init;
    procedure SetConnectParams;
  end;

implementation

uses FireDAC.VCLUI.ConnEdit;

{$R *.DFM}

procedure TfrConnectParams.Button1Click(Sender: TObject);
begin
  TfrmFDGUIxFormsConnEdit.Execute(FDConnection, '');
end;

procedure TfrConnectParams.cbDBTypeChange(Sender: TObject);
begin
  Connection.DBType := cbDBType.Text;
  if Connection.DBAdaptor <> nil then
    cbVersions.Items.Assign(Connection.DBAdaptor.SupportedVersions);
end;

procedure TfrConnectParams.Init;
begin
  Connection.DatabaseTypes( cbDBType.Items);
end;

procedure TfrConnectParams.SetConnectParams;
begin
  Connection.DBVersion := cbVersions.Text;
end;

end.
