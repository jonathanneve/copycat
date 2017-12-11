unit fConnectParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcProvFIBPlus, CcProviders, CcInterbaseConn, DB,
  DBAccess, MyAccess, CcProvMyDAC, CcProvZeos;

type
  TfrConnectParams = class(TFrame)
    Label10: TLabel;
    cbVersions: TComboBox;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    edDBName: TEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    edHostName: TEdit;
    OpenDialog: TOpenDialog;
    CcConnectionZeos1: TCcConnectionZeos;
    cbDBType: TComboBox;
    Label5: TLabel;
    cbCatalog: TComboBox;
    Label6: TLabel;
    cbProtocol: TComboBox;
    Label7: TLabel;
    Edit1: TEdit;
  public
    procedure Init;
    procedure SetConnectParams;
  end;

implementation

{$R *.DFM}

procedure TfrConnectParams.Init;
begin
  cbVersions.Items.Assign(Connection.DBAdaptor.SupportedVersions);
end;

procedure TfrConnectParams.SetConnectParams;
begin
  with Connection.MyConnection do begin
    Database := Trim(edDBName.Text);
    Server := Trim(edServer.Text);
    Username := Trim(edUserName.Text);
    Password := Trim(edPassword.Text);
    Port := StrToIntDef(edPort.Text, 3306);
  end;
  Connection.DBVersion := cbVersions.Text;
end;

end.
