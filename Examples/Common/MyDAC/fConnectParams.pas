unit fConnectParams;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcProvFIBPlus, CcProviders, CcInterbaseConn, DB,
  DBAccess, MyAccess, CcProvMyDAC, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  CcProvFireDAC, FireDAC.Phys.FB;

type
  TfrConnectParams = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label10: TLabel;
    edDBName: TEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    edPort: TEdit;
    cbVersions: TComboBox;
    edServer: TEdit;
    Label5: TLabel;
    FDTransaction1: TFDTransaction;
    Connection: TCcConnectionFireDAC;
    FDConnection1: TFDConnection;
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
  with Connection.FDConnection do begin
    Params.Values['Database'] := Trim(edDBName.Text);
    Params.Values['User_name'] := Trim(edUserName.Text);
    Params.Values['Password'] := Trim(edPassword.Text);
  end;
  Connection.DBVersion := cbVersions.Text;
end;

end.
