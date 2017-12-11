unit fConnectParams;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcProvFIBPlus, CcProviders, CcInterbaseConn, DB,
  DBAccess, MyAccess, CcProvMyDAC;

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
    Connection: TCcConnectionMyDAC;
    MyConnection1: TMyConnection;
    edServer: TEdit;
    Label5: TLabel;
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
