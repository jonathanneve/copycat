unit fConnectParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcProvIBX, CcProviders, CcInterbaseConn;

type
  TfrConnectParams = class(TFrame)
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    edDBName: TEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    cbDialect: TComboBox;
    edCharset: TEdit;
    cbVersions: TComboBox;
    OpenDialog: TOpenDialog;
    Connection: TCcConnectionIBX;
    procedure SpeedButton1Click(Sender: TObject);
  private
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
  with Connection do begin
    DBName := Trim(edDBName.Text);
    CharSet := Trim(edCharset.Text);
    UserLogin := Trim(edUserName.Text);
    UserPassword := Trim(edPassword.Text);
    DBVersion := cbVersions.Text;
    SQLDialect := StrToInt(cbDialect.Text);
  end;
end;

procedure TfrConnectParams.SpeedButton1Click(Sender: TObject);
begin
  if (OpenDialog.Execute) then
    edDBName.Text := OpenDialog.FileName;
end;

end.
