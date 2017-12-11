unit fConnectParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcRTCTransport, rtcDataCli, rtcCliModule, rtcInfo,
  rtcConn, rtcHttpCli, CcProviders, CcTransports;

type
  TfrConnectParams = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edDBAlias: TEdit;
    edKeepAliveInterval: TEdit;
    edPassword: TEdit;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edSecureKey: TEdit;
    edEncryptionKey: TEdit;
    edModuleFileName: TEdit;
    edModuleHost: TEdit;
    cbCompression: TComboBox;
    Label9: TLabel;
    edServerAddress: TEdit;
    edServerPort: TEdit;
    Label10: TLabel;
    Connection: TCcRtcClientTransport;
    HttpClient: TRtcHttpClient;
    ClientModule: TRtcClientModule;
  private
  public
    procedure Init;
    procedure SetConnectParams;
  end;

implementation

{$R *.DFM}

procedure TfrConnectParams.Init;
begin

end;

procedure TfrConnectParams.SetConnectParams;
begin
  with Connection do begin
    DatabaseAlias := Trim(edDBAlias.Text);
    KeepAliveInterval := StrToInt(edKeepAliveInterval.Text);
    Password := edPassword.Text;
  end;
  ClientModule.Compression := TRtcCompressLevel(cbCompression.ItemIndex);
  ClientModule.SecureKey := edSecureKey.Text;
  ClientModule.EncryptionKey := StrToIntDef(edEncryptionKey.Text, 0);
  ClientModule.ModuleHost := edModuleHost.Text;
  ClientModule.ModuleFileName := edModuleFileName.Text;
  HttpClient.ServerAddr := edServerAddress.Text;
  HttpClient.ServerPort := edServerPort.Text;
end;

end.
