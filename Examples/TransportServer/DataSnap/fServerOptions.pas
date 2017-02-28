unit fServerOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, CcTransports, CcRTCTransport, rtcFunction, rtcDataSrv,
  rtcSrvModule, rtcInfo, rtcConn, rtcHttpSrv, CcProviders;

type
  TfrServerOptions = class(TFrame)
    RtcHttpServer1: TRtcHttpServer;
    RtcServerModule1: TRtcServerModule;
    RtcFunctionGroup1: TRtcFunctionGroup;
    ServerTransport: TCcRtcServerTransport;
    Label1: TLabel;
    edPassword: TEdit;
    Label2: TLabel;
    edDBAlias: TEdit;
    Label3: TLabel;
    edSessionTimeout: TEdit;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    edSecureKey: TEdit;
    Label5: TLabel;
    edEncryptionKey: TEdit;
    Label6: TLabel;
    edModuleFileName: TEdit;
    Label7: TLabel;
    edModuleHost: TEdit;
    Label8: TLabel;
    cbCompression: TComboBox;
    ListenPortLabel: TLabel;
    ListenPortEdit: TEdit;
  private
    { Private declarations }
  public
    ServerRunning: Boolean;
    procedure StartServer(Connection: TCcConnection);
    procedure StopServer;
  end;

implementation

{$R *.dfm}



{ TfrServerOptions }

procedure TfrServerOptions.StartServer(Connection: TCcConnection);
begin
  ServerTransport.Connection := Connection;
  ServerTransport.DatabaseAlias := Trim(edDBAlias.Text);
  ServerTransport.SessionTimeout := StrToInt(edSessionTimeout.Text);
  ServerTransport.Password := edPassword.Text;

  RtcServerModule1.Compression := TRtcCompressLevel(cbCompression.ItemIndex);
  RtcServerModule1.SecureKey := edSecureKey.Text;
  RtcServerModule1.EncryptionKey := StrToIntDef(edEncryptionKey.Text, 0);
  RtcServerModule1.ModuleHost := edModuleHost.Text;
  RtcServerModule1.ModuleFileName := edModuleFileName.Text;
  RtcHttpServer1.ServerPort := ListenPortEdit.Text;

//  RtcHttpServer1.Listen;
  ServerTransport.StartServer;
  ServerRunning := True;
end;

procedure TfrServerOptions.StopServer;
begin
//  RtcHttpServer1.StopListen;
  ServerTransport.StopServer;
  ServerRunning := False;
end;

end.
