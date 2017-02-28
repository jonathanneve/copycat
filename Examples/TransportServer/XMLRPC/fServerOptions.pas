unit fServerOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, CcProviders, CcTransports, CcRTCTransport, rtcFunction, rtcDataSrv,
  rtcSrvModule, rtcInfo, rtcConn, rtcHttpSrv, CcXmlRpcServer,
  CcXMLRPCTransport;

type
  TfrServerOptions = class(TFrame)
    ListenPortLabel: TLabel;
    ListenPortEdit: TEdit;
    ServerTransport: TCcXmlRpcServerTransport;
    XmlRpcServer: TCcXmlRpcServer;
    Label1: TLabel;
    edPassword: TEdit;
    Label2: TLabel;
    edDBAlias: TEdit;
    Label3: TLabel;
    edSessionTimeout: TEdit;
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
  XmlRpcServer.ListenPort := StrToInt(ListenPortEdit.Text);
  ServerTransport.StartServer;
  ServerRunning := True;
end;

procedure TfrServerOptions.StopServer;
begin
  XmlRpcServer.Active := False;
  ServerRunning := False;
end;

end.
