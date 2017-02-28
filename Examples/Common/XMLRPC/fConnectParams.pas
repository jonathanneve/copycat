unit fConnectParams;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CcProvFIBPlus, CcProviders, CcTransports,
  CcXMLRPCTransport;

type
  TfrConnectParams = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edDBAlias: TEdit;
    edKeepAliveInterval: TEdit;
    edPassword: TEdit;
    edServerAddress: TEdit;
    edServerPort: TEdit;
    Connection: TCcXmlRpcClientTransport;
  public
    procedure Init;
    procedure SetConnectParams;
  end;

implementation

{$R *.DFM}

procedure TfrConnectParams.Init;
begin
//  Connection := Conn;
//  cbVersions.Items.Assign(Connection.DBAdaptor.SupportedVersions);
end;

procedure TfrConnectParams.SetConnectParams;
begin
  with Connection do begin
    DatabaseAlias := Trim(edDBAlias.Text);
    KeepAliveInterval := StrToInt(edKeepAliveInterval.Text);
    Password := edPassword.Text;
    HostName := edServerAddress.Text;
    HostPort := StrToInt(edServerPort.Text);
  end;
end;

end.
