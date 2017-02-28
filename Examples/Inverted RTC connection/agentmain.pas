unit agentmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Comp.Client, Data.DB, CcProviders, CcProvFireDAC, rtcConn, rtcDataCli,
  rtcHttpCli, rtcInfo, rtcCliModule, CcTransports, CcRTCTransport, rtcFunction,
  Vcl.StdCtrls;

type
  TForm7 = class(TForm)
    CcRtcClientGateway1: TCcRtcClientGateway;
    RtcClientModule1: TRtcClientModule;
    RtcHttpClient1: TRtcHttpClient;
    CcConnectionFireDAC1: TCcConnectionFireDAC;
    FDConnection1: TFDConnection;
    FDTransaction1: TFDTransaction;
    RtcFunctionGroup1: TRtcFunctionGroup;
    procedure FormCreate(Sender: TObject);
    procedure RtcHttpClient1Reconnect(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

procedure TForm7.FormCreate(Sender: TObject);
begin
  CcRtcClientGateway1.Connect;
end;

procedure TForm7.RtcHttpClient1Reconnect(Sender: TRtcConnection);
begin
  CcRtcClientGateway1.Connect;
end;

end.
