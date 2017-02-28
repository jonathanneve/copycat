unit TrnSrvUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CcTransports, CcRTCTransport, StdCtrls, Buttons, fConnectParams,
  rtcFunction, rtcDataSrv, rtcSrvModule, rtcInfo, rtcConn, rtcHttpSrv,
  ComCtrls, fServerOptions, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  CcProviders, Data.DB;

type
  TForm1 = class(TForm)
    PageControl: TPageControl;
    tsDBConnection: TTabSheet;
    tsServerOptions: TTabSheet;
    frServerOptions: TfrServerOptions;
    lbConnected: TLabel;
    btConnect: TBitBtn;
    frConnectParams: TfrConnectParams;
    procedure btConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.btConnectClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if not frServerOptions.ServerRunning then begin
      frConnectParams.SetConnectParams;
      frServerOptions.StartServer(frConnectParams.Connection);
      btConnect.Caption := 'Stop server';
      lbConnected.Visible := True;
    end else begin
      frServerOptions.StopServer;
      btConnect.Caption := 'Start server';
      lbConnected.Visible := False;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  frConnectParams.Init;
  PageControl.ActivePage := tsDBConnection;
end;

end.
