unit server_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, rtcFunction, rtcConn, rtcDataSrv,
  rtcHttpSrv, rtcInfo, rtcSrvModule, CcProviders, CcTransports, CcRTCTransport,
  Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Data.DB, CcMemDS, CcDB;

type
  TForm8 = class(TForm)
    CcRtcServerGateway1: TCcRtcServerGateway;
    CcRtcServerConnection1: TCcRtcServerConnection;
    RtcServerModule1: TRtcServerModule;
    RtcHttpServer1: TRtcHttpServer;
    RtcFunctionGroup1: TRtcFunctionGroup;
    Button1: TButton;
    CcDataSet1: TCcDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Conn: TLabel;
    ListBox1: TListBox;
    btStartServer: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btStartServerClick(Sender: TObject);
    procedure CcRtcServerGateway1NodeConnect(Sender: TCcRtcServerGateway;
      nodeName: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure RefreshConnectedNodes;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.btStartServerClick(Sender: TObject);
begin
  CcRtcServerGateway1.StartServer;
end;

procedure TForm8.Button1Click(Sender: TObject);
begin
  CcRtcServerConnection1.Connect;
  CcDataSet1.Open;
end;

procedure TForm8.CcRtcServerGateway1NodeConnect(Sender: TCcRtcServerGateway;
  nodeName: string);
begin
  TThread.Synchronize(TThread.CurrentThread, RefreshConnectedNodes);
end;

procedure TForm8.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CcRtcServerGateway1.StopServer;
end;

procedure TForm8.RefreshConnectedNodes;
var
  cNodeName: String;
begin
  ListBox1.Items.Clear;
  for cNodeName in CcRtcServerGateway1.ConnectedNodes.Keys do begin
    ListBox1.Items.Add(cNodeName);
  end;
end;

end.
