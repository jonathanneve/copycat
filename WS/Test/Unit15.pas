unit Unit15;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CcWSTransport, Vcl.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.Comp.UI, FireDAC.Phys.IBBase, Data.DB, CcProviders, CcProvFireDAC,
  Vcl.Grids, Vcl.DBGrids, CcMemDS, CcDB;

type
  TForm15 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CcConnection: TCcConnectionFireDAC;
    FDConnection1: TFDConnection;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDTransaction1: TFDTransaction;
    CcDataSet: TCcDataSet;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    FServer :TCcWSServerTransport;
    FClient :TCcWSClientTransport;
    FGateway: TCcWSGateway;
    FAgent: TCcWSAgentConnection;
  public
    { Public declarations }
  end;

var
  Form15: TForm15;

implementation

{$R *.dfm}

procedure TForm15.Button1Click(Sender: TObject);
begin
  FServer.StartServer;
end;

procedure TForm15.Button2Click(Sender: TObject);
begin
  FClient.Connect;
end;

procedure TForm15.Button3Click(Sender: TObject);
begin
  CcDataSet.Connection := FClient;
  CcDataSet.Open;
end;

procedure TForm15.Button4Click(Sender: TObject);
begin
  FGateway.StartServer;
end;

procedure TForm15.Button7Click(Sender: TObject);
begin
  FAgent.Connect;
end;

procedure TForm15.FormCreate(Sender: TObject);
begin
{  FServer := TCcWSServerTransport.Create(Self);
  FServer.Port := 9093;
  FServer.DatabaseAlias := 'Test';
  FServer.Connection := CcConnection;}

  FClient := TCcWSClientTransport.Create(Self);
  FClient.Host := 'localhost';
  FClient.Port := 9093;
  FClient.DatabaseAlias := 'Test2';
  FClient.RequestTimeout := 10000;

{  FGateway := TCcWSGateway.Create(Self);
  FGateway.Port := 9093;}

  FAgent := TCcWSAgentConnection.Create(Self);
  FAgent.DatabaseAlias := 'Test2';
  FAgent.Connection := CcConnection;
  FAgent.Host := 'localhost';
  FAgent.Port := 9093;
end;

end.
