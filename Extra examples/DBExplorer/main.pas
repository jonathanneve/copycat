unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CcProviders, CcTransports, CcXMLRPCTransport, DB,
  CcMemDS, CcDB, Grids, DBGridEh;

type
  TForm1 = class(TForm)
    CcXmlRpcClientTransport1: TCcXmlRpcClientTransport;
    Button1: TButton;
    DBGridEh1: TDBGridEh;
    Memo1: TMemo;
    CcDataSet1: TCcDataSet;
    DataSource1: TDataSource;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CcDataSet1.Close;
  CcDataSet1.SQL.Assign(Memo1.Lines);
  CcDataSet1.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if CcXmlRpcClientTransport1.Connected then
    CcXmlRpcClientTransport1.Disconnect
  else
    CcXmlRpcClientTransport1.Connect;
end;

end.
