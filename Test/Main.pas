unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CcConfStorage, CcRplList, CcProviders, CcProvIBX, CcTest,
  CcTransports, CcXMLRPCTransport, CcConf, CcReplicator, CcXmlRpcServer,
  CcConnADO;


type
  TForm1 = class(TForm)
    Button1: TButton;
    lbOk: TLabel;
    lbConnectOk: TLabel;
    Button2: TButton;
    lbBlobOk: TLabel;
    OpenDialog1: TOpenDialog;
    edFile: TEdit;
    Button3: TButton;
    Button4: TButton;
    CcConnectionIBX1: TCcConnectionIBX;
    CcXmlRpcServer1: TCcXmlRpcServer;
    CcXmlRpcClientTransport1: TCcXmlRpcClientTransport;
    CcXmlRpcServerTransport1: TCcXmlRpcServerTransport;
    CcReplicator1: TCcReplicator;
    CcConnectionIBX2: TCcConnectionIBX;
    CcConnectionADO1: TCcConnectionADO;
    CcConfig1: TCcConfig;
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    test: TCcTest;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses CcSQLServer;

procedure TForm1.Button1Click(Sender: TObject);
begin
  test := TCcTest.Create(CcConnectionADO1);
//  test := TCcTest.Create(CcConnectionIBX1);
  test.TestConnection;
  lbConnectOk.Visible := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ss: TStringStream;
  fs: TFileStream;
begin
  fs := TFileStream.Create(edFile.Text, fmOpenRead);
  ss := TStringStream.Create('');
  try
    ss.CopyFrom(fs, fs.Size);
    test := TCcTest.Create(CcConnectionADO1);
    test.TestBlob(ss.DataString);
    lbBlobOk.Visible := True;
    lbOk.Visible := True;
  finally
    fs.Free;
    ss.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edFile.Text := OpenDialog1.FileName;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  test.TestReplication(CcConfig1, CcReplicator1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  CcConnectionADO1.DBType := 'Oracle';
end;

end.
