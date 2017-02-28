unit Unit6;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DBXDataSnap, IPPeerClient,
  Data.DBXCommon, Data.DB, Data.SqlExpr, CcProviders, CcTransports,
  CcDataSnapTransport, Vcl.StdCtrls, CcConfStorage, CcConf, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.Comp.UI,
  FireDAC.Comp.Client, CcProvFireDAC, CcReplicator;

type
  TForm6 = class(TForm)
    CcDSClientTransport1: TCcDSClientTransport;
    SQLConnection1: TSQLConnection;
    Button1: TButton;
    CcConfig: TCcConfig;
    CcConnectionFireDAC1: TCcConnectionFireDAC;
    FDConnection1: TFDConnection;
    FDTransaction1: TFDTransaction;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    CcConfig2: TCcConfig;
    Button2: TButton;
    CcReplicator: TCcReplicator;
    memLog: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CcReplicatorRowReplicated(Sender: TObject; TableName: string;
      Fields: TFields);
    procedure CcReplicatorEmptyLog(Sender: TObject);
    procedure CcReplicatorException(Sender: TObject; e: Exception);
    procedure CcReplicatorReplicationError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
  CcConfig.Connect;
  CcConfig.GenerateConfig;
  CcConfig.Disconnect;

  CcConfig2.Connect;
  CcConfig2.GenerateConfig;
  CcConfig2.Disconnect;
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  CcReplicator.Replicate;
end;

procedure TForm6.CcReplicatorEmptyLog(Sender: TObject);
begin
  memLog.Lines.Add('Empty log');
end;

procedure TForm6.CcReplicatorException(Sender: TObject; e: Exception);
begin
  memLog.Lines.Add(e.Message);
end;

procedure TForm6.CcReplicatorReplicationError(Sender: TObject; e: Exception;
  var CanContinue: Boolean);
begin
  memLog.Lines.Add(e.Message);
end;

procedure TForm6.CcReplicatorRowReplicated(Sender: TObject; TableName: string;
  Fields: TFields);
begin
  memLog.Lines.Add(TableName + ': From ' + CcReplicator.Log.Origin.Name);
end;

end.
