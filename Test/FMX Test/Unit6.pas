unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.DBXDataSnap, IPPeerClient, Data.DBXCommon, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  CcReplicator, FireDAC.Comp.UI, FireDAC.Comp.Client,
  CcProvFireDAC, CcConfStorage, CcConf, Data.DB, Data.SqlExpr, CcProviders,
  CcTransports, CcDataSnapTransport, FMX.Layouts, FMX.Memo, FMX.StdCtrls,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLite, FireDAC.FMXUI.Wait,
  CcProvFDSQLite, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, System.Rtti, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Fmx.Bind.Navigator, Data.Bind.Grid, FireDAC.Comp.DataSet, Data.Bind.DBScope,
  FMX.Grid;

type
  TForm6 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    memLog: TMemo;
    CcDSClientTransport1: TCcDSClientTransport;
    SQLConnection1: TSQLConnection;
    CcConfig: TCcConfig;
    FDConnection1: TFDConnection;
    FDTransaction1: TFDTransaction;
    CcConfig2: TCcConfig;
    CcReplicator: TCcReplicator;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    CcConnectionFDSQLite1: TCcConnectionFDSQLite;
    qInsert: TCcQuery;
    Grid1: TGrid;
    BindSourceTEST: TBindSourceDB;
    FDTableTEST: TFDTable;
    LinkGridToDataSourceBindSourceTEST: TLinkGridToDataSource;
    NavigatorBindSourceTEST: TBindNavigator;
    BindingsList1: TBindingsList;
    Button3: TButton;
    Button4: TButton;
    qUpdate: TCcQuery;
    FDMemTable1: TFDMemTable;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CcReplicatorEmptyLog(Sender: TObject);
    procedure CcReplicatorException(Sender: TObject; e: Exception);
    procedure CcReplicatorReplicationError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
    procedure CcReplicatorRowReplicated(Sender: TObject; TableName: string;
      Fields: TFields);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

uses System.IOUtils;

{$R *.fmx}

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

procedure TForm6.Button3Click(Sender: TObject);
begin
  CcConnectionFDSQLite1.Connect;
//  qUpdate.Exec;
//  CcConnectionFDSQLite1.CommitRetaining;
  FDTableTEST.Active := False;
  FDTableTEST.Active := True;
end;

procedure TForm6.Button4Click(Sender: TObject);
begin
  CcConnectionFDSQLite1.Disconnect;
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

procedure TForm6.FDConnection1BeforeConnect(Sender: TObject);
begin
  FDConnection1.Params.Values['Database'] := TPath.Combine(TPath.GetDocumentsPath, 'SQLiteTest.db');
end;

end.
