unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CcConf, CcConfStorage, CcReplicator, StdCtrls, CcProviders,
  CcInterbaseConn, CcProvUIB, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB, CcMemDS, CcDB;

type
  TForm1 = class(TForm)
    CcReplicatorA: TCcReplicator;
    CcConfig: TCcConfig;
    Memo: TMemo;
    CcReplicatorB: TCcReplicator;
    HeadDB: TCcConnectionUIB;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Bra: TTabSheet;
    TabSheet3: TTabSheet;
    qHeadCust: TCcDataSet;
    qHeadCustDS: TDataSource;
    BranchADB: TCcConnectionUIB;
    BranchBDB: TCcConnectionUIB;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel7: TPanel;
    DBGrid1: TDBGrid;
    Panel8: TPanel;
    DBGrid7: TDBGrid;
    btn1: TButton;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    btn3: TButton;
    Panel5: TPanel;
    Splitter3: TSplitter;
    Panel6: TPanel;
    Panel9: TPanel;
    btn4: TButton;
    qHeadProd: TCcDataSet;
    qHeadProdDS: TDataSource;
    qHeadProdID: TIntegerField;
    qHeadProdNAME: TStringField;
    qHeadProdPRICE: TFloatField;
    qHeadCustID: TIntegerField;
    qHeadCustBRANCH: TStringField;
    qHeadCustNAME: TStringField;
    qBranchACust: TCcDataSet;
    qBranchACustDS: TDataSource;
    qBranchAProdDS: TDataSource;
    qBranchAProd: TCcDataSet;
    IntegerField2: TIntegerField;
    StringField7: TStringField;
    FloatField1: TFloatField;
    qBranchBCust: TCcDataSet;
    IntegerField3: TIntegerField;
    StringField8: TStringField;
    StringField9: TStringField;
    qBranchBCustDS: TDataSource;
    qBranchBProdDS: TDataSource;
    qBranchBProd: TCcDataSet;
    IntegerField4: TIntegerField;
    StringField14: TStringField;
    FloatField2: TFloatField;
    DBGrid3: TDBGrid;
    DBGrid5: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid4: TDBGrid;
    qBranchACustID: TIntegerField;
    qBranchACustBRANCH: TStringField;
    qBranchACustNAME: TStringField;
    btn2: TButton;
    btn5: TButton;
    procedure CcReplicatorARowReplicated(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure CcReplicatorAEmptyLog(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure CcReplicatorAException(Sender: TObject; e: Exception);
    procedure CcReplicatorAReplicationError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
    procedure btn5Click(Sender: TObject);
    procedure CcReplicatorAResolveConflict(Sender: TObject;
      var Conflict: TConflictRecord);
    procedure Button1Click(Sender: TObject);
    procedure qHeadCustAfterPost(DataSet: TDataSet);
    procedure FormShow(Sender: TObject);
  private
    procedure RefreshDataSets;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CcReplicatorARowReplicated(Sender: TObject);
begin
	Memo.Lines.Add('Replicated row from table ' + (Sender as TCcReplicator).Log.TableName);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  RefreshDataSets;
end;

procedure TForm1.qHeadCustAfterPost(DataSet: TDataSet);
begin
  (DataSet as TCcDataSet).Connection.CommitRetaining;
  RefreshDataSets;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  CcReplicatorA.Disconnect;
  CcReplicatorA.Connect;
	CcReplicatorA.Replicate;
  RefreshDataSets;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  CcConfig.Connection := HeadDB;
	CcConfig.Connect;
	CcConfig.GenerateTriggers('PRODUCTS');
	CcConfig.GenerateTriggers('CUSTOMERS');
	CcConfig.Disconnect;
end;

procedure TForm1.CcReplicatorAEmptyLog(Sender: TObject);
begin
	Memo.Lines.Add('Nothing to replicate');
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
	CcConfig.Connection := BranchADB;
	CcConfig.Connect;
	CcConfig.GenerateTriggers('CUSTOMERS');
	CcConfig.Disconnect;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
	CcConfig.Connection := BranchBDB;
	CcConfig.Connect;
	CcConfig.GenerateTriggers('CUSTOMERS');
	CcConfig.Disconnect;
end;

procedure TForm1.CcReplicatorAException(Sender: TObject; e: Exception);
begin
	Memo.Lines.Add('Error : ' + e.Message);
end;

procedure TForm1.CcReplicatorAReplicationError(Sender: TObject;
  e: Exception; var CanContinue: Boolean);
begin
	Memo.Lines.Add('Error : ' + e.Message);
	CanContinue := true;
end;

procedure TForm1.btn5Click(Sender: TObject);
begin
  CcReplicatorB.Disconnect;
  CcReplicatorB.Connect;
	CcReplicatorB.Replicate;
  RefreshDataSets;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RefreshDataSets;
end;

procedure TForm1.RefreshDataSets;
begin
  qHeadCust.Close;
  qHeadProd.Close;
  qBranchACust.Close;
  qBranchAProd.Close;
  qBranchBCust.Close;
  qBranchBProd.Close;
  HeadDB.Disconnect;
  BranchADB.Disconnect;
  BranchBDB.Disconnect;

  HeadDB.Connect;
  BranchADB.Connect;
  BranchBDB.Connect;
  qHeadCust.Open;
  qHeadProd.Open;
  qBranchACust.Open;
  qBranchAProd.Open;
  qBranchBCust.Open;
  qBranchBProd.Open;
end;

procedure TForm1.CcReplicatorAResolveConflict(Sender: TObject;
  var Conflict: TConflictRecord);
begin
  Memo.Lines.Add('Conflict detected!');
  Conflict.ChosenNode := 'BRANCH A';
end;

end.
