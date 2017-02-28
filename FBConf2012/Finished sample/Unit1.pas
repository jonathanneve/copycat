unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CcConf, CcConfStorage, CcReplicator, StdCtrls, CcProviders,
	CcInterbaseConn, ComCtrls, ExtCtrls, Grids, DBGrids,
	DB, CcMemDS, CcDB, FIBDataSet, pFIBDataSet, FIBDatabase, pFIBDatabase,
	CcProvFIBPlus, FIBQuery;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Bra: TTabSheet;
    TabSheet3: TTabSheet;
    qHeadCustDS: TDataSource;
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
    qHeadProdDS: TDataSource;
    qBranchACustDS: TDataSource;
		qBranchAProdDS: TDataSource;
    qBranchBCustDS: TDataSource;
    qBranchBProdDS: TDataSource;
    DBGrid3: TDBGrid;
    DBGrid5: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid4: TDBGrid;
    btn2: TButton;
    btn5: TButton;
		Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel10: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    HeadDB: TCcConnectionFIB;
    BranchADB: TCcConnectionFIB;
    BranchBDB: TCcConnectionFIB;
    FIBHeadDB: TpFIBDatabase;
    FIBHeadTR: TpFIBTransaction;
    qHeadCust: TpFIBDataSet;
    qHeadProd: TpFIBDataSet;
    qHeadCustID: TFIBIntegerField;
    qHeadCustBRANCH: TFIBStringField;
    qHeadCustNAME: TFIBStringField;
    FIBBranchADB: TpFIBDatabase;
    FIBBranchATR: TpFIBTransaction;
    FIBBranchBTR: TpFIBTransaction;
    FIBBranchBDB: TpFIBDatabase;
		qBranchACust: TpFIBDataSet;
    FIBIntegerField1: TFIBIntegerField;
    FIBStringField1: TFIBStringField;
    FIBStringField2: TFIBStringField;
    qBranchBCust: TpFIBDataSet;
    FIBIntegerField2: TFIBIntegerField;
    FIBStringField3: TFIBStringField;
    FIBStringField4: TFIBStringField;
    qBranchAProd: TpFIBDataSet;
		qBranchBProd: TpFIBDataSet;
    CcReplicatorA: TCcReplicator;
    CcReplicatorB: TCcReplicator;
    qHeadCustSYNC_DATE: TFIBDateTimeField;
    qBranchACustSYNC_DATE: TFIBDateTimeField;
    qBranchBCustSYNC_DATE: TFIBDateTimeField;
    qUpdateSyncDate: TCcQuery;
    CcConfigHead: TCcConfig;
    CcConfigA: TCcConfig;
    CcConfigB: TCcConfig;
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
    procedure FormShow(Sender: TObject);
    procedure FIBHeadTRAfterSQLExecute(Query: TFIBQuery;
      SQLType: TFIBSQLTypes);
    procedure CcReplicatorARowReplicating(Sender: TObject;
      TableName: String; Fields: TFields; var ReplicateRow,
      AbortAndTryLater: Boolean);
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

procedure TForm1.RefreshDataSets;
begin
	qHeadCust.Close;
	qHeadProd.Close;
	qBranchACust.Close;
	qBranchAProd.Close;
	qBranchBCust.Close;
	qBranchBProd.Close;

	FIBHeadDB.Connected := false;
	FIBHeadDB.Connected := true;
	FIBBranchADB.Connected := false;
	FIBBranchADB.Connected := true;
	FIBBranchBDB.Connected := false;
	FIBBranchBDB.Connected := true;

	qHeadCust.Open;
	qHeadProd.Open;
	qBranchACust.Open;
	qBranchAProd.Open;
	qBranchBCust.Open;
	qBranchBProd.Open;
end;

procedure TForm1.FIBHeadTRAfterSQLExecute(Query: TFIBQuery;
	SQLType: TFIBSQLTypes);
begin
	Query.Transaction.CommitRetaining;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
	RefreshDataSets;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
	CcConfigHead.Connect;
	CcConfigHead.GenerateConfig;
	CcConfigHead.Disconnect;
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
	CcConfigA.Connect;
	CcConfigA.GenerateConfig;
	CcConfigA.Disconnect;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
	CcConfigB.Connect;
	CcConfigB.GenerateConfig;
	CcConfigB.Disconnect;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
	CcReplicatorA.Replicate;
	RefreshDataSets;
end;

procedure TForm1.btn5Click(Sender: TObject);
begin
	CcReplicatorB.Replicate;
	RefreshDataSets;
end;

procedure TForm1.CcReplicatorAEmptyLog(Sender: TObject);
begin
	Memo.Lines.Add('Nothing to replicate');
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

procedure TForm1.CcReplicatorAResolveConflict(Sender: TObject;
	var Conflict: TConflictRecord);
begin
{	Memo.Lines.Add('Conflict detected!');
	Conflict.ChosenNode := 'BRANCHA';}
end;

procedure TForm1.CcReplicatorARowReplicating(Sender: TObject;
	TableName: String; Fields: TFields; var ReplicateRow,
	AbortAndTryLater: Boolean);
var
	i:Integer;
	cLine: String;
begin
	cLine := 'Replicating row from table ' + TableName + ' :';
	for I := 0 to CcReplicatorA.Log.Keys.Count -1 do begin
		cLine := cLine + ' ' + CcReplicatorA.Log.Keys[i].KeyName + ' = ' + String(CcReplicatorA.Log.Keys[i].KeyValue);
	end;
	Memo.Lines.Add(cLine);

	if (TableName = 'CUSTOMERS') then begin

		Fields.DataSet.Edit;
		Fields.FieldByName('SYNC_DATE').AsDateTime := Now;
		Fields.DataSet.Post;

		qUpdateSyncDate.Close;
		qUpdateSyncDate.Connection := CcReplicatorA.Log.Origin.Connection;
		qUpdateSyncDate.Param['SYNC_DATE'].Value := Now;
		qUpdateSyncDate.Param['ID'].Value := CcReplicatorA.Log.Keys.FindKey('ID').KeyValue;
		qUpdateSyncDate.Param['BRANCH'].Value := CcReplicatorA.Log.Keys.FindKey('BRANCH').KeyValue;
		qUpdateSyncDate.Exec;
	end;
end;

end.
