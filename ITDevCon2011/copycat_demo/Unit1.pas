unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CcConf, CcConfStorage, CcReplicator, StdCtrls, CcProviders,
  CcInterbaseConn, CcProvUIB, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB, CcMemDS, CcDB;

type
  TForm1 = class(TForm)
    Memo: TMemo;
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
    Button1: TButton;
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
    Button2: TButton;
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

procedure TForm1.FormShow(Sender: TObject);
begin
  RefreshDataSets;
end;

procedure TForm1.qHeadCustAfterPost(DataSet: TDataSet);
begin
  (DataSet as TCcDataSet).Connection.Commit;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RefreshDataSets;
end;

procedure TForm1.RefreshDataSets;
begin
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

end.
