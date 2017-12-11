unit genproc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CcConf, Db, CcProviders, CcMemDS, CcDB, StdCtrls, Grids, DBGrids,
  Buttons, ExtCtrls;

type
  TfmGenProc = class(TForm)
    Panel1: TPanel;
    btOK: TBitBtn;
    btAnnuler: TBitBtn;
    Panel2: TPanel;
    Label4: TLabel;
    lbPKName: TLabel;
    Label5: TLabel;
    lbTableName: TLabel;
    cbRien: TRadioButton;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edProcedureName: TComboBox;
    dbgParams: TDBGrid;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    edGeneratorName: TComboBox;
    edGenIncrement: TEdit;
    edOutputParam: TComboBox;
    InParamsDS: TDataSource;
    InParams: TCcProcParams;
    OutParams: TCcProcParams;
    cbGen: TRadioButton;
    cbProc: TRadioButton;
    procedure FormShow(Sender: TObject);
		procedure edProcedureNameChange(Sender: TObject);
    procedure GroupBox3Enter(Sender: TObject);
    procedure GroupBox2Enter(Sender: TObject);
    procedure btOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
		cPKName: String;
    cGenValue:String;
  end;

var
  fmGenProc: TfmGenProc;

implementation

uses
  main;

{$R *.dfm}

procedure TfmGenProc.FormShow(Sender: TObject);
begin
  lbPKName.Caption := cPKName;
	lbTableName.Caption := MainForm.RPLTablesTABLE_NAME.AsString;
	edProcedureName.Items.Assign(MainForm.Connection.ListAllProcedures);
	edGeneratorName.Items.Assign(MainForm.Connection.ListGenerators);
	dbgParams.Columns[1].PickList.Assign(MainForm.Connection.ListTableFields(MainForm.RPLTablesTABLE_NAME.AsString));
end;

procedure TfmGenProc.edProcedureNameChange(Sender: TObject);
begin
  MainForm.CcConfig.GetProcParams(Trim(edProcedureName.Text), OutParams, False);
  edOutputParam.Items.Clear;
  OutParams.First;
  while not OutParams.Eof do begin
    edOutputParam.Items.Add(OutParams.FieldByName('PARAM_NAME').AsString);
		OutParams.Next;
    end;

  MainForm.CcConfig.GetProcParams(Trim(edProcedureName.Text), InParams, True);
  InParams.First;
end;

procedure TfmGenProc.GroupBox3Enter(Sender: TObject);
begin
  cbProc.Checked := True;
end;

procedure TfmGenProc.GroupBox2Enter(Sender: TObject);
begin
  cbGen.Checked := True;
end;

procedure TfmGenProc.btOKClick(Sender: TObject);
begin
  if (cbProc.Checked) then
    cGenValue := MainForm.CcConfig.GetProcGenerator(MainForm.RPLTablesTABLE_NAME.AsString, edProcedureName.Text, InParams, Trim(edOutputParam.Text))
  else if (cbGen.Checked) then
    cGenValue := MainForm.CcConfig.GetGenerator(edGeneratorName.Text, StrToInt(edGenIncrement.Text))
  else
    cGenValue := '';
end;

end.
