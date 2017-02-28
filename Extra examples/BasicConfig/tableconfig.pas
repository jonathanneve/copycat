unit tableconfig;

interface

{$I CC.INC}

uses
  CcKeys,
  Windows, Messages, SysUtils, {$IFDEF CC_D6} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, CcProviders, DB, CcMemDS, StdCtrls, DBCtrls, Buttons, ExtCtrls,
  Grids, DBGrids, Mask, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TfmTables = class(TForm)
    Label1: TLabel;
    Label9: TLabel;
    DBEdit9: TDBEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    btGenerate: TBitBtn;
    DBText1: TDBText;
    Label20: TLabel;
    Label6: TLabel;
    mPrimaryKeys: TCcMemoryData;
    mPrimaryKeysName: TStringField;
    mPrimaryKeysSyncSQL: TStringField;
    mPrimaryKeysDS: TDataSource;
    DBGrid2: TDBGrid;
    mUniqueKeys: TCcMemoryData;
    mUniqueKeysName: TStringField;
    mUniqueKeysSyncSQL: TStringField;
    mUniqueKeysDS: TDataSource;
    DBGrid1: TDBGrid;
    BitBtn2: TBitBtn;
    GroupBox3: TGroupBox;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBCheckBox3: TDBCheckBox;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    cbFilterField: TDBComboBox;
    Label3: TLabel;
    DBMemo1: TDBMemo;
    qGetFields: TCcQuery;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DBGrid1EditButtonClick(Sender: TObject);
    procedure DBGrid2EditButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btGenerateClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    KeyRing: TCcKeyRing;
    procedure EditSyncSQL(mTable: TCcMemoryData);
  public
    { Public declarations }
  end;

var
  fmTables: TfmTables;

implementation

uses main, genproc;

{$R *.dfm}

procedure TfmTables.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then begin
    if MainForm.RPLTables.State <> dsBrowse then
      MainForm.RPLTables.Cancel;
  end;
end;

procedure TfmTables.EditSyncSQL(mTable: TCcMemoryData);
begin
  with TfmGenProc.Create(Self) do try
    cPKName := mTable.FieldByName('Name').AsString;
    if ShowModal = mrOK then begin
      mTable.Edit;
      mTable.FieldByName('SyncSQL').AsString := cGenValue;
      mTable.Post;
    end;
  finally
    Free;
  end;
end;

procedure TfmTables.FormCreate(Sender: TObject);
begin
  mPrimaryKeys.Active := True;
  mUniqueKeys.Active := True;
  KeyRing := TCcKeyRing.Create(MainForm.Connection);
	KeyRing.LoadKeys(MainForm.RPLTablesTABLE_NAME.AsString, '', '', '', MainForm.RPLTablesPRIMARY_KEY_SYNC.AsString, MainForm.RPLTablesUNIQUE_KEY_NAMES.AsString, MainForm.RPLTablesUNIQUE_KEY_SYNC.AsString);
	KeyRing.SaveToDataSet(mPrimaryKeys, 'Name', 'SyncSQL', True);
	KeyRing.SaveToDataSet(mUniqueKeys, 'Name', 'SyncSQL', False);
end;

procedure TfmTables.FormDestroy(Sender: TObject);
begin
  KeyRing.Free;
end;

procedure TfmTables.DBGrid1EditButtonClick(Sender: TObject);
begin
  EditSyncSQL(mUniqueKeys);
end;

procedure TfmTables.DBGrid2EditButtonClick(Sender: TObject);
begin
  EditSyncSQL(mPrimaryKeys);
end;

procedure TfmTables.btGenerateClick(Sender: TObject);
var
  cRefField: String;
begin
	KeyRing.Clear;
	KeyRing.LoadFromDataSet(mPrimaryKeys, MainForm.RPLTablesTABLE_NAME.AsString, 'Name', 'SyncSQL');
	KeyRing.LoadFromDataSet(mUniqueKeys, MainForm.RPLTablesTABLE_NAME.AsString, 'Name', 'SyncSQL');
	KeyRing.SaveKeys;
	MainForm.RPLTables.Edit;
	MainForm.RPLTablesPRIMARY_KEY_SYNC.AsString := KeyRing.GenericPrimaryKeySync;
	MainForm.RPLTablesUNIQUE_KEY_SYNC.AsString := KeyRing.GenericUniqueKeySync;
	cRefField := MainForm.RPLTablesREF_FIELD.AsString;
	MainForm.RPLTablesUNIQUE_KEY_NAMES.AsString := KeyRing.UniqueKeyNames;
  MainForm.RPLTables.Post;
  MainForm.CcConfig.GenerateTriggers(MainForm.RPLTablesTABLE_NAME.AsString);
end;

procedure TfmTables.BitBtn2Click(Sender: TObject);
begin
  MainForm.CcConfig.RemoveTriggers(MainForm.RPLTablesTABLE_NAME.AsString);
end;

procedure TfmTables.FormShow(Sender: TObject);
var
	slList: TStringList;
begin
	slList := MainForm.Connection.ListTableFields(MainForm.RPLTablesTABLE_NAME.AsString);
	cbFilterField.Items.Assign(slList);
end;

end.
