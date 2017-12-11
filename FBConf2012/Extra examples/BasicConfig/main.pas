unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, CcMemDS, CcDB, ImgList, CcConfStorage, CcConf, Buttons, Grids,
  DBGrids, ExtCtrls, fConnectParams, StdCtrls, ComCtrls, CcProviders,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TMainForm = class(TForm)
    CcConfig: TCcConfig;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    tsTables: TTabSheet;
    dbgTables: TDBGrid;
    btConnect: TBitBtn;
    lbConnected: TLabel;
    Label6: TLabel;
    ImageList1: TImageList;
    Panel1: TPanel;
    Label7: TLabel;
    RPLTablesDS: TDataSource;
    tsSubNodes: TTabSheet;
    Panel2: TPanel;
    Label8: TLabel;
    dbgNodes: TDBGrid;
    RPLUsers: TCcDataSet;
    RPLUsersDS: TDataSource;
    Panel3: TPanel;
    btAddNode: TSpeedButton;
    btEditNode: TSpeedButton;
    btDeleteNode: TSpeedButton;
    RPLUsersLOGIN: TStringField;
    RPLUsersLIBELLE: TStringField;
    RPLTables: TCcDataSet;
    RPLTablesTABLE_NAME: TStringField;
    RPLTablesCREATED: TStringField;
    RPLTablesREF_FIELD: TStringField;
    RPLTablesUNIQUE_KEY_SYNC: TStringField;
    RPLTablesUNIQUE_KEY_NAMES: TStringField;
    RPLTablesTRIG_BASE_NAME: TStringField;
    RPLTablesREF_TABLE: TStringField;
    RPLTablesREF_TABLE_KEY: TStringField;
    RPLTablesPRIMARY_KEY_SYNC: TStringField;
    RPLTablesREPL_INSERTS: TStringField;
    RPLTablesREPL_UPDATES: TStringField;
    RPLTablesREPL_DELETES: TStringField;
    RPLTablesCONDITION_FIELD: TStringField;
    RPLTablesCONDITION: TMemoField;
    RPLUsersCONDITION_VALUE: TStringField;
    frConnectParams: TfrConnectParams;
    RPLTablesPRIORITY: TIntegerField;
    Button1: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure dbgTablesDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormShow(Sender: TObject);
    procedure dbgTablesDblClick(Sender: TObject);
    procedure btAddNodeClick(Sender: TObject);
    procedure btDeleteNodeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure LoadTables;
    procedure ConnectionLost(Sender: TObject; var RaiseException: Boolean);
    { Private declarations }
  public
    { Public declarations }
    Connection: TCcConnection;
  end;

var
  MainForm: TMainForm;

implementation

uses
  tableconfig, subnode;

{$R *.dfm}

procedure TMainForm.btConnectClick(Sender: TObject);
begin
  if not Connection.Connected then begin
    Screen.Cursor := crHourGlass;
    try
      frConnectParams.SetConnectParams;
      CcConfig.Connection := Connection;
      CcConfig.Connect;
      btConnect.Caption := 'Disconnect';
      lbConnected.Visible := True;
      tsTables.TabVisible := True;
      tsSubNodes.TabVisible := True;
      PageControl.ActivePage := tsSubNodes;
      LoadTables;
    finally
      Screen.Cursor := crDefault;
    end;
  end else begin
    CcConfig.Disconnect;
    btConnect.Caption := 'Connect';
    lbConnected.Visible := False;
  end;
end;

procedure TMainForm.LoadTables;
begin
  Connection.StartTransaction;
  RPLTables.Close;
  RPLTables.Connection := Connection;
  RPLTables.Open;

  RPLUsers.Close;
  RPLUsers.Connection := Connection;
  RPLUsers.Open;
end;

procedure TMainForm.dbgTablesDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  if (RPLTablesCREATED.AsString = 'Y') then begin
    dbgTables.Canvas.Brush.Color := $009AF5F8;
    dbgTables.Canvas.Font.Color := clBlack;
  end;
  dbgTables.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  tsTables.TabVisible := False;
  tsSubNodes.TabVisible := False;

  frConnectParams.Init;
  //Connection is just a pointer to the connection object so as to make it visible from other forms
  Connection := frConnectParams.Connection;
  Connection.OnConnectionLost := ConnectionLost;
end;

procedure TMainForm.ConnectionLost(Sender: TObject; var RaiseException: Boolean);
begin
  btConnect.Caption := 'Connect';
  lbConnected.Visible := False;
end;

procedure TMainForm.dbgTablesDblClick(Sender: TObject);
var
  cTableName: String;
begin
  cTableName := RPLTablesTABLE_NAME.AsString;
  with TfmTables.Create(Self) do try
		if ShowModal = mrOk then begin
      LoadTables;
      RPLTables.Locate('TABLE_NAME', cTableName, []);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.btAddNodeClick(Sender: TObject);
var
  cSubNodeName:String;
begin
  cSubNodeName := RPLUsersLOGIN.AsString;
  if Sender = btAddNode then
    RPLUsers.Append;
  with TfmSubNode.Create(Self) do try
    if ShowModal = mrOk then begin
      LoadTables;
      RPLUsers.Locate('LOGIN', cSubNodeName, []);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.btDeleteNodeClick(Sender: TObject);
begin
  if Application.MessageBox('Are you sure you want to delete this sub-node?',
    'Confirmation', MB_ICONQUESTION + MB_YESNO) = IDYES then begin
    RPLUsers.Delete;
    Connection.CommitRetaining;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  RPLTables.First;
  while not RPLTables.Eof do begin
    if (RPLTablesCREATED.AsString = 'Y') then begin
      CcConfig.GenerateTriggers(RPLTablesTABLE_NAME.AsString);
    end;
    RPLTables.Next;
  end;
  CcConfig.Connection.Commit;
end;

end.
