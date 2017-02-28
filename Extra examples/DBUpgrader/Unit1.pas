unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fConnectParams, CcConfStorage, CcConf, Db, ExtCtrls, Grids, DBGrids,
  StdCtrls, Buttons, ComCtrls, CcProviders, CcMemDS, CcDB;

type
  TForm1 = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    lbConnected: TLabel;
    Label6: TLabel;
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    tsTables: TTabSheet;
    dbgTables: TDBGrid;
    Panel1: TPanel;
    Label7: TLabel;
    RPLTablesDS: TDataSource;
    CcConfig: TCcConfig;
    Label8: TLabel;
    Button1: TButton;
    Label9: TLabel;
    frConnectParams: TfrConnectParams;
    RPLTables: TCcDataSet;
    RPLTablesTABLE_NAME: TStringField;
    RPLTablesCREATED: TStringField;
    RPLTablesTRIG_BASE_NAME: TStringField;
    RPLTablesPRIORITY: TIntegerField;
    RPLProcedures: TCcDataSet;
    RPLProceduresNEW_PROCEDURE_NAME: TStringField;
    RPLUsersDS: TDataSource;
    RPLProceduresCREATED: TStringField;
    RPLProceduresPROCEDURE_NAME: TStringField;
    tsSQLLog: TTabSheet;
    SQLLog: TMemo;
    Label1: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CcConfigQueryReady(Sender: TObject; Script: TStrings);
  private
    Connection : TCcConnection;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin

  Screen.Cursor := crHourGlass;
  try
    frConnectParams.SetConnectParams;
    Connection.Connected := True;
    lbConnected.Visible := True;
    tsTables.TabVisible := True;
    PageControl.ActivePage := tsTables;
    RPLTables.Open;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);

procedure ExecQuery(cSQL: String);
begin
  try
    Connection.ExecQuery(cSQL);
    Connection.CommitRetaining;
    SQLLog.Lines.Add(cSQL + ';');
  except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

var
  cTrigBaseName: String;

begin
  if Application.MessageBox('Are you sure you are ready to upgrade the meta-data for this database?', 'Confirmation', MB_ICONWARNING + MB_YESNO)
    = IDYES then begin

    SQLLog.Lines.Clear;
    Screen.Cursor := crHourGlass;
    try
      RPLTables.First;
      ExecQuery('alter table rpl$log add PRIMARY_KEY_SYNC VARCHAR(5000), add UNIQUE_KEY_SYNC VARCHAR(5000), add SENT_FROM varchar(50)');
      ExecQuery('alter table rpl$tables alter PRIMARY_KEY_SYNC type VARCHAR(5000), alter UNIQUE_KEY_SYNC type VARCHAR(5000)');
      ExecQuery('alter table rpl$users add passwrd varchar(100)');
      ExecQuery('alter table RPL$CONFLICTS add SQL1 blob sub_type 1, add SQL2 blob sub_type 1');
      ExecQuery('alter table rpl$tables add condition2 blob sub_type 1');
      ExecQuery('update rpl$tables set condition2 = condition');
      ExecQuery('alter table rpl$tables drop condition, add condition blob sub_type 1');
      ExecQuery('update rpl$tables set condition = condition2');
      ExecQuery('alter table rpl$tables drop condition2');
      ExecQuery('update rpl$log set primary_key_sync = (select primary_key_sync from rpl$tables where table_name = rpl$log.table_name), unique_key_sync = (select unique_key_sync from rpl$tables where table_name = rpl$log.table_name)');

      while not RPLTables.Eof do begin
        if (RPLTablesCREATED.AsString = 'Y') then begin
          cTrigBaseName := Trim(RPLTablesTRIG_BASE_NAME.AsString);
          if (cTrigBaseName <> '') then begin
          if Connection.DBAdaptor.TriggerExists(cTrigBaseName + '_I') then
            ExecQuery('drop trigger ' + cTrigBaseName + '_I');
            if Connection.DBAdaptor.TriggerExists(cTrigBaseName + '_U') then
            ExecQuery('drop trigger ' + cTrigBaseName + '_U');
            if Connection.DBAdaptor.TriggerExists(cTrigBaseName + '_D') then
            ExecQuery('drop trigger ' + cTrigBaseName + '_D');
          end;
        end;
        RPLTables.Next;
      end;

      RPLProcedures.Open;
      RPLProcedures.First;
      while not RPLProcedures.Eof do begin
        if (RPLProceduresCREATED.AsString = 'Y') then begin
          Application.MessageBox(Pchar('Procedure ' + RPLProceduresNEW_PROCEDURE_NAME.AsString + ' will be dropped.'#13#10'Please remove now any and all dependancies.'), 'Remove dependencies', MB_ICONINFORMATION + MB_OK);
          ExecQuery('drop procedure ' + RPLProceduresNEW_PROCEDURE_NAME.AsString);
        end;
        RPLProcedures.Next;
      end;

      Connection.Connected := False;
      Connection.Connected := True;

      ExecQuery('drop procedure rpl$generate_log');
      ExecQuery('drop procedure rpl$quote_str');

      CcConfig.Connect;

      RPLProcedures.Open;
      RPLProcedures.First;
      while not RPLProcedures.Eof do begin
        try
          if (RPLProceduresCREATED.AsString = 'Y') then
            CcConfig.GenerateProcedure(Trim(RPLProceduresPROCEDURE_NAME.AsString));
        except on e: Exception do
          ShowMessage(e.Message);
        end;
        RPLProcedures.Next;
      end;

      RPLTables.Open;
      RPLTables.First;
      while not RPLTables.Eof do begin
        try
          if (RPLTablesCREATED.AsString = 'Y') then
            CcConfig.GenerateTriggers(Trim(RPLTablesTABLE_NAME.AsString));
        except on e: Exception do
          ShowMessage(e.Message);
        end;
        RPLTables.Next;
      end;

(*    qUpdateVersion.Close();
      qUpdateVersion.Param['copycat_version'].Value := CcConfig.Version;
      qUpdateVersion.Param['dbtype'].Value := Connection.DBType;
      qUpdateVersion.Param['dbversion'].Value := Connection.DBVersion;
      qUpdateVersion.Exec;*)

      CcConfig.Disconnect;
      Application.MessageBox('This database has been successfully upgraded!', 'Upgrade completed', MB_ICONINFORMATION + MB_OK);
      tsSQLLog.TabVisible := True;
      PageControl.ActivePage := tsSQLLog;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connection := frConnectParams.Connection;
  CcConfig.Connection := Connection;
  frConnectParams.Init;
end;

procedure TForm1.CcConfigQueryReady(Sender: TObject; Script: TStrings);
begin
  SQLLog.Lines.AddStrings(Script);
end;

end.
