unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IBDatabase, IBSQL, ExtCtrls, CcConf, CcConfStorage,
  CcMemDS, CCDBParams, CcProviders,
  CcReplicator, DBCtrls, Grids, DBGrids, Mask,
  ComCtrls, StdCtrls, SyncObjs,
  IBServices, IBCustomDataSet, Buttons, CcProvIBX;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    btReactiver: TBitBtn;
    btDesactiver: TBitBtn;
    btReplicate: TBitBtn;
    btArreter: TBitBtn;
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    lbReplicationEnCours: TLabel;
    RzLabel1: TLabel;
    Image1: TImage;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    edLog: TMemo;
    ProgressBar: TProgressBar;
    TabSheet2: TTabSheet;
    DBGrid3: TDBGrid;
    Panel3: TPanel;
    RzDBNavigator1: TDBNavigator;
    Replicator: TCcReplicator;
    LocalDataSetDS: TDataSource;
    RemoteDataSetDS: TDataSource;
    qLocalDataSet: TIBDataSet;
    qLocalDataSetCUST_NO: TIntegerField;
    qLocalDataSetNAME: TStringField;
    qScript: TIBSQL;
    qConflicts: TIBDataSet;
    qConflictsDS: TDataSource;
    RestoreService: TIBRestoreService;
    SecurityService: TIBSecurityService;
    LocalDB: TIBDatabase;
    LocalTR: TIBTransaction;
    RplConfig: TCcConfig;
    Timer: TTimer;
    RemoteDB: TIBDatabase;
    RemoteTR: TIBTransaction;
    qLocalUpdateRPLLog: TIBSQL;
    qLocalUpdateRPLUser: TIBSQL;
    qLocalInsertRPLUser: TIBSQL;
    qRemoteUpdateRPLLog: TIBSQL;
    qRemoteUpdateRPLUser: TIBSQL;
    qRemoteInsertRPLUser: TIBSQL;
    qGetLocalUser: TIBSQL;
    qGetRemoteUser: TIBSQL;
    qRemoteDataSet: TIBDataSet;
    qRemoteDataSetCUST_NO: TIntegerField;
    qRemoteDataSetNAME: TStringField;
    Panel4: TPanel;
    Panel6: TPanel;
    Label1: TLabel;
    edLocalUser: TEdit;
    RzDBNavigator3: TDBNavigator;
    DBGrid1: TDBGrid;
    Panel7: TPanel;
    RzDBNavigator2: TDBNavigator;
    Panel5: TPanel;
    Label2: TLabel;
    edRemoteUser: TEdit;
    DBGrid2: TDBGrid;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    CcProviderIBX1: TCcProviderIBX;
    qLocalTables: TIBSQL;
    qRemoteTables: TIBSQL;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btReplicateClick(Sender: TObject);
    procedure btArreterClick(Sender: TObject);
    procedure btDesactiverClick(Sender: TObject);
    procedure btReactiverClick(Sender: TObject);
    procedure ReplicatorTableBegin(Sender: TObject; Name: String);
    procedure ReplicatorConflict(Sender: TObject);
    procedure ReplicatorRowReplicating(Sender: TObject);
    procedure ReplicatorReplicateProc(Sender: TObject; Name: String);
    procedure ReplicatorReplicationError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
    procedure ReplicatorException(Sender: TObject; e: Exception);
    procedure ReplicatorAutoCommit(Sender: TObject);
    procedure ReplicatorAbort(Sender: TObject);
    procedure ReplicatorGenReplError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
    procedure ReplicatorQueryDone(Sender: TObject; QueryType: TCcQueryType;
      Rows: Integer);
    procedure ReplicatorProgress(Sender: TObject);
    procedure ReplicatorEmptyLog(Sender: TObject);
    procedure ReplicatorFinished(Sender: TObject);
    procedure ReplicatorLogLoaded(Sender: TObject);
    procedure ReplicatorGenReplicated(Sender: TObject; Name,
      NewValue: String);
    procedure FormShow(Sender: TObject);
    procedure RestoreServiceTextNotify(Sender: TObject;
      const Text: String);
    procedure ReplicatorBeforeReplicate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure edLocalUserEnter(Sender: TObject);
    procedure edLocalUserExit(Sender: TObject);
    procedure edRemoteUserExit(Sender: TObject);
    procedure edRemoteUserEnter(Sender: TObject);
    procedure DBGrid1Enter(Sender: TObject);
    procedure DBGrid2Enter(Sender: TObject);
    procedure ReplicatorConnectLocal(Sender: TObject);
    procedure ReplicatorConnectRemote(Sender: TObject);
    procedure ReplicatorDisconnect(Sender: TObject);
    procedure qRemoteDataSetAfterPost(DataSet: TDataSet);
    procedure qRemoteDataSetAfterDelete(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure ReplicatorConnectionLost(Sender: TObject;
      Database: TCcDatabase);
    procedure LogEvent(cLine: String);
    procedure ReOpenTables();
    procedure OpenLocalTable();
    procedure CheckConnected();
    procedure OpenRemoteTable();
    procedure CheckRestore(cFileName:String);
    procedure CheckUsers();
    procedure CheckUser(cUserName:String);
    procedure CheckMetadata();
    procedure LocalDBLogin(Database: TIBDatabase; LoginParams: TStrings);
  private
    RestoreEof: TEvent;
    cOldLocalUser,
    cOldRemoteUser,
    cSYSDBA,
    cSYSDBAPassword:String;
    { Private declarations }
  public
    { Public declarations }  
  end;

var
  Form1: TForm1;

implementation

uses IB, Unit2;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RestoreEof := TEvent.Create(nil, false, false, 'RestoreServiceEof');
  cSYSDBA := 'SYSDBA';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RestoreEof.Free;
end;

procedure TForm1.btReplicateClick(Sender: TObject);
begin
  Replicator.Replicate();
end;

procedure TForm1.btArreterClick(Sender: TObject);
begin
  Replicator.AbortReplication();
end;

procedure TForm1.btDesactiverClick(Sender: TObject);
begin
  Replicator.AutoReplicate.Stop();
  btReactiver.Enabled := true;
  btDesactiver.Enabled := false;
end;

procedure TForm1.btReactiverClick(Sender: TObject);
var
  cFrequency:String;
begin
  cFrequency := InputBox('Auto-replication', 'Auto-replication frequency (seconds):', '');
  if (Trim(cFrequency) <> '') then begin
    Replicator.AutoReplicate.Frequency := StrToInt(Trim(cFrequency));
    Replicator.AutoReplicate.Start();
    btReactiver.Enabled := false;
    btDesactiver.Enabled := true;
  end;
end;

procedure TForm1.LogEvent(cLine: String);
var
  dNow:TDateTime;
  cDateTime :String;
  nHours, nMinutes, nSeconds, nMilliSeconds:Word;
begin
  dNow := Now;
  DecodeTime(dNow, nHours, nMinutes, nSeconds, nMilliSeconds);
  cDateTime := FormatDateTime('dd/mm/yyyy hh:mm:ss', dNow) + '.' + FormatFloat('000', nMilliSeconds);
  cLine := cDateTime + ' : ' + cLine;
  edLog.Lines.Add(cLine);
end;

procedure TForm1.ReplicatorTableBegin(Sender: TObject; Name: String);
begin
  LogEvent('Table: ' + Name);
end;

procedure TForm1.ReplicatorConflict(Sender: TObject);
begin
  LogEvent(''#9'Conflict!');
end;

procedure TForm1.ReplicatorRowReplicating(Sender: TObject);
var
  cMessage:String;
begin
  cMessage := ''#9'Replicating from ' + Replicator.Log.FBN('Origin') + ' ';
  if (Replicator.Log.FBN('REF_FIELD') <> '') then
    cMessage := cMessage + Replicator.Log.FBN('REF_FIELD') + ' := ' + Replicator.Log.FBN('REF_VALUE')
  else
    cMessage := cMessage + Replicator.Log.FBN('PK1_NAME') + ' := ' + Replicator.Log.FBN('PK1_VALUE') + ', ' + Replicator.Log.FBN('PK2_NAME') + ' := ' + Replicator.Log.FBN('PK2_VALUE') + ', ' + Replicator.Log.FBN('PK3_NAME') + ' := ' + Replicator.Log.FBN('PK3_VALUE');
  LogEvent(cMessage);
end;

procedure TForm1.ReplicatorReplicateProc(Sender: TObject; Name: String);
begin
  LogEvent('Procedure: ' + Replicator.Log.FBN('PROCEDURE_STATEMENT'));
end;

procedure TForm1.ReplicatorReplicationError(Sender: TObject; e: Exception;
  var CanContinue: Boolean);
begin
  LogEvent('Replication error: ' + e.Message);
  CanContinue := true;
end;

procedure TForm1.ReplicatorException(Sender: TObject; e: Exception);
begin
  LogEvent('Error: ' + e.Message);
end;

procedure TForm1.ReplicatorAutoCommit(Sender: TObject);
begin
  LogEvent('Auto-commiting...');
end;

procedure TForm1.ReplicatorAbort(Sender: TObject);
begin
  LogEvent('Replication aborted!');
end;

procedure TForm1.ReplicatorGenReplError(Sender: TObject; e: Exception;
  var CanContinue: Boolean);
var
  f:EIBError;
begin
  f := (e as EIBError);
  if ((f.SQLCode = -903) or (f.SQLCode = -901)) then begin
    LogEvent('Local record locked!!');
    Application.ProcessMessages();
    CanContinue := false;
  end
  else begin
    LogEvent('Error synchronizing the primary keys!' + e.Message);
    CanContinue := false;
  end
end;

procedure TForm1.ReplicatorQueryDone(Sender: TObject;
  QueryType: TCcQueryType; Rows: Integer);
var
  cQueryType:String;
begin
  case (QueryType) of
    qtSelect: cQueryType := 'SELECT';
    qtDelete: cQueryType := 'DELETE';
    qtInsert: cQueryType := 'INSERT';
    qtUpdate: cQueryType := 'UPDATE'; 
  end;
  LogEvent(#9 + cQueryType + ': ' + IntToStr(Rows) + ' rows');
end;

procedure TForm1.ReplicatorProgress(Sender: TObject);
begin
  if (Replicator.Busy)  then begin
    ProgressBar.Max := Replicator.Log.LineCount;
    ProgressBar.Position := Replicator.Log.CurrentLine;
  end
  else
    ProgressBar.Position := 0;
  Application.ProcessMessages();
end;

procedure TForm1.ReplicatorEmptyLog(Sender: TObject);
begin
  LogEvent('Nothing to replicate!');
  ReOpenTables();
end;

procedure TForm1.ReplicatorFinished(Sender: TObject);
begin
  ProgressBar.Position := 0;
  LogEvent('Repliction finished');
  Application.ProcessMessages();

  btReplicate.Enabled := true;
  btArreter.Enabled := false;

  lbReplicationEnCours.Visible := false;

  ReOpenTables();
  Screen.Cursor := crDefault;
end;

procedure TForm1.ReplicatorLogLoaded(Sender: TObject);
begin
  edLog.Lines.Add('');
  LogEvent('Replication started: ' + IntToStr(Replicator.Log.LineCount) + ' rows');

  btReplicate.Enabled := false;
  btArreter.Enabled := true;

  lbReplicationEnCours.Visible := true;
  Screen.Cursor := crHourGlass;
end;

procedure TForm1.ReplicatorGenReplicated(Sender: TObject; Name,
  NewValue: String);
begin
  LogEvent('Replicated generator : ' + Name + ' => ' + NewValue);
end;

procedure TForm1.OpenLocalTable();
begin
  CheckConnected();
  qLocalDataSet.Close();
  qLocalDataSet.Open();
end;

procedure TForm1.OpenRemoteTable();
begin
  CheckConnected();
  qRemoteDataSet.Close();
  qRemoteDataSet.Open();
end;

procedure TForm1.CheckRestore(cFileName:String);
var
  cBackup:String;
begin
  if (not FileExists(cFileName)) then begin
    //Restore
    cBackup := ChangeFileExt(cFileName, '.gbk');
    RestoreService.BackupFile.Text := cBackup;
    RestoreService.DatabaseName.Text := cFileName;
    RestoreService.Params.Values['USER_NAME'] := cSYSDBA;
    try
      RestoreService.Active := true;
    except
      on e:EIBClientError do begin //User canceled
        Application.Terminate();
        Abort;
      end;
    end;
    RestoreService.ServiceStart();
    RestoreService.Active := false;
    Sleep(10000);
    RestoreService.LoginPrompt := false;
    cSYSDBA := RestoreService.Params.Values['USER_NAME'];
    cSYSDBAPassword := RestoreService.Params.Values['PASSWORD'];
  end
end;

procedure TForm1.FormShow(Sender: TObject);
var
  cAppDir:String;
begin
  if not (Assigned(Replicator.DBProvider) and Assigned(RplConfig.DBProvider)) then begin
    Application.MessageBox('Replicator and configuration components must be connected to a DBProvider in order to function. You need to compile the CopyCat_???.dpk packages for the data-access components you use (FIBPlus, IBX, UIB, etc), if you have not already done so.', 'DBProvider missing!', MB_ICONINFORMATION + MB_OK);
    Application.Terminate;
  end;
  cAppDir := ExtractFileDir(Application.ExeName);
  if (not ((FileExists(cAppDir + '\\local.gdb') and FileExists(cAppDir + '\\remote.gdb')) or
         (FileExists(cAppDir + '\\local.gbk') and FileExists(cAppDir + '\\remote.gbk'))) ) then begin
    Application.MessageBox('Demonstration database files missing!'#13#10'Reinstalling CopyCat demo should correct the problem.', 'Error', MB_ICONERROR + MB_OK);
    Application.Terminate();
  end;
  if (not (FileExists(cAppDir + '\\local.gdb') and FileExists(cAppDir + '\\remote.gdb'))) then begin
    if (Application.MessageBox('For compatability reasons, the example databases are supplied as backups only, and must therefore be restored. Restore now?', 'Confirmation', MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
      CheckRestore(cAppDir + '\\local.gdb');
      CheckRestore(cAppDir + '\\remote.gdb');
    end
    else begin
      Application.MessageBox('Both local and remote databases must be present in the application directory'#13#10'in order for the demo to function correctly.', 'Error', MB_ICONERROR + MB_OK);
      Application.Terminate();
    end
  end;
  Replicator.LocalDB.DBName := cAppDir + '\\local.gdb';
  Replicator.RemoteDB.DBName := cAppDir + '\\remote.gdb';
  Timer.Enabled := true;
end;

procedure TForm1.CheckUsers;
begin
  if ((Trim(edLocalUser.Text) <> '') and (Trim(edRemoteUser.Text) <> '')) then begin
    SecurityService.Params.Clear();
    SecurityService.Params.Add('USER_NAME=' + cSYSDBA);
    SecurityService.Params.Add('PASSWORD=' + cSYSDBAPassword);
    SecurityService.Active := true;
    try
      CheckUser(Trim(edLocalUser.Text));
      CheckUser(Trim(edRemoteUser.Text));
    finally 
      SecurityService.Active := false;
    end;
  end
  else begin
    Application.MessageBox('Both usernames must have a value before replication can start!', 'User name required', MB_ICONERROR + MB_OK);
    Abort();
  end;
end;

procedure TForm1.CheckUser(cUserName:String);
begin
  SecurityService.Params.Clear();
  SecurityService.Params.Add('USER_NAME=' + cSYSDBA);
  SecurityService.Params.Add('PASSWORD=' + cSYSDBAPassword);
  SecurityService.Active := true;
  try 
    SecurityService.DisplayUser(cUserName);
    if (SecurityService.UserInfoCount <= 0) then begin
      if (Application.MessageBox(Pchar('User ''' + cUserName + ''' does not exist. Create it now?'), 'Confirmation', MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
        SecurityService.UserName := cUserName;
        SecurityService.FirstName := '';
        SecurityService.Password := 'CopyCat';
        SecurityService.AddUser();
      end
      else Abort();
    end
  finally
    SecurityService.Active := false;
  end;
end;


procedure TForm1.RestoreServiceTextNotify(Sender: TObject;
  const Text: String);
begin
  if (RestoreService.Eof) then begin
    RestoreService.Active := false;
    RestoreEof.SetEvent();
  end
end;

procedure TForm1.ReplicatorBeforeReplicate(Sender: TObject);
begin
  if ((Trim(edLocalUser.Text) = '') or (Trim(edRemoteUser.Text) = '')) then begin
    Application.MessageBox('Both usernames must have a value before replication can start!', 'User name required', MB_ICONERROR + MB_OK);
    Abort();
  end;

  Replicator.Users.LocalSYSDBA.Name := cSYSDBA;
  Replicator.Users.LocalSYSDBA.Password := cSYSDBAPassword;
  Replicator.Users.LocalUser.Name := Trim(edLocalUser.Text);
  Replicator.Users.LocalUser.Password := 'CopyCat';
  Replicator.Users.RemoteUser.Name := Trim(edRemoteUser.Text);
  Replicator.Users.RemoteUser.Password := 'CopyCat';

  if (qLocalDataSet.Active and (qLocalDataSet.State <> dsBrowse)) then
    qLocalDataSet.Post();
  if (qRemoteDataSet.Active and (qRemoteDataSet.State <> dsBrowse)) then
    qRemoteDataSet.Post();
  if (qConflicts.Active and (qConflicts.State <> dsBrowse)) then
    qConflicts.Post();

  if (LocalTR.InTransaction) then
    LocalTR.Commit();
  LocalDB.Connected := false;
  if (RemoteTR.InTransaction) then
    RemoteTR.Commit();
  RemoteDB.Connected := false;
end;

procedure TForm1.CheckConnected();
begin
  if (not LocalDB.Connected) then begin
    LocalDB.DatabaseName := Replicator.LocalDB.DBName;
    if (cSYSDBAPassword <> '') then begin
      LocalDB.Params.Values['User_Name'] := cSYSDBA;
      LocalDB.Params.Values['Password'] := cSYSDBAPassword;
    end;
    try
      LocalDB.Connected := true;
    except
      on e:EIBClientError do begin //User canceled
        Application.Terminate();
        Abort();
      end;
    end;
    LocalDB.LoginPrompt := false;
    LocalTR.Active := true;
  end;
  if (not RemoteDB.Connected) then begin
    RemoteDB.DatabaseName := Replicator.RemoteDB.DBName;
    RemoteDB.Params.Values['User_Name'] := LocalDB.Params.Values['User_Name'];
    RemoteDB.Params.Values['Password'] := LocalDB.Params.Values['Password'];
    RemoteDB.Connected := true;
    RemoteTR.Active := true;
  end
end;

procedure TForm1.CheckMetadata();
var
  lCreated:Boolean;
begin
  lCreated := false;
  RplConfig.Disconnect();
  RplConfig.ConnectParams.CharSet := Replicator.LocalDB.CharSet;
  RplConfig.ConnectParams.SQLDialect := Replicator.LocalDB.SQLDialect;
  RplConfig.ConnectParams.DBName := Replicator.LocalDB.DBName;
  RplConfig.ConnectParams.UserName := cSYSDBA;
  RplConfig.ConnectParams.Password := cSYSDBAPassword;
  RplConfig.Connect();
  qLocalTables.Close;
  qLocalTables.ParamByName('table_name').AsString := 'CUSTOMERS';
  qLocalTables.ExecQuery;
  if (qLocalTables.FieldByName('CREATED').AsString = 'N') then begin
    RplConfig.GenerateTriggers('CUSTOMERS');
    lCreated := true;
  end;

  RplConfig.Disconnect();
  RplConfig.ConnectParams.CharSet := Replicator.RemoteDB.CharSet;
  RplConfig.ConnectParams.SQLDialect := Replicator.RemoteDB.SQLDialect;
  RplConfig.ConnectParams.DBName := Replicator.RemoteDB.DBName;
  RplConfig.ConnectParams.UserName := cSYSDBA;
  RplConfig.ConnectParams.Password := cSYSDBAPassword;
  RplConfig.Connect();
  qRemoteTables.Close;
  qRemoteTables.ParamByName('table_name').AsString := 'CUSTOMERS';
  qRemoteTables.ExecQuery;
  if (qRemoteTables.FieldByName('CREATED').AsString = 'N') then begin
    RplConfig.GenerateTriggers('CUSTOMERS');
    lCreated := true;
  end;
  if (lCreated) then
    Application.MessageBox('The replication meta-data has been successfully created.'#10#13#10#13''
      +'You may now make changes to either or both databases, and then click on'
      +''#10#13'the ''Replicate now'' button to synchronize.'#10#13#10#13'Note: Only changes made after '
      +'this point will be synchronized.'#10#13'To replicate changes made previously, simply touch the '
      +'record again (in any way),'#13#10'and the current version of the record will be replicated to the other database.', 'Meta-data created', MB_ICONINFORMATION + MB_OK);
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  CheckConnected();
  CheckMetadata();
  ReOpenTables();

  qGetLocalUser.Close();
  qGetLocalUser.ExecQuery();
  if (Trim(qGetLocalUser.FieldByName('LOGIN').AsString) <> '') then
    edLocalUser.Text := Trim(qGetLocalUser.FieldByName('LOGIN').AsString);

  qGetRemoteUser.Close();
  qGetRemoteUser.ExecQuery();
  if (Trim(qGetRemoteUser.FieldByName('LOGIN').AsString) <> '') then
    edRemoteUser.Text := Trim(qGetRemoteUser.FieldByName('LOGIN').AsString);
end;

procedure TForm1.edLocalUserEnter(Sender: TObject);
begin
  cOldLocalUser := Trim(edLocalUser.Text);
end;

procedure TForm1.edLocalUserExit(Sender: TObject);
begin
  if (Trim(edLocalUser.Text) <> '') then begin
    if (cOldLocalUser <> Trim(edLocalUser.Text)) then begin
      CheckUser(Trim(edLocalUser.Text));
      if (cOldLocalUser <> '') then begin
        qRemoteUpdateRPLUser.Close();
        qRemoteUpdateRPLUser.ParamByName('old_user').Value := cOldLocalUser;
        qRemoteUpdateRPLUser.ParamByName('new_user').Value := Trim(edLocalUser.Text);
        qRemoteUpdateRPLUser.ExecQuery();

        qRemoteUpdateRPLLog.Close();
        qRemoteUpdateRPLLog.ParamByName('old_user').Value := cOldLocalUser;
        qRemoteUpdateRPLLog.ParamByName('new_user').Value := Trim(edLocalUser.Text);
        qRemoteUpdateRPLLog.ExecQuery();
      end
      else begin
        qRemoteInsertRPLUser.Close();
        qRemoteInsertRPLUser.ParamByName('new_user').Value := Trim(edLocalUser.Text);
        qRemoteInsertRPLUser.ExecQuery();
      end;
      RemoteTR.CommitRetaining();
    end
  end
end;

procedure TForm1.edRemoteUserExit(Sender: TObject);
begin
  if (Trim(edRemoteUser.Text) <> '') then begin
    if (cOldRemoteUser <> Trim(edRemoteUser.Text)) then begin
      CheckUser(Trim(edRemoteUser.Text));
      if (cOldRemoteUser <> '') then begin
        qLocalUpdateRPLUser.Close();
        qLocalUpdateRPLUser.ParamByName('old_user').Value := cOldRemoteUser;
        qLocalUpdateRPLUser.ParamByName('new_user').Value := Trim(edRemoteUser.Text);
        qLocalUpdateRPLUser.ExecQuery();

        qLocalUpdateRPLLog.Close();
        qLocalUpdateRPLLog.ParamByName('old_user').Value := cOldRemoteUser;
        qLocalUpdateRPLLog.ParamByName('new_user').Value := Trim(edRemoteUser.Text);
        qLocalUpdateRPLLog.ExecQuery();
      end
      else begin
        qLocalInsertRPLUser.Close();
        qLocalInsertRPLUser.ParamByName('new_user').Value := Trim(edRemoteUser.Text);
        qLocalInsertRPLUser.ExecQuery();
      end;
      LocalTR.CommitRetaining();
    end
  end
end;

procedure TForm1.edRemoteUserEnter(Sender: TObject);
begin
  cOldRemoteUser := Trim(edRemoteUser.Text);
end;

procedure TForm1.DBGrid1Enter(Sender: TObject);
begin
  if (Trim(edRemoteUser.Text) = '') then begin
    ActiveControl := edLog;
    Application.MessageBox('You must provide a user name before editing any data.', 'Information', MB_ICONINFORMATION + MB_OK);
    ActiveControl := edRemoteUser;
  end
end;

procedure TForm1.DBGrid2Enter(Sender: TObject);
begin
  if (Trim(edLocalUser.Text) = '') then begin
    ActiveControl := edLog;
    Application.MessageBox('You must provide a user name before editing any data.', 'Information', MB_ICONINFORMATION + MB_OK);
    ActiveControl := edLocalUser;
  end
end;

procedure TForm1.ReplicatorConnectLocal(Sender: TObject);
begin
  LogEvent('Connecting to local database...');
end;

procedure TForm1.ReplicatorConnectRemote(Sender: TObject);
begin
  LogEvent('Connecting to remote database...');
end;

procedure TForm1.ReplicatorDisconnect(Sender: TObject);
begin
  LogEvent('Disconnecting from databases...');
end;

procedure TForm1.ReOpenTables();
begin
  Application.ProcessMessages();
  OpenLocalTable();
  Application.ProcessMessages();
  OpenRemoteTable();
  Application.ProcessMessages();
  qConflicts.Close();
  qConflicts.Open();
end;

procedure TForm1.qRemoteDataSetAfterPost(DataSet: TDataSet);
begin
  (DataSet as TIBDataSet).Transaction.CommitRetaining();
end;

procedure TForm1.qRemoteDataSetAfterDelete(DataSet: TDataSet);
begin
  (DataSet as TIBDataSet).Transaction.CommitRetaining();
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (qConflicts.Active and (qConflicts.State <> dsBrowse))then
    qConflicts.Post();
  if (qLocalDataSet.Active and (qLocalDataSet.State <> dsBrowse))then
    qLocalDataSet.Post();
  if (qRemoteDataSet.Active and (qRemoteDataSet.State <> dsBrowse))then
    qRemoteDataSet.Post();
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Application.Terminate();
end;

procedure TForm1.ReplicatorConnectionLost(Sender: TObject;
  Database: TCCDatabase);
begin
  LogEvent('Connection lost to database: ' + Database.DBName + '!');
end;

procedure TForm1.LocalDBLogin(Database: TIBDatabase;
  LoginParams: TStrings);
begin
  if (cSYSDBAPassword = '') then begin
    fmDBLogin := TfmDBLogin.Create(Self);
    if fmDBLogin.ShowModal = mrOk then begin
      cSYSDBA := fmDBLogin.cUserName;
      cSYSDBAPassword := fmDBLogin.cPassword;
    end;
    fmDBLogin.Free;
  end;
  LoginParams.Values['User_Name'] := cSYSDBA;
  LoginParams.Values['Password'] := cSYSDBAPassword;
end;

end.
