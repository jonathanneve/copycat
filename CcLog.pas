// CopyCat replication suite<p/>
// Copyright (c) 2014 Microtec Communications<p/>
// For any questions or technical support, contact us at contact@copycat.fr
unit CcLog;

{$I CC.INC}

interface

uses
  Classes, DB, SysUtils, CCat, CcProviders, CcKeys, CcReplicator, CcDB;

type
  { ********************************************************************************************
    Summary:
    Component for accessing the replication log.
    Description:
    TCcLog is used by TCcReplicator to hold and manage the replication log.
    Applications can access the replication log by using the Log property of
    TCcReplicator. TCcLog reflects the merged content of both local and remote
    RPL$LOG tables, grouping duplicate record logs, so as to optimize performance.<p/>
    <p/>
    Internally, TCcLog holds a dataset with the following fields, which are made
    accessible by using the FieldByName or FBN methods:

    <table 25c%, 10c%, 65c%>
    Field name                Type          \Description
    ========================  ============  ----------------------------------------------------
    TABLE_NAME                String        Table name
    REMOTE_TABLE_NAME         String        Remote table name
    PRIMARY_KEY_VALUES<p/>    String        Values of all the primary key fields
    PRIMARY_KEY_SYNC<p/>      String        Synchronization statement(s) for updating the primary key field(s) (taken from RPL$LOG)
    GENERIC_PRIMARY_KEY_SYNC  String        Generic synchronization statement(s) for updating the primary key field(s) (taken from RPL$TABLES)
    UNIQUE_KEY_NAMES<p/>      String        Values of all the unique key fields (fields that aren't part of the primary key
    yet need to be synchronized).
    OLD_PRIMARY_KEY_VALUES    String        We keep a trace of the original primary key value in case it changes (due to key synchronisation).
    Only for internal use, do not edit.
    UNIQUE_KEY_SYNC<p/>       String        Synchronization statement(s) for updating the unique key field(s) (taken from RPL$LOG)
    GENERIC_UNIQUE_KEY_SYNC   String        Generic synchronization statement(s) for updating the unique key field(s) (taken from RPL$TABLES)
    REPL_INSERTS<p/>          String        'Y' or 'N' to indicate whether INSERT statements are replicated or not.
    REPL_UPDATES<p/>          String        'Y' or 'N' to indicate whether UPDATE statements are replicated or not.
    REPL_DELETES<p/>          String        'Y' or 'N' to indicate whether DELETE statements are replicated or not.
    OPERATION_DATE<p/>        DateTime      Date and time of the earliest change for the current record
    PROCEDURE_STATEMENT<p/>   String<p/>    SQL of the EXECUTE PROCEDURE statement that is to
    be executed in order to replicate a stored
    procedure.
    PRIORITY<p/>              Integer       Priority of the table. This value is designed to
    avoid problems arising from inter-table
    dependencies, and is used for sorting the rows in
    the log.
    </table>

    Most of these fields take their value from the corresponding fields in the
    RPL$LOG table of the database, and are filled in by the replication triggers.
    These triggers are generated using the TCcConfig component.
    See Also:
    TCcReplicator.Log, TCcConfig.Tables, <link RPL$LOG>
    ******************************************************************************************** }
  TCcLog = class(TCcCustomLog)
  private
    FAllowAllOperations: Boolean;
    FTableName: string;
    FPrimaryKeys: string;
    FKeys: TCcKeyRing;
    FInitiated: Boolean;
    FPrevOnError: TCcErrorEvent;
    FLocalMode: Boolean;
    FLineCount: Integer;
    FCurrentLine: Integer;
    FLogLocal: TCcQuery;
    FLogRemote: TCcQuery;
    FLogValues : TCcQuery;
    FLog: TCcQuery;
    FRecordID: string;
    FOldRecordID: string;
    FRowsDone: TStringList;
    FRowsInError: TStringList;
    FMemValues: TCcMemoryTable;
    procedure ResetKeys(LoadSyncFields: Boolean = True);
    // procedure DoLoadLog(qLog: TCcQuery; lLocal: Boolean);

    procedure ReportConflict;
    procedure GetLogValues;
  protected
    function GetRemoteTableName: string; override;
    procedure ClearKeyRing; override;
    procedure LoadFromDataSet(cTableName: string; DataSet: TCcQuery); override;
    function GetKeys: TCcKeyRing; override;
    procedure Init; override;
    function GetDest: TCcNode; override;
    function GetOrigin: TCcNode; override;
    procedure SetReplicator(Repl: TCcReplicator); override;
    function GetLocalMode: Boolean; override;
    procedure SetLocalMode(Value: Boolean); override;
    function GetEof: Boolean; override;
    function GetCurrentLine: Integer; override;
    function GetLineCount: Integer; override;
    procedure SignalError(Sender: TObject; e: Exception; var CanContinue: Boolean);override;
    procedure RecordReplicated(Sender: TObject); override;
    procedure UpdateState(State: TCcLogState);
    function LoadFromDatabase: Boolean; override;
    function BuildLogSQL(CheckingConflicts: Boolean; node: TCcNode): string;
    function GetAllowedOperations: TCcQueryTypes; override;
    function GetPrimaryKeys: string; override;
    function GetTableName: string; override;
    procedure LoadKeys(cTableName, PrimaryValues, PrimarySync, UniqueSync, GenericPrimarySync, UniqueNames,
      GenericUniqueSync: string); override;
    function GetRecordID: string; override;
    function GetOldRecordID: string;
    function RowAlreadyDone: Boolean; override;
    function RowInError: Boolean; override;
    procedure LoadLogValues(DataSet: TCcMemoryTable);override;
    procedure SkipToRemote;override;
  public
    procedure DeleteLogLine;overload;override;
    procedure DeleteLogLine(nCode: Integer); overload;override;
    procedure ListChangedFields(conn: TCcConnection; sl: TStringList; cLogin, cTableName, cPrimaryKeyValues : String);
    function FBN(cFieldName: string): string; override;
    function FieldByName(cFieldName: string): TCcField; override;
    function FieldExists(cFieldName: string): Boolean; override;
		procedure Next(lRowSkipped: Boolean = False); override;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF CC_D6} uses Variants; {$ENDIF}


constructor TCcLog.Create(Owner: TComponent);
begin
  inherited;
  FReplicator := Owner as TCcReplicator;
  FRowsDone := TStringList.Create;
  FRowsDone.Sorted := True;
  FRowsInError := TStringList.Create;
  FRowsInError.Sorted := True;
  FMemValues := TCcMemoryTable.Create(Self);
  FLog := nil;
end;

procedure TCcLog.DeleteLogLine;
var
  qDeleteLog: TCcQuery;
begin
  DeleteLogLine(StrToInt(FBN('code')));
end;

procedure TCcLog.DeleteLogLine(nCode: Integer);
var
  qDeleteLog: TCcQuery;
begin
  qDeleteLog := Origin.Connection.UpdateQuery['TCcLog_qDeleteLog'];
  qDeleteLog.Close;
  if FReplicator.KeepRowsInLog then
    qDeleteLog.SQL.Text := 'update RPL$LOG set replication_state = ''REPLICATED'' where code = :code'
  else
    qDeleteLog.SQL.Text := 'delete from RPL$LOG where code = :code'; // login = :login and table_name = ''%table_name'' and %primary_key_macro';
  qDeleteLog.Param['code'].Value := FBN('code');
  qDeleteLog.Exec;
end;

destructor TCcLog.Destroy;
begin
  if Assigned(FKeys) then
    FKeys.Free;
  FRowsDone.Free;
  FRowsInError.Free;
  FMemValues.Free;
  inherited;
end;

function TCcLog.GetRecordID: string;
begin
  Result := Trim(FTableName) + Trim(FPrimaryKeys);
end;

function TCcLog.GetRemoteTableName: string;
var
  cTableName: string;
begin
  if FLog <> nil then begin
    cTableName := Trim(FLog.Field['table_name'].AsString);
    if (LocalMode) then
      Result := FReplicator.GetRemoteTableName(cTableName)
    else
      Result := cTableName;
  end;
end;

function TCcLog.FBN(cFieldName: string): string;
begin
  if (FLog <> nil) and (FLog.Field[cFieldName] <> nil) then
    Result := FLog.Field[cFieldName].AsString;
end;

function TCcLog.FieldByName(cFieldName: string): TCcField;
begin
  if FLog <> nil then
    Result := FLog.Field[cFieldName];
end;

function TCcLog.FieldExists(cFieldName: string): Boolean;
begin
  Result := False;
  if FLog <> nil then
    Result := FLog.FindField(cFieldName) <> nil;
end;

function TCcLog.GetAllowedOperations: TCcQueryTypes;
begin
  if FAllowAllOperations then
    Result := [qtInsert, qtUpdate, qtDelete]
  else
  begin
    Result := [];
    if FBN('REPL_INSERTS') = 'Y' then
      Result := Result + [qtInsert];
    if FBN('REPL_UPDATES') = 'Y' then
      Result := Result + [qtUpdate];
    if FBN('REPL_DELETES') = 'Y' then
      Result := Result + [qtDelete];
  end;
end;

function TCcLog.GetCurrentLine: Integer;
begin
  Result := FCurrentLine;
end;

function TCcLog.GetDest: TCcNode;
begin
  if LocalMode then
    Result := FReplicator.RemoteNode
  else
    Result := FReplicator.LocalNode;
end;

function TCcLog.GetOldRecordID: string;
begin
  Result := FOldRecordID;
end;

function TCcLog.GetOrigin: TCcNode;
begin
  if LocalMode then
    Result := FReplicator.LocalNode
  else
    Result := FReplicator.RemoteNode;
end;

function TCcLog.GetEof: Boolean;
begin
  Result := FLog.Eof;
end;

function TCcLog.GetKeys: TCcKeyRing;
begin
  if not Assigned(FKeys) then
    FKeys := TCcKeyRing.Create(FReplicator.LocalDB);
  Result := FKeys;
end;

procedure TCcLog.ClearKeyRing;
begin
  FreeAndNil(FKeys);
end;

function TCcLog.GetLineCount: Integer;
begin
  Result := FLineCount;
end;

function TCcLog.GetLocalMode: Boolean;
begin
  Result := FLocalMode // (FieldByName('LOCAL').Value = 1);
end;

procedure TCcLog.Init;
begin
  if (not FInitiated) then
  begin
    // FPrevOnRowReplicated := FReplicator.OnRowReplicated;
    //FPrevOnError := FReplicator.OnReplicationError;
    // FReplicator.OnLoadLog := LoadLog;
    // FReplicator.OnRowReplicated := RecordReplicated;
    //FReplicator.OnReplicationError := SignalError;
    FInitiated := True;
  end;
end;

function TCcLog.BuildLogSQL(CheckingConflicts: Boolean; node: TCcNode): string;
begin
  if CheckingConflicts then
    Result := 'select l.login, l.primary_key_values, l.conflict, l.table_name, l.procedure_statement, ' +
      'min(l.operation_date) as min_operation_date, max(l.operation_date) as max_operation_date, count(l.code) as record_count ' +
      'from RPL$LOG l ' +
      'where l.login = :login and l.replication_state is null ' +
      'group by l.login, l.primary_key_values, l.conflict, l.table_name, l.procedure_statement ' +
      'order by l.login,  l.table_name, l.primary_key_values '
  else
  begin
    Result := 'select l.change_number, t.priority, l.table_name, l.primary_key_values, l.code, l.conflict, l.login, ' +
      'l.procedure_statement, l.operation_date, l.primary_key_sync, l.unique_key_sync, ' +
      't.REPL_DELETES as REPL_DELETES, t.REPL_INSERTS as REPL_INSERTS, t.REPL_UPDATES as REPL_UPDATES, ';

//    if FReplicator.ReplicateOnlyChangedFields  then
      Result := Result + 'l.operation_type, ';

    Result := Result + 't.unique_key_sync as generic_unique_key_sync, t.primary_key_sync as generic_PRIMARY_KEY_SYNC, t.unique_key_names ';
    Result := Result + 'from RPL$LOG l ';

    if node.ConfigName = '' then
      Result := Result + 'join RPL$TABLES t on t.table_name = l.table_name '
    else
      Result := Result + 'join RPL$TABLES_CONFIG t on t.table_name = l.table_name and t.config_name = ' + QuotedStr(node.ConfigName) + ' ';

    Result := Result + 'where l.login = :login and l.replication_state is null ';

    if (FReplicator.AutoPriority) or FReplicator.ReplicateOnlyChangedFields then
      Result := Result + 'order by l.change_number, l.code'
    else
      Result := Result + 'order by t.priority,  l.table_name, l.primary_key_values, l.code';
  end;
end;

procedure TCcLog.ReportConflict;
var
  Conflict: TConflictRecord;
begin
  // There's a conflict!
  // We call ResetKeys so that the Keys property reflects the current record...
  ResetKeys(False);

  Conflict.ChangedFields1 := TStringList.Create;
  Conflict.ChangedFields2 := TStringList.Create;
  Conflict.ConflictingFields := TStringList.Create;
  try
    ListChangedFields(FReplicator.RemoteDB, Conflict.ChangedFields1, FLogRemote.Field['LOGIN'].AsString, GetRemoteTableName, FBN('PRIMARY_KEY_VALUES'));
    ListChangedFields(FReplicator.LocalDB, Conflict.ChangedFields2, FLogLocal.Field['LOGIN'].AsString, TableName, FBN('PRIMARY_KEY_VALUES'));
    ExtractItemsInCommon(Conflict.ChangedFields1, Conflict.ChangedFields2, Conflict.ConflictingFields);

    Conflict.Node1 := FReplicator.LocalNode.Name;
    Conflict.Node2 := FReplicator.RemoteNode.Name;
    Conflict.FirstOperationDate1 := FLogLocal.Field['MIN_OPERATION_DATE'].AsDateTime;
    Conflict.FirstOperationDate2 := FLogRemote.Field['MIN_OPERATION_DATE'].AsDateTime;
    Conflict.LastOperationDate1 := FLogLocal.Field['MAX_OPERATION_DATE'].AsDateTime;
    Conflict.LastOperationDate2 := FLogRemote.Field['MAX_OPERATION_DATE'].AsDateTime;
    Conflict.NumChanges1 := FLogLocal.Field['RECORD_COUNT'].AsInteger;
    Conflict.NumChanges2 := FLogRemote.Field['RECORD_COUNT'].AsInteger;

    LogConflict(Conflict);

    if (Conflict.ChosenNode = Conflict.Node2) then
    begin
      FLog := FLogRemote;
      FLogLocal.Next;
      Inc(FLineCount);
    end
    else if (Conflict.ChosenNode = Conflict.Node1) then
    begin
      FLog := FLogLocal;
      FLogRemote.Next;
      Inc(FLineCount);
    end
    else if (Conflict.ChosenNode = 'REPLICATE_BOTH_WAYS') then begin
      Inc(FLineCount, 2);
      FLogRemote.Next;
      FLogLocal.Next;
    end
    else begin
      Inc(FReplicator.LastResult.RowsConflicted);
      FLogRemote.Next;
      FLogLocal.Next;
    end;
  finally
    Conflict.ChangedFields1.Free;
    Conflict.ChangedFields2.Free;
    Conflict.ConflictingFields.Free;
  end;
  FLogRemote.GetQueryObject.AfterClose := nil;
end;

procedure TCcLog.ListChangedFields(conn: TCcConnection; sl: TStringList;  cLogin, cTableName, cPrimaryKeyValues : String);
begin
  sl.Clear;
  if FReplicator.ReplicateOnlyChangedFields then begin
    with conn.SelectQuery['TCcLog_listChangedFields'] do begin
      Close;
      SQL.Text := 'select distinct rlv.field_name '+
                  'from RPL$LOG l '+
                  'join RPL$LOG_VALUES rlv on rlv.change_number = l.change_number and node_name = rlv.node_name '+
                  'where l.table_name = :table_name and l.primary_key_values = :primary_key_values and l.login = :node_name and replication_state is null';
      Param['table_name'].Value := cTableName;
      Param['primary_key_values'].Value := cPrimaryKeyValues;
      Param['node_name'].Value := cLogin;
      Exec;
      while not Eof do begin
        sl.Add(Field['field_name'].AsString);
        Next;
      end;
    end;
  end;
end;

procedure TCcLog.GetLogValues;
begin
  if FLog = FLogLocal then
    FLogValues := FReplicator.LocalDB.SelectQuery['TCcLog_qLogValues']
  else
    FLogValues := FReplicator.RemoteDB.SelectQuery['TCcLog_qLogValues'];

  if not FLog.Eof then begin
    FLogValues.Close;
    FLogValues.SQL.Text := 'select * from RPL$LOG_VALUES where change_number = :change_number and node_name = :node_name';
    FLogValues.Param['change_number'].Value := FLog.Field['change_number'].Value;
    FLogValues.Param['node_name'].Value := FLog.Field['login'].Value;
    FLogValues.Exec;

    if FLogValues.RecordCount = 0 then begin
      DeleteLogLine;
      Next;
    end;
  end;
end;

// Called by TCcReplicator to load log from database
// Returns a boolean to indicate whether there are any more rows in the log
function TCcLog.LoadFromDatabase: Boolean;
// Return the query with the lower value of the provided field
  function GetSmallerQuery(cFieldName: string): TCcQuery;
  begin
    if FLogLocal.Eof then
      Result := FLogRemote
    else if FLogRemote.Eof then
      Result := FLogLocal
    else
    begin
      if FLogLocal.Field[cFieldName].Value < FLogRemote.Field[cFieldName].Value then
        Result := FLogLocal
      else if FLogRemote.Field[cFieldName].Value < FLogLocal.Field[cFieldName].Value then
        Result := FLogRemote
      else
        Result := nil;
    end;
  end;

  procedure NextRow(q: TCcQuery);
  begin
    q.Next;
    Inc(FLineCount);
  end;

  procedure ExecQueries(CheckingConflicts: Boolean);
  begin
    if (FReplicator.Direction = sdLocalToRemote) or (FReplicator.Direction = sdBoth) then begin
      FLogLocal.Close;
      FLogLocal.UnPrepare;
      // We are looking in the local DB for records marked out for the remote DB
      FLogLocal.SQL.Text := BuildLogSQL(CheckingConflicts, FReplicator.RemoteNode);
      FLogLocal.Param['login'].Value := FReplicator.RemoteNode.Name;
      FLogLocal.Exec;
    end;

    if (FReplicator.Direction = sdRemoteToLocal) or (FReplicator.Direction = sdBoth) then begin
      FLogRemote.Close;
      FLogRemote.UnPrepare;
      // We are looking in the remote DB for records marked out for the local DB
      FLogRemote.SQL.Text := BuildLogSQL(CheckingConflicts, FReplicator.LocalNode);
      FLogRemote.Param['login'].Value := FReplicator.LocalNode.Name;
      FLogRemote.Exec;
    end;
  end;

begin
  FKeys.ClearTableKeys;

  FLineCount := 0;
  FCurrentLine := 0;
  FAllowAllOperations := False;

  // Find the name of the configuration that the local node is attached to
  // (if any) in the remote node and vice versa
  if (FReplicator.Direction = sdRemoteToLocal) or (FReplicator.Direction = sdBoth) then
    FReplicator.LocalNode.FindConfigName(FReplicator.RemoteNode);
  if (FReplicator.Direction = sdLocalToRemote) or (FReplicator.Direction = sdBoth) then
    FReplicator.RemoteNode.FindConfigName(FReplicator.LocalNode);

  FLogLocal := FReplicator.LocalDB.SelectQuery['TCcLog_qLogLocal'];
  FLogRemote := FReplicator.RemoteDB.SelectQuery['TCcLog_qLogRemote'];

  if FReplicator.Direction = sdBoth then begin
    ExecQueries(True);

    while not(FLogLocal.Eof and FLogRemote.Eof) do
    begin
      FReplicator.RefreshDisplay;
      // TODO: handle case-sensitive table names (also different databases)
      FLog := GetSmallerQuery('table_name');
      if FLog = nil then
        FLog := GetSmallerQuery('primary_key_values');

      // if FLog is still nil, that means that the current record
      // is the same in both local and remote logs
      if FLog = nil then
      begin
        if FLogLocal.Field['procedure_statement'].AsString <> '' then
        begin
          NextRow(FLogRemote);
          NextRow(FLogLocal);
        end
        else begin
          FLog := FLogLocal;
          ReportConflict;
        end;
      end
      else
        NextRow(FLog);
    end;
  end;

  FReplicator.RefreshDisplay;

  ExecQueries(False);

  FReplicator.RefreshDisplay;

  if FReplicator.Direction = sdLocalToRemote then
    FLog := FLogLocal
  else if FReplicator.Direction = sdRemoteToLocal then
    FLog := FLogRemote
  else begin
    if FLogLocal.Active then begin
      FLog := FLogLocal;
      if FLogLocal.Eof and FLogRemote.Active then
        FLog := FLogRemote;
    end else
      FLog := FLogRemote;
  end;

  if not FLog.Eof then begin
    FCurrentLine := 1;

    //We didn't check the conflicts, so we don't know the record count
    //We could add a property to check record count explicitly
    if FReplicator.Direction <> sdBoth then
      FLineCount := 1;
  end;

  ResetKeys;
  FRowsDone.Clear;
  FRowsInError.Clear;

  if FReplicator.ReplicateOnlyChangedFields then
    GetLogValues;
end;

function TCcLog.RowAlreadyDone: Boolean;
begin
  Result := (FRowsDone.IndexOf(RecordID) <> -1) or (FRowsInError.IndexOf(RecordID) <> -1);
end;

function TCcLog.RowInError: Boolean;
begin
  Result := (FRowsInError.IndexOf(RecordID) <> -1);
end;

procedure TCcLog.SkipToRemote;
begin
  FLog.Close;
end;

procedure TCcLog.Next(lRowSkipped: Boolean);
begin
	FOldRecordID := RecordID;

  if FLog.Active then
    FLog.Next;

  if (not FLogLocal.Active or FLogLocal.Eof) and (FReplicator.Direction = sdBoth) then
    FLog := FLogRemote;

	if not FLog.Eof and not lRowSkipped then
    Inc(FCurrentLine);

  if FLog.Active then
    ResetKeys;

  if FReplicator.ReplicateOnlyChangedFields then
    GetLogValues;
end;

procedure TCcLog.ResetKeys(LoadSyncFields: Boolean);
begin
  if FReplicator.UsePKSynchronization and LoadSyncFields then
    Keys.LoadKeys(FBN('TABLE_NAME'), FBN('PRIMARY_KEY_VALUES'), FBN('PRIMARY_KEY_SYNC'), FBN('UNIQUE_KEY_SYNC'),
      FBN('GENERIC_PRIMARY_KEY_SYNC'), FBN('UNIQUE_KEY_NAMES'), FBN('GENERIC_UNIQUE_KEY_SYNC'))
  else
    Keys.LoadKeys(FBN('TABLE_NAME'), FBN('PRIMARY_KEY_VALUES'), '', '', '', '', '');

  FTableName := FBN('TABLE_NAME');
  FLocalMode := (FLog = FLogLocal);
  FPrimaryKeys := FBN('PRIMARY_KEY_VALUES');
end;

procedure TCcLog.RecordReplicated(Sender: TObject);
begin
  // UpdateState(lsOk);
  if not RowAlreadyDone then
    FRowsDone.Add(RecordID);
  DeleteLogLine;
  // if assigned(FPrevOnRowReplicated) then
  // FPrevOnRowReplicated(FReplicator);
end;

procedure TCcLog.SetLocalMode(Value: Boolean);
begin
  FLocalMode := Value;
end;

procedure TCcLog.SetReplicator(Repl: TCcReplicator);
begin
  FReplicator := Repl;
end;

procedure TCcLog.SignalError(Sender: TObject; e: Exception;
  var CanContinue: Boolean);
begin
  FRowsInError.Add(RecordID);

  // UpdateState(lsError); // On indique que la réplication de l'enregistrement a échoué
//  if (Assigned(FPrevOnError)) then
  //  FPrevOnError(FReplicator, e, CanContinue);
end;

procedure TCcLog.UpdateState(State: TCcLogState);
begin
end;

procedure TCcLog.LoadKeys(cTableName, PrimaryValues, PrimarySync, UniqueSync, GenericPrimarySync, UniqueNames,
  GenericUniqueSync: string);
begin
  FTableName := cTableName;
  FAllowAllOperations := True;
  FPrimaryKeys := PrimaryValues;
  Keys.LoadKeys(cTableName, PrimaryValues, PrimarySync, UniqueSync, GenericPrimarySync, UniqueNames, GenericUniqueSync);
end;

{$IFDEF CC_D2K9}
procedure TBytesFromVariant(const V: Variant; var VArr: TBytes);
var
  ptr: PByte;
begin
  SetLength(VArr, (VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1)) + 1);
  if Length(VArr) > 0 then
  begin
    ptr := PByte(VarArrayLock(V));
    try
      Move(ptr^, VArr[0], Length(VArr));
    finally
      VarArrayUnlock(V);
    end;
  end;
end;
{$ENDIF}

procedure TCcLog.LoadLogValues(DataSet: TCcMemoryTable);
var
  f: TCcMemoryField;

  {$IFDEF CC_D2K9}
  vt: TVarType;
  Bytes: TBytes;
  {$ENDIF}
begin
  DataSet.Active := False;
  DataSet.fields.Clear;
  FMemValues.LoadFromDataSet(FLogValues, False, True, False);
  FMemValues.First;
  while not FMemValues.Eof do
  begin
    DataSet.fields.Add(Trim(FMemValues.FieldByName('Field_name').AsString)).DataType := TFieldType(Integer(FMemValues.FieldByName('field_type').Value));
    FMemValues.Next;
  end;

  DataSet.Active := True;
  DataSet.Append;

  FMemValues.First;
  while not FMemValues.Eof do
  begin
    f := DataSet.FieldByName(Trim(FMemValues.FieldByName('Field_name').AsString));
    if not FMemValues.FieldByName('new_value').IsNull then
      f.Value := Origin.Connection.DBAdaptor.ConvertValue(FMemValues.FieldByName('new_value').Value, f.DataType)
    else begin
       if FMemValues.FieldByName('new_blob_null').AsString = 'Y' then
         f.Clear
       else
        if Length(FMemValues.FieldByName('new_value_blob').AsString) > 0  then begin
          {$IFDEF CC_D2K9}
          TBytesFromVariant(FMemValues.FieldByName('new_value_blob').Value, Bytes);
          if (f.DataType in [ftString, ftFixedChar, ftOraClob, ftMemo, ftFmtMemo]) then
            f.Value := TEncoding.Default.GetString(Bytes)
          else if (f.DataType in [ftWideString, ftFixedWideChar, ftWideMemo]) then
            f.Value := TEncoding.UTF8.GetString(Bytes)
          else
          {$ENDIF}
            f.Value := FMemValues.FieldByName('new_value_blob').Value;
        end
        else
          f.Value := '';
    end;
    FMemValues.Next;
  end;
  FMemValues.Active := False;
end;

procedure TCcLog.LoadFromDataSet(cTableName: string; DataSet: TCcQuery);
// var
// I: Integer;
// cPKVals: String;
begin
  FTableName := cTableName;
  FAllowAllOperations := True;
  FPrimaryKeys := FKeys.LoadKeysFromDataSet(cTableName, DataSet, '', '');
end;

function TCcLog.GetPrimaryKeys: string;
begin
  Result := FPrimaryKeys;
end;

function TCcLog.GetTableName: string;
begin
  Result := FTableName;
end;

end.
