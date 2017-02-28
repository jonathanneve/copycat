unit CcNexusDB;


{$I CC.INC}

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys;

type

  TCcNexusDBAdaptor = class(TCcDBAdaptor)
  private
    function GetDeclaration(DataType: TFieldType; nSize: Integer): string;
    function ConvertToOldGen(cPKGen: string; cNewOld: string): string;
    function StringToDate(cDate: string; lHasTimeInfo: Boolean): TDateTime;
    procedure GetReplicatingNode;
    function ParseSQLCondition(cTable, cCondition: string; cNewOld: string): string;
  protected
    FKeys: TCcKeyRing;
    function GetQuoteMetadata: Boolean; override;

    procedure DoListTables(list: TStringList; IncludeTempTables: Boolean); override;
    procedure DoListTableFields(cTableName: string; list: TStringList); override;
    procedure DoListUpdatableTableFields(cTableName: string; list: TStringList); override;
    procedure DoListPrimaryKeys(cTableName: string; list: TStringList); override;
    procedure DoListTriggers(list: TStringList); override;
    procedure DoListProcedures(list: TStringList); override;
    procedure DoListAllProcedures(list: TStringList); override;

    function MaxDDLNameLength: Integer; override;
    function GetUseRowsAffected: Boolean; override;

    // Called after a new connection has been started
    procedure InitConnection; override;
    function GetCurrentTimeStampSQL: string; override;
  public
    procedure GrantRightsToTable(tableName: String); override;
    function SupportsGenerators: Boolean; override;
    class function GetAdaptorName: string; override;
    function SQLFormatValue(Data: Variant; FieldType: TFieldType): string; override;
    procedure CheckCustomMetadata; override;
    procedure RemoveCustomMetadata; override;

    function ConvertValue(Val: Variant; DataType: TFieldType): Variant; override;
    function DeclareField(FieldName: string; FieldType: TFieldType;
      Length: Integer; NotNull: Boolean; PK: Boolean; AutoInc: Boolean): string; override;
    function DeclarePK(FieldNames: string): string; override;

    function GenDeclared(GenName: string): Boolean; override;
    procedure RemoveTriggers(qTable: TCcQuery); override;
    function GenerateTriggers(qTable: TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer; override;

    constructor Create(Conn: TCcConnection); override;
    destructor Destroy; override;
  end;

implementation

uses {$IFDEF CC_UseVariants}Variants, {$ENDIF}Math;

function TCcNexusDBAdaptor.StringToDate(cDate: string; lHasTimeInfo: Boolean): TDateTime;
begin
  if Copy(cDate, 1, 4) = 'DATE' then
    Result := EncodeDate(StrToInt(Copy(cDate, 6, 4)), StrToInt(Copy(cDate, 11, 2)), StrToInt(Copy(cDate, 14, 2)))
  else if Copy(cDate, 1, 4) = 'TIME' then
  begin
    if Copy(cDate, 1, 9) = 'TIMESTAMP' then
      Result := EncodeDate(StrToInt(Copy(cDate, 6, 4)), StrToInt(Copy(cDate, 11, 2)), StrToInt(Copy(cDate, 14, 2)))
    else
      Result := EncodeDate(StrToInt(Copy(cDate, 6, 4)), StrToInt(Copy(cDate, 11, 2)), StrToInt(Copy(cDate, 14, 2)));
  end
  else
    Result := 0;
end;

function TCcNexusDBAdaptor.SupportsGenerators: Boolean;
begin
  Result := False;
end;

function TCcNexusDBAdaptor.ConvertValue(Val: Variant; DataType: TFieldType): Variant;
begin
  if VarType(Val) = varString then
  begin
    if ((DataType = ftDateTime) or (DataType = ftDate) or (DataType = ftTime)) then
      Result := StringToDate(Val, (DataType <> ftDate))
    else
      Result := Val;
  end
  else
    Result := Val;
end;

// This function is called in order to see if a certain predefined UDF is available on the
// database server.
function TCcNexusDBAdaptor.MaxDDLNameLength: Integer;
begin
  Result := 31;
end;

destructor TCcNexusDBAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCcNexusDBAdaptor.DeclareField(FieldName: string; FieldType: TFieldType;
  Length: Integer; NotNull, PK: Boolean; AutoInc: Boolean): string;
begin
  if AutoInc then
    Result := FieldName + ' AUTOINC'
  else
    Result := FieldName + ' ' + GetDeclaration(FieldType, Length);

  if NotNull then
    Result := Result + ' NOT NULL';
  if PK then
    Result := Result + ' PRIMARY KEY';
end;

// This function is called in order to see if a certain generator exists in the database
// If database does not handle or need generators (for example, if auto-increment fields are used instead)
// then simply return true.
function TCcNexusDBAdaptor.GenDeclared(GenName: string): Boolean;
begin
  { with FConnection.Query['IB_FINDGEN'] do begin
    Close;
    SQL.Text := 'select * from rdb$generators where %upper_case(rdb$generator_name) = %upper_case(:gen_name)';
    if QuoteMetadata then
    Macro['upper_case'].Value := ''
    else
    Macro['upper_case'].Value := 'upper';
    Param['gen_name'].AsString := GenName;
    Exec;
    if RecordCount > 0 then Result := True
    else Result := False;
    end; }
  Result := False;
end;

procedure TCcNexusDBAdaptor.GetReplicatingNode;
begin
  Query.Add('  if exists(select table_name from #tables where upper(table_name) = ''##REPLICATING_NODE'') then ');
  Query.Add('    set replicating_node = (select node from ##replicating_node);');
  Query.Add('  else');
  Query.Add('    set replicating_node = '''';');
  Query.Add('  end if;');
end;

function TCcNexusDBAdaptor.ParseSQLCondition(cTable, cCondition: string; cNewOld: string): string;
begin
  Result := ConvertToOldGen(Trim(cCondition), cNewOld);
  Result := ReplaceString(Result, '%%TABLE_NAME', Trim(cTable));
end;

function TCcNexusDBAdaptor.GenerateTriggers(qTable: TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;
var
  cTriggerName: string;
  TableName, ConfigName: string;

  procedure DeclareVariables(cType: string);
  begin
    Query.Add('declare primary_key_values clob;');
    Query.Add('declare user_login varchar(50);');
    Query.Add('declare repl_operation char(1);');
    Query.Add('declare replicating_node varchar(50);');
  end;

  function CheckFieldRestrictions: Boolean;
  begin
    Result := False;
    { Result := False;
      if Assigned(qTableConf) and (qTableConf.RecordCount > 0) then begin
      slIncludedFields := TStringList.Create;
      try
      slIncludedFields.CommaText := qTableConf.Field['included_fields'].AsString;
      if (slIncludedFields.Count > 0) then begin
      Query.Add('if ((rdb$get_context(''USER_TRANSACTION'', ''FORCE_REPLICATION'') = ''TRUE'') or ');
      for I := 0 to slIncludedFields.Count -1 do begin
      if (I > 0) then
      Query.Add(' or ');
      Query.Add('(old.' + slIncludedFields[i] + ' is distinct from new.' + slIncludedFields[i] + ')');
      end;
      Query.Add(') then begin');
      Result := True;
      end;
      finally
      slIncludedFields.Free;
      end;
      end; }
  end;

  procedure DoGenerateSQL(cNewOld: string; cOperationType: string);
  var
    I: Integer;
    lConditionField: Boolean;
    lCondition: Boolean;
    cPK: string;

  begin
    lConditionField := (Trim(qTable.Field['CONDITION_FIELD'].AsString) <> '');
    lCondition := (Trim(qTable.Field['CONDITION'].AsString) <> '');

    cPK := '';
    for I := 0 to FKeys.Count - 1 do
    begin
      if (FKeys[I].PrimaryKey) then
      begin
        if cPK <> '' then
          cPK := cPK + ' || ';
        cPK := cPK + 'rpl$quotedstr(tostring(' + cNewOld + '.' + MetaQuote(FKeys[I].KeyName) + ')) || '';''';
      end;
    end;
    Query.Add('  set primary_key_values = ' + cPK + ';');

    Query.Add('  declare node_cursor cursor for select u.login, t.repl_' + cOperationType + 's');
    Query.Add('    from RPL$users u');
    if (ConfigName <> '') then
      Query.Add('    join rpl$tables_config t on t.table_name = ' + QuotedStr(TableName) + ' and t.config_name = ' + QuotedStr(ConfigName))
    else
      Query.Add('    join rpl$tables t on t.table_name = ' + QuotedStr(TableName));

    Query.Add('    where ((u.login <> replicating_node) or (replicating_node is null))');

    if (ConfigName <> '') then
      Query.Add('    and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

    // User-definable SQL condition, configurable per table in RPL$TABLES
    if lCondition then
      Query.Add('    and (' + ParseSQLCondition(TableName, qTable.Field['CONDITION'].AsString, cNewOld) + ')');

    if Assigned(qTableConf) then
    begin
      if Trim(qTableConf.Field['CONDITION'].AsString) <> '' then
        Query.Add('    and (' + ParseSQLCondition(TableName, qTableConf.Field['CONDITION'].AsString, cNewOld) + ')');

      if Trim(qTableConf.Field[cOperationType + '_CONDITION'].AsString) <> '' then
        Query.Add('    and (' + ParseSQLCondition(TableName, qTableConf.Field[cOperationType + '_CONDITION'].AsString, cNewOld) + ')');
    end;

    if lConditionField then
      // If the condition field is null, the record is not replicated to any user
      // If RPL$USERS.Condition_value is null, the user gets all changes
      Query.Add('    and ((' + cNewOld + '.' + MetaQuote(Trim(qTable.Field['CONDITION_FIELD'].AsString)) + ' = u.condition_value) or (u.condition_value is null))');

    Query.Text := Trim(Query.Text);
    Query.Add(';');

    Query.Add('  open node_cursor;');
    Query.Add('  fetch first from node_cursor into user_login, repl_operation;');
    Query.Add('  while @@fetch_status = 0 do');
    Query.Add('    if (repl_operation = ''Y'') then');
    Query.Add('      insert into rpl$log(login, operation_date, table_name, primary_key_values, conflict, sent_from)');
    Query.Add('      values(user_login, current_timestamp, ' + QuotedStr(TableName) + ', primary_key_values, ''N'', replicating_node);');
    Query.Add('      fetch next from node_cursor into user_login, repl_operation;');
    Query.Add('    end if;');
    Query.Add('  end while;');
    Query.Add('  close node_cursor;'); // Fix, KM: Need to close cursor after open, fetch
  end;

var
  lFieldRestrictions: Boolean;

begin
  if not FConnection.InTransaction then
    FConnection.StartTransaction;

  TableName := Trim(qTable.Field['TABLE_NAME'].AsString);
  if Assigned(qTableConf) then
  begin
    cTriggerName := Trim(qTableConf.Field['TRIG_BASE_NAME'].AsString);
    ConfigName := Trim(qTableConf.Field['CONFIG_NAME'].AsString);
  end
  else
    cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString);

  FKeys.FailIfNoPK := FailIfNoPK;
  with qTable do
    FKeys.LoadKeys(TableName, '', '', '', Field['PRIMARY_KEY_SYNC'].AsString,
      Field['UNIQUE_KEY_NAMES'].AsString, Field['UNIQUE_KEY_SYNC'].AsString);

  Query.Add('create trigger ' + MetaQuote(cTriggerName + '_I') + ' AFTER INSERT on ' + MetaQuote(TableName));
  Query.Add('begin');
  DeclareVariables('I');
  GetReplicatingNode;
  DoGenerateSQL('new', 'insert');
  Query.Add('end');
  ExecConfQuery;

  Query.Add('create trigger ' + MetaQuote(cTriggerName + '_U') + ' AFTER UPDATE on ' + MetaQuote(TableName));
  Query.Add('begin');
  DeclareVariables('U');
  GetReplicatingNode;
  lFieldRestrictions := CheckFieldRestrictions;
  DoGenerateSQL('old', 'update');
  DoGenerateSQL('new', 'update');
  if (lFieldRestrictions) then
    Query.Add('end');
  Query.Add('end');
  ExecConfQuery;

  Query.Add('create trigger ' + MetaQuote(cTriggerName + '_D') + ' AFTER DELETE on ' + MetaQuote(TableName));
  Query.Add('begin');
  DeclareVariables('D');
  GetReplicatingNode;
  DoGenerateSQL('old', 'delete');
  Query.Add('end');
  ExecConfQuery;
end;

procedure TCcNexusDBAdaptor.RemoveCustomMetadata;
begin
  if ProcedureExists('RPL$QUOTEDSTR') then
  begin
    Query.Clear;
    Query.Add('drop function RPL$QUOTEDSTR');
    ExecConfQuery;
  end;
end;

procedure TCcNexusDBAdaptor.RemoveTriggers(qTable: TCcQuery);

  procedure RemoveTrigger(cType: string);
  var
    cTriggerName: string;
  begin
    cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString) + '_' + cType;
    if (TriggerExists(cTriggerName)) then
    begin
      Query.Add('DROP TRIGGER ' + MetaQuote(cTriggerName));
      ExecConfQuery;
    end;
  end;

begin
  RemoveTrigger('I');
  RemoveTrigger('U');
  RemoveTrigger('D');
end;

procedure TCcNexusDBAdaptor.CheckCustomMetadata;
begin
  inherited;
  if not ProcedureExists('RPL$QUOTEDSTR') then
    with Query do
    begin
      Add('CREATE function RPL$QUOTEDSTR (s clob) RETURNS clob ');
      Add('begin');
      Add('declare nStringLength integer;');
      Add('declare i integer;');
      Add('declare c varchar(2);');
      Add('declare quoted_str clob;');
      Add('  set quoted_str = '''';');
      Add('  set i = 1;');
      Add('  set nStringLength = char_length(s);');
      Add('  while (i <= nStringLength) do');
      Add('    set c = substring(s from i for 1);');
      Add('    if (c = '''''''') then');
      Add('      set c = '''''''''''';');
      Add('    end if;');
      Add('    set quoted_str = quoted_str || c;');
      Add('    set i = i + 1;');
      Add('  end while;');
      Add('  return '''''''' || quoted_str || '''''''';');
      Add('end;');
      ExecConfQuery;
    end;
end;

function TCcNexusDBAdaptor.ConvertToOldGen(cPKGen: string; cNewOld: string): string;
begin
  Result := ReplaceString(cPKGen, 'new.', cNewOld + '.');
end;

function TCcNexusDBAdaptor.GetCurrentTimeStampSQL: string;
begin
  Result := 'current_timestamp';
end;

function TCcNexusDBAdaptor.GetDeclaration(DataType: TFieldType; nSize: Integer): string;
begin
  case (DataType) of
    ftInteger:
      Result := 'INTEGER';
    ftFloat:
      Result := 'FLOAT';
    ftDate:
      Result := 'DATE';
    ftTime:
      Result := 'TIME';
    ftDateTime:
      Result := 'TIMESTAMP';
    ftString:
      Result := 'VARCHAR(' + InttoStr(nSize) + ')';
    ftFixedChar:
      Result := 'CHAR(' + InttoStr(nSize) + ')';
    ftBlob:
      Result := 'BLOB';
    ftMemo:
      Result := 'CLOB';
  else
    raise Exception.Create('Data type ' + InttoStr(Integer(DataType)) + ' not handled by TCcNexusDBAdaptor!');
  end;
end;

constructor TCcNexusDBAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['3.12']);
end;

procedure TCcNexusDBAdaptor.InitConnection;
begin
  inherited;
  with FConnection.UpdateQuery['NX_SetReplicatingNode'] do
  begin
    Close;
    SQL.Text := 'drop table if exists ##replicating_node; create table ##replicating_node(node varchar(50)); insert into ##replicating_node(node) values (:node)';
    Param['node'].Value := FConnection.ReplicatingNode;
    Exec;
  end;
end;

// Cette fonction sert à mettre entre guillemets les dates et les chaînes de caractères
// Si cData est vide, on renvoie 'null'
function TCcNexusDBAdaptor.SQLFormatValue(Data: Variant; FieldType: TFieldType): string;
var
  cData: string;
  DataType: Integer;
begin
  if VarIsEmpty(Data) or VarIsNull(Data) then
    Result := 'null'
  else
  begin
    cData := Data;
    if Trim(cData) = '' then
      Result := 'null'
    else
    begin
      DataType := VarType(Data);
      if (DataType = varString) then
        Result := QuotedStr(Data)
      else if (DataType = varDate) then
      begin
        if FieldType = ftDateTime then
          Result := 'TIMESTAMP''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', TDateTime(Data)) + ''''
        else if (FieldType = ftDate) then
          Result := 'DATE''' + FormatDateTime('yyyy-mm-dd', TDateTime(Data)) + ''''
        else if (FieldType = ftTime) then
          Result := 'TIME''' + FormatDateTime('hh:nn:ss', TDateTime(Data)) + ''''
        else if (FieldType = ftGuid) then // Added by Kick Martens
          Result := 'GUID' + QuotedStr(Data)
      end
      else
        Result := Data;
    end;
  end;
end;

class function TCcNexusDBAdaptor.GetAdaptorName: string;
begin
  Result := 'NexusDB';
end;

function TCcNexusDBAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := True;
end;

procedure TCcNexusDBAdaptor.GrantRightsToTable(tableName: String);
begin
  //Do nothing (grants not implemented)
end;

procedure TCcNexusDBAdaptor.DoListPrimaryKeys(cTableName: string; list: TStringList);
begin
  // with TCcQuery.Create(FConnection, '') do begin
  with FConnection.SelectQuery['NXAdaptor.DoListPrimaryKeys'] do
  begin
    Close;
    UnPrepare;
    // Fix KM: use metadata of indexfields and looking at indexAllowsDups=NO to determine primary index instead of "default" prop of index
    SQL.Text := 'select ixf.segment_field as pk_name, ixf.segment_index ' + #13#10 +
      'from (select top 1 i.Table_index, i.index_index ' + #13#10 +
      '      from #indexes i' + #13#10 +
      '      join #indexfields f on (i.table_index=f.table_index and i.index_index=f.index_index)' + #13#10 +
      '      where (i.index_allowsdups = ''NO'' or i.index_allowsdups = ''NO (NULLS IGNORED)'' ) ' + #13#10 +
      '      and upper(i.table_name) = upper( :table_name ) ) i' + #13#10 +
      'join #indexfields ixf on (ixf.index_index = i.index_index and ixf.table_index = i.table_index)' + #13#10 +
      'order by ixf.segment_index';
    Param['table_name'].AsString := Trim(cTableName);
    Exec;
    while not Eof do
    begin
      list.Add(Trim(Field['pk_name'].AsString));
      Next;
    end;
    // Free;
  end;
end;

procedure TCcNexusDBAdaptor.DoListProcedures(list: TStringList);
begin
  with FConnection.SelectQuery['NXAdaptor.DoListProcedures'] do
  begin
    Close;
    SQL.Text := 'select procedure_name from #procedures ' +
      'order by 1';
    Exec;

    while not Eof do
    begin
      list.Add(Trim(Field['procedure_name'].AsString));
      Next;
    end;
  end;
end;

procedure TCcNexusDBAdaptor.DoListAllProcedures(list: TStringList);
begin
  with FConnection.SelectQuery['NXAdaptor.DoListAllProcedures'] do
  begin
    Close;
    SQL.Text := 'select procedure_name from #procedures ' +
      'union all ' +
      'select function_name from #functions ' +
      'order by 1 ';
    Exec;

    while not Eof do
    begin
      list.Add(Trim(Field['procedure_name'].AsString));
      Next;
    end;
  end;
end;

procedure TCcNexusDBAdaptor.DoListTableFields(cTableName: string;
  list: TStringList);
begin
  with FConnection.SelectQuery['NXAdaptor.DoListTableFields'] do
  begin
    Close;
    SQL.Text := 'select field_name from #fields ' +
      'where upper(table_name) = upper(:table_name)';
    Param['table_name'].AsString := Trim(cTableName);
    Exec;
    while not Eof do
    begin
      list.Add(Trim(Field['field_name'].AsString));
      Next;
    end;
  end;
end;

procedure TCcNexusDBAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
  with FConnection.SelectQuery['NXAdaptor.DoListTables'] do
  begin
    Close;
    SQL.Text := 'select table_name from #tables ' +
      ' where table_type = ''BASE TABLE'' order by 1';
    Exec;
    while not Eof do
    begin
      list.Add(Trim(Field['table_name'].AsString));
      Next;
    end;
  end;
end;

procedure TCcNexusDBAdaptor.DoListTriggers(list: TStringList);
begin
  with FConnection.SelectQuery['NXAdaptor.DoListTriggers'] do
  begin
    Close;
    SQL.Text := 'select name as trigger_name from $SQL$TRIGGERS';
    Exec;
    while not Eof do
    begin
      list.Add(Trim(Field['trigger_name'].AsString));
      Next;
    end;
  end;
end;

procedure TCcNexusDBAdaptor.DoListUpdatableTableFields(
  cTableName: string; list: TStringList);
begin
  with FConnection.SelectQuery['NXAdaptor.DoListUpdatableTableFields'] do
  begin
    Close;
    SQL.Text := 'select field_name from #fields ' +
      'where upper(table_name) = upper(:table_name)';
    Param['table_name'].AsString := Trim(cTableName);
    Exec;
    while not Eof do
    begin
      list.Add(Trim(Field['FIELD_NAME'].AsString));
      Next;
    end;
  end;
end;

function TCcNexusDBAdaptor.DeclarePK(FieldNames: string): string;
begin
  Result := 'primary key (' + FieldNames + ')';
end;

function TCcNexusDBAdaptor.GetQuoteMetadata:Boolean;
begin
  Result := True;
end;

end.
