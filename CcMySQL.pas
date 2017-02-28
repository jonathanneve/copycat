unit CcMySQL;

{$I CC.INC}

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys;

type

TCcMySQLAdaptor = class (TCcDBAdaptor)
	private
		function GetParamDecl(DataType: String; nSize, nScale: Integer): String;
		function GetDeclaration(DataType: TFieldType; nSize: Integer): String;
		function ConvertToOldGen(cPKGen: String; cNewOld: String): String;
		function QuoteSQLData(cData: String; DataType: String;
			lSQLStyle: Boolean): String;
		function GetDatabaseName(): string;
		function StringToDate(cDate: String; lHasTimeInfo: Boolean): TDateTime;
		function ParseSQLCondition(qTable: TCcQuery; cNewOld: String): String;
		property FDatabaseName: string read GetDatabaseName;
	protected
		FKeys : TCcKeyRing;

		procedure DoListTables(list: TStringList; IncludeTempTables: Boolean); override;
		procedure DoListTableFields(cTableName: String; list: TStringList);override;
		procedure DoListUpdatableTableFields(cTableName: String; list: TStringList);override;
		procedure DoListPrimaryKeys(cTableName: String; list: TStringList);override;
		procedure DoListTriggers(list: TStringList);override;
		procedure DoListProcedures(list: TStringList);override;
		procedure DoListAllProcedures(list: TStringList);override;

//    function GetSQL(SQLType: TCcMetaSQL ): String;override;
		function MaxDDLNameLength: Integer;override;
		procedure InitConnection;override;
		function GetGeneratorValue(GenName: String; Increment: Integer):
      {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}override;
		function GetCurrentTimeStampSQL:String;override;
		function DoMetaQuote(Identifier: String): String;override;
		function GetUseRowsAffected: Boolean; override;
		function GetQuoteMetadata:Boolean;override;
	public
    procedure GrantRightsToTable(tableName: String);override;
    function SupportsGenerators: Boolean;override;
		function SQLFormatValue(Data: Variant; FieldType :TFieldType): String;override;
		procedure CreateProcedures;override;
		procedure CheckCustomMetadata;override;
    procedure RemoveCustomMetadata;override;
    procedure DropProcedures;override;

		function ConvertValue(Val: Variant; DataType: TFieldType): Variant;override;
		function DeclareField(FieldName: String; FieldType: TFieldType;
			Length: Integer; NotNull: Boolean; PK: Boolean; AutoInc: Boolean): String;override;

		function GenDeclared(GenName:String): Boolean;override;
		procedure RemoveTriggers(qTable: TCcQuery);override;
		function GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;override;

		procedure GetProcParams(ProcName: String;
			Params: TDataSet; InputParam: Boolean);override;

		procedure DeclareGenerator(GenName:String);override;
		function GetGenerator(GenName: String; Increment: Integer): String;override;
//    function GetGeneratorSQL(GenName: String; Increment: Integer): String;override;
		constructor Create(Conn: TCcConnection);override;
    destructor Destroy;override;

    class function GetAdaptorName: String;override;
end;

implementation

uses
	CcConf

{$IFDEF CC_UseVariants} , Variants {$ENDIF} ;

{function TCcMySQLAdaptor.GetSQL(SQLType: TCcMetaSQL): String;
begin
  if (FDatabaseName = '') then begin
    FDatabaseName := Self.GetDatabaseName();
    if (FDatabaseName = '') then
      Exception.Create('Unabled to find database name !');
	end;

  case (SQLType) of
		sqlTables:
			Result := 'select table_name from information_schema.tables where table_schema = ''' + FDatabaseName + ''' and table_type=''BASE TABLE'' order by table_name';
		sqlTableFields:
			Result := 'select column_name as field_name from information_schema.columns ' +
								'where table_schema = ''' + FDatabaseName + ''' and table_name = :table_name ' +
								'order by column_name';
		 sqlUpdatableFields:
			Result := 'select column_name as field_name from information_schema.columns ' +
								'where table_schema = ''' + FDatabaseName + ''' and table_name = :table_name ' +
								'and ( (extra = '''') or (extra = ''auto_increment'' and column_key = ''PRI''));';
		sqlTablePKs:
			Result := 'select column_name as pk_name from information_schema.key_column_usage where table_schema = ''' + FDatabaseName +
								''' and constraint_name = ''PRIMARY'' and table_name = :table_name order by ordinal_position';
		sqlProcedures:
			Result := 'select r.routine_name as procedure_name from information_schema.routines r  where r.routine_schema = ''' + FDatabaseName + ''' and r.routine_type=''PROCEDURE'' order by r.routine_name';
		sqlFindTrigger:
			Result:='select count(*) as rec_count from information_schema.triggers where trigger_schema = ''' + FDatabaseName + ''' and upper(trigger_name) = :trigger_name';
		sqlFindTableField:
			Result := 'select count(*) as rec_count from information_schema.columns ' +
								'where table_schema = ''' + FDatabaseName + ''' and table_name = :table_name ' +
								'and column_name = :field_name';
		sqlFindProc:
			Result:='select count(*) as rec_count from information_schema.routines where routine_schema = ''' + FDatabaseName + ''' and upper(routine_name) = :proc_name';
		sqlFindTable:
      Result:='select count(*) as rec_count from information_schema.tables where table_schema = ''' + FDatabaseName + ''' and upper(table_name) = :table_name';
    else
      raise Exception.Create('CopyCat SQLServer adaptor not up-to-date (SQL constant unknown)!');
  end;
end;
}
function TCcMySQLAdaptor.MaxDDLNameLength: Integer;
begin
  Result := 31;
end;

destructor TCcMySQLAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCcMySQLAdaptor.DeclareField(FieldName: String; FieldType: TFieldType;
  Length: Integer; NotNull, PK: Boolean; AutoInc: Boolean): String;
begin
  Result := '`' + FieldName + '` ' + GetDeclaration(FieldType, Length);

  if NotNull then
    Result := Result + ' NOT NULL';
  if AutoInc then
    Result := Result + ' AUTO_INCREMENT';
  if PK then
    Result := Result + ', PRIMARY KEY (`' + FieldName + '`)';
end;

procedure TCcMySQLAdaptor.DeclareGenerator(GenName: String);
begin
  Query.Clear;
  Query.Add('insert into RPL$GENERATORS(name, value) values (' + QuotedStr(GenName) + ', 1)');
  ExecConfQuery;
end;

function TCcMySQLAdaptor.GenDeclared(GenName: String): Boolean;
begin
  with FConnection.SelectQuery['MySQL_CheckGenDeclared'] do begin
    Close;
    SQL.Text := 'select count(*) rec_count from RPL$GENERATORS where name = :gen_name';
    Param['gen_name'].Value := GenName;
    Exec;
    Result := (Field['rec_count'].Value >= 1);
  end;
end;

(*
procedure TCcMySQLAdaptor.GenerateProcedure(qProcedure:TCcQuery; Params: TDataSet);
var
  cParamsWithTypes, cParamNamesAssignment, cParamValues, cParamName, NewProcName, ProcName:String;
  cParamDecl: string;
begin
  ProcName := qProcedure.Field['PROCEDURE_NAME'].AsString;
  NewProcName := qProcedure.Field['NEW_PROCEDURE_NAME'].AsString;

  Params.First;
  while (not Params.Eof) do begin
    cParamName := '@' + Trim(Params.FieldByName('PARAM_NAME').AsString);
    cParamDecl := GetParamDecl(Params.FieldByName('FIELD_DECL').AsString,
      Params.FieldByName('FIELD_LENGTH').AsInteger, Params.FieldByName('FIELD_SCALE').AsInteger);//  GetDeclaration(ParamType, Params.FieldByName('FIELD_LENGTH').AsInteger);

    if (cParamNamesAssignment <> '') then cParamNamesAssignment := cParamNamesAssignment + ', ';
    if (cParamsWithTypes <> '') then cParamsWithTypes := cParamsWithTypes + ', ';
    if (cParamValues <> '') then cParamValues := cParamValues + ' + '', '' + ';

    cParamValues := cParamValues + cParamName + ' = ' + QuoteSQLData(cParamName, Params.FieldByName('FIELD_DECL').AsString, true);
    cParamNamesAssignment := cParamNamesAssignment + cParamName + ' = ' + cParamName;
    cParamsWithTypes := cParamsWithTypes + cParamName + ' ' + cParamDecl;
    Params.Next;
  end;


  Query.Add('CREATE PROCEDURE ' + NewProcName + '(' + cParamsWithTypes + ')');
  Query.Add('AS');
  Query.Add('BEGIN');
  Query.Add('  DECLARE @PRIMARY_KEY_VALUES varchar(500)');
  Query.Add('  DECLARE @EXEC_PROC_STATEMENT VARCHAR(500)');
  Query.Add('  DECLARE @EXEC_PROC_STATEMENT_QUOTED varchar(500)');
  Query.Add('  SELECT QUOTED_STR FROM RPL$QUOTE_STR(cast(GEN_ID(GEN_' + copy(NewProcName, 1, 27) + ', 1) as varchar(1000))) into :PRIMARY_KEY_VALUES;');
  Query.Add('  SET @EXEC_PROC_STATEMENT = quotename(''EXEC ' + NewProcName + ''' + ' + cParamValues + ', '''''''')');
  Query.Add('  EXEC ' + ProcName + cParamNamesAssignment);
  Query.Add('  EXEC RPL$GENERATE_LOG @TABLE_NAME = ' + QuotedStr(ProcName) + ', @PRIMARY_KEY_VALUES = @PRIMARY_KEY_VALUES, @REF_VALUE = null, @PROCEDURE_STATEMENT = @EXEC_PROC_STATEMENT');
  Query.Add('END');
  ExecConfQuery;
end;
*)

function TCcMySQLAdaptor.ParseSQLCondition(qTable: TCcQuery; cNewOld: String): String;
begin
  Result := ConvertToOldGen(Trim(qTable.Field['CONDITION'].AsString), cNewOld);
  Result := ReplaceString(Result, '%%TABLE_NAME', Trim(qTable.Field['TABLE_NAME'].AsString));
end;

function TCcMySQLAdaptor.GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;
var
  cTriggerName: String;
  TableName, cSync, ConfigName: String;
  IsDeclaredOnce: boolean;

  procedure DeclareVariables(cType: String);
  begin
    Query.Add('  declare `cr_end` int default 0;');
    Query.Add('  declare `primary_key_values` varchar(500);');
    Query.Add('  declare `user_login` varchar(50);');
    Query.Add('  declare `repl_operation` char(1);');
//    Query.Add('  declare `replicating_node` varchar(50);');
    Query.Add('  declare `primary_key_sync` varchar(5000);');
		Query.Add('  declare `unique_key_sync` varchar(5000);');
    if cType = 'U' then
			Query.Add('  declare `old_primary_key_values` varchar(500);');
  end;

  procedure DoGenerateSQL(cNewOld: String; cOperationType: String);
  var
    I: Integer;
    cRefField: String;
		lRefField: Boolean;
    lConditionField: Boolean;
    lCondition : Boolean;
    lCheckPKChanged : Boolean;
    cCondition: String;
  begin
    lCheckPKChanged := (cNewOld = 'NEW') and (cOperationType = 'update');
    lConditionField := (Trim(qTable.Field['CONDITION_FIELD'].AsString) <> '');
    lCondition := (Trim(qTable.Field['CONDITION'].AsString) <> '');
    cRefField := 'null';

		if (not IsDeclaredOnce) then begin
      IsDeclaredOnce := true;
      //Convert 'new.' to 'old.', if applicable
      if lCondition then
        cCondition := ParseSQLCondition(qTable, cNewOld);

      Query.Add('  declare `cr_nodes` CURSOR for select u.login, t.repl_inserts');
			Query.Add('                            from RPL$USERS u');
      if (ConfigName <> '') then
        Query.Add('    join RPL$TABLES_CONFIG t on t.table_name = ' + QuotedStr(TableName) + ' and t.config_name = ' + QuotedStr(ConfigName))
      else
			Query.Add('                            join RPL$TABLES t on t.table_name = ' + QuotedStr(TableName));

			Query.Add('                            where (u.login <> @COPYCAT_REPLICATING_NODE or @COPYCAT_REPLICATING_NODE is null)');
			if (ConfigName      <> '') then
				Query.Add('    												 and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

			//User-definable SQL condition, configurable per table in RPL$TABLES
      if lCondition then
        Query.Add('                            and (' + cCondition + ')');

      //If the condition field is null, the record is not replicated to any user
      //If RPL$USERS.Condition_value is null, the user gets all changes
      if lConditionField then
        Query.Add('                            and ((' + cNewOld + '.' + MetaQuote(Trim(qTable.Field['CONDITION_FIELD'].AsString)) + ' = u.condition_value) or (u.condition_value is null))');

      Query.Add('                            ;');
			Query.Add('');
      Query.Add('  DECLARE CONTINUE HANDLER FOR NOT FOUND SET cr_end = 1;');
		end;


    Query.Add('  SET cr_end = 0;');
    Query.Add('  SET primary_key_values = '''';');
    for I := 0 to FKeys.Count - 1 do begin
      if (FKeys[i].PrimaryKey) then
				Query.Add('  SET primary_key_values = CONCAT(primary_key_values, quote(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + '), '';'');')
    end;

    Query.Add('  SET primary_key_sync = '''';');
    Query.Add('  SET unique_key_sync = '''';');
    if (cOperationType <> 'delete') then begin
      for I := 0 to FKeys.Count - 1 do begin
				cSync := FKeys[i].GenericSyncStatement;
        if (Copy(cSync, 1, 1) <> '''') then
          cSync := QuotedStr(cSync);
        if (FKeys[i].PrimaryKey) then
          Query.Add('  SET primary_key_sync = CONCAT(primary_key_sync, quote(' + cSync + '), '';'');')
        else
          Query.Add('  SET unique_key_sync = CONCAT(unique_key_sync, quote(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + '), '';'');');
      end;
    end;

    if lCheckPKChanged then begin
      Query.Add('  if ((primary_key_values <> old_primary_key_values)');
      Query.Add('  ) then');
    end;

    // fetch
    Query.Add('  OPEN cr_nodes;');
    Query.Add('  repeat');
    Query.Add('    fetch cr_nodes into user_login, repl_operation;');
    Query.Add('    if (repl_operation = ''Y'') and (cr_end = 0) then');
    Query.Add('      CALL RPL$GENERATE_LOG (' + QuotedStr(TableName) + ', primary_key_values, ' + cRefField + ', null, user_login, primary_key_sync, unique_key_sync);');
    Query.Add('    end if;');
    Query.Add('    until cr_end = 1');
    Query.Add('    end repeat;');
    Query.Add('    CLOSE cr_nodes;');
    if lCheckPKChanged then
      Query.Add('end if;');
  end;

begin
	if not FConnection.InTransaction then
    FConnection.StartTransaction;

  TableName := Trim(qTable.Field['TABLE_NAME'].AsString);

  if Assigned(qTableConf) then begin
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
  Query.Add('FOR EACH ROW begin');
  DeclareVariables('I');
  IsDeclaredOnce := false;
  DoGenerateSQL('NEW', 'insert');
  Query.Add('END');
  ExecConfQuery;

  Query.Add('create trigger ' + MetaQuote(cTriggerName + '_U') + ' AFTER UPDATE on ' + MetaQuote(TableName));
  Query.Add('FOR EACH ROW begin');
  IsDeclaredOnce := false;
  DeclareVariables('U');
  DoGenerateSQL('OLD', 'update');
  Query.Add('');
  Query.Add('  SET cr_end = 0;');
  Query.Add('  SET old_primary_key_values = primary_key_values;');
  DoGenerateSQL('NEW', 'update');
  Query.Add('END');
  ExecConfQuery;

  Query.Add('create trigger ' + MetaQuote(cTriggerName + '_D') + ' AFTER DELETE on ' + MetaQuote(TableName));
  Query.Add('FOR EACH ROW begin');
  IsDeclaredOnce := false;
  DeclareVariables('D');
  DoGenerateSQL('OLD', 'delete');
  Query.Add('END');
  ExecConfQuery;
end;

procedure TCcMySQLAdaptor.RemoveTriggers(qTable: TCcQuery);

	procedure RemoveTrigger(cType: String);
	var
		cTriggerName: String;
	begin
		cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString) + '_' + cType;
		if (TriggerExists(cTriggerName)) then begin
			Query.Add('DROP TRIGGER ' + cTriggerName);
			ExecConfQuery;
		end;
	end;
begin
	RemoveTrigger('I');
  RemoveTrigger('U');
  RemoveTrigger('D');
end;

procedure TCcMySQLAdaptor.InitConnection;
begin
  inherited;
	with FConnection.UpdateQuery['MySQL_SetReplicatingNode'] do begin
    Close;
    SQL.Text := 'set @COPYCAT_REPLICATING_NODE = ' + QuotedStr(FConnection.ReplicatingNode) + ';';
    Exec;
  end;
end;

procedure TCcMySQLAdaptor.RemoveCustomMetadata;
begin
  if TableExists('RPL$GENERATORS') then begin
    Query.Clear;
    Query.Add('drop table RPL$GENERATORS');
    ExecConfQuery;
  end;

  if ProcedureExists('RPL$GEN_ID') then begin
    Query.Clear;
    Query.Add('drop PROCEDURE RPL$GEN_ID');
    ExecConfQuery;
  end;
end;


procedure TCcMySQLAdaptor.GrantRightsToTable(tableName: String);
begin
  //Do nothing : MySQL doesn't allow to grant access to all users at once
end;

procedure TCcMySQLAdaptor.CheckCustomMetadata;
begin
  if not TableExists('RPL$GENERATORS') then begin
    Query.Clear;
    Query.Add('CREATE table RPL$GENERATORS(');
    Query.Add('  name varchar(100) primary key,');
    Query.Add('  value int)');
    ExecConfQuery;
  end;

  //RPL$GEN_ID is used for simulating a Firebird generator using MSSQL.
  //This procedure must always be called within an independant transaction,
  //so that it can be committed and the locks released as fast as possible.
  if not ProcedureExists('RPL$GEN_ID') then begin
    Query.Add('CREATE PROCEDURE RPL$GEN_ID');
    Query.Add('(');
    Query.Add('  GENERATOR_NAME VARCHAR(100),');
    Query.Add('  INCREMENT INT');
    Query.Add(')');
    Query.Add('begin');
    Query.Add('  update RPL$GENERATORS set value = last_insert_id(value + increment) where name = generator_name;');
    Query.Add('  select last_insert_id() as next_value;');
    Query.Add('end');
    ExecConfQuery;
  end;
end;

function TCcMySQLAdaptor.ConvertToOldGen(cPKGen: String; cNewOld: String):String;
begin
  Result := ReplaceString(cPKGen, 'new.', cNewOld + '.');
end;

procedure TCcMySQLAdaptor.GetProcParams(ProcName: String; Params: TDataSet; InputParam: Boolean);
var
  nParamType:Integer;
begin
  if InputParam then nParamType := 0
                else nParamType := 1;
  Params.Close;
  Params.Open;
  if (ProcName <> '') then
	with FConnection.SelectQuery['MySQL_GetProcParams'] do
  begin
    Close;
    SQL.Text :='select o.name, c.name as param_name, c.prec as field_length, c.scale as field_scale, t.name as ftype '+
         'from sysobjects o, syscolumns c, systypes t '+
         'where c.id=o.id '+
         'and t.xtype=c.xtype '+
         'and o.type=''P'' '+
//         'and o.status >0 '+
         'order by o.name';

    Param['procedure_name'].Value := ProcName;
    Param['param_type'].Value := nParamType;
    Exec;
    while (not Eof) do
      begin
      Params.Append;

      Params.FieldByName('PARAM_NAME').AsString := Trim(Field['PARAM_NAME'].AsString);
      Params.FieldByName('FIELD_DECL').AsString := Field['FTYPE'].AsString;
      Params.FieldByName('FIELD_LENGTH').AsInteger := Field['FIELD_LENGTH'].AsInteger;
      Params.FieldByName('FIELD_SCALE').AsInteger := Field['FIELD_SCALE'].AsInteger;
      Params.Post;
      Next;
    end;
  end;
end;

function TCcMySQLAdaptor.GetCurrentTimeStampSQL: String;
begin
  Result := 'current_timestamp';
end;

function TCcMySQLAdaptor.GetDeclaration(DataType: TFieldType; nSize: Integer): String;
begin
  case (DataType) of
    ftInteger: Result := 'INT';
    ftFloat: Result := 'FLOAT';
    ftDate, ftTime,
    ftDateTime: Result := 'DATETIME';
    ftString: Result := 'VARCHAR(' + InttoStr(nSize) + ')';
    ftFixedChar: Result := 'CHAR(' + InttoStr(nSize) + ')';
    ftBlob: Result := 'BLOB';
    ftMemo: Result := 'TEXT';
    else
      raise Exception.Create('Data type ' + IntToStr(Integer(DataType)) + ' not handled by TCcMySQLAdaptor!');
  end;
end;

function TCcMySQLAdaptor.GetParamDecl(DataType: String; nSize, nScale: Integer) : String;
begin
  Result := UpperCase(DataType);
  if (nSize > 0) and ((Result = 'CHAR') or (Result = 'VARCHAR') or (Result = 'VARBINARY')
    or (Result = 'NCHAR') or (Result = 'NVARCHAR') or (Result = 'NVARBINARY')
   or (Result = 'BINARY')) then
     Result := Result + '(' + IntToStr(nSize) + ')'

  else if (Result = 'DECIMAL') or (Result = 'NUMERIC') then
     Result := Result + '(' + IntToStr(nSize) + ', ' + IntToStr(nScale) + ')';
end;

function TCcMySQLAdaptor.QuoteSQLData(cData: String; DataType: String; lSQLStyle: Boolean):String;
begin
  Result := cData;
  if (DataType = 'DATETIME') or (DataType = 'SMALLDATETIME') or (DataType = 'VARCHAR')
       or (DataType = 'CHAR') or (DataType = 'NVARCHAR') or (DataType = 'NCHAR')
       or (DataType = 'TEXT') or (DataType = 'NTEXT')then
  begin
    if (lSQLStyle) then
      Result := 'quote(' + cData + ')'
    else
      Result := QuotedStr(cData);
  end;
end;


function TCcMySQLAdaptor.GetGenerator(GenName: String;
  Increment: Integer): String;
begin
  //This can't work. Key synchronization needs to be abstracted
  Result := 'declare @id int '#13#10' exec @id = RPL$GEN_ID @generator_name = ' +
    QuotedStr(Trim(GenName)) + ', @increment = ' + IntToStr(Increment)
    + #13#10 + 'select @id as code';
end;

function TCcMySQLAdaptor.GetGeneratorValue(GenName: String;
  Increment: Integer):
      {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}
begin
	with FConnection.UpdateQuery['MySQL_GetGeneratorValue'] do begin
    Close;
    SQL.Text := 'call RPL$GEN_ID(' + QuotedStr(Trim(GenName)) + ', ' + IntToStr(Increment) + ')';
    Exec;
    Result := Field['next_value'].Value;
  end;
end;

(*
function TCcMySQLAdaptor.GetProcGenerator(ProcName: String; Params: TDataSet; OutputParam: String; FieldNames: TStringList): String;
var
  cParams, cParamValue:String;
begin
  Params.First;
  while not Params.Eof do begin
    cParamValue := Trim(Params.FieldByName('PARAM_VALUE').AsString);

    if (cParams <> '') then cParams := cParams + ', ';
    if (FieldNames.IndexOf(cParamValue) <> -1) then
      cParams := cParams + ':' + cParamValue
    else
      cParams := cParams + QuoteSQLData(cParamValue, Params.FieldByName('FIELD_DECL').AsString, false);
    Params.Next;
  end;
  Result := '(SELECT ' + Trim(OutputParam) + ' FROM ' + Trim(ProcName) + '(' + cParams + ' ))';
end;
*)

constructor TCcMySQLAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['5.0']);
end;

procedure TCcMySQLAdaptor.CreateProcedures;
begin
  if not ProcedureExists('RPL$GENERATE_LOG') then with Query do begin
    Add('CREATE PROCEDURE `RPL$GENERATE_LOG`');
    Add('(');
    Add('  `TABLE_NAME` VARCHAR(100),');
    Add('  `PRIMARY_KEY_VALUES` VARCHAR(500),');
    Add('  `PROCEDURE_STATEMENT` VARCHAR(200),');
    Add('  `user_login` varchar(50),');
    Add('  `PRIMARY_KEY_SYNC` VARCHAR(5000),');
    Add('  `UNIQUE_KEY_SYNC` VARCHAR(5000)');
    Add(')');
    Add('  insert into RPL$LOG (login, operation_date, table_name,');
    Add('    primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, SENT_FROM)');
    Add('  values (user_login, CURRENT_TIMESTAMP, table_name,');
    Add('    primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, @COPYCAT_REPLICATING_NODE)');
    ExecConfQuery;
  end;
end;

class function TCcMySQLAdaptor.GetAdaptorName: String;
begin
  Result := 'MySQL';
end;

function TCcMySQLAdaptor.GetDatabaseName(): string;
var
  aQuery: TCcQuery;
begin
  aQuery := TCcQuery.Create(FConnection, 'GetDatabaseName', True);
  aQuery.SQL.Text := 'SELECT DATABASE();';
  aQuery.Exec();
  Result := Trim(aQuery.FieldByIndex[0].AsString);
  FreeAndNil(aQuery);
end;

function TCcMySQLAdaptor.SQLFormatValue(Data: Variant; FieldType: TFieldType): String;
var
  cData :String;
  DataType :Integer;
begin
  if VarIsEmpty(Data) or VarIsNull(Data) then
    Result := 'null'
  else begin
    cData := Data;
    if Trim(cData) = '' then
      Result := 'null'
    else begin
      DataType := VarType(Data);
      if (DataType = varString) then
        Result := QuotedStr(Data)
      else if (DataType = varDate) then begin
        if FieldType = ftDateTime then
          Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', TDateTime(Data)) + ''''
        else if (FieldType = ftDate) then
          Result := '''' + FormatDateTime('yyyy-mm-dd', TDateTime(Data)) + ''''
        else if (FieldType = ftTime) then
          Result := '''' + FormatDateTime('hh:nn:ss', TDateTime(Data)) + ''''
      end
      else
        Result := Data;
    end;
  end;
end;

function TCcMySQLAdaptor.ConvertValue(Val: Variant; DataType: TFieldType): Variant;
begin
  if VarType(Val) = varString then begin
    if ((DataType = ftDateTime) or (DataType = ftDate) or (DataType = ftTime)) then
      Result := StringToDate(Val, (DataType <> ftDate))
    else
      Result := Val;
  end
  else
    Result := Val;
end;

function TCcMySQLAdaptor.StringToDate(cDate: String; lHasTimeInfo: Boolean): TDateTime;
var
  cDateFormat: String;
  nMilliSec: Integer;
begin
  //Convert from string to date/time...
  if Length(cDate) > 21 then begin
    //This indicates that our date/time includes milliseconds
    nMilliSec := StrToIntDef(Copy(cDate, Length(cDate) - 3, 4), 0);
    cDate := Copy(cDate, 1, Length(cDate) - 5);
  end
  else
    nMilliSec := 0;

  {$IFDEF CC_D2K13}
  cDate := ReplaceString(cDate, '-', FormatSettings.DateSeparator);
  cDate := ReplaceString(cDate, '.', FormatSettings.DateSeparator);
  cDate := ReplaceString(cDate, '/', FormatSettings.DateSeparator);

  //Save current locale date format for later
  cDateFormat := FormatSettings.ShortDateFormat;
  {$ELSE}
  cDate := ReplaceString(cDate, '-', DateSeparator);
  cDate := ReplaceString(cDate, '.', DateSeparator);
  cDate := ReplaceString(cDate, '/', DateSeparator);

  //Save current locale date format for later
  cDateFormat := ShortDateFormat;
  {$ENDIF}

  try
    // MySQL format: 'YYYY-MM-DD HH:NN:SS.ssss'
    {$IFDEF CC_D2K13}
    FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
    {$ELSE}
    ShortDateFormat := 'yyyy-mm-dd';
    {$ENDIF}

    //Perform the actual conversion
    Result := StrToDateTime(cDate) + EncodeTime(0, 0, 0, nMilliSec);
  finally
    //Restore the locale setting
    {$IFDEF CC_D2K13}
    FormatSettings.ShortDateFormat := cDateFormat;
    {$ELSE}
    ShortDateFormat := cDateFormat;
    {$ENDIF}
  end;
end;

function TCcMySQLAdaptor.SupportsGenerators: Boolean;
begin
  Result := True;
end;

function TCcMySQLAdaptor.DoMetaQuote(Identifier: String): String;
begin
  Result := '`' + Identifier + '`';
end;

function TCcMySQLAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := False;
end;

function TCcMySQLAdaptor.GetQuoteMetadata:Boolean;
begin
  Result := False;
end;

procedure TCcMySQLAdaptor.DoListPrimaryKeys(cTableName: String;
	list: TStringList);
begin
	with FConnection.SelectQuery['MySQLAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'select column_name as pk_name from information_schema.key_column_usage where table_schema = ''' + FDatabaseName +
								''' and constraint_name = ''PRIMARY'' and table_name = :table_name order by ordinal_position';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['pk_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcMySQLAdaptor.DoListProcedures(list: TStringList);
begin
		with FConnection.SelectQuery['MySQLAdaptor.DoListProcedures'] do begin
		Close;
		SQL.Text := 'select r.routine_name as procedure_name from information_schema.routines r  where r.routine_schema = ''' + FDatabaseName + ''' and r.routine_type=''PROCEDURE'' order by r.routine_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['procedure_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcMySQLAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['MySQLAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'select column_name as field_name from information_schema.columns ' +
								'where table_schema = ''' + FDatabaseName + ''' and table_name = :table_name ' +
								'order by column_name';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcMySQLAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
	with FConnection.SelectQuery['MySQLAdaptor.DoListTables'] do begin
		Close;
		SQL.Text := 'select table_name from information_schema.tables where table_schema = ''' + FDatabaseName + ''' and table_type=''BASE TABLE'' order by table_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['table_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcMySQLAdaptor.DoListTriggers(list: TStringList);
begin
	with FConnection.SelectQuery['MySQLAdaptor.DoListTriggers'] do begin
		Close;
		SQL.Text := 'select trigger_name from information_schema.triggers where trigger_schema = ''' + FDatabaseName + '''';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['trigger_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcMySQLAdaptor.DoListUpdatableTableFields(cTableName: String;
	list: TStringList);
begin
	with FConnection.SelectQuery['MySQLAdaptor.DoListUpdatableTableFields'] do begin
		Close;
		SQL.Text := 'select column_name as field_name from information_schema.columns ' +
								'where table_schema = ''' + FDatabaseName + ''' and table_name = :table_name ' +
								'and ( (extra = '''') or (extra = ''auto_increment'' and column_key = ''PRI''));';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcMySQLAdaptor.DoListAllProcedures(list: TStringList);
begin
	DoListProcedures(list);
end;

procedure TCcMySQLAdaptor.DropProcedures;
begin
  if ProcedureExists('RPL$GENERATE_LOG') then begin
    Query.Add('DROP PROCEDURE `RPL$GENERATE_LOG`');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
end;


initialization
//  RegisterDBAdaptors(['MySQL'], [TCcMySQLAdaptor]);

end.
