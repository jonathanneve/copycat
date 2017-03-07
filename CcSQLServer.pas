unit CcSQLServer;

{$I CC.INC}

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys;

type


//Database adaptor for Microsoft SQL Server
//See also:
//TCcDBAdaptor
TCcSQLServerAdaptor = class (TCcDBAdaptor)
  private
    lTableHasIdentityFields: Boolean;
    function GetParamDecl(DataType: String; nSize, nScale: Integer): String;
    function GetDeclaration(DataType: TFieldType; nSize: Integer): String;
    function ConvertToOldGen(cPKGen: String; cNewOld: String): String;
    function QuoteSQLData(cData: String; DataType: String;
      lSQLStyle: Boolean): String;
    procedure SetIdentityInsert(cTableName, cOnOff: String);
    function StringToDate(cDate: String): TDateTime;
  protected
    FKeys : TCcKeyRing;
    function GetQuoteMetadata: Boolean;override;
    function DoMetaQuote(Identifier: string): string;override;

    function ConvertValue(Val: Variant; DataType: TFieldType): Variant;override;
    function MaxDDLNameLength: Integer;override;
    procedure BeforeConnect;override;
    function GetCurrentTimeStampSQL:String;override;
		function GetUseRowsAffected: Boolean; override;

    procedure DoListTables(list: TStringList; IncludeTempTables: Boolean); override;
		procedure DoListTableFields(cTableName: String; list: TStringList);override;
		procedure DoListFieldsForNoPK(cTableName: String; list: TStringList);override;
		procedure DoListUpdatableTableFields(cTableName: String; list: TStringList);override;
		procedure DoListPrimaryKeys(cTableName: String; list: TStringList);override;
		procedure DoListTriggers(list: TStringList);override;
		procedure DoListProcedures(list: TStringList);override;
		procedure DoListAllProcedures(list: TStringList);override;
    procedure ExecutingReplicationQuery(cTableName, queryType: String;
      fieldList: TStringList);override;
    procedure ExecutedReplicationQuery(cTableName, queryType: String;
      fieldList: TStringList);override;
    procedure ExecutingReplicationQuerySQL(cTableName, queryType: String;
      query: TCcQuery);override;
  public
    function SubStringFunction(str: String; start, length: Integer): String; override;
    function ConcatenationOperator: String; override;

    function SupportsGenerators: Boolean;override;
    class function GetAdaptorName: String;override;
    function SQLFormatValue(Data: Variant; FieldType: TFieldType): String;override;
    procedure CreateProcedures;override;
    procedure CheckCustomMetadata;override;
    procedure RemoveCustomMetadata;override;
    procedure DropProcedures;override;

    function DeclareField(FieldName: String; FieldType: TFieldType;
      Length: Integer; NotNull: Boolean; PK: Boolean; AutoInc: Boolean): String;override;

    function GenDeclared(GenName:String): Boolean;override;
    procedure RemoveTriggers(qTable: TCcQuery);override;
    function GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;override;

    procedure DeclareGenerator(GenName:String);override;
    procedure DropGenerator(cGeneratorName: string);override;
    function GetGenerator(GenName: String; Increment: Integer): String;override;
    function GetGeneratorValue(GenName: String; Increment: Integer):
     {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}override;
    constructor Create(Conn: TCcConnection);override;
    destructor Destroy;override;
end;

implementation

{$IFDEF CC_UseVariants}  uses Variants; {$ENDIF}

{function TCcSQLServerAdaptor.GetSQL(SQLType: TCcMetaSQL): String;
begin
  case (SQLType) of
    sqlTables:
			Result:='select name as table_name from sysobjects where type=''U'''+
				 'order by name';
    sqlTableFields:
			Result:='select c.name as field_name '+
				'from sysobjects o '+
				'join syscolumns c on c.id = o.id '+
				'where %upper_case(o.name) = %upper_case(:table_name) '+
				'order by c.name';
     sqlUpdatableFields:
			Result:='select syscolumns.name as field_name from syscolumns, sysobjects '+
				'where sysobjects.id=syscolumns.id '+
				'and %upper_case(sysobjects.name)=%upper_case(:table_name) ' +
				'and syscolumns.autoval is null ' +
				'and syscolumns.iscomputed = 0 ' +
				'and syscolumns.xusertype <> 189 '; //We exclude timestamp fields...

    sqlTablePKs:
			Result:='select sc.name as pk_name '+
				 'from sysobjects o '+
				 'join sysobjects p on p.parent_obj = o.id ' +
				 'join sysindexes si on si.name = p.name '+
				 'join sysindexkeys sik on sik.id = o.id and sik.indid = si.indid '+
				 'join syscolumns sc on sc.id = o.id and sc.colid = sik.colid ' +
				 'where o.type=''U'' '+
				 'and p.xtype=''PK'' '+
				 'and %upper_case(o.name)=%upper_case(:table_name) ' +
				 'order by sik.keyno ';
    sqlProcedures:
			Result:='select o.name as procedure_name from sysobjects o where o.type=''P'' ' +
				'and not exists (select * from syscolumns c where c.id = o.id and c.isoutparam = 1) ' +
				'order by o.name';
		sqlGenerators:
      Result := 'select name as generator_name from RPL$GENERATORS order by name';
    sqlFindTrigger:
			Result:='select count(*) as rec_count from sysobjects '+
				'where type=''TR'' '+
				'and %upper_case(name) = %upper_case(:trigger_name)';
    sqlFindTableField:
      Result:='select count(*) as rec_count from syscolumns, sysobjects '+
        'where sysobjects.id=syscolumns.id '+
        'and %upper_case(sysobjects.name)=%upper_case(:table_name) '+
        'and %upper_case(syscolumns.name)=%upper_case(:field_name)';
    sqlFindProc:
      Result:='select count(*) as rec_count from sysobjects where %upper_case(name) =%upper_case(:proc_name)';
    sqlFindTable:
      Result:='select count(*) as rec_count from sysobjects where %upper_case(name) =%upper_case(:table_name)';

    else
      raise Exception.Create('CopyCat SQLServer adaptor not up-to-date (SQL constant unknown)!');
  end;
end;
}

function TCcSQLServerAdaptor.MaxDDLNameLength: Integer;
begin
	Result := 31;
end;

destructor TCcSQLServerAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCcSQLServerAdaptor.DeclareField(FieldName: String; FieldType: TFieldType;
  Length: Integer; NotNull, PK: Boolean; AutoInc: Boolean): String;
begin
  Result := FieldName + ' ' + GetDeclaration(FieldType, Length);

  if NotNull then
    Result := Result + ' NOT NULL';
  if PK then
    Result := Result + ' PRIMARY KEY';
  if AutoInc then
    Result := Result + ' IDENTITY';
end;

function TCcSQLServerAdaptor.ConvertValue(Val: Variant; DataType: TFieldType): Variant;
var
  valType:Integer;
begin
  valType := VarType(Val);
  if (valType = varString) or (valType = varOleStr) {$IFDEF CC_D2K9} or (VarType(Val) = varUString){$ENDIF} then
  begin
    if (DataType in [ftDateTime, ftDate, ftTime{$IFDEF CC_D2K12} , ftTimeStamp, ftOraTimeStamp, ftTimeStampOffset, ftOraInterval{$ENDIF}]) then
      Result := StringToDate(Val)
    else if (DataType = ftGuid) and (Copy(Val,1,1) <> '{') then
      Result := '{' + Val + '}'
    else
      Result := Val;
  end
  else
    Result := Val;
end;

function TCcSQLServerAdaptor.StringToDate(cDate: String): TDateTime;
begin
  Result := EncodeDate(StrToInt(Copy(cDate, 1, 4)), StrToInt(Copy(cDate, 6, 2)), StrToInt(Copy(cDate, 9, 2)))
    + EncodeTime(StrToIntDef(Copy(cDate, 12, 2), 0), StrToIntDef(Copy(cDate, 15, 2), 0), StrToIntDef(Copy(cDate, 18, 2), 0), StrToIntDef(Copy(cDate, 21, 3), 0));
end;

procedure TCcSQLServerAdaptor.DeclareGenerator(GenName: String);
begin
  Query.Clear;
  Query.Add('insert into rpl$generators(name, value) values (' + QuotedStr(GenName) + ', 1)');
  ExecConfQuery;
end;

procedure TCcSQLServerAdaptor.DropGenerator(cGeneratorName: string);
begin

end;

function TCcSQLServerAdaptor.GenDeclared(GenName: String): Boolean;
begin
  with FConnection.SelectQuery['MSSQL_CheckGenDeclared'] do begin
    Close;
		SQL.Text := 'select count(*) rec_count from rpl$generators where name = :gen_name';
		Param['gen_name'].Value := GenName;
    Exec;
    Result := (Field['rec_count'].Value >= 1);
  end;
end;

(*
procedure TCcSQLServerAdaptor.GenerateProcedure(qProcedure:TCcQuery; Params: TDataSet);
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
  Query.Add('  set @PRIMARY_KEY_VALUES = cast(GEN_ID(GEN_' + copy(NewProcName, 1, 27) + ', 1) as varchar(500))');
  Query.Add('  SET @EXEC_PROC_STATEMENT = quotename(''EXEC ' + NewProcName + ''' + ' + cParamValues + ', '''''''')');
  Query.Add('  EXEC ' + ProcName + cParamNamesAssignment);
  Query.Add('  EXEC RPL$GENERATE_LOG @TABLE_NAME = ' + QuotedStr(ProcName) + ', @PRIMARY_KEY_VALUES = @PRIMARY_KEY_VALUES, @REF_VALUE = null, @PROCEDURE_STATEMENT = @EXEC_PROC_STATEMENT');
  Query.Add('END');
  ExecConfQuery;
end;
*)

function TCcSQLServerAdaptor.GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;
var
  cTriggerName: String;
	TableName, ConfigName: String;

	procedure DoGenerateTriggers(cOperationType: String);
		procedure GetSelect(cNewOld: String);
		var
			cCondition: String;
			cDataTableName: String;
			lConditionField, lCondition: Boolean;
			I: Integer;
			cPKValues, cPKSync, cUKSync: String;
			nPKCount, nPKSyncCount, nUKSyncCount: Integer;
      cPK: string;
		begin
			if cNewOld = 'new' then
				cDataTableName := 'inserted'
			else
				cDataTableName := 'deleted';

			lConditionField := (Trim(qTable.Field['CONDITION_FIELD'].AsString) <> '');
			lCondition := (Trim(qTable.Field['CONDITION'].AsString) <> '');

			//Convert 'new' to 'old', if applicable
			if lCondition then
				cCondition := ConvertToOldGen(Trim(qTable.Field['CONDITION'].AsString), cNewOld);

			Query.Add('  select u.login, t.repl_' + cOperationType + 's as repl_operation, ');
			nPKCount := 0;
			for I := 0 to FKeys.Count - 1 do begin
				if (FKeys[i].PrimaryKey) then begin
          if FKeys[i].DataType in [ftDateTime, ftDate, ftTime{$IFDEF CC_D2K12}, ftTimeStamp, ftOraTimeStamp, ftTimeStampOffset, ftOraInterval{$ENDIF}] then
            cPK := 'convert(varchar(23),' + cNewOld + '.' + Trim(FKeys[i].KeyName) + ', 121)'
          else
            cPK := cNewOld + '.' + Trim(FKeys[i].KeyName);
            cPKValues := '  coalesce(quotename(' + cPK + ', ''''''''), ''"'') + '';''';
					if nPKCount>0 then
						cPKValues := ' + ' + cPKValues;
					Query.Add(cPKValues);
					Inc(nPKCount);
				end;
			end;

			nPKSyncCount := 0;
			cPKSync := '';
			for I := 0 to FKeys.Count - 1 do begin
				if (FKeys[i].GenericSyncStatement <> '') and (FKeys[i].PrimaryKey) then begin
					if nPKSyncCount>0 then
						cPKSync := cPKSync + ' + ';
					cPKSync := cPKSync + '  quotename(' + FKeys[i].GenericSyncStatement + ', '''''''') + '';''';
					Inc(nPKSyncCount);
				end;
			end;
			if nPKSyncCount = 0 then
				cPKSync := 'null';
			Query.Add(', ' + cPKSync);

			nUKSyncCount := 0;
			cUKSync := '';
			for I := 0 to FKeys.Count - 1 do begin
				if (FKeys[i].GenericSyncStatement <> '') and (not FKeys[i].PrimaryKey) then begin
					if nUKSyncCount>0 then
						cUKSync := cUKSync + ' + ';
					cUKSync := cUKSync + '  quotename(' + FKeys[i].GenericSyncStatement + ', '''''''') + '';''';
					Inc(nUKSyncCount);
				end;
			end;
			if nUKSyncCount = 0 then
				cUKSync := 'null';
			Query.Add(', ' + cUKSync);

			Query.Add('    from RPL$users u, ' + cDataTableName + ' ' + cNewOld);
			if (ConfigName <> '') then
				Query.Add('    join rpl$tables_config t on t.table_name = ' + QuotedStr(TableName) + ' and t.config_name = ' + QuotedStr(ConfigName))
			else
				Query.Add('    join rpl$tables t on t.table_name = ' + QuotedStr(TableName));
			Query.Add('    where (u.login <> app_name())');

			if (ConfigName <> '') then
				Query.Add('    and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

      if Assigned(qTableConf) then begin
        if Trim(qTableConf.Field['CONDITION'].AsString) <> '' then
          Query.Add('    and (' + ConvertToOldGen(qTableConf.Field['CONDITION'].AsString, cNewOld) + ')');

        if Trim(qTableConf.Field[cOperationType + '_CONDITION'].AsString) <> '' then
          Query.Add('    and (' + ConvertToOldGen(qTableConf.Field[cOperationType + '_CONDITION'].AsString, cNewOld) + ')');
      end;

			//User-definable SQL condition, configurable per table in RPL$TABLES
			if lCondition then
				Query.Add('    and (' + cCondition + ')');
			if lConditionField then
        //If the condition field is null, the record is not replicated to any user
        //If RPL$USERS.Condition_value is null, the user gets all changes
        Query.Add('    and ((' + cNewOld + '.' + Trim(qTable.Field['CONDITION_FIELD'].AsString) + ' = u.condition_value) or (u.condition_value is null))');
    end;

  begin
    Query.Add('create trigger ' + cTriggerName + '_' + Copy(cOperationType,1,1) + ' on ' + TableName + ' for ' + cOperationType);
    Query.Add('AS begin');
    Query.Add('SET NOCOUNT ON');
    Query.Add('declare @primary_key_values varchar(500)');
    Query.Add('declare @old_primary_key_values varchar(500)');
		Query.Add('declare @QuotedValue varchar(200)');
    Query.Add('declare @old_user_login varchar(50)');
		Query.Add('declare @user_login varchar(50)');
    Query.Add('declare @repl_operation char(1)');
		Query.Add('declare @primary_key_sync varchar(5000)');
		Query.Add('declare @unique_key_sync varchar(5000)');
		Query.Add('  create table #tmpnodes (node_name varchar(50), repl_operation char(1), primary_key_values varchar(500), primary_key_sync text, unique_key_sync text)');
		Query.Add('  insert into #tmpnodes (node_name, repl_operation, primary_key_values, primary_key_sync, unique_key_sync)');
		GetSelect('new');
		Query.Add('union');
		GetSelect('old');
		Query.Add('  while exists (select * from #tmpnodes)');
		Query.Add('  begin');
		Query.Add('    set @old_primary_key_values = @primary_key_values');
		Query.Add('    set @old_user_login = @user_login');
		Query.Add('    select top 1 @user_login = node_name, @repl_operation = repl_operation, @primary_key_values = primary_key_values, @primary_key_sync = primary_key_sync, @unique_key_sync = unique_key_sync ');
		Query.Add('    from #tmpnodes order by node_name, primary_key_values');
		Query.Add('    if ((@repl_operation = ''Y'') and ((coalesce(@old_primary_key_values, '''') <> coalesce(@primary_key_values, '''')) or (coalesce(@old_user_login, '''') <> coalesce(@user_login, ''''))))');
    Query.Add('    insert into RPL$log (login, operation_date, table_name,');
    Query.Add('      primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, SENT_FROM)');
    Query.Add('    values (@user_login, CURRENT_TIMESTAMP, ' + QuotedStr(TableName) + ',');
    Query.Add('      @primary_key_values, null, @PRIMARY_KEY_SYNC, @UNIQUE_KEY_SYNC, app_name())');
		Query.Add('    delete from #tmpnodes where node_name = @user_login and primary_key_values = @primary_key_values');
		Query.Add('  end');
		Query.Add('  drop table #tmpnodes');
		Query.Add('end');
		ExecConfQuery;
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

  DoGenerateTriggers('INSERT');
  DoGenerateTriggers('UPDATE');
  DoGenerateTriggers('DELETE');
end;

procedure TCcSQLServerAdaptor.RemoveTriggers(qTable: TCcQuery);

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

procedure TCcSQLServerAdaptor.BeforeConnect;
begin
  FConnection.ConnectionParams.Values['APP'] := FConnection.ReplicatingNode;
  FConnection.ConnectionParams.Values['ApplicationName'] := FConnection.ReplicatingNode;
end;

//procedure TCcSQLServerAdaptor.InitConnection;
//begin
//  inherited;
//  with FConnection.Query['MSSQL.qInsertRplVars'] do begin
////    Close;
////    SQL := 'create table #rpl$vars (replicating_node varchar(100))';
////    Exec;
////    FConnection.Commit;
////    FConnection.StartTransaction;
//    Close;
//    SQL := 'declare @replicating_node varchar(50)'#13#10'' +
//           'set @replicating_node = :replicating_node'#13#10'' +
//           'select @replicating_node as replicating_node into #rpl$vars';
//    Param['replicating_node'].Value := FConnection.ReplicatingNode;
//    Exec;
//  end;
//end;

procedure TCcSQLServerAdaptor.CheckCustomMetadata;
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
    Query.Add('  @GENERATOR_NAME VARCHAR(100),');
    Query.Add('  @INCREMENT INT');
    Query.Add(')');
    Query.Add('AS');
    Query.Add('begin');
    Query.Add('  declare @NEXT_VALUE INT');
    Query.Add('  set @next_value = -1');
    Query.Add('  update RPL$generators set @next_value = value = value + @increment where name = @generator_name');
    Query.Add('  return @next_value');
    Query.Add('end');
    ExecConfQuery;
  end;
end;

procedure TCcSQLServerAdaptor.RemoveCustomMetadata;
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

function TCcSQLServerAdaptor.ConcatenationOperator: String;
begin
  Result := '+';
end;

function TCcSQLServerAdaptor.ConvertToOldGen(cPKGen: String; cNewOld: String):String;
begin
  Result := ReplaceString(cPKGen, 'new.', cNewOld + '.');
end;

(*
procedure TCcSQLServerAdaptor.GetProcParams(ProcName: String; Params: TDataSet; InputParam: Boolean);
var
  nParamType:Integer;
begin
  if InputParam then nParamType := 0
                else nParamType := 1;
  Params.Close;
  Params.Open;
  if (ProcName <> '') then
  with FConnection.SelectQuery['MSSQL_GetProcParams'] do
  begin
    Close;
//    SQL :='select o.name, c.name as param_name, t.xtype as field_type, c.prec as field_length, c.scale as field_scale, t.name as ftype '+//, c.type '+
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
*)

function TCcSQLServerAdaptor.GetCurrentTimeStampSQL: String;
begin
  Result := 'current_timestamp';
end;

function TCcSQLServerAdaptor.GetDeclaration(DataType: TFieldType; nSize: Integer): String;
begin
  case (DataType) of
    ftInteger: Result := 'INT';
    ftFloat: Result := 'FLOAT';
    ftDate, ftTime,
    ftDateTime: Result := 'DATETIME';
    ftString: Result := 'VARCHAR(' + InttoStr(nSize) + ')';
    ftFixedChar: Result := 'CHAR(' + InttoStr(nSize) + ')';
    ftBlob: Result := 'SQL_VARIANT';
    ftMemo: Result := 'NTEXT';
    else
      raise Exception.Create('Data type ' + IntToStr(Integer(DataType)) + ' not handled by TCcSQLServerAdaptor!');
  end;
end;

function TCcSQLServerAdaptor.GetParamDecl(DataType: String; nSize, nScale: Integer) : String;
begin
  Result := UpperCase(DataType);
  if (nSize > 0) and ((Result = 'CHAR') or (Result = 'VARCHAR') or (Result = 'VARBINARY')
    or (Result = 'NCHAR') or (Result = 'NVARCHAR') or (Result = 'NVARBINARY')
   or (Result = 'BINARY')) then
     Result := Result + '(' + IntToStr(nSize) + ')'

  else if (Result = 'DECIMAL') or (Result = 'NUMERIC') then
     Result := Result + '(' + IntToStr(nSize) + ', ' + IntToStr(nScale) + ')';
end;

//function TCcSQLServerAdaptor.QuoteIdentifier(fieldName: String): String;
//begin
////  if fieldName in SQLServerKeywords then
//  if Uppercase(fieldName) = 'LINENO' then
//    Result := '"' + fieldName + '"'
//  else
//    Result := fieldName;
//end;

function TCcSQLServerAdaptor.QuoteSQLData(cData: String; DataType: String; lSQLStyle: Boolean):String;
begin
  Result := cData;
  if (DataType = 'DATETIME') or (DataType = 'SMALLDATETIME') or (DataType = 'VARCHAR')
       or (DataType = 'CHAR') or (DataType = 'NVARCHAR') or (DataType = 'NCHAR')
       or (DataType = 'TEXT') or (DataType = 'NTEXT')then
  begin
    if (lSQLStyle) then
      Result := 'quotedname(' + cData + ', '''''''')'
    else
      Result := QuotedStr(cData);
  end;
end;


function TCcSQLServerAdaptor.GetGenerator(GenName: String;
  Increment: Integer): String;
begin
  //This can't work. Key synchronization needs to be abstracted
  Result := 'declare @id int '#13#10' exec @id = rpl$gen_id @generator_name = ' +
    QuotedStr(Trim(GenName)) + ', @increment = ' + IntToStr(Increment)
    + #13#10 + 'select @id as code';
end;

//function TCcSQLServerAdaptor.GetGeneratorSQL(GenName: String;
//  Increment: Integer): String;
//begin
//  Result := 'declare @id int '#13#10' exec @id = rpl$gen_id @generator_name = ' +
//    QuotedStr(Trim(GenName)) + ', @increment = ' + IntToStr(Increment)
//    + #13#10 + 'select @id as code';
//end;

function TCcSQLServerAdaptor.GetGeneratorValue(GenName: String;
  Increment: Integer):
      {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}
begin
	with FConnection.SelectQuery['MSSQL_GetGeneratorValue'] do begin
    Close;
//    SQL := 'declare @rpl$generator_value int' +
//      #13#10 + 'set @rpl$generator_value = 10' +//'exec @rpl$generator_value = rpl$gen_id @generator_name = ' + QuotedStr(Trim(GenName)) + ', @increment = ' + IntToStr(Increment) +
//      #13#10 + 'select @rpl$generator_value as code';
    SQL.Text := 'declare @rpl$generator_value int' +
      #13#10 + 'exec @rpl$generator_value = rpl$gen_id @generator_name = ' + QuotedStr(Trim(GenName)) + ', @increment = ' + IntToStr(Increment) +
      #13#10 + 'select @rpl$generator_value as code';
    Exec;
//    Close;
//    SQL := ;
//    Exec;
//    Close;
//    SQL := 'select @rpl$generator_value as code';
//    Exec;
    Result := Field['code'].Value;
  end;
end;

(*
function TCcSQLServerAdaptor.GetProcGenerator(ProcName: String; Params: TDataSet; OutputParam: String; FieldNames: TStringList): String;
var
  cParams, cParamValue:String;
//  ParamType :TFieldType;
begin
  Params.First;
  while not Params.Eof do begin
    cParamValue := Trim(Params.FieldByName('PARAM_VALUE').AsString);
//    ParamType := TFieldType(Params.FieldByName('FIELD_TYPE').AsInteger); //Trim(GetDataType(Params.FieldByName('FIELD_TYPE').AsInteger, Params.FieldByName('FIELD_LENGTH').AsInteger));

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

constructor TCcSQLServerAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['MSSQL2000']);
end;

procedure TCcSQLServerAdaptor.CreateProcedures;
begin
  if not ProcedureExists('RPL$GENERATE_LOG') then with Query do begin
    Add('CREATE PROCEDURE RPL$GENERATE_LOG');
    Add('(');
    Add('  @TABLE_NAME VARCHAR(100),');
    Add('  @PRIMARY_KEY_VALUES VARCHAR(500),');
    Add('  @PROCEDURE_STATEMENT VARCHAR(200),');
    Add('  @user_login varchar(50),');
    Add('  @PRIMARY_KEY_SYNC VARCHAR(5000),');
    Add('  @UNIQUE_KEY_SYNC VARCHAR(5000)');
    Add(')');
    Add('AS');
    Add('begin');
    Add('  insert into RPL$log (login, operation_date, table_name,');
    Add('    primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, SENT_FROM)');
    Add('  values (@user_login, CURRENT_TIMESTAMP, @table_name,');
    Add('    @primary_key_values, @procedure_statement, @PRIMARY_KEY_SYNC, @UNIQUE_KEY_SYNC, app_name())');
    Add('end');
    ExecConfQuery;
  end;
end;

function TCcSQLServerAdaptor.SQLFormatValue(Data :Variant; FieldType :TFieldType) :String;
var
  cData :String;
  DataType: Integer;
begin
  cData := Data;
  DataType := VarType(Data);
  if (not VarIsNull(Data)) then begin
		if (DataType = varString) or (DataType = varOleStr) {$IFDEF CC_D2K9} or (DataType = varUString){$ENDIF} then
			Result := QuotedStr(Data)
    else if DataType = varDate then begin
      if (FieldType = ftDateTime) then
        Result := '''' + FormatDateTime('YYYYMMDD hh:nn:ss.zzz', TDateTime(Data)) + ''''
      else if (FieldType = ftDate) then
        Result := '''' + FormatDateTime('YYYYMMDD', TDateTime(Data)) + ''''
      else if (FieldType = ftTime) then
        Result := '''' + FormatDateTime('hh:nn:ss', TDateTime(Data)) + ''''
    end
    else
      Result := Data;
  end
  else
    Result := 'null';
end;

function TCcSQLServerAdaptor.SubStringFunction(str: String; start,
  length: Integer): String;
begin
  Result := 'substring(' + str + ',' + IntToStr(start) + ',' + IntToStr(length) + ')';
end;

function TCcSQLServerAdaptor.SupportsGenerators: Boolean;
begin
  Result := False;
end;

class function TCcSQLServerAdaptor.GetAdaptorName: String;
begin
  Result := 'MSSQL';
end;

function TCcSQLServerAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := True;
end;

procedure TCcSQLServerAdaptor.DoListPrimaryKeys(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListPrimaryKeys'] do begin
		Close;
		SQL.Text := 'select sc.name as pk_name '+
				 'from sysobjects o '+
				 'join sysobjects p on p.parent_obj = o.id ' +
				 'join sysindexes si on si.name = p.name '+
				 'join sysindexkeys sik on sik.id = o.id and sik.indid = si.indid '+
				 'join syscolumns sc on sc.id = o.id and sc.colid = sik.colid ' +
				 'where o.type=''U'' '+
				 'and p.xtype=''PK'' '+
				 'and %upper_case(o.name)=%upper_case(:table_name) ' +
         'order by sik.keyno';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].Value := cTableName;
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['pk_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DoListProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListProcedures'] do begin
		Close;
		SQL.Text := 'select o.name as procedure_name from sys.procedures o where is_ms_shipped = 0 ' +
				'and not exists (select * from syscolumns c where c.id = o.object_id and c.isoutparam = 1)  ' +
				'order by o.name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['procedure_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'select c.name as field_name '+
				'from sysobjects o '+
				'join syscolumns c on c.id = o.id '+
				'where %upper_case(o.name) = %upper_case(:table_name) '+
				'order by c.name';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].Value := cTableName;
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.ExecutedReplicationQuery(cTableName: String; queryType: String; fieldList: TStringList);
begin
//  if queryType = 'INSERT' then
//begin
//    SetIdentityInsert(cTableName, 'OFF');
//  end;
end;

procedure TCcSQLServerAdaptor.ExecutingReplicationQuery(cTableName: String; queryType: String; fieldList: TStringList);
begin
  lTableHasIdentityFields := False;
  if queryType = 'INSERT' then
  begin
    with FConnection.SelectQuery['SQLServerAdaptor.GetIdentityColumns'] do begin
      Close;
      SQL.Text := 'select c.name as field_name '+
          'from sys.objects o inner join sys.columns c on o.object_id = c.object_id '+
          'where %upper_case(o.name) = %upper_case(:table_name) '+
          'and c.is_identity = 1 ' +
          'order by c.name';

      if QuoteMetadata then
        Macro['upper_case'].Value := ''
      else
        Macro['upper_case'].Value := 'upper';
      Param['table_name'].Value := cTableName;
      Exec;
      while not Eof do begin
        lTableHasIdentityFields := True;
        fieldList.Add(Trim(Field['field_name'].AsString));
        Next;
      end;
    end;
//    SetIdentityInsert(cTableName, 'ON');
  end;
end;

procedure TCcSQLServerAdaptor.ExecutingReplicationQuerySQL(cTableName: String; queryType: String; query: TCcQuery);
begin
  if (queryType = 'INSERT') and (Copy(query.SQL.Text, 1, 4) <> 'SET ') and lTableHasIdentityFields then
    query.SQL.Text := 'SET IDENTITY_INSERT ' + MetaQuote(cTableName) + ' ON '#13#10'' + query.SQL.Text + 'SET IDENTITY_INSERT ' + MetaQuote(cTableName) + ' OFF '#13#10'';
end;

procedure TCcSQLServerAdaptor.SetIdentityInsert(cTableName, cOnOff: String);
begin
	with FConnection.UpdateQuery['SQLServerAdaptor.SetIdentityInsert'] do begin
		Close;
		SQL.Text := 'SET IDENTITY_INSERT %TABLE_NAME %ON_OFF';
		Macro['table_name'].Value := MetaQuote(cTableName);
		Macro['on_off'].Value := cOnOff;
		Exec;
	end;
end;

procedure TCcSQLServerAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListTables'] do begin
		Close;
		SQL.Text := 'select name as table_name from sysobjects where type=''U'''+
				 'order by name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['table_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DoListTriggers(list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListTriggers'] do begin
		Close;
		SQL.Text := 'select name from sysobjects where type=''TR'' ';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DoListUpdatableTableFields(
  cTableName: String; list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListUpdatableTableFields'] do begin
		Close;
		SQL.Text := 'select syscolumns.name as field_name from syscolumns, sysobjects '+
				'where sysobjects.id=syscolumns.id '+
				'and %upper_case(sysobjects.name)=%upper_case(:table_name) ' +
				'and syscolumns.autoval is null ' +
				'and (syscolumns.status & 0x80 = 0) ' +
				'and syscolumns.iscomputed = 0 ' +
				'and syscolumns.xusertype <> 189 ';//We exclude timestamp fields...

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].Value := cTableName;
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DoListAllProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListProcedures'] do begin
		Close;
		SQL.Text := 'select o.name as procedure_name from sysobjects o where o.type=''P'' ' +
				'order by o.name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['procedure_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DoListFieldsForNoPK(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['SQLServerAdaptor.DoListFieldsNoPK'] do begin
		Close;
		SQL.Text := 'select c.name as field_name '+
				'from sysobjects o '+
				'join syscolumns c on c.id = o.id '+
				'where %upper_case(o.name) = %upper_case(:table_name) '+
        'and c.xtype NOT IN (35, 165, 99, 34, 173) ' +
        'and c.length < 50 ' +
				'order by c.name';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].Value := cTableName;
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLServerAdaptor.DropProcedures;
begin
  if ProcedureExists('RPL$GENERATE_LOG') then begin
    Query.Add('DROP PROCEDURE RPL$GENERATE_LOG');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
end;

function TCcSQLServerAdaptor.GetQuoteMetadata:Boolean;
begin
  Result := True;
end;

function TCcSQLServerAdaptor.DoMetaQuote(Identifier: string): string;
begin
  if QuoteMetadata then
    Result := '[' + Identifier + ']'
  else
    Result := Identifier;
end;

initialization
//  RegisterDBAdaptors(['MSSQL'], [TCcSQLServerAdaptor]);

end.
