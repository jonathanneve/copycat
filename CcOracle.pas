unit CcOracle;

interface

{$I CC.INC}

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys  {$IFDEF CC_UseVariants} , Variants{$ENDIF}{$IFDEF CC_D6} , DateUtils , SqlTimSt {$ENDIF};

type

TCcOracleAdaptor = class (TCcDBAdaptor)
  private
    FBranchVersion: Integer;
    function GetDeclaration(DataType: TFieldType; nSize: Integer): String;
    //function PackageExists(): Boolean;
    procedure EmptyTempTable;
    procedure InsertTempTable;
    function ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
    function TranslateDataType(OracleDataType: String): TFieldType;
    function GetFieldType(TableName: String ; FieldName: string): String;
    function StringToDateTime(cDate: String ; DataType: TFieldType): Variant;

  protected
		FKeys : TCcKeyRing;
    procedure DoRegisterNode(NodeName: String);override;
		function GetQuoteMetadata: Boolean;override;

		procedure DoListTables(list: TStringList; IncludeTempTables: Boolean); override;
		procedure DoListTableFields(cTableName: String; list: TStringList);override;
		procedure DoListUpdatableTableFields(cTableName: String; list: TStringList);override;
		procedure DoListPrimaryKeys(cTableName: String; list: TStringList);override;
		procedure DoListTriggers(list: TStringList);override;
		procedure DoListProcedures(list: TStringList);override;
		procedure DoListAllProcedures(list: TStringList);override;
    procedure DoListGenerators(list: TStringList);override;
    procedure DoListFieldsForNoPK(cTableName: String; list: TStringList);override;

    function MaxDDLNameLength: Integer;override;
    function GetUseRowsAffected: Boolean; override;
    procedure SetVersion(const Value: String);override;

    //Called after a new connection has been started
    procedure InitConnection;override;
    //Called after a new transaction has been started
    procedure InitTransaction;override;
    //Called before a transaction is committed
    procedure CleanupTransaction;override;
    procedure BeforeConnect;override;
    function GetCurrentTimeStampSQL:String;override;

  public
    procedure RemoveExtraCustomMetadata;override;
    procedure RemoveCustomMetadata;override;
    procedure DropProcedures;override;
  	procedure CheckExtraCustomMetadata;override;
    function SupportsGenerators: Boolean;override;
    class function GetAdaptorName: String;override;
    function SQLFormatValue(Data: Variant; FieldType :TFieldType): String;override;
    procedure CreateProcedures;override;
    procedure CheckCustomMetadata;override;

    function ConvertValue(Val: Variant; DataType: TFieldType): Variant;override;
    function DeclareField(FieldName: String; FieldType: TFieldType;
			Length: Integer; NotNull: Boolean; PK: Boolean; AutoInc: Boolean): String;override;
		function DeclarePK(FieldNames: String): String;override;

    procedure DeclareGenerator(GenName:String);override;
    function GenDeclared(GenName:String): Boolean;override;

		procedure RemoveTriggers(qTable: TCcQuery);override;
		function GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;override;

    function GetGeneratorValue(GenName: String; Increment: Integer):
      {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}override;
    function GetGenerator(GenName: String; Increment: Integer): String;override;
    property BranchVersion: Integer read FBranchVersion;

		constructor Create(Conn: TCcConnection);override;
		destructor Destroy;override;
end;


implementation

{ TCcOracleAdaptor }

function TCcOracleAdaptor.GetDeclaration(DataType: TFieldType; nSize: Integer): String;
begin
  case (DataType) of
    ftInteger: Result := 'NUMBER';
    ftFloat: Result := 'BINARY_FLOAT';
    ftDate,ftTime, ftDateTime: Result := 'DATE';
    {$IFDEF CC_D6}
    ftTimeStamp: Result := 'TIMESTAMP';
    {$ENDIF}
    ftString:
      if (nSize >= 4000) and (fBranchVersion < 121) then
        Result := 'VARCHAR2(4000)'
      else
        Result := 'VARCHAR2(' + InttoStr(nSize) + ')';
    ftFixedChar: Result := 'CHAR(' + InttoStr(nSize) + ')';
    ftBlob: Result := 'BLOB';
    ftMemo: Result := 'CLOB';
    else
      raise Exception.Create('Data type ' + IntToStr(Integer(DataType)) + ' not handled by TCcOracleAdaptor!');
  end;
end;

(*function TCcOracleAdaptor.PackageExists(): Boolean;
begin
	with FConnection.SelectQuery['Oracle_Package_Exists'] do begin
		Close;
    SQL.Text := 'select * from user_objects where object_type=''PACKAGE'' and OBJECT_NAME=''rpl$package''';
    Exec;
    if RecordCount > 0 then Result := True
    else Result := False;
  end;
end;*)

procedure TCcOracleAdaptor.EmptyTempTable;
begin
  if (tableExists('RPL$VARS')) then begin
    with FConnection.UpdateQuery['Oracle_EmptyTempTable'] do begin
      Close;
      SQL.Text := 'delete from RPL$VARS';
      Exec;
    end;
  end;
end;

procedure TCcOracleAdaptor.InsertTempTable;
begin
  with FConnection.UpdateQuery['Oracle_qInsertRplVars'] do begin
    Close;
    SQL.Text := 'insert into RPL$VARS (replicating_node) values (''' + FConnection.ReplicatingNode + ''')';
    Exec;
  end;
end;

function TCcOracleAdaptor.ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
begin
	Result := ReplaceString(Trim(cCondition), 'new.', cNewOld + '.');
  Result := ReplaceString(Result, '%%TABLE_NAME', Trim(cTable));
end;

function TCcOracleAdaptor.TranslateDataType(OracleDataType: String): TFieldType;
begin
  Result := ftUnknown;

  if (OracleDataType = 'VARCHAR2') or (OracleDataType = 'NVARCHAR2') then
    Result := ftString;

  if (OracleDataType = 'NUMBER') or (OracleDataType = 'FLOAT') or (OracleDataType = 'BINARY_FLOAT') or (OracleDataType = 'BINARY_DOUBLE') then
    Result := ftFloat;

  if (OracleDataType = 'DATE') then
    Result := ftDateTime;

  if (OracleDataType = 'TIMESTAMP') then
    {$IFDEF CC_D6}
    Result := ftTimeStamp;
    {$ELSE}
    Result := ftDateTime;
    {$ENDIF}

  if (OracleDataType = 'CHAR') or (OracleDataType = 'NCHAR') then
    Result := ftFixedChar;

end;

function TCcOracleAdaptor.GetFieldType(TableName: String ; FieldName: string): String;
begin
	with FConnection.SelectQuery['OracleAdaptor.GetFieldType'] do begin
    Close;
		SQL.Text := 'SELECT DATA_TYPE FROM user_tab_columns WHERE TABLE_NAME = ''' + MetaQuote(TableName) + ''' and COLUMN_NAME = ''' + MetaQuote(FieldName) + ''' ORDER BY DATA_TYPE';
		Exec;
    Result := Trim(Field['DATA_TYPE'].AsString);
	end;
end;

function TCcOracleAdaptor.StringToDateTime(cDate: String ; DataType: TFieldType): Variant;
var
  year : Word;
  month : Word;
  day : Word;
  hour : Word;
  min : Word;
  sec : Word;
  ms : Word;
begin
  Result := cDate;
  if cDate <> '' then begin
    year := StrToInt(Copy(cDate,1,4));
    month := StrToInt(Copy(cDate,5,2));
    day := StrToInt(Copy(cDate,7,2));
    hour := StrToInt(Copy(cDate,10,2));
    min := StrToInt(Copy(cDate,13,2));
    sec := StrToInt(Copy(cDate,16,2));
    if DataType = ftDateTime then begin
      ms := 0;
      Result := EncodeDate(year,month,day) + EncodeTime(hour,min,sec,ms);
    end
    else begin
      ms := StrToInt(Copy(cDate,19,3));;
      {$IFDEF CC_D6}
      Result := VarSQLTimeStampCreate(EncodeDateTime(year,month,day,hour,min,sec,ms));
      {$ELSE}
      Result := EncodeDate(year,month,day) + EncodeTime(hour,min,sec,ms);
      {$ENDIF}
    end;
  end;
end;

procedure TCcOracleAdaptor.BeforeConnect;
begin
  inherited;

end;

procedure TCcOracleAdaptor.CheckCustomMetadata;
begin
  if not tableExists('RPL$VARS') then begin
    Query.Add('CREATE GLOBAL TEMPORARY TABLE RPL$VARS (replicating_node varchar2(100)) ON COMMIT DELETE ROWS');
    ExecConfQuery;
    InsertTempTable;
	end;
end;

procedure TCcOracleAdaptor.CheckExtraCustomMetadata;
begin
  inherited;

end;

procedure TCcOracleAdaptor.CleanupTransaction;
begin
  inherited;
  EmptyTempTable;
end;

function TCcOracleAdaptor.ConvertValue(Val: Variant;
  DataType: TFieldType): Variant;
begin
  if {$IFDEF CC_D2K9} (VarType(Val) = varUString) or {$ENDIF}(VarType(Val) = varString) then begin
    if (DataType = ftDateTime){$IFDEF CC_D6} or (DataType = ftTimeStamp) {$ENDIF} then
      Result := StringToDateTime(Val,DataType)
    else
      Result := Val;
  end
  else
    Result := Val;
end;

constructor TCcOracleAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['11.2', '12.1']);
end;

procedure TCcOracleAdaptor.CreateProcedures;
begin
  inherited;

end;

function TCcOracleAdaptor.DeclareField(FieldName: String; FieldType: TFieldType;
  Length: Integer; NotNull, PK, AutoInc: Boolean): String;
begin
  Result := FieldName + ' ' + GetDeclaration(FieldType, Length);

  if NotNull then
    Result := Result + ' NOT NULL';
  if PK then
    Result := Result + ' PRIMARY KEY';
end;

procedure TCcOracleAdaptor.DeclareGenerator(GenName: String);
begin
  Query.Clear;
  Query.Add('CREATE SEQUENCE ' + MetaQuote(GenName) + ' MINVALUE 1 START WITH 1 INCREMENT BY 1');
  ExecConfQuery;
end;

function TCcOracleAdaptor.DeclarePK(FieldNames: String): String;
begin
  Result := 'primary key (' + FieldNames + ')';
end;

destructor TCcOracleAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

procedure TCcOracleAdaptor.DoListAllProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['OracleAdaptor.DoListProcedures'] do begin
    Close;
		SQL.Text := 'SELECT OBJECT_NAME FROM USER_OBJECTS WHERE OBJECT_TYPE = ''PROCEDURE'' ORDER BY OBJECT_NAME';
    Exec;
		while not Eof do begin
			list.Add(Trim(Field['OBJECT_NAME'].AsString));
			Next;
  end;
	end;
end;

procedure TCcOracleAdaptor.DoListFieldsForNoPK(cTableName: String;
  list: TStringList);
begin
 	with FConnection.SelectQuery['OracleAdaptor.DoListFieldsForNoPK'] do begin
 		Close;
				SQL.Text := 'SELECT COLUMN_NAME as field_name FROM USER_TAB_COLS c WHERE TABLE_NAME = :table_name AND DATA_LENGTH <= 100 '
        + 'AND VIRTUAL_COLUMN = ''NO'' AND EXISTS (SELECT * FROM USER_TABLES t WHERE c.TABLE_NAME = t.TABLE_NAME) ORDER BY COLUMN_NAME';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoListGenerators(list: TStringList);
begin
	with FConnection.SelectQuery['OracleAdaptor.DoListGenerators'] do begin
		Close;
		SQL.Text := 'SELECT SEQUENCE_NAME as generator_name FROM USER_SEQUENCES ORDER BY SEQUENCE_NAME';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['generator_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoListPrimaryKeys(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['OracleAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'SELECT cols.COLUMN_NAME as pk_name FROM USER_CONSTRAINTS cons, USER_CONS_COLUMNS cols '
                + 'WHERE cols.TABLE_NAME = :table_name AND cons.CONSTRAINT_TYPE = ''P'' ' +
                'AND cons.CONSTRAINT_NAME = cols.CONSTRAINT_NAME ORDER BY cols.COLUMN_NAME';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['pk_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoListProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['OracleAdaptor.DoListProcedures'] do begin
		Close;
		SQL.Text := 'SELECT DISTINCT a.OBJECT_NAME FROM USER_ARGUMENTS a , USER_PROCEDURES p '
          + 'WHERE IN_OUT <> ''IN'' AND p.OBJECT_NAME = a.OBJECT_NAME AND p.OBJECT_TYPE = ''PROCEDURE'' ';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['OBJECT_NAME'].AsString));
			Next;
	end;
	end;
end;

procedure TCcOracleAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
begin
 	with FConnection.SelectQuery['OracleAdaptor.DoListTableFields'] do begin
 		Close;
		SQL.Text := 'SELECT COLUMN_NAME as field_name FROM USER_TAB_COLUMNS c WHERE TABLE_NAME = :table_name '
              +  'AND EXISTS (SELECT * FROM USER_TABLES t WHERE c.TABLE_NAME = t.TABLE_NAME) ORDER BY COLUMN_NAME';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
	with FConnection.SelectQuery['OracleAdaptor.DoListTables'] do begin
    Close;
		SQL.Text := 'SELECT TABLE_NAME FROM user_tables ORDER BY TABLE_NAME';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['TABLE_NAME'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoListTriggers(list: TStringList);
begin
	with FConnection.SelectQuery['OracleAdaptor.DoListTriggers'] do begin
		Close;
		SQL.Text := 'SELECT TRIGGER_NAME FROM USER_TRIGGERS ORDER BY TRIGGER_NAME';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['TRIGGER_NAME'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoListUpdatableTableFields(cTableName: String;
  list: TStringList);
begin
 	with FConnection.SelectQuery['OracleAdaptor.DoListUpdatableTableFields'] do begin
 		Close;
		SQL.Text := 'SELECT COLUMN_NAME as field_name FROM USER_TAB_COLS c WHERE TABLE_NAME = :table_name '
        + 'AND VIRTUAL_COLUMN = ''NO'' AND EXISTS (SELECT * FROM USER_TABLES t WHERE c.TABLE_NAME = t.TABLE_NAME) ORDER BY COLUMN_NAME';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcOracleAdaptor.DoRegisterNode(NodeName: String);
begin
  inherited;

end;

procedure TCcOracleAdaptor.DropProcedures;
begin
  inherited;

end;

function TCcOracleAdaptor.GenDeclared(GenName: String): Boolean;
begin
	with FConnection.SelectQuery['Oracle_FINDGEN'] do begin
		Close;
    SQL.Text := 'SELECT SEQUENCE_NAME FROM USER_SEQUENCES WHERE SEQUENCE_NAME=:gen_name';
    Param['gen_name'].AsString := GenName;
    Exec;
    if RecordCount > 0 then
      Result := True
    else
      Result := False;
  end;
end;

function TCcOracleAdaptor.GenerateTriggers(qTable, qTableConf: TCcQuery;
  FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;
var
  cTriggerName: String;
	TableName, ConfigName: String;
  lConditionField: Boolean;
  lCondition : Boolean;
  cSync: String;

  procedure DoGenerateSQL(cNewOld: String);
  var
    I: Integer;
    cRefField: String;

  begin

			cRefField := 'null';

    // Condition de old et new
    if cNewOld = 'new' then
      Query.Add('  IF UPDATING or INSERTING THEN')
    else
      Query.Add('  IF UPDATING OR DELETING THEN');


    // Recuperation des cles primaires
    Query.Add('  primary_key_values := '''';');
    for I := 0 to FKeys.Count - 1 do begin
      if (FKeys[i].PrimaryKey) then begin
        Query.Add('    IF :' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) +' IS NULL THEN');
        Query.Add('      primary_key_values := primary_key_values || ''"'' || '';'';');
        Query.Add('    ELSE');
        // Formate les date et timestamp
        if GetFieldType(TableName,FKeys[i].KeyName) = 'DATE' then
          Query.Add('      quoted_value := REPLACE(to_char(:' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ',''yyyymmdd hh24:mi:ss''),q''('')'',q''('''')'');')
        else if Pos('TIMESTAMP', GetFieldType(TableName,FKeys[i].KeyName)) > 0 then
          Query.Add('      quoted_value := REPLACE(to_char(:' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ',''yyyymmdd hh24:mi:ss.FF3''),q''('')'',q''('''')'');')
        else
          Query.Add('      quoted_value := REPLACE(:' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ',q''('')'',q''('''')'');');
        Query.Add('      primary_key_values := primary_key_values || q''('')'' || quoted_value || q''('')'' || '';'';');
        Query.Add('    END IF;');
    end;
    end;

    // Synchronisation des clés primaires et uniques
    Query.Add('    primary_key_sync := '''';');
    Query.Add('    unique_key_sync := '''';');
    if cNewOld = 'old' then
      Query.Add('    IF not DELETING THEN');

    for I := 0 to FKeys.Count - 1 do begin
      cSync := FKeys[i].GenericSyncStatement;
      if (FKeys[i].PrimaryKey) then begin
        Query.Add('      quoted_value := REPLACE(''' + cSync + ''',q''('')'',q''('''')'');');
        Query.Add('      primary_key_sync := primary_key_sync || q''('')'' || quoted_value || q''('')'' || '';'';');
      end
      else begin
        Query.Add('      quoted_value := REPLACE(''' + cSync + ''',q''('')'',q''('''')'');');
        Query.Add('      unique_key_sync := unique_key_sync || q''('')'' || quoted_value || q''('')'' || '';'';');
  end;
    end;
    if cNewOld = 'old' then
      Query.Add('    END IF;');


    // Creation du curseur pour la boucle
    Query.Add('    DECLARE');
    Query.Add('      CURSOR c IS');
    Query.Add('        SELECT u.login , t.repl_updates, t.repl_inserts , t.repl_deletes');

    if (ConfigName <> '') then begin
      Query.Add('        FROM RPL$users u , rpl$tables_config t');
      Query.Add('        WHERE t.table_name = ' + QuotedStr(TableName) + ' and t.config_name = ' + QuotedStr(ConfigName));
    end
    else begin
      Query.Add('        FROM RPL$users u , rpl$tables t');
      Query.Add('        WHERE t.table_name = ' + QuotedStr(TableName));
    end;

    Query.Add('        and (u.login <> replicating_node or replicating_node is null)');

    if (ConfigName <> '') then
      Query.Add('        and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

  //User-definable SQL condition, configurable per table in RPL$TABLES
		if lCondition then
      Query.Add('        and (' + ParseSQLCondition(TableName, qTable.Field['CONDITION'].AsString, cNewOld) + ')');

    if Assigned(qTableConf) then begin
      if Trim(qTableConf.Field['CONDITION'].AsString) <> '' then
        Query.Add('        and (' + ParseSQLCondition(TableName, qTableConf.Field['CONDITION'].AsString, cNewOld) + ')');
      if Trim(qTableConf.Field['UPDATE_CONDITION'].AsString) <> '' then
        Query.Add('        and ((not operation_type = ''updating'') or (' + ParseSQLCondition(TableName, qTableConf.Field['UPDATE_CONDITION'].AsString, cNewOld) + ') )');
      if Trim(qTableConf.Field['DELETE_CONDITION'].AsString) <> '' then
        Query.Add('        and ( (not operation_type = ''deleting'') or (' + ParseSQLCondition(TableName, qTableConf.Field['DELETE_CONDITION'].AsString, cNewOld) + ') )');
      if Trim(qTableConf.Field['INSERT_CONDITION'].AsString) <> '' then
        Query.Add('        and ( (not operation_type = ''inserting'') or (' + ParseSQLCondition(TableName, qTableConf.Field['INSERT_CONDITION'].AsString, cNewOld) + ') )');
    end;

    //If the condition field is null, the record is replicated to any user
    //If RPL$USERS.Condition_value is null, the user gets all changes
    if lConditionField then
      Query.Add('        and ((:' + cNewOld + '.' + Trim(qTable.Field['CONDITION_FIELD'].AsString) + ' = u.condition_value) or (u.condition_value is null))');

    Query.Add('        ;');


    // Boucle sur les differents utilisateurs
    Query.Add('    BEGIN');
    Query.Add('      FOR user IN c LOOP');

    if cNewOld = 'new' then
      Query.Add('        IF ( (user.repl_updates = ''Y'' and UPDATING) or (user.repl_inserts = ''Y'' and INSERTING)) THEN')
    else
      Query.Add('        IF ( (user.repl_updates = ''Y'' and UPDATING) or (user.repl_deletes = ''Y'' and DELETING)) THEN');

    Query.Add('          INSERT INTO rpl$log (code,login , sent_from , operation_date,table_name,primary_key_values,primary_key_sync,unique_key_sync) VALUES ( GEN_RPL$LOG.nextval ,user.login , ora_login_user , SYSTIMESTAMP ,' + QuotedStr(TableName) + ',primary_key_values,primary_key_sync,unique_key_sync);');
    Query.Add('        END IF;');
    Query.Add('      END LOOP;');
    Query.Add('    END;');
    Query.Add('  END IF;');

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

  lConditionField := (Trim(qTable.Field['CONDITION_FIELD'].AsString) <> '');
  lCondition := (Trim(qTable.Field['CONDITION'].AsString) <> '');

  // Declaration du trigger
  Query.Add('CREATE OR REPLACE TRIGGER ' + cTriggerName);
  Query.Add('AFTER DELETE OR INSERT OR UPDATE ON ' + TableName + ' FOR EACH ROW');

  // Declaration des variables
  Query.Add('DECLARE');
  Query.Add('  primary_key_values CLOB;');
  Query.Add('  primary_key_sync CLOB;');
  Query.Add('  unique_key_sync CLOB;');
  Query.Add('  quoted_value VARCHAR2(200);');
  Query.Add('  replicating_node varchar2(100) := '''';');
  Query.Add('  operation_type VARCHAR2(20);');

  // Début du trigger
  Query.Add('BEGIN');

  // Met en place la verification de l'operation
  Query.Add('  IF UPDATING THEN');
  Query.Add('    operation_type := ''updating'';');
  Query.Add('  END IF;');
  Query.Add('  IF DELETING THEN');
  Query.Add('    operation_type := ''deleting'';');
  Query.Add('  END IF;');
  Query.Add('  IF INSERTING THEN');
  Query.Add('    operation_type := ''inserting'';');
  Query.Add('  END IF;');

  // Recuperation de replicating_node
  Query.Add('  BEGIN');
  Query.Add('    SELECT * INTO replicating_node FROM RPL$VARS;');
  Query.Add('  EXCEPTION');
  Query.Add('    WHEN NO_DATA_FOUND THEN');
  Query.Add('      NULL;');
  Query.Add('  END;');

  DoGenerateSQL('old');
  DoGenerateSQL('new');

  // Fin du trigger
  Query.Add('END;');

  ExecConfQuery;
end;

class function TCcOracleAdaptor.GetAdaptorName: String;
begin
  Result := 'Oracle';
end;

function TCcOracleAdaptor.GetCurrentTimeStampSQL: String;
begin
  Result := 'SYSTIMESTAMP';
end;

function TCcOracleAdaptor.GetGenerator(GenName: String;
  Increment: Integer): String;
begin
  Result := Trim(GenName) + '.nextval';
end;

function TCcOracleAdaptor.GetGeneratorValue(GenName: String;
  Increment: Integer):
      {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}
begin
with FConnection.SelectQuery['Oracle_GetGeneratorValue'] do begin
    Close;
    SQL.Text := 'select ' + GenName + '.nextval as code from dual';
    Exec;
    Result := Field['code'].Value;
  end;
end;


(*
procedure TCcOracleAdaptor.GetProcParams(ProcName: String; Params: TDataSet;
  InputParam: Boolean);
var
  nParamType:String;
begin
  if InputParam then nParamType := '(''IN'', ''IN/OUT'')'
                else nParamType := '(''OUT'', ''IN/OUT'')';
  Params.Close;
  Params.Open;
  if (ProcName <> '') then
  with FConnection.SelectQuery['Oracle_GetProcParams'] do
  begin
    Close;
		SQL.Text := 'SELECT ARGUMENT_NAME as param_name , DATA_TYPE as field_type, DATA_LENGTH as field_length ' +
              'FROM user_arguments' + ' WHERE IN_OUT IN ' + nParamType + ' and OBJECT_NAME = ''' + ProcName + '''';
    Exec;
    while (not Eof) do
      begin
      Params.Append;
      Params.FieldByName('PARAM_NAME').AsString := Trim(Field['PARAM_NAME'].AsString);
      Params.FieldByName('FIELD_TYPE').AsInteger := Integer(TranslateDataType(Field['FIELD_TYPE'].AsString));
      Params.FieldByName('FIELD_LENGTH').AsInteger := Field['FIELD_LENGTH'].AsInteger;
      Params.Post;
      Next;
    end;
  end;
end;
*)

function TCcOracleAdaptor.GetQuoteMetadata: Boolean;
begin
  Result := True;
end;

function TCcOracleAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := False;
end;

procedure TCcOracleAdaptor.InitConnection;
begin
    inherited;
    if (tableExists('RPL$VARS')) then begin
      EmptyTempTable;
      InsertTempTable;
  end;
end;

procedure TCcOracleAdaptor.InitTransaction;
begin
  inherited;
  if ((tableExists('RPL$VARS')) and (FConnection.ReplicatingNode <> '')) then begin
    with FConnection.SelectQuery['Oracle_CheckTempTableEmpty'] do begin
      Close;
      SQL.Text := 'select count(*) as reccount from RPL$VARS';
      Exec;

      //If the table is already empty, we can safely continue
      //Since we are using a snapshot transaction, only our own temporary data will
      //be visible to us, during the lifetime of the current transaction, even if
      //other simulatenous transactions also insert their own data.

      //If there is still data in the table, we must delete it
      if Field['reccount'].Value > 0 then begin
        try
          EmptyTempTable;
        except
              //If deleting fails, it means that another transaction has already deleted these records.
              //We can therefore rollback and start the transaction again.
          FConnection.Rollback;
          FConnection.StartTransaction;
  end;
      end;
    end;
    //Then insert our temporary data. As soon as the transaction is committed,
    //this data must be deleted.
    InsertTempTable;
  end;
end;

function TCcOracleAdaptor.MaxDDLNameLength: Integer;
begin
  Result := 30;
end;

procedure TCcOracleAdaptor.RemoveCustomMetadata;
begin

end;

procedure TCcOracleAdaptor.RemoveExtraCustomMetadata;
begin
  inherited;

end;

procedure TCcOracleAdaptor.RemoveTriggers(qTable: TCcQuery);
begin
  if (TriggerExists(Trim(qTable.Field['TRIG_BASE_NAME'].AsString))) then begin
    Query.Add('DROP TRIGGER ' + Trim(qTable.Field['TRIG_BASE_NAME'].AsString));
    ExecConfQuery;
  end;
end;

procedure TCcOracleAdaptor.SetVersion(const Value: String);
begin
  if (Trim(Value) <> '') then begin
      {$IFDEF CC_D2K13}FormatSettings.{$ENDIF}DecimalSeparator := '.';
      FBranchVersion := Round(StrToFloat(Trim(Value)) * 10);
  end;
end;

function TCcOracleAdaptor.SQLFormatValue(Data: Variant; FieldType: TFieldType): String;
var
  DataType :Integer;
begin
  if VarIsEmpty(Data) or VarIsNull(Data) then
    Result := 'null'
  else begin
    DataType := VarType(Data);
    if (DataType = varString) {$IFDEF CC_D2K9}or (DataType = varUString) {$ENDIF}then
      Result := QuotedStr(Data)
    {$IFDEF CC_D6}
    else if (FieldType = ftTimeStamp) then
      Result := 'TO_TIMESTAMP(''' + SQLTimeStampToStr('yyyymmdd hh.nn.ss.zzz',VarToSQLTimeStamp(Data)) + ''',''yyyymmdd hh24.mi.ss.FF3'')'
    {$ENDIF}
    else if (FieldType = ftDateTime) then
      Result := 'TO_DATE(''' + FormatDateTime('yyyymmdd hh.nn.ss',Data) + ''',''yyyymmdd hh24.mi.ss'')'
    else if (FieldType = ftFloat) {$IFDEF CC_D6}or (FieldType = ftfmtBCD){$ENDIF} {$IFDEF CC_D2K13}or (FieldType = ftSingle) {$ENDIF}or (FieldType = ftBCD) then
      Result := QuotedStr(Data)
    else
      Result := Data;
  end;
end;

function TCcOracleAdaptor.SupportsGenerators: Boolean;
begin
  Result := True;
end;

end.

