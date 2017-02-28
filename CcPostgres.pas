unit CcPostgres;

{$I CC.INC}

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys  {$IFDEF CC_D6} ,DateUtils,SqlTimSt,StrUtils{$ENDIF};

type

{$I CC.INC}


//Database adaptor for Postgres
//See also:
//TCcDBAdaptor
TCcPostgresAdaptor = class (TCcDBAdaptor)
  private
    FBranchVersion: Integer;
    procedure EmptyTempTable;
    procedure InsertTempTable;
    function ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
    function StringToTimeStamp(cDate: String): Variant;
    function StringToTime(cDate: String): Variant;
    function StringToDate(cDate: String): TDateTime;
    function GetFieldType(TableName: String ; FieldName: string): String;

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
    function UnQuotedIdentifier(identifier: String):String;override;
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

uses {$IFDEF CC_USEVARIANTS}Variants, FMTBcd, {$ENDIF}Math;



{ TCcPostgresAdaptor }

procedure TCcPostgresAdaptor.EmptyTempTable;
begin
  if (tableExists('rpl$vars')) then begin
    with FConnection.UpdateQuery['Postgres_EmptyTempTable'] do begin
      Close;
      SQL.Text := 'TRUNCATE rpl$vars';
      Exec;
    end;
  end;
end;

procedure TCcPostgresAdaptor.InsertTempTable;
begin
  with FConnection.UpdateQuery['Postgres_qInsertRplVars'] do begin
    Close;
    SQL.Text := 'insert into rpl$vars (replicating_node) values (''' + FConnection.ReplicatingNode + ''')';
    Exec;
  end;
end;

function TCcPostgresAdaptor.ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
begin
	Result := ReplaceString(Trim(cCondition), 'new.', cNewOld + '.');
  Result := ReplaceString(Result, '%%TABLE_NAME', Trim(cTable));
end;

procedure TCcPostgresAdaptor.BeforeConnect;
begin
  inherited;

end;

procedure TCcPostgresAdaptor.CheckCustomMetadata;
begin
end;

procedure TCcPostgresAdaptor.CheckExtraCustomMetadata;
begin
  inherited;

end;

procedure TCcPostgresAdaptor.CleanupTransaction;
begin
  inherited;
end;

function TCcPostgresAdaptor.ConvertValue(Val: Variant;
  DataType: TFieldType): Variant;
{$IFDEF CC_D2K13}
var
  fs: TFormatSettings;
{$ENDIF}
begin
   if ({$IFDEF CC_D2K9}(VarType(Val) = varUString) or {$ENDIF} (VarType(Val) = varString) or (VarType(Val) = varOleStr) ) and (not AnsiSameText(Val,'')) then begin
      if (DataType = ftInteger) or (DataType = ftLargeInt) or (DataType = ftWord) or (DataType = ftSmallInt) then
        Result := StrToInt(Val)
      {$IFDEF CC_D6}
      else if (DataType = ftBCD) or (DataType = ftFMTBcd) then begin
        {$IFDEF CC_D2K13}fs.{$ENDIF}DecimalSeparator := '.';
        Result := VarFMTBcdCreate(StrToBcd(Val{$IFDEF CC_D2K13}, fs{$ENDIF}));
      end
      {$ENDIF}
      else if (DataType = ftFloat) {$IFDEF CC_D2K13}or (DataType = ftSingle) {$ENDIF}then begin
        {$IFDEF CC_D2K13}fs.{$ENDIF}DecimalSeparator := '.';
        Result := StrToFloat(Val {$IFDEF CC_D2K13}, fs {$ENDIF});
      end
      else if DataType = ftTime then
        Result := StringToTime(Val)
      else if DataType = ftDate then
        Result := StringToDate(Val)
      {$IFDEF CC_D6}
      else if DataType = ftTimeStamp then
        Result := StringToTimeStamp(Val)
      {$ENDIF}
      {$IFDEF CC_D6}
      else if DataType = ftBoolean then
        Result := StrToBool(Val)
      {$ENDIF}
      else if (DataType = ftGuid) and (Copy(Val,1,1) <> '{') then
        Result := '{' + Copy(Val,7,2) + Copy(Val,5,2) + Copy(Val,3,2) + Copy(Val,1,2) + '-'
          + Copy(Val,12,2) + Copy(Val,10,2) + '-' + Copy(Val,17,2) + Copy(Val,15,2) + '-' + Copy(Val,20,17) + '}'
      else
        Result := Val;
  end
  else
    Result := Val;
end;

constructor TCcPostgresAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['9.3']);
end;

procedure TCcPostgresAdaptor.CreateProcedures;
begin
  inherited;

end;

function TCcPostgresAdaptor.DeclareField(FieldName: String;
  FieldType: TFieldType; Length: Integer; NotNull, PK,
  AutoInc: Boolean): String;
begin
  Result := FieldName;

    case (FieldType) of
      ftInteger:
        if AutoInc then
          Result := Result + ' SERIAL'
        else
          Result := Result + ' INTEGER';
      ftFloat: Result := Result + ' REAL';
      ftTime: Result := Result + ' TIME';
      ftDate: Result := Result + ' DATE';
      {$IFDEF CC_D6} ftTimeStamp,{$ENDIF}ftDateTime: Result := Result + ' TIMESTAMP';
      ftString: Result := Result + ' CHARACTER VARYING(' + InttoStr(Length) + ')';
      ftFixedChar: Result := Result + ' CHAR(' + InttoStr(Length) + ')';
      ftBlob: Result := Result + ' BYTEA';
      ftMemo: Result := Result + ' TEXT';
    else
      raise Exception.Create('Data type ' + IntToStr(Integer(FieldType)) + ' not handled by TCcPostgresAdaptor!');
  end;

  if NotNull then
    Result := Result + ' NOT NULL';
  if PK then
    Result := Result + ' PRIMARY KEY';
end;

procedure TCcPostgresAdaptor.DeclareGenerator(GenName: String);
begin
  inherited;

end;

function TCcPostgresAdaptor.DeclarePK(FieldNames: String): String;
begin
  Result := 'primary key (' + FieldNames + ')';
end;

destructor TCcPostgresAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

procedure TCcPostgresAdaptor.DoListAllProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['PostgresAdaptor.DoListAllProcedures'] do begin
		Close;
		SQL.Text := 'SELECT routine_name as procedure_name FROM information_schema.routines '
          + 'WHERE routine_schema NOT IN (''pg_catalog'',''information_schema'') '
          + 'ORDER BY routine_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['procedure_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListFieldsForNoPK(cTableName: String;
  list: TStringList);
begin
 	with FConnection.SelectQuery['PostgresAdaptor.DoListFieldsForNoPK'] do begin
 		Close;
		SQL.Text := 'SELECT column_name as field_name FROM information_schema.columns c WHERE table_name = :table_name '
              +  'AND EXISTS (SELECT * FROM information_schema.tables t WHERE table_type = ''BASE TABLE'' AND c.table_name = t.table_name) '
              + 'AND (data_type NOT IN (''USER-DEFINED'',''bit'',''bit varying'',''bytea'',''character'',''character varying'',''text'',''xml'',''ARRAY'') '
              + 'OR character_octet_length < 200) ORDER BY column_name';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListGenerators(list: TStringList);
begin
	with FConnection.SelectQuery['PostgresAdaptor.DoListGenerators'] do begin
		Close;
		SQL.Text := 'SELECT sequence_name as generator_name FROM information_schema.sequences ORDER BY sequence_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['generator_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListPrimaryKeys(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['PostgresAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'SELECT kc.column_name as pk_name FROM information_schema.table_constraints tc, '
              + 'information_schema.key_column_usage kc WHERE tc.constraint_type = ''PRIMARY KEY'' '
              + 'AND kc.table_name = tc.table_name AND kc.table_name = :table_name AND kc.table_schema = tc.table_schema '
              + 'AND kc.constraint_name = tc.constraint_name ORDER BY kc.column_name';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['pk_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['PostgresAdaptor.DoListProcedures'] do begin
		Close;
		SQL.Text := 'SELECT routine_name as procedure_name FROM information_schema.routines '
          + 'WHERE routine_schema NOT IN (''pg_catalog'',''information_schema'') '
          + 'AND type_udt_name = ''void'' ORDER BY routine_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['procedure_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
begin
 	with FConnection.SelectQuery['PostgresAdaptor.DoListTableFields'] do begin
 		Close;
		SQL.Text := 'SELECT column_name as field_name FROM information_schema.columns c WHERE table_name = :table_name '
              +  'AND EXISTS (SELECT * FROM information_schema.tables t WHERE table_type = ''BASE TABLE'' AND c.table_name = t.table_name) ORDER BY column_name';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
	with FConnection.SelectQuery['PostgresAdaptor.DoListTables'] do begin
    Close;
		SQL.Text := 'SELECT table_name FROM information_schema.tables ' +
      'WHERE table_schema NOT IN (''pg_catalog'', ''information_schema'') ' +
      'and table_type in (''BASE TABLE'',''LOCAL TEMPORARY'') ORDER BY table_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['TABLE_NAME'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListTriggers(list: TStringList);
begin
	with FConnection.SelectQuery['PostgresAdaptor.DoListTriggers'] do begin
		Close;
		SQL.Text := 'SELECT DISTINCT trigger_name FROM information_schema.triggers ORDER BY trigger_name';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['TRIGGER_NAME'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoListUpdatableTableFields(cTableName: String;
  list: TStringList);
begin
 	with FConnection.SelectQuery['PostgresAdaptor.DoListUpdatableTableFields'] do begin
 		Close;
		SQL.Text := 'SELECT column_name as field_name FROM information_schema.columns c WHERE table_name = :table_name '
              +  'AND EXISTS (SELECT * FROM information_schema.tables t WHERE table_type = ''BASE TABLE'' '
              + 'AND c.table_name = t.table_name) and is_updatable = ''YES'' ORDER BY column_name';
    Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcPostgresAdaptor.DoRegisterNode(NodeName: String);
begin
  inherited;

end;

procedure TCcPostgresAdaptor.DropProcedures;
begin
  inherited;

end;

function TCcPostgresAdaptor.GenDeclared(GenName: String): Boolean;
begin
	with FConnection.SelectQuery['Postgres_FINDGEN'] do begin
		Close;
    SQL.Text := 'SELECT sequence_name FROM information_schema.sequences WHERE sequence_name=:gen_name';
    Param['gen_name'].AsString := GenName;
    Exec;
    if RecordCount > 0 then
      Result := True
    else
      Result := False;
  end;
end;

function TCcPostgresAdaptor.GenerateTriggers(qTable, qTableConf: TCcQuery;
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
      Query.Add('  if TG_OP = ''UPDATE'' or TG_OP = ''INSERT'' then')
    else
      Query.Add('  if TG_OP = ''UPDATE'' or TG_OP = ''DELETE'' then');


    // Recuperation des cles primaires
    Query.Add('    primary_key_values := '''';');
    for I := 0 to FKeys.Count - 1 do begin
      if (FKeys[i].PrimaryKey) then begin
        Query.Add('    IF ' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) +' IS NULL THEN');
        Query.Add('      primary_key_values := primary_key_values || ''";'';');
        Query.Add('    ELSE');
        if Pos('timestamp', GetFieldType(TableName,FKeys[i].KeyName)) > 0 then
          Query.Add('      primary_key_values := primary_key_values || quote_literal(to_char(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ',''yyyymmdd hh24:mi:ss.ms'')) || '';'';')
        else if Pos('date', GetFieldType(TableName,FKeys[i].KeyName)) > 0 then
          Query.Add('      primary_key_values := primary_key_values || quote_literal(to_char(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ',''yyyymmdd'')) || '';'';')
        else if Pos('time', GetFieldType(TableName,FKeys[i].KeyName)) > 0 then
          Query.Add('      primary_key_values := primary_key_values || quote_literal(to_char(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ',''hh24:mi:ss.ms'')) || '';'';')
        else
          Query.Add('      primary_key_values := primary_key_values || quote_literal(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ') || '';'';');
        Query.Add('    END IF;');
      end;
    end;


    // Synchronisation des clés primaires et uniques
    Query.Add('    primary_key_sync := '''';');
    Query.Add('    unique_key_sync := '''';');
    if cNewOld = 'old' then
      Query.Add('    IF TG_OP <> ''DELETE'' THEN');

    for I := 0 to FKeys.Count - 1 do begin
      cSync := FKeys[i].GenericSyncStatement;
      if (FKeys[i].PrimaryKey) then
        Query.Add('      primary_key_sync := primary_key_sync || quote_nullable(''' + cSync + ''') || '';'';')
      else
        Query.Add('      unique_key_sync := unique_key_sync || quote_nullable(''' + cSync + ''') || '';'';')
    end;

    if cNewOld = 'old' then
      Query.Add('    END IF;');


    // Condition pour la boucle
    Query.Add('    for line in');
    Query.Add('      SELECT u.login , t.repl_updates, t.repl_inserts , t.repl_deletes');

    if (ConfigName <> '') then begin
      Query.Add('      FROM RPL$users u , rpl$tables_config t');
      Query.Add('      WHERE t.table_name = ' + QuotedStr(TableName) + ' and t.config_name = ' + QuotedStr(ConfigName));
    end
    else begin
      Query.Add('      FROM RPL$users u , rpl$tables t');
      Query.Add('      WHERE t.table_name = ' + QuotedStr(TableName));
    end;

    Query.Add('      and (u.login <> replicating_node or replicating_node is null)');

    if (ConfigName <> '') then
      Query.Add('      and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

    //User-definable SQL condition, configurable per table in RPL$TABLES
		if lCondition then
      Query.Add('      and (' + ParseSQLCondition(TableName, qTable.Field['CONDITION'].AsString, cNewOld) + ')');

    if Assigned(qTableConf) then begin
      if Trim(qTableConf.Field['CONDITION'].AsString) <> '' then
        Query.Add('      and (' + ParseSQLCondition(TableName, qTableConf.Field['CONDITION'].AsString, cNewOld) + ')');
      if Trim(qTableConf.Field['UPDATE_CONDITION'].AsString) <> '' then
        Query.Add('      and ((not TG_OP = ''update'') or (' + ParseSQLCondition(TableName, qTableConf.Field['UPDATE_CONDITION'].AsString, cNewOld) + ') )');
      if Trim(qTableConf.Field['DELETE_CONDITION'].AsString) <> '' then
        Query.Add('      and ( (not TG_OP = ''delete'') or (' + ParseSQLCondition(TableName, qTableConf.Field['DELETE_CONDITION'].AsString, cNewOld) + ') )');
      if Trim(qTableConf.Field['INSERT_CONDITION'].AsString) <> '' then
        Query.Add('      and ( (not TG_OP = ''insert'') or (' + ParseSQLCondition(TableName, qTableConf.Field['INSERT_CONDITION'].AsString, cNewOld) + ') )');
    end;

    //If the condition field is null, the record is replicated to any user
    //If RPL$USERS.Condition_value is null, the user gets all changes
    if lConditionField then
      Query.Add('      and ((' + cNewOld + '.' + Trim(qTable.Field['CONDITION_FIELD'].AsString) + ' = u.condition_value) or (u.condition_value is null))');

    // Boucle sur les differents utilisateurs
    Query.Add('    LOOP');

    if cNewOld = 'new' then
      Query.Add('      IF ( (line.repl_updates = ''Y'' and TG_OP = ''UPDATE'') or (line.repl_inserts = ''Y'' and TG_OP = ''INSERT'')) THEN')
    else
      Query.Add('      IF ( (line.repl_updates = ''Y'' and TG_OP = ''UPDATE'') or (line.repl_deletes = ''Y'' and TG_OP = ''DELETE'')) THEN');

    Query.Add('        INSERT INTO rpl$log (login , sent_from , operation_date,table_name,primary_key_values,primary_key_sync,unique_key_sync) VALUES (line.login , current_user , CURRENT_TIMESTAMP ,' + QuotedStr(TableName) + ',primary_key_values,primary_key_sync,unique_key_sync);');
    Query.Add('      END IF;');
    Query.Add('    END LOOP;');
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

  // Declaration de la fonction trigger
  Query.Add('CREATE OR REPLACE FUNCTION ' + MetaQuote(cTriggerName + '_function') + '() RETURNS trigger AS');
  Query.Add('$BODY$');

  // Declaration des variables
  Query.Add('DECLARE');
  Query.Add('  primary_key_values TEXT;');
  Query.Add('  primary_key_sync TEXT;');
  Query.Add('  unique_key_sync TEXT;');
  Query.Add('  replicating_node varchar(100) := '''';');
  Query.Add('  line record;');

  // Début de la fonction trigger
  Query.Add('BEGIN');

  // Recuperation de replicating_node
  Query.Add('  BEGIN');
  Query.Add('    SELECT * INTO replicating_node FROM RPL$VARS;');
  Query.Add('  EXCEPTION');
  Query.Add('    WHEN others THEN');
  Query.Add('      NULL;');
  Query.Add('  END;');

  DoGenerateSQL('old');
  DoGenerateSQL('new');

  // Fin du trigger
  Query.Add('  RETURN NULL;');
  Query.Add('END;');
  Query.Add('$BODY$');
  Query.Add('LANGUAGE plpgsql');

  ExecConfQuery;

  // Création du trigger
  Query.Add('CREATE TRIGGER ' + metaQuote(cTriggerName));
  Query.Add('  AFTER INSERT OR UPDATE OR DELETE');
  Query.Add('  ON ' + MetaQuote(TableName));
  Query.Add('  FOR EACH ROW');
  Query.Add('  EXECUTE PROCEDURE ' + MetaQuote(cTriggerName + '_function') + '();');

  ExecConfQuery;
end;

class function TCcPostgresAdaptor.GetAdaptorName: String;
begin
  Result := 'Postgres';
end;

function TCcPostgresAdaptor.GetCurrentTimeStampSQL: String;
begin
  Result := 'CURRENT_TIMESTAMP';
end;

function TCcPostgresAdaptor.GetFieldType(TableName, FieldName: string): String;
begin
	with FConnection.SelectQuery['PostgresAdaptor.GetFieldType'] do begin
    Close;
		SQL.Text := 'SELECT data_type FROM information_schema.columns WHERE table_name = ''' + TableName + ''' and column_name = ''' + FieldName + ''' ORDER BY data_type';
		Exec;
    Result := Trim(Field['data_type'].AsString);
	end;
end;

function TCcPostgresAdaptor.GetGenerator(GenName: String;
  Increment: Integer): String;
begin
  Result := 'nextval(''' + Trim(GenName) + ''')' ;
end;

function TCcPostgresAdaptor.GetGeneratorValue(GenName: String;
  Increment: Integer):
      {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}
begin
with FConnection.SelectQuery['Postgres_GetGeneratorValue'] do begin
    Close;
    SQL.Text := 'select nextval(''' + GenName + ''')';
    Exec;
    Result := Field['nextval'].Value;
  end;
end;

function TCcPostgresAdaptor.GetQuoteMetadata: Boolean;
begin
  Result := True;
end;

function TCcPostgresAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := False;
end;

procedure TCcPostgresAdaptor.InitConnection;
begin
  inherited;
end;

procedure TCcPostgresAdaptor.InitTransaction;
begin
  inherited;
  if ((FConnection.ReplicatingNode <> '')) then begin
    if not tableExists('rpl$vars') then begin
      Query.Add('CREATE TEMPORARY TABLE rpl$vars (replicating_node varchar(100)) ON COMMIT DELETE ROWS');
      ExecConfQuery;
    end;

    with FConnection.SelectQuery['Postgres_CheckTempTableEmpty'] do begin

      Close;
      SQL.Text := 'select count(*) as reccount from rpl$vars';
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

function TCcPostgresAdaptor.MaxDDLNameLength: Integer;
begin
  Result := 31;
end;

procedure TCcPostgresAdaptor.RemoveCustomMetadata;
begin
  inherited;

end;

procedure TCcPostgresAdaptor.RemoveExtraCustomMetadata;
begin
  inherited;

end;

procedure TCcPostgresAdaptor.RemoveTriggers(qTable: TCcQuery);
begin
  if (TriggerExists(Trim(qTable.Field['TRIG_BASE_NAME'].AsString ))) then begin
    Query.Add('DROP FUNCTION ' + MetaQuote(Trim(qTable.Field['TRIG_BASE_NAME'].AsString) + '_function') + '() cascade');
    ExecConfQuery;
  end;
end;

procedure TCcPostgresAdaptor.SetVersion(const Value: String);
{$IFDEF CC_D2K13}
var
  fs : TFormatSettings;
{$ENDIF}
begin
  if (Trim(Value) <> '') then begin
      {$IFDEF CC_D2K13}fs.{$ENDIF}DecimalSeparator := '.';
      FBranchVersion := Round(StrToFloat(Trim(Value){$IFDEF CC_D2K13},fs{$ENDIF}) * 10);
  end;
end;

function TCcPostgresAdaptor.SQLFormatValue(Data: Variant;
  FieldType: TFieldType): String;
var
  DataType :Integer;
{$IFDEF CC_D2K13}
  fs: TFormatSettings;
{$ENDIF}
begin
  if VarIsEmpty(Data) or VarIsNull(Data) then
    Result := 'null'
  else begin
    DataType := VarType(Data);
    if (FieldType = ftGuid) then
        Result := QuotedStr(Copy(Data,8,2) + Copy(Data,6,2) + Copy(Data,4,2) + Copy(Data,2,2) + '-'
          + Copy(Data,13,2) + Copy(Data,11,2) + '-' + Copy(Data,18,2) + Copy(Data,16,2) + '-' + Copy(Data,21,17))
    else if (DataType = varString) {$IFDEF CC_D2K9}or (DataType = varUString) {$ENDIF} then
      Result := QuotedStr(Data)
    {$IFDEF CC_D6}
    else if (FieldType = ftBCD) or (FieldType = ftFMTBcd)then begin
      {$IFDEF CC_D2K13}fs.{$ENDIF}DecimalSeparator := '.';
      Result := BcdToStr(VarToBcd(Data) {$IFDEF CC_D2K13} ,fs {$ENDIF});
    end
    {$ENDIF}
    else if (FieldType = ftFloat) {$IFDEF CC_D2K13} or (FieldType = ftSingle){$ENDIF} then begin
      {$IFDEF CC_D2K13}fs.{$ENDIF}DecimalSeparator := '.';
      Result := FloatToStr(Data{$IFDEF CC_D2K13}, fs{$ENDIF});
    end
    {$IFDEF CC_D6}
    else if (FieldType = ftTimeStamp) then
      Result := 'TO_TIMESTAMP(''' + SQLTimeStampToStr('yyyymmdd hh.nn.ss.zzz',VarToSQLTimeStamp(Data)) + ''',''yyyymmdd hh24.mi.ss.ms'')'
    {$ENDIF}
    else if (FieldType = ftDate) then
      Result := 'TO_DATE(''' + FormatDateTime('yyyymmdd',Data) + ''',''yyyymmdd'')'
    else if (FieldType = ftTime) then
      Result := 'TO_TIMESTAMP(''' + FormatDateTime('hh.nn.ss.zzz',Data) + ''',''hh24.mi.ss.ms'')'
    {$IFDEF CC_D6}
    else if FieldType = ftBoolean then
      Result := BoolToStr(Data)
    {$ENDIF}
    else
    Result := Data;
  end;
end;

function TCcPostgresAdaptor.StringToDate(cDate: String): TDateTime;
var
  year : Word;
  month : Word;
  day : Word;
begin
  year := StrToInt(Copy(cDate,1,4));
  month := StrToInt(Copy(cDate,5,2));
  day := StrToInt(Copy(cDate,7,2));
  Result := EncodeDate(year,month,day);
end;

function TCcPostgresAdaptor.StringToTimeStamp(cDate: String): Variant;
var
  year : Word;
  month : Word;
  day : Word;
  hour : Word;
  min : Word;
  sec : Word;
  ms : Word;
begin
  year := StrToInt(Copy(cDate,1,4));
  month := StrToInt(Copy(cDate,5,2));
  day := StrToInt(Copy(cDate,7,2));
  hour := StrToInt(Copy(cDate,10,2));
  min := StrToInt(Copy(cDate,13,2));
  sec := StrToInt(Copy(cDate,16,2));
  ms := StrToInt(Copy(cDate,19,3));
  {$IFDEF CC_D6}
  Result := VarSQLTimeStampCreate(EncodeDateTime(year,month,day,hour,min,sec,ms));
  {$ELSE}
  Result := EncodeDate(year,month,day) + EncodeTime(hour,min,sec, ms);
  {$ENDIF}
end;

function TCcPostgresAdaptor.StringToTime(cDate: String): Variant;
var
  hour : Word;
  min : Word;
  sec : Word;
  ms : Word;
begin
  hour := StrToInt(Copy(cDate,1,2));
  min := StrToInt(Copy(cDate,4,2));
  sec := StrToInt(Copy(cDate,7,2));
  ms := StrToInt(Copy(cDate,10,3));;
  Result := EncodeTime(hour,min,sec,ms);
end;

function TCcPostgresAdaptor.SupportsGenerators: Boolean;
begin
  Result := False;
end;

function TCcPostgresAdaptor.UnQuotedIdentifier(identifier: String): String;
begin
  Result := Lowercase(identifier);
end;

initialization
//  RegisterDBAdaptors([TCcPostgresAdaptor.GetAdaptorName], [TCcPostgresAdaptor]);

end.
