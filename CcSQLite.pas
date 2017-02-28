unit CcSQLite;

{$I CC.INC}

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys;

type

TCcSQLiteAdaptor = class (TCcDBAdaptor)
	private
		function ConvertToOldGen(cPKGen: String; cNewOld: String): String;
		function ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
		function GetReplicatingNode: string;
    function StringToDate(cDate: String): TDateTime;
	protected
		FKeys : TCcKeyRing;
		function MaxDDLNameLength: Integer;override;
		function GetCurrentTimeStampSQL:String;override;
		function GetUseRowsAffected: Boolean; override;
		function GetQuoteMetadata:Boolean;override;

		procedure DoListTables(list: TStringList; IncludeTempTables: Boolean); override;
		procedure DoListTableFields(cTableName: String; list: TStringList);override;
		procedure DoListUpdatableTableFields(cTableName: String; list: TStringList);override;
		procedure DoListPrimaryKeys(cTableName: String; list: TStringList);override;
		procedure DoListTriggers(list: TStringList);override;
	public
    function SubStringFunction(str: String; start, length: Integer): String;override;
    procedure GrantRightsToTable(tableName: String); override;
    function SupportsGenerators: Boolean;override;
		function SQLFormatValue(Data: Variant; FieldType :TFieldType): String;override;
		procedure CheckCustomMetadata;override;

		function DeclareField(FieldName: String; FieldType: TFieldType;
			Length: Integer; NotNull: Boolean; PK: Boolean; AutoInc: Boolean): String;override;

		function GenDeclared(GenName:String): Boolean;override;
		procedure RemoveTriggers(qTable: TCcQuery);override;
		function GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;override;

    function ConvertValue(Val: Variant; DataType: TFieldType): Variant;override;

		constructor Create(Conn: TCcConnection);override;
		destructor Destroy;override;

    class function GetAdaptorName: String;override;
end;

implementation

{$IFDEF CC_UseVariants} uses Variants; {$ENDIF}

function TCcSQLiteAdaptor.ConvertValue(Val: Variant; DataType: TFieldType): Variant;
var
  valType:Integer;
begin
  valType := VarType(Val);
  if (valType = varString) {$IFDEF CC_D2K9} or (VarType(Val) = varUString){$ENDIF} then begin
    if ((DataType = ftDateTime) or (DataType = ftDate) or (DataType = ftTime)) then
      Result := StringToDate(Val)
    else
      Result := Val;
  end
  else
    Result := Val;
end;


{ TCcSQLiteAdaptor }

function TCcSQLiteAdaptor.SubStringFunction(str: String; start,
  length: Integer): String;
begin
  Result := 'substring(' + str + ',' + IntToStr(start) + ',' + IntToStr(length) + ')';
end;

procedure TCcSQLiteAdaptor.CheckCustomMetadata;
begin
{  if not TableExists('RPL$VARS') then begin
		Query.Add('create table RPL$VARS(replicating_node varchar(100))');
		ExecConfQuery;
	end;}
end;

function TCcSQLiteAdaptor.ConvertToOldGen(cPKGen, cNewOld: String): String;
begin
	Result := ReplaceString(cPKGen, 'new.', cNewOld + '.');
end;

constructor TCcSQLiteAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['3.x']);
end;

function TCcSQLiteAdaptor.DeclareField(FieldName: String;
  FieldType: TFieldType; Length: Integer; NotNull, PK,
  AutoInc: Boolean): String;
begin
	Result := FieldName + ' ';
	case FieldType of
		ftString:
			Result := Result + 'TEXT';
		ftInteger:
			Result := Result + 'INTEGER';
		ftDateTime, ftDate, ftTime:
			Result := Result + 'TIMESTAMP';
		ftBlob, ftMemo:
			Result := Result + 'BLOB';
	end;
	if NotNull then
		Result := Result + ' NOT NULL';
	if PK then
		Result := Result + ' PRIMARY KEY';
end;

destructor TCcSQLiteAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCcSQLiteAdaptor.GetReplicatingNode: string;
begin
  Result := '(select replicating_node())';
//  Result := '(select replicating_node from rpl$vars)';
end;

function TCcSQLiteAdaptor.GenDeclared(GenName: String): Boolean;
begin
  Result := False;
end;

function TCcSQLiteAdaptor.GenerateTriggers(qTable: TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;

var
	TableName, ConfigName, cReplNode : string;
	
	procedure DoGenerateSQL(cNewOld: String; cOperationType: String);
	var
		cPKVals, cCondition: String;
		lCondition: Boolean;
		I: Integer;
	begin
		lCondition := (Trim(qTable.Field['CONDITION'].AsString) <> '');

		//Convert 'new.' to 'old.', if applicable
		cCondition := '0=0';
		if lCondition then
			cCondition := ParseSQLCondition(TableName, qTable.Field['CONDITION'].AsString, cNewOld);

		if Trim(qTableConf.Field['CONDITION'].AsString) <> '' then
			cCondition := cCondition + ' and (' + ParseSQLCondition(TableName, qTableConf.Field['CONDITION'].AsString, cNewOld) + ')';

		if Trim(qTableConf.Field[cOperationType + '_CONDITION'].AsString) <> '' then
			cCondition := cCondition + ' and (' + ParseSQLCondition(TableName, qTableConf.Field[cOperationType + '_CONDITION'].AsString, cNewOld) + ')';

		cPKVals := '';
		for I := 0 to FKeys.Count - 1 do begin
			if (FKeys[i].PrimaryKey) then begin
				if cPKVals <> '' then
					cPKVals := cPKVals + ' || '';'' || ';
				cPKVals := cPKVals + 'quote(cast(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ' as text))';
			end;
		end;
  	cPKVals := cPKVals + ' || '';''';

		Query.Add('begin');
		Query.Add('insert into rpl$log (login, operation_date, table_name, sent_from, primary_key_values)');
		Query.Add('select u.login, current_timestamp, ' + QuotedStr(TableName) + ', ' + cReplNode + ', ' + cPKVals);
		Query.Add('from rpl$users u where ((u.login <> ' + cReplNode + ') or ' + cReplNode + ' is null)');
//		if (ConfigName <> '') then
//			Query.Add('and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');
		Query.Add('and (' + cCondition + ');');
		Query.Add('end');
	end;

var
	cTriggerName, cPKCompare: String;
	i : Integer;
begin
	cReplNode := GetReplicatingNode;

	if not FConnection.InTransaction then
		FConnection.StartTransaction;

	if Assigned(qTableConf) then begin
		cTriggerName := Trim(qTableConf.Field['TRIG_BASE_NAME'].AsString);
		ConfigName := Trim(qTableConf.Field['CONFIG_NAME'].AsString);
	end
	else
		cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString);

	TableName := Trim(qTable.Field['TABLE_NAME'].AsString);

  FKeys.FailIfNoPK := FailIfNoPK;
	with qTable do
		FKeys.LoadKeys(TableName, '', '', '', Field['PRIMARY_KEY_SYNC'].AsString,
			Field['UNIQUE_KEY_NAMES'].AsString, Field['UNIQUE_KEY_SYNC'].AsString);

	Query.Add('create trigger ' + MetaQuote(cTriggerName + '_I') + ' after insert on ' + MetaQuote(TableName));
	DoGenerateSQL('new', 'insert');
	ExecConfQuery;

	Query.Add('create trigger ' + MetaQuote(cTriggerName + '_U1') + ' after update on ' + MetaQuote(TableName));
	DoGenerateSQL('new', 'update');
	ExecConfQuery;

	cPKCompare := '';
	for i:=0 to FKeys.Count -1 do begin
		if (cPKCompare <> '') then
			cPKCompare := cPKCompare + ' or ';
		cPKCompare := cPKCompare + 'old.' + FKeys[i].KeyName + '<>new.' + FKeys[i].KeyName;
	end;

	Query.Add('create trigger ' + MetaQuote(cTriggerName + '_U2') + ' after update on ' + MetaQuote(TableName) + ' when ' + cPKCompare);
	DoGenerateSQL('old', 'update');
	ExecConfQuery;

	Query.Add('create trigger ' + MetaQuote(cTriggerName + '_D') + ' after delete on ' + MetaQuote(TableName));
	DoGenerateSQL('old', 'delete');
	ExecConfQuery;
end;

class function TCcSQLiteAdaptor.GetAdaptorName: String;
begin
  Result := 'SQLite';
end;

function TCcSQLiteAdaptor.GetCurrentTimeStampSQL: String;
begin
	Result := 'current_timestamp';
end;

function TCcSQLiteAdaptor.GetQuoteMetadata: Boolean;
begin
  Result := False;
end;

function TCcSQLiteAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := True;
end;

procedure TCcSQLiteAdaptor.GrantRightsToTable(tableName: String);
begin
//Do nothing (grants not implemented)
end;

function TCcSQLiteAdaptor.MaxDDLNameLength: Integer;
begin
	Result := 255;
end;

function TCcSQLiteAdaptor.ParseSQLCondition(cTable, cCondition: String; 
  cNewOld: String): String;
begin
	Result := ConvertToOldGen(Trim(cCondition), cNewOld);
  Result := ReplaceString(Result, '%%TABLE_NAME', Trim(cTable));
	if (Trim(Result) = '') then
	  Result := '0=0';
end;

procedure TCcSQLiteAdaptor.RemoveTriggers(qTable: TCcQuery);
var
	cTriggerName: String;

	procedure RemoveTrigger(cType: String);
  begin
		Query.Add('DROP TRIGGER IF EXISTS ' + MetaQuote(cTriggerName + '_' + cType));
		ExecConfQuery;
	end;
begin
	cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString);
	RemoveTrigger('I');
	RemoveTrigger('U1');
	RemoveTrigger('U2');
	RemoveTrigger('D');
end;

function TCcSQLiteAdaptor.SQLFormatValue(Data: Variant;
	FieldType: TFieldType): String;
var
	cData: string;
	DataType: Integer;  
begin
  if VarIsEmpty(Data) or VarIsNull(Data) then
    Result := 'null'
  else begin
		cData := Data;
    if Trim(cData) = '' then
			Result := 'null'
		else begin
			DataType := VarType(Data);
			if (DataType = varString) {$IFDEF CC_D2K9} or (DataType = varUString) {$ENDIF} then
				Result := QuotedStr(Data)
			else if (DataType = varDate) then
				Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', TDateTime(Data)) + ''''
			else
				Result := Data;
		end;
	end;
end;

function TCcSQLiteAdaptor.StringToDate(cDate: String): TDateTime;
begin
  Result := EncodeDate(StrToInt(Copy(cDate, 1, 4)), StrToInt(Copy(cDate, 6, 2)), StrToInt(Copy(cDate, 9, 2)))
    + EncodeTime(StrToIntDef(Copy(cDate, 12, 2), 0), StrToIntDef(Copy(cDate, 15, 2), 0), StrToIntDef(Copy(cDate, 18, 2), 0), StrToIntDef(Copy(cDate, 21, 4), 0));
end;

function TCcSQLiteAdaptor.SupportsGenerators: Boolean;
begin
  Result := False;
end;

procedure TCcSQLiteAdaptor.DoListPrimaryKeys(cTableName: String;
  list: TStringList);
begin
		with FConnection.SelectQuery['SQLiteAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'pragma table_info(' + cTableName + ')';
		Exec;
		while not Eof do begin
			if (Field['pk'].AsInteger = 1) then
				list.Add(Trim(Field['name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLiteAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
begin
		with FConnection.SelectQuery['SQLiteAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'pragma table_info(' + cTableName + ')';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLiteAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
	with FConnection.SelectQuery['SQLiteAdaptor.DoListTables'] do begin
		Close;
		SQL.Text := 'select name as table_name from sqlite_master where type = ''table''';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['table_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLiteAdaptor.DoListTriggers(list: TStringList);
begin
		with FConnection.SelectQuery['SQLiteAdaptor.DoListTriggers'] do begin
		Close;
		SQL.Text := 'select name as trigger_name from sqlite_master where type = ''trigger''';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['trigger_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcSQLiteAdaptor.DoListUpdatableTableFields(cTableName: String;
  list: TStringList);
begin
	DoListTableFields(cTableName, list);
end;

end.
