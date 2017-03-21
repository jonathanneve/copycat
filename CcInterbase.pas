unit CcInterbase;

{$I CC.INC}

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys;

type

TCcInterbaseBranch = (dbInterbase, dbFirebird);

//Database adaptor for Interbase and Firebird
//See also:
//TCcDBAdaptor
TCcInterbaseAdaptor = class (TCcDBAdaptor)
  private
    FDBBranch: TCcInterbaseBranch;
    FBranchVersion: Integer;
    procedure EmptyTempTable;
    procedure InsertTempTable;
    function GetDeclaration(DataType: TFieldType; nSize: Integer): String;
    function TranslateDataType(LowLevelDataType: Integer): TFieldType;
    function ConvertToOldGen(cPKGen: String; cNewOld: String): String;
    function QuoteSQLData(cData: String; DataType: TFieldType;
      lSQLStyle: Boolean): String;
    function GetSQLDialect: Integer;
    function StringToDate(cDate: String; lHasTimeInfo, lHasDateInfo: Boolean): TDateTime;
    procedure GetReplicatingNode;
    function ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
    procedure GetUniqueIndices(cTableName: String; list: TStringList);
    function GetMillisecFractions(d: TDateTime): String;
    function GetFieldTypeSQLText(FieldName, TableName: String): String;override;
    function QuoteSQLDataLite(cData: String; DataType: TFieldType;
      lSQLStyle: Boolean): String;
  protected
    FKeys : TCcKeyRing;
    procedure DoRegisterNode(NodeName: String);override;
		function GetQuoteMetadata: Boolean;override;
    function CheckConnectionLossException(E: Exception): Boolean; override;

    function GetInsertOrUpdateSQL(slFields: TStringList; sourceDBAdaptor: TCcDBAdaptor; keys: TCcCustomKeyRing; tableName: String): string;override;
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
    function UDFDeclared(UDFName: String): Boolean;
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
    procedure DoListKeywordsForbiddenAsFieldNames(list: TStringList);override;

  	function IndexDeclared(IndexName: String): Boolean;
    function SupportsInsertOrUpdate: Boolean;override;
  public
    procedure DropGenerator(cGeneratorName: string); override;
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

//    procedure GetProcParams(ProcName: String;
//      Params: TDataSet; InputParam: Boolean);override;

    function GetGeneratorValue(GenName: String; Increment: Integer):
  {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}override;
    function GetGenerator(GenName: String; Increment: Integer): String;override;

    property SQLDialect: Integer read GetSQLDialect;

    property DBBranch: TCcInterbaseBranch read FDBBranch;
    property BranchVersion: Integer read FBranchVersion;

    constructor Create(Conn: TCcConnection);override;
    destructor Destroy;override;
end;

const
  IB_SQL_VARYING                    =        448;
  IB_SQL_TEXT                       =        452;
  IB_SQL_DOUBLE                     =        480;
  IB_SQL_FLOAT                      =        482;
  IB_SQL_LONG                       =        496;
  IB_SQL_SHORT                      =        500;
  IB_SQL_TIMESTAMP                  =        510;
  IB_SQL_BLOB                       =        520;
  IB_SQL_D_FLOAT                    =        530;
  IB_SQL_ARRAY                      =        540;
  IB_SQL_QUAD                       =        550;
  IB_SQL_TYPE_TIME                  =        560;
  IB_SQL_TYPE_DATE                  =        570;
  IB_SQL_INT64                      =        580;
  IB_SQL_DATE                       =        IB_SQL_TIMESTAMP;
  IB_SQL_BOOLEAN                    =        590;

implementation

uses {$IFDEF CC_UseVariants}Variants, FMTBcd, {$ENDIF}Math {$IFDEF CC_D6}, Strutils{$ENDIF};

procedure TCcInterbaseAdaptor.SetVersion(const Value: String);
var
  cPrefix, cSuffix: String;
  OldDecimalSeparator: Char;
begin
  inherited;
  if (Trim(Value) <> '') then begin
    cPrefix := Copy(Value, 1, 2);
    cSuffix := Copy(Value, 3, Length(Value));

    if cPrefix = 'IB' then
      FDBBranch := dbInterbase
    else
      FDBBranch := dbFirebird;

    {$IFDEF CC_D2K13}
    OldDecimalSeparator := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    {$ELSE}
    OldDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    {$ENDIF}
    try
      FBranchVersion := Round(StrToFloat(Trim(cSuffix)) * 10);
    finally
      {$IFDEF CC_D2K13}
      FormatSettings.DecimalSeparator := OldDecimalSeparator;
      {$ELSE}
      DecimalSeparator := OldDecimalSeparator;
      {$ENDIF}
    end;
  end;
end;

function TCcInterbaseAdaptor.StringToDate(cDate: String; lHasTimeInfo, lHasDateInfo: Boolean): TDateTime;
var
  cDateFormat: String;
  nMilliSec: Integer;
  nDashPos: Integer;
  nDotPos: Integer;

  function GetMonth(cMonth: String): String;
  begin
    //This function converts a three letter English abbreviation of the month
    //name to its corresponding month number
    cMonth := UpperCase(Trim(cMonth));
    if cMonth = 'JAN' then
      Result := '01'
    else if cMonth = 'FEB' then
      Result := '02'
    else if cMonth = 'MAR' then
      Result := '03'
    else if cMonth = 'APR' then
      Result := '04'
    else if cMonth = 'MAY' then
      Result := '05'
    else if cMonth = 'JUN' then
      Result := '06'
    else if cMonth = 'JUL' then
      Result := '07'
    else if cMonth = 'AUG' then
      Result := '08'
    else if cMonth = 'SEP' then
      Result := '09'
    else if cMonth = 'OCT' then
      Result := '10'
    else if cMonth = 'NOV' then
      Result := '11'
    else if cMonth = 'DEC' then
      Result := '12';
  end;
begin
  if Trim(cDate) = '' then begin
    Result := 0;
    Exit;
  end;

  if lHasDateInfo then begin
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
  end else begin
    if Length(cDate) > 8 then begin
      //This indicates that our time includes milliseconds
      nMilliSec := StrToIntDef(Copy(cDate, Length(cDate) - 3, 4), 0);
      cDate := Copy(cDate, 1, Length(cDate) - 5);
    end
    else
      nMilliSec := 0;

    {$IFDEF CC_D2K13}
    cDateFormat := FormatSettings.ShortDateFormat;
    {$ELSE}
    cDateFormat := ShortDateFormat;
    {$ENDIF}
  end;

  try
    if SQLDialect = 3 then
      //Dialect 3 format: 'YYYY.MM.DD HH:NN:SS.ssss'
      {$IFDEF CC_D2K13}
      FormatSettings.ShortDateFormat := 'yyyy/mm/dd'
      {$ELSE}
      ShortDateFormat := 'yyyy/mm/dd'
      {$ENDIF}
    else begin
      //Dialect 1 format: 'D-MMM-YYYY HH:NN:SS.ssss'
      //converted (above) to: 'D.MMM.YYYY HH:NN:SS.ssss'
      nDotPos := AnsiPos('.', cDate);
      cDate := Copy(cDate, 1, nDotPos) + GetMonth(Copy(cDate, nDotPos+1, 3)) + Copy(cDate, nDotPos + 4, Length(cDate));
      {$IFDEF CC_D2K13}
      FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
      {$ELSE}
      ShortDateFormat := 'dd/mm/yyyy';
      {$ENDIF}
    end;

    //Perform the actual conversion
    Result := StrToDateTime(cDate) + EncodeTime(0, 0, 0, nMilliSec div 10);
  finally
    //Restore the locale setting
    {$IFDEF CC_D2K13}
    FormatSettings.ShortDateFormat := cDateFormat;
    {$ELSE}
    ShortDateFormat := cDateFormat;
    {$ENDIF}
  end;
end;

function TCcInterbaseAdaptor.ConvertValue(Val: Variant; DataType: TFieldType): Variant;
var
  vt: Word;
  cOrigDecimalSeparator: Char;
begin
  {$IFDEF CC_D2K13}
  cOrigDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  {$ELSE}
  cOrigDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  {$ENDIF}
  try
    vt := VarType(Val);
    if  {$IFDEF CC_D2K9} (vt = varUString) or {$ENDIF} (vt = varOleStr) or (vt = varString) then begin
      if ((DataType = ftDateTime) or (DataType = ftDate) or (DataType = ftTime) {$IFDEF CC_D2K12} or (DataType = ftTimeStamp)
        or (DataType = ftOraTimeStamp){$ENDIF}) then
        Result := StringToDate(Val, (DataType <> ftDate), (DataType <> ftTime))
      else if ((DataType = ftInteger) or (DataType = ftSmallInt)) then
        Result := StrToInt(Val)
      else if ((DataType = ftFloat) {$IFDEF CC_D2K12} or (DataType = ftSingle){$ENDIF} ) then
        Result := StrToFloat(Val)
      else if ((DataType = ftBcd) {$IFDEF CC_D2K12} or (DataType = ftFMTBcd){$ENDIF} ) then
        Result := StrToCurr(Val)
      else begin
        Result := Val
      end;
    end
    else
      Result := Val;
  finally
    //Restore the locale setting
    {$IFDEF CC_D2K13}
    FormatSettings.DecimalSeparator := cOrigDecimalSeparator;
    {$ELSE}
    DecimalSeparator := cOrigDecimalSeparator;
    {$ENDIF}
  end;
end;

//This function is called in order to see if a certain predefined UDF is available on the
//database server.
function TCcInterbaseAdaptor.UDFDeclared(UDFName: String):Boolean;
begin
  with FConnection.SelectQuery['IB_FINDUDF'] do begin
    Close;
    SQL.Text := 'select rdb$function_name from rdb$functions where upper(rdb$function_name) = ' + QuotedStr(UDFName);
    Exec;
    if RecordCount > 0 then Result := True
    else Result := False;
  end;
end;

////GrantProcedure is called in order to grant access to the given database procedure.
////If cUserName = 'PUBLIC', access should be granted to all users
//function TCcInterbaseAdaptor.GetGrantProcedure(cProcName, cUserName:String):String;
//begin
//  Result := 'GRANT EXECUTE ON PROCEDURE ' + cProcName + ' TO ' + cUserName;
//end;

////GetGrantTable is called in order to grant access to the given database table.
////If cUserName = 'PUBLIC', access should be granted to all users
//function TCcInterbaseAdaptor.GetGrantTable(cTableName, cUserName:String): String;
//begin
//  Result := 'GRANT ALL ON ' + cTableName + ' TO ' + cUserName;
//end;

function TCcInterbaseAdaptor.MaxDDLNameLength: Integer;
begin
  Result := 31;
end;

destructor TCcInterbaseAdaptor.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCcInterbaseAdaptor.DeclareField(FieldName: String; FieldType: TFieldType;
  Length: Integer; NotNull, PK: Boolean; AutoInc: Boolean): String;
begin
	Result := FieldName + ' ' + GetDeclaration(FieldType, Length);

  if NotNull then
    Result := Result + ' NOT NULL';
  if PK then
    Result := Result + ' PRIMARY KEY';
end;

procedure TCcInterbaseAdaptor.DeclareGenerator(GenName:String);
begin
  Query.Clear;
  Query.Add('CREATE GENERATOR ' + MetaQuote(GenName));
  ExecConfQuery;
end;

//This function is called in order to see if a certain generator exists in the database
//If database does not handle or need generators (for example, if auto-increment fields are used instead)
//then simply return true.
function TCcInterbaseAdaptor.GenDeclared(GenName: String): Boolean;
begin
	with FConnection.SelectQuery['IB_FINDGEN'] do begin
		Close;
    SQL.Text := 'select cast(rdb$generator_name as varchar(50)) from rdb$generators where %upper_case(cast(rdb$generator_name as varchar(50))) = %upper_case(:gen_name)';
    if QuoteMetadata then
      Macro['upper_case'].Value := ''
    else
      Macro['upper_case'].Value := 'upper';
    Param['gen_name'].AsString := GenName;
    Exec;
    if RecordCount > 0 then Result := True
    else Result := False;
  end;
end;

procedure TCcInterbaseAdaptor.GetReplicatingNode;
begin
  if (DBBranch = dbInterbase) or (BranchVersion < 20) then
    Query.Add('  select replicating_node from rpl$vars into :replicating_node;')
  else
		Query.Add('  replicating_node = rdb$get_context(''USER_SESSION'', ''REPLICATING_NODE'');');
end;

function TCcInterbaseAdaptor.ParseSQLCondition(cTable, cCondition: String; cNewOld: String): String;
begin
	Result := ConvertToOldGen(Trim(cCondition), cNewOld);
  Result := ReplaceString(Result, '%%TABLE_NAME', Trim(cTable));
end;

function TCcInterbaseAdaptor.GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean; TrackFieldChanges: Boolean): Integer;
var
  nTriggerNumber: Integer;
  cTriggerName: String;
	TableName, ConfigName: String;

  procedure DeclareVariables(cType: String);
  begin
    Query.Add('AS');
    Query.Add('declare variable primary_key_values varchar(500);');
    Query.Add('declare variable QuotedValue varchar(200);');
    Query.Add('declare variable user_login varchar(50);');
    Query.Add('declare variable repl_operation char(1);');
    Query.Add('declare variable replicating_node varchar(50);');
    Query.Add('declare variable primary_key_sync varchar(5000);');
    Query.Add('declare variable unique_key_sync varchar(5000);');
    Query.Add('declare variable operation_type char(1);');
    if cType = 'U' then
      Query.Add('declare variable old_primary_key_values varchar(500);');
		Query.Add('begin');
	end;

  function CheckFieldRestrictions: Boolean;
	var
		I: Integer;
		slIncludedFields: TStringList;
	begin
		Result := False;
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
		end;
	end;

  function GetPKVals(cNewOld: String): String;
	var
		I: Integer;
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
		sl.Add('  primary_key_values = '''';');
      for I := 0 to FKeys.Count - 1 do begin
        if (FKeys[i].PrimaryKey) then begin
          sl.Add('  if (' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ' is null) then QuotedValue = ''"'';');
          sl.Add('  else select quoted_str from RPL$quote_str(' + cNewOld + '.' + MetaQuote(FKeys[i].KeyName) + ') into :QuotedValue;');
          sl.Add('  primary_key_values = primary_key_values || QuotedValue || '';'';');
        end;
    end;
    Result := sl.Text;
    finally
      sl.free;
    end;
  end;

  function GetKeySyncVals: String;
  var
		cSync: String;
    I: Integer;
    lSync: Boolean;
    sl2: TStringList;
  begin
    lSync := False;
    sl2 := TStringList.Create;
    try
      sl2.Add('  primary_key_sync = '''';');
      sl2.Add('  unique_key_sync = '''';');
      for I := 0 to FKeys.Count - 1 do begin
        cSync := FKeys[i].GenericSyncStatement;
        if (Trim(cSync) = '') then
          cSync := QuotedStr(cSync)
        else
          lSync := True;
        sl2.Add('    select quoted_str from RPL$quote_str(' + cSync + ') into :QuotedValue;');
        if (FKeys[i].PrimaryKey) then
          sl2.Add('    primary_key_sync = primary_key_sync || QuotedValue || '';'';')
        else
          sl2.Add('    unique_key_sync = unique_key_sync || QuotedValue || '';'';');
      end;
      if lSync then
        Result := sl2.Text;
    finally
      sl2.Free;
    end;

  end;

  procedure GenerateChangedFieldsOld;
  var
    TriggerBody: TStringList;
    slFields: TStringList;
    I: Integer;
    size: Integer;
    ft: TFieldType;
    q: TCcQuery;
    cFieldVals, cFieldName : String;
    sl: TStringList;
      lFirst: Boolean;
      cMainTriggerText: String;
      cCondition: string;
      nTriggerLength: Integer;
      nFinalTriggerLength: Integer;

    procedure CreateTrigger(lFinal: Boolean);
      var
        nPos: Integer;
        cFullTrigName :String;
    begin
      if nTriggerNumber = 1 then
        nPos := 32765
      else if lFinal then
        nPos := 32767
      else
        nPos := 32766;

      if nTriggerNumber = 1 then
        cFullTrigName := MetaQuote(cTriggerName)
      else
        cFullTrigName := MetaQuote(cTriggerName + IntToStr(nTriggerNumber));
      Query.Add('create trigger ' + cFullTrigName + ' for ' + MetaQuote(TableName) + ' AFTER INSERT OR UPDATE OR DELETE position ' + IntToStr(nPos));
      Query.Add('as');
      if lFinal then begin
        Query.Add('declare variable primary_key_values varchar(500);');
        Query.Add('declare variable QuotedValue varchar(200);');
        Query.Add('declare variable operation_type char(1);');
        Query.Add('declare variable primary_key_sync varchar(5000);');
        Query.Add('declare variable unique_key_sync varchar(5000);');
      end;

      Query.Add('declare variable user_login varchar(50);');
      Query.Add('declare variable replicating_node varchar(50);');
      Query.Add('declare variable change_number integer;');
      Query.Add('begin');
      Query.Add('  if (rdb$get_context(''USER_TRANSACTION'', ''RPL$NO_REPLICATION'') = ''TRUE'') then exit;');

      if nTriggerNumber = 1 then begin
         Query.Add('  change_number = gen_id(gen_rpl$log_change_number, 1);');
         Query.Add('  rdb$set_context(''USER_TRANSACTION'', ''RPL$CHANGE_NUMBER'', change_number);');
      end
      else
        Query.Add('  change_number = rdb$get_context(''USER_TRANSACTION'', ''RPL$CHANGE_NUMBER'');');

      GetReplicatingNode;

      if lFinal then begin
        Query.Add('if (inserting) then begin');
        Query.Add(GetPKVals('new'));
        Query.Add('end else begin');
        Query.Add(GetPKVals('old'));
        Query.Add('end');

        if GetKeySyncVals <> '' then begin
          //Generate the synchronization statements for primary and unique keys
          //We don't convert "new." to "old." in the GenericSyncStatement, which means we always take the new value of the fields, if applicable
          //This doesn't matter because the synchronization statements are only used for inserting new key values, not for old records
          //For this reason, the following code is also excluded from the delete triggers
          Query.Add('if (not deleting) then begin');
          Query.Add(GetKeySyncVals);
          Query.Add('end');
        end;

        Query.Add('if (inserting) then operation_type = ''I'';');
        Query.Add('else if (updating) then operation_type = ''U'';');
        Query.Add('else operation_type = ''D'';');
      end;

      Query.Add('  for select u.login');
      Query.Add('    from RPL$users u');
      Query.Add('    where (u.login <> :replicating_node or :replicating_node is null)');
      Query.Add('    and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

      //User-definable SQL condition
      if cCondition <> '' then
        Query.Add('    and ((' + ParseSQLCondition(TableName, cCondition, 'old') + ') or (' + ParseSQLCondition(TableName, cCondition, 'new') + '))');

      Query.Add('    into :user_login do');
      Query.Add('  begin');

      Query.AddStrings(TriggerBody);

      if lFinal then begin
        Query.Add('    if ((rdb$get_context(''USER_TRANSACTION'', ''FORCE_REPLICATION'') = ''TRUE'') or (rdb$get_context(''USER_TRANSACTION'', ''RPL$FIELDS_CHANGED'') = 1)) then begin');
        Query.Add('      insert into RPL$log (code, change_number, login, operation_date, table_name, sent_from,');
        Query.Add('        primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, operation_type, transaction_number)');
        Query.Add('      values (gen_id(gen_rpl$log, 1), :change_number, :user_login, current_timestamp, ' + QuotedStr(TableName) + ', :replicating_node,');
        Query.Add('        :primary_key_values, null, :PRIMARY_KEY_SYNC, :UNIQUE_KEY_SYNC, :operation_type, current_transaction);');
        Query.Add('      end');
      end;
      Query.Add('  end');
      if lFinal then
        Query.Add('  rdb$set_context(''USER_TRANSACTION'', ''RPL$FIELDS_CHANGED'', 0);');
      Query.Add('end');
      ExecConfQuery;
      TriggerBody.Clear;
      Inc(nTriggerNumber);
    end;

  begin
    cCondition := Trim(qTableConf.Field['CONDITION'].AsString);
    nTriggerLength := 700 + Length(cCondition);
    nFinalTriggerLength := 1300 + Length(cCondition) + 2 * Length(GetPKVals('new')) + Length(GetKeySyncVals);

    lFirst := True;
    q := TCcQuery.Create(FConnection);
    q.Connection := FConnection;
    q.SelectStatement := True;
    sl := TStringList.Create;
    TriggerBody := TStringList.Create;
    try
			q.SQL.Text := 'select rf.rdb$field_name, rfs.rdb$field_type, rfs.rdb$field_length '+
                    ' from rdb$relation_fields rf '+
                    ' join rdb$fields rfs on rfs.rdb$field_name = rf.rdb$field_source ' +
                    ' where rfs.rdb$computed_blr is null and rf.rdb$relation_name = :table_name order by rf.rdb$field_name';
      q.Param['table_name'].Value := TableName;
      q.Exec;
      while not q.Eof do begin
        cFieldName := q.Field['rdb$field_name'].AsString;
        sl.Add('if (old.' + cFieldName + ' is distinct from new.' + cFieldName + ') then begin');
        sl.Add('  insert into RPL$LOG_VALUES (CHANGE_NUMBER, NODE_NAME, OLD_VALUE, OLD_VALUE_BLOB,new_VALUE, new_VALUE_BLOB,FIELD_NAME,FIELD_TYPE)');

        ft :=  TranslateDataType(q.Field['rdb$field_type'].AsInteger);
        size := q.Field['rdb$field_length'].AsInteger;
        if (ft = ftBlob) or (size > 200) then
          cFieldVals := 'null, old.' + cFieldName + ', null, new.' + cFieldName
        else
          cFieldVals := 'old.' + cFieldName + ', null, new.' + cFieldName + ', null';
        sl.Add('    values (:change_number, :user_login, ' + cFieldVals + ', ' + QuotedStr(cFieldName) + ', ' + IntToStr(Integer(ft)) + ');');
        sl.Add('    rdb$set_context(''USER_TRANSACTION'', ''RPL$FIELDS_CHANGED'', 1);');
        sl.Add('end');

        //Check if trigger is too big...
        if nTriggerLength + Length(TriggerBody.Text) + Length(sl.Text) > 30000 then
          CreateTrigger(False);

        TriggerBody.Append(sl.Text);
        sl.Clear;
        q.Next;
      end;
      if nFinalTriggerLength + Length(TriggerBody.Text) > 30000 then
        CreateTrigger(False)
      else
        CreateTrigger(True);
      nTriggerNumber := nTriggerNumber - 1;
    finally
      q.Free;
      TriggerBody.Free;
      sl.Free;
    end;
  end;

  procedure GenerateChangedFields;
  var
    cCondition: string;

    procedure DeclareVariables;
    begin
      Query.Add('declare variable field_name varchar(50);');
      Query.Add('declare variable field_length integer;');
      Query.Add('declare variable field_type integer;');
      Query.Add('declare variable stmt varchar(5000);');
      Query.Add('declare variable current_stmt varchar(500);');
      Query.Add('declare variable val_blob blob;');
      Query.Add('declare variable val varchar(250);');
      Query.Add('declare variable counter integer;');
      Query.Add('declare variable dbkey char(8) character set octets;');
      Query.Add('declare variable change_number integer;');
    end;

    procedure GetFieldValues(cNewOld:String);

      function GetStmtWhere: String;
      var
        I: Integer;
        cKeyName: string;
      begin
        Result := '';
        if (DBBranch = dbFirebird) and (BranchVersion >= 25) then
          Result := 'rdb$db_key='''''' || :dbkey || '''''''''
        else begin
          for I := 0 to FKeys.Count-1 do begin
            if Result <> '' then
              Result := Result + ' || '' and '' || ';
            cKeyName := MetaQuote(FKeys[i].KeyName);
            Result := Result + cKeyName + '='' || ' + QuoteSQLDataLite(cNewOld + '.' + cKeyName, FKeys[i].DataType, true);
          end;
        end;
      end;

    begin
      Query.Add('  counter = 0;');
      Query.Add('  for select trim(rf.rdb$field_name), coalesce(f.rdb$character_length, f.rdb$field_length), case f.rdb$field_type');
      Query.Add('    when 7 then 3 when 8 then 3 when 9 then 3');
      Query.Add('    when 10 then 6 when 27 then 6 when 12 then 9');
      Query.Add('    when 13 then 10');
      Query.Add('    when 35 then 11');
      Query.Add('    when 14 then iif(rdb$character_set_id = 4, 38, 23)');
      Query.Add('    when 37 then iif(rdb$character_set_id = 4, 24, 1)');
      Query.Add('    when 40 then iif(rdb$character_set_id = 4, 24, 1)');
      Query.Add('    when 261 then iif(rdb$field_sub_type = 0, 15, iif(rdb$character_set_id = 4, 39, 16))');
      Query.Add('    when 16 then 8');
      Query.Add('    when 23 then 5');
      Query.Add('  end');
      Query.Add('  from rdb$relation_fields rf');
      Query.Add('  join rdb$fields f on f.rdb$field_name = rf.rdb$field_source');
      Query.Add('  join rpl$tables_config t on t.table_name = rf.rdb$relation_name and t.config_name = ' + QuotedStr(ConfigName));
      Query.Add('  where f.rdb$computed_blr is null and rf.rdb$relation_name = ' + QuotedStr(TableName));
      Query.Add('  and ((coalesce(t.included_fields, '''') = '''') or rf.rdb$field_name in (select str from rpl$split_list(t.included_fields)))');
      Query.Add('  and ((coalesce(t.excluded_fields, '''') = '''') or rf.rdb$field_name not in (select str from rpl$split_list(t.excluded_fields)))');
      Query.Add('  into :field_name, :field_length, :field_type do');
      Query.Add('  begin');
      Query.Add('      if (field_length <= 250 and field_type not in (15, 16, 39)) then');
      Query.Add('        current_stmt = ''select ''''''|| :field_name || '''''',cast('' || :field_name || '' as varchar(250)), cast(null as blob),'' || :field_type || '' from ' + MetaQuote(TableName) + ' where ' + GetStmtWhere + ';');
      Query.Add('      else');
      Query.Add('        current_stmt = ''select ''''''|| :field_name || '''''',cast(null as varchar(250)),cast('' || :field_name || '' as blob),'' || :field_type || '' from ' + MetaQuote(TableName) + ' where ' + GetStmtWhere + ';');
      Query.Add('');
      Query.Add('      if (stmt is null) then');
      Query.Add('        stmt = current_stmt;');
      Query.Add('      else');
      Query.Add('        stmt = stmt || '' union all '' || current_stmt;');
      Query.Add('');
      Query.Add('      if (character_length(stmt) >= 4000 or octet_length(stmt) >= 10000 or counter >= 100) then begin');
      Query.Add('        for execute statement (stmt) into :field_name, :val, :val_blob, :field_type do  begin');
      Query.Add('          if (val is not null or val_blob is not null) then');
      Query.Add('            update or insert into rpl$tmp_values(field_name,field_type,'+ cNewOld + '_value,'+ cNewOld + '_blob, '+ cNewOld + '_blob_null, change_number) values (trim(:field_name), :field_type, :val, :val_blob, iif(:val_blob is null, ''Y'', ''N''), :change_number);');
      Query.Add('        end');
      Query.Add('        stmt = null;');
      Query.Add('        counter = 0;');
      Query.Add('      end');
      Query.Add('     counter = counter + 1;');
      Query.Add('  end');
      Query.Add('  if (stmt is not null) then begin');
      Query.Add('    for execute statement (stmt) into :field_name, :val, :val_blob, :field_type do  begin');
      Query.Add('      if (val is not null or val_blob is not null) then');
      Query.Add('        update or insert into rpl$tmp_values(field_name,field_type,'+ cNewOld + '_value,'+ cNewOld + '_blob, '+ cNewOld + '_blob_null, change_number) values (trim(:field_name), :field_type, :val, :val_blob, iif(:val_blob is null, ''Y'', ''N''), :change_number);');
      Query.Add('    end');
      Query.Add('  end');
    end;

  begin
    cCondition := Trim(qTableConf.Field['CONDITION'].AsString);

    Query.Add('create trigger ' + cTriggerName + ' for ' + MetaQuote(TableName) + ' BEFORE INSERT OR UPDATE OR DELETE');
    Query.Add('as');
    DeclareVariables;
    Query.Add('begin');
    Query.Add('  if (rdb$get_context(''USER_TRANSACTION'', ''RPL$NO_REPLICATION'') = ''TRUE'') then exit;');
    Query.Add('  change_number = gen_id(gen_rpl$log_change_number, 1);');
    Query.Add('  insert into rpl$tmp_changes(change_number) values (:change_number);');
    Query.Add('  if (inserting) then exit;');
    Query.Add('  if (deleting) then');
    Query.Add('    dbkey = old.rdb$db_key;');
    Query.Add('  else');
    Query.Add('    dbkey = new.rdb$db_key;');
    GetFieldValues('old');
    Query.Add('end');
    ExecConfQuery;

    Query.Add('create trigger ' + cTriggerName + '2 for ' + MetaQuote(TableName) + ' AFTER INSERT OR UPDATE OR DELETE');
    Query.Add('as');
    DeclareVariables;

    Query.Add('declare variable primary_key_sync varchar(5000);');
    Query.Add('declare variable unique_key_sync varchar(5000);');
    Query.Add('declare variable user_login varchar(50);');
    Query.Add('declare variable replicating_node varchar(50);');
    Query.Add('declare variable primary_key_values varchar(500);');
    Query.Add('declare variable operation_type char(1);');
    Query.Add('begin');
    Query.Add('  if (rdb$get_context(''USER_TRANSACTION'', ''RPL$NO_REPLICATION'') = ''TRUE'') then exit;');
    GetReplicatingNode;
    Query.Add('  dbkey = coalesce(new.rdb$db_key, old.rdb$db_key);');
    Query.Add('  select max(change_number) from rpl$tmp_changes into :change_number;');
    Query.Add('  if (not deleting) then begin');
    if GetKeySyncVals <> '' then begin
      //Generate the synchronization statements for primary and unique keys
      //We don't convert "new." to "old." in the GenericSyncStatement, which means we always take the new value of the fields, if applicable
      //This doesn't matter because the synchronization statements are only used for inserting new key values, not for old records
      //For this reason, the following code is also excluded from the delete triggers
      Query.Add(GetKeySyncVals);
    end;
    GetFieldValues('new');
    Query.Add('  end');

    Query.Add('  select list((select quoted_str from RPL$QUOTE_STR(val)), '';'') || '';''');
    Query.Add('  from (select coalesce(r.old_value, r.new_value) as val');
    Query.Add('  from rdb$relation_constraints rel');
    Query.Add('  join rdb$index_segments i on rel.rdb$index_name = i.rdb$index_name');
    Query.Add('  join rpl$tmp_values r on r.field_name = i.rdb$field_name and r.change_number = :change_number');
    Query.Add('  where rel.rdb$constraint_type = ''PRIMARY KEY''');
    Query.Add('  and rel.rdb$relation_name = ' + QuotedStr(TableName));
    Query.Add('  order by i.rdb$field_position) into :primary_key_values;');
    Query.Add('  if (primary_key_values is null) then');
    Query.Add('    select list(coalesce((select quoted_str from RPL$QUOTE_STR(val)), ''"''), '';'') || '';''');
    Query.Add('      from (select coalesce(r.old_value, r.new_value) as val');
    Query.Add('        from rdb$index_segments ins');
    Query.Add('        join rpl$tmp_values r on r.field_name = ins.rdb$field_name and r.change_number = :change_number');
    Query.Add('        where ins.rdb$index_name = (select first 1 i.rdb$index_name as index_name');
    Query.Add('          from rdb$indices i');
    Query.Add('          where i.rdb$relation_name = ' + QuotedStr(TableName));
    Query.Add('          and i.rdb$unique_flag = 1)');
    Query.Add('        order by ins.rdb$field_position');
    Query.Add('    ) into :primary_key_values;');
    Query.Add('  if (primary_key_values is null) then');
    Query.Add('    select list(coalesce((select quoted_str from RPL$QUOTE_STR(val)), ''"''), '';'') || '';''');
    Query.Add('      from (select coalesce(r.old_value, r.new_value) as val');
    Query.Add('          from rdb$relation_fields rf');
    Query.Add('          join rdb$fields f on f.rdb$field_name = rf.rdb$field_source');
    Query.Add('          join rpl$tmp_values r on r.field_name = rf.rdb$field_name and r.change_number = :change_number');
    Query.Add('          where rf.rdb$relation_name = ' + QuotedStr(TableName));
    Query.Add('          and f.rdb$field_type <> 261 and f.rdb$field_length < 50');
    Query.Add('          order by rf.rdb$field_name');
    Query.Add('    ) into :primary_key_values;');

    Query.Add('  if (inserting) then operation_type = ''I'';');
    Query.Add('  else if (updating) then operation_type = ''U'';');
    Query.Add('  else operation_type = ''D'';');

    Query.Add('  if (exists(select field_name from rpl$tmp_values where change_number = :change_number and (old_value is distinct from new_value or old_blob is distinct from new_blob)) or (rdb$get_context(''USER_TRANSACTION'', ''FORCE_REPLICATION'') = ''TRUE'')) then begin');
    Query.Add('    for select u.login');
    Query.Add('    from RPL$users u');
    Query.Add('    where (u.login <> :replicating_node or :replicating_node is null)');
    Query.Add('    and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

    //User-definable SQL condition
    if cCondition <> '' then
      Query.Add('    and ((' + ParseSQLCondition(TableName, cCondition, 'old') + ') or (' + ParseSQLCondition(TableName, cCondition, 'new') + '))');

    Query.Add('    into :user_login do');
    Query.Add('    begin');
    Query.Add('      insert into RPL$log (code, change_number, login, operation_date, table_name, sent_from,');
    Query.Add('        primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, operation_type, transaction_number)');
    Query.Add('      values (gen_id(gen_rpl$log, 1), :change_number, :user_login, current_timestamp, ' + QuotedStr(TableName) + ', :replicating_node,');
    Query.Add('        :primary_key_values, null, :PRIMARY_KEY_SYNC, :UNIQUE_KEY_SYNC, :operation_type, current_transaction);');
    Query.Add('      insert into rpl$log_values (CHANGE_NUMBER, NODE_NAME, OLD_VALUE, OLD_VALUE_BLOB,new_VALUE, new_VALUE_BLOB,FIELD_NAME,FIELD_TYPE, OLD_BLOB_NULL, NEW_BLOB_NULL)');
    Query.Add('        select :change_number, :user_login, v.old_value, v.old_blob, v.new_value, v.new_blob, v.field_name, v.field_type, v.OLD_BLOB_NULL, v.NEW_BLOB_NULL');
    Query.Add('          from rpl$tmp_values v');
    Query.Add('          where v.change_number = :change_number and ((v.old_value is distinct from v.new_value or v.old_blob is distinct from v.new_blob) or (rdb$get_context(''USER_TRANSACTION'', ''FORCE_REPLICATION'') = ''TRUE''));');
    Query.Add('    end');
    Query.Add('  end');
    Query.Add('  delete from rpl$tmp_values where change_number = :change_number;');
    Query.Add('  delete from rpl$tmp_changes where change_number = :change_number;');
    Query.Add('end');
    ExecConfQuery;

    nTriggerNumber := 2;
  end;


	procedure DoGenerateSQL(cNewOld: String; cOperationType: String);
	var
		I: Integer;
		lConditionField: Boolean;
		lCondition : Boolean;
	//	lCheckPKChanged : Boolean;
		cSync: String;

	begin
//		lCheckPKChanged := (cNewOld = 'new') and (cOperationType = 'update');
		lConditionField := (Trim(qTable.Field['CONDITION_FIELD'].AsString) <> '');
		lCondition := (Trim(qTable.Field['CONDITION'].AsString) <> '');

		Query.Add('  primary_key_values = '''';');
    Query.Add(GetPKVals(cNewOld));

    //Generate the synchronization statements for primary and unique keys
    //We don't convert "new." to "old." in the GenericSyncStatement, which means we always take the new value of the fields, if applicable
    //This doesn't matter because the synchronization statements are only used for inserting new key values, not for old records
    //For this reason, the following code is also excluded from the delete triggers
    if (cOperationType <> 'delete') then begin
      Query.Add(GetKeySyncVals);
    end;

    Query.Add('if (inserting) then operation_type = ''I'';');
    Query.Add('else if (updating) then operation_type = ''U'';');
    Query.Add('else operation_type = ''D'';');

//    if lCheckPKChanged then begin
//      Query.Add('  if ((primary_key_values <> old_primary_key_values)');
//      if lRefField then
//        Query.Add('    or (old.' + MetaQuote(qTable.Field['REF_FIELD'].AsString) + ' <> new.' + MetaQuote(qTable.Field['REF_FIELD'].AsString) + ')');
//			Query.Add('  ) then begin');
//    end;

		Query.Add('  for select u.login, t.repl_' + cOperationType + 's');
		Query.Add('    from RPL$users u');
		if (ConfigName <> '') then
			Query.Add('    join rpl$tables_config t on t.table_name = ' + QuotedStr(TableName) + ' and t.config_name = ' + QuotedStr(ConfigName))
		else
			Query.Add('    join rpl$tables t on t.table_name = ' + QuotedStr(TableName));

		Query.Add('    where (u.login <> :replicating_node or :replicating_node is null)');

		if (ConfigName <> '') then
			Query.Add('    and (u.config_name is null or u.config_name = ' + QuotedStr(ConfigName) + ')');

		//User-definable SQL condition, configurable per table in RPL$TABLES
		if lCondition then
			Query.Add('    and (' + ParseSQLCondition(TableName, qTable.Field['CONDITION'].AsString, cNewOld) + ')');

    if Assigned(qTableConf) then begin
      if Trim(qTableConf.Field['CONDITION'].AsString) <> '' then
        Query.Add('    and (' + ParseSQLCondition(TableName, qTableConf.Field['CONDITION'].AsString, cNewOld) + ')');

      if Trim(qTableConf.Field[cOperationType + '_CONDITION'].AsString) <> '' then
        Query.Add('    and (' + ParseSQLCondition(TableName, qTableConf.Field[cOperationType + '_CONDITION'].AsString, cNewOld) + ')');
    end;

		if lConditionField then
      //If the condition field is null, the record is replicated to any user
      //If RPL$USERS.Condition_value is null, the user gets all changes
      Query.Add('    and ((' + cNewOld + '.' + MetaQuote(Trim(qTable.Field['CONDITION_FIELD'].AsString)) + ' = u.condition_value) or (u.condition_value is null))');

    Query.Add('    into :user_login, :repl_operation do');
    Query.Add('  begin');
    Query.Add('    if (repl_operation = ''Y'') then');
    Query.Add('      insert into RPL$log (code, login, operation_date, table_name, sent_from,');
    Query.Add('        primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC, OPERATION_TYPE, transaction_number)');
    Query.Add('      values (gen_id(gen_rpl$log, 1), :user_login, current_timestamp, ' + QuotedStr(TableName) + ', :replicating_node,');
    Query.Add('        :primary_key_values, null, :PRIMARY_KEY_SYNC, :UNIQUE_KEY_SYNC, :OPERATION_TYPE, current_transaction);');
//    Query.Add('      execute procedure RPL$generate_log (' + QuotedStr(TableName) + ', :primary_key_values, null, null, :user_login, :primary_key_sync, :unique_key_sync);');
    Query.Add('  end');
//		if lCheckPKChanged then
//			Query.Add('  end');
	end;

var
	lFieldRestrictions : Boolean;
	
begin
  nTriggerNumber := 1;
	if not FConnection.InTransaction then
		FConnection.StartTransaction;

	TableName := Trim(qTable.Field['TABLE_NAME'].AsString);
  FKeys.FailIfNoPK := FailIfNoPK;
  FKeys.ClearTableKeys;
	if Assigned(qTableConf) then begin
		cTriggerName := Trim(qTableConf.Field['TRIG_BASE_NAME'].AsString);
		ConfigName := Trim(qTableConf.Field['CONFIG_NAME'].AsString);
    FKeys.LoadKeys(TableName, '', '', '', qTableConf.Field['PRIMARY_KEY_SYNC'].AsString,
      qTableConf.Field['UNIQUE_KEY_NAMES'].AsString, qTableConf.Field['UNIQUE_KEY_SYNC'].AsString, koDelphi);
	end
	else begin
		cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString);
    FKeys.LoadKeys(TableName, '', '', '', qTable.Field['PRIMARY_KEY_SYNC'].AsString,
      qTable.Field['UNIQUE_KEY_NAMES'].AsString, qTable.Field['UNIQUE_KEY_SYNC'].AsString, koDelphi);
  end;

  if TrackFieldChanges and (DBBranch = dbFirebird) and (BranchVersion >= 15) then begin
    GenerateChangedFields;
  end
  else begin
    Query.Add('create trigger ' + MetaQuote(cTriggerName + '_I') + ' for ' + MetaQuote(TableName) + ' AFTER INSERT position 32766');
    DeclareVariables('I');
    GetReplicatingNode;
    DoGenerateSQL('new', 'insert');
    Query.Add('end');
    ExecConfQuery;

    Query.Add('create trigger ' + MetaQuote(cTriggerName + '_U') + ' for ' + MetaQuote(TableName) + ' AFTER UPDATE position 32766');
    DeclareVariables('U');
    GetReplicatingNode;
    lFieldRestrictions := CheckFieldRestrictions;
    DoGenerateSQL('old', 'update');
    Query.Add('  old_primary_key_values = primary_key_values;');
    DoGenerateSQL('new', 'update');
    Query.Add('end');
    if (lFieldRestrictions) then
      Query.Add('end');
    ExecConfQuery;

    Query.Add('create trigger ' + MetaQuote(cTriggerName + '_D') + ' for ' + MetaQuote(TableName) + ' AFTER DELETE position 32766');
    DeclareVariables('D');
    GetReplicatingNode;
    DoGenerateSQL('old', 'delete');
    Query.Add('end');
    ExecConfQuery;
  end;
  Result := nTriggerNumber;
//  Query.Add('update rpl$tables set created = ''Y'' where table_name = ' + QuotedStr(TableName));
//  ExecConfQuery;
end;

procedure TCcInterbaseAdaptor.RemoveTriggers(qTable: TCcQuery);

	procedure RemoveTrigger(cType: String);
	var
		cTriggerName: String;
	begin
		cTriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString) + cType;
		if (TriggerExists(cTriggerName)) then begin
			Query.Add('DROP TRIGGER ' + MetaQuote(cTriggerName));
			ExecConfQuery;
		end;
	end;
var
  nTriggerNumber: Integer;
  i: Integer;
begin
  RemoveTrigger('');
  nTriggerNumber := qTable.Field['NUMBER_OF_TRIGGERS'].AsInteger;
  for i := 2 to nTriggerNumber do begin
    RemoveTrigger(IntToStr(i));
    Inc(nTriggerNumber);
  end;

  RemoveTrigger('_I');
  RemoveTrigger('_U');
  RemoveTrigger('_D');
end;

function TCcInterbaseAdaptor.IndexDeclared(IndexName: String): Boolean;
begin
  with FConnection.SelectQuery['IBAdaptor.IndexDeclared'] do begin
	Close;
	SQL.Text := 'select rdb$index_name from rdb$indices where upper(rdb$index_name) = upper(:name)';
	Param['name'].AsString := IndexName;
	Exec;
	Result := (RecordCount > 0);
  end;
end;

procedure TCcInterbaseAdaptor.CheckExtraCustomMetadata;
begin
  if not TriggerExists('TR_RPL$LOG') then begin
    Query.Add('CREATE OR ALTER TRIGGER TR_RPL$LOG FOR RPL$LOG');
    Query.Add('AFTER DELETE');
    Query.Add('as');
    Query.Add('begin');
    Query.Add('  delete from rpl$log_values where node_name = old.login and change_number = old.change_number;');
    Query.Add('end');
		ExecConfQuery;
		FConnection.CommitRetaining;
  end;

//	if (DBBranch = dbFirebird) and (BranchVersion >= 20) then begin
{		if not IndexDeclared('I_RPL$LOG') then begin
			Query.Add('CREATE INDEX I_RPL$LOG on RPL$LOG (LOGIN, TABLE_NAME)');
			ExecConfQuery;
//		end;
		FConnection.CommitRetaining;
	end;}
end;

procedure TCcInterbaseAdaptor.RemoveExtraCustomMetadata;
begin
  if IndexDeclared('I_RPL$LOG') then begin
    Query.Add('DROP INDEX I_RPL$LOG');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
end;

procedure TCcInterbaseAdaptor.RemoveCustomMetadata;
begin
  if ProcedureExists('RPL$QUOTE_STR') then begin
    Query.Add('DROP PROCEDURE RPL$QUOTE_STR');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
  if ProcedureExists('RPL$SPLIT_LIST') then begin
    Query.Add('DROP PROCEDURE RPL$SPLIT_LIST');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
  if ProcedureExists('RPL$FORCE_REPL') then begin
    Query.Add('DROP PROCEDURE RPL$FORCE_REPL');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
  if TableExists('RPL$TMP_VALUES') then begin
    Query.Add('DROP TABLE RPL$TMP_VALUES');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
end;

procedure TCcInterbaseAdaptor.DropGenerator(cGeneratorName: string);
begin
  if GenDeclared(cGeneratorName) then
  begin
    FConnection.ExecQuery('DROP GENERATOR ' + cGeneratorName);
    FConnection.CommitRetaining;
  end;
end;

procedure TCcInterbaseAdaptor.DropProcedures;
begin
  if ProcedureExists('RPL$GENERATE_LOG') then begin
    Query.Add('DROP PROCEDURE RPL$GENERATE_LOG');
    ExecConfQuery;
    FConnection.CommitRetaining;
  end;
end;

function TCcInterbaseAdaptor.CheckConnectionLossException(
  E: Exception): Boolean;
begin
{$IFDEF CC_D6}  if AnsiContainsText(e.Message, 'ISC ERROR CODE:335544721') then
    Result := True
  else
{$ENDIF}
    Result := False;
end;

procedure TCcInterbaseAdaptor.CheckCustomMetadata;
var
	FB2: Boolean;
begin
	inherited;
	FB2 := (DBBranch = dbFirebird) and (BranchVersion >= 20);
	if not FB2 then begin
		if not UDFDeclared('SUBSTR') then begin
      Query.Add('DECLARE EXTERNAL FUNCTION SUBSTR');
      Query.Add('CSTRING(1000), SMALLINT, SMALLINT');
      Query.Add('RETURNS CSTRING(1000) FREE_IT');
      Query.Add('ENTRY_POINT ''IB_UDF_substr''  MODULE_NAME ''ib_udf''');
      ExecConfQuery;

//      Query.Add('GRANT ALL ON SUBSTR TO PUBLIC');
//      ExecConfQuery;
    end;
		if not UDFDeclared('STRLEN') then begin
      Query.Add('DECLARE EXTERNAL FUNCTION STRLEN');
      Query.Add('CSTRING(32767)');
      Query.Add('RETURNS INTEGER BY VALUE');
      Query.Add('ENTRY_POINT ''IB_UDF_strlen''  MODULE_NAME ''ib_udf''');
      ExecConfQuery;

//      Query.Add('GRANT ALL ON STRLEN TO PUBLIC');
//      ExecConfQuery;
    end;
    FConnection.CommitRetaining;
	end;

{ else //In Firebird 2+ we needn't use any UDF, because the built-in SUBSTRING function works properly
  if not ProcedureExists('RPL$STRLEN') then with Query do begin
    Add('CREATE PROCEDURE RPL$strlen (str VARCHAR(100))');
    Add('  RETURNS (len INTEGER) AS');
    Add('DECLARE VARIABLE pat VARCHAR(100);');
    Add('BEGIN');
    Add('  IF (str IS NULL) THEN EXIT;');
    Add('  pat = '''';');
    Add('  len = 0;');
    Add('  WHILE (NOT str LIKE pat) DO BEGIN');
    Add('    pat = pat || ''_'';');
    Add('    len = len + 1;');
    Add('  END');
    Add('  SUSPEND;');
    Add('END');
    ExecConfQuery;
  end;}

	if not ProcedureExists('RPL$QUOTE_STR') then with Query do begin
		Add('CREATE PROCEDURE RPL$QUOTE_STR (CSTR VARCHAR(1000)) RETURNS (QUOTED_STR VARCHAR(1000))AS');
		Add('declare variable nStringLength integer;');
		Add('declare variable i integer;');
		Add('declare variable c varchar(6);');
    Add('begin');
    Add('  if (cStr is null) then');
    Add('    quoted_str = null;');
    Add('  else begin');
    Add('    quoted_str = '''';');
    Add('    i = 1;');
    if FB2 then
      Add('    nStringLength = char_length(cStr);')
    else
      Add('    nStringLength = strlen(cStr);');
    Add('    while (i <= nStringLength) do');
    Add('    begin');
    if FB2 then
      Add('      c = substring(cStr from i for 1);')
    else
      Add('      c = substr(cStr, i, i);');
    Add('        if (c = '''''''') then');
    Add('          c = '''''''''''';');
    Add('      quoted_str = quoted_str || c;');
    Add('      i = i + 1;');
    Add('    end');
    Add('    quoted_str = '''''''' || quoted_str || '''''''';');
		Add('  end');
    Add('  suspend;');
    Add('end');
    ExecConfQuery;

    Query.Add('GRANT EXECUTE ON PROCEDURE RPL$QUOTE_STR TO PUBLIC');
    ExecConfQuery;
  end;

	if not ProcedureExists('RPL$SPLIT_LIST') then with Query do begin
  	Add('create procedure RPL$SPLIT_LIST(list_to_split blob sub_type 1) returns (str varchar(100))');
  	Add('as');
  	Add('declare variable i integer;');
  	Add('declare variable inQuote integer;');
  	Add('declare variable c char(1);');
  	Add('declare variable nStringLength integer;');
  	Add('begin');
    Add('i = 1;');
    Add('str = '''';');
    Add('inQuote = 0;');
    Add('nStringLength = char_length(list_to_split);');
    Add('while (i <= nStringLength) do');
    Add('begin');
    Add('  c = substring(list_to_split from i for 1);');
    Add('  if (c = ''"'') then begin');
    Add('    if (inQuote = 0) then');
    Add('      inQuote = 1;');
    Add('    else');
    Add('      inQuote = 0;');
    Add('  end');
    Add('  else if (c = '','' and inQuote = 0) then begin');
    Add('    suspend;');
    Add('    str = '''';');
    Add('  end');
    Add('  else');
    Add('    str = str || c;');
    Add('');
    Add('  i = i + 1;');
    Add('end');
    Add('if (char_length(str) > 0) then');
    Add('  suspend;');
    Add('end');
    ExecConfQuery;

    Query.Add('GRANT EXECUTE ON PROCEDURE RPL$SPLIT_LIST TO PUBLIC');
    ExecConfQuery;
  end;

	if ((DBBranch = dbInterbase) or (BranchVersion < 20))and not TableExists('RPL$VARS') then begin
    Query.Clear;
    if (BranchVersion >= 75) then
      Query.Add('CREATE global temporary table RPL$VARS(replicating_node varchar(100)) on commit preserve')
    else
      Query.Add('CREATE table RPL$VARS(replicating_node varchar(100))');
    ExecConfQuery;
    Query.Add('GRANT ALL ON RPL$VARS TO PUBLIC');
    ExecConfQuery;
	end
	else if (DBBranch = dbFirebird) and (BranchVersion >= 20) then begin
		if not ProcedureExists('RPL$FORCE_REPL') then with Query do begin
			Add('CREATE PROCEDURE RPL$FORCE_REPL (config_name varchar(100), table_name varchar(50), sql_condition blob sub_type 1) returns (rows_affected integer) AS');
			Add('declare variable field_name varchar(50);');
			Add('declare variable sql blob sub_type 1;');
			Add('begin');
			Add('  select first 1 rf.rdb$field_name');
			Add('  from rdb$relation_fields rf');
			Add('  join rdb$fields f on f.rdb$field_name = rf.rdb$field_source');
			Add('  where f.rdb$field_type <> 261');
			Add('  and rf.rdb$relation_name = :table_name');
			Add('  order by rf.rdb$field_position into :field_name;');
			Add('  ');
			Add('  rdb$set_context(''USER_TRANSACTION'', ''FORCE_REPLICATION'', ''TRUE'');');
			Add('  ');
			Add('  sql = ''update '' || table_name || '' set '' || field_name || '' = '' || field_name || '' where '' || sql_condition;');
			Add('  execute statement sql;');
			Add('  rows_affected = row_count;');
			Add('  ');
			Add('  rdb$set_context(''USER_TRANSACTION'', ''FORCE_REPLICATION'', '''');');
			Add('  suspend;');
			Add('end');
      ExecConfQuery;
      Query.Add('GRANT EXECUTE ON PROCEDURE RPL$FORCE_REPL TO PUBLIC');
      ExecConfQuery;
		end;

    if not TableExists(UnQuotedIdentifier('RPL$TMP_VALUES')) then begin
      with Query do
      begin
        Clear;
        Add('CREATE GLOBAL TEMPORARY TABLE RPL$TMP_VALUES (');
        Add('  FIELD_NAME  VARCHAR(50) NOT NULL,');
        Add('  CHANGE_NUMBER  INTEGER NOT NULL,');
        Add('  FIELD_TYPE  INTEGER NOT NULL,');
        Add('  OLD_VALUE   VARCHAR(250), ');
        Add('  NEW_VALUE   VARCHAR(250), ');
        Add('  OLD_BLOB    BLOB SUB_TYPE 0,');
        Add('  NEW_BLOB    BLOB SUB_TYPE 0, ');
        Add('  OLD_BLOB_NULL CHAR(1) DEFAULT ''Y'', ');
        Add('  NEW_BLOB_NULL CHAR(1) DEFAULT ''Y'', ');
        Add('  PRIMARY KEY (CHANGE_NUMBER, FIELD_NAME)');
        Add(') ON COMMIT DELETE ROWS');
      end;
      ExecConfQuery;
      Query.Add('GRANT ALL ON RPL$TMP_VALUES TO PUBLIC');
      ExecConfQuery;
    end
    else if not FieldExists(UnQuotedIdentifier('RPL$TMP_VALUES'), UnQuotedIdentifier('OLD_BLOB_NULL')) then begin
      with Query do
      begin
        Clear;
        Add('ALTER TABLE RPL$TMP_VALUES ');
        Add('ADD ' + DeclareField('OLD_BLOB_NULL', ftFixedChar, 1, False, False, False) + ',');
        Add('ADD ' + DeclareField('NEW_BLOB_NULL', ftFixedChar, 1, False, False, False));
        ExecConfQuery;
      end;
    end;

    if not TableExists(UnQuotedIdentifier('RPL$TMP_CHANGES')) then begin
      with Query do
      begin
        Clear;
        Add('CREATE GLOBAL TEMPORARY TABLE RPL$TMP_CHANGES');
        Add('(');
        Add('  change_number integer not null primary key');
        Add(')');
      end;
      ExecConfQuery;
      Query.Add('GRANT ALL ON RPL$TMP_CHANGES TO PUBLIC');
      ExecConfQuery;
    end;
	end;
end;

procedure TCcInterbaseAdaptor.DoRegisterNode(NodeName: String);
begin
//  if (DBBranch = dbFirebird) and (BranchVersion <= 20) then begin
//    with TCcQuery.Create(FConnection, '') do begin
//      SQL.Text := 'select rdb$role_name from rdb$roles where rdb$role_name = ' + QuotedStr(NodeName);
//      Exec;
//      if RecordCount = 0 then begin
//        Query.Text := 'create role ' + NodeName;
//        ExecConfQuery;
//        Query.Text := 'grant ' + NodeName + ' to public';
//        ExecConfQuery;
//      end;
//      Free;
//    end;
//  end;
end;

function TCcInterbaseAdaptor.ConvertToOldGen(cPKGen: String; cNewOld: String):String;
begin
  Result := ReplaceString(cPKGen, 'new.', cNewOld + '.');
end;

{
procedure TCcInterbaseAdaptor.GetProcParams(ProcName: String; Params: TDataSet; InputParam: Boolean);
var
  nParamType:Integer;
begin
  if InputParam then nParamType := 0
                else nParamType := 1;
  Params.Close;
  Params.Open;
  if (ProcName <> '') then
  with FConnection.SelectQuery['IB_GetProcParams'] do
  begin
    Close;
		SQL.Text := 'select pp.rdb$parameter_name as param_name, f.rdb$field_type as field_type, f.rdb$field_length as field_length '+
      'from rdb$procedure_parameters pp '+
      'join rdb$fields f on f.rdb$field_name = pp.rdb$field_source '+
      'where (pp.rdb$system_flag = 0 or pp.rdb$system_flag is null) '+
      'and pp.rdb$procedure_name = :procedure_name '+
      'and pp.rdb$parameter_type = :param_type '+
      'order by pp.rdb$parameter_number ';
    Param['procedure_name'].Value := ProcName;
    Param['param_type'].Value := nParamType;
    Exec;
    while (not Eof) do
      begin
      Params.Append;
      Params.FieldByName('PARAM_NAME').AsString := Trim(Field['PARAM_NAME'].AsString);
      Params.FieldByName('FIELD_TYPE').AsInteger := Integer(TranslateDataType(Field['FIELD_TYPE'].AsInteger));
      Params.FieldByName('FIELD_LENGTH').AsInteger := Field['FIELD_LENGTH'].AsInteger;
      Params.Post;
      Next;
    end;
  end;
end; }

function TCcInterbaseAdaptor.GetCurrentTimeStampSQL: String;
begin
  Result := '''now''';
end;

function TCcInterbaseAdaptor.GetDeclaration(DataType: TFieldType; nSize: Integer): String;
begin
  case (DataType) of
    ftInteger: Result := 'INTEGER';
    ftFloat: Result := 'FLOAT';
    ftDate: Result := 'DATE';
    ftTime:
      if SQLDialect = 3 then
        Result := 'TIME'
      else
        Result := 'TIMESTAMP';
    ftDateTime:
//      if SQLDialect = 3 then
        Result := 'TIMESTAMP';
//      else
//        Result := 'DATE';
    ftString: Result := 'VARCHAR(' + InttoStr(nSize) + ')';
		ftFixedChar: Result := 'CHAR(' + InttoStr(nSize) + ')';
    ftBlob: Result := 'BLOB';
    ftMemo: Result := 'BLOB SUB_TYPE 1';
    else
      raise Exception.Create('Data type ' + IntToStr(Integer(DataType)) + ' not handled by TCcInterbaseAdaptor!');
  end;
end;

function TCcInterbaseAdaptor.TranslateDataType(LowLevelDataType: Integer): TFieldType;
begin
  case (LowLevelDataType) of
    7, 8, 9: Result := ftInteger;
    10: Result := ftFloat;
    12: Result := ftDate;
    13: Result := ftTime;
    14: Result := ftFixedChar;
    27: Result := ftFloat;
    35: Result := ftDateTime;
    37, 40: Result := ftString;
    261: Result := ftBlob;
    16: Result := ftBCD;
    23: Result := ftBoolean;
    else
      raise Exception.Create('Cannot determine datatype of field type ' + IntToStr(LowLevelDataType));
      //Result := ftUnknown;
  end;
end;

function TCcInterbaseAdaptor.QuoteSQLData(cData: String; DataType: TFieldType; lSQLStyle: Boolean):String;
begin
  Result := cData;
  if (DataType = ftDate) or (DataType = ftTime) or (DataType = ftDateTime)
       or (DataType = ftFixedChar) or (DataType = ftString) then
  begin
    if (lSQLStyle) then
      Result := 'select quoted_str from rpl$quote_str(' + cData + ')'
    else
      Result := QuotedStr(cData);
  end;
end;

function TCcInterbaseAdaptor.QuoteSQLDataLite(cData: String; DataType: TFieldType; lSQLStyle: Boolean):String;
begin
  Result := cData;
  if (DataType = ftDate) or (DataType = ftTime) or (DataType = ftDateTime)
       or (DataType = ftFixedChar) or (DataType = ftString) then
  begin
    if (lSQLStyle) then
      Result := '''' + cData + ''''
    else
      Result := QuotedStr(cData);
  end;
end;

function TCcInterbaseAdaptor.GetGenerator(GenName: String;
  Increment: Integer): String;
begin
  Result := 'GEN_ID(' + Trim(GenName) + ', ' + IntToStr(Increment) + ')';
end;

//function TCcInterbaseAdaptor.GetGeneratorSQL(GenName: String;
//  Increment: Integer): String;
//begin
//  Result := 'select ' + GetGenerator(GenName, Increment) + ' as code from rdb$database';
//end;

(*
function TCcInterbaseAdaptor.GetProcGenerator(ProcName: String; Params: TDataSet; OutputParam: String; FieldNames: TStringList): String;
var
  cParams, cParamValue:String;
  ParamType :TFieldType;
begin
  Params.First;
  while not Params.Eof do begin
    cParamValue := Trim(Params.FieldByName('PARAM_VALUE').AsString);
    ParamType := TFieldType(Params.FieldByName('FIELD_TYPE').AsInteger); //Trim(GetDataType(Params.FieldByName('FIELD_TYPE').AsInteger, Params.FieldByName('FIELD_LENGTH').AsInteger));

    if (cParams <> '') then cParams := cParams + ', ';
    if (FieldNames.IndexOf(cParamValue) <> -1) then
      cParams := cParams + ':' + cParamValue
    else
      cParams := cParams + QuoteSQLData(cParamValue, ParamType, false);
    Params.Next;
  end;
  Result := '(SELECT ' + Trim(OutputParam) + ' FROM ' + Trim(ProcName) + '(' + cParams + ' ))';
end;
*)

constructor TCcInterbaseAdaptor.Create(Conn: TCcConnection);
begin
  inherited;
  FKeys := TCcKeyRing.Create(Conn);
  RegisterVersions(['IB6.0', 'IB6.5', 'IB7', 'IB7.5', 'FB1.0', 'FB1.5', 'FB2.0', 'FB2.5']);
end;

procedure TCcInterbaseAdaptor.CreateProcedures;
begin
{  if not ProcedureExists('RPL$GENERATE_LOG') then with Query do begin
    Add('CREATE PROCEDURE RPL$GENERATE_LOG');
    Add('(');
    Add('  TABLE_NAME VARCHAR(100),');
    Add('  PRIMARY_KEY_VALUES VARCHAR(500),');
    Add('  PROCEDURE_STATEMENT VARCHAR(200),');
    Add('  user_login varchar(50),');
    Add('  PRIMARY_KEY_SYNC VARCHAR(5000),');
    Add('  UNIQUE_KEY_SYNC VARCHAR(5000)');
    Add(')');
    Add('AS');
    Add('declare variable replicating_node varchar(50);');
    Add('begin');
    GetReplicatingNode;
    Add('  insert into RPL$log (code, login, operation_date, table_name, sent_from,');
    Add('    primary_key_values, procedure_statement, PRIMARY_KEY_SYNC, UNIQUE_KEY_SYNC)');
    Add('  values (gen_id(GEN_RPL$LOG, 1), :user_login, ''now'', :table_name, :replicating_node,');
    Add('    :primary_key_values, :procedure_statement, :PRIMARY_KEY_SYNC, :UNIQUE_KEY_SYNC);');
    Add('end');
    ExecConfQuery;

    Add('GRANT EXECUTE ON PROCEDURE RPL$GENERATE_LOG TO PUBLIC');
    ExecConfQuery;
  end;       }
end;

procedure TCcInterbaseAdaptor.CleanupTransaction;
begin
  inherited;
  if (FConnection.ReplicatingNode <> '')
    and ((DBBranch = dbInterbase) and (BranchVersion < 75)
         or ((DBBranch = dbFirebird) and (BranchVersion < 20))) then
    EmptyTempTable;
end;

function TCcInterbaseAdaptor.GetSQLDialect: Integer;
begin
   Result := StrToIntDef(FConnection.ConnectionParams.Values['SQLDIALECT'], 3);
end;

procedure TCcInterbaseAdaptor.InitConnection;
begin
  inherited;
  if (DBBranch = dbInterbase) and (BranchVersion >= 75) then
    InsertTempTable
  else if (DBBranch = dbFirebird) and (BranchVersion >= 20) then
    with FConnection.SelectQuery['FB_SetContextInfo'] do begin
      Close;
      SQL.Text := 'select rdb$set_context(''USER_SESSION'', ''REPLICATING_NODE'', :node_name) from rdb$database';
      Param['node_name'].Value := FConnection.ReplicatingNode;
      Exec;
    end;
end;

function  TCcInterbaseAdaptor.GetGeneratorValue(GenName: String; Increment: Integer):
  {$IFDEF CC_D2K9}
     Int64;
  {$ELSE}
     Integer;
  {$ENDIF}
begin
  with FConnection.SelectQuery['IB_GetGeneratorValue'] do begin
    Close;
    SQL.Text := 'select GEN_ID(' + MetaQuote(GenName) + ', ' + IntToStr(Increment) + ') as code from rdb$database';
    Exec;
    Result := Field['code'].Value;
  end;
end;


procedure TCcInterbaseAdaptor.InsertTempTable;
begin
  with FConnection.UpdateQuery['IB_qInsertRplVars'] do begin
    Close;
    SQL.Text := 'insert into rpl$vars (replicating_node) values (:replicating_node)';
    Param['replicating_node'].Value := FConnection.ReplicatingNode;
    Exec;
  end;
end;

procedure TCcInterbaseAdaptor.InitTransaction;
begin
  inherited;
  if (FConnection.ReplicatingNode <> '')
   and (((DBBranch = dbInterbase) and (BranchVersion <= 70))
       or ((DBBranch = dbFirebird) and (BranchVersion < 20)))
   then begin
    //We have to emulate temporary tables...

    //First delete any records left over from last connection
    with FConnection.SelectQuery['IB_CheckTempTableEmpty'] do begin
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

procedure TCcInterbaseAdaptor.EmptyTempTable;
begin
  with FConnection.UpdateQuery['IB_EmptyTempTable'] do begin
    Close;
    SQL.Text := 'delete from rpl$vars';
    Exec;
  end;
end;

function TCcInterbaseAdaptor.GetFieldTypeSQLText(FieldName, TableName : String): String;
begin
  Result := 'select first 1 ' + MetaQuote(FieldName) + ' from ' + MetaQuote(tableName);
end;


procedure TCcInterbaseAdaptor.BeforeConnect;
begin
  inherited;
  if (DBBranch = dbFirebird) and (BranchVersion <= 20) then
    FConnection.ConnectionParams.Values['ROLE_NAME'] := FConnection.ReplicatingNode;
end;

function TCcInterbaseAdaptor.GetInsertOrUpdateSQL(slFields: TStringList; sourceDBAdaptor: TCcDBAdaptor; keys: TCcCustomKeyRing; tableName: String): string;

begin
  Result := 'update or insert into %cc_table_name_macro (' + ListFieldNames(slFields, sourceDBAdaptor, '') + ')'#13#10'values'#13#10'(' + ListFieldNames(slFields, sourceDBAdaptor, ':') + ')' +
    ' matching (' + ListFieldNames((keys as TCcKeyRing).TableKeys[tableName], keys.Connection.DBAdaptor, '') + ')';
end;

function TCcInterbaseAdaptor.GetMillisecFractions(d: TDateTime): String;
var
  nDays, nHours, nMinutes, nSeconds, nMilliSeconds: Integer;
  fraction: double;
begin
  fraction := Double(d);
  nDays := Floor(fraction);
  fraction := (fraction - nDays) * 24;

  nHours := Floor(fraction);
  fraction := (fraction - nHours) * 60;

  nMinutes := Floor(fraction);
  fraction := (fraction - nMinutes) * 60;

  nSeconds := Floor(fraction);
  fraction := (fraction - nSeconds) * 1000;

  nMilliSeconds := Floor(fraction);
  fraction := (fraction - nMilliSeconds) * 10;

  if(Round(fraction) = 10) then
    Result := '0'
  else
    Result := IntToStr(Round(fraction));
end;

//Cette fonction sert à mettre entre guillemets les dates et les chaînes de caractères
//Si cData est vide, on renvoie 'null'
function TCcInterbaseAdaptor.SQLFormatValue(Data :Variant; FieldType :TFieldType) :String;
var
  DataType :Integer;
  OldDecimalSeparator: Char;
  OldCurrencyString: String;
begin
  if VarIsEmpty(Data) or VarIsNull(Data) then
    Result := 'null'
  else begin
{    cData := Data;
    if Trim(cData) = '' then
      Result := 'null'
    else begin}
      DataType := VarType(Data) and varTypeMask;
      if ((DataType = varString) or (DataType = varOleStr) {$IFDEF CC_D2K9} or (DataType = varUString){$ENDIF} ) then
        Result := QuotedStr(Data)
        else if (FieldType = ftDateTime) {$IFDEF CC_D6}or (FieldType = ftTimeStamp) {$ENDIF} then
          Result := '''' + FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', TDateTime(Data)) + GetMillisecFractions(TDateTime(Data)) +''''
        else if (FieldType = ftDate) then
          Result := '''' + FormatDateTime('dd.mm.yyyy', TDateTime(Data)) + ''''
        else if (FieldType = ftTime) then
          Result := '''' + FormatDateTime('hh:nn:ss.zzz', TDateTime(Data)) + GetMillisecFractions(TDateTime(Data)) + ''''
      else begin

        {$IFDEF CC_D2K13}
        OldDecimalSeparator := FormatSettings.DecimalSeparator;
        OldCurrencyString := FormatSettings.CurrencyString;

        FormatSettings.DecimalSeparator := '.';
        FormatSettings.CurrencyString := '';
        {$ELSE}
        OldDecimalSeparator := DecimalSeparator;
        OldCurrencyString := CurrencyString;

        DecimalSeparator := '.';
        CurrencyString := '';
        {$ENDIF}
        try
          Result := Data;
        finally
          {$IFDEF CC_D2K13}
          FormatSettings.DecimalSeparator := OldDecimalSeparator;
          FormatSettings.CurrencyString := OldCurrencyString;
          {$ELSE}
          DecimalSeparator := OldDecimalSeparator;
          CurrencyString := OldCurrencyString;
          {$ENDIF}
        end;
      end;
  end;
end;

class function TCcInterbaseAdaptor.GetAdaptorName: String;
begin
  Result := 'Interbase';
end;

function TCcInterbaseAdaptor.GetQuoteMetadata:Boolean;
begin
  Result := (SQLDialect = 3);
end;

function TCcInterbaseAdaptor.GetUseRowsAffected: Boolean;
begin
  Result := True;
end;

procedure TCcInterbaseAdaptor.GetUniqueIndices(cTableName: String; list :TStringList);
var
  qIndices, qSegments: TCcQuery;
  cIndexName: String;
begin
  qIndices := FConnection.SelectQuery['IBAdaptor.GetUniqueIndices'];
  qIndices.Close;
  qIndices.SQL.Text := 'select cast(i.rdb$index_name as varchar(50)) as index_name ' +
                'from rdb$indices i ' +
        				'where %upper_case(i.rdb$relation_name) = %upper_case(:table_name) '+
                'and i.rdb$unique_flag = 1 '+
                'order by i.rdb$index_name ';
  if QuoteMetadata then
    qIndices.Macro['upper_case'].Value := ''
  else
    qIndices.Macro['upper_case'].Value := 'upper';
  qIndices.Param['table_name'].AsString := Trim(cTableName);
  qIndices.Exec;

  if qIndices.RecordCount > 0 then begin
    cIndexName := qIndices.Field['index_name'].AsString;
    qSegments := FConnection.SelectQuery['IBAdaptor.GetUniqueIndices'];
    qSegments.Close;
    qSegments.SQL.Text := 'select cast(ins.rdb$field_name  as varchar(50)) as field_name ' +
                  'from rdb$index_segments ins ' +
                  'where ins.rdb$index_name = :index_name '+
                  'order by ins.rdb$field_position ';
    qSegments.Param['index_name'].AsString := cIndexName;
    qSegments.Exec;

		while not qSegments.Eof do begin
			list.Add(qSegments.Field['field_name'].AsString);
			qSegments.Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListPrimaryKeys(cTableName: String;
	list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListPrimaryKeys'] do begin
		Close;
		SQL.Text := 'select cast(i.rdb$field_name as varchar(50)) as pk_name '+
				'from rdb$relation_constraints rel '+
				'join rdb$index_segments i on rel.rdb$index_name = i.rdb$index_name '+
				'where rel.rdb$constraint_type = ''PRIMARY KEY'' '+
				'and %upper_case(rel.rdb$relation_name) = %upper_case(:table_name) '+
				'order by i.rdb$field_position';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['pk_name'].AsString));
			Next;
		end;
	end;
  if list.Count = 0 then
    GetUniqueIndices(cTableName, list);
end;

procedure TCcInterbaseAdaptor.DoListProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListProcedures'] do begin
		Close;
		SQL.Text := 'select distinct cast(p.rdb$procedure_name as varchar(50)) as all_proc_name '+
				'from rdb$procedures p '+
				'where p.rdb$system_flag = 0 '+
				'and not exists (select rdb$parameter_name from rdb$procedure_parameters pp where pp.rdb$procedure_name = p.rdb$procedure_name and pp.rdb$parameter_type = 1) ' +
				'order by p.rdb$procedure_name ';
		Exec;

		while not Eof do begin
			list.Add(Trim(Field['all_proc_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListAllProcedures(list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListAllProcedures'] do begin
		Close;
		SQL.Text := 'select distinct cast(p.rdb$procedure_name as varchar(50)) as proc_name  '+
				'from rdb$procedures p '+
				'where p.rdb$system_flag = 0 '+
				'order by p.rdb$procedure_name ';
		Exec;

		while not Eof do begin
			list.Add(Trim(Field['proc_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListFieldsForNoPK(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListFieldForNoPK'] do begin
		Close;
		SQL.Text := 'select cast(rf.rdb$field_name as varchar(50)) as field_name from rdb$relation_fields rf ' +
         ' join rdb$fields f on f.rdb$field_name = rf.rdb$field_source ' +
				 ' where %upper_case(rdb$relation_name) = %upper_case(:table_name) ' +
         ' and f.rdb$field_type <> 261 and f.rdb$field_length < 50' +
         ' order by rf.rdb$field_name';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListGenerators(list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListGenerators'] do begin
		Close;
		SQL.Text := 'select cast(g.rdb$generator_name as varchar(50)) as generator_name '+
				'from rdb$generators g '+
				'where ((g.rdb$system_flag is null) or (g.rdb$system_flag = 0)) '+
				'order by 1 ';
		Exec;

		while not Eof do begin
			list.Add(Trim(Field['generator_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListKeywordsForbiddenAsFieldNames(
  list: TStringList);
begin
  inherited;
  if SQLDialect = 1 then begin
    list.Add('TYPE');
    list.Add('DATE');
    list.Add('USER');
    list.Add('START');
  end;
end;

procedure TCcInterbaseAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListTableFields'] do begin
		Close;
		SQL.Text := 'select cast(rf.rdb$field_name as varchar(50)) as field_name from rdb$relation_fields rf ' +
				 ' where %upper_case(rdb$relation_name) = %upper_case(:table_name) order by rf.rdb$field_name';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
	end;
end;

function TCcInterbaseAdaptor.SupportsGenerators: Boolean;
begin
  Result := True;
end;

function TCcInterbaseAdaptor.SupportsInsertOrUpdate: Boolean;
begin
  Result := (DBBranch = dbFirebird) and (BranchVersion >= 21);
end;

procedure TCcInterbaseAdaptor.DoListTables(list: TStringList; IncludeTempTables: Boolean);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListTables'] do begin
		Close;
		SQL.Text := 'select cast(r.rdb$relation_name as varchar(50)) as table_name from rdb$relations r '+
				' where (r.rdb$system_flag = 0 or r.rdb$system_flag is null) and r.RDB$VIEW_BLR IS NULL ';
    if (DBBranch = dbFirebird) and (BranchVersion >= 21) and not IncludeTempTables then
      SQL.Add(' and ((r.rdb$relation_type = 0) or r.rdb$relation_type is null) ');
    SQL.Add('order by 1');
		Exec;
		while not Eof do begin
			list.Add(TrimRight(Field['table_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListTriggers(list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListTriggers'] do begin
		Close;
		SQL.Text := 'select cast(rdb$trigger_name as varchar(50)) as trigger_name from rdb$triggers';
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['trigger_name'].AsString));
			Next;
		end;
	end;
end;

procedure TCcInterbaseAdaptor.DoListUpdatableTableFields(
  cTableName: String; list: TStringList);
begin
	with FConnection.SelectQuery['IBAdaptor.DoListUpdatableTableFields'] do begin
		Close;
		SQL.Text := 'select cast(rf.rdb$field_name as varchar(50)) as FIELD_NAME from rdb$relation_fields rf ' +
				' join rdb$fields f on f.rdb$field_name = rf.rdb$field_source ' +
				' where f.rdb$computed_blr is null and %upper_case(rf.rdb$relation_name) = %upper_case(:table_name)';

		if QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
		Param['table_name'].AsString := Trim(cTableName);
		Exec;
		while not Eof do begin
			list.Add(Trim(Field['FIELD_NAME'].AsString));
			Next;
		end;
	end;
end;

function TCcInterbaseAdaptor.DeclarePK(FieldNames: String): String;
begin
  Result := 'primary key (' + FieldNames + ')';
end;

initialization
//  RegisterDBAdaptors([TCcInterbaseAdaptor.GetAdaptorName], [TCcInterbaseAdaptor]);

end.
