unit CcKeys;

interface

{$I CC.INC}

uses
  Classes, DB, CcDb, CcProviders;

type

// Indicates whether the keys come from RPL$LOG (via the triggers), or were loaded manually
// This is important to know because the value formatting may differ (eg. for date fields)
TCcKeyOrigin = (koDelphi, koRPLLOG);

TCcKey = class
  public
    PrimaryKey :Boolean;
    KeyName: String;
    KeyValue: Variant;
    OriginalKeyValue: Variant;
    GenericSyncStatement: String;
    SyncStatement: String;
    DataType: TFieldType;
end;

//This class holds a list of primary key values as parsed from the RPL$LOG.PRIMARY_KEY_FIELDS.
//You can use the Keys property to get the values of each of the fields that are part of the primary key.
//See also: 
//TCcLog.Keys
TCcKeyRing = class(TCcCustomKeyRing)
  private
    FTableKeys: TStringList;
    FPKValues: String;
    FGenericPKSync: String;
    FGenericUKSync: String;
    FUKSync: string;
    FUKNames: string;
    FPKSync: string;
    FTableName: String;
    FKeyValues: TStringList;
    FFailIfNoPK: Boolean;
    FKeyOrigin : TCcKeyOrigin;
    FKeyList: TStringList;
    FKeyTypes: TStringList;
    function GetKeyCount: Integer;
    function GetKey(nKeyNum: Integer): TCcKey;
    procedure GetPrimaryKeys(cTableName: String; List: TStringList);
    function GetTableKeys(cTableName: String): TStringList;
    function GetKeyValues: TStringList;
    procedure SetFailIfNoPK(const Value: Boolean);
    procedure AddKey(cTableName, cFieldName, cFieldSync, cGenericFieldSync: String);
    function GetKeyFieldType(Table, Field: String): TFieldType;
  protected
    property KeyValues: TStringList read GetKeyValues;
    function DoLoadKeysFromDataSet(cTableName: String; cPKVals, cPKSync, cUKSync: String): String;
  public
    //ClearTableKeys clears the cache of primary key lists.
    //Call this method if you need to connect to a different database
    procedure ClearTableKeys;

    //Indicates whether tables with no primary key are accepted or not
    //If FailIfNoPK is true (the default value), then an error will be raised
    //if a table is detected with no primary key.
    //If FailIfNoPK is false, the full list of field values (excluding blobs and string fields over 50 chars)
    //will be used instead
    property FailIfNoPK: Boolean read FFailIfNoPK write SetFailIfNoPK;

    function LoadKeysFromDataSet(cTableName: String; DataSet: TCcQuery; cPKSync, cUKSync: String): String;overload;
    function LoadKeysFromDataSet(cTableName: String; DataSet: TCcMemoryTable; cPKSync, cUKSync: String): String;overload;

    //Call LoadKeys to load the keys of a new record into the key list.
    //Note: If the table is not the same, the list of primary keys will be fetched from the server, but if
    //the same table is used twice, the values are cached internally until ClearTableKeys is called.
    procedure LoadKeys(cTableName, PrimaryValues, PrimarySync, UniqueSync, GenericPrimarySync, UniqueNames, GenericUniqueSync: String; KeyOrigin: TCcKeyOrigin = koRPLLOG);
    property TableName: String read FTableName;
    property PrimaryKeyValues: String read FPKValues;
    property PrimaryKeySync: String read FPKSync;
    property UniqueKeySync: String read FUKSync;
    property GenericPrimaryKeySync: String read FGenericPKSync;
    property UniqueKeyNames: String read FUKNames;
    property GenericUniqueKeySync: String read FGenericUKSync;

    procedure LoadFromDataSet(DataSet: TDataSet; cTableName: String; cNameField, cSyncField: String);
    procedure SaveToDataSet(DataSet: TDataSet; cNameField, cSyncField: String; lPrimaryKey:Boolean);
    //Call SaveKeys to write the name, value and synchronization SQL of each key into
    //the PrimaryKeyValues, PrimaryKeySync, UniqueKeyNames, and UniqueKeySync properties
    procedure SaveKeys;
    procedure Clear;
    property TableKeys[cTableName: String]: TStringList read GetTableKeys;
    property Keys[Index: Integer]: TCcKey read GetKey; default;
    property Count: Integer read GetKeyCount;
    function KeyExists(cKeyName: String): Boolean;
    function FindKey(cKeyName: String): TCcKey;
    constructor Create(Conn: TCcConnection);override;
    destructor Destroy; override;
end;

function ParseKeyValues(Values: String; var Index: Integer; FailIfEmpty: Boolean): Variant;

implementation

uses Sysutils {$IFDEF CC_D6}, Variants{$ENDIF};

procedure TCcKeyRing.AddKey(cTableName, cFieldName, cFieldSync, cGenericFieldSync: String);
var
  Key: TCcKey;
begin
  Key := TCcKey.Create;
  Key.KeyName := cFieldName;
  Key.DataType := FConnection.GetFieldType(cTableName, Key.KeyName);
  Key.SyncStatement := cFieldSync;
  Key.GenericSyncStatement := cGenericFieldSync;
  if (TableKeys[FTableName].IndexOf(cTableName) > -1) then
    Key.PrimaryKey := True
  else
    Key.PrimaryKey := False;
  FKeyValues.AddObject(Key.KeyName, Key);
end;

procedure TCcKeyRing.Clear;
var
  i:Integer;
begin
  for I := 0 to FKeyValues.Count-1 do begin
    if Assigned(FKeyValues.Objects[i]) then
      FKeyValues.Objects[i].Free;
  end;
  FKeyValues.Clear;
  FPKValues := '';
  FPKSync := '';
  FUKSync := '';
  FGenericPKSync := '';
  FUKNames := '';
  FGenericUKSync := '';
//  FKeysSet := False;
end;

procedure TCcKeyRing.ClearTableKeys;
var
  I: Integer;
begin
  for I := 0 to FTableKeys.Count-1 do begin
    if Assigned(FTableKeys.Objects[i]) then
      FTableKeys.Objects[i].Free;
  end;
  FTableKeys.Clear;
  FKeyTypes.Clear;
//  FKeysSet := False;
end;

constructor TCcKeyRing.Create(Conn: TCcConnection);
begin
  inherited;
  FFailIfNoPK := True;
  FKeyValues := TStringList.Create;
	FTableKeys := TStringList.Create;
	FKeyTypes := TStringList.Create;
end;

destructor TCcKeyRing.Destroy;
begin
  Clear;
  ClearTableKeys;
  FTableKeys.Free;
	FKeyValues.Free;
	FKeyTypes.Free;
end;

function TCcKeyRing.FindKey(cKeyName: String): TCcKey;
var
  nIndex: Integer;
begin
  nIndex := KeyValues.IndexOf(cKeyName);
  if (nIndex > -1) then
    Result := TCcKey(KeyValues.Objects[nIndex])
  else
    Result := nil;
end;

function TCcKeyRing.GetKey(nKeyNum: Integer): TCcKey;
begin
  Result := nil;
  if (not Assigned(KeyValues.Objects[nKeyNum])) then
    Exit;
  Result := TCcKey(KeyValues.Objects[nKeyNum]);
end;

function TCcKeyRing.GetKeyCount: Integer;
begin
  Result := KeyValues.Count;
end;

function TCcKeyRing.GetKeyFieldType(Table, Field: String): TFieldType;
var
  cIndex: string;
begin
  cIndex := FTableName + '.' + Field;
  if FKeyTypes.Values[cIndex] = '' then
    FKeyTypes.Values[cIndex] := IntToStr(Integer(FConnection.GetFieldType(FTableName, Field)));
  Result := TFieldType(StrToInt(FKeyTypes.Values[cIndex]));
end;

function TCcKeyRing.GetKeyValues: TStringList;
var
  I: Integer;
  KeyIndex: Integer;
  SyncIndex: Integer;
  GenericSyncIndex: Integer;
  UniqueNameIndex, UniqueSyncIndex, GenericUniqueSyncIndex: Integer;
  Key: TCcKey;
	cUniqueKeyNames: String;
	cKeyName: String;
begin
  if FKeyValues.Count = 0 then begin
    KeyIndex := {$IFDEF NEXTGEN}0{$ELSE}1{$ENDIF};
    SyncIndex := {$IFDEF NEXTGEN}0{$ELSE}1{$ENDIF};
    GenericSyncIndex := 1;
    with TableKeys[FTableName] do begin
      for I := 0 to Count - 1 do begin
        Key := TCcKey.Create;
        Key.PrimaryKey := True;
        Key.KeyName := Strings[i];
        Key.DataType := GetKeyFieldType(FTableName, Key.KeyName);
        if FKeyOrigin = koRPLLOG then begin
          if FPKValues <> '' then
            Key.KeyValue := FConnection.DBAdaptor.ConvertValue(ParseKeyValues(FPKValues, KeyIndex, True), Key.DataType)
        end
        else
          Key.KeyValue := ParseKeyValues(FPKValues, KeyIndex, True);
        Key.OriginalKeyValue := Key.KeyValue;
        Key.SyncStatement := ParseKeyValues(FPKSync, SyncIndex, False);
        Key.GenericSyncStatement := ParseKeyValues(FGenericPKSync, GenericSyncIndex, False);
        FKeyValues.AddObject(Key.KeyName, Key);
      end;
    end;

		cUniqueKeyNames := FUKNames;
    UniqueNameIndex := 1;
    UniqueSyncIndex := 1;
		GenericUniqueSyncIndex := 1;
		while UniqueNameIndex <= Length(cUniqueKeyNames) do begin
			cKeyName := ParseKeyValues(cUniqueKeyNames, UniqueNameIndex, False);
			if (Trim(cKeyName) <> '') then begin
				Key := TCcKey.Create;
				Key.PrimaryKey := False;
				Key.KeyName := cKeyName;
				Key.SyncStatement := ParseKeyValues(FUKSync, UniqueSyncIndex, False);
				Key.GenericSyncStatement := ParseKeyValues(FGenericUKSync, GenericUniqueSyncIndex, False);
				FKeyValues.AddObject(Key.KeyName, Key);
			end
      else
        break;
		end;
  end;
  Result := FKeyValues;
end;

procedure TCcKeyRing.GetPrimaryKeys(cTableName: String; List:TStringList);
begin
	List.Assign(FConnection.ListPrimaryKeys(cTableName));
  if (List.Count = 0) then begin
    if FFailIfNoPK then
      raise Exception.Create(Format('No primary key defined for table %s', [cTableName]))
    else
      List.Assign(FConnection.ListFieldsForNoPK(cTableName));
  end;

	{
	with FConnection.MetaQuery[sqlTablePKs] do begin
//    SQL := 'select i.rdb$field_name as pk_name '+
//      'from rdb$relation_constraints rel '+
//      'join rdb$index_segments i on rel.rdb$index_name = i.rdb$index_name '+
//      'where rel.rdb$constraint_type = ''PRIMARY KEY'' '+
//      'and rel.rdb$relation_name = :table_name '+
//      'order by i.rdb$field_position';
		Close;
		Param['table_name'].AsString := cTableName;
		Exec;

		while not Eof do begin
			List.Add(Trim(Field['pk_name'].AsString));
			Next;
		end;
	end;}
end;

function TCcKeyRing.GetTableKeys(cTableName: String): TStringList;
var
  nIndex: Integer;
begin
  nIndex := FTableKeys.IndexOf(cTableName);
  if nIndex = -1 then begin
    Result := TStringList.Create;
    GetPrimaryKeys(cTableName, Result);
    FTableKeys.AddObject(cTableName, Result);
  end
  else
    Result := TStringList(FTableKeys.Objects[nIndex]);
end;

function TCcKeyRing.KeyExists(cKeyName: String): Boolean;
begin
  Result := FindKey(cKeyName) <> nil;
end;

procedure TCcKeyRing.LoadFromDataSet(DataSet: TDataSet; cTableName, cNameField,
  cSyncField: String);
var
  cFieldName: String;
  cSyncSQL: String;
begin
//  DataSet.First;
  while not DataSet.Eof do begin
    cFieldName := DataSet.FieldByName(cNameField).AsString;
    cSyncSQL := DataSet.FieldByName(cSyncField).AsString;
    if KeyExists(cFieldName) then
      FindKey(cFieldName).GenericSyncStatement := cSyncSQL
    else
      AddKey(cTableName, cFieldName, '', cSyncSQL);
    DataSet.Next;
  end;
end;

procedure TCcKeyRing.LoadKeys(cTableName, PrimaryValues, PrimarySync, UniqueSync,
  GenericPrimarySync, UniqueNames, GenericUniqueSync: String; KeyOrigin: TCcKeyOrigin);
begin
  Clear;
  FTableName := cTableName;
  FPKValues := PrimaryValues;
  FPKSync := PrimarySync;
  FUKSync := UniqueSync;
	FGenericPKSync := GenericPrimarySync;
	FUKNames := UniqueNames;
  FGenericUKSync := GenericUniqueSync;
  FKeyOrigin := KeyOrigin;
end;

function ParseKeyValues(Values: String; var Index: Integer; FailIfEmpty: Boolean): Variant;
var
  CurrentChar: Char;
  CurrentExpression:String;
  InQuote:Boolean;
  Escaped:Boolean;
  i:Integer;
  IsNull: Boolean;

  procedure EndExpression;
  begin
    Index := I+1;
    if IsNull then
      Result := Null
    else
      Result := CurrentExpression;
  end;

begin
  Result := '';
  Escaped := False;
  InQuote := False;
  I := Index;
  while I <= {$IFDEF NEXTGEN}High(Values){$ELSE}Length(Values){$ENDIF} do begin
    CurrentChar := Values[I];

    IsNull := False;
    if (CurrentChar = '''') and (not Escaped) then begin
      if (not InQuote) then
        InQuote := True
      else
        Escaped := True;
    end
    else if Escaped and (CurrentChar <> '''') then begin
      EndExpression;
      Exit;
      //InQuote := False;
    end
    else if (not InQuote) and (CurrentChar = '"') then begin
      IsNull := True;
      Inc(I);
      EndExpression;
      Exit;
    end
    else begin
      CurrentExpression := CurrentExpression + CurrentChar;
      Escaped := False;
    end;
    Inc(I);
  end;
  if Escaped then
    EndExpression;

  if (Trim(Values) <> '') and (not IsNull) and (Trim(Result) = '') and FailIfEmpty then
    raise Exception.Create('Error parsing key values, value : ' + Values);
end;

procedure TCcKeyRing.SaveKeys;
var
  i: Integer;
  Key: TCcKey;
begin
  FPKValues := '';
  FPKSync := '';
  FUKSync := '';
  FGenericPKSync := '';
  FUKNames := '';
  FGenericUKSync := '';
  for i := 0 to KeyValues.Count - 1 do begin
    Key := GetKey(i);
    if Key.PrimaryKey then begin
      FPKSync := FPKSync + QuotedStr(Key.SyncStatement) + ';';
      FGenericPKSync := FGenericPKSync + QuotedStr(Key.GenericSyncStatement) + ';';
      FPKValues := FPKValues + QuotedStr(Key.KeyValue) + ';';
    end
    else begin
      FUKSync := FUKSync + QuotedStr(Key.SyncStatement) + ';';
      FGenericUKSync := FGenericUKSync + QuotedStr(Key.GenericSyncStatement) + ';';
      FUKNames := FUKNames + QuotedStr(Key.KeyName) + ';';
    end;
  end;
end;

procedure TCcKeyRing.SaveToDataSet(DataSet: TDataSet; cNameField,
  cSyncField: String; lPrimaryKey:Boolean);
var
  i:Integer;
  Key :TCcKey;
begin
  for i:=0 to KeyValues.Count-1 do begin
    Key := Keys[i];
    if Key.PrimaryKey = lPrimaryKey then begin
      DataSet.Append;
      DataSet.FieldByName(cNameField).AsString := Key.KeyName;
      DataSet.FieldByName(cSyncField).AsString := Key.GenericSyncStatement;
      DataSet.Post;
    end;
  end;
end;

procedure TCcKeyRing.SetFailIfNoPK(const Value: Boolean);
begin
  FFailIfNoPK := Value;
end;

function TCcKeyRing.DoLoadKeysFromDataSet(cTableName: String; cPKVals: String; cPKSync, cUKSync: String): String;
var
  cGenericPKSync, cUniqueNames, cGenericUniqueSync: String;
begin
  if TableName <> cTableName then begin
		with FConnection.SelectQuery['TCcKeyRing_LoadKeysFromDataSet'] do begin
      Close;
      SQL.Text := 'select primary_key_sync, unique_key_names, unique_key_sync from RPL$TABLES where table_name = ' + QuotedStr(cTableName);
      Exec;
      cGenericPKSync := Field['primary_key_sync'].AsString;
      cUniqueNames := Field['unique_key_names'].AsString;
      cGenericUniqueSync := Field['unique_key_sync'].AsString;
    end;
    LoadKeys(cTableName, cPKVals, cPKSync, cUKSync, cGenericPKSync, cUniqueNames, cGenericUniqueSync, koDelphi);
  end
  else
    LoadKeys(cTableName, cPKVals, cPKSync, cUKSync, FGenericPKSync, FUKNames, FGenericUKSync, koDelphi);
  Result := cPKVals;
end;

function TCcKeyRing.LoadKeysFromDataSet(cTableName: String; DataSet: TCcQuery; cPKSync, cUKSync: String): String;
var
  cPKVals: string;
  I: Integer;
  f: TCcField;
begin
  cPKVals := '';
  for I := 0 to TableKeys[cTableName].Count - 1 do begin
    f := DataSet.Field[TableKeys[cTableName].Strings[i]];
    if f.IsNull then
      cPKVals := cPKVals + '";'
    else
      cPKVals := cPKVals + QuotedStr(f.AsString) + ';';
  end;
//   cPKVals := cPKVals + QuotedStr(DataSet.Field[TableKeys[cTableName].Strings[i]].AsString) + ';';

  Result := DoLoadKeysFromDataSet(cTableName, cPKVals, cPKSync, cUKSync);
end;

function TCcKeyRing.LoadKeysFromDataSet(cTableName: String; DataSet: TCcMemoryTable; cPKSync, cUKSync: String): String;
var
  cPKVals: string;
  I: Integer;
  f: TCcMemoryField;
begin
  cPKVals := '';
  for I := 0 to TableKeys[cTableName].Count - 1 do begin
    f := DataSet.FieldByName(TableKeys[cTableName].Strings[i]);
    if f.IsNull then
      cPKVals := cPKVals + '";'
    else
      cPKVals := cPKVals + QuotedStr(f.AsString) + ';';
  end;

  Result := DoLoadKeysFromDataSet(cTableName, cPKVals, cPKSync, cUKSync);
end;

end.

