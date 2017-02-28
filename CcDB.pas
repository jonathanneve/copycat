unit CcDB;

{$I CC.INC}

interface

uses Classes, Sysutils, CcProviders, DB{$IFNDEF FPC}, CcMemDS{$ENDIF}{$IFDEF CC_D2K12}, System.Generics.Collections{$ENDIF};

type

  TCcFieldInfo = class
    // private
    // FValIsAssigned : Boolean;
    // FValue: Variant;
    // procedure SetValue(Val: Variant);
  public
    FieldName: string;
    DataType: TFieldType;
    Size: Integer;
    IsNull: Boolean;
    Value: Variant;

    // property Value: Variant read FValue write SetValue;
    // property ValIsAssigned : Boolean read FValIsAssigned;
    // constructor Create;
  end;

  TCcFieldList = class
  private
    FList: TStringList;
    function GetCount: Integer;
    function GetFieldInfo(Index: Integer): TCcFieldInfo;
    function GetFieldInfoByName(FieldName: string): TCcFieldInfo;
  public
    procedure Add(field: TCcFieldInfo);
    procedure Clear;
    property FieldInfo[index: Integer]: TCcFieldInfo read GetFieldInfo; default;
    property FieldInfoByName[FieldName: string]: TCcFieldInfo read GetFieldInfoByName;
    property Count: Integer read GetCount;
    constructor Create;
    destructor Destroy; override;
  end;

  TCcArray = class;

  TCcValueType = (vtField, vtArray, vtNull);

  TCcValue = class
  private
    FType: TCcValueType;
    FField: TCcFieldInfo;
    FArray: TCcArray;
    procedure SetValue(const Val: Variant);
    function GetValue: Variant;
    function GetAsArray: TCcArray;
    function GetAsField: TCcFieldInfo;
    procedure SetAsArray(const Value: TCcArray);
    procedure SetAsField(const Value: TCcFieldInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Val: TCcValue);
    procedure SetValueAsType(Val: Variant; DataType: TFieldType);
    property ValueType: TCcValueType read FType;
    property Value: Variant read GetValue write SetValue;
    property AsField: TCcFieldInfo read GetAsField write SetAsField;
    property AsArray: TCcArray read GetAsArray write SetAsArray;
  end;

  TCcArray = class
  private
    FValues: TStringList;
    function GetCount: Integer;
    function GetValue(Index: Integer): TCcValue;
  public
    procedure Clear;
    function Add: TCcValue;
    procedure AddValue(Val: TCcValue);
    property Value[index: Integer]: TCcValue read GetValue; default;
    property Count: Integer read GetCount;
    constructor Create;
    destructor Destroy; override;
  end;

  TCcRecord = class
  private
    FValues: TStringList;
    function GetFieldName(Index: Integer): string;
    function GetCount: Integer;
    function GetValue(FieldName: string): TCcValue;
  public
    procedure Clear;
    function Add(FieldName: string): TCcValue;
    property Value[FieldName: string]: TCcValue read GetValue; default;
    property Count: Integer read GetCount;
    property FieldName[index: Integer]: string read GetFieldName;
    function FieldExists(cFieldName: string): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TCcMemoryTable = class;
  TCcMemoryFields = class;

  TCcMemoryField = class
  private
    FFields: TCcMemoryFields;
    FFieldName: string;
    FVisible: Boolean;
    FIndex: Integer;
    FDateType: TFieldType;
    FSize: Integer;
    FIsNull: Boolean;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    function GetIsNull: Boolean;
    function GetAsString: string;
    procedure SetAsString(const Val: string);
    procedure SetIsNull(const Value: Boolean);
  public
    property FieldName: string read FFieldName;
    property DataType: TFieldType read FDateType write FDateType;
    property Size: Integer read FSize write FSize;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    procedure Clear;
    property Value: Variant read GetValue write SetValue;
    property AsString: string read GetAsString write SetAsString;
    property index: Integer read FIndex;
    property Visible: Boolean read FVisible write FVisible;
    constructor Create(fields: TCcMemoryFields);
  end;

  TCcMemoryFields = class
  private
    FFields: TList{$IFDEF CC_D2K12}<TCcMemoryField>{$ENDIF};
    FFieldsByName: TStringList;
    FTable: TCcMemoryTable;
    function GetFieldByIndex(Index: Integer): TCcMemoryField;
    function GetCount: Integer;
  public
    procedure Clear;
    constructor Create(table: TCcMemoryTable);
    destructor Destroy; override;
    function Add(cFieldName: String): TCcMemoryField;
    function FindField(name: string): TCcMemoryField;
    function FieldByName(name: string): TCcMemoryField;
    property FieldByIndex[index: Integer]: TCcMemoryField read GetFieldByIndex; default;
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
  end;

  TCcMemoryRow = class
  private
    FCount: Integer;
    FValues: Variant;
    FMemoryTable: TCcMemoryTable;
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; Val: Variant);
  public
    property Count: Integer read FCount;
    property Value[index: Integer]: Variant read GetValue write SetValue;
    procedure CopyFrom(row: TCcMemoryRow);
    constructor Create(table: TCcMemoryTable);
    destructor Destroy; override;
  end;

  TCcMemoryTable = class(TComponent)
  private
    FRecords: TList{$IFDEF CC_D2K12}<TCcMemoryRow>{$ENDIF};
    FCurrentRecNo: Integer;
    FBof: Boolean;
    FEof: Boolean;
    FFields: TCcMemoryFields;
    FActive: Boolean;
    FCanCancelEdits: Boolean;
    FEditing: Boolean;
    FCurrentEditingRow: TCcMemoryRow;
    FAppending: Boolean;

    procedure SetRecNo(val: Integer);
    function GetRecNo: Integer;
    function GetRecordCount: Integer;
    function GetCurrentRow: TCcMemoryRow;
    procedure SetActive(const Value: Boolean);
  protected
    property CurrentRow: TCcMemoryRow read GetCurrentRow;
  public
    procedure EditCurrentRecord;
    procedure AddNewRecord;
    procedure CancelChanges;
    procedure PostChanges;

    procedure CheckInactive;
    procedure CheckActive;
    property Eof: Boolean read FEof;
    property Bof: Boolean read FBof;
    procedure Clear;
    procedure Append;
    procedure Next;
    procedure Prior;
    procedure Last;
    procedure First;
    procedure Delete;
    property RecordCount: Integer read GetRecordCount;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property fields: TCcMemoryFields read FFields;
    function FieldByName(cName: string): TCcMemoryField;
    function FindField(cName: string): TCcMemoryField;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
    procedure CopyStructure(Source: TCcMemoryTable);overload;
    procedure LoadRow(Source: TCcMemoryTable; lTrimCharFields, lEmptyStringsToNull: Boolean);overload;
    procedure CopyStructure(Source: TCcQuery);overload;
    procedure LoadRow(Source: TCcQuery; lTrimCharFields, lEmptyStringsToNull: Boolean);overload;
    function LoadFromDataSet(Source: TCcQuery; lTrimCharFields, lCopyStructure,
      lEmptyStringsToNull: Boolean): Integer;
    property CanCancelEdits:Boolean read FCanCancelEdits write FCanCancelEdits;
  end;

{$IFNDEF FPC}
{$IFNDEF NEXTGEN}

  // TCcDataSet is a TDataset descendant encapsulating several TCcQuery objects
  // (SQL, SQLUpdate, SQLInsert, SQLDelete, SQLRefresh) and enabling a read-write access to data
  // through a TCcConnection. If you set only the SQL property, the dataset will be read-only.
  TCcDataSet = class(TCcMemoryData)
  private
    FOldConnectLost: TCcExceptionNotifyEvent;
    FConnection: TCcConnection;
    FSQLUpdate: TStrings;
    FSQLDelete: TStrings;
    FSQLInsert: TStrings;
    FSQLRefresh: TStrings;
    FSQL: TStrings;
    OldRecord: TCcRecord;
    lLoadingData: Boolean;
    lRefreshingRow: Boolean;
    FRowsAffected: Integer;
    procedure ConnectionLost(Sender: TObject; var RaiseException: Boolean);
    procedure LoadRow(qry: TCcQuery; lAppend: Boolean);
    procedure SetSQLRefresh(const Value: TStrings);
    procedure ExecQuery(qry: TCcQuery);
    procedure SetConnection(const Value: TCcConnection);
    procedure SetSQL(const Value: TStrings);
    procedure SetSQLDelete(const Value: TStrings);
    procedure SetSQLInsert(const Value: TStrings);
    procedure SetSQLUpdate(const Value: TStrings);
    function GetSQLDelete: TStrings;
    function GetSQLInsert: TStrings;
    function GetSQLRefresh: TStrings;
    function GetSQLUpdate: TStrings;
    function GetSQL: TStrings;
  protected
    FSelectQuery: TCcQuery;
    FInsertQuery: TCcQuery;
    FUpdateQuery: TCcQuery;
    FDeleteQuery: TCcQuery;
    FRefreshQuery: TCcQuery;
    procedure InternalInitFieldDefs; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalPost; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure DoAfterPost; override;
    procedure DoAfterOpen; override;

    procedure DoAfterEdit; override;
    procedure DoAfterInsert; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforePost; override;
    procedure DoOnNewRecord; override;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    property SelectQuery: TCcQuery read FSelectQuery;
    property InsertQuery: TCcQuery read FInsertQuery;
    property UpdateQuery: TCcQuery read FUpdateQuery;
    property DeleteQuery: TCcQuery read FDeleteQuery;
    property RefreshQuery: TCcQuery read FRefreshQuery;
    property RowsAffected: Integer read FRowsAffected;
    procedure CopyCurrentRecord(DestRecord: TCcRecord);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SQL: TStrings read GetSQL write SetSQL;
    property SQLDelete: TStrings read GetSQLDelete write SetSQLDelete;
    property SQLInsert: TStrings read GetSQLInsert write SetSQLInsert;
    property SQLRefresh: TStrings read GetSQLRefresh write SetSQLRefresh;
    property SQLUpdate: TStrings read GetSQLUpdate write SetSQLUpdate;
    property Connection: TCcConnection read FConnection write SetConnection;
    property Active;
  end;
  {$ENDIF}
  {$ENDIF FPC}

implementation

uses Math {$IFNDEF NEXTGEN}{$IFDEF CC_D2K14}, VCL.Forms{$ELSE} {$IFNDEF FPC}, Forms{$ENDIF}{$ENDIF}{$ENDIF} {$IFDEF CC_UseVariants} ,Variants{$ENDIF};

{$IFNDEF NEXTGEN}
{$IFNDEF FPC}
type

  TCcBookmarkData = Integer;
  PCcMemBookmarkInfo = ^TCcMemBookmarkInfo;

  TCcMemBookmarkInfo = record
    BookmarkData: TCcBookmarkData;
    BookmarkFlag: TBookmarkFlag;
  end;

{ TCcDataSet }

constructor TCcDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FRowsAffected := -1;
  FSelectQuery := TCcQuery.Create(Self);
  FSelectQuery.SelectStatement := True;
  FUpdateQuery := TCcQuery.Create(Self);
  FInsertQuery := TCcQuery.Create(Self);
  FDeleteQuery := TCcQuery.Create(Self);
  FRefreshQuery := TCcQuery.Create(Self);
  FRefreshQuery.SelectStatement := True;

  FSQL := TStringList.Create;
  FSQLUpdate := TStringList.Create;
  FSQLInsert := TStringList.Create;
  FSQLDelete := TStringList.Create;
  FSQLRefresh := TStringList.Create;
  OldRecord := TCcRecord.Create;
end;

destructor TCcDataSet.Destroy;
begin
  FSelectQuery.Free;
  FUpdateQuery.Free;
  FInsertQuery.Free;
  FDeleteQuery.Free;
  FRefreshQuery.Free;

  FSQL.Free;
  FSQLUpdate.Free;
  FSQLInsert.Free;
  FSQLDelete.Free;
  FSQLRefresh.Free;
  OldRecord.Free;
  inherited;
end;

procedure TCcDataSet.LoadRow(qry: TCcQuery; lAppend: Boolean);
var
  I: Integer;
  cFieldName: string;
begin
  if not qry.Active then
    Exit;
  DisableControls;
  lLoadingData := True;
  try
    if lAppend then
      Append
    else
      Edit;

    for I := 0 to qry.FieldCount - 1 do
    begin
      cFieldName := qry.FieldByIndex[I].FieldName;
      if FindField(cFieldName) <> nil then
      begin
//        if qry.FieldByIndex[I].DataType = ftString then
//          FieldByName(cFieldName).AsString := Trim(qry.FieldByIndex[I].AsString)
//        else
            FieldByName(cFieldName).Value := qry.FieldByIndex[I].Value;
      end;
    end;
    Post;
  finally
    if State <> dsBrowse then
      Cancel;
    EnableControls;
    lLoadingData := False;
  end;
end;

procedure TCcDataSet.DoAfterOpen;
begin
  try
    while not FSelectQuery.Eof do
    begin
      LoadRow(FSelectQuery, True);
      FSelectQuery.Next;
    end;
  finally
    First;
  end;
  inherited;
end;

procedure TCcDataSet.InternalCancel;
begin
  inherited;
  OldRecord.Clear;
end;

procedure TCcDataSet.InternalClose;
begin
  inherited;
  FSelectQuery.Close;
end;

procedure TCcDataSet.InternalDelete;
begin
  if not lLoadingData then
    ExecQuery(FDeleteQuery);
  inherited;
end;

procedure TCcDataSet.InternalEdit;
begin
  inherited;
  if not lLoadingData then
    CopyCurrentRecord(OldRecord);
end;

procedure TCcDataSet.InternalInitFieldDefs;
var
  lWasConnected: Boolean;
  lWasActive: Boolean;
  I: Integer;
  f: TCcField;
begin
  if not Assigned(Connection) then
    Exit;

  lWasConnected := Connection.Connected;
  lWasActive := FSelectQuery.Active;
  try
    if (csDesigning in ComponentState) then
      // Open connection if it's not open
      Connection.Connected := True;

    // Execute query
    if not FSelectQuery.Active then
      FSelectQuery.Exec;

    FieldDefs.BeginUpdate;
    try
      FieldDefs.Clear;
      for I := 0 to FSelectQuery.FieldCount - 1 do
      begin
        f := FSelectQuery.FieldByIndex[I];
        with TFieldDef.Create(FieldDefs, f.FieldName,
          f.DataType, f.Size, False, I + 1) do
          InternalCalcField := False;
      end;
    finally
      FieldDefs.EndUpdate;
    end
  finally
    Connection.Connected := lWasConnected;
    FSelectQuery.Active := lWasActive;
  end;
end;

procedure TCcDataSet.InternalInsert;
begin
  inherited;
  if not lLoadingData then
    CopyCurrentRecord(OldRecord);
end;

procedure TCcDataSet.CopyCurrentRecord(DestRecord: TCcRecord);
var
  I: Integer;
begin
  if not Assigned(DestRecord) then
    raise Exception.Create('Destination record not initialized');
  DestRecord.Clear;
  for I := 0 to FieldCount - 1 do
    DestRecord[fields[I].FieldName].Value := fields[I].Value;
end;

procedure TCcDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if not Assigned(Connection) then
    raise Exception.Create('Database connection not assigned!');
  FSelectQuery.Exec;
  InternalInitFieldDefs;
  inherited OpenCursor(InfoQuery);
end;

procedure TCcDataSet.InternalOpen;
begin
  inherited;
end;

procedure TCcDataSet.InternalPost;
var
  qry: TCcQuery;
begin
  if not lLoadingData then
  begin
    if State = dsInsert then
      qry := FInsertQuery
    else
      qry := FUpdateQuery;

    if not qry.Connection.InTransaction then
      qry.Connection.StartTransaction;

    // Load parameters and execute query
    ExecQuery(qry);

    // Post record to memory table
    inherited;

    // Clear old record version, so as to avoid possible errors
    // if the user puts an 'OLD_' parameter where he shouldn't
    OldRecord.Clear;
  end
  else
    inherited;
end;

procedure TCcDataSet.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if AOperation = opRemove then
  begin
    if AComponent = Connection then
      SetConnection(nil);
  end;
  inherited;
end;

procedure TCcDataSet.ExecQuery(qry: TCcQuery);
var
  I: Integer;
  f: TField;
  cFieldName: string;
begin
  // If the SQL is empty, it means that the operation is not permitted
  // Simply abort so that nothing will happen, don't raise any exception
  if Trim(qry.SQL.Text) = '' then
    Abort;

  qry.Close;
  for I := 0 to qry.ParamCount - 1 do
  begin
    cFieldName := UpperCase(Trim(qry.ParamByIndex[I].FieldName));
    f := FindField(cFieldName);
    if f <> nil then
      qry.ParamByIndex[I].Value := f.Value
    else if Copy(cFieldName, 1, 4) = 'OLD_' then
    begin
      cFieldName := Copy(cFieldName, 5, Length(cFieldName));
      if OldRecord.FieldExists(cFieldName) then
        qry.ParamByIndex[I].Value := OldRecord[cFieldName].Value;
    end;
  end;
  qry.Exec;
  if qry <> FRefreshQuery then
    FRowsAffected := qry.RowsAffected;
end;

procedure TCcDataSet.SetConnection(const Value: TCcConnection);
begin
  if FConnection = Value then
    Exit;

  if Assigned(FConnection) then
    FConnection.OnConnectionLost := FOldConnectLost;

  FConnection := Value;

  if Assigned(FConnection) then
  begin
    FOldConnectLost := FConnection.OnConnectionLost;
    FConnection.OnConnectionLost := ConnectionLost;
  end;

  FSelectQuery.Connection := Value;
  FUpdateQuery.Connection := Value;
  FInsertQuery.Connection := Value;
  FDeleteQuery.Connection := Value;
  FRefreshQuery.Connection := Value;
end;

procedure TCcDataSet.SetSQL(const Value: TStrings);
begin
  Close;
  FSelectQuery.SQL.Assign(Value);
  // SetQuerySQL(FSelectQuery, FSQL, Value);
end;

procedure TCcDataSet.SetSQLRefresh(const Value: TStrings);
begin
  FRefreshQuery.SQL.Assign(Value);
  // SetQuerySQL(FRefreshQuery, FSQLRefresh, Value);
end;

procedure TCcDataSet.SetSQLDelete(const Value: TStrings);
begin
  FDeleteQuery.SQL.Assign(Value);
  // SetQuerySQL(FDeleteQuery, FSQLDelete, Value);
end;

procedure TCcDataSet.SetSQLInsert(const Value: TStrings);
begin
  FInsertQuery.SQL.Assign(Value);
  // SetQuerySQL(FInsertQuery, FSQLInsert, Value);
end;

procedure TCcDataSet.SetSQLUpdate(const Value: TStrings);
begin
  FUpdateQuery.SQL.Assign(Value);
  // SetQuerySQL(FUpdateQuery, FSQLUpdate, Value);
end;

function TCcDataSet.GetSQLDelete: TStrings;
begin
  Result := FDeleteQuery.SQL;
end;

function TCcDataSet.GetSQLInsert: TStrings;
begin
  Result := FInsertQuery.SQL;
end;

function TCcDataSet.GetSQLRefresh: TStrings;
begin
  Result := FRefreshQuery.SQL;
end;

function TCcDataSet.GetSQLUpdate: TStrings;
begin
  Result := FUpdateQuery.SQL;
end;

function TCcDataSet.GetSQL: TStrings;
begin
  Result := FSelectQuery.SQL;
end;

procedure TCcDataSet.DoAfterPost;
begin
  // Refresh current row
  if not lLoadingData then
  begin
    if not lRefreshingRow then
    begin
      lRefreshingRow := True;
      try
        if Trim(FRefreshQuery.SQL.Text) <> '' then
        begin
          ExecQuery(FRefreshQuery);
          if (FRefreshQuery.RecordCount > 0) then
            LoadRow(FRefreshQuery, False);
        end;
      finally
        lRefreshingRow := False;
      end;
    end;
    inherited;
  end;
end;

procedure TCcDataSet.DoAfterEdit;
begin
  if not lLoadingData then
    inherited;
end;

procedure TCcDataSet.DoAfterInsert;
begin
  if not lLoadingData then
    inherited;
end;

procedure TCcDataSet.DoBeforeEdit;
begin
  if not lLoadingData then
    inherited;
end;

procedure TCcDataSet.DoBeforeInsert;
begin
  if not lLoadingData then
    inherited;
end;

procedure TCcDataSet.DoBeforePost;
begin
  if not lLoadingData then
    inherited;
end;

procedure TCcDataSet.DoOnNewRecord;
begin
  if not lLoadingData then
    inherited;
end;

procedure TCcDataSet.ConnectionLost(Sender: TObject;
  var RaiseException: Boolean);
begin
  Close;
  if Assigned(FOldConnectLost) then
    FOldConnectLost(Sender, RaiseException);
end;
{$ENDIF}
{$ENDIF}
{ TCcValue }

procedure TCcValue.Assign(Val: TCcValue);
var
  I: Integer;
begin
  if Val.ValueType = vtArray then
  begin
    AsArray.Clear;
    for I := 0 to Val.AsArray.Count - 1 do
      AsArray.Add.Assign(Val.AsArray[I]);
  end
  else if Val.ValueType = vtField then
  begin
    AsField.FieldName := Val.AsField.FieldName;
    AsField.DataType := Val.AsField.DataType;
    AsField.Value := Val.AsField.Value;
    AsField.IsNull := Val.AsField.IsNull;
    AsField.Size := Val.AsField.Size;
  end;
end;

procedure TCcValue.Clear;
begin
  if FType = vtArray then
    FArray.Free
  else if FType = vtField then
    FField.Free;

  FType := vtNull;
end;

constructor TCcValue.Create;
begin
  FType := vtNull;
end;

destructor TCcValue.Destroy;
begin
  Clear;
  if FType = vtArray then
    FArray.Free
  else if FType = vtField then
    FField.Free;
  inherited;
end;

function TCcValue.GetAsArray: TCcArray;
begin
  // If no type is set, set type to array
  if FType = vtNull then
    SetAsArray(TCcArray.Create);

  if FType = vtArray then
    Result := FArray
  else
    raise EConvertError.Create('Cannot convert variant type to array!');
end;

function TCcValue.GetAsField: TCcFieldInfo;
begin
  // If no type is set, set type to variant
  if FType = vtNull then
    SetAsField(TCcFieldInfo.Create);

  if FType = vtField then
    Result := FField
  else
    raise EConvertError.Create('Cannot convert array type to variant!');
end;

function TCcValue.GetValue: Variant;
begin
  Result := GetAsField.Value;
end;

procedure TCcValue.SetAsArray(const Value: TCcArray);
begin
  FType := vtArray;
  FArray := Value;
end;

procedure TCcValue.SetAsField(const Value: TCcFieldInfo);
begin
  FType := vtField;
  FField := Value;
end;

procedure TCcValue.SetValue(const Val: Variant);
var
  vType: Integer;
begin
  vType := VarType(Val);
  case vType of
    varSmallint, varByte, varInteger:
      AsField.DataType := ftInteger;
    varDouble, varCurrency:
      AsField.DataType := ftFloat;
{$IFDEF CC_D6}
    varWord: // Added by Kick Martens
      AsField.DataType := ftWord;
{$ENDIF}
{$IFDEF CC_D2K9}
    varLongWord: // Added by Kick Martens
      AsField.DataType := ftLongWord;
{$ENDIF}
    varString {$IFDEF CC_D2K9} , varUString {$ENDIF}:
      AsField.DataType := ftString;
    // varUString:
    // AsField.DataType := ftWideString;
    varBoolean:
      AsField.DataType := ftBoolean;
    varDate:
      AsField.DataType := ftDateTime;
  else
    AsField.DataType := ftUnknown;
  end;
  AsField.IsNull := (VarIsNull(Val) {$IFDEF CC_D6} or VarIsClear(Val){$ENDIF} or VarIsEmpty(Val));
  AsField.Value := Val;
end;

procedure TCcValue.SetValueAsType(Val: Variant; DataType: TFieldType);
begin
  Value := Val;
  AsField.DataType := DataType;

  /// /  If the value is null, don't change the datatype
  // if AsField.DataType <> ftNull then
  // AsField.DataType := dataType;
end;

{ TCcFieldList }

procedure TCcFieldList.Add(field: TCcFieldInfo);
begin
  FList.AddObject(field.FieldName, field);
end;

procedure TCcFieldList.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    if Assigned(FList.Objects[I]) then
      FList.Objects[I].Free;
    FList.Delete(I);
  end;
end;

constructor TCcFieldList.Create;
begin
  FList := TStringList.Create;
end;

destructor TCcFieldList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCcFieldList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCcFieldList.GetFieldInfo(Index: Integer): TCcFieldInfo;
begin
  Result := TCcFieldInfo(FList.Objects[index]);
end;

function TCcFieldList.GetFieldInfoByName(FieldName: string): TCcFieldInfo;
begin
  Result := GetFieldInfo(FList.IndexOf(FieldName));
end;

{ TCcArray }

function TCcArray.Add: TCcValue;
begin
  Result := TCcValue.Create;
  FValues.AddObject(IntToStr(FValues.Count), Result);
end;

procedure TCcArray.AddValue(Val: TCcValue);
begin
  if Assigned(Val) and (Val.ValueType <> vtNull) then
    Add.Assign(Val);
end;

procedure TCcArray.Clear;
var
  I: Integer;
begin
  for I := 0 to FValues.Count - 1 do
    TCcValue(FValues.Objects[I]).Free;
  FValues.Clear;
end;

constructor TCcArray.Create;
begin
  FValues := TStringList.Create;
end;

destructor TCcArray.Destroy;
begin
  Clear;
  FValues.Free;
  inherited;
end;

function TCcArray.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TCcArray.GetValue(Index: Integer): TCcValue;
begin
  if (index <> -1) and (index < FValues.Count) then
    Result := TCcValue(FValues.Objects[index])
  else
    Result := nil;
end;

{ TCcRecord }

function TCcRecord.Add(FieldName: string): TCcValue;
begin
  Result := TCcValue.Create;
  Result.AsField.FieldName := FieldName;
  FValues.AddObject(FieldName, Result);
end;

procedure TCcRecord.Clear;
var
  I: Integer;
begin
  for I := 0 to FValues.Count - 1 do
    TCcValue(FValues.Objects[I]).Free;
  FValues.Clear;
end;

constructor TCcRecord.Create;
begin
  FValues := TStringList.Create;
end;

destructor TCcRecord.Destroy;
begin
  Clear;
  FValues.Free;
  inherited;
end;

function TCcRecord.FieldExists(cFieldName: string): Boolean;
begin
  Result := (FValues.IndexOf(cFieldName) > -1);
end;

function TCcRecord.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TCcRecord.GetFieldName(Index: Integer): string;
begin
  Result := FValues[index];
end;

function TCcRecord.GetValue(FieldName: string): TCcValue;
var
  nIndex: Integer;
begin
  nIndex := FValues.IndexOf(FieldName);
  if nIndex <> -1 then
    Result := TCcValue(FValues.Objects[nIndex])
  else
    Result := Add(FieldName);
end;

{ TCcMemoryTable }

procedure TCcMemoryTable.AddNewRecord;
begin
  if not FEditing then
  begin
    FCurrentEditingRow := TCcMemoryRow.Create(Self);
    FAppending := True;
    FEditing := True;
  end;
end;

procedure TCcMemoryTable.Append;
begin
  FCurrentRecNo := FRecords.Add(TCcMemoryRow.Create(Self));
  FEof := False;
  FBof := False;
end;

procedure TCcMemoryTable.Clear;
var
  I: Integer;
begin
  for I := FRecords.Count - 1 downto 0 do
    {$IFDEF NEXTGEN}
    TCcMemoryRow(FRecords[I]).DisposeOf;
    {$ELSE}
    TCcMemoryRow(FRecords[I]).Free;
    {$ENDIF}
  FRecords.Clear;

  FEof := True;
  FBof := True;
end;

constructor TCcMemoryTable.Create(AOwner: TComponent);
begin
  inherited;
  FRecords := TList{$IFDEF CC_D2K12}<TCcMemoryRow>{$ENDIF}.Create;
  FFields := TCcMemoryFields.Create(Self);
  FActive := False;
  FCurrentRecNo := -1;
end;

procedure TCcMemoryTable.Delete;
begin
  if FEditing then
    CancelChanges
  else begin
    {$IFDEF NEXTGEN}
    TCcMemoryRow(FRecords[FCurrentRecNo]).DisposeOf;
    {$ELSE}
    TCcMemoryRow(FRecords[FCurrentRecNo]).Free;
    {$ENDIF}
    FRecords.Delete(FCurrentRecNo);
  end;
end;

destructor TCcMemoryTable.Destroy;
var
  I: Integer;
begin
  Clear;
  FActive := False;
  FRecords.Free;
  FFields.Free;
  inherited;
end;

procedure TCcMemoryTable.EditCurrentRecord;
begin
  if not FEditing then
  begin
    FCurrentEditingRow := TCcMemoryRow.Create(Self);
    FCurrentEditingRow.CopyFrom(CurrentRow);
    FAppending := False;
    FEditing := True;
  end;
end;

function TCcMemoryTable.FieldByName(cName: string): TCcMemoryField;
begin
  Result := FFields.FieldByName(cName)
end;

function TCcMemoryTable.FindField(cName: string): TCcMemoryField;
begin
  Result := FFields.FindField(cName)
end;

procedure TCcMemoryTable.First;
begin
  FCurrentRecNo := 0;
  FBof := True;
  if RecordCount > 0 then
    FEof := False;
end;

function TCcMemoryTable.GetCurrentRow: TCcMemoryRow;
begin
  if FEditing then
    Result := FCurrentEditingRow
  else
    Result := TCcMemoryRow(FRecords[FCurrentRecNo]);
end;

function TCcMemoryTable.GetRecNo: Integer;
begin
  Result := FCurrentRecNo;
end;

procedure TCcMemoryTable.SetRecNo(val: Integer);
begin
  FCurrentRecNo := val;
end;

function TCcMemoryTable.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

procedure TCcMemoryTable.Last;
begin
  FCurrentRecNo := RecordCount - 1;
  FEof := True;
  if FCurrentRecNo > 0 then
    FBof := False;
end;

procedure TCcMemoryTable.Next;
begin
  if FCurrentRecNo >= RecordCount - 1 then
    FEof := True
  else
    Inc(FCurrentRecNo);

  if FCurrentRecNo > 0 then
    FBof := False;
end;

procedure TCcMemoryTable.PostChanges;
begin
  if FEditing then
  begin
    if FAppending then
      FCurrentRecNo := FRecords.Add(FCurrentEditingRow)
    else begin
      {$IFDEF NEXTGEN}
      TCcMemoryRow(FRecords[FCurrentRecNo]).DisposeOf;
      {$ELSE}
      TCcMemoryRow(FRecords[FCurrentRecNo]).Free;
      {$ENDIF}

      FRecords.Delete(FCurrentRecNo);
      FRecords.Insert(FCurrentRecNo, FCurrentEditingRow);
    end;
    FAppending := False;
    FEditing := False;
  end;
end;

procedure TCcMemoryTable.Prior;
begin
  if FCurrentRecNo = 0 then
    FBof := True
  else
    Dec(FCurrentRecNo);

  if RecordCount > 0 then
    FEof := False;
end;

procedure TCcMemoryTable.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    Clear;
    FActive := Value;
  end;
end;

{ TCcRow }

procedure TCcMemoryRow.CopyFrom(row: TCcMemoryRow);
var
  I: Integer;
begin
  for I := 0 to min(count, row.count) -1 do
    FValues[I] := row.Value[i];
end;

constructor TCcMemoryRow.Create(table: TCcMemoryTable);
var
  I: Integer;
begin
  FMemoryTable := table;
  FCount := table.fields.Count;
  FValues := VarArrayCreate([0, Count - 1], varVariant);
  for I := 0 to Count - 1 do
    FValues[I] := Null;
end;

destructor TCcMemoryRow.Destroy;
begin
  VarClear(FValues);
  inherited;
end;

function TCcMemoryRow.GetValue(Index: Integer): Variant;
begin
  Result := FValues[index];
end;

procedure TCcMemoryRow.SetValue(Index: Integer; Val: Variant);
begin
  FValues[index] := Val;
end;

{ TCcMemoryFields }

function TCcMemoryFields.Add(cFieldName: String): TCcMemoryField;
var
  field: TCcMemoryField;
  I: Integer;
  newRow: TCcMemoryRow;
begin
//  FTable.CheckInactive;

  field := TCcMemoryField.Create(Self);
  field.FIndex := FFields.Add(field);
  field.FFieldName := cFieldName;
  FFieldsByName.AddObject(cFieldName, field);
  Result := field;

  for I := 0 to FTable.RecordCount -1 do begin
    newRow := TCcMemoryRow.Create(FTable);
    newRow.CopyFrom(TCcMemoryRow(FTable.FRecords[i]));
    VarClear(TCcMemoryRow(FTable.FRecords[I]).FValues);
    FTable.FRecords[I] := newRow;
  end;
end;

procedure TCcMemoryTable.CheckInactive;
begin
  if Active then
    raise Exception.Create('Cannot perform this operation on an open memory table');
end;

procedure TCcMemoryTable.CancelChanges;
begin
  if FEditing then
  begin
    {$IFDEF NEXTGEN}
    FCurrentEditingRow.DisposeOf;
    {$ELSE}
    FCurrentEditingRow.Free;
    {$ENDIF}
    FEditing := False;
  end;
end;

procedure TCcMemoryTable.CheckActive;
begin
  if not Active then
    raise Exception.Create('Cannot perform this operation on an closed memory table');
end;

constructor TCcMemoryFields.Create(table: TCcMemoryTable);
begin
  FTable := table;
  FFieldsByName := TStringList.Create;
  FFieldsByName.Sorted := True;
  FFields := TList{$IFDEF CC_D2K12}<TCcMemoryField>{$ENDIF}.Create;
end;

procedure TCcMemoryFields.Delete(Index: Integer);
var
  field: TCcMemoryField;
begin
  FTable.CheckInactive;

  field := TCcMemoryField(FFields[index]);
  FFieldsByName.Delete(FFieldsByName.IndexOf(field.FieldName));
  FFields.Delete(index);

  {$IFDEF NEXTGEN}
  field.DisposeOf;
  {$ELSE}
  field.Free;
  {$ENDIF}
end;

procedure TCcMemoryFields.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  Delete(I);
end;

destructor TCcMemoryFields.Destroy;
var
  field: TCcMemoryField;
  I: Integer;
begin
  Clear;
  FFieldsByName.Free;
  FFields.Free;
  inherited;
end;

function TCcMemoryFields.FindField(name: string): TCcMemoryField;
begin
  if FFieldsByName.IndexOf(name) <> -1 then
    Result := TCcMemoryField(FFieldsByName.Objects[FFieldsByName.IndexOf(name)])
  else
    Result := nil;
end;

function TCcMemoryFields.FieldByName(name: string): TCcMemoryField;
begin
  Result := FindField(name);
  if Result = nil then
    Result := Add(name);
//    raise Exception.Create('Field "' + name + '" not found in memory table');
end;

function TCcMemoryFields.GetCount: Integer;
begin
  Result := FFields.Count;
end;

function TCcMemoryFields.GetFieldByIndex(Index: Integer): TCcMemoryField;
begin
  Result := TCcMemoryField(FFields[index]);
end;

{ TCcMemoryField }

procedure TCcMemoryField.Clear;
begin
  IsNull := True;
  Value := Null;
end;

constructor TCcMemoryField.Create(fields: TCcMemoryFields);
begin
  FFields := fields;
  FVisible := True;
end;

function TCcMemoryField.GetAsString: string;
begin
  if IsNull then
    Result := ''
  else
    Result := Value;
end;

function TCcMemoryField.GetIsNull: Boolean;
begin
  Result := FIsNull or (VarIsNull(Value) {$IFDEF CC_D6} or VarIsClear(Value) {$ENDIF CC_D6});
end;

function TCcMemoryField.GetValue: Variant;
begin
  FFields.FTable.CheckActive;
  if (FFields.FTable.RecordCount = 0) then
    Result := Null
  else
    Result := FFields.FTable.CurrentRow.Value[index];
end;

procedure TCcMemoryField.SetAsString(const Val: string);
begin
  Value := Val;
end;

procedure TCcMemoryField.SetIsNull(const Value: Boolean);
begin
  FIsNull := Value;
end;

procedure TCcMemoryField.SetValue(const Value: Variant);
begin
  FFields.FTable.CheckActive;
  FFields.FTable.CurrentRow.Value[index] := Value;
end;

procedure TCcMemoryTable.CopyStructure(Source: TCcMemoryTable);
var
  I: Integer;
begin
  CheckInactive;
  if (Source = nil) then
    Exit;
  for I := fields.Count - 1 downto 0 do
    fields.Delete(I);

  for I := 0 to Source.Fields.Count - 1 do
    with fields.Add(Source.Fields.FieldByIndex[I].FieldName) do
    begin
      DataType := Source.Fields.FieldByIndex[I].DataType;
      Size := Source.Fields.FieldByIndex[I].Size
    end;
end;

procedure TCcMemoryTable.LoadRow(Source:TCcMemoryTable; lTrimCharFields, lEmptyStringsToNull: Boolean);
var
  I: Integer;
  f: TCcMemoryField;
  str: String;
begin
  Append;
  for I := 0 to Source.Fields.Count - 1 do
  begin
    f := FieldByName(Source.Fields.FieldByIndex[I].FieldName);
    if Source.Fields.FieldByIndex[I].IsNull then
      Continue
    else if (Source.Fields.FieldByIndex[I].DataType = ftString) or (Source.Fields.FieldByIndex[I].DataType = ftMemo) then
    begin
      str := Source.Fields.FieldByIndex[I].AsString;
      if lTrimCharFields then
        str := Trim(str);

      f.AsString := str;
    end
    else
    begin
      try
        if lEmptyStringsToNull and (Source.Fields.FieldByIndex[I].AsString = '') then
          f.Clear
        else
        begin
          f.Value := Source.Fields.FieldByIndex[I].Value;
        end;
      except
        on E: Exception do
          raise Exception.Create(f.FieldName + ' = ' + Source.Fields.FieldByIndex[I].AsString + #13 + #10 + E.Message);
      end;
    end
  end;
end;

procedure TCcMemoryTable.CopyStructure(Source: TCcQuery);
var
  I: Integer;
begin
  CheckInactive;
  if (Source = nil) then
    Exit;
  for I := fields.Count - 1 downto 0 do
    fields.Delete(I);

  for I := 0 to Source.FieldCount - 1 do
    with fields.Add(Source.FieldByIndex[I].FieldName) do
    begin
      DataType := Source.FieldByIndex[I].DataType;
      Size := Source.FieldByIndex[I].Size
    end;
end;

procedure TCcMemoryTable.LoadRow(Source:TCcQuery; lTrimCharFields, lEmptyStringsToNull: Boolean);
var
  I: Integer;
  f: TCcMemoryField;
  str: String;
begin
  Append;
  for I := 0 to Source.FieldCount - 1 do
  begin
    f := FieldByName(Source.FieldByIndex[I].FieldName);
    if Source.FieldByIndex[I].IsNull then
      Continue
    else if (Source.FieldByIndex[I].DataType = ftString) or (Source.FieldByIndex[I].DataType = ftMemo) then
    begin
      str := Source.FieldByIndex[I].AsString;
      if lTrimCharFields then
        str := Trim(str);

      f.AsString := str;
    end
    else
    begin
      try
        if lEmptyStringsToNull and (Source.FieldByIndex[I].AsString = '') then
          f.Clear
        else
        begin
          f.Value := Source.FieldByIndex[I].Value;
        end;
      except
        on E: Exception do
          raise Exception.Create(f.FieldName + ' = ' + Source.FieldByIndex[I].AsString + #13 + #10 + E.Message);
      end;
    end
  end;
end;

function TCcMemoryTable.LoadFromDataSet(Source: TCcQuery; lTrimCharFields: Boolean; lCopyStructure: Boolean; lEmptyStringsToNull: Boolean): Integer;
var
  I: Integer;
  f: TCcMemoryField;
  str: string;
begin
  Result := 0;
  if not Source.Active then
    Exit;

  Active := False;
  Clear;
  if lCopyStructure then
    CopyStructure(Source);

  Active := True;
  try
    while not Source.Eof do
    begin
      LoadRow(Source, lTrimCharFields, lEmptyStringsToNull);
      Inc(Result);
      Source.Next;
    end;
  finally
    First;
  end;
end;

end.
