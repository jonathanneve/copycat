unit CcComparer;

{$I CC.INC}

interface

uses Classes, CcProviders, CcKeys, CcDb, System.Generics.Collections;

type

TCcComparerMetaDataType = (mdtTables, mdtSequences, mdtViews, mdtProcedures, mdtFunctions, mdtTypes, mdtOthers);
TCcComparerMetaDataTypes = set of TCcComparerMetaDataType;
TCcComparerOrder = (coAscending, coDescending);
TCcComparerRowAction = (raIgnore, raApplySourceToDest, raApplyDestToSource);

TCcComparer = class;
TCcComparerRowEvent = procedure(Sender: TCcComparer; TableName: String; RowKeys: TCcKeyRing; SourceFields, DestFields: TCcQuery; var Action: TCcComparerRowAction) of object;
TCcComparerSubtableEvent = procedure(Sender: TCcComparer; TableName, SubTableName: String; MasterSourceFields, MasterDestFields: TCcQuery; var Action: TCcComparerRowAction) of object;

TCcComparerSubTable = class (TCollectionItem)
  private
    FTableName: String;
    FGroupFields: String;
    FMasterGroupFields: String;
    FActive: Boolean;
  published
    constructor Create(Collection: TCollection); override;
    property TableName: String read FTableName write FTableName;
    property GroupFields: String read FGroupFields write FGroupFields;
    property MasterGroupFields: String read FMasterGroupFields write FMasterGroupFields;
    property Active : Boolean read FActive write FActive default True;
end;

TCcComparerSubTables = class (TCollection)
 private
   FOwner: TPersistent;
 	 function GetItem(Index: Integer): TCcComparerSubTable;
 protected
   function GetOwner: TPersistent; override;
 public
   constructor Create(AOwner: TPersistent);
	 function FindTable(tableName: String): TCcComparerSubTable;
	 function Add: TCcComparerSubTable;
	 property Item[Index: Integer]: TCcComparerSubTable read GetItem; default;
end;

TCcComparerTable = class (TCollectionItem)
  private
    FTableName: String;
    FOrder: TCcComparerOrder;
    FNumberOfRows: Integer;
    FOrderField: String;
    FSubtables: TCcComparerSubTables;
    FActive: Boolean;
    procedure SetSubtables(const Value: TCcComparerSubTables);
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy;override;
    property TableName: String read FTableName write FTableName;
    property Subtables: TCcComparerSubTables read FSubtables write SetSubtables;
    property OrderFields: String read FOrderField write FOrderField;
    property NumberOfRows: Integer read FNumberOfRows write FNumberOfRows;
    property Order: TCcComparerOrder read FOrder write FOrder;
    property Active : Boolean read FActive write FActive default True;
end;

TCcComparerTables = class (TCollection)
 private
   FOwner: TPersistent;
 	 function GetItem(Index: Integer): TCcComparerTable;
 protected
   function GetOwner: TPersistent; override;
 public
   constructor Create(AOwner: TPersistent);
	 function FindTable(tableName: String): TCcComparerTable;
	 function Add: TCcComparerTable;
	 property Item[Index: Integer]: TCcComparerTable read GetItem; default;
end;

TCcScript = class(TList<String>)
  private
    FTerminator: String;
    function GetScriptText: String;
  public
    property ScriptText: String read GetScriptText;
    property Terminator: String read FTerminator write FTerminator;
end;

TCcComparer = class (TComponent)
  private
    FMetaDataTypes: TCcComparerMetaDataTypes;
    FOnRowMissingInSource: TCcComparerRowEvent;
    FOnRowIdentical: TCcComparerRowEvent;
    FSourceConnection: TCcConnection;
    FTables: TCcComparerTables;
    FCurrentRow: Integer;
    FOnRowChanged: TCcComparerRowEvent;
    FOnProgress: TNotifyEvent;
    FOnRowMissingInDest: TCcComparerRowEvent;
    FDestConnection: TCcConnection;
    FDestScript: TStrings;
    FReadOnly: Boolean;
    FScriptTerminator: String;
    FTableRowNumber: Integer;
    FChangeList: TDictionary<String, TCcMemoryTable>;
    FCurrentTableName: String;
    FOnCompareSubtable: TCcComparerSubtableEvent;
    function GetRowCount: Integer;
    procedure SetDestConnection(const Value: TCcConnection);
    procedure SetSourceConnection(const Value: TCcConnection);
    procedure SetTables(const Value: TCcComparerTables);
    procedure SetScriptTerminator(const Value: String);
    procedure CopyRowToChangeList(q: TCcQuery; db, tablename, operation: String);overload;
    procedure CopyRowToChangeList(m: TCcMemoryTable; db, tablename, operation: String);overload;
    procedure ClearChangeLists;
    function GetScript(db:String): String;
    function GetFieldsSQL(tableName: String; connection: TCcConnection; fields: TCcMemoryFields; cOperationType: string): string;
    function CalcWhereClause(DB: TCcConnection; keys: TCcKeyRing): string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property TableRowNumber: Integer read FTableRowNumber;
    property CurrentRow: Integer read FCurrentRow;
    property RowCount: Integer read GetRowCount;
    property CurrentTableName: String read FCurrentTableName;
    function GetDestScript: String;
    function GetSourceScript: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CompareMetaData;
    procedure CompareData;
    procedure ApplyChanges;
  published
    property SourceConnection: TCcConnection read FSourceConnection write SetSourceConnection;
    property DestConnection: TCcConnection read FDestConnection write SetDestConnection;
    property Tables: TCcComparerTables read FTables write SetTables;
    property MetaDataTypes: TCcComparerMetaDataTypes read FMetaDataTypes write FMetaDataTypes;
    property ScriptTerminator: String read FScriptTerminator write SetScriptTerminator;

    property OnCompareSubtable: TCcComparerSubtableEvent read FOnCompareSubtable write FOnCompareSubtable;
    property OnRowMissingInDest: TCcComparerRowEvent read FOnRowMissingInDest write FOnRowMissingInDest;
    property OnRowMissingInSource: TCcComparerRowEvent read FOnRowMissingInSource write FOnRowMissingInSource;
    property OnRowChanged: TCcComparerRowEvent read FOnRowChanged write FOnRowChanged;
    property OnRowIdentical: TCcComparerRowEvent read FOnRowIdentical write FOnRowIdentical;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
end;


implementation

uses Sysutils, System.StrUtils;

{ TCcComparer }

procedure TCcComparer.ApplyChanges;
begin
//
end;

procedure TCcComparer.ClearChangeLists;
var
  I: Integer;
  key: String;
begin
  for key in FChangeList.Keys do begin
    FChangeList[key].Free;
    FChangeList.Remove(key);
  end;
end;

procedure TCcComparer.CopyRowToChangeList(q: TCcQuery; db: String; tablename: String; operation: String);
var
  list: TCcMemoryTable;
begin
  if FChangeList.ContainsKey(db+'_'+tablename+'_'+operation) then
    list := FChangeList.Items[db+'_'+tablename+'_'+operation]
  else begin
    list := TCcMemoryTable.Create(Self);
    list.CopyStructure(q);
    FChangeList.Add(db+'_'+tablename+'_'+operation, list);
    list.Active := True;
  end;

  list.LoadRow(q, False, False);
end;

procedure TCcComparer.CopyRowToChangeList(m: TCcMemoryTable; db: String; tablename: String; operation: String);
var
  list: TCcMemoryTable;
begin
  if FChangeList.ContainsKey(db+'_'+tablename+'_'+operation) then
    list := FChangeList.Items[db+'_'+tablename+'_'+operation]
  else begin
    list := TCcMemoryTable.Create(Self);
    list.CopyStructure(m);
    FChangeList.Add(db+'_'+tablename+'_'+operation, list);
    list.Active := True;
  end;

  list.LoadRow(m, False, False);
end;

procedure TCcComparer.CompareData;
var
  qSource: TCcQuery;
  qDest: TCcQuery;
  I: Integer;
  q: TCcQuery;
  sourceKeys: TCcKeyRing;
  destKeys: TCcKeyRing;
  Action: TCcComparerRowAction;

  procedure OpenQuery(table: TCcComparerTable; q: TCcQuery);
  var
    sl: TStringList;
    i: Integer;
    cOrder: string;
  begin
    sl := TStringList.Create;
    try
      sl.CommaText := table.OrderFields;
      for I := 0 to sl.Count-1 do
      begin
        if cOrder <> '' then
          cOrder := cOrder + ',';
        cOrder := cOrder + sl[i] + ifthen(table.Order = coDescending, ' desc', '')
      end;
    finally
      sl.free;
    end;

    q.Close;
    q.SQL.Text := 'select * from ' + q.Connection.DBAdaptor.MetaQuote(table.TableName)
      + ' order by ' + cOrder;
    q.Exec;
  end;

  function FindQuery(table:TCcComparerTable) : TCcQuery;

    function CompareField(fieldName: String) : TCcQuery;
    var
      lCompare: Boolean;
    begin
      if (qSource.Field[fieldName].Value = qDest.Field[fieldName].Value) then
        Exit(nil);

      if table.Order = coAscending then
        lCompare := (qSource.Field[fieldName].Value < qDest.Field[fieldName].Value)
      else
        lCompare := (qSource.Field[fieldName].Value > qDest.Field[fieldName].Value);

      if lCompare then
        Result := qSource
      else
        Result := qDest;
    end;

  var
    sl: TStringList;
    I: Integer;
  begin
    Result := nil;
    if sourceKeys.PrimaryKeyValues = destKeys.PrimaryKeyValues then
      Exit;

    sl := TStringList.Create;
    try
      sl.CommaText := table.OrderFields;
      for I := 0 to sl.Count-1 do
      begin
        Result := CompareField(sl[i]);
        if Result <> nil then
          Break;
      end;
    finally
      sl.free;
    end;
  end;

  function QueryFieldsDifferent: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to qSource.FieldCount-1 do begin
      if qSource.FieldByIndex[i].Value <> qDest.Field[qSource.FieldByIndex[i].FieldName].Value then begin
        Result := True;
        Break;
      end;
    end;
  end;

  procedure CopySubTableRows(conn: TCcConnection; db: String; qMaster: TCcQuery; table: TCcComparerTable; operation: String);
  var
    I: Integer;
    subtable: TCcComparerSubTable;
    sl: TStringList;
    slMaster: TStringList;
    cWhere: String;
    qSubTable: TCcQuery;
    J: Integer;
  begin
    for I := 0 to table.Subtables.Count-1 do
    begin
      if not table.Subtables[i].Active then
        Continue;

      subtable := table.Subtables[i];
      sl := TStringList.Create;
      slMaster := TStringList.Create;
      try
        sl.CommaText := subtable.GroupFields;
        slMaster.CommaText := subtable.MasterGroupFields;
        for J := 0 to sl.Count-1 do
        begin
          if cWhere <> '' then
            cWhere := cWhere + ' and ';
          cWhere := cWhere + sl[J] + ' = ' + conn.DBAdaptor.SQLFormatValue(qMaster.Field[slMaster[J]].Value, q.Field[slMaster[J]].DataType);
        end;
      finally
        sl.free;
      end;

      qSubTable := conn.SelectQuery['CopySubTableRows_' + subtable.TableName];
      qSubTable.Close;
      qSubTable.SQL.Text := 'select * from ' + conn.DBAdaptor.MetaQuote(subtable.TableName) + ' where ' + cWhere;
      qSubTable.Exec;
      while not qSubTable.Eof do
      begin
        CopyRowToChangeList(qSubTable, db, subtable.TableName, operation);
        qSubTable.Next;
      end;
    end;
  end;

  procedure CompareSubtables(table: TCcComparerTable; qMaster: TCcQuery);

    function rowExistsInTable(conn: TCcConnection; TableName: String; memTable: TCcMemoryTable; sourceRow: TCcMemoryTable): Boolean;
    var
      J: Integer;
      keys: TCcKeyRing;
      KeysEqual: Boolean;
    begin
      Result := False;
      keys := TCcKeyRing.Create(conn);
      try
        keys.LoadKeysFromDataSet(TableName, sourceRow, '','');

        memTable.First;
        while not memTable.Eof  do
        begin
          KeysEqual := True;
          for J := 0 to keys.Count-1 do
            if memTable.FieldByName(keys[j].KeyName).Value <> keys[j].KeyValue then begin
              KeysEqual := False;
              Break;
            end;

          if KeysEqual then begin
            Result := True;
            Exit;
          end;
          memTable.Next;
        end;
      finally
        keys.Free;
      end;
    end;

      function FieldsDifferent(mTable1, mTable2: TCcMemoryTable): Boolean;
      var
        I: Integer;
      begin
        Result := False;
        for I := 0 to mTable1.fields.Count-1 do begin
          if mTable1.fields.FieldByIndex[i].Value <> mTable2.FieldByName(mTable1.fields.FieldByIndex[i].FieldName).Value then begin
            Result := True;
            Break;
          end;
        end;
      end;

  var
    I: Integer;
    subtable: TCcComparerSubTable;
    sl: TStringList;
    slMaster: TStringList;
    cWhere: String;
    qSubTable: TCcQuery;
    qSubTableDest: TCcQuery;
    memSubTableDest: TCcMemoryTable;
    connOrigin: TCcConnection;
    connDest: TCcConnection;
    memSubTableSource: TCcMemoryTable;
    J: Integer;
  begin
    memSubTableDest := TCcMemoryTable.Create(Self);
    memSubTableSource := TCcMemoryTable.Create(Self);
    try
      for I := 0 to table.Subtables.Count-1 do
      begin
        if not table.Subtables[i].Active then
          Continue;

        subtable := table.Subtables[i];
        sl := TStringList.Create;
        slMaster := TStringList.Create;
        try
          sl.CommaText := subtable.GroupFields;
          slMaster.CommaText := subtable.MasterGroupFields;
          for J := 0 to sl.Count-1 do
          begin
            if cWhere <> '' then
              cWhere := cWhere + ' and ';
            cWhere := cWhere + sl[J] + ' = ' + SourceConnection.DBAdaptor.SQLFormatValue(qMaster.Field[slMaster[J]].Value, qMaster.Field[slMaster[J]].DataType);
          end;
        finally
          sl.free;
        end;

        Action := raApplySourceToDest;
        if Assigned(FOnCompareSubtable) then
          FOnCompareSubtable(Self, FCurrentTableName, subtable.TableName, qSource, qDest, Action);

        if Action = raApplySourceToDest then begin
          connOrigin := SourceConnection;
          connDest := DestConnection;
        end else if Action = raApplyDestToSource then begin
          connOrigin := SourceConnection;
          connDest := DestConnection;
        end
        else Continue;


        qSubTable := connOrigin.SelectQuery['CompareSubtables_' + subtable.TableName];
        qSubTable.Close;
        qSubTable.SQL.Text := 'select * from ' + connOrigin.DBAdaptor.MetaQuote(subtable.TableName) + ' where ' + cWhere;
        qSubTable.Exec;

        qSubTableDest := connDest.SelectQuery['CompareSubtables_' + subtable.TableName];
        qSubTableDest.Close;
        qSubTableDest.SQL.Text := 'select * from ' + connDest.DBAdaptor.MetaQuote(subtable.TableName) + ' where ' + cWhere;
        qSubTableDest.Exec;
        memSubTableDest.LoadFromDataSet(qSubTableDest, False, True, False);
        memSubTableSource.LoadFromDataSet(qSubTable, False, True, False);

        memSubTableSource.First;
        while not memSubTableSource.Eof do
        begin
          if rowExistsInTable(SourceConnection, subtable.TableName, memSubTableDest, memSubTableSource) then begin
            if FieldsDifferent(memSubTableSource, memSubTableDest) then begin
              if Action = raApplySourceToDest then
                CopyRowToChangeList(memSubTableSource, 'DEST', subtable.TableName, 'UPDATE')
              else
                CopyRowToChangeList(memSubTableDest, 'SOURCE', subtable.TableName, 'UPDATE');
            end;
          end
          else begin
            if Action = raApplySourceToDest then
              CopyRowToChangeList(memSubTableSource, 'DEST', subtable.TableName, 'INSERT')
            else
              CopyRowToChangeList(memSubTableSource, 'SOURCE', subtable.TableName, 'DELETE');
          end;
          memSubTableSource.Next;
        end;

        memSubTableDest.First;
        while not memSubTableDest.Eof do
        begin
          if not rowExistsInTable(SourceConnection, subtable.TableName, memSubTableSource, memSubTableDest) then begin
            if Action = raApplySourceToDest then
              CopyRowToChangeList(memSubTableDest, 'DEST', subtable.TableName, 'DELETE')
            else
              CopyRowToChangeList(memSubTableDest, 'SOURCE', subtable.TableName, 'INSERT')
          end;
          memSubTableDest.Next;
        end;
      end;
    finally
      memSubTableDest.Free;
      memSubTableSource.FRee;
    end;
  end;

begin
  ClearChangeLists;

  FTableRowNumber := 0;
  FCurrentRow := 0;

  qSource := TCcQuery.Create(Self);
  qSource.Connection := SourceConnection;
  qSource.SelectStatement := True;

  qDest := TCcQuery.Create(Self);
  qDest.Connection := DestConnection;
  qDest.SelectStatement := True;

  sourceKeys := TCcKeyRing.Create(SourceConnection);
  destKeys := TCcKeyRing.Create(DestConnection);
  try
    qSource.Connection := SourceConnection;
    qDest.Connection := DestConnection;

    for I := 0 to Tables.Count-1 do
    begin
      if not Tables[i].Active then
        Continue;

      FCurrentTableName := Tables[i].TableName;
      OpenQuery(Tables[i], qSource);
      OpenQuery(Tables[i], qDest);
      FTableRowNumber := 0;

      while not (qSource.Eof and qDest.Eof) and (TableRowNumber <= Tables[i].NumberOfRows) do
      begin
        sourceKeys.LoadKeysFromDataSet(FCurrentTableName, qSource, '', '');
        destKeys.LoadKeysFromDataSet(FCurrentTableName, qDest, '', '');

        if qSource.Eof then
          q := qDest
        else if qDest.Eof then
          q := qSource
        else
          q := FindQuery(Tables[i]);
        if q = nil then
        begin
          //The rows are present on both sides
          if QueryFieldsDifferent then begin
            Action := raApplySourceToDest;
            if Assigned(FOnRowChanged) then
              FOnRowChanged(Self, FCurrentTableName, sourceKeys, qSource, qDest, Action);

            if Action = raApplySourceToDest then
              CopyRowToChangeList(qSource, 'DEST', FCurrentTableName, 'UPDATE')
            else if Action = raApplyDestToSource then
              CopyRowToChangeList(qDest, 'SOURCE', FCurrentTableName, 'UPDATE')
          end else begin
            Action := raIgnore;
            if Assigned(FOnRowIdentical) then
              FOnRowIdentical(Self, FCurrentTableName, sourceKeys, qSource, qDest, Action);
          end;
          CompareSubtables(Tables[i], qSource);

          Inc(FTableRowNumber);
          Inc(FCurrentRow);
          qSource.Next;
          qDest.Next;
        end else begin
          //Row is missing from one side
          Action := raApplySourceToDest;
          if q = qSource then begin
            if Assigned(FOnRowMissingInDest) then
              FOnRowMissingInDest(Self, FCurrentTableName, sourceKeys, qSource, nil, Action);

            if Action = raApplySourceToDest then begin
              CopyRowToChangeList(q, 'DEST', FCurrentTableName, 'INSERT');
              CopySubTableRows(SourceConnection, 'DEST', q, Tables[i], 'INSERT');
            end
            else if Action = raApplyDestToSource then begin
              CopyRowToChangeList(q, 'SOURCE', FCurrentTableName, 'DELETE');
              CopySubTableRows(SourceConnection, 'SOURCE', q, Tables[i], 'DELETE');
            end;
          end else begin
            if Assigned(FOnRowMissingInSource) then
              FOnRowMissingInSource(Self, FCurrentTableName, sourceKeys, nil, qSource, Action);

            if Action = raApplySourceToDest then begin
              CopyRowToChangeList(q, 'DEST', FCurrentTableName, 'DELETE');
              CopySubTableRows(DestConnection, 'DEST', q, Tables[i], 'DELETE');
            end
            else if Action = raApplyDestToSource then begin
              CopyRowToChangeList(q, 'SOURCE', FCurrentTableName, 'INSERT');
              CopySubTableRows(DestConnection, 'SOURCE', q, Tables[i], 'INSERT');
            end;
          end;
          Inc(FCurrentRow);
          Inc(FTableRowNumber);
          q.Next;
        end;
      end;
    end;
  finally
    qSource.Free;
    qDest.Free;
    sourceKeys.Free;
    destKeys.Free;
  end;
end;

procedure TCcComparer.CompareMetaData;
begin
//Not implemented for now

end;


constructor TCcComparer.Create(AOwner: TComponent);
begin
  inherited;
  FChangeList := TDictionary<String, TCcMemoryTable>.Create;
  FTables := TCcComparerTables.Create(Self);
end;

destructor TCcComparer.Destroy;
begin
  ClearChangeLists;
  FTables.Free;
  FChangeList.Free;
  inherited;
end;

function TCcComparer.GetFieldsSQL(tableName: String; connection: TCcConnection; fields: TCcMemoryFields; cOperationType: string): string;
var
  I: Integer;
  cFields, cParams, cFieldName: string;
  slFields: TStringList;
begin
  slFields := connection.ListUpdatableTableFields(tableName);
  for I := 0 to slfields.Count - 1 do
  begin
    cFieldName := slFields[I];
    if (Trim(cFields) <> '') then
      cFields := cFields + ', ';
    if (Trim(cParams) <> '') then
      cParams := cParams + ', ';

    cFields := cFields + cFieldName;
    cParams := cParams + connection.DBAdaptor.SQLFormatValue(fields.FieldByName(cFieldName).Value, fields.FieldByName(cFieldName).DataType);

    if (cOperationType = 'UPDATE') then
    begin
      // S''il s''agit d''un UPDATE, on remet à zéro cParams, de sorte que cParams ne contienne
      // à chaque itération que la nom du paramètre.
      cFields := cFields + ' = ' + cParams;
      cParams := '';
    end
  end;
  if (cOperationType = 'UPDATE') then
    Result := cFields
  else
    Result := '(' + cFields + ')'#13#10'values'#13#10'(' + cParams + ')';
end;

function TCcComparer.CalcWhereClause(DB: TCcConnection; keys: TCcKeyRing): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to keys.Count - 1 do
    if keys[I].PrimaryKey then
    begin
      if I > 0 then
        Result := Result + ' and ';
      Result := Result + DB.DBAdaptor.MetaQuote(keys[I].KeyName) + ' = ' +  db.DBAdaptor.SQLFormatValue(keys[I].KeyValue, keys[I].DataType);
    end;
end;


function TCcComparer.GetScript(db:String): String;
var
  cTableName: string;
  keys: TCcKeyRing;
  Connection: TCcConnection;
  J: Integer;

  function GetRowStatements(operation: String): String;
  var
    memTable: TCcMemoryTable;
  begin
    Result := '';
    if fChangeList.ContainsKey(db+'_'+cTableName+'_' + operation) then begin
      memTable := FChangeList[db+'_'+cTableName+'_' + operation];
      memTable.First;
      while not memTable.Eof do
      begin
        keys.LoadKeysFromDataSet(cTableName, memTable, '','');

        if operation = 'UPDATE' then
          Result := Result + 'UPDATE ' + cTableName + ' set ' + GetFieldsSQL(cTableName, Connection, memTable.fields, 'UPDATE') + ' WHERE ' + CalcWhereClause(Connection, keys) + ScriptTerminator + #13#10
        else if operation = 'INSERT' then
          Result := Result + 'INSERT INTO ' + cTableName + GetFieldsSQL(cTableName, Connection, memTable.fields, 'INSERT') + ScriptTerminator + #13#10
        else
          Result := Result + 'DELETE FROM ' + cTableName + ' WHERE ' + CalcWhereClause(Connection, keys) + ScriptTerminator + #13#10;

        memTable.Next;
      end;
    end;
  end;

var
  I: Integer;
begin
  if db = 'DEST' then
    Connection := DestConnection
  else
    Connection := SourceConnection;

  keys := TCcKeyRing.Create(Connection);
  try
    for I := 0 to Tables.Count-1 do
    begin
      cTableName := Tables[i].TableName;
      Result := Result + GetRowStatements('UPDATE');
      Result := Result + GetRowStatements('INSERT');
      Result := Result + GetRowStatements('DELETE');

      for J := 0 to Tables[i].Subtables.Count - 1 do
      begin
        cTableName := Tables[i].Subtables[j].TableName;
        Result := Result + GetRowStatements('UPDATE');
        Result := Result + GetRowStatements('INSERT');
        Result := Result + GetRowStatements('DELETE');
      end;
    end;
  finally
    keys.Free;
  end;
end;

function TCcComparer.GetDestScript: String;
begin
  Result := GetScript('DEST');
end;

function TCcComparer.GetRowCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Tables.Count - 1 do begin
    if Tables[i].NumberOfRows = 0 then begin
      Result := 0;
      break;
    end;

    Result := Result + Tables[i].NumberOfRows;
  end;
end;

function TCcComparer.GetSourceScript: String;
begin
  Result := GetScript('SOURCE');
end;

procedure TCcComparer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if (AComponent = SourceConnection) then
      SetSourceConnection(nil)
    else if (AComponent = DestConnection) then
      SetDestConnection(nil);
  end;
  inherited;
end;

procedure TCcComparer.SetDestConnection(const Value: TCcConnection);
begin
  FDestConnection := Value;
end;

procedure TCcComparer.SetScriptTerminator(const Value: String);
begin
  FScriptTerminator := Value;
end;

procedure TCcComparer.SetSourceConnection(const Value: TCcConnection);
begin
  FSourceConnection := Value;
end;

procedure TCcComparer.SetTables(const Value: TCcComparerTables);
begin
	FTables.Assign(Value);
end;

function TCcComparerTables.Add: TCcComparerTable;
begin
	result := inherited Add as TCcComparerTable;
end;

constructor TCcComparerTables.Create(AOwner: TPersistent);
begin
  inherited Create(TCcComparerTable);
   FOwner := AOwner;
end;

function TCcComparerTables.FindTable(tableName: String): TCcComparerTable;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count -1 do begin
	 if (UpperCase(Trim(Item[I].TableName)) = UpperCase(Trim(tableName))) then begin
		 Result := Item[I] as TCcComparerTable;
		 Exit;
	 end;
	end;
end;

function TCcComparerTables.GetItem(Index: Integer): TCcComparerTable;
begin
	result := inherited Items[Index] as TCcComparerTable;
end;


function TCcComparerTables.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


{ TCcScript }

function TCcScript.GetScriptText: String;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Result := Result + Items[i] + Terminator;
end;

{ TCcComparerSubTables }

function TCcComparerSubTables.Add: TCcComparerSubTable;
begin
	result := inherited Add as TCcComparerSubTable;
end;

constructor TCcComparerSubTables.Create(AOwner: TPersistent);
begin
  inherited Create(TCcComparerSubTable);
   FOwner := AOwner;
end;

function TCcComparerSubTables.FindTable(tableName: String): TCcComparerSubTable;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count -1 do begin
	 if (UpperCase(Trim(Item[I].TableName)) = UpperCase(Trim(tableName))) then begin
		 Result := Item[I] as TCcComparerSubTable;
		 Exit;
	 end;
	end;
end;

function TCcComparerSubTables.GetItem(Index: Integer): TCcComparerSubTable;
begin
	result := inherited Items[Index] as TCcComparerSubTable;
end;

function TCcComparerSubTables.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TCcComparerTable }

constructor TCcComparerTable.Create(Collection: TCollection);
begin
  inherited;
  FSubtables := TCcComparerSubtables.Create(Self);
  Active := True;
end;

destructor TCcComparerTable.Destroy;
begin
  FSubtables.Free;
  inherited;
end;

procedure TCcComparerTable.SetSubtables(const Value: TCcComparerSubTables);
begin
  FSubtables.Assign(Value);
end;

{ TCcComparerSubTable }

constructor TCcComparerSubTable.Create(Collection: TCollection);
begin
  inherited;
  Active := True;
end;

end.
