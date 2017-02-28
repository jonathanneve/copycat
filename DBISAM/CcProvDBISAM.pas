unit CcProvDBISAM;

interface

uses Classes, Sysutils, DB, CcProviders, dbisamtb ,CcDBISAM;

type

TCcQueryDBISAM = class(TCcAbstractQueryObject)
  private
    FdbisamQuery: TDBISAMQuery;
  protected
    function GetRowsAffected: Integer;override;
    function GetEof: Boolean;override;
    procedure DoInitParams(ParamList: TStringList);override;
    procedure DoInitFields(FieldList: TStringList);override;
    procedure DoExec;override;
    procedure DoPrepare(SQLText:String);override;
    procedure DoUnPrepare;override;
    procedure SetParamCheck(lParamCheck: Boolean);override;
    procedure DoClose;override;
    procedure DoNext;override;
    function GetFieldType(FieldName: String; IsParam: Boolean): TFieldType;override;
    function GetFieldSize(FieldName: String; IsParam: Boolean): Integer;override;
    function GetFieldValue(Field: TCCField): Variant;override;
    procedure SetFieldValue(Field: TCCField; Val: Variant);override;
  public
		constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
		property dbisamQuery : TDBISAMQuery read FdbisamQuery;
end;

TCcConnectionDBISAM = class(TCcConnection)
  private
    FdbisamDatabase: TDBISAMDatabase;
    FAutoCreatedConnection: Boolean;
    procedure SetdbisamDatabase(const Value: TDBISAMDatabase);
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;override;
    procedure DoDisconnect;override;
    procedure DoConnect; override;
    procedure DoCommit;override;
    procedure DoCommitRetaining;override;
    procedure DoRollback;override;
    procedure DoRollbackRetaining;override;
    procedure DoStartTransaction;override;
    function GetConnected: Boolean;override;
    function GetInTransaction: Boolean;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
  published
    property dbisamDatabase: TDBISAMDatabase read FdbisamDatabase write SetdbisamDatabase;
    property DBType;
    property DBVersion;
end;

TCcDBISAMEngine = class (TComponent)
  private
    procedure InsertTrigger(Sender: TObject; TriggerSession: TDBISAMSession;
                             TriggerDatabase: TDBISAMDatabase;
                             const TableName: String;
                             CurrentRecord: TDBISAMRecord);
    procedure UpdateTrigger(Sender: TObject; TriggerSession: TDBISAMSession;
                             TriggerDatabase: TDBISAMDatabase;
                             const TableName: String;
                             CurrentRecord: TDBISAMRecord);
    procedure DeleteTrigger(Sender: TObject; TriggerSession: TDBISAMSession;
                             TriggerDatabase: TDBISAMDatabase;
                             const TableName: String;
                             CurrentRecord: TDBISAMRecord);
  public
    procedure Activate;
    procedure Deactivate;
end;

procedure Register;

implementation

procedure TCcConnectionDBISAM.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FdbisamDatabase then
      SetdbisamDatabase(nil);
  end;
  inherited;
end;

procedure TCcConnectionDBISAM.SetdbisamDatabase(const Value: TDBISAMDatabase);
begin
  if Assigned(FdbisamDatabase) then
    FdbisamDatabase.RemoveFreeNotification(Self);

  FdbisamDatabase := Value;

  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

function TCcConnectionDBISAM.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryDBISAM;
begin
	q := TCcQueryDBISAM.Create(Self, qry, nID, qry.SelectStatement);
  q.dbisamQuery.DatabaseName := dbisamDatabase.DatabaseName;
  Result := q;
end;

class function TCcConnectionDBISAM.ConnectorName: String;
begin
  Result := 'DBISAM';
end;

procedure TCcQueryDBISAM.DoClose;
begin
  dbisamQuery.Close;
end;

constructor TCcQueryDBISAM.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
	inherited;
	FdbisamQuery := TDBISAMQuery.Create(Conn);
end;

procedure TCcQueryDBISAM.DoExec;
begin
  dbisamQuery.Open;
end;

function TCcQueryDBISAM.GetEof: Boolean;
begin
  Result := dbisamQuery.Eof;
end;

function TCcQueryDBISAM.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType :TFieldType;
begin
{  fType := GetFieldType(FieldName, IsParam);
  if (fType = ftString) or (fType = ftWideString) or
     (fType = ftFixedChar) or (fType = ftBcd) or (fType = ftGuid) then begin}
    if IsParam then
      Result := dbisamQuery.ParamByName(FieldName).GetDataSize
    else
      Result := dbisamQuery.FieldByName(FieldName).Size;
{  end else
    Result := 0;}
end;

function TCcQueryDBISAM.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := dbisamQuery.ParamByName(FieldName).DataType
  else
    Result := dbisamQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryDBISAM.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := dbisamQuery.Params[Field.Index].Value
  else
    Result := dbisamQuery.Fields[Field.Index].Value;
end;

function TCcQueryDBISAM.GetRowsAffected: Integer;
begin
  Result := dbisamQuery.RowsAffected;
end;

procedure TCcQueryDBISAM.DoNext;
begin
  dbisamQuery.Next;
end;

procedure TCcQueryDBISAM.DoPrepare(SQLText:String);
begin
  dbisamQuery.SQL.Text := SQLText;
  dbisamQuery.Prepare;
end;

procedure TCcQueryDBISAM.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to dbisamQuery.FieldCount-1 do
    FieldList.Add(dbisamQuery.Fields[i].FieldName);
end;

procedure TCcQueryDBISAM.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to dbisamQuery.Params.Count-1 do
    ParamList.Add(dbisamQuery.Params[i].Name);
end;

procedure TCcQueryDBISAM.DoUnPrepare;
begin
  dbisamQuery.UnPrepare;
end;

procedure TCcQueryDBISAM.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then begin
    dbisamQuery.Params[Field.Index].DataType := field.DataType;
    dbisamQuery.Params[Field.Index].Value := Value
  end else
    dbisamQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryDBISAM.SetParamCheck(lParamCheck: Boolean);
begin
  dbisamQuery.ParamCheck := lParamCheck;
end;

function TCcConnectionDBISAM.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(dbisamDatabase) then
    Result := dbisamDatabase.Connected;
end;

function TCcConnectionDBISAM.GetInTransaction: Boolean;
begin
  Result := False;
  if Assigned(dbisamDatabase) then
    Result := dbisamDatabase.InTransaction;
end;

constructor TCcConnectionDBISAM.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcDBISamAdaptor);
  FAutoCreatedConnection := False;
end;

procedure TCcConnectionDBISAM.DoDisconnect;
begin
  dbisamDatabase.Connected := False;
end;

destructor TCcConnectionDBISAM.Destroy;
begin
  if FAutoCreatedConnection then begin
    dbisamDatabase.Session.Free;
    dbisamDatabase.Free;
  end;
  inherited;
end;

procedure TCcConnectionDBISAM.DoCommit;
begin
  if dbisamDatabase.InTransaction then
     dbisamDatabase.Commit;
end;

procedure TCcConnectionDBISAM.DoCommitRetaining;
begin
  if dbisamDatabase.InTransaction then
  begin
    dbisamDatabase.Commit;
    dbisamDatabase.StartTransaction;
  end;
end;

procedure TCcConnectionDBISAM.DoConnect;
begin
  inherited;
  FdbisamDatabase.Connected := False;
  FdbisamDatabase.Connected := True;
end;

procedure TCcConnectionDBISAM.DoRollback;
begin
  if dbisamDatabase.InTransaction then
    dbisamDatabase.Rollback;
end;

procedure TCcConnectionDBISAM.DoRollbackRetaining;
begin
  if dbisamDatabase.InTransaction then begin
     dbisamDatabase.Rollback;
     dbisamDatabase.StartTransaction;
  end;
end;

procedure TCcConnectionDBISAM.DoStartTransaction;
begin
  if not dbisamDatabase.InTransaction then
    dbisamDatabase.StartTransaction;
end;

procedure TCcConnectionDBISAM.Assign(Source: TPersistent);
var
  sourceDB: TCcConnectionDBISAM;
begin
  if Source is TCcConnectionDBISAM then begin
    sourceDB := TCcConnectionDBISAM(Source);
    self.dbisamDatabase := TdbisamDatabase.Create(Self);
    self.dbisamDatabase.Assign(sourceDB.dbisamDatabase);
    FAutoCreatedConnection := True;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionDBISAM, TCcDBISAMEngine]);
end;

{ TCcDBISAMEngine }

procedure TCcDBISAMEngine.Activate;
begin
  //OldInsertTrigger := Engine.AfterInsertTrigger;
  Engine.AfterInsertTrigger := InsertTrigger;
end;

procedure TCcDBISAMEngine.Deactivate;
begin
//  Engine.AfterInsertTrigger := OldInsertTrigger;
end;

procedure TCcDBISAMEngine.DeleteTrigger(Sender: TObject;
  TriggerSession: TDBISAMSession; TriggerDatabase: TDBISAMDatabase;
  const TableName: String; CurrentRecord: TDBISAMRecord);
begin

end;

procedure TCcDBISAMEngine.InsertTrigger(Sender: TObject;
  TriggerSession: TDBISAMSession; TriggerDatabase: TDBISAMDatabase;
  const TableName: String; CurrentRecord: TDBISAMRecord);
begin


end;

procedure TCcDBISAMEngine.UpdateTrigger(Sender: TObject;
  TriggerSession: TDBISAMSession; TriggerDatabase: TDBISAMDatabase;
  const TableName: String; CurrentRecord: TDBISAMRecord);
begin

end;

initialization
  RegisterDBConnector(TCcConnectionDBISAM, TCcConnectionDBISAM.ConnectorName);

end.
