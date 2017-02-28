unit CcProvNexusDB;

interface

uses Classes, Sysutils, DB, CcProviders, nxdb;

type

TCcQueryNexusDB = class(TCcAbstractQueryObject)
  private
    FnxQuery: TNxQuery;
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
    destructor Destroy; override;
		property nxQuery : TNxQuery read FnxQuery;
end;

TCcConnectionNexusDB = class(TCcConnection)
  private
    FnxDatabase: TnxDatabase;
    FAutoCreatedConnection: Boolean;
    procedure SetNXDatabase(const Value: TnxDatabase);
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;override;
    procedure DoDisconnect;override;
    procedure DoConnect; override;
    procedure DoCommit;override;
    procedure DoCommitRetaining;override;
    procedure DoRollback;override;
    procedure DoRollbackRetaining;override;
    procedure DoStartTransaction;override;
    function GetConnectorConnected: Boolean;override;
    function GetInTransaction: Boolean;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
  published
    property nxDatabase: TnxDatabase read FnxDatabase write SetNXDatabase;
    property DBType;
    property DBVersion;
end;

procedure Register;

implementation

uses CcNexusDB;

procedure TCcConnectionNexusDB.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FnxDatabase then
      SetnxDatabase(nil);
  end;
  inherited;
end;

procedure TCcConnectionNexusDB.SetNXDatabase(const Value: TnxDatabase);
begin
  if Assigned(FnxDatabase) then
    FnxDatabase.RemoveFreeNotification(Self);

  FnxDatabase := Value;

  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

function TCcConnectionNexusDB.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryNexusDB;
begin
	q := TCcQueryNexusDB.Create(Self, qry, nID, qry.SelectStatement);
  q.nxQuery.Database := nxDatabase;
  Result := q;
end;

class function TCcConnectionNexusDB.ConnectorName: String;
begin
  Result := 'NexusDB';
end;

procedure TCcQueryNexusDB.DoClose;
begin
  nxQuery.Close;
end;

constructor TCcQueryNexusDB.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
	inherited;
	FnxQuery := TnxQuery.Create(Conn);
end;

destructor TCcQueryNexusDB.Destroy;
begin
  FnxQuery.Free;
  inherited;
end;

procedure TCcQueryNexusDB.DoExec;
begin
  nxQuery.Open;
end;

function TCcQueryNexusDB.GetEof: Boolean;
begin
  Result := nxQuery.Eof;
end;

function TCcQueryNexusDB.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType :TFieldType;
begin
{  fType := GetFieldType(FieldName, IsParam);
  if (fType = ftString) or (fType = ftWideString) or
     (fType = ftFixedChar) or (fType = ftBcd) or (fType = ftGuid) then begin}
    if IsParam then
      Result := nxQuery.ParamByName(FieldName).Size
    else
      Result := nxQuery.FieldByName(FieldName).Size;
{  end else
    Result := 0;}
end;

function TCcQueryNexusDB.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := nxQuery.ParamByName(FieldName).DataType
  else
    Result := nxQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryNexusDB.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := nxQuery.Params[Field.Index].Value
  else
    Result := nxQuery.Fields[Field.Index].Value;
end;

function TCcQueryNexusDB.GetRowsAffected: Integer;
begin
  Result := nxQuery.RowsAffected;
end;

procedure TCcQueryNexusDB.DoNext;
begin
  nxQuery.Next;
end;

procedure TCcQueryNexusDB.DoPrepare(SQLText:String);
begin
  nxQuery.SQL.Text := SQLText;
  nxQuery.Prepare;
end;

procedure TCcQueryNexusDB.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to nxQuery.FieldCount-1 do
    FieldList.Add(nxQuery.Fields[i].FieldName);
end;

procedure TCcQueryNexusDB.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to nxQuery.Params.Count-1 do
    ParamList.Add(nxQuery.Params[i].Name);
end;

procedure TCcQueryNexusDB.DoUnPrepare;
begin
  nxQuery.UnPrepare;
end;

procedure TCcQueryNexusDB.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then begin
    nxQuery.Params[Field.Index].DataType := field.DataType;
    nxQuery.Params[Field.Index].Value := Value
  end else
    nxQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryNexusDB.SetParamCheck(lParamCheck: Boolean);
begin
  nxQuery.ParamCheck := lParamCheck;
end;

function TCcConnectionNexusDB.GetConnectorConnected: Boolean;
begin
  Result := False;
  if Assigned(nxDatabase) then
    Result := nxDatabase.Connected;
end;

function TCcConnectionNexusDB.GetInTransaction: Boolean;
begin
  Result := False;
  if Assigned(nxDatabase) then
    Result := nxDatabase.InTransaction;
end;

constructor TCcConnectionNexusDB.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcNexusDBAdaptor);
  FAutoCreatedConnection := False;
end;

procedure TCcConnectionNexusDB.DoDisconnect;
begin
  nxDatabase.Connected := False;
end;

destructor TCcConnectionNexusDB.Destroy;
begin
  if FAutoCreatedConnection then begin
    nxDatabase.Session.Free;
    nxDatabase.Free;
  end;
  inherited;
end;

procedure TCcConnectionNexusDB.DoCommit;
begin
  if nxDatabase.InTransaction then
     nxDatabase.Commit;
end;

procedure TCcConnectionNexusDB.DoCommitRetaining;
begin
  if nxDatabase.InTransaction then
  begin
    nxDatabase.Commit;
    nxDatabase.StartTransaction;
  end;
end;

procedure TCcConnectionNexusDB.DoConnect;
begin
  inherited;
  FnxDatabase.Connected := False;
  FnxDatabase.Connected := True;
end;

procedure TCcConnectionNexusDB.DoRollback;
begin
  if nxDatabase.InTransaction then
    nxDatabase.Rollback;
end;

procedure TCcConnectionNexusDB.DoRollbackRetaining;
begin
  if nxDatabase.InTransaction then begin
     nxDatabase.Rollback;
     nxDatabase.StartTransaction;
  end;
end;

procedure TCcConnectionNexusDB.DoStartTransaction;
begin
  if not nxDatabase.Active then
    nxDatabase.StartTransaction;
end;

procedure TCcConnectionNexusDB.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionNexusDB then begin
    with TCcConnectionNexusDB(Source) do
    begin
      self.nxDatabase := TnxDatabase.Create(Self);
      Self.nxDatabase.AliasName := nxDatabase.AliasName;
      self.nxDatabase.Session := TnxSession.Create(self);
      self.nxDatabase.Session.UserName := nxDatabase.Session.UserName;
      self.nxDatabase.Session.Password := nxDatabase.Session.Password;
      TnxSession(self.nxDatabase.Session).ServerEngine := nxDatabase.Session.ServerEngine;
    end;
    FAutoCreatedConnection := True;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionNexusDB]);
end;

initialization
  RegisterDBConnector(TCcConnectionNexusDB, TCcConnectionNexusDB.ConnectorName);

end.
