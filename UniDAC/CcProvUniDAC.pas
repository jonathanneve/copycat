unit CcProvUniDAC;

interface

uses Classes, Sysutils, DB, CcProviders, DBAccess, Uni;

type

TCcQueryUniDAC = class(TCcAbstractQueryObject)
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
    UniQuery : TUniQuery;
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
    destructor Destroy; override;
end;

TCcConnectionUniDAC = class(TCcConnection)
  private
    FUniConnection: TUniConnection;
    procedure SetUniConnection(const Value: TUniConnection);
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;override;
    procedure DoDisconnect;override;
    procedure DoConnect; override;
    procedure DoCommit;override;
    procedure DoCommitRetaining;override;
    procedure DoRollback;override;
    procedure DoRollbackRetaining;override;
    procedure DoStartTransaction;override;
    function GetUniConnection: TUniConnection;
    function GetConnectorConnected: Boolean;override;
    function GetInTransaction: Boolean;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;

  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    procedure Assign(Source: TPersistent);override;
  published
    property UniConnection: TUniConnection read FUniConnection write SetUniConnection;
    property DBType;
    property DBVersion;
end;

procedure Register;

implementation

uses CcMySQL, CcInterbase, CcSQLServer, CcPostgres, CcSQLite;

procedure TCcConnectionUniDAC.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FUniConnection then
      SetUniConnection(nil);
  end;
  inherited;
end;

procedure TCcConnectionUniDAC.SetUniConnection(const Value: TUniConnection);
begin
  if Assigned(FUniConnection) then
    FUniConnection.RemoveFreeNotification(Self);

  FUniConnection := Value;

  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

function TCcConnectionUniDAC.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryUniDAC;
  ibAdaptor: TCcInterbaseAdaptor;
begin
  q := TCcQueryUniDAC.Create(Self, qry, nID, qry.SelectStatement);
  q.UniQuery.Connection := GetUniConnection;
  if (DBType = 'Interbase') then begin
    ibAdaptor := DBAdaptor as TCcInterbaseAdaptor;
    if ((ibAdaptor.DBBranch = dbInterbase) and (ibAdaptor.BranchVersion < 70))
      or ((ibAdaptor.DBBranch = dbFirebird) and (ibAdaptor.BranchVersion < 30)) then
      q.UniQuery.SpecificOptions.Values['BooleanDomainFields'] := 'False';
  end;

  Result := q;
end;

class function TCcConnectionUniDAC.ConnectorName: String;
begin
  Result := 'UniDAC';
end;

procedure TCcQueryUniDAC.DoClose;
begin
  UniQuery.Close;
end;

constructor TCcQueryUniDAC.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
  UniQuery := TUniQuery.Create(Conn);
end;

destructor TCcQueryUniDAC.Destroy;
begin
  UniQuery.Free;
  inherited;
end;

procedure TCcQueryUniDAC.DoExec;
begin
  UniQuery.Execute;
end;

function TCcQueryUniDAC.GetEof: Boolean;
begin
  Result := UniQuery.Eof;
end;

function TCcQueryUniDAC.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType :TFieldType;
begin
  fType := GetFieldType(FieldName, IsParam);
  if fType = ftString then begin
    if IsParam then
      Result := UniQuery.ParamByName(FieldName).Size
    else
      Result := UniQuery.FieldByName(FieldName).Size;
  end else
    Result := 0;
end;

function TCcQueryUniDAC.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := UniQuery.ParamByName(FieldName).DataType
  else
    Result := UniQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryUniDAC.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := UniQuery.Params[Field.Index].Value
  else
    Result := UniQuery.Fields[Field.Index].Value;
end;

function TCcQueryUniDAC.GetRowsAffected: Integer;
begin
  Result := UniQuery.RowsAffected;
end;

procedure TCcQueryUniDAC.DoNext;
begin
  UniQuery.Next;
end;

procedure TCcQueryUniDAC.DoPrepare(SQLText:String);
begin
  UniQuery.SQL.Text := SQLText;
end;

procedure TCcQueryUniDAC.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to UniQuery.FieldCount-1 do
    FieldList.Add(UniQuery.Fields[i].FieldName);
end;

procedure TCcQueryUniDAC.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to UniQuery.Params.Count-1 do
    ParamList.Add(UniQuery.Params[i].Name);
end;

procedure TCcQueryUniDAC.DoUnPrepare;
begin
   UniQuery.Close;
end;

procedure TCcQueryUniDAC.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then begin
    UniQuery.Params[Field.Index].DataType := Field.DataType;
    UniQuery.Params[Field.Index].Value := Value;
  end
  else
    UniQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryUniDAC.SetParamCheck(lParamCheck: Boolean);
begin
  UniQuery.ParamCheck := lParamCheck;
end;

function TCcConnectionUniDAC.GetConnectorConnected: Boolean;
begin
  Result := False;
  if Assigned(UniConnection) then
    Result := GetUniConnection.Connected;
end;

function TCcConnectionUniDAC.GetInTransaction: Boolean;
begin
  Result := False;
  if Assigned(UniConnection) then
    Result := GetUniConnection.InTransaction
end;

constructor TCcConnectionUniDAC.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcMySQLAdaptor);
  AddDBAdaptor(TCcInterbaseAdaptor);
  AddDBAdaptor(TCcSQLServerAdaptor);
  AddDBAdaptor(TCcPostgresAdaptor);
  AddDBAdaptor(TCcSQLiteAdaptor);
end;

procedure TCcConnectionUniDAC.DoDisconnect;
begin
  GetUniConnection.Disconnect;
end;

procedure TCcConnectionUniDAC.DoCommit;
begin
  if GetUniConnection.InTransaction then
     GetUniConnection.Commit;
end;

procedure TCcConnectionUniDAC.DoCommitRetaining;
begin
  if GetUniConnection.InTransaction then
  begin
    GetUniConnection.CommitRetaining;
  end;
end;

procedure TCcConnectionUniDAC.DoConnect;
begin
  inherited;
  GetUniConnection.SpecificOptions.Assign(ConnectionParams);
  GetUniConnection.Connected := False;
  GetUniConnection.Connect;
end;

procedure TCcConnectionUniDAC.DoRollback;
begin
  if GetUniConnection.InTransaction then
    GetUniConnection.Rollback;
end;

procedure TCcConnectionUniDAC.DoRollbackRetaining;
begin
  if GetUniConnection.InTransaction then
  begin
     GetUniConnection.RollbackRetaining;
  end
end;

procedure TCcConnectionUniDAC.DoStartTransaction;
begin
  if not GetUniConnection.InTransaction then
    GetUniConnection.StartTransaction;
end;

procedure TCcConnectionUniDAC.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionUniDAC then
    with TCcConnectionUniDAC(Source) do
      Self.UniConnection := UniConnection;
  inherited;
end;

function TCcConnectionUniDAC.GetUniConnection: TUniConnection;
begin
  if Assigned(UniConnection) then Result := UniConnection
  else raise Exception.Create('UniDAC connection object must be assigned');
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionUniDAC]);
end;

initialization
  RegisterDBConnector(TCcConnectionUniDAC, TCcConnectionUniDAC.ConnectorName);

end.
