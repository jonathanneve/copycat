unit CcProvLazSQLDB;

{$I CC.INC}

interface

uses Classes, Sysutils, DB, CcProviders, SQLDB;
                                             
type

TCcQuerySQLDB = class(TCcAbstractQueryObject)
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
    SQLQuery :TSQLQuery;
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
    destructor Destroy; override;
end;

{ TCcConnectionSQLDB }

TCcConnectionSQLDB = class(TCcConnection)
  private
    FDatabase: TSQLConnector;
    FTransaction: TSQLTransaction;
    FAutoCreatedConnection :Boolean;
    FUserLogin: String;
    procedure SetDatabase(AValue: TSQLConnector);
    procedure SetTransaction(AValue: TSQLTransaction);
    procedure SetConnectParams;
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
    procedure CreateDatabase;
  published
    property Database: TSQLConnector read FDatabase write SetDatabase;
    property Transaction :TSQLTransaction read FTransaction write SetTransaction;
    property DBType;
    property DBVersion;
end;

procedure Register;

implementation

uses
  CcMySQL, CcSQLServer, CcInterbase , CCOracle , CCPostgres, CcSQLite;


procedure TCcConnectionSQLDB.SetDatabase(AValue: TSQLConnector);
begin
  if Assigned(FDatabase) then
    FDatabase.RemoveFreeNotification(Self);

  FDatabase := AValue;

  if Assigned(AValue) then begin
    AValue.FreeNotification(Self);
  end;
end;

procedure TCcConnectionSQLDB.SetTransaction(AValue: TSQLTransaction);
begin
  if Assigned(FTransaction) then
    FTransaction.RemoveFreeNotification(Self);

  FTransaction := AValue;

  if Assigned(AValue) then begin
    AValue.FreeNotification(Self);
  end;
end;

function TCcConnectionSQLDB.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQuerySQLDB;
begin
  q := TCcQuerySQLDB.Create(Self, qry, nID, qry.SelectStatement);
  q.SQLQuery.Database := Database;
  q.SQLQuery.Transaction := Transaction;
  Result := q;
end;

class function TCcConnectionSQLDB.ConnectorName: String;
begin
  Result := 'LazSQLDB';
end;

{ TCcQuerySQLDB }

procedure TCcQuerySQLDB.DoClose;
begin
  SQLQuery.Close;
end;

constructor TCcQuerySQLDB.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
  SQLQuery := TSQLQuery.Create(Conn);
  SQLQuery.UniDirectional := True;
  SQLQuery.ReadOnly := True;
end;

destructor TCcQuerySQLDB.Destroy;
begin
  SQLQuery.Free;
  inherited;
end;

procedure TCcQuerySQLDB.DoExec;
begin
  if SelectStatement then
    SQLQuery.Open
  else
    SQLQuery.ExecSQL;
end;

function TCcQuerySQLDB.GetEof: Boolean;
begin
  Result := SQLQuery.Eof;
end;

function TCcQuerySQLDB.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
begin
  if IsParam then
    Result := SQLQuery.ParamByName(FieldName).Size
  else
    Result := SQLQuery.FieldByName(FieldName).Size;
end;

function TCcQuerySQLDB.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := SQLQuery.ParamByName(FieldName).DataType
  else
    Result := SQLQuery.FieldByName(FieldName).DataType;
end;

function TCcQuerySQLDB.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := SQLQuery.Params[Field.Index].Value
  else
    Result := SQLQuery.Fields[Field.Index].Value;
end;

function TCcQuerySQLDB.GetRowsAffected: Integer;
begin
  Result := SQLQuery.RowsAffected;
end;

procedure TCcQuerySQLDB.DoNext;
begin
  SQLQuery.Next;
end;

procedure TCcQuerySQLDB.DoPrepare(SQLText:String);
begin
  SQLQuery.SQL.Text := SQLText;
  SQLQuery.Prepare;
end;

procedure TCcQuerySQLDB.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to SQLQuery.FieldCount-1 do
    FieldList.Add(SQLQuery.Fields[i].FieldName);
end;

procedure TCcQuerySQLDB.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to SQLQuery.Params.Count-1 do
    ParamList.Add(SQLQuery.Params[i].Name);
end;

procedure TCcQuerySQLDB.DoUnPrepare;
begin
  if SQLQuery.Prepared then
    SQLQuery.UnPrepare;
end;

procedure TCcQuerySQLDB.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then begin
    if SQLQuery.Params[Field.Index].DataType = ftUnknown then
      SQLQuery.Params[Field.Index].DataType := Field.DataType;
    SQLQuery.Params[Field.Index].Value := Value
  end
  else
    SQLQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQuerySQLDB.SetParamCheck(lParamCheck: Boolean);
begin
  SQLQuery.ParamCheck := lParamCheck;
end;

constructor TCcConnectionSQLDB.Create(AOwner: TComponent);
begin
  inherited;
  FAutoCreatedConnection := False;

  AddDBAdaptor(TCcMySQLAdaptor);
  AddDBAdaptor(TCcInterbaseAdaptor);
  AddDBAdaptor(TCcSQLServerAdaptor);
  AddDBAdaptor(TCcOracleAdaptor);
  AddDBAdaptor(TCcPostgresAdaptor);
  AddDBAdaptor(TCcSQLiteAdaptor);
end;

destructor TCcConnectionSQLDB.Destroy;
begin
  if FAutoCreatedConnection then begin
    Database.Free;
    Transaction.Free;
  end;
  inherited Destroy;
end;

procedure TCcConnectionSQLDB.CreateDatabase;
begin
  SetConnectParams;
  Database.CreateDB;
end;

procedure TCcConnectionSQLDB.DoDisconnect;
begin
  Database.Connected := False;
end;

procedure TCcConnectionSQLDB.DoCommit;
begin
  Transaction.Commit;
end;

procedure TCcConnectionSQLDB.DoCommitRetaining;
begin
  Transaction.CommitRetaining;
end;

procedure TCcConnectionSQLDB.SetConnectParams;
begin
  ConnectionParams.Assign(Database.Params);
end;

procedure TCcConnectionSQLDB.DoConnect;
begin
  inherited;
  SetConnectParams;
  Database.Connected := True;
end;

procedure TCcConnectionSQLDB.DoRollback;
begin
  Transaction.Rollback;
end;

procedure TCcConnectionSQLDB.DoRollbackRetaining;
begin
  Transaction.RollbackRetaining;
end;

procedure TCcConnectionSQLDB.DoStartTransaction;
begin
  if not Transaction.Active then
    Transaction.StartTransaction;
end;

procedure TCcConnectionSQLDB.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionSQLDB then begin
    Database := TSQLConnector.Create(self);
    Database.Assign(TCcConnectionSQLDB(Source).Database);

    Transaction := TSQLTransaction.Create(self);
    Transaction.Database := Database;
    Transaction.Assign(TCcConnectionSQLDB(Source).Transaction);

    Database.Transaction := Transaction;

    FAutoCreatedConnection := True;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionSQLDB]);
end;

function TCcConnectionSQLDB.GetConnectorConnected: Boolean;
begin
  Result := False;
  if Assigned(Database) then
    Result := Database.Connected;
end;

function TCcConnectionSQLDB.GetInTransaction: Boolean;
begin
  Result := False;
  if Assigned(Transaction) then
    Result := Transaction.Active;
end;

procedure TCcConnectionSQLDB.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FTransaction then
      SetTransaction(nil)
    else if AComponent = FDatabase then
      SetDatabase(nil);
  end;
  inherited Notification(AComponent, Operation);
end;

initialization
	RegisterDBConnector(TCcConnectionSQLDB, TCcConnectionSQLDB.ConnectorName);

end.
