unit CcProvZeos;

interface

uses Classes, Sysutils, DB, CcProviders, ZConnection, ZAbstractRODataset, ZDataset, ZDbcIntfs;

type

TCcQueryZeos = class(TCcAbstractQueryObject)
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
    ZeosQuery :TZReadOnlyQuery;
		constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
    destructor Destroy; override;
end;

TCcConnectionZeos = class(TCcConnection)
  private
    FProperties: TStringList;
    FPort: Integer;
    FHostName: String;
    FCatalog: String;
    function GetTransactIsolationLevel: TZTransactIsolationLevel;
    procedure SetTransactIsolationLevel(
      const Value: TZTransactIsolationLevel);
    procedure SetDatabase(const Value: String);
    function GetDatabase: String;
    function GetUser: String;
    procedure SetUser(const Value: String);
    procedure SetProperties(const Value: TStringList);
    function GetProtocol: String;
    procedure SetProtocol(const Value: String);
    function GetPassword: String;
    procedure SetPassword(const Value: String);
    function GetCatalog: String;
    function GetHostName: String;
    function GetPort: Integer;
    procedure SetCatalog(const Value: String);
    procedure SetHostName(const Value: String);
    procedure SetPort(const Value: Integer);
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;override;
		procedure DoDisconnect;override;
    procedure DoConnect; override;
    procedure DoCommit;override;
    procedure DoCommitRetaining;override;
    procedure DoRollback;override;
    procedure DoRollbackRetaining;override;
    procedure DoStartTransaction;override;
    function GetConnectorConnected: Boolean; override;
    function GetInTransaction: Boolean; override;

  public
    ZConnection: TZConnection;
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;
    procedure Assign(Source: TPersistent);override;
  published
    property TransactIsolationLevel: TZTransactIsolationLevel read GetTransactIsolationLevel write SetTransactIsolationLevel;
    property DBType;
    property DBVersion;
    property Database: String read GetDatabase write SetDatabase;
    property User: String read GetUser write SetUser;
    property Protocol: String read GetProtocol write SetProtocol;
    property Password: String read GetPassword write SetPassword;
    property HostName : String read GetHostName write SetHostName;
    property Port : Integer read GetPort write SetPort;
    property Catalog : String read GetCatalog write SetCatalog;
    property Properties : TStringList read FProperties write SetProperties;
end;

implementation

uses CcMySQL, CcInterbase, CcSQLServer, ZMessages;


function TCcConnectionZeos.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryZeos;
begin
  q := TCcQueryZeos.Create(Self, qry, nID, qry.SelectStatement);
  q.ZeosQuery.Connection := ZConnection;
  Result := q;
end;

class function TCcConnectionZeos.ConnectorName: String;
begin
  Result := 'Zeos';
end;

procedure TCcQueryZeos.DoClose;
begin
// Don't call ZeosQuery.Close otherwise it will unprepare the query
//  ZeosQuery.Close;
end;

constructor TCcQueryZeos.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
	ZeosQuery := TZReadOnlyQuery.Create(Conn);
end;

destructor TCcQueryZeos.Destroy;
begin
  ZeosQuery.Free;
  inherited;
end;

procedure TCcQueryZeos.DoExec;
begin
  try
    ZeosQuery.Open;
  except on E: EZSQLException do begin
    if (E.Message <> SCanNotRetrieveResultSetData) then
      raise;
    end;
  end;
end;

function TCcQueryZeos.GetEof: Boolean;
begin
  Result := ZeosQuery.Eof;
end;

function TCcQueryZeos.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType :TFieldType;
begin
  fType := GetFieldType(FieldName, IsParam);
  if fType = ftString then begin
    if IsParam then
      Result := ZeosQuery.ParamByName(FieldName).Size
    else
      Result := ZeosQuery.FieldByName(FieldName).Size;
  end else
    Result := 0;
end;

function TCcQueryZeos.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := ZeosQuery.ParamByName(FieldName).DataType
  else
    Result := ZeosQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryZeos.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := ZeosQuery.Params[Field.Index].Value
  else
    Result := ZeosQuery.Fields[Field.Index].Value;
end;

function TCcQueryZeos.GetRowsAffected: Integer;
begin
  Result := ZeosQuery.RowsAffected;
end;

procedure TCcQueryZeos.DoNext;
begin
  ZeosQuery.Next;
end;

procedure TCcQueryZeos.DoPrepare(SQLText:String);
begin
  ZeosQuery.SQL.Text := SQLText;
end;

procedure TCcQueryZeos.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to ZeosQuery.FieldCount-1 do
    FieldList.Add(ZeosQuery.Fields[i].FieldName);
end;

procedure TCcQueryZeos.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to ZeosQuery.Params.Count-1 do
    ParamList.Add(ZeosQuery.Params[i].Name);
end;

procedure TCcQueryZeos.DoUnPrepare;
begin
  if (ZeosQuery.DbcStatement <> nil) then
    ZeosQuery.DbcStatement.Close;
end;

procedure TCcQueryZeos.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then
    ZeosQuery.Params[Field.Index].Value := Value
  else
    ZeosQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryZeos.SetParamCheck(lParamCheck: Boolean);
begin
  ZeosQuery.ParamCheck := lParamCheck;
end;

constructor TCcConnectionZeos.Create(AOwner: TComponent);
begin
  inherited;
  FProperties := TStringList.Create;

  ZConnection := TZConnection.Create(Self);
  ZConnection.TransactIsolationLevel := tiRepeatableRead;
  ZConnection.LoginPrompt := False;

  AddDBAdaptor(TCcMySQLAdaptor);
  AddDBAdaptor(TCcInterbaseAdaptor);
  AddDBAdaptor(TCcSQLServerAdaptor);
end;

procedure TCcConnectionZeos.DoDisconnect;
begin
  ZConnection.Connected := False;
end;

procedure TCcConnectionZeos.DoCommit;
begin
  ZConnection.Commit;
end;

procedure TCcConnectionZeos.DoCommitRetaining;
begin
  ZConnection.Commit;
  ZConnection.StartTransaction;
end;

procedure TCcConnectionZeos.DoConnect;
var
  i: Integer;
begin
  inherited;

  ZConnection.LoginPrompt := False;

  ZConnection.Database := Database;
  ZConnection.Protocol := Protocol;
  ZConnection.Catalog := Catalog;
  ZConnection.HostName := HostName;
  ZConnection.Port := Port;
  ZConnection.TransactIsolationLevel := TransactIsolationLevel;
  ZConnection.User := User;
  ZConnection.Password := Password;

  ZConnection.Properties.Assign(Properties);
  for i := 0 to Properties.Count-1 do
    ConnectionParams.Values[Properties.Names[i]] := Properties.Values[Properties.Names[i]];

  ZConnection.Connected := True;
end;

procedure TCcConnectionZeos.DoRollback;
begin
  ZConnection.Rollback;
end;

procedure TCcConnectionZeos.DoRollbackRetaining;
begin
  ZConnection.Rollback;
end;

procedure TCcConnectionZeos.DoStartTransaction;
begin
  ZConnection.StartTransaction;
end;

procedure TCcConnectionZeos.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionZeos then with TCcConnectionZeos(Source) do begin
    Self.Port := Port;
    Self.Protocol := Protocol;
    Self.Database := Database;
    Self.Catalog := Catalog;
    Self.HostName := HostName;
    Self.User := User;
    Self.Password := Password;
    Self.TransactIsolationLevel := TransactIsolationLevel;
  end;
  inherited;
end;

function TCcConnectionZeos.GetTransactIsolationLevel: TZTransactIsolationLevel;
begin
  Result := TZTransactIsolationLevel(StrToIntDef(ConnectionParams.Values['TRANS_ISOLATION_LEVEL'], 0));
end;

procedure TCcConnectionZeos.SetTransactIsolationLevel(
  const Value: TZTransactIsolationLevel);
begin
  ConnectionParams.Values['TRANS_ISOLATION_LEVEL'] := IntToStr(Integer(Value));
end;

procedure TCcConnectionZeos.SetDatabase(const Value: String);
begin
  ConnectionParams.Values['DATABASE'] := Value;
end;

function TCcConnectionZeos.GetDatabase: String;
begin
  Result := ConnectionParams.Values['DATABASE'];
end;

function TCcConnectionZeos.GetUser: String;
begin
  Result := ConnectionParams.Values['USER_NAME'];
end;

procedure TCcConnectionZeos.SetUser(const Value: String);
begin
  ConnectionParams.Values['USER_NAME'] := Value;
end;

procedure TCcConnectionZeos.SetProperties(const Value: TStringList);
begin
  FProperties.Assign(Value);
end;

function TCcConnectionZeos.GetProtocol: String;
begin
  Result := ConnectionParams.Values['PROTOCOL'];
end;

procedure TCcConnectionZeos.SetProtocol(const Value: String);
begin
  ConnectionParams.Values['PROTOCOL'] := Value;
end;

function TCcConnectionZeos.GetPassword: String;
begin
  Result := ConnectionParams.Values['PASSWORD'];
end;

procedure TCcConnectionZeos.SetPassword(const Value: String);
begin
  ConnectionParams.Values['PASSWORD'] := Value;
end;

function TCcConnectionZeos.GetCatalog: String;
begin
  Result := ConnectionParams.Values['CATALOG'];
end;

function TCcConnectionZeos.GetConnectorConnected: Boolean;
begin
  Result := ZConnection.Connected;
end;

function TCcConnectionZeos.GetHostName: String;
begin
  Result := ConnectionParams.Values['HOST_NAME'];
end;

function TCcConnectionZeos.GetInTransaction: Boolean;
begin
  Result := ZConnection.InTransaction;
end;

function TCcConnectionZeos.GetPort: Integer;
begin
  Result := StrToIntDef(ConnectionParams.Values['PORT'], 0);
end;

procedure TCcConnectionZeos.SetCatalog(const Value: String);
begin
  ConnectionParams.Values['CATALOG'] := Value;
end;

procedure TCcConnectionZeos.SetHostName(const Value: String);
begin
  ConnectionParams.Values['HOST_NAME'] := Value;
end;

procedure TCcConnectionZeos.SetPort(const Value: Integer);
begin
  ConnectionParams.Values['PORT'] := IntToStr(Value);
end;

destructor TCcConnectionZeos.Destroy;
begin
  FProperties.Free;
end;

initialization
  RegisterDBConnector(TCcConnectionZeos, TCcConnectionZeos.ConnectorName);


end.
