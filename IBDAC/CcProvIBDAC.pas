unit CcProvIBDAC;

interface

uses Classes, Sysutils, DB, CcProviders, CcInterbaseConn, DBAccess, IBC;

type

  TCcQueryIBDAC = class(TCcAbstractQueryObject)
  protected
    function GetRowsAffected: Integer; override;
    function GetEof: Boolean; override;
    procedure DoInitParams(ParamList: TStringList); override;
    procedure DoInitFields(FieldList: TStringList); override;
    procedure DoExec; override;
    procedure DoPrepare(SQLText: string); override;
    procedure DoUnPrepare; override;
    procedure SetParamCheck(lParamCheck: Boolean); override;
    procedure DoClose; override;
    procedure DoNext; override;
    function GetFieldType(FieldName: string; IsParam: Boolean): TFieldType; override;
    function GetFieldSize(FieldName: string; IsParam: Boolean): Integer; override;
    function GetFieldValue(Field: TCCField): Variant; override;
    procedure SetFieldValue(Field: TCCField; Val: Variant); override;
  public
    IbcQuery: TIBCQuery;
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean); override;
    destructor Destroy; override;
  end;



  TCcConnectionIBDAC = class(TCcInterbaseConnection)
  private
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject; override;
    procedure DoDisconnect; override;
    procedure DoConnect; override;
    procedure DoCommit; override;
    procedure DoCommitRetaining; override;
    procedure DoRollback; override;
    procedure DoRollbackRetaining; override;
    procedure DoStartTransaction; override;
    function GetConnectorConnected: Boolean; override;
    function GetInTransaction: Boolean; override;
    function GetIbcConnection: TIBCConnection;
    function GetValidIbcConnection: TIBCConnection;
    procedure SetIbcConnection(c: TIBCConnection);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    FIbcConnectionAssigned: TIBCConnection;
    FIbcConnectionDefault: TIBCConnection;
    class function ConnectorName: string; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property IbcConnection: TIBCConnection read GetIbcConnection write SetIbcConnection;
    property DBType;
    property DBVersion;
  end;

procedure Register;

implementation

uses CcInterbase, IBCClasses;

function TCcConnectionIBDAC.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryIBDAC;
  c: TIBCConnection;
begin
  c := GetValidIbcConnection;
  q := TCcQueryIBDAC.Create(Self, qry, nID, qry.SelectStatement);
  q.IbcQuery.Connection := c;
  Result := q;
end;

procedure TCcConnectionIBDAC.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FIbcConnectionAssigned then
      SetIbcConnection(nil);
  end;
  inherited;
end;

class function TCcConnectionIBDAC.ConnectorName: string;
begin
  Result := 'IBDAC';
end;

{ TCcQueryMyDAC }

procedure TCcQueryIBDAC.DoClose;
begin
  IbcQuery.Close;
end;

constructor TCcQueryIBDAC.Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
  IbcQuery := TIBCQuery.Create(Conn);
  // Wymusza odpowiednie ustawienie typu parametru przed wstawieniem wartoœci
  IbcQuery.Options.DescribeParams := True;
  IbcQuery.AutoCommit := False;
  IbcQuery.ReadOnly := True;
  IbcQuery.UniDirectional := True;
  IbcQuery.Options.AutoClose := True;
  IbcQuery.Options.CacheBlobs := False;
  IbcQuery.Options.QueryRecCount := False;
  IbcQuery.Options.DefaultValues := False;
end;

destructor TCcQueryIBDAC.Destroy;
begin
  IbcQuery.Free;
  inherited;
end;

procedure TCcQueryIBDAC.DoExec;
begin
  IbcQuery.Execute;
end;

function TCcQueryIBDAC.GetEof: Boolean;
begin
  Result := IbcQuery.Eof;
end;

function TCcQueryIBDAC.GetFieldSize(FieldName: string; IsParam: Boolean): Integer;
begin
  if IsParam then
    Result := IbcQuery.ParamByName(FieldName).Size
  else
    Result := IbcQuery.FieldByName(FieldName).Size;
end;

function TCcQueryIBDAC.GetFieldType(FieldName: string;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := IbcQuery.ParamByName(FieldName).DataType
  else
    Result := IbcQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryIBDAC.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := IbcQuery.Params[Field.Index].Value
  else
    Result := IbcQuery.Fields[Field.Index].Value;
end;

function TCcQueryIBDAC.GetRowsAffected: Integer;
begin
  Result := IbcQuery.RowsAffected;
end;

procedure TCcQueryIBDAC.DoNext;
begin
  IbcQuery.Next;
end;

procedure TCcQueryIBDAC.DoPrepare(SQLText: string);
begin
  IbcQuery.SQL.Text := SQLText;
  IbcQuery.Prepare;
end;

procedure TCcQueryIBDAC.DoInitFields(FieldList: TStringList);
var
  i: Integer;
begin
  for i := 0 to IbcQuery.FieldCount - 1 do
    FieldList.Add(IbcQuery.Fields[i].FieldName);
end;

procedure TCcQueryIBDAC.DoInitParams(ParamList: TStringList);
var
  i: Integer;
begin
  for i := 0 to IbcQuery.Params.Count - 1 do
    ParamList.Add(IbcQuery.Params[i].Name);
end;

procedure TCcQueryIBDAC.DoUnPrepare;
begin
  IbcQuery.UnPrepare;
end;

procedure TCcQueryIBDAC.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then
    IbcQuery.Params[Field.Index].Value := Value
  else
    IbcQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryIBDAC.SetParamCheck(lParamCheck: Boolean);
begin
  IbcQuery.ParamCheck := lParamCheck;
end;

constructor TCcConnectionIBDAC.Create(AOwner: TComponent);
begin
  inherited;
  FIbcConnectionAssigned := nil;
  FIbcConnectionDefault := TIBCConnection.Create(Self);
  FIbcConnectionDefault.LoginPrompt := False;
  AddDBAdaptor(TCcInterbaseAdaptor);
end;

procedure TCcConnectionIBDAC.DoDisconnect;
begin
  GetValidIbcConnection.Disconnect;
end;

destructor TCcConnectionIBDAC.Destroy;
begin
  FIbcConnectionDefault.Free;
  FIbcConnectionDefault := nil;
  inherited;
end;

procedure TCcConnectionIBDAC.DoCommit;
begin
  GetValidIbcConnection.Commit;
end;

procedure TCcConnectionIBDAC.DoCommitRetaining;
begin
  GetValidIbcConnection.CommitRetaining;
end;

procedure SetConnectionString(IbcConnection: TIBCConnection; sConnectionString: string);
var
  sServer: string;
  sDatabase: string;
  IBCProtocol: TIBCProtocol;

  iPos1: Integer;
  iPos2: Integer;
begin
  sServer := '';
  sDatabase := '';
  IBCProtocol := TCP;

  iPos1 := Pos(':', sConnectionString);
  iPos2 := Pos(':\', sConnectionString);

  // Uwaga obs³uguje tylko protokó³ lokalny i TCP
  if ((iPos1 > 0) and ((iPos2 = 0) or (iPos2 > iPos1))) then
  begin
    // TCP
    sServer := Copy(sConnectionString, 1, iPos1 - 1);
    sDatabase := Copy(sConnectionString, iPos1 + 1, Length(sConnectionString));
  end
  else
  begin
    // local XNET
    sServer := '';
    sDatabase := sConnectionString;
  end;

  IbcConnection.Server := sServer;
  IbcConnection.Database := sDatabase;
  IbcConnection.Options.Protocol := IBCProtocol;
end;

procedure TCcConnectionIBDAC.DoConnect;
begin
  inherited;
  if IbcConnection.Connected = True then
    IbcConnection.Disconnect;

  SetConnectionString(IbcConnection, DBName);
  IbcConnection.Username := UserLogin;
  IbcConnection.Password := UserPassword;
  IbcConnection.LoginPrompt := False;
  IbcConnection.Options.Role := RoleName;
  IbcConnection.Options.Charset := Charset;
  IbcConnection.SQLDialect := SQLDialect;
  // Do poprawnej obs³ugi BLOB-a tekstowego SubType 1 (ju¿ niewymagane)
  // IbcConnection.Options.EnableMemos := True;
  IbcConnection.AutoCommit := False;

  // RepeatableRead;
  IbcConnection.DefaultTransaction.IsolationLevel := iblSnapshot;
  IbcConnection.Connect;

end;

procedure TCcConnectionIBDAC.DoRollback;
begin
  GetValidIbcConnection.Rollback;
end;

procedure TCcConnectionIBDAC.DoRollbackRetaining;
begin
  GetValidIbcConnection.RollbackRetaining;
end;

procedure TCcConnectionIBDAC.DoStartTransaction;
begin
  GetValidIbcConnection.StartTransaction;
end;

function TCcConnectionIBDAC.GetConnectorConnected: Boolean;
begin
  Result := False;
  if GetIbcConnection <> nil then
    Result := GetIbcConnection.Connected;
end;

function TCcConnectionIBDAC.GetInTransaction: Boolean;
begin
  Result := False;
  if GetIbcConnection <> nil then
    Result := GetIbcConnection.DefaultTransaction.Active;
end;

function TCcConnectionIBDAC.GetValidIbcConnection: TIBCConnection;
begin
  Result := GetIbcConnection;
  if Result = nil then
    raise Exception.Create('IBDAC connection object must be assigned');
end;

procedure TCcConnectionIBDAC.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionIBDAC then
    with TCcConnectionIBDAC(Source) do
    begin
      Self.IbcConnection.AssignConnect(IbcConnection);
    end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionIBDAC]);
end;

function TCcConnectionIBDAC.GetIbcConnection: TIBCConnection;
begin
  if (Assigned(FIbcConnectionAssigned)) then
    Result := FIbcConnectionAssigned
  else
    if (Assigned(FIbcConnectionDefault)) then
    Result := FIbcConnectionDefault
  else
    Result := nil;
end;

procedure TCcConnectionIBDAC.SetIbcConnection(c: TIBCConnection);
begin
  if Assigned(FIbcConnectionAssigned) then
    FIbcConnectionAssigned.RemoveFreeNotification(Self);

  FIbcConnectionAssigned := c;

  if Assigned(c) then
    c.FreeNotification(Self);
end;

initialization

RegisterDBConnector(TCcConnectionIBDAC, TCcConnectionIBDAC.ConnectorName);

end.
