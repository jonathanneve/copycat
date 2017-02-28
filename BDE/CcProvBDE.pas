unit CcProvBDE;

interface

uses Classes, Sysutils, DB, pFIBQuery, pFIBDatabase, pFIBProps, CcProviders, CcInterbaseConn;
                                             
type

TCcQueryFIB = class(TCcAbstractQueryObject)
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
    FIBQuery :TpFIBQuery;
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer);override;
end;

TCcConnectionFIB = class(TCcInterbaseConnection)
  private
    function GetClientDLL: String;
    procedure SetClientDLL(const Value: String);
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;override;
    procedure DoDisconnect;override;
    procedure DoConnect; override;
    procedure DoCommit;override;
    procedure DoCommitRetaining;override;
    procedure DoRollback;override;
    procedure DoRollbackRetaining;override;
    procedure DoStartTransaction;override;
  public
    FIBDatabase: TpFIBDatabase;
    FIBTransaction :TpFIBTransaction;
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    procedure Assign(Source: TPersistent);override;
  published
    property ClientDLL: String read GetClientDLL write SetClientDLL;
end;

procedure Register;

implementation

uses FIBQuery, FIBDatabase, CcInterbase, ibase;

function TCcConnectionFIB.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryFIB;
begin
  q := TCcQueryFIB.Create(Self, qry, nID);
  q.FIBQuery.Database := FIBDatabase;
  q.FIBQuery.Transaction := FIBTransaction;
  Result := q;
end;

class function TCcConnectionFIB.ConnectorName: String;
begin
  Result := 'FIB';
end;

{ TCCQueryFIB }

procedure TCcQueryFIB.DoClose;
begin
  FIBQuery.Close;
end;

constructor TCcQueryFIB.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer);
begin
  inherited;
  FIBQuery := TpFIBQuery.Create(Conn);
  FIBQuery.Options := [qoNoForceIsNull];
end;

procedure TCcQueryFIB.DoExec;
begin
  FIBQuery.ExecQuery;
end;

function TCcQueryFIB.GetEof: Boolean;
begin
  Result := FIBQuery.Eof;
end;

function TCcQueryFIB.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  field :TFIBXSQLVAR;
  fType :TFieldType;
begin
  if IsParam then
    field := FIBQuery.ParamByName(FieldName)
  else
    field := FIBQuery.Fields[FIBQuery.FieldIndex[FieldName]];

  fType := GetFieldType(FieldName, IsParam);
  if fType = ftString then
    Result := field.Size
  else if (fType = ftBlob) or (fType = ftMemo) or (fType = ftArray) then
    Result := Sizeof(TISC_QUAD)
  else
    Result := 0;
end;

function TCcQueryFIB.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
var
	field :TFIBXSQLVAR;
	q:TQuery;
begin
  q.

  if IsParam then
    field := FIBQuery.ParamByName(FieldName)
  else
    field := FIBQuery.FN(FieldName);
  with field.Data^ do
    case sqltype and (not 1) of
      IB_SQL_VARYING, IB_SQL_TEXT: Result := ftString;

      IB_SQL_DOUBLE, IB_SQL_FLOAT, IB_SQL_D_FLOAT:
        Result := ftFloat;

      IB_SQL_LONG, IB_SQL_SHORT:
        if sqlscale < 0 then Result := ftFloat
        else if sqllen <> 4 then Result := ftSmallInt
        else Result := ftInteger;

      IB_SQL_DATE: Result := ftDateTime;
      IB_SQL_TYPE_TIME: Result := ftTime;
      IB_SQL_TYPE_DATE: Result := ftDate;

      IB_SQL_BLOB :
        if sqlsubtype = 1 then Result := ftMemo
        else Result := ftBlob;

      IB_SQL_INT64:
        Result := ftFloat;

      IB_SQL_BOOLEAN:
        Result := ftBoolean;

      IB_SQL_ARRAY:
        Result := ftBytes;

      else
        Result := ftUnknown;
    end;
end;

function TCcQueryFIB.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := FIBQuery.Params[Field.Index].Value
  else
    Result := FIBQuery.Fields[Field.Index].Value;
end;

function TCcQueryFIB.GetRowsAffected: Integer;
begin
  Result := FIBQuery.RowsAffected;
end;

procedure TCcQueryFIB.DoNext;
begin
  FIBQuery.Next;
end;

procedure TCcQueryFIB.DoPrepare(SQLText:String);
begin
  FIBQuery.BeginModifySQLText;
  FIBQuery.SQL.Text := SQLText;
  FIBQuery.EndModifySQLText;
  FIBQuery.Prepare;
end;

procedure TCcQueryFIB.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to FIBQuery.FieldCount-1 do
    FieldList.Add(FIBQuery.Fields[i].Name);
end;

procedure TCcQueryFIB.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to FIBQuery.Params.Count-1 do
    ParamList.Add(FIBQuery.Params.Vars[i].Name);
end;

procedure TCcQueryFIB.DoUnPrepare;
begin
  FIBQuery.FreeHandle;
end;

procedure TCcQueryFIB.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
//  (Connection.DBAdaptor as TCcInterbaseAdaptor).CheckDateValue(Field, Value);
  if Field.IsParam then
    FIBQuery.Params[Field.Index].Value := Value
  else
    FIBQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryFIB.SetParamCheck(lParamCheck: Boolean);
begin
  FIBQuery.ParamCheck := lParamCheck;
end;

constructor TCcConnectionFIB.Create(AOwner: TComponent);
begin
  inherited;
  FIBDatabase := TpFIBDatabase.Create(Self);
  FIBDatabase.SynchronizeTime := False;
  ClientDLL := FIBDatabase.LibraryName;

  FIBTransaction := TpFIBTransaction.Create(Self);
  FIBTransaction.DefaultDatabase := FIBDatabase;
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBDatabase.DefaultUpdateTransaction := FIBTransaction;

  AddDBAdaptor(TCcInterbaseAdaptor);
  SQLDialect := 1;
end;

procedure TCcConnectionFIB.DoDisconnect;
begin
  FIBDatabase.Connected := False;
end;

procedure TCcConnectionFIB.DoCommit;
begin
  FIBTransaction.Commit;
end;

procedure TCcConnectionFIB.DoCommitRetaining;
begin
  FIBTransaction.CommitRetaining;
end;

procedure TCcConnectionFIB.DoConnect;
begin
  inherited;
  FIBDatabase.DatabaseName := DBName;
  FIBDatabase.ConnectParams.UserName := UserLogin;
  FIBDatabase.ConnectParams.CharSet := CharSet;
  FIBDatabase.ConnectParams.Password := UserPassword;
  FIBDatabase.ConnectParams.RoleName := RoleName;
  FIBDatabase.SQLDialect := SQLDialect;
  FIBDatabase.LibraryName := ClientDLL;
  FIBDatabase.Connected := True;
end;

procedure TCcConnectionFIB.DoRollback;
begin
  FIBTransaction.Rollback;
end;

procedure TCcConnectionFIB.DoRollbackRetaining;
begin
  FIBTransaction.RollbackRetaining;
end;

procedure TCcConnectionFIB.DoStartTransaction;
begin
  FIBTransaction.TPBMode := tpbDefault;
  FIBTransaction.TRParams := TRParams;

//  FIBTransaction.TRParams.BeginUpdate;
//  try
//    FIBTransaction.TRParams.Assign(TRParams);
//  finally
//    FIBTransaction.TRParams.EndUpdate;
//  end;

  FIBTransaction.StartTransaction;
end;

procedure TCcConnectionFIB.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionFIB then with TCcConnectionFIB(Source) do begin
    Self.ClientDLL := ClientDLL;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionFIB]);
end;

function TCcConnectionFIB.GetClientDLL: String;
begin
  Result := ConnectionParams.Values['CLIENT_DLL'];
end;

procedure TCcConnectionFIB.SetClientDLL(const Value: String);
begin
  ConnectionParams.Values['CLIENT_DLL'] := Value;
end;

initialization
  RegisterDBConnector(TCcConnectionFIB, TCcConnectionFIB.ConnectorName);

end.
