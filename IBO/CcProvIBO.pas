unit CcProvIBO;

{$I CC.INC}

interface

uses Classes, Sysutils, DB, IB_Components, IB_Access, IBODataset, CcInterbaseConn, CcProviders;

type

TCcQueryIBO = class(TCcAbstractQueryObject)
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
    procedure SetFieldValue(Field: TCCField; Value: Variant);override;
  public
		IBQuery :TIBOQuery;
    constructor Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
    destructor Destroy; override;
end;

TCcConnectionIBO = class(TCcInterbaseConnection)
  private
    FRecVersion: Boolean;
    FIsolation: TIB_Isolation;
    FLockWait: Boolean;
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
		function GetInTransaction: Boolean;override;
  public
    IBDatabase :TIB_Connection;
    IBTransaction :TIB_Transaction;
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
		procedure Assign(Source: TPersistent);override;
  published
    property LockWait: Boolean read FLockWait write FLockWait;
    property RecVersion: Boolean read FRecVersion write FRecVersion;
    property Isolation: TIB_Isolation read FIsolation write FIsolation;
end;

procedure Register;

implementation

uses CcInterbase {$IFDEF CC_UseVariants}, Variants{$ENDIF};

{ TCcProviderIBO }

constructor TCcConnectionIBO.Create(AOwner: TComponent);
begin
  inherited;
  IBDatabase := TIB_Connection.Create(Self);
  IBDatabase.LoginPrompt := False;
  IBDatabase.DefaultNoTrimming := True;
  IBTransaction := TIB_Transaction.Create(Self);
  IBTransaction.IB_Connection := IBDatabase;
  AddDBAdaptor(TCcInterbaseAdaptor);
  DBType := TCcInterbaseAdaptor.GetAdaptorName;
end;

procedure TCcConnectionIBO.DoDisconnect;
begin
  IBDatabase.Connected := False;
end;

procedure TCcConnectionIBO.DoCommit;
begin
  IBTransaction.Commit;
end;

procedure TCcConnectionIBO.DoCommitRetaining;
begin
  IBTransaction.CommitRetaining;
end;

procedure TCcConnectionIBO.DoConnect;
begin
//IBDatabase.Path := DBName;
  IBDatabase.Database := DBName;
  IBDatabase.Username := UserLogin;
  IBDatabase.CharSet := CharSet;
  IBDatabase.Password := UserPassword;
  IBDatabase.SQLRole := RoleName;
  IBDatabase.SQLDialect := SQLDialect;
  IBDatabase.Connected := True;
end;

procedure TCcConnectionIBO.DoRollback;
begin
  IBTransaction.Rollback;
end;

procedure TCcConnectionIBO.DoRollbackRetaining;
begin
  IBTransaction.RollbackRetaining;
end;

procedure TCcConnectionIBO.DoStartTransaction;
begin
  if (not IBTransaction.InTransaction) then begin
    IBTransaction.Isolation := Isolation;
    IBTransaction.RecVersion := RecVersion;
    IBTransaction.LockWait := LockWait;
    IBTransaction.StartTransaction;
  end;
end;

function TCcConnectionIBO.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryIBO;
begin
  q := TCcQueryIBO.Create(Self, qry, nID, qry.SelectStatement);
	q.IBQuery.IB_Connection := IBDatabase;
  q.IBQuery.IB_Transaction := IBTransaction;
  Result := q;
end;

procedure TCcConnectionIBO.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionIBO then with TCcConnectionIBO(Source) do begin
    Self.RecVersion := RecVersion;
    Self.Isolation := Isolation;
    Self.LockWait := LockWait;
  end;
  inherited;
end;

class function TCcConnectionIBO.ConnectorName: String;
begin
  Result := 'IBO';
end;

{ TCcQueryIBO }

procedure TCcQueryIBO.DoClose;
begin
	IBQuery.Close;
end;

constructor TCcQueryIBO.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
	inherited;
	IBQuery := TIBOQuery.Create((Conn as TCcConnectionIBO).IBDatabase);
  IBQuery.Unidirectional := True;
	IBQuery.ReadOnly := True;
  IBQuery.AutoCalcFields := False;
  IBQuery.AutoFetchAll := False;
end;

destructor TCcQueryIBO.Destroy;
begin
  IBQuery.Free;
  inherited;
end;

procedure TCcQueryIBO.DoExec;
begin
  IBQuery.Open;
end;

function TCcQueryIBO.GetEof: Boolean;
begin
  Result := IBQuery.Eof;
end;

function TCcQueryIBO.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType: TFieldType;
begin
	fType := GetFieldType(FieldName, IsParam);
	if (fType <> ftString) and (fType <> ftBcd) then
		Result := 0
	else begin
		if IsParam then
			Result := IBQuery.ParamByName(FieldName).Size
		else
			Result := IBQuery.FieldByName(FieldName).Size;
	end;
end;

function TCcQueryIBO.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
	if IsParam then
		Result := IBQuery.ParamByName(FieldName).DataType
	else
		Result := IBQuery.FieldByName(FieldName).DataType
end;

function TCcQueryIBO.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
		Result := IBQuery.Params[Field.Index].Value
	else
    Result := IBQuery.Fields[Field.Index].Value;
end;

function TCcQueryIBO.GetRowsAffected: Integer;
begin
  Result := IBQuery.RowsAffected;
end;

procedure TCcQueryIBO.DoNext;
begin
  if not IBQuery.Eof then
    IBQuery.Next;
end;

procedure TCcQueryIBO.DoPrepare(SQLText:String);
begin
  IBQuery.SQL.Text := SQLText;
	IBQuery.Prepare;
end;

procedure TCcQueryIBO.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to IBQuery.FieldCount-1 do
    FieldList.Add(IBQuery.Fields.Fields[i].FieldName);
end;

procedure TCcQueryIBO.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for i:=0 to IBQuery.ParamCount-1 do
		ParamList.Add(IBQuery.Params.Items[i].Name);
end;

procedure TCcQueryIBO.SetFieldValue(Field: TCCField; Value: Variant);
begin
  if Field.IsParam then begin
    if VarIsNull(Value) or VarIsEmpty(Value) then
      IBQuery.Params[Field.Index].Clear
    else
   		IBQuery.Params[Field.Index].Value := Value
  end
	else
    IBQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryIBO.SetParamCheck(lParamCheck: Boolean);
begin
  IBQuery.ParamCheck := lParamCheck;
end;

procedure TCcQueryIBO.DoUnPrepare;
begin
  IBQuery.Unprepare;
end;


function TCcConnectionIBO.GetConnectorConnected: Boolean;
begin
	Result := IBDatabase.Connected;
end;

function TCcConnectionIBO.GetInTransaction: Boolean;
begin
	Result := IBTransaction.InTransaction;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionIBO]);
end;

initialization
  RegisterDBConnector(TCcConnectionIBO, TCcConnectionIBO.ConnectorName);

end.
