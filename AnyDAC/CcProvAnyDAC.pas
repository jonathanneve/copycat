unit CcProvAnyDAC;

interface

uses Classes, Sysutils, DB, CcProviders, daADCompClient;

type

TCcQueryAnyDAC = class(TCcAbstractQueryObject)
  private
    FADQuery: TADQuery;
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
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer);override;
    property ADQuery : TADQuery read FADQuery;
end;

TCcAbstractConnectionAnyDAC = class(TCcConnection)
  private
    FADConnection: TADConnection;
		    FAutoCreatedConnection: Boolean;
    procedure SetADConnection(const Value: TADConnection);
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
    function RowsAffectedSupported: Boolean;override;
  public
    procedure Assign(Source: TPersistent);override;
  published
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    property ADConnection: TADConnection read FADConnection write SetADConnection;

end;

TCcConnectionAnyDAC = class(TCcAbstractConnectionAnyDAC)
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
  published
    property DBType;
    property DBVersion;
end;

procedure Register;

implementation

uses CcMySQL, CcSQLServer, CcInterbase;

function TCcAbstractConnectionAnyDAC.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryAnyDAC;
begin
  q := TCcQueryAnyDAC.Create(Self, qry, nID);
  q.ADQuery.Connection := ADConnection;
  Result := q;
end;

class function TCcConnectionAnyDAC.ConnectorName: String;
begin
  Result := 'FireDAC';
end;

procedure TCcQueryAnyDAC.DoClose;
begin
  ADQuery.Close;
end;

constructor TCcQueryAnyDAC.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer);
begin
  inherited;
  FADQuery := TADQuery.Create(Conn);
end;

procedure TCcQueryAnyDAC.DoExec;
begin
	try
		ADQuery.Open;
	except
		ADQuery.ExecSQL;
  end; 
end;

function TCcQueryAnyDAC.GetEof: Boolean;
begin
  Result := ADQuery.Eof;
end;

function TCcQueryAnyDAC.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType :TFieldType;
begin
//  fType := GetFieldType(FieldName, IsParam);
//  if fType = ftString then begin
		if IsParam then
			Result := ADQuery.ParamByName(FieldName).Size
		else
			Result := ADQuery.FieldByName(FieldName).Size;
//  end else
//    Result := 0;
end;

function TCcQueryAnyDAC.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := ADQuery.ParamByName(FieldName).DataType
  else
    Result := ADQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryAnyDAC.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := ADQuery.Params[Field.Index].Value
  else
    Result := ADQuery.Fields[Field.Index].Value;
end;

function TCcQueryAnyDAC.GetRowsAffected: Integer;
begin
  Result := ADQuery.RowsAffected;
end;

procedure TCcQueryAnyDAC.DoNext;
begin
  ADQuery.Next;
end;

procedure TCcQueryAnyDAC.DoPrepare(SQLText:String);
begin
  ADQuery.SQL.Text := SQLText;
//  ADQuery.Prepare;
end;

procedure TCcQueryAnyDAC.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to ADQuery.FieldCount-1 do
    FieldList.Add(ADQuery.Fields[i].FieldName);
end;

procedure TCcQueryAnyDAC.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to ADQuery.Params.Count-1 do
    ParamList.Add(ADQuery.Params[i].Name);
end;

procedure TCcQueryAnyDAC.DoUnPrepare;
begin
  if ADQuery.Prepared then
    ADQuery.UnPrepare;
end;

procedure TCcQueryAnyDAC.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then
    ADQuery.Params[Field.Index].Value := Value
  else
    ADQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryAnyDAC.SetParamCheck(lParamCheck: Boolean);
begin
	ADQuery.ResourceOptions.DirectExecute := not lParamCheck;
end;

function TCcAbstractConnectionAnyDAC.GetConnectorConnected: Boolean;
begin
	Result := False;
  if Assigned(ADConnection) then
    Result := ADConnection.Connected;
end;

function TCcAbstractConnectionAnyDAC.GetInTransaction: Boolean;
begin
	Result := False;
  if Assigned(ADTransaction) then
    Result := ADTransaction.Active;
end;

constructor TCcConnectionAnyDAC.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcMySQLAdaptor);
  AddDBAdaptor(TCcInterbaseAdaptor);
  AddDBAdaptor(TCcSQLServerAdaptor);
end;

function TCcAbstractConnectionAnyDAC.RowsAffectedSupported: Boolean;
begin
  Result := False;
end;

procedure TCcAbstractConnectionAnyDAC.DoDisconnect;
begin
  ADConnection.Connected := False;
end;

constructor TCcAbstractConnectionAnyDAC.Create(AOwner: TComponent);
begin
  inherited;
  FAutoCreatedConnection := False;
end;

destructor TCcAbstractConnectionAnyDAC.Destroy;
begin
  if FAutoCreatedConnection then begin
		ADConnection.Free;
  end;
  inherited;
end;

procedure TCcAbstractConnectionAnyDAC.DoCommit;
begin
	if ADConnection.InTransaction then
     ADConnection.Commit;
end;

procedure TCcAbstractConnectionAnyDAC.DoCommitRetaining;
begin
	if ADConnection.InTransaction then
  begin
		ADConnection.Commit;
		ADConnection.StartTransaction;
  end;
end;

procedure TCcAbstractConnectionAnyDAC.DoConnect;
begin
  inherited;
	FADConnection.Connected := False;
  FADConnection.Connected := True;
end;

procedure TCcAbstractConnectionAnyDAC.DoRollback;
begin
	if ADConnection.InTransaction then
		ADConnection.Rollback;
end;

procedure TCcAbstractConnectionAnyDAC.DoRollbackRetaining;
begin
	if ADConnection.InTransaction then begin
		 ADConnection.Rollback;
		 ADConnection.StartTransaction;
	end;
end;

procedure TCcAbstractConnectionAnyDAC.DoStartTransaction;
begin
	if not ADConnection.InTransaction then
		ADConnection.StartTransaction;
end;

procedure TCcAbstractConnectionAnyDAC.Assign(Source: TPersistent);
begin
  if Source is TCcAbstractConnectionAnyDAC then begin
    ADConnection := TADConnection.Create(self);
		ADConnection.Assign(TCcAbstractConnectionAnyDAC(Source).ADConnection);

    FAutoCreatedConnection := True;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionAnyDAC]);
end;

procedure TCcAbstractConnectionAnyDAC.SetADConnection(
  const Value: TADConnection);
begin
	FADConnection := Value;
	if Assigned(FADConnection) then
	  FADConnection.TxOptions.AutoCommit := False;
end;

initialization
  RegisterDBConnector(TCcConnectionAnyDAC, TCcConnectionAnyDAC.ConnectorName);

end.
