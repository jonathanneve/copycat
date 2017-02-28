unit CcProvMyDAC;

interface

uses Classes, Sysutils, DB, CcProviders, DBAccess, MyAccess;
                                             
type

TCcQueryMyDAC = class(TCcAbstractQueryObject)
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
    MyQuery :TMyQuery;
		constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
    destructor Destroy; override;
end;

TCcConnectionMyDAC = class(TCcConnection)
  private
    FMyConnection: TMyConnection;
    procedure SetMyConnection(const Value: TMyConnection);
  protected
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;override;
    procedure DoDisconnect;override;
    procedure DoConnect; override;
    procedure DoCommit;override;
    procedure DoCommitRetaining;override;
    procedure DoRollback;override;
    procedure DoRollbackRetaining;override;
    procedure DoStartTransaction;override;
    function GetMyConnection: TMyConnection;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    procedure Assign(Source: TPersistent);override;
  published
    property MyConnection: TMyConnection read FMyConnection write SetMyConnection;
    property DBType;
    property DBVersion;
end;

procedure Register;

implementation

uses CcMySQL;

function TCcConnectionMyDAC.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryMyDAC;
  c: TMyConnection;
begin
	c := GetMyConnection;
  q := TCcQueryMyDAC.Create(Self, qry, nID, qry.SelectStatement);
  q.MyQuery.Connection := c;
  Result := q;
end;

class function TCcConnectionMyDAC.ConnectorName: String;
begin
  Result := 'MyDAC';
end;

{ TCcQueryMyDAC }

procedure TCcQueryMyDAC.DoClose;
begin
  MyQuery.Close;
end;

constructor TCcQueryMyDAC.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
	MyQuery := TMyQuery.Create(Conn);
end;

destructor TCcQueryMyDAC.Destroy;
begin
  MyQuery.Free;
  inherited;
end;

procedure TCcQueryMyDAC.DoExec;
begin
  MyQuery.Execute;
end;

function TCcQueryMyDAC.GetEof: Boolean;
begin
  Result := MyQuery.Eof;
end;

function TCcQueryMyDAC.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType :TFieldType;
  nSize: Integer;
begin
  if IsParam then
    nSize := MyQuery.ParamByName(FieldName).Size
  else
    nSize := MyQuery.FieldByName(FieldName).Size;

  fType := GetFieldType(FieldName, IsParam);
  if fType = ftString then
    Result := nSize
//  else if (fType = ftBlob) or (fType = ftMemo) or (fType = ftArray) then
//    Result := Sizeof(TISC_QUAD)
  else
    Result := 0;
end;

function TCcQueryMyDAC.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := MyQuery.ParamByName(FieldName).DataType
  else
    Result := MyQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryMyDAC.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then
    Result := MyQuery.Params[Field.Index].Value
  else
    Result := MyQuery.Fields[Field.Index].Value;
end;

function TCcQueryMyDAC.GetRowsAffected: Integer;
begin
  Result := MyQuery.RowsAffected;
end;

procedure TCcQueryMyDAC.DoNext;
begin
  MyQuery.Next;
end;

procedure TCcQueryMyDAC.DoPrepare(SQLText:String);
begin
  MyQuery.SQL.Text := SQLText;
  MyQuery.Prepare;
end;

procedure TCcQueryMyDAC.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to MyQuery.FieldCount-1 do
    FieldList.Add(MyQuery.Fields[i].FieldName);
end;

procedure TCcQueryMyDAC.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to MyQuery.Params.Count-1 do
    ParamList.Add(MyQuery.Params[i].Name);
end;

procedure TCcQueryMyDAC.DoUnPrepare;
begin
  MyQuery.UnPrepare;
end;

procedure TCcQueryMyDAC.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then
    MyQuery.Params[Field.Index].Value := Value
  else
    MyQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryMyDAC.SetParamCheck(lParamCheck: Boolean);
begin
  MyQuery.ParamCheck := lParamCheck;
end;

constructor TCcConnectionMyDAC.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcMySQLAdaptor);
end;

procedure TCcConnectionMyDAC.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FMyConnection then
      SetMyConnection(nil);
  end;
  inherited;
end;

procedure TCcConnectionMyDAC.SetMyConnection(const Value: TMyConnection);
begin
  if Assigned(FMyConnection) then
    FMyConnection.RemoveFreeNotification(Self);

  FMyConnection := Value;

  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TCcConnectionMyDAC.DoDisconnect;
begin
  GetMyConnection.Disconnect;
end;

procedure TCcConnectionMyDAC.DoCommit;
begin
  GetMyConnection.Commit;
end;

procedure TCcConnectionMyDAC.DoCommitRetaining;
begin
  GetMyConnection.Commit;
end;

procedure TCcConnectionMyDAC.DoConnect;
begin
  inherited;
  GetMyConnection.Connected := False;
  GetMyConnection.IsolationLevel := ilRepeatableRead;
  GetMyConnection.Connect;
end;

procedure TCcConnectionMyDAC.DoRollback;
begin
  GetMyConnection.Rollback;
end;

procedure TCcConnectionMyDAC.DoRollbackRetaining;
begin
  GetMyConnection.Rollback;
end;

procedure TCcConnectionMyDAC.DoStartTransaction;
begin
  GetMyConnection.StartTransaction;
end;

procedure TCcConnectionMyDAC.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionMyDAC then with TCcConnectionMyDAC(Source) do begin
    Self.MyConnection := MyConnection;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionMyDAC]);
end;

function TCcConnectionMyDAC.GetMyConnection: TMyConnection;
begin
  if Assigned(MyConnection) then Result := MyConnection
  else raise Exception.Create('MySQL connection object must be assigned');
end;

initialization
  RegisterDBConnector(TCcConnectionMyDAC, TCcConnectionMyDAC.ConnectorName);

end.
