unit CcProvFireDAC;

interface

{$I CC.INC}

uses Classes, Sysutils, DB, CcProviders, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type

TCcQueryFireDAC = class(TCcAbstractQueryObject)
  private
    FFDQuery: TFDQuery;
    procedure FDQueryAfterClose(DataSet: TDataSet);
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
    destructor Destroy;override;
    property FDQuery : TFDQuery read FFDQuery;
end;

TCcAbstractConnectionFireDAC = class(TCcConnection)
  private
    FFDConnection: TFDConnection;
    FFDTransaction: TFDTransaction;
    FAutoCreatedConnection: Boolean;
    procedure SetFDTransaction(const Value: TFDTransaction);
    procedure SetFDConnection(const Value: TFDConnection);
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
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    procedure Assign(Source: TPersistent);override;
  published
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    property FDConnection: TFDConnection read FFDConnection write SetFDConnection;
    property FDTransaction: TFDTransaction read FFDTransaction write SetFDTransaction;
end;

TCcConnectionFireDAC = class(TCcAbstractConnectionFireDAC)
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
  published
    property DBType;
    property DBVersion;
end;

procedure Register;

implementation

uses CcMySQL, CcSQLServer, CcInterbase , CCOracle , CCPostgres {$IFDEF CC_D6}, Variants{$ENDIF};

function TCcAbstractConnectionFireDAC.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryFireDAC;
begin
  q := TCcQueryFireDAC.Create(Self, qry, nID, qry.SelectStatement);
  q.FDQuery.Connection := FDConnection;
  Result := q;
end;

class function TCcConnectionFireDAC.ConnectorName: String;
begin
  Result := 'FireDAC';
end;

destructor TCcQueryFireDAC.Destroy;
begin
  FFDQuery.Free;
  inherited;
end;

procedure TCcQueryFireDAC.DoClose;
begin
  FDQuery.Close;
end;

constructor TCcQueryFireDAC.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
  FFDQuery := TFDQuery.Create(Conn);
  FFDQuery.AfterClose := FDQueryAfterClose;
  FFDQuery.FetchOptions.Mode := fmOnDemand;//fmAll;//OnDemand;
  FFDQuery.FetchOptions.Cache := [];
//  FFDQuery.FetchOptions.Items := [];
  FFDQuery.FetchOptions.Unidirectional := True;
  FFDQuery.UpdateOptions.RequestLive := False;
end;

procedure TCcQueryFireDAC.FDQueryAfterClose(DataSet: TDataSet);
begin
  if Assigned(AfterClose) then
    AfterClose(DataSet);
end;

procedure TCcQueryFireDAC.DoExec;
begin
  if SelectStatement then
    FDQuery.Open
  else
    FDQuery.ExecSQL;
end;

function TCcQueryFireDAC.GetEof: Boolean;
begin
  Result := FDQuery.Eof;
end;

function TCcQueryFireDAC.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
begin
//  fType := GetFieldType(FieldName, IsParam);
//  if fType = ftString then begin
    if IsParam then
      Result := FDQuery.ParamByName(FieldName).Size
    else
      Result := FDQuery.FieldByName(FieldName).Size;
//  end else
//    Result := 0;
end;

function TCcQueryFireDAC.GetFieldType(FieldName: String;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := FDQuery.ParamByName(FieldName).DataType
  else
    Result := FDQuery.FieldByName(FieldName).DataType;
end;

function TCcQueryFireDAC.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then begin
    Result := FDQuery.Params[Field.Index].Value;
    if (FDQuery.Params[Field.Index].IsNull) then
      Result := Null;
  end
  else begin
    Result := FDQuery.Fields[Field.Index].Value;
    if (FDQuery.Fields[Field.Index].IsNull) then
      Result := Null
  end;
end;

function TCcQueryFireDAC.GetRowsAffected: Integer;
begin
  Result := FDQuery.RowsAffected;
end;

procedure TCcQueryFireDAC.DoNext;
begin
  FDQuery.Next;
end;

procedure TCcQueryFireDAC.DoPrepare(SQLText:String);
begin
  FDQuery.SQL.Text := SQLText;
//  FDQuery.Prepare;
end;

procedure TCcQueryFireDAC.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to FDQuery.FieldCount-1 do
    FieldList.Add(FDQuery.Fields[i].FieldName);
end;

procedure TCcQueryFireDAC.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for I:=0 to FDQuery.Params.Count-1 do
    ParamList.Add(FDQuery.Params[i].Name);
end;

procedure TCcQueryFireDAC.DoUnPrepare;
begin
  if FDQuery.Prepared then
    FDQuery.UnPrepare;
end;

procedure TCcQueryFireDAC.SetFieldValue(Field: TCCField; Val: Variant);
var
  Value: Variant;
begin
  Value := Val;
  if Field.IsParam then begin
    if (Field.DataType <> ftUnknown) and (FDQuery.Params[Field.Index].DataType = ftUnknown) then
      FDQuery.Params[Field.Index].DataType := Field.DataType;

    FDQuery.Params[Field.Index].Value := Value;
  end
  else
    FDQuery.Fields[Field.Index].Value := Value;
end;

procedure TCcQueryFireDAC.SetParamCheck(lParamCheck: Boolean);
begin
  FDQuery.ResourceOptions.PreprocessCmdText := lParamCheck;
end;

function TCcAbstractConnectionFireDAC.GetConnectorConnected: Boolean;
begin
  Result := False;
  if Assigned(FDConnection) then
    Result := FDConnection.Connected;
end;

function TCcAbstractConnectionFireDAC.GetInTransaction: Boolean;
begin
  Result := False;
  if Assigned(FDTransaction) then
    Result := FDTransaction.Active;
end;

constructor TCcConnectionFireDAC.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcMySQLAdaptor);
  AddDBAdaptor(TCcInterbaseAdaptor);
  AddDBAdaptor(TCcSQLServerAdaptor);
  AddDBAdaptor(TCcOracleAdaptor);
  AddDBAdaptor(TCcPostgresAdaptor);
end;

function TCcAbstractConnectionFireDAC.RowsAffectedSupported: Boolean;
begin
  Result := False;
end;

procedure TCcAbstractConnectionFireDAC.SetFDConnection(
  const Value: TFDConnection);
begin
  if Assigned(FFDConnection) then
    FFDConnection.RemoveFreeNotification(Self);

  FFDConnection := Value;

  if Assigned(Value) then begin
    Value.ResourceOptions.AutoReconnect := False;
    Value.TxOptions.AutoCommit := False;
    Value.FreeNotification(Self);
  end;
end;

procedure TCcAbstractConnectionFireDAC.SetFDTransaction(
  const Value: TFDTransaction);
begin
  if Assigned(FFDTransaction) then
    FFDTransaction.RemoveFreeNotification(Self);

  FFDTransaction := Value;

  if Assigned(Value) then begin
    Value.Options.AutoCommit := False;
    Value.FreeNotification(Self);
  end;
end;

procedure TCcAbstractConnectionFireDAC.DoDisconnect;
begin
  FDConnection.Connected := False;
end;

constructor TCcAbstractConnectionFireDAC.Create(AOwner: TComponent);
begin
  inherited;
  FAutoCreatedConnection := False;
end;

procedure TCcAbstractConnectionFireDAC.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FFDTransaction then
      SetFDTransaction(nil)
    else if AComponent = FFDConnection then
      SetFDConnection(nil);
  end;
  inherited;
end;

destructor TCcAbstractConnectionFireDAC.Destroy;
begin
  if FAutoCreatedConnection then begin
    FDConnection.Free;
    FDTransaction.Free;
  end;
  inherited;
end;

procedure TCcAbstractConnectionFireDAC.DoCommit;
begin
  if FFDTransaction.Active then
     FFDTransaction.Commit;
end;

procedure TCcAbstractConnectionFireDAC.DoCommitRetaining;
begin
  if FFDTransaction.Active then
  begin
    if DBType = 'Interbase' then
      FDTransaction.CommitRetaining
    else begin
      FFDTransaction.Commit;
      FFDTransaction.StartTransaction;
    end;
  end;
end;

procedure TCcAbstractConnectionFireDAC.DoConnect;
var
  I: Integer;
begin
  inherited;

  for I := 0 to ConnectionParams.Count-1 do
    FFDConnection.Params.Values[ConnectionParams.Names[i]] := ConnectionParams.ValueFromIndex[i];

  ConnectionParams.Assign(FFDConnection.Params);
  FFDConnection.Connected := False;
  FFDConnection.Connected := True;
end;

procedure TCcAbstractConnectionFireDAC.DoRollback;
begin
  if FFDTransaction.Active then
    FFDTransaction.Rollback;
end;

procedure TCcAbstractConnectionFireDAC.DoRollbackRetaining;
begin
  if FFDTransaction.Active then
     FFDTransaction.RollbackRetaining;
end;

procedure TCcAbstractConnectionFireDAC.DoStartTransaction;
begin
  if not FFDTransaction.Active then
    FFDTransaction.StartTransaction;
end;

procedure TCcAbstractConnectionFireDAC.Assign(Source: TPersistent);
begin
  if Source is TCcAbstractConnectionFireDAC then begin
    FDConnection := TFDConnection.Create(self);
    FDConnection.Assign(TCcAbstractConnectionFireDAC(Source).FDConnection);

    FDTransaction := TFDTransaction.Create(self);
    FDTransaction.Connection := FDConnection;
    FDTransaction.Options.Assign(TCcAbstractConnectionFireDAC(Source).FDTransaction.Options);

    FDConnection.Transaction := FDTransaction;
    FDConnection.UpdateTransaction := FDTransaction;

    FAutoCreatedConnection := True;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionFireDAC]);
end;

initialization
  RegisterDBConnector(TCcConnectionFireDAC, TCcConnectionFireDAC.ConnectorName);

end.
