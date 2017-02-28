unit CcProvUIB;

interface

{$I ..\CC.INC}
{$I jvuib.inc}

uses Classes, SysUtils, DB, CcProviders, CcInterbaseConn, JvUIB, JvUIBLib;

type

TCcQueryUIB = class(TCcAbstractQueryObject)
  protected
    function GetRowsAffected: Integer;override;
    function GetEof: Boolean;override;
    procedure DoInitParams(ParamList: TStringList);override;
    procedure DoInitFields(FieldList: TStringList);override;
    procedure DoExec;override;
    procedure DoPrepare(SQLText: String);override;
    procedure DoUnPrepare;override;
    procedure SetParamCheck(lParamCheck: Boolean);override;
    procedure DoClose;override;
    procedure DoNext;override;
    function GetFieldType(FieldName: String; IsParam: Boolean): TFieldType;override;
    function GetFieldSize(FieldName: String; IsParam: Boolean): Integer;override;
    function GetFieldValue(Field: TCCField): Variant;override;
    procedure SetFieldValue(Field: TCCField; Val: Variant);override;
  public
    IBQuery :TJvUIBQuery;
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer);override;
  end;

  TCcConnectionUIB = class(TCcInterbaseConnection)
  private
    FOptions: TTransParams;
    FLibraryName: TFileName;
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
    IBDatabase: TJvUIBDatabase;
    IBTransaction :TJvUIBTransaction;
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    procedure Assign(Source: TPersistent);override;
  published
    property Options: TTransParams read FOptions write FOptions;
    property LibraryName: TFileName read FLibraryName write FLibraryName;
end;

procedure Register;

implementation

uses JvUIBase, CcInterbase {$IFDEF CC_D6}, Variants{$ENDIF};

{ TCcProviderUIB }

constructor TCcConnectionUIB.Create(AOwner: TComponent);
begin
  inherited;
  IBDatabase := TJvUIBDatabase.Create(Self);
  LibraryName := IBDatabase.LibraryName;

  IBTransaction := TJvUIBTransaction.Create(Self);
  IBTransaction.DataBase := IBDatabase;

  FOptions := [tpWrite,tpNowait,tpRecVersion];

  AddDBAdaptor(TCcInterbaseAdaptor);
end;

function TCcConnectionUIB.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryUIB;
begin
  q := TCcQueryUIB.Create(Self, qry, nID);
  q.IBQuery.DataBase := IBDatabase;
  q.IBQuery.Transaction := IBTransaction;
  Result := q;
end;

class function TCcConnectionUIB.ConnectorName: String;
begin
  Result := 'UIB';
end;

{ TCcQueryUIB }

procedure TCcQueryUIB.DoClose;
begin
  IBQuery.Close;
end;

constructor TCcQueryUIB.Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer);
begin
  inherited;
  IBQuery := TJvUIBQuery.Create(Conn);
end;

procedure TCcQueryUIB.DoExec;
begin
  IBQuery.Prepare; //Just in case
  if IBQuery.StatementType = stSelect then
    IBQuery.Open(True)
  else
    IBQuery.Execute;
end;

function TCcQueryUIB.GetEof: Boolean;
begin
  Result := IBQuery.Eof;
end;

function TCcQueryUIB.GetRowsAffected: Integer;
begin
  Result := IBQuery.RowsAffected;
end;

procedure TCcQueryUIB.DoNext;
begin
  IBQuery.Next;
end;

procedure TCcQueryUIB.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to IBQuery.Fields.FieldCount - 1 do
    FieldList.Add(IBQuery.Fields.AliasName[i]);
end;

procedure TCcQueryUIB.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for i:=0 to IBQuery.Params.ParamCount -1 do
    ParamList.Add(IBQuery.Params.FieldName[i]);
end;

procedure TCcQueryUIB.DoUnPrepare;
begin
  IBQuery.CloseCursor;
end;

procedure TCcQueryUIB.DoPrepare(SQLText: String);
begin
  IBQuery.SQL.Text := SQLText;
  IBQuery.Prepare;
end;

procedure TCcQueryUIB.SetParamCheck(lParamCheck: Boolean);
begin
  IBQuery.ParseParams := lParamCheck;
end;

function TCcQueryUIB.GetFieldSize(FieldName: String;
  IsParam: Boolean): Integer;
var
  field: TSQLDA;
  fType :TFieldType;
begin
  if IsParam then
    field := IBQuery.Params
  else
    field := IBQuery.Fields;

  fType := GetFieldType(FieldName, IsParam);
  if fType = ftString then
    Result := field.SQLLen[field.GetFieldIndex(FieldName)]
  else if (fType = ftBlob) or (fType = ftMemo) or (fType = ftBytes) then
    Result := Sizeof(TIscQuad)
  else
    Result := 0;
end;

function TCcQueryUIB.GetFieldType(FieldName:string; IsParam: Boolean): TFieldType;
var
  field: TSQLDA;
  sqlvar: TUIBSQLVar;
  index: Integer;
begin
  if IsParam then
    field := IBQuery.Params
  else
    field := IBQuery.Fields;

  index := field.GetFieldIndex(FieldName);
  sqlvar := field.Data^.sqlvar[index];

  case field.FieldType[index] of
    uftVarchar, uftChar, uftCstring : Result := ftString;
    uftNumeric, uftFloat, uftDoublePrecision : Result := ftFloat;
    uftSmallint : Result := ftSmallint;
    uftInteger : Result := ftInteger;
    uftDate : Result := ftDate;
    uftTime : Result := ftTime;
    uftTimestamp : Result := ftDateTime;
    uftBlob :
    begin
      if sqlvar.SqlSubType = 1 then
        Result := ftMemo
      else
        Result := ftBlob;
    end;
    uftInt64 : Result := ftFloat;
    uftArray : Result := ftBytes;
    {$IFDEF IB7_UP}
    uftBoolean: Result := ftBoolean;
    {$ENDIF}
  else
    Result := ftUnknown;
  end;
end;

procedure TCcConnectionUIB.DoDisconnect;
begin
  IBDatabase.Connected := False;
end;

procedure TCcConnectionUIB.DoCommit;
begin
  IBTransaction.Commit;
end;

procedure TCcConnectionUIB.DoCommitRetaining;
begin
  IBTransaction.CommitRetaining;
end;

procedure TCcConnectionUIB.DoRollback;
begin
  IBTransaction.Rollback;
end;

procedure TCcConnectionUIB.DoRollbackRetaining;
begin
  IBTransaction.RollbackRetaining;
end;

function TCcQueryUIB.GetFieldValue(Field: TCcField): Variant;
begin
  if Field.IsParam then
    Result := IBQuery.Params.ByNameAsVariant[Field.FieldName]
  else
    Result := IBQuery.Fields.ByNameAsVariant[Field.FieldName];
end;

procedure TCcQueryUIB.SetFieldValue(Field: TCcField; Val: Variant);
var
  SQLDA: TSQLDA;
begin
  if Field.IsParam then
    SQLDA := IBQuery.Params
  else
    SQLDA := IBQuery.Fields;

  if VarIsNull(Val) then
    SQLDA.IsNull[Field.Index] := True
  else
  case VarType(Val) of
    varEmpty, varNull:
      SQLDA.IsNull[Field.Index] := True;
    varSmallint:
      SQLDA.AsSmallint[Field.Index] := Val;
    varInteger, varByte {$IFDEF CC_D6}, varWord, varShortInt, varLongWord{$ENDIF}:
      SQLDA.AsInteger[Field.Index] := Val;
    varSingle:
      SQLDA.AsSingle[Field.Index] := Val;
    varDouble:
      SQLDA.AsDouble[Field.Index] := Val;
    varCurrency:
      SQLDA.AsCurrency[Field.Index] := Val;
    varBoolean:
      SQLDA.AsBoolean[Field.Index] := Val;
    varDate:
      SQLDA.AsDateTime[Field.Index] := Val;
    varOleStr, varString:
      SQLDA.AsString[Field.Index] := Val;
    {$IFDEF CC_D6}
    varInt64:
      SQLDA.AsInt64[Field.Index] := Val;
    {$ENDIF}
    varArray:
      raise Exception.Create('Value cannot be used to handle variant arrays');
    else
      raise Exception.Create('Value cannot be used to handle such a variant type');
  end;
end;

procedure TCcConnectionUIB.DoConnect;
begin
  IBDatabase.DatabaseName := DBName;
  IBDatabase.Params.Values['lc_ctype'] := CharSet;
  IBDatabase.Params.Values['Password'] := UserPassword;
  IBDatabase.Params.Values['sql_role_name'] := RoleName;
  IBDatabase.Params.Values['user_name'] := UserLogin;
  IBDatabase.SQLDialect := SQLDialect;
  IBDatabase.LibraryName := LibraryName;
  IBDatabase.Connected := True;
end;

procedure TCcConnectionUIB.DoStartTransaction;
begin
  IBTransaction.Options := Options;
  IBTransaction.StartTransaction;
end;

procedure TCcConnectionUIB.Assign(Source: TPersistent);
begin
  if Source is TCcConnectionUIB then with TCcConnectionUIB(Source) do begin
    Self.Options := Options;
    Self.LibraryName := LibraryName;
  end;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionUIB]);
end;

initialization
  RegisterDBConnector(TCcConnectionUIB, TCcConnectionUIB.ConnectorName);

end.
