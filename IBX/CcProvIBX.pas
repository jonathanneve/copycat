unit CcProvIBX;

interface

uses Classes, Sysutils, DB, IBSQL, IBDatabase, CcInterbaseConn, CcProviders;

type

{$I ..\CC.INC}

TCcQueryIBX = class(TCcAbstractQueryObject)
  private
    function FieldOrParam(field: TCcField): TIBXSQLVAR;overload;
    function FieldOrParam(fieldName: String; IsParam: Boolean): TIBXSQLVAR;overload;
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
//    function GetFieldValueAsInt64(Field: TCCField): Int64;override;
    procedure SetFieldValue(Field: TCCField; Val: Variant);override;
  public
    IBQuery :TIBSQL;
		constructor Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
    destructor Destroy; override;
end;

TCcConnectionIBX = class(TCcInterbaseConnection)
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
    IBDatabase :TIBDatabase;
    IBTransaction :TIBTransaction;
    constructor Create(AOwner: TComponent);override;
    class function ConnectorName: String;override;
    procedure CreateDatabase;
end;

procedure Register;

implementation

uses IBHeader, IB, CcInterbase {$IFDEF CC_UseVariants}, Variants{$ENDIF};

{ TCcProviderIBX }

constructor TCcConnectionIBX.Create(AOwner: TComponent);
begin
  inherited;
  IBDatabase := TIBDatabase.Create(Self);
  IBDatabase.LoginPrompt := False;
//  IBDatabase.TraceFlags := [tfQFetch, tfQExecute];

  IBTransaction := TIBTransaction.Create(Self);
  IBTransaction.DefaultDatabase := IBDatabase;
  IBDatabase.AddTransaction(IBTransaction);

  AddDBAdaptor(TCcInterbaseAdaptor);
end;

procedure TCcConnectionIBX.CreateDatabase;
begin
  IBDatabase.DatabaseName := DBName;
  IBDatabase.Params.Add('User ' + QuotedStr(UserLogin));
  IBDatabase.Params.Add('Password ' + QuotedStr(UserPassword));
  if CharSet <> '' then
    IBDatabase.Params.Add('DEFAULT CHARACTER SET ' + QuotedStr(CharSet));
  IBDatabase.SQLDialect := SQLDialect;
  IBDatabase.CreateDatabase;
end;

procedure TCcConnectionIBX.DoCommit;
begin
  IBTransaction.Commit;
end;

procedure TCcConnectionIBX.DoCommitRetaining;
begin
  IBTransaction.CommitRetaining;
end;

procedure TCcConnectionIBX.DoConnect;
begin
  IBDatabase.DatabaseName := DBName;
  IBDatabase.Params.Values['user_name'] := UserLogin;
  IBDatabase.Params.Values['lc_ctype'] := CharSet;
  IBDatabase.Params.Values['Password'] := UserPassword;
  IBDatabase.Params.Values['sql_role_name'] := RoleName;
  IBDatabase.SQLDialect := SQLDialect;
  IBDatabase.Connected := True;
end;

procedure TCcConnectionIBX.DoDisconnect;
begin
  IBDatabase.Connected := False;
end;

procedure TCcConnectionIBX.DoRollback;
begin
  IBTransaction.Rollback;
end;

procedure TCcConnectionIBX.DoRollbackRetaining;
begin
  IBTransaction.RollbackRetaining;
end;

procedure TCcConnectionIBX.DoStartTransaction;
begin
  IBTransaction.Params.Assign(TRParams);
  IBTransaction.StartTransaction;
end;

function TCcConnectionIBX.GetConnectorConnected: Boolean;
begin
	Result := IBDatabase.Connected;
end;

function TCcConnectionIBX.GetInTransaction: Boolean;
begin
  Result := IBTransaction.InTransaction;
end;

function TCcConnectionIBX.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryIBX;
begin
  q := TCcQueryIBX.Create(Self, qry, nID, qry.SelectStatement);
  q.IBQuery.Database := IBDatabase;
  q.IBQuery.Transaction := IBTransaction;
  Result := q;
end;

{ TCcQueryIBX }

procedure TCcQueryIBX.DoClose;
begin
  IBQuery.Close;
end;

constructor TCcQueryIBX.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
	IBQuery := TIBSQL.Create(Conn);
  IBQuery.GoToFirstRecordOnExecute := True;
end;

destructor TCcQueryIBX.Destroy;
begin
  IBQuery.Free;
  inherited;
end;

procedure TCcQueryIBX.DoExec;
begin
  IBQuery.ExecQuery;
end;

function TCcQueryIBX.GetEof: Boolean;
begin
  Result := IBQuery.Eof;
end;

function TCcQueryIBX.GetRowsAffected: Integer;
begin
  Result := IBQuery.RowsAffected;
end;

procedure TCcQueryIBX.DoNext;
begin
  IBQuery.Next;
end;

procedure TCcQueryIBX.DoPrepare(SQLText: String);
begin
  IBQuery.SQL.Text := SQLText;
  IBQuery.Prepare;
end;

procedure TCcQueryIBX.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  for I:=0 to IBQuery.Current.Count-1 do
    FieldList.Add(IBQuery.Current.Vars[i].Name);
end;

procedure TCcQueryIBX.DoInitParams(ParamList: TStringList);
var
  i:Integer;
begin
  for i:=0 to IBQuery.Params.Count-1 do
    ParamList.Add(IBQuery.Params[i].Name);
end;

procedure TCcQueryIBX.DoUnPrepare;
begin
  IBQuery.FreeHandle;
end;

function TCcQueryIBX.FieldOrParam(field: TCcField): TIBXSQLVAR;
begin
  if Field.IsParam then
    Result := IBQuery.Params[Field.Index]
  else
    Result := IBQuery.Fields[Field.Index];
end;

function TCcQueryIBX.FieldOrParam(fieldName: String; IsParam: Boolean): TIBXSQLVAR;
begin
  if IsParam then
    Result := IBQuery.Params.ByName(fieldName)
  else
    Result := IBQuery.FieldByName(fieldName);
end;

procedure TCcQueryIBX.SetParamCheck(lParamCheck: Boolean);
begin
  IBQuery.ParamCheck := lParamCheck;
end;

function TCcQueryIBX.GetFieldType(FieldName: String; IsParam: Boolean) : TFieldType;
begin
  with FieldOrParam(FieldName, IsParam) do
    case Data.sqltype and (not 1) of
 {     IB_SQL_VARYING, IB_SQL_TEXT: Result := ftString;

      IB_SQL_DOUBLE, IB_SQL_FLOAT, IB_SQL_D_FLOAT:
        Result := ftFloat;

      IB_SQL_LONG, IB_SQL_SHORT:
        if Data.sqlscale < 0 then Result := ftFloat
        else if Data.sqllen <> 4 then Result := ftSmallInt
        else Result := ftInteger;

      IB_SQL_DATE: Result := ftDateTime;
      IB_SQL_TYPE_TIME: Result := ftTime;
      IB_SQL_TYPE_DATE: Result := ftDate;

      IB_SQL_BLOB :
        if Data.sqlsubtype = 1 then Result := ftMemo
        else Result := ftBlob;

      IB_SQL_INT64: begin
        if Data.sqlscale > 0 then

       {$IFDEF CC_D6
          Result := ftLargeint;
       {$ELSE
          Result := ftInteger;
       {$ENDIF
      end;

      IB_SQL_ARRAY:
        Result := ftArray;

      IB_SQL_BOOLEAN:
        Result := ftBoolean;

      else
        Result := ftUnknown;
           }

        IB_SQL_VARYING, IB_SQL_TEXT:
        begin
          {$IFDEF CC_D2K9}
          Result := ftWideString;
          {$ELSE}
          Result := ftString
          {$ENDIF}
        end;
        { All Doubles/Floats should be cast to doubles }
        IB_SQL_DOUBLE, IB_SQL_FLOAT:
          Result := ftFloat;
        IB_SQL_SHORT:
        begin
          if (Data.sqlscale = 0) then
            Result := ftSmallInt
          else
            Result := ftBCD;
        end;
        IB_SQL_LONG:
        begin
          if (Data.sqlscale = 0) then
            Result := ftInteger
          else
            if (Data.sqlscale >= (-4)) then
              Result := ftBCD
            else if (Connection as TCcConnectionIBX).SQLDialect = 1 then
              Result := ftFloat
            else
              Result := ftFMTBCD;
        end;
        IB_SQL_INT64:
        begin
          if (Data.sqlscale = 0) then
            Result := ftLargeInt
          else
            if (Data.sqlscale >= (-4)) then
              Result := ftBCD
            else
              Result := ftFMTBCD;
        end;
        IB_SQL_TIMESTAMP: Result := ftDateTime;
        IB_SQL_TYPE_TIME: Result := ftTime;
        IB_SQL_TYPE_DATE: Result := ftDate;
        IB_SQL_BLOB:
        begin
          if (Data.sqlsubtype = 1) then
            {$IFDEF CC_D2K9}
            Result := ftWideMemo
            {$ELSE}
            Result := ftMemo
            {$ENDIF}
          else
            Result := ftBlob;
        end;
        IB_SQL_ARRAY:
        begin
          Result := ftArray;
        end;
        IB_SQL_BOOLEAN:
          Result := ftBoolean;
        else
          Result := ftUnknown;
    end;
end;

function TCcQueryIBX.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
var
  fType: TFieldType;
  field :TIBXSQLVAR;
begin
  field := FieldOrParam(FieldName, IsParam);
  fType := GetFieldType(FieldName, IsParam);
  if (fType = ftString) or (fType = ftWideString) then
    Result := field.Size div field.CharsetSize
  else if (fType = ftBCD) or (fType = ftFMTBcd) then
    Result := -field.Data.sqlscale
  else if (fType = ftBlob) or (fType = ftMemo) or (fType = ftArray) or (fType = ftWideMemo) then
    Result := Sizeof(TISC_QUAD)
  else
    Result := 0;
end;

function TCcQueryIBX.GetFieldValue(Field: TCCField): Variant;
begin
  if FieldOrParam(Field).IsNull then
    Result := Null
  else if (Field.DataType = ftBlob) or (Field.DataType = ftMemo) or (Field.DataType = ftWideMemo) then
    Result := FieldOrParam(Field).AsString
  {$IFDEF CC_D6}
  else if (Field.DataType = ftLargeInt) then
    Result := FieldOrParam(Field).AsInt64
  {$ENDIF}
  else
    Result := FieldOrParam(Field).Value;
end;

procedure TCcQueryIBX.SetFieldValue(Field: TCCField; Val: Variant);
begin
  if VarIsNull(Val) or VarIsEmpty(Val) then
    FieldOrParam(Field).Clear
  else
  {$IFDEF CC_D6}
  if (Field.DataType = ftLargeInt) then
    FieldOrParam(Field).AsInt64 := Val
  else
  {$ENDIF}
    FieldOrParam(Field).Value := Val;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionIBX]);
end;

class function TCcConnectionIBX.ConnectorName:String;
begin
  Result := 'IBX';
end;

//function TCcQueryIBX.GetFieldValueAsInt64(Field: TCCField): Int64;
//begin
//  Result := FieldOrParam(Field).AsInt64;
//end;

initialization
  RegisterDBConnector(TCcConnectionIBX, TCcConnectionIBX.ConnectorName);
end.
