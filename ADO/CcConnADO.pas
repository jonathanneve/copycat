unit CcConnADO;

interface

{$I ..\CC.INC}

uses
  Classes, Sysutils, DB, CcProviders, ADODB, AdoConEd, Dialogs;

type

TCcQueryADO = class(TCcAbstractQueryObject)
	private
		FRowsAffected:Integer;
		procedure CheckResultSetAvailable;
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
		ADODataSet: TADODataSet;
		ADOCommand: TADOCommand;
		constructor Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);override;
		destructor Destroy;override;
end;

TCcADOAdaptor = class;

TCcConnectionADO = class(TCcConnection)
  private
    FADOAdaptor: TCcADOAdaptor;
    FConnectionString: WideString;
    FCommandTimeout: Integer;
    FConnectionTimeout: Integer;
    FDBInUse: String;
    function GetADOAdaptor: TCcADOAdaptor;
    procedure SetDBInUse(const Value: String);
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
  protected
    property ADOAdaptor: TCcADOAdaptor read GetADOAdaptor;
    procedure DoResetQueryObjects(NewValue: String);override;
  public
    ADOConnection: TADOConnection;
    function EditConnection: Boolean;
    class function ConnectorName: String;override;
    procedure CreateDatabase(dbName: String);
    procedure DropDatabase(dbName: String);
    constructor Create(AOwner: TComponent);override;
    property DBInUse : String read FDBInUse write SetDBInUse;
  published
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
    property CommandTimeout: Integer read FCommandTimeout write FCommandTimeout;
    property ConnectionString: WideString read FConnectionString write FConnectionString;
    property DBType;
    property DBVersion;
end;

TCcADOAdaptor = class
  protected
    FConnection: TCcConnectionADO;
    FADOConnection: TADOConnection;

    //Procedures to be implemented by descendants in order to set database-specific parameters
    procedure SetDBName(Value: String);virtual;abstract;
    procedure SetCharSet(Value: String);virtual;abstract;
    procedure SetRoleName(Value: String);virtual;abstract;
    constructor Create(conn: TCcConnectionADO);
    procedure SetConnectionString(Value: String);
end;

TCcADOAdaptorClass = class of TCcADOAdaptor;

TCcADODBType = class
  public
    Name:String;
    AdaptorClass: TCcADOAdaptorClass;
end;

//procedure CcRegisterADOAdaptor(AdaptorName: String; AdaptorClass: TCcADOAdaptorClass);

//var
//  CcAvailableADOAdaptors: TStringList;

procedure Register;

procedure ParseConStr(AList: TStrings; AConStr: string);
function ParseDataSetFields(ADataSet: TDataSet): string;
procedure AssignADOConnection(Source, Dest: TADOConnection);

implementation

uses AdoInt {$IFDEF CC_D6} ,Variants {$ENDIF} ;

{ Common procedures}

procedure ParseConStr(AList: TStrings; AConStr: string);
var
  I, J, K: Integer;
  CStr, LStr, S: string;
begin
  if Assigned(AList) then begin
     CStr := TrimLeft(AConStr);
     LStr := CStr;
     I := AnsiPos(';', CStr);
     while I > 0 do begin
       J := AnsiPos('"', LStr);
       if ((J > 0) and (J < I)) then begin
          LStr := Copy(LStr, J + 1, Length(LStr));
          K := AnsiPos('"', LStr);
          if K > 0 then begin
             while ((Lstr[K] <> ';') and (K < Length(LStr) - 1)) do
               Inc(K);
             I := J + K;
          end;
       end;
       S := Copy(CStr, 1 , I - 1);
       for J := Length(S) downto 1 do begin
         if S[J] = '"' then
            System.Delete(S,J,1);
       end;
       AList.Add(S);
       CStr := TrimLeft(Copy(CStr, I + 1, Length(CStr)));
       LStr := CStr;
       I := AnsiPos(';', CStr);
     end;
     if Length(CStr) > 0 then
       AList.Add(CStr);
  end;
end;

function ParseDataSetFields(ADataSet: TDataSet): string;
var
  List: TStrings;
  I: Integer;
begin
  Result := '';
  if not Assigned(ADataSet) then Exit;
  List := TStringList.Create;
  try
    ADataSet.GetFieldNames(List);
    for I := 0 to Pred(List.Count) do begin
      if I < Pred(List.Count) then
         Result := Result + List[I] + ','
      else Result := Result + List[I];
    end;
  finally
    List.Free;
  end;
end;

procedure AssignADOConnection(Source, Dest: TADOConnection);
begin
  if ((not Assigned(Source)) or (not Assigned(Dest))) then Exit;
  Dest.Close;
  Dest.Attributes := Source.Attributes;
  Dest.CommandTimeout := Source.CommandTimeout;
  Dest.ConnectionString := Source.ConnectionString;
  Dest.ConnectionTimeout := Source.ConnectionTimeout;
  Dest.ConnectOptions := Source.ConnectOptions;
  Dest.CursorLocation := Source.CursorLocation;
  //Dest.DefaultDatabase := Source.DefaultDatabase;
  Dest.IsolationLevel := Source.IsolationLevel;
  Dest.KeepConnection := Source.KeepConnection;
  Dest.LoginPrompt := Source.LoginPrompt;
  Dest.Mode := Source.Mode;
end;

{ TCcConnectionADO }

constructor TCcConnectionADO.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
//  for i := 0 to CcAvailableADOAdaptors.Count - 1 do
//    AddDBAdaptor(CcAvailableADOAdaptors[i]);
  for I := 0 to CcAvailableAdaptors.Count-1 do
    AddDBAdaptor(TCcDBAdaptorClass(CcAvailableAdaptors[i]));
//  for I := 0 to CcAvailableDBTypes.Count-1 do
//    AddDBAdaptor(TCcDBAdaptorClass(TCcClassInfo(CcAvailableDBTypes.Objects[i]).ClassReference));

  ADOConnection := TADOConnection.Create(Self);
  ADOConnection.LoginPrompt := False;

  //Copy the default timeout values from ADOConnection
  FCommandTimeout := ADOConnection.CommandTimeout;
  FConnectionTimeout := ADOConnection.ConnectionTimeout;
end;

procedure TCcConnectionADO.CreateDatabase(dbName: String);
begin
  if ADOConnection.InTransaction then
    ADOConnection.CommitTrans;
  ADOConnection.Execute('create database ' + dbName);
  DBInUse := dbName;
  DoStartTransaction;
end;

function TCcConnectionADO.GetConnectorConnected: Boolean;
begin
  Result := ADOConnection.Connected;
end;

function TCcConnectionADO.GetInTransaction: Boolean;
begin
  Result := ADOConnection.InTransaction;
end;

procedure TCcConnectionADO.DoCommit;
begin
  ADOConnection.Attributes := ADOConnection.Attributes - [xaCommitRetaining];
  ADOConnection.CommitTrans;
end;

procedure TCcConnectionADO.DoCommitRetaining;
begin
//  if (ADOConnection.InTransaction and (xaCommitRetaining in ADOConnection.Attributes)) then
  ADOConnection.Attributes := ADOConnection.Attributes - [xaCommitRetaining];
  ADOConnection.CommitTrans;
  ADOConnection.BeginTrans;
end;

procedure TCcConnectionADO.DoConnect;
var
  i: Integer;
  cConnStr: String;
begin
  cConnStr := FConnectionString;
  for i := 0 to ConnectionParams.Count - 1 do
    cConnStr := cConnStr + ';' + ConnectionParams[i];
  ADOConnection.Close;
  ADOConnection.ConnectionString := cConnStr;
//  ADOAdaptor.SetCharSet(CharSet);
//  FADOAdaptor.SetRoleName(RoleName);
//  ADOAdaptor.SetDBName(DBName);

  ADOConnection.CommandTimeout := FCommandTimeout;
  ADOConnection.ConnectionTimeout := FConnectionTimeout;
  ADOConnection.Open;//(UserLogin, UserPassword);
  SetDBInUse(FDBInUse);
end;

procedure TCcConnectionADO.DoDisconnect;
begin
  ADOConnection.Close;
end;

procedure TCcConnectionADO.DoRollback;
begin
  ADOConnection.Attributes := ADOConnection.Attributes - [xaAbortRetaining];
  ADOConnection.RollbackTrans;
end;

procedure TCcConnectionADO.DoRollbackRetaining;
begin
  ADOConnection.Attributes := ADOConnection.Attributes + [xaAbortRetaining];
  ADOConnection.RollbackTrans;
end;

procedure TCcConnectionADO.DoStartTransaction;
begin
  if not ADOConnection.InTransaction then
    ADOConnection.BeginTrans;
end;

procedure TCcConnectionADO.DropDatabase(dbName: String);
begin
  if ADOConnection.InTransaction then
    ADOConnection.CommitTrans;
  ADOConnection.Execute('use master');
  ADOConnection.Execute('drop database ' + dbName);
  DoStartTransaction;
end;

function TCcConnectionADO.GetADOAdaptor: TCcADOAdaptor;
begin
  if Assigned(FADOAdaptor) then
    Result := FADOAdaptor
  else
    raise Exception.Create('ADO database type not assigned!');
end;

function TCcConnectionADO.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
var
  q: TCcQueryADO;
begin
	q := TCcQueryADO.Create(Self, qry, nID, qry.SelectStatement);
	if qry.SelectStatement then begin
		q.ADODataSet.Connection := ADOConnection;
		q.ADODataSet.CommandTimeout := FCommandTimeout;
	end else begin
		q.ADOCommand.Connection := ADOConnection;
		q.ADOCommand.CommandTimeout := FCommandTimeout;
	end;
	Result := q;
end;

procedure TCcConnectionADO.SetDBInUse(const Value: String);
begin
  FDBInUse := Value;
  if FDBInUse <> '' then
    ADOConnection.Execute('use ' + FDBInUse);
end;

class function TCcConnectionADO.ConnectorName: String;
begin
  Result := 'ADO';
end;

procedure TCcConnectionADO.DoResetQueryObjects(NewValue: String);
//var
//  nIndex :Integer;
begin
//  if Assigned(FADOAdaptor) then
//    FADOAdaptor.Free;
//  nIndex := CcAvailableADOAdaptors.IndexOf(NewValue);
//  FADOAdaptor := TCcADODBType(CcAvailableADOAdaptors.Objects[nIndex]).AdaptorClass.Create(Self);
end;

function TCcConnectionADO.EditConnection: Boolean;
begin
  Result := EditConnectionString(Self);
end;

{ TCcQueryADO }

destructor TCcQueryADO.Destroy;
begin
  if SelectStatement then
    ADODataSet.Free
  else
  	ADOCommand.Free;
  inherited;
end;

procedure TCcQueryADO.DoClose;
begin
	if SelectStatement then
		ADODataSet.Close;
end;

constructor TCcQueryADO.Create(Conn:TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
	inherited;
	if Select then
		ADODataSet := TADODataSet.Create(Conn)
	else begin
		ADOCommand := TADOCommand.Create(Conn);
		ADOCommand.Prepared := True;
	end;
end;

procedure TCcQueryADO.DoExec;
var
	resultSet: _Recordset;
  nRowsAffected: Integer;
  oleRowsAffected: OleVariant;
//var
//  lExec: Boolean;
//  s: string;
begin
//  //If the SQL begins with 'SELECT' it returns a result set
//  s := UpperCase(TrimLeft(ADOQuery.SQL.Text));
//  if AnsiPos('SELECT', s) = 1 then
//    lExec := False
//  else
//    lExec := True;
//
//  if lExec then
//    ADOQuery.ExecSQL
//  else

//	ADOQuery.Open;
//	resultSet := ADOQuery.Recordset;
//  nRowsAffected := ADOQuery.RowsAffected;

	if SelectStatement then begin
		ADODataSet.Active := True;
		nRowsAffected := -1;
	end else
		ADOCommand.Execute(FRowsAffected, EmptyParam);

{  //We only want the last result-set
	while (resultSet <> nil) do begin
		if (resultSet.State and adStateOpen <> 0) then begin
			ADODataSet.Recordset := resultSet;
			lResultSetAvailable := True;
		end;
		//Store RowsAffected of current result-set
		FRowsAffected := nRowsAffected;

		//Fetch next result-set, if any
		resultSet := resultSet.NextRecordset(oleRowsAffected);

		//Get rowsaffected of next result-set
		//If this was the last result-set, FRowsAffected holds the rowsAffected
		nRowsAffected := oleRowsAffected;
	end;}

//  if (resultSet <> nil) and (resultSet.State and adStateOpen <> 0) then begin
//    ADODataSet.Recordset := resultSet;
//    lResultSetAvailable := True;
//  end;
//  if ADODataSet.Active then
//    ADODataSet.First;
end;

function TCcQueryADO.GetEof: Boolean;
begin
//	CheckResultSetAvailable;
	if SelectStatement then
		Result := ADODataSet.Eof
	else
	  Result := False;
end;

function TCcQueryADO.GetRowsAffected: Integer;
begin
	Result := FRowsAffected;//ADOQuery.RowsAffected;
end;

procedure TCcQueryADO.DoNext;
begin
//	CheckResultSetAvailable;
	if SelectStatement then
    ADODataSet.Next;
end;

procedure TCcQueryADO.DoPrepare(SQLText: String);
begin
	if SelectStatement then
		ADODataSet.CommandText := SQLText
	else
		ADOCommand.CommandText := SQLText;
end;

procedure TCcQueryADO.DoInitFields(FieldList: TStringList);
var
  i:Integer;
begin
  //CheckResultSetAvailable;
	if SelectStatement then begin
		for I:=0 to ADODataSet.FieldCount-1 do
			FieldList.Add(ADODataSet.Fields.Fields[I].FieldName);
	end;
end;

procedure TCcQueryADO.DoInitParams(ParamList: TStringList);
var
  i:Integer;
	cParamName: String;
	params: TParameters;
begin
	if SelectStatement then
		params := ADODataSet.Parameters
	else
		params := ADOCommand.Parameters;

	for i:=0 to params.Count-1 do begin
		cParamName := params.Items[i].Name;
    //If there are multiple parameters with the same name, only one must be returned
		if ParamList.IndexOf(cParamName) = -1 then
      ParamList.Add(cParamName);
  end;
end;

procedure TCcQueryADO.DoUnPrepare;
begin
//inherited;
	if SelectStatement then
		ADODataSet.Close;
end;

procedure TCcQueryADO.SetParamCheck(lParamCheck: Boolean);
begin
	if SelectStatement then
		ADODataSet.ParamCheck := lParamCheck
	else
		ADOCommand.ParamCheck := lParamCheck;
end;

function TCcQueryADO.GetFieldType(FieldName: String; IsParam: Boolean) : TFieldType;
var
  dType: TFieldType;
begin
	if IsParam then begin
		if SelectStatement then
			Result := ADODataSet.Parameters.ParamByName(FieldName).DataType
		else
			Result := ADOCommand.Parameters.ParamByName(FieldName).DataType;
	end
	else begin
		CheckResultSetAvailable;
		dType := ADODataSet.FieldByName(FieldName).DataType;
    Result := dType;
  end;
end;

function TCcQueryADO.GetFieldSize(FieldName: String; IsParam: Boolean): Integer;
begin
	if IsParam then begin
		if SelectStatement then
			Result := ADODataSet.Parameters.ParamByName(FieldName).Size
		else
			Result := ADOCommand.Parameters.ParamByName(FieldName).Size;
  end
	else begin
    CheckResultSetAvailable;
    Result := ADODataSet.FieldByName(FieldName).Size;
  end;
end;

function TCcQueryADO.GetFieldValue(Field: TCCField): Variant;
begin
  if Field.IsParam then begin
		if SelectStatement then
			Result := ADODataSet.Parameters.Items[Field.Index].Value
		else
			Result := ADOCommand.Parameters.Items[Field.Index].Value;
	end
	else begin
    CheckResultSetAvailable;
//    if Field.DataType = ftBlob then
//      Result := ADODataSet.Fields[Field.Index].AsString
//    else
      Result := ADODataSet.Fields[Field.Index].Value
  end;
end;

procedure TCcQueryADO.SetFieldValue(Field: TCCField; Val: Variant);
var
  I: Integer;
  NewVal: Variant;
  StrStream: TStringStream;
	valType : Integer;
	params: TParameters;
//  dataType: TFieldType;

begin
	if Field.IsParam then begin
		valType := VarType(Val);

		if SelectStatement then
		  params := ADODataSet.Parameters
		else
			params := ADOCommand.Parameters;

		//If a parameter is used multiple times, it will correspond to multiple independant
		//parameters as far as ADO is concerned. We must therefore set them all...
		for I := 0 to params.Count - 1 do begin
			NewVal := Val;
			if params.Items[i].Name = Field.FieldName then begin
        if VarType(Val) <> varNull then begin
//          dataType := ADOCommand.Parameters.Items[i].DataType;
					if (Field.DataType = ftString) and (Val = '') then
            NewVal := Null;
//          else if (dataType = ftGuid) and (VarType(Val) = varString) then
//            NewVal := StringToGuid(Val)
        end;

        if Field.DataType = ftBlob then begin
          ADOCommand.Parameters.Items[i].Value := NewVal;
          {StrStream := TStringStream.Create(NewVal);
          try
						params.Items[i].LoadFromStream(StrStream, ftBlob);
          finally
            StrStream.Free;
          end;}
        end else if (Field.DataType = ftGuid) and ((valType = varString) {$IFDEF CC_D2K9}or (valType = varUString) {$ENDIF}) then begin
					if Copy(NewVal, 1, 1) = '{' then
						params.Items[i].Value := NewVal
          else
						params.Items[i].Value := '{' + NewVal + '}';
				end else
					params.Items[i].Value := NewVal;

				if (Field.DataType <> ftUnknown) and (params.Items[i].DataType = ftUnknown)
					and (NewVal = Null) then
					params.Items[i].DataType := Field.DataType;
      end;
    end;
  end
  else begin
    CheckResultSetAvailable;
    ADODataSet.Fields[Field.Index].Value := Val;
  end;
end;

procedure TCcQueryADO.CheckResultSetAvailable;
begin
	if not SelectStatement then
		raise Exception.Create('Can''t perform this method on statement with no resultset');
//	if not lResultSetAvailable then
//		raise Exception.Create('No result-set available');
end;

{ TCcADOAdaptor }

constructor TCcADOAdaptor.Create(conn: TCcConnectionADO);
begin
  FConnection := conn;
  FADOConnection := conn.ADOConnection;
end;

procedure Register;
begin
  RegisterComponents('CopyCat connectors', [TCcConnectionADO]);
end;

{procedure CcRegisterADOAdaptor(AdaptorName: String; AdaptorClass: TCcADOAdaptorClass);
var
  dbType : TCcADODBType;
begin
  if not Assigned(CcAvailableADOAdaptors) then
    CcAvailableADOAdaptors := TStringList.Create;

  dbType := TCcADODBType.Create;
  dbType.Name := AdaptorName;
  dbType.AdaptorClass := AdaptorClass;
  CcAvailableADOAdaptors.AddObject(AdaptorName, dbType);
end; }

procedure TCcADOAdaptor.SetConnectionString(Value: String);
begin
//  if FADOConnection.InTransaction then begin
//     raise Exception.Create('Transaction is in progress...');
//  end;
  if FADOConnection.InTransaction then
     FADOConnection.CommitTrans;
  FADOConnection.Close;
  FADOConnection.ConnectionString := Value;
end;

initialization
  RegisterDBConnector(TCcConnectionADO, TCcConnectionADO.ConnectorName);

//  if not Assigned(CcAvailableADOAdaptors) then
//    CcAvailableADOAdaptors := TStringList.Create;

finalization
//  FreeAndNil(CcAvailableADOAdaptors);


end.
