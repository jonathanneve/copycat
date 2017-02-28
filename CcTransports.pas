unit CcTransports;

interface

{$I CC.INC}


uses Classes, CcProviders, CcDB, DB, {$IFDEF FPC}fptimer{$ELSE}
{$IFDEF MSWINDOWS} {$IFDEF CC_D2K14}Vcl.ExtCtrls{$ELSE}ExtCtrls{$ENDIF} {$ELSE} FMX.Types{$ENDIF}{$ENDIF}
    , SyncObjs;

type

  TCcRemoteFunction = (funcConnNewQuery, funcSetProperty, funcConnDisconnect, funcConnConnect, funcConnCommit,
    funcConnCommitRetaining, funcConnRollback, funcConnRollbackRetaining, funcConnStartTransaction,
    funcQryExec, funcQryPrepare, funcQryUnPrepare, funcQryClose, funcQryFetch, funcQrySetParam, funcQryParamCheck, funcKeepAlive);

  TCcSession = class
  private
    FConnection: TCcConnection;
    FSessionID: string;
  protected
    FLastActivity: {$IFDEF CC_D2K15}Int64{$ELSE}Cardinal{$ENDIF};
    FClientCopyCatVersion: string;
  public
    property Connection: TCcConnection read FConnection;
    property SessionID: string read FSessionID;
    // CopyCat version of the currently connected client
    property ClientCopyCatVersion: string read FClientCopyCatVersion;
    constructor Create(ASessionID: string; AConnection: TCcConnection);
    destructor Destroy; override;
  end;

  TCcServerTransport = class;
  TCcSessionEvent = procedure(Server: TCcServerTransport; Session: TCcSession) of object;

  // Summary:
  // Abstract ancestor for all client transport connections
  // Description:
  // TCcClientTransport implements a client connection for replicating over a custom
  // transport. Descendants should override DoRemoteCall to link with a concrete
  // remoting technology.<p/>
  // In order to use this feature, you need to compile and install one of the transport packages
  // descending from this class (currently XMLRPC, RTC and DataSnap). You can then drop a TCcClientTransport
  // descendant onto your form and hook it up to your TCcReplicator or TCcConfig components as if it
  // were a direct database connection. You can also use it with a TCcQuery or a TCcDataSet in order to
  // execute an SQL query over the middleware connection.
  TCcClientTransport = class(TCcConnection)
  private
    FCS: TCriticalSection;
    FSessionID: string;
    FDatabaseAlias: string;
    FKeepAliveTimer: {$IFDEF FPC}TFPTimer{$ELSE}TTimer{$ENDIF};
    FInRemoteCall: Boolean;
    FPassword: string;
    FServerCopyCatVersion: string;
    FSessionActive: Boolean;
    FAutoCommit: Boolean;
    FServerConnectorSupportsRowsAffected: Boolean;
    FInTrasaction: Boolean;
    function GetProcedureName: string;
    procedure KeepAliveTimer(Sender: TObject);
    procedure SetKeepAliveInterval(const Value: Integer);
    function GetKeepAliveInterval: Integer;
    function GetLoginProcName: string;
    procedure CleanupSession;
  protected
    FConnected: Boolean;
    function GetConnectorConnected: Boolean; override;
    function GetInTransaction: Boolean; override;
    property ProcedureName: string read GetProcedureName;
    property LoginProcName: string read GetLoginProcName;
    procedure SignalConnectLost; override;
    procedure DoDisconnect; override;
    procedure DoConnect; override;
    procedure DoCommit; override;
    procedure DoCommitRetaining; override;
    procedure DoRollback; override;
    procedure DoRollbackRetaining; override;
    procedure DoStartTransaction; override;
    procedure RemoteCall(FunctionName: TCcRemoteFunction; Params: array of Variant; Result: TCcValue); overload;
    procedure RemoteCall(FunctionName: TCcRemoteFunction; Params, Result: TCcValue); overload;
    procedure RemoteCall(ObjectID: Integer; FunctionName: TCcRemoteFunction; Params: array of Variant; Result: TCcValue); overload;
    procedure RemoteCall(ObjectID: Integer; FunctionName: TCcRemoteFunction; Params: TCcValue; Result: TCcValue); overload;
    procedure DoRemoteCall(FunctionName: string; Params: TCcValue; Result: TCcValue); virtual;
    function NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject; override;
    procedure DoBeforeConnect; override;
    function RowsAffectedSupported: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // The version of CopyCat that the server was compiled with. This is only valid when a session is active.
    property ServerCopyCatVersion: string read FServerCopyCatVersion;
    // Indicates whether the session is active.
    property SessionActive: Boolean read FSessionActive;
  published

    // Setting autocommit optimizes communication by not explicitly starting nor ending transactions
    // This implies that the server side must automatically commit after executying every query
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;

    // Overview: Alias of the database connection on the CopyCat server.
    // Description:
    // Rather than specify the exact database-specific connection parameters in the
    // client, you need to set up one or several TCcServerTransport components and
    // define for each one a DatabaseAlias. This alias uniquely identifies this
    // connection, and should therefore be used in the client in order to establish te
    // connection.
    // See Also:
    // TCcServerTransport.DatabaseAlias
    property DatabaseAlias: string read FDatabaseAlias write FDatabaseAlias;

    // KeepAliveInterval is the interval in seconds between the keep-alive signals that are emitted when the connection is open
    // These signals are only sent out when the connection is idle, so they do not slow down performance
    property KeepAliveInterval: Integer read GetKeepAliveInterval write SetKeepAliveInterval default 30;

    // Password for connecting to the CopyCat server. This password must correspond to the password specified in TCcServerTransport.Password.
    // See also: TCcServerTransport.Password
    property Password: string read FPassword write FPassword;
  end;

  // Summary:
  // Abstract ancestor for all server transport connections
  // Description:
  // TCcServerTransport implements a server-side listener, enabling TCcClientTransport objects to open a database
  // connection and execute queries through a custom transport. TCcServerTransport establishes the link between the incoming
  // remote procedure calls and the database connection (Connection).<p/><p/>
  // Descendants should override DoStartServer and DoStopServer to link with a concrete
  // remoting technology.<p/><p/>
  // To use this feature, you need to compile and install one of the transport packages
  // descending from this class (currently XMLRPC, RTC and DataSnap). You can then drop a TCcServerTransport
  // descendant onto your form and hook it up to your database connection, and call StartServer in order
  // to start listening for client connections.
  TCcServerTransport = class(TComponent)
  private
    FConnection: TCcConnection;
    FDatabaseAlias: string;
    FSessions: TStringList;
{$IFNDEF CC_D6}
    nSessionCounter: Integer;
{$ENDIF}
    FOnNewSession: TCcSessionEvent;
    FOnCloseSession: TCcSessionEvent;
    FSessionCleanupTimer: {$IFDEF FPC}TFPTimer{$ELSE}TTimer{$ENDIF};
    FPassword: string;
    FRowBatchSize: Integer;

    function GetProcedureName: string;
    function GetLoginProcName: string;
    procedure SetConnection(const Value: TCcConnection);
    procedure Exec(Conn: TCcConnection; qID: Integer; SQLParams: TCcValue; Result: TCcValue);
    procedure Prepare(Conn: TCcConnection; QueryID: Integer; SQLText: string; QryParamCheck: Boolean; QrySelectStatement: Boolean; Result: TCcValue);
    procedure DoExecuteFunction(Session: TCcSession; ObjectID: Integer; FunctionName: TCcRemoteFunction; Params: TCcValue; Result: TCcValue);
    function GetSession(nIndex: Integer): TCcSession;
    function GetSessionCount: Integer;
    procedure SetSessionTimeout(const Value: Integer);
    function GetSessionTimeout: Integer;
    procedure SessionCleanup(Sender: TObject);
    procedure CloseSession(ASession: TCcSession);
    function FetchRows(Conn: TCcConnection; QueryID: Integer; Res: TCcValue): Boolean;
  protected
    procedure SetDatabaseAlias(const Value: string); virtual;
    property ProcedureName: string read GetProcedureName;
    property LoginProcName: string read GetLoginProcName;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // ExecuteFunction should be called by descendant classes when a remote function call
    // is received, and the result should get sent back to the client
    procedure ExecuteFunction(Params: TCcValue; Result: TCcValue);
    // Login should be called by descendants upon receiving a login call in order to perform the login
    // Login returns an array of data containing the session ID, the DBType and DBVersion, and the CopyCat version of the server
    procedure Login(Params: TCcArray; Result: TCcValue);
    // Override to implement server startup
    procedure DoStartServer; virtual;
    // Override to implement server shutdown
    procedure DoStopServer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Find a session by its session ID
    function FindSession(SessionID: string): TCcSession;
    // Use Session to get a session by index
    property Session[nIndex: Integer]: TCcSession read GetSession;
    // Number of active sessions
    property SessionCount: Integer read GetSessionCount;

    // Start the server and accept incoming CopyCat clients
    procedure StartServer;
    // Stop the server
    procedure StopServer;
  published

    // Maximum number of rows to send back at a time when executing a query
    // Description: Set RowBatchSize to a number greater than 0 to indicate
    // that records are to be sent to the client in batches of RowBatchSize rows.
    // Set RowBatchSize to 0 to send them all in one packet.
    // The lower the value of RowBatchSize, the more round trips will be needed
    // to send the data, but the lower memory usage will be on both server and client sides.
    property RowBatchSize: Integer read FRowBatchSize write FRowBatchSize;

    // Alias for this database connection.
    // Description: Remote ClientTransports must connect specifing this alias,
    // rather than use an explicit database connection string.
    property DatabaseAlias: string read FDatabaseAlias write SetDatabaseAlias;

    // Connection object for accessing the server database
    property Connection: TCcConnection read FConnection write SetConnection;

    // SessionTimeout is the number of minutes of inactivity after which a session should expire.
    // When a session expires, the connection is closed and destroyed
    property SessionTimeout: Integer read GetSessionTimeout write SetSessionTimeout default 10;

    // Password that clients must specify in order to be able to open a session with this server
    // Description: The TCcClientTransport objects that connect to this server have no way of knowing the exact database
    // connection parameters that are needed, so instead, they only provide a database alias and a password.
    // It's then up to the server to define the exact user name and other connection parameters that must be used to establish
    // the connection. It is also recommended to use encryption (if supported by the underlying transport) in order
    // to further secure the exchange and avoid any intrusion.
    property Password: string read FPassword write FPassword;

    // Fired every time a new session is opened
    property OnNewSession: TCcSessionEvent read FOnNewSession write FOnNewSession;
    // Fired when a session is about to be destroyed
    property OnCloseSession: TCcSessionEvent read FOnCloseSession write FOnCloseSession;
  end;

  // Used internally to implement a TCcAbstractQueryObject in relation to a TCcClientTransport
  TCcQueryClient = class(TCcAbstractQueryObject)
  private
    FEof: Boolean;
    FRowsAffected: Integer;
    FParamList: TCcFieldList;
    FFieldList: TCcFieldList;
    FRecordBuffer: TCcValue;
    FCurrentRecNo: Integer;
    FParamCheck: Boolean;
    FMoreRowsToFetch: Boolean;
    function GetTransport: TCcClientTransport;
    procedure Fetch;
    function ArrayHasElement(val: TCcValue; nIndex: Integer): Boolean;
  protected
    function GetRowsAffected: Integer; override;
    function GetEof: Boolean; override;
    procedure DoInitParams(ParamList: TStringList); override;
    procedure DoInitFields(FieldList: TStringList); override;
    procedure DoExec; override;
    procedure DoPrepare(SQLText: string); override;
    procedure DoUnPrepare; override;
    procedure SetParamCheck(Value: Boolean); override;
    procedure DoClose; override;
    procedure DoNext; override;
    function GetFieldType(FieldName: string; IsParam: Boolean): TFieldType; override;
    function GetFieldSize(FieldName: string; IsParam: Boolean): Integer; override;
    function GetFieldValue(Field: TCcField): Variant; override;
    procedure SetFieldValue(Field: TCcField; val: Variant); override;
  public
    property Transport: TCcClientTransport read GetTransport;
    constructor Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean); override;
    destructor Destroy; override;
  end;

  TCcFieldType = record
    ft: TFieldType;
    Name: string;
  end;

const
{$IFDEF CC_D2K9}
  FieldTypeCount = 25;
{$ELSE}
{$IFDEF CC_D2K6}
  FieldTypeCount = 24;
{$ELSE}
{$IFDEF CC_D6}
  FieldTypeCount = 21;
{$ELSE}
  FieldTypeCount = 20;
{$ENDIF}
{$ENDIF}
{$ENDIF}


type
  TCcFieldTypes = array [0 .. FieldTypeCount] of TCcFieldType;

implementation

uses CCat, {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  SysUtils
{$IFDEF CC_D2K15}, System.Diagnostics{$ENDIF}
{$IFDEF CC_USEVARIANTS}, Variants{$ENDIF};

const
  CcFieldTypes: TCcFieldTypes = ((ft: ftUnknown; name: 'UNKNOWN'), (ft: ftString; name: 'STRING'),
    (ft: ftFloat; name: 'FLOAT'), (ft: ftSmallint; name: 'SMALLINT'), (ft: ftInteger; name: 'INTEGER'),
    (ft: ftDateTime; name: 'DATETIME'), (ft: ftDate; name: 'DATE'), (ft: ftTime; name: 'TIME'),
    (ft: ftMemo; name: 'MEMO'), (ft: ftBlob; name: 'BLOB'), (ft: ftBoolean; name: 'BOOLEAN'),
    (ft: ftArray; name: 'ARRAY'), (ft: ftLargeint; name: 'LARGEINT'), (ft: ftFixedChar; name: 'FIXEDCHAR'),
    (ft: ftWideString; name: 'WIDESTRING'),
{$IFDEF CC_D2K6}
    (ft: ftTimeStamp; name: 'DATETIME'),
    (ft: ftFixedWideChar; name: 'FIXEDWIDECHAR'),
    (ft: ftWideMemo; name: 'WIDEMEMO'),
{$ENDIF}
{$IFDEF CC_D2K9}
    (ft: ftLongword; name: 'LONGWORD'),
{$ENDIF}
    (ft: ftCurrency; name: 'CURRENCY'),
    (ft: ftBCD; name: 'BCD'),
{$IFDEF CC_D6}
    (ft: ftFMTBcd; name: 'FMTBCD'),
{$ENDIF}
    (ft: ftWord; name: 'WORD'),
    (ft: ftAutoInc; name: 'AUTOINC'), (ft: ftGraphic; name: 'GRAPHIC'),
    (ft: ftGuid; name: 'GUID')
    );


    { Missing types...
    ftBytes, ftVarBytes, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch,
    ftOraTimeStamp, ftOraInterval,
    ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream,
    ftTimeStampOffset, ftObject, ftSingle}

function DataTypeByName(cDataTypeName: string): TFieldType;
var
  I: Integer;
begin
  Result := ftUnknown;
  for I := 0 to high(CcFieldTypes) do
  begin
    if CcFieldTypes[I].Name = cDataTypeName then
    begin
      Result := CcFieldTypes[I].ft;
      Exit;
    end;
  end;
end;

function NameOfDataType(ftDataType: TFieldType): string;
var
  I: Integer;
begin
  Result := 'UNKNOWN';
  for I := 0 to high(CcFieldTypes) do
  begin
    if CcFieldTypes[I].ft = ftDataType then
    begin
      Result := CcFieldTypes[I].Name;
      Exit;
    end;
  end;
end;

{ TCcTransQuery }

constructor TCcQueryClient.Create(Conn: TCcConnection; qry: TCcQuery; nID: Integer; Select: Boolean);
begin
  inherited;
  FRowsAffected := -1;
  // FRecNo := -1;
  FCurrentRecNo := -1;
  FEof := False;
  FParamList := TCcFieldList.Create;
  FFieldList := TCcFieldList.Create;
  FRecordBuffer := TCcValue.Create;
end;

destructor TCcQueryClient.Destroy;
begin
  FParamList.Free;
  FFieldList.Free;
  FRecordBuffer.Free;
  inherited;
end;

procedure TCcQueryClient.DoClose;
begin
  // Transport.RemoteCall(Name, funcQryClose, nil, nil);
  FRecordBuffer.Clear;
  FCurrentRecNo := -1;
end;

function TCcQueryClient.ArrayHasElement(val: TCcValue; nIndex: Integer): Boolean;
begin
  Result := False;
  if val.ValueType = vtArray then
  begin
    if val.AsArray.Count >= nIndex + 1 then
    begin
      if val.AsArray[nIndex].ValueType <> vtNull then
        Result := True;
    end;
  end;
end;

procedure TCcQueryClient.DoExec;
var
  I: Integer;
  SQLParams, ResultSet: TCcValue;
  FieldDefs: TCcArray;
  fieldInfo: TCcFieldInfo;
  cNeedFieldDefs: string;
  val: Variant;
  paramType: TFieldType;

  { function ArrayHasElement(val: TCcValue; nIndex: Integer): Boolean;
    begin
    Result := False;
    if val.ValueType = vtArray then begin
    if val.AsArray.Count >= nIndex+1 then begin
    if val.AsArray[nIndex].ValueType <> vtNull then
    Result := True;
    end;
    end;
    end; }

begin
  ResultSet := TCcValue.Create;
  SQLParams := TCcValue.Create;
  FCurrentRecNo := -1;
  FEof := True;
  try
    // We have to tell the server if whether we need the field defs or not
    // We only ask for the field defs if this is the first execute after prepare
    // After unpreparing, we of course need to request them again
    if Query.Properties.Values['FIELD_DEFS_RECEIVED'] = 'Y' then
      cNeedFieldDefs := 'N'
    else
      cNeedFieldDefs := 'Y';
    SQLParams.AsArray.Add.Value := cNeedFieldDefs;

    // We pass all the SQL parameters in bulk to the exec method for performance reasons
    for I := 0 to FParamList.Count - 1 do
    begin
      val := FParamList[I].Value;

      // By default, use the parameter type provided by the database when preparing the query
      paramType := FParamList[I].DataType;

      if (paramType = ftUnknown) then
      begin
        // Failing that, use the parameter type specified when setting the parameter value (eg. using SetValueAsType)
        paramType := Query.Param[FParamList[I].FieldName].DataType;

        if (paramType = ftUnknown) then
        begin
          // If there's still no value provided, we have to guess the param type from the value provided
          if (VarType(val) = varDate) then
            paramType := ftDateTime
          else if (VarType(val) = varInteger) {$IFDEF CC_D6} or (VarType(val) = varShortInt) {$ENDIF} or (VarType(val) = varSmallint) then
            paramType := ftInteger
          else if (VarType(val) = varBoolean) then
            paramType := ftBoolean
          else if (VarType(val) = varDouble) or (VarType(val) = varSingle) then
            paramType := ftFloat
{$IFDEF CC_D2K9}
          else if (VarType(val) = varLongWord) then // Added by Kick Martens
            paramType := ftLongword
{$ENDIF}
{$IFDEF CC_D6}
          else if (VarType(val) = varWord) then // Added by Kick Martens
            paramType := ftWord
{$ENDIF}
          else
            paramType := ftString;
        end;
      end;

      with SQLParams.AsArray.Add.AsArray do begin
        Add.SetValueAsType(val, paramType);
        Add.Value := NameOfDataType(paramType);
      end;
    end;

    // Execute the function
    Transport.RemoteCall(ID, funcQryExec, SQLParams, ResultSet);
    if ArrayHasElement(ResultSet, 0) then
      FRowsAffected := ResultSet.AsArray[0].AsField.Value;

    // Field definitions
    // They are only sent upon first execution, for performance reasons
    // NOTE: it's important to process the field defs before the field values below
    if ArrayHasElement(ResultSet, 3) then
    begin
      FFieldList.Clear;
      FieldDefs := ResultSet.AsArray[3].AsArray;
      for I := 0 to FieldDefs.Count - 1 do
      begin
        fieldInfo := TCcFieldInfo.Create;
        fieldInfo.FieldName := FieldDefs[I].AsArray[0].Value;
        fieldInfo.DataType := DataTypeByName(FieldDefs[I].AsArray[1].Value);
        fieldInfo.Size := FieldDefs[I].AsArray[2].Value;
        FFieldList.Add(fieldInfo);
      end;
    end;

    if ArrayHasElement(ResultSet, 2) then
      FMoreRowsToFetch := ResultSet.AsArray[2].Value;

    if ArrayHasElement(ResultSet, 1) then
    begin
      // Buffer all the data
      FRecordBuffer.Assign(ResultSet.AsArray[1]);

      // Fetch first line of data
      Fetch;
    end;
  finally
    ResultSet.Free;
    SQLParams.Free;
  end;
end;

procedure TCcQueryClient.DoInitFields(FieldList: TStringList);
var
  I: Integer;
begin
  for I := 0 to FFieldList.Count - 1 do
    FieldList.Add(FFieldList[I].FieldName);
end;

procedure TCcQueryClient.DoInitParams(ParamList: TStringList);
var
  I: Integer;
begin
  for I := 0 to FParamList.Count - 1 do
    ParamList.Add(FParamList[I].FieldName);
end;

procedure TCcQueryClient.DoNext;
begin
  { if FRecNo <= FRecordCount then
    Inc(FRecNo)
    else
    FEof := True; }
  if not FEof then
    Fetch;
end;

procedure TCcQueryClient.DoPrepare(SQLText: string);
var
  val: TCcValue;
  I: Integer;
  paramInfo: TCcFieldInfo;
  ParamDefs: TCcArray;
begin
  val := TCcValue.Create;
  try
    // FFieldList.Clear;
		Transport.RemoteCall(ID, funcQryPrepare, [SQLText, FParamCheck, SelectStatement], val);
    FParamList.Clear;

		if (val.AsArray.Count = 0) then
			Transport.SignalConnectLost;

		if (val.AsArray[0].ValueType = vtArray) then begin
			ParamDefs := val.AsArray[0].AsArray;
			for I := 0 to ParamDefs.Count - 1 do
			begin
				with ParamDefs[I].AsArray do
				begin
					paramInfo := TCcFieldInfo.Create;
					paramInfo.FieldName := Value[0].Value;
					paramInfo.DataType := DataTypeByName(Value[1].Value);
					paramInfo.Size := Value[2].Value;
					FParamList.Add(paramInfo);
				end;
			end;
		end;
	finally
		val.Free;
	end;
end;

procedure TCcQueryClient.DoUnPrepare;
begin
  // We don't actually unprepare the query on the server (to save performance).
  // It doesn't matter, because when the session gets cleaned up, all the queries
  // will get destroyed and therefore unprepared

  FParamList.Clear;
  FFieldList.Clear;
  // We clear this property to show that we need to request the field defs
  // again after unpreparing the query
  Query.Properties.Values['FIELD_DEFS_RECEIVED'] := 'N';
end;

procedure TCcQueryClient.Fetch;

  procedure FetchRowsFromServer;
  var
    I: Integer;
    SQLParams, ResultSet: TCcValue;
    FieldDefs: TCcArray;
    fieldInfo: TCcFieldInfo;
    cNeedFieldDefs: string;
    val: Variant;
    paramType: TFieldType;

  begin
    ResultSet := TCcValue.Create;
    FCurrentRecNo := -1;
    try
      // Execute the function
      Transport.RemoteCall(ID, funcQryFetch, nil, ResultSet);

      if ArrayHasElement(ResultSet, 1) then
        FMoreRowsToFetch := ResultSet.AsArray[1].Value;

      if ArrayHasElement(ResultSet, 0) then
      begin
        // Buffer all the data
        FRecordBuffer.Assign(ResultSet.AsArray[0]);
      end;
    finally
      ResultSet.Free;
    end;
  end;

var
  // ResultSet: TCcValue;
  CurrentRec: TCcArray;
  I: Integer;
begin
  if FCurrentRecNo >= FRecordBuffer.AsArray.Count - 1 then begin
    if FMoreRowsToFetch then
      FetchRowsFromServer
    else begin
      FEof := True;
      Exit;
    end;
  end;

  if FRecordBuffer.ValueType = vtNull then
    // Query is inactive
    FEof := True
  else
  begin
    FEof := False;
    Inc(FCurrentRecNo);
    CurrentRec := FRecordBuffer.AsArray[FCurrentRecNo].AsArray;
    for I := 0 to CurrentRec.Count - 1 do
    begin
      FFieldList[I].Value := CurrentRec[I].Value;
    end;
  end;
end;

function TCcQueryClient.GetEof: Boolean;
begin
  Result := FEof;
end;

function TCcQueryClient.GetFieldSize(FieldName: string;
  IsParam: Boolean): Integer;
begin
  if IsParam then
    Result := FParamList.FieldInfoByName[FieldName].Size
  else
    Result := FFieldList.FieldInfoByName[FieldName].Size;
end;

function TCcQueryClient.GetFieldType(FieldName: string;
  IsParam: Boolean): TFieldType;
begin
  if IsParam then
    Result := FParamList.FieldInfoByName[FieldName].DataType
  else
    Result := FFieldList.FieldInfoByName[FieldName].DataType;
end;

function TCcQueryClient.GetFieldValue(Field: TCcField): Variant;
begin
  if Field.IsParam then
    Result := FParamList.FieldInfoByName[Field.FieldName].Value
  else
    Result := FFieldList.FieldInfoByName[Field.FieldName].Value;
end;

function TCcQueryClient.GetRowsAffected: Integer;
begin
  Result := FRowsAffected;
end;

function TCcQueryClient.GetTransport: TCcClientTransport;
begin
  Result := (Connection as TCcClientTransport);
end;

procedure TCcQueryClient.SetFieldValue(Field: TCcField; val: Variant);
// var
// rpcVal: TCcValue;
begin
  // This method is only used for parameters, never fields.
  if Field.IsParam then
    FParamList.FieldInfoByName[Field.FieldName].Value := val;

  // //This method is only used for parameters, never fields.
  // if Field.IsParam then begin
  // rpcVal := TCcValue.Create;
  // rpcVal.AsArray.Add.Value := Field.FieldName;
  // rpcVal.AsArray.Add.SetValueAsType(Val, Field.DataType);
  // FParamList.FieldInfoByName[Field.FieldName].Value := Val;
  // Transport.RemoteCall(Name, funcQrySetParam, rpcVal, nil);
  // rpcVal.Free;
  // end;
end;

procedure TCcQueryClient.SetParamCheck(Value: Boolean);
begin
  // We buffer this parameter and pass it with the prepare, for performance reasons
  FParamCheck := Value;
  // Transport.RemoteCall(Name, funcQryParamCheck, [Value], nil);
end;

{ TCcTransport }

constructor TCcClientTransport.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FConnected := False;
  FInTrasaction := False;
  FAutoCommit := False;
  for I := 0 to CcAvailableAdaptors.Count - 1 do
    AddDBAdaptor(TCcDBAdaptorClass(CcAvailableAdaptors[I]));

  FInRemoteCall := False;
  FCS := Syncobjs.TCriticalSection.Create;

  FKeepAliveTimer := {$IFDEF FPC}TFPTimer{$ELSE}TTimer{$ENDIF}.Create(Self);
  FKeepAliveTimer.OnTimer := KeepAliveTimer;
  FKeepAliveTimer.Enabled := False;
  SetKeepAliveInterval(30);
end;

function TCcClientTransport.GetConnectorConnected: Boolean;
begin
  Result := FConnected;
end;

function TCcClientTransport.GetInTransaction: Boolean;
begin
  Result := FInTrasaction;
end;

procedure TCcClientTransport.DoDisconnect;
begin
  try
    RemoteCall(funcConnDisconnect, nil, nil);
  finally
    CleanupSession;
  end;
end;

procedure TCcClientTransport.DoCommit;
begin
  if not FAutoCommit then
    RemoteCall(funcConnCommit, nil, nil);
  FInTrasaction := False;
end;

procedure TCcClientTransport.DoCommitRetaining;
begin
  if not FAutoCommit then
    RemoteCall(funcConnCommitRetaining, nil, nil);
  FInTrasaction := False;
end;

procedure TCcClientTransport.DoBeforeConnect;
var
  vReturn, vParams: TCcValue;
begin
  // We have to start by logging into the server
  vReturn := TCcValue.Create;
  vParams := TCcValue.Create;
  try
    vParams.AsArray.Add.Value := VersionNumber;
    vParams.AsArray.Add.Value := FPassword;
    DoRemoteCall(LoginProcName, vParams, vReturn);
    if vReturn.AsArray.Count >= 4 then
    begin
      FSessionID := vReturn.AsArray[0].Value;
      DBType := vReturn.AsArray[1].Value;
      DBVersion := vReturn.AsArray[2].Value;

      if (vReturn.AsArray.Count >= 5) then
      begin
        DBName := vReturn.AsArray[3].Value;
        FServerCopyCatVersion := vReturn.AsArray[4].Value;
      end
      else
        FServerCopyCatVersion := vReturn.AsArray[3].Value;

      if (vReturn.AsArray.Count >= 6) then
      begin
        FServerConnectorSupportsRowsAffected := vReturn.AsArray[5].Value;
      end;

    end
    else
      raise Exception.Create('Cannot connect to database');

    FSessionActive := True;
  finally
    vReturn.Free;
    vParams.Free;
  end;
end;

procedure TCcClientTransport.DoConnect;
var
  // vParams,
  vReturn: TCcValue;
  // I: Integer;
begin
  // vParams := TCcValue.Create;
  vReturn := TCcValue.Create;
  try
    // with vParams.AsArray.Add.AsArray do begin
    // for I:=0 to ConnectionParams.Count-1 do
    // Add.Value := ConnectionParams[i];
    // Add.Value := ConnectionParams.Names[i];
    // Add.Value := ConnectionParams.Values[ConnectionParams.Names[i]];
    // end;
    // Add.Value := UserLogin;
    // Add.Value := UserPassword;
    // Add.Value := RoleName;
    // end;
    RemoteCall(funcConnConnect, [FReplicatingNode, FReplicatingNodePassword], vReturn);

    // We get back the connection parameters, so that our local DBAdaptor object will be able to know
    // what the actual, remote, database-specific connection parameters are. This can be important for
    // settings -- such as Interbase SQL dialect -- that can affect the SQL that is generated
    ConnectionParams.Text := vReturn.Value;

    FConnected := True;

    // Start the keep-alive timer
    FKeepAliveTimer.Enabled := True;
  finally
    // vParams.Free;
    vReturn.Free;
  end;
end;

procedure TCcClientTransport.DoRollback;
begin
  if not FAutoCommit then
    RemoteCall(funcConnRollback, nil, nil);
  FInTrasaction := False;
end;

procedure TCcClientTransport.DoRollbackRetaining;
begin
  if not FAutoCommit then
    RemoteCall(funcConnRollbackRetaining, nil, nil);
  FInTrasaction := False;
end;

procedure TCcClientTransport.DoStartTransaction;
begin
  if not FAutoCommit then
    RemoteCall(funcConnStartTransaction, nil, nil);
  FInTrasaction := True;
end;

function TCcClientTransport.GetProcedureName: string;
begin
  Result := 'copycat.' + DatabaseAlias + '.RemoteCall';
end;

function TCcClientTransport.NewQueryObject(qry: TCcQuery; nID: Integer): TCcAbstractQueryObject;
begin
  // Create the query on the server
  // RemoteCall(funcConnNewQuery, [cName], nil);

  // Create the query locally
  Result := TCcQueryClient.Create(Self, qry, nID, qry.SelectStatement);
end;

procedure TCcClientTransport.RemoteCall(FunctionName: TCcRemoteFunction;
  Params: array of Variant; Result: TCcValue);
begin
  RemoteCall(-1, FunctionName, Params, Result);
end;

procedure TCcClientTransport.RemoteCall(FunctionName: TCcRemoteFunction;
  Params, Result: TCcValue);
begin
  RemoteCall(-1, FunctionName, Params, Result);
end;

procedure TCcClientTransport.RemoteCall(ObjectID: Integer;
  FunctionName: TCcRemoteFunction; Params: array of Variant; Result: TCcValue);
var
  I: Integer;
  val: TCcValue;
begin
  val := TCcValue.Create;
  if high(Params) = 0 then
    val.Value := Params[0]
  else
    for I := 0 to high(Params) do
      val.AsArray.Add.Value := Params[I];
  RemoteCall(ObjectID, FunctionName, val, Result);
end;

{ TCcTransportServer }

procedure TCcServerTransport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FConnection then
      SetConnection(nil);
  end;
  inherited;
end;

procedure TCcServerTransport.ExecuteFunction(Params: TCcValue; Result: TCcValue);
var
  Session: TCcSession;
  SessionID: string;
  nIndex: Integer;
begin
  SessionID := Params.AsArray[0].Value;
  nIndex := FSessions.IndexOf(SessionID);
  if nIndex = -1 then
  begin
    Result.AsArray.Add.Value := 'ERROR:NOT_LOGGED_IN';
    Exit;
  end;
  // raise Exception.Create('You must be logged in before executing this remote function call');

  // Return the session ID to the client, so that the same session can be used again
  Result.AsArray.Add.Value := SessionID;

  Session := TCcSession(FSessions.Objects[nIndex]);
{$IFDEF CC_D2K15}
  Session.FLastActivity := TStopWatch.GetTimeStamp;
{$ELSE}
  Session.FLastActivity := GetTickCount;
{$ENDIF}
  // Execute the function, and return the result as the second element of the result array
  DoExecuteFunction(Session, Params.AsArray[1].Value, Params.AsArray[2].Value, Params.AsArray[3], Result.AsArray.Add);
end;

{ procedure TCcServerTransport.Fetch(Conn: TCcConnection; QueryID: Integer; Result: TCcValue);
  var
  i: Integer;
  fieldValue: TCcArray;
  begin
  with Conn.Query[IntToStr(QueryID)] do begin
  if not Eof then begin
  for I := 0 to FieldCount - 1 do begin
  fieldValue := Result.AsArray.Add.AsArray;
  with FieldByIndex[i] do begin
  fieldValue.Add.Value := FieldName;
  fieldValue.Add.SetValueAsType(Value, DataType);
  end;
  end;
  Next;
  end;
  end;
  end;
}

function TCcServerTransport.GetLoginProcName: string;
begin
  Result := 'copycat.' + DatabaseAlias + '.Login';
end;

function TCcServerTransport.GetProcedureName: string;
begin
  Result := 'copycat.' + DatabaseAlias + '.RemoteCall';
end;

procedure TCcServerTransport.Prepare(Conn: TCcConnection; QueryID: Integer; SQLText: string; QryParamCheck: Boolean; QrySelectStatement: Boolean; Result: TCcValue);
var
  I: Integer;
  { currField, } currParam: TCcField;
  ParamDefs { , FieldDefs } : TCcArray;
  q: TCcQuery;
begin
  if QrySelectStatement then
    q := Conn.SelectQuery[IntToStr(QueryID)]
  else
    q := Conn.UpdateQuery[IntToStr(QueryID)];
  with q do
  begin
    Close;
    SQL.Text := SQLText;
    ParamCheck := QryParamCheck;
    Prepare;

    ParamDefs := Result.AsArray.Add.AsArray;
    for I := 0 to ParamCount - 1 do
    begin
      currParam := ParamByIndex[I];
      with ParamDefs.Add.AsArray do
      begin
        Add.Value := currParam.FieldName;
        Add.Value := NameOfDataType(currParam.DataType);
        Add.Value := currParam.Size;
      end;
    end;
    // FieldDefs := Result.AsArray.Add.AsArray;
    // for I := 0 to FieldCount - 1 do begin
    // currField := FieldByIndex[i];
    // with FieldDefs.Add.AsArray do begin
    // Add.Value := currField.FieldName;
    // Add.Value := NameOfDataType(currField.DataType);
    // Add.Value := currField.Size;
    // end;
    // end;
  end;
end;

procedure TCcServerTransport.SetConnection(const Value: TCcConnection);
begin
  FConnection := Value;
end;

procedure TCcServerTransport.SetDatabaseAlias(const Value: string);
begin
  FDatabaseAlias := Value;
end;

constructor TCcServerTransport.Create(AOwner: TComponent);
begin
  inherited;
  FSessions := TStringList.Create;
  FSessionCleanupTimer := {$IFDEF FPC}TFPTimer{$ELSE}TTimer{$ENDIF}.Create(Self);
  FSessionCleanupTimer.Enabled := False;
  FSessionCleanupTimer.OnTimer := SessionCleanup;
  SessionTimeout := 10;

{$IFNDEF CC_D6}
  nSessionCounter := 0;
{$ENDIF}
end;

procedure TCcServerTransport.SessionCleanup(Sender: TObject);
var
  I: Integer;
  Session: TCcSession;
begin
  for I := FSessions.Count - 1 downto 0 do
  begin
    Session := TCcSession(FSessions.Objects[I]);
{$IFDEF CC_D2K15}
    if (TStopWatch.GetTimeStamp - Session.FLastActivity) > (SessionTimeout * 60000) then
{$ELSE}
    if (GetTickCount - Session.FLastActivity) > (SessionTimeout * 60000) then
{$ENDIF}
      // Session expired
      CloseSession(Session);
  end;
end;

procedure TCcServerTransport.CloseSession(ASession: TCcSession);
begin
  if Assigned(OnCloseSession) then
    OnCloseSession(Self, ASession);
  if Assigned(ASession.Connection) then
    if ASession.Connection.Connected then
      ASession.Connection.Disconnect;
  FSessions.Delete(FSessions.IndexOf(ASession.SessionID));
  ASession.Free;
end;

destructor TCcServerTransport.Destroy;
begin
  FSessions.Free;
  FSessionCleanupTimer.Free;
  inherited;
end;

procedure TCcServerTransport.DoExecuteFunction(Session: TCcSession; ObjectID: Integer; FunctionName: TCcRemoteFunction; Params,
  Result: TCcValue);

  procedure CheckParamCount(ParamCount: Integer);
  begin
    if not(Assigned(Params) and (((Params.ValueType = vtArray)
      and (Params.AsArray.Count = ParamCount)) or (ParamCount = 1))) then
      raise Exception.Create('Invalid parameters for call to function ' + IntToStr(Integer(FunctionName)) + '!');
  end;
// var
// I: Integer;

begin
  case FunctionName of
    // funcLogin: begin
    // CheckParamCount(1);
    // Login(Params.Value);
    // end;

    funcKeepAlive:
      begin
        // Nothing to do here, just calling the function is enough to keep the session alive.
      end;

    // funcConnNewQuery:
    // Session.Connection.Query[IntToStr(ObjectID)];

    funcConnDisconnect:
      // Here, we just close the session, which implicitly closes the connection as well
      CloseSession(Session);

    funcConnConnect:
      begin
        CheckParamCount(2);
        Session.Connection.ConnectAsNode(Params.AsArray[0].Value, Params.AsArray[1].Value);
        Result.Value := Session.Connection.ConnectionParams.Text;
      end;

    funcConnCommit:
      Session.Connection.Commit;

    funcConnCommitRetaining:
      Session.Connection.CommitRetaining;

    funcConnRollback:
      Session.Connection.Rollback;

    funcConnRollbackRetaining:
      Session.Connection.RollbackRetaining;

    funcConnStartTransaction:
      Session.Connection.StartTransaction;

    funcQryExec:
      begin
        // Close, set parameters, execute, return the field list and values, and close again
        Exec(Session.Connection, ObjectID, Params, Result);
      end;

    funcQryFetch:
       FetchRows(Session.Connection, ObjectID, Result);

    funcQryPrepare:
      begin
        // Prepare and return the param list
        CheckParamCount(3);
        Prepare(Session.Connection, ObjectID, Params.AsArray[0].Value, Params.AsArray[1].Value, Params.AsArray[2].Value, Result);
      end;

    // funcQryClose:
    // Session.Connection.Query[IntToStr(ObjectID)].Close;

    // funcQryUnPrepare:
    // Session.Connection.Query[IntToStr(ObjectID)].UnPrepare;

    // funcQrySetParam: begin
    // CheckParamCount(2);
    // Session.Connection.Query[IntToStr(ObjectID)].Param[Params.AsArray[0].Value].Value := Params.AsArray[1].Value;
    // end;

    // funcQryParamCheck: begin
    // CheckParamCount(1);
    // Session.Connection.Query[IntToStr(ObjectID)].ParamCheck := Params.Value;
    // end

  else
    raise Exception.Create('Invalid remote function code (' + IntToStr(Integer(FunctionName)) + ')!');
  end;
end;

function TCcServerTransport.FetchRows(Conn: TCcConnection; QueryID: Integer; Res: TCcValue): Boolean;
var
  Records, FieldValues: TCcArray;
  I: Integer;
begin
  Records := Res.AsArray.Add.AsArray;

  with Conn.FindQuery(IntToStr(QueryID)) do
  begin
    while not (Eof or ((RowBatchSize > 0) and (Records.Count >= RowBatchSize))) do
    begin
      FieldValues := Records.Add.AsArray;
      for I := 0 to FieldCount - 1 do
      begin
        with FieldByIndex[I] do
          FieldValues.Add.SetValueAsType(Value, DataType);
      end;
      Next;
    end;
    //Return boolean to tell if there are more rows or not
    Result := not Eof;
    Res.AsArray.Add.Value := Result;
  end;
end;

procedure TCcServerTransport.Exec(Conn: TCcConnection; qID: Integer; SQLParams: TCcValue; Result: TCcValue);
var
  I: Integer;
  currField: TCcField;
  FieldDefs: TCcArray;
  cNeedFieldDefs: string;
  lMoreRows: Boolean;
  p: TCcArray;
  dt : TFieldType;
begin
  lMoreRows := False;
  with Conn.FindQuery(IntToStr(qID)) do
  begin
    Close;
    cNeedFieldDefs := SQLParams.AsArray[0].Value;
    if SQLParams <> nil then
      for I := 1 to SQLParams.AsArray.Count - 1 do begin
        p := SQLParams.AsArray[I].AsArray;
        dt := p[0].AsField.DataType;
        if dt = ftUnknown then
          dt := DataTypeByName(p[1].Value);
        ParamByIndex[I - 1].SetValueAsType(p[0].Value, dt);
      end;
    Exec;
    Result.AsArray.Add.Value := RowsAffected;
    if FieldCount > 0 then
    begin
      lMoreRows := FetchRows(Conn, qID, Result);
    end;

    if cNeedFieldDefs = 'Y' then
    begin
      FieldDefs := Result.AsArray.Add.AsArray;
      for I := 0 to FieldCount - 1 do
      begin
        currField := FieldByIndex[I];
        with FieldDefs.Add.AsArray do
        begin
          Add.Value := currField.FieldName;
          Add.Value := NameOfDataType(currField.DataType);
          Add.Value := currField.Size;
        end;
      end;
    end;
    if not lMoreRows then
      Close;
  end;
end;

procedure TCcClientTransport.KeepAliveTimer(Sender: TObject);
begin
  if FSessionActive and not FInRemoteCall and not(csDesigning in ComponentState) then begin
    try
      RemoteCall(funcKeepAlive, nil, nil);
    except
      //We need to swallow any exception that occurs, otherwise it might fire an exception at the wrong
      //time while we're doing something else, since the keepalive is sent on a timer.

      //If there was a connection loss, we will have called the TCcConnection.FConnectionLost flag
      //will be set, so TCcConnection.GetConnected will return false, which means that next time
      //someone tries to use the connection, a new connection loss event will correctly get fired.
    end;
  end;
end;

procedure TCcClientTransport.RemoteCall(ObjectID: Integer;
  FunctionName: TCcRemoteFunction; Params, Result: TCcValue);
var
  res: TCcValue;
  realParams: TCcValue;
  cSessionID: String;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if not FSessionActive then
    Exit;

  // Reset the keep-alive timer
  FKeepAliveTimer.Enabled := False;
  FKeepAliveTimer.Enabled := True;

  // else raise Exception.Create('This function call cannot be made before logging in to the server');

  res := TCcValue.Create;
  realParams := TCcValue.Create;
  FCS.Enter;
  FInRemoteCall := True;
  try
    realParams.AsArray.Add.Value := FSessionID;
    realParams.AsArray.Add.Value := ObjectID;
    realParams.AsArray.Add.Value := FunctionName;
    realParams.AsArray.AddValue(Params);

    DoRemoteCall(ProcedureName, realParams, res);

//    if res.ValueType = vtField then
//    begin
      //if VarIsNull(res.Value) or (res.Value = 'ERROR:NOT_LOGGED_IN') then
//        SignalConnectLost;
//    end
    //else
    if res.ValueType = vtArray then
    begin
      if res.AsArray[0] = nil then
        SignalConnectLost;
      // raise Exception.Create('Internal transport error: remote function call returned incoherent result!');

      cSessionID := res.AsArray[0].Value;
    end
    else
      SignalConnectLost;

    // If there no session ID, it means the session has expired
    if (cSessionID = '') or (cSessionID <> FSessionID) then
      SignalConnectLost;

    if Assigned(Result) and (res.AsArray.Count = 2) then
      Result.Assign(res.AsArray[1]);
  finally
    FInRemoteCall := False;
    FCS.Leave;
    res.Free;
    realParams.Free;
  end;
end;

function TCcClientTransport.RowsAffectedSupported: Boolean;
begin
  Result := FServerConnectorSupportsRowsAffected;
end;

procedure TCcClientTransport.SignalConnectLost;
begin
  CleanupSession;
  inherited;
end;

procedure TCcClientTransport.CleanupSession;
begin
  FConnected := False;
  FSessionID := '';
  FSessionActive := False;
  FKeepAliveTimer.Enabled := False;
end;

procedure TCcClientTransport.DoRemoteCall(FunctionName: string; Params: TCcValue; Result: TCcValue);
begin

end;

function TCcServerTransport.FindSession(
  SessionID: string): TCcSession;
var
  nIndex: Integer;
begin
  nIndex := FSessions.IndexOf(SessionID);
  if nIndex > -1 then
    Result := GetSession(nIndex)
  else
    Result := nil;
end;

function TCcServerTransport.GetSession(
  nIndex: Integer): TCcSession;
begin
  Result := TCcSession(FSessions[nIndex]);
end;

function TCcServerTransport.GetSessionCount: Integer;
begin
  Result := FSessions.Count;
end;

destructor TCcClientTransport.Destroy;
begin
  FKeepAliveTimer.Free;
  FCS.Free;
  inherited;
end;

procedure TCcClientTransport.SetKeepAliveInterval(const Value: Integer);
begin
  FKeepAliveTimer.Enabled := (Value > 0);
  FKeepAliveTimer.Interval := Value * 1000;
end;

function TCcClientTransport.GetKeepAliveInterval: Integer;
begin
  Result := FKeepAliveTimer.Interval div 1000;
end;

function TCcClientTransport.GetLoginProcName: string;
begin
  Result := 'copycat.' + DatabaseAlias + '.Login';
end;

procedure TCcServerTransport.Login(Params: TCcArray; Result: TCcValue);
var
{$IFDEF CC_D6}
  ID: TGUID;
{$ENDIF}
  SessionID: string;
  cPassword: string;
  Session: TCcSession;
begin
  if Params.Count <> 2 then
    raise Exception.Create('Invalid parameters for call to Login');

  cPassword := Params[1].Value;

  if cPassword <> FPassword then
    raise Exception.Create('Incorrect password for accessing this database connection');

{$IFDEF CC_D6}
  CreateGUID(ID);
  SessionID := GUIDToString(ID);
{$ELSE}
  SessionID := IntToStr(nSessionCounter);
  Inc(nSessionCounter);
{$ENDIF}
  // Create a new session and add it to the session list
  Session := TCcSession.Create(SessionID, Connection);
  Session.FClientCopyCatVersion := Params[0].Value;
  FSessions.AddObject(SessionID, Session);
  if Assigned(OnNewSession) then
    OnNewSession(Self, Session);

  Result.AsArray.Add.Value := SessionID;
  Result.AsArray.Add.Value := Connection.DBType;
  Result.AsArray.Add.Value := Connection.DBVersion;
  Result.AsArray.Add.Value := Connection.DBName;
  Result.AsArray.Add.Value := VersionNumber;
  Result.AsArray.Add.Value := Connection.CanUseRowsAffected;
end;

{ TCcSession }

constructor TCcSession.Create(ASessionID: string; AConnection: TCcConnection);
begin
  FSessionID := ASessionID;
  FConnection := TCcConnectionClass(AConnection.ClassType).Create(AConnection);
  FConnection.Assign(AConnection);
end;

destructor TCcSession.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TCcServerTransport.SetSessionTimeout(const Value: Integer);
begin
  FSessionCleanupTimer.Interval := Value * 60000;
end;

function TCcServerTransport.GetSessionTimeout: Integer;
begin
  Result := FSessionCleanupTimer.Interval div 60000;
end;

procedure TCcServerTransport.DoStopServer;
begin

end;

procedure TCcServerTransport.StartServer;
begin
  DoStartServer;
  FSessionCleanupTimer.Enabled := True;
end;

procedure TCcServerTransport.StopServer;
var
  I: Integer;
begin
  FSessionCleanupTimer.Enabled := False;
  DoStopServer;
  for I := FSessions.Count - 1 downto 0 do
    CloseSession(TCcSession(FSessions.Objects[I]));
end;

procedure TCcServerTransport.DoStartServer;
begin

end;

end.
