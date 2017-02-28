unit CcRTCTransport;

interface

{$I ..\CC.INC}

//{$R '..\CopyCat.DCR'}

uses Classes, SysUtils, DB, CcDB, CcTransports, CcProviders, rtcConn, rtcInfo, rtcCliModule, rtcFunction, rtcSrvModule, SyncObjs{$IFDEF CC_D2K9}, Generics.Collections{$ENDIF}, extctrls;

const
  NULL_STRING = '!!!XML-RPC-NULL-STRING!!!';
  NULL_VALUE = '!!!XML-RPC-NULL!!!';

type

TCcEncodingEvent = procedure (var s: String) of object;

TCcRtcAbstractClientTransport = class (TCcClientTransport)
	private
		FModuleName: String;
		FEncodeStringsAsBase64: Boolean;
		FOnDecodeString: TCcEncodingEvent;
    FOnEncodeString: TCcEncodingEvent;
    FRequestTimeout: Integer;
	public
    constructor Create(AOwner: TComponent);override;
	published
		property RequestTimeout: Integer read FRequestTimeout write FRequestTimeout;
		property ModuleName: String read FModuleName write FModuleName;
		property EncodeStringsAsBase64: Boolean read FEncodeStringsAsBase64 write FEncodeStringsAsBase64;
		property OnDecodeString: TCcEncodingEvent read FOnDecodeString write FOnDecodeString;
		property OnEncodeString: TCcEncodingEvent read FOnEncodeString write FOnEncodeString;
end;

{$IFDEF CC_D2K9}
  TCcRtcServerGateway = class;

  TCcNodeConnectionInfo = class
    private
      FGateway: TCcRtcServerGateway;
      NodeName: String;
      RequestValue: TRtcValue;
      ResponseValue: TRtcValue;
      RequestAvailable : TEvent;
      ResponseAvailable: TEvent;
      LastCallTimestamp: Cardinal;
    procedure OnConnectionLost(Sender: TRtcConnection);
    public
      constructor Create(AGateway: TCcRtcServerGateway);
      destructor Destroy; override;
  end;

  TCcNodeConnectionEvent = procedure(Sender: TCcRtcServerGateway; nodeName: String) of object;


  TCcRtcServerGateway = class (TComponent)
  private
    FKeepAliveFunction: TRtcFunction;
    ConnectedNodesSection: TCriticalSection;
    NodesByConnection: TDictionary<TRtcConnection, TCcNodeConnectionInfo>;
    FConnectedNodes: TDictionary<String, TCcNodeConnectionInfo>;
    FServerModule: TRtcServerModule;
    FOnNodeConnect: TCcNodeConnectionEvent;
    FOnNodeDisconnect: TCcNodeConnectionEvent;
    FNodeSessionTimeout: Integer;
    FNodeCleanupTimer: TTimer;
    FKeepAliveFrequency: Integer;
    FDeleting :Boolean;
    procedure KeepAliveExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure OnNodeCleanupTimer(Sender: TObject);
  protected
		procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    procedure StartServer;
    procedure StopServer;
		constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    function NodeConnect(nodeName: String; cnx: TRtcConnection): TCcNodeConnectionInfo;
    function NodeConnected(nodeName: String): Boolean;
    procedure NodeDisconnect(nodeName: String);
    property ConnectedNodes: TDictionary<String, TCcNodeConnectionInfo> read FConnectedNodes;
  published
		property ServerModule: TRtcServerModule read FServerModule write FServerModule;
    property KeepAliveFrequency: Integer read FKeepAliveFrequency write FKeepAliveFrequency;
    property NodeSessionTimeout: Integer read FNodeSessionTimeout write FNodeSessionTimeout;
    property OnNodeConnect: TCcNodeConnectionEvent read FOnNodeConnect write FOnNodeConnect;
    property OnNodeDisconnect: TCcNodeConnectionEvent read FOnNodeDisconnect write FOnNodeDisconnect;
end;

TCcRtcServerConnection = class (TCcRtcAbstractClientTransport)
  private
    FGateway: TCcRtcServerGateway;
    procedure SignalConnectLost;
  protected
    class function ConnectorName: String; override;
		procedure DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);override;
  published
    property Gateway : TCcRtcServerGateway read FGateway write FGateway;
end;
{$ENDIF}
TCcRtcClientTransport = class (TCcRtcAbstractClientTransport)
  private
    FFunctionReturned: Boolean;
		FConnectionLost: Boolean;
		FConnectionFail: Boolean;
		FConnectionError: Exception;
		FOldOnConnLost: TRtcNotifyEvent;
		FOldOnConnFail: TRtcNotifyEvent;
		FFunctionResult: TRtcResult;
		FFunctionResultValue: TRtcValue;
		FClientModule: TRtcClientModule;
 		 procedure OnConnectLost(Sender: TRtcConnection);
    procedure OnConnectFail(Sender: TRtcConnection);
    procedure FunctionReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  protected
    class function ConnectorName: String; override;
		procedure DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);override;
    procedure DoCleanup;override;
  public
		procedure Notification(AComponent: TComponent; Operation: TOperation);override;
		constructor Create(AOwner: TComponent);override;
  published
		property ClientModule: TRtcClientModule read FClientModule write FClientModule;
end;

TCcRtcAbstractServerTransport = class (TCcServerTransport)
	private
		FDataFormat: TRtcDataFormat;
    FEncodeStringsAsBase64: Boolean;
		procedure MethodExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
 	protected
		FRpcFunction: TRtcFunction;
		FLoginFunction: TRtcFunction;
	public
		constructor Create(AOwner: TComponent);override;
		destructor Destroy;override;
	published
		property DataFormat: TRtcDataFormat read FDataFormat write FDataFormat;
		property EncodeStringsAsBase64: Boolean read FEncodeStringsAsBase64 write FEncodeStringsAsBase64;
end;

TCcRtcServerTransport = class (TCcRtcAbstractServerTransport)
	private
		FFunctionGroup: TRtcFunctionGroup;
		FServer: TRtcServer;
    procedure Connect;
  protected
    procedure DoStartServer;override;
    procedure DoStopServer; override;
		procedure Notification(AComponent: TComponent; Operation: TOperation);override;
	published
		//The server to use for our remote functions
		property Server: TRtcServer read FServer write FServer;
    //The function group in which our remote functions are to be placed
		property FunctionGroup: TRtcFunctionGroup read FFunctionGroup write FFunctionGroup;
end;

{$IFDEF CC_D2K9}
TCcRtcClientGateway = class (TCcRtcAbstractServerTransport)
  private
    FClientModule: TRtcClientModule;
    FRtcResult: TRtcResult;
    procedure RtcResultReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  protected
		procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    procedure Connect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	published
		//The server to use for our remote functions
    property ClientModule: TRtcClientModule read FClientModule write FClientModule;
end;
{$ENDIF}
procedure Register;

implementation

uses
 rtcDataCli, rtcHttpCli, rtcConnProv {$IFDEF CC_D6}, Variants {$ENDIF} ;


procedure ConvertFromXMLDataType(lMimeEncodeStrings: Boolean; RtcValueObject: TRtcValueObject; CcRpcValue: TCcValue; OnDecodeString: TCcEncodingEvent);

	function GetStringValue(cVal: String): Variant;
	begin
		if cVal = NULL_VALUE then
			Result := Null
		else begin
			if lMimeEncodeStrings then
				cVal := Mime_Decode(cVal);
			if cVal = NULL_STRING then
				cVal := '';

			if Assigned(OnDecodeString) then
				OnDecodeString(cVal);

			Result := cVal;
		end;
	end;

	function GetByteStream(byteStream: TStream): String;
	var
		strStream: TStringStream;
	begin
		strStream := TStringStream.Create('');
		try
			byteStream.Position := 0;
			strStream.CopyFrom(byteStream, byteStream.Size);
			Result := strStream.DataString;
		finally
			strStream.Free;
		end;
	end;

  function GetByteStreamAsVariant(byteStream: TStream): Variant;
	var
    Data: PByteArray;
  begin
    Result := VarArrayCreate ([0, byteStream.Size - 1], varByte);
    Data := VarArrayLock(Result);
    try
      byteStream.Position := 0;
      byteStream.ReadBuffer(Data^, byteStream.Size);
    finally
      VarArrayUnlock(Result);
    end;
	end;


	procedure ProcessArray(arr: TRtcArray; CcRoot: TCcValue);
	var
		i : Integer;
		CcVal: TCcValue;
	begin
		for I := 0 to arr.Count - 1 do begin
			CcVal := CcRoot.AsArray.Add;
			case arr.isType[i] of
				rtc_String, rtc_Text: begin
					if lMimeEncodeStrings then
						CcVal.Value := GetStringValue(arr.asString[i])
					else
						CcVal.Value := GetStringValue(arr.asText[i]);
				end;
				rtc_ByteStream:
					CcVal.SetValueAsType(GetByteStreamAsVariant(arr.asByteStream[i]), ftBlob);
//					CcVal.SetValueAsType(GetByteStream(arr.asByteStream[i]), ftBlob);
				rtc_Integer, rtc_Float, rtc_LargeInt, rtc_DateTime, rtc_Boolean, rtc_Currency, rtc_Exception:
					CcVal.Value := arr.Value[i];
				rtc_Array:
					ProcessArray(arr.asArray[i], CcVal)
			else
				raise Exception.Create('Unsupported RTC data-type ' + IntToStr(Integer(arr.isType[i])) + '!');
			end;
		end;
	end;

var
	RtcValue: TRtcValue;
begin
	if RtcValueObject is TRtcArray then
		ProcessArray(RtcValueObject as TRtcArray, CcRpcValue)
	else if RtcValueObject is TRtcValue then begin
		RtcValue := RtcValueObject as TRtcValue;
		case RtcValue.isType of
			rtc_String, rtc_Text: begin
				if lMimeEncodeStrings then
					CcRpcValue.Value := GetStringValue(rtcValue.asString)
				else
					CcRpcValue.Value := GetStringValue(rtcValue.asText);
			end;
			rtc_ByteStream:
				CcRpcValue.SetValueAsType(GetByteStreamAsVariant(RtcValue.asByteStream), ftBlob);
//				CcRpcValue.SetValueAsType(GetByteStream(RtcValue.asByteStream), ftBlob);
			rtc_Integer, rtc_Float, rtc_LargeInt, rtc_DateTime, rtc_Boolean, rtc_Currency, rtc_Exception:
				CcRpcValue.Value := RtcValue.Value;
			rtc_Array:
				ProcessArray(RtcValue.asArray, CcRpcValue)
		else
			raise Exception.Create('Unsupported RTC data-type '+ IntToStr(Integer(RtcValue.isType)) + '!');
		end;
	end;
end;

procedure ConvertToXMLDataType(lMimeEncodeStrings: Boolean; CcRpcArray: TCcArray; XMLRpcArray: TRtcArray; OnEncodeString: TCcEncodingEvent);
var
	i : Integer;
	cVal: String;
	strStream: TStringStream;
  varData: PVarData;
  Data: PByteArray;
  Size: integer;
  bs: TStream;
  memStr: TMemoryStream;
  vt: TVarType;
  val: Variant;

procedure CleanUpStringValues(var s: String);
var
	I: Integer;
	s2: String;
	c: Char;
begin
	s2 := '';
	// Removing control characters except CR and LF
	for I := 1 to Length(s) do begin
		c := s[i];
		if (c >= #32) or (c = #10) or (c = #13) then
			s2 := s2 + c;
	end;
	s := s2;
end;

begin
	for i := 0 to CcRpcArray.Count - 1 do begin
		with CcRpcArray[i] do begin
			if ValueType = vtArray then
				ConvertToXMLDataType(lMimeEncodeStrings, CcRpcArray[i].AsArray, XmlRpcArray.NewArray(i), OnEncodeString)

      //If the value is null, just ignore it
			else if ValueType = vtField then
				if AsField.IsNull then begin
{					if (DataFormat = fmt_XMLRPC) then
						XMLRpcArray.Value[i] := Mime_Encode(NULL_VALUE)
					else}
						XMLRpcArray.Value[i] := NULL_VALUE;
				end
				else begin
          case AsField.DataType of
            ftString, ftGuid, ftFixedChar, ftWideString, {$IFDEF CC_D2K6} ftFixedWideChar, ftWideMemo, {$ENDIF} ftMemo, ftFmtMemo:
            begin
              cVal := Value;

							CleanUpStringValues(cVal);

							if Assigned(OnEncodeString) then
								OnEncodeString(cVal);

							//Hack to avoid trouble with empty strings...
							if cVal = '' then
								cVal := NULL_STRING;

							if lMimeEncodeStrings then
								XMLRpcArray.asString[i] := Mime_Encode(cVal)
							else
                XMLRpcArray.asText[i] := cVal;
            end;
            ftCurrency, ftBCD {$IFDEF CC_D6}, ftFMTBcd {$ENDIF}:
              XMLRpcArray.asCurrency[i] := Value;
            ftFloat:
              XMLRpcArray.asFloat[i] := Value;

            {$IFDEF CC_D6}
            ftLargeint:
              XMLRPCArray.asLargeInt[i] := Value;
            {$ENDIF}

            ftSmallint, ftInteger, ftWord, ftAutoInc:
              XMLRpcArray.asInteger[i] := Value;
            ftBoolean:
              XMLRpcArray.asBoolean[i] := Value;
            ftDateTime, ftDate, ftTime, ftTimeStamp:
              XMLRpcArray.asDateTime[i] := Value;
						ftBlob, ftGraphic, ftBytes, ftVarBytes, ftArray, ftTypedBinary:
						begin
							{$IFDEF CC_D2K9}
							val := Value;
							if VarIsNull(val) or VarIsClear(val) then
								XMLRpcArray.Value[i] := NULL_VALUE
 							else if VarIsArray(val) then begin
								bs := XMLRpcArray.NewByteStream(i);
								Size := VarArrayHighBound (Val, 1) - VarArrayLowBound(Val, 1) + 1;
								Data := VarArrayLock(Val);
								try
									bs.Position := 0;
									bs.WriteBuffer(Data^, Size);
								finally
									VarArrayUnlock(Val);
								end;
							end else begin
							{$ENDIF}

							strStream := TStringStream.Create(Value);
							try
								XMLRpcArray.NewByteStream(i).CopyFrom(strStream, strStream.Size);
							finally
								strStream.Free;
							end;

              {$IFDEF CC_D2K9}
              end;
              {$ENDIF}
						end
            else
              raise Exception.Create('Unsupported RTC data-type ' + IntToStr(Integer(CcRpcArray[i].AsField.DataType)) + '!');
          end;
        end;
		end;
  end;
end;

class function TCcRtcClientTransport.ConnectorName: String;
begin
  Result := 'RTC';
end;

{$IFDEF CC_D2K9}
class function TCcRtcServerConnection.ConnectorName: String;
begin
  Result := 'RTCSERVER';
end;
{$ENDIF}

constructor TCcRtcAbstractClientTransport.Create(AOwner: TComponent);
begin
	inherited;
  FRequestTimeout := 0;
	EncodeStringsAsBase64 := True;
end;

procedure TCcRtcClientTransport.FunctionReturn(Sender: TRtcConnection; Data: TRtcValue; Result: TRtcValue);
begin
  FFunctionReturned := True;
	FFunctionResultValue.isNull := True;
  FFunctionResultValue.asObject := Result.copyOf;
end;

procedure TCcRtcClientTransport.DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);
var
	arr: TRtcArray;
  cProcName: String;
begin
  if not Assigned(FClientModule.Client)  then
    raise Exception.Create('Cannot establish RTC connection: no client assigned');

  if Trim(ModuleName) <> '' then
    cProcName := Trim(ModuleName) + '.' + functionName
  else
    cProcName := functionName;

	FClientModule.Data.Clear;
	if (FClientModule.DataFormat = fmt_XMLRPC) then
    arr := FClientModule.Data.NewFunction(cProcName).NewArray('params').NewArray(0)
  else
    arr := FClientModule.Data.NewFunction(cProcName).NewArray('params');

  if Assigned(Params) then
		ConvertToXMLDataType(EncodeStringsAsBase64, Params.AsArray, arr, OnEncodeString);

  FFunctionResultValue.isNull := True;
  FFunctionReturned := False;

  //Hook connection loss events
  FOldOnConnLost := FClientModule.Client.OnConnectLost;
  FClientModule.Client.OnConnectLost := OnConnectLost;
  FOldOnConnFail := FClientModule.Client.OnConnectFail;
  FClientModule.Client.OnConnectFail := OnConnectFail;

  FConnectionLost := False;
	FConnectionFail := False;
	FConnectionError := nil;

	try
		//Try to connect and send request
//		FClientModule.Client.Connect;
    (FClientModule.Client as TRtcHttpClient).Blocking := True;

		if (FClientModule.Client.State <> conActive) then
			FClientModule.Client.Connect;
		FClientModule.Call(FFunctionResult);
//		if not FClientModule.WaitForCompletion(True, FRequestTimeout) then
//		  SignalConnectLost;

		if Assigned(FConnectionError) then
			raise Exception.Create(FConnectionError.Message);
	finally
		//Reassign events the way they were
		FClientModule.Client.OnConnectLost := FOldOnConnLost;
		FClientModule.Client.OnConnectFail := FOldOnConnFail;
		FConnectionError := nil;

		//Disconnect from host
//    FClientModule.Client.Disconnect;
  end;

	if FConnectionLost or FConnectionFail or not FFunctionReturned or FFunctionResultValue.isNull then begin
		//The OnConnectLost event indicates that the connection can't be established or was lost
		FConnectionLost := False;
		FConnectionFail := False;
		SignalConnectLost;
//    raise Exception.Create('Cannot connect to remote server');
	end
  else begin
		if FFunctionResultValue.isType = rtc_Exception then
			raise Exception.Create(FFunctionResultValue.asException)
		else
			ConvertFromXMLDataType(EncodeStringsAsBase64, FFunctionResultValue, Result, FOnDecodeString)
	end
end;


procedure TCcRtcClientTransport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = ClientModule then
      ClientModule := nil;
  end;
  inherited;
end;

constructor TCcRtcAbstractServerTransport.Create(AOwner: TComponent);
begin
  inherited;
	FRpcFunction := TRtcFunction.Create(Self);
	FRpcFunction.OnExecute := MethodExecute;
	FLoginFunction := TRtcFunction.Create(Self);
	FLoginFunction.OnExecute := MethodExecute;
	EncodeStringsAsBase64 := True;
end;

destructor TCcRtcAbstractServerTransport.Destroy;
begin
  FRpcFunction.Free;
  FLoginFunction.Free;
  inherited;
end;

procedure TCcRtcAbstractServerTransport.MethodExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
var
	Params, Res: TCcValue;
	rtcArr: TRtcArray;
begin
	if Param.FieldCount <> 1 then
		raise Exception.Create('Incorrect parameters for remote function call!');

	Params := TCcValue.Create;
	Res := TCcValue.Create;
	try
		//First and only parameter is an array of parameters
		rtcArr := Param.asArray[Param.FieldName[0]];
		if (rtcArr.Count = 1) and (rtcArr.isType[0] = rtc_Array) then
			rtcArr := rtcArr.asArray[0];
		ConvertFromXMLDataType(EncodeStringsAsBase64, rtcArr, Params, nil);

		//Execute the function
		if Param.FunctionName = ProcedureName then
			ExecuteFunction(Params, Res)
		else
			Login(Params.AsArray, Res);

		//Return the results to the client
		ConvertToXMLDataType(EncodeStringsAsBase64, Res.AsArray, Result.NewArray, nil);
	finally
		Params.Free;
    Res.Free;
  end;
end;

procedure Register;
begin
	RegisterComponents('CopyCat Connectors', [TCcRtcClientTransport, TCcRtcServerTransport{$IFDEF CC_D2K9}, TCcRtcServerConnection, TCcRtcServerGateway, TCcRtcClientGateway{$ENDIF}]);
end;

procedure TCcRtcClientTransport.DoCleanup;
begin
  FFunctionResult.Free;
  FFunctionResultValue.Free;
end;

procedure TCcRtcClientTransport.OnConnectLost(Sender: TRtcConnection);
begin
	if (not FConnectionLost) then begin
		if not FClientModule.Client.ReconnectOn.ConnectLost then
  		FConnectionLost := True;

		if Assigned(FOldOnConnLost) then
			FOldOnConnLost(Sender);
	end;
end;

{$IFDEF CC_D2K9}
procedure TCcRtcServerConnection.DoRemoteCall(functionName: String; Params,
  Result: TCcValue);
var
  node: TCcNodeConnectionInfo;
  cProcName: string;
  arr: TRtcArray;
begin
  if not Assigned(FGateway)  then
    raise Exception.Create('Cannot establish RTC connection: no server gateway assigned');

  if Trim(ModuleName) <> '' then
    cProcName := Trim(ModuleName) + '.' + functionName
  else
    cProcName := functionName;

  if not Gateway.NodeConnected(DatabaseAlias)  then
    SignalConnectLost;

  node := Gateway.ConnectedNodes[DatabaseAlias];
  if node.RequestValue = nil then
    SignalConnectLost;

  node.RequestValue.isNull := True;
  arr := node.RequestValue.NewFunction(cProcName).NewArray('params');

  if Assigned(Params) then
 		ConvertToXMLDataType(EncodeStringsAsBase64, Params.AsArray, arr, OnEncodeString);

  node.RequestAvailable.SetEvent;
  if node.ResponseAvailable.WaitFor(FRequestTimeout) <> wrSignaled then begin
    SignalConnectLost;
  end;
  node.ResponseAvailable.ResetEvent;

  if node.ResponseValue.isType = rtc_Exception then
    raise Exception.Create(node.ResponseValue.asException)
  else
    ConvertFromXMLDataType(EncodeStringsAsBase64, node.ResponseValue, Result, FOnDecodeString)
end;

procedure TCcRtcServerConnection.SignalConnectLost;
begin
  Gateway.NodeDisconnect(DatabaseAlias);
  inherited;
end;


constructor TCcRtcServerGateway.Create(AOwner: TComponent);
begin
  inherited;
  FDeleting := False;
  ConnectedNodesSection := TCriticalSection.Create;
  FConnectedNodes := TDictionary<String, TCcNodeConnectionInfo>.Create;
  NodesByConnection := TDictionary<TRtcConnection, TCcNodeConnectionInfo>.Create;

  FKeepAliveFunction := TRtcFunction.Create(Self);
  FKeepAliveFunction.OnExecute := KeepAliveExecute;
  FNodeCleanupTimer := TTimer.Create(Self);
  FNodeCleanupTimer.OnTimer := OnNodeCleanupTimer;
end;

destructor TCcRtcServerGateway.Destroy;
begin
  FDeleting := True;
  FKeepAliveFunction.Free;
  ConnectedNodesSection.Free;
  ConnectedNodes.Free;
  NodesByConnection.Free;
  FNodeCleanupTimer.Free;
  inherited;
end;

procedure TCcRtcServerGateway.OnNodeCleanupTimer(Sender: TObject);
var
  node : TCcNodeConnectionInfo;
begin
  for node in ConnectedNodes.Values do begin
    if (GetTickTime - node.LastCallTimestamp) >= FNodeSessionTimeout then
      NodeDisconnect(node.nodeName);
  end;
end;

procedure TCcRtcServerGateway.KeepAliveExecute(Sender: TRtcConnection;
  Param: TRtcFunctionInfo; Result: TRtcValue);
var
  cNode: string;
  node: TCcNodeConnectionInfo;
begin
  cNode := Param.Value['node'];
  node := NodeConnect(cNode, Sender);
  node.RequestValue := Result;
  if not Param.isNull['response'] then begin
    node.ResponseValue.Clear;
    node.ResponseValue.asObject := Param.asObject['response'].copyOf;
    node.ResponseAvailable.SetEvent;
  end;
  Sender.OnDisconnect := node.OnConnectionLost;
  if node.RequestAvailable.WaitFor(FKeepAliveFrequency) <> wrSignaled then
    Result.Value := 'NOOP';

  if FDeleting or not NodeConnected(cNode) or (Sender = nil) then
    Abort;
  node.RequestAvailable.ResetEvent;
  node.RequestValue := nil;

  node.LastCallTimestamp := GetTickTime;
end;

procedure TCcNodeConnectionInfo.OnConnectionLost(Sender:TRtcConnection);
begin
  FGateway.NodeDisconnect(nodeName);
end;

function TCcRtcServerGateway.NodeConnect(nodeName: String;
  cnx: TRtcConnection): TCcNodeConnectionInfo;
var
  node: TCcNodeConnectionInfo;
begin
  ConnectedNodesSection.Acquire;
  try
    if not ConnectedNodes.ContainsKey(nodeName) then begin
      node := TCcNodeConnectionInfo.Create(Self);
      node.LastCallTimestamp := GetTickTime;
      node.NodeName := nodeName;
      ConnectedNodes.Add(nodeName, node);
     // NodesByConnection.Add(cnx, node);
      if Assigned(FOnNodeConnect) then
        FOnNodeConnect(Self, nodeName);
    end;
    Result := ConnectedNodes[nodeName];
  finally
    ConnectedNodesSection.Release;
  end;
end;

function TCcRtcServerGateway.NodeConnected(nodeName: String): Boolean;
begin
  ConnectedNodesSection.Acquire;
  try
    Result := ConnectedNodes.ContainsKey(nodeName);
  finally
    ConnectedNodesSection.Release;
  end;
end;

procedure TCcRtcServerGateway.NodeDisconnect(nodeName: String);
var
  cnx: TRtcConnection;
  node: TCcNodeConnectionInfo;
begin
  ConnectedNodesSection.Acquire;
  try
    if ConnectedNodes.ContainsKey(nodeName) then begin
      node := ConnectedNodes[nodeName];
      ConnectedNodes.Remove(nodeName);

     // for n in NodesByConnection.values do begin
       // if n = node then
         // NodesByConnection.
     //     Remove(node.RtcConnection);
      //end;

      node.RequestAvailable.SetEvent;
      node.Free;

      if Assigned(FOnNodeDisconnect) then
        FOnNodeDisconnect(Self, nodeName);
    end;
  finally
    ConnectedNodesSection.Release;
  end;
end;

procedure TCcRtcServerGateway.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = ServerModule then
      ServerModule := nil;
  end;
  inherited;
end;

procedure TCcRtcServerGateway.StartServer;
begin
  if not Assigned(FServerModule) then
    raise Exception.Create('RTC ServerModule must be assigned before starting the server');
  if not Assigned(FServerModule.Server) then
    raise Exception.Create('RTC Server must be assigned before starting the server');

  FKeepAliveFunction.Group := FServerModule.FunctionGroup;
  FKeepAliveFunction.FunctionName := 'keepalive';
  FServerModule.Server.Listen();

  FNodeCleanupTimer.Interval := FNodeSessionTimeout;
  FNodeCleanupTimer.Enabled := True;
end;

procedure TCcRtcServerGateway.StopServer;
begin
  if not Assigned(FServerModule) then
    raise Exception.Create('RTC ServerModule must be assigned before stopping the server');
  if not Assigned(FServerModule.Server) then
    raise Exception.Create('RTC Server must be assigned before stopping the server');
  FServerModule.Server.StopListenNow;
  FNodeCleanupTimer.Enabled := False;
end;
{$ENDIF}

procedure TCcRtcClientTransport.OnConnectFail(Sender: TRtcConnection);
begin
	if (not FConnectionFail) then begin
		FConnectionFail := True;
		if Assigned(FOldOnConnFail) then
			FOldOnConnFail(Sender);
	end;
end;

procedure TCcRtcServerTransport.DoStartServer;
begin
  if not Assigned(FServer) then
    raise Exception.Create('RTC Server must be assigned before starting the server');
  if not Assigned(FFunctionGroup) then
    raise Exception.Create('RTC function group must be assigned before starting the server');
  if Trim(DatabaseAlias) = '' then
    raise Exception.Create('A database alias must be assigned before starting the server');

  if FServer.isListening then
    FServer.StopListen;

  FRpcFunction.Group := FFunctionGroup;
  FRpcFunction.FunctionName := ProcedureName;
  FLoginFunction.Group := FFunctionGroup;
  FLoginFunction.FunctionName := LoginProcName;

  FServer.Listen;
end;

procedure TCcRtcServerTransport.DoStopServer;
begin
  Server.StopListen;
end;

{ TCcRtcClientTransport }

constructor TCcRtcClientTransport.Create(AOwner: TComponent);
begin
  inherited;
	FConnectionLost := False;
  FFunctionResult := TRtcResult.Create(Self);
  FFunctionResult.OnReturn := FunctionReturn;
	FFunctionResultValue := TRtcValue.Create;
end;

{ TCcNodeConnectionInfo }
{$IFDEF CC_D2K9}
constructor TCcNodeConnectionInfo.Create(AGateway: TCcRtcServerGateway);
begin
  FGateway := AGateway;
  RequestAvailable := TEvent.Create();
  ResponseAvailable := TEvent.Create();
  ResponseValue := TRtcValue.Create;
end;

destructor TCcNodeConnectionInfo.Destroy;
begin
  RequestAvailable.Free;
  ResponseAvailable.Free;
  ResponseValue.Free;
  inherited;
end;

{ TCcRtcClientGateway }

procedure TCcRtcClientGateway.Connect;
begin
  FRpcFunction.Group := ClientModule.FunctionGroup;
  FRpcFunction.FunctionName := ProcedureName;
  FLoginFunction.Group := ClientModule.FunctionGroup;
  FLoginFunction.FunctionName := LoginProcName;

  ClientModule.Client.Connect();
  ClientModule.Data.NewFunction('keepalive').Value['node'] := DatabaseAlias;
  ClientModule.Call(FRtcResult);
end;

procedure TCcRtcClientGateway.RtcResultReturn(Sender: TRtcConnection; Data,
  Result: TRtcValue);
var
  lResponse: Boolean;
begin
  lResponse := True;
  if (Result.isType = rtc_Text) and (Result.Value = 'NOOP') then
    lResponse := False;

  with ClientModule.Data.NewFunction('keepalive') do begin
    Value['node'] := DatabaseAlias;
    if lResponse then
      asObject['response'] := Result.copyOf;
  end;
  ClientModule.Call(FRtcResult);
end;

constructor TCcRtcClientGateway.Create(AOwner: TComponent);
begin
  inherited;
  FRtcResult := TRtcResult.Create(Self);
  FRtcResult.OnReturn := RtcResultReturn;
end;

destructor TCcRtcClientGateway.Destroy;
begin
  FRtcResult.Free;
  inherited;
end;

procedure TCcRtcClientGateway.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FClientModule then
      FClientModule := nil;
  end;
  inherited;
end;
{$ENDIF}
{ TCcRtcServerTransport }

procedure TCcRtcServerTransport.Connect;
begin

end;

procedure TCcRtcServerTransport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FFunctionGroup then
      FFunctionGroup := nil
    else if AComponent = FServer then
      FServer := nil;
  end;
  inherited;
end;

initialization
	RegisterDBConnector(TCcRtcClientTransport, TCcRtcClientTransport.ConnectorName);
	{$IFDEF CC_D2K9}
	RegisterDBConnector(TCcRtcServerConnection, TCcRtcServerConnection.ConnectorName);
	{$ENDIF}

end.
