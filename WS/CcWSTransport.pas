unit CcWSTransport;

{$I ..\CC.INC}

interface

uses
  Classes, ExtCtrls, SysUtils, DB, CcDB, CcTransports, CcProviders, SyncObjs, Generics.Collections, IdHTTPWebsocketClient,
  IdServerWebsocketContext, IdWebsocketServer, IdSocketIOHandling,
  IdIOHandlerWebsocket, uLkJSON, CcDIMime;

type

TCcEncodingEvent = procedure (var s: String) of object;

TCcWSClientTransport = class (TCcClientTransport)
  private
		FModuleName: String;
		FEncodeStringsAsBase64: Boolean;
		FOnDecodeString: TCcEncodingEvent;
    FOnEncodeString: TCcEncodingEvent;
    FRequestTimeout: Integer;
    FFunctionReturned: Boolean;
		FConnectionLost: Boolean;
		FConnectionFail: Boolean;
		FConnectionError: Exception;
	  FWSClient: TIdHTTPWebsocketClient;
    FReceivedData: string;
    FResponseReceived: TEvent;
    FPort: Integer;
    FHost: String;
    procedure TextReceived(const aData: string);
    procedure SetPort(const Value: Integer);
    procedure SetHost(const Value: String);
  protected
    class function ConnectorName: String; override;
		procedure DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);override;
    procedure DoCleanup;override;
  public
		constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  published
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
		property RequestTimeout: Integer read FRequestTimeout write FRequestTimeout;
		property OnDecodeString: TCcEncodingEvent read FOnDecodeString write FOnDecodeString;
		property OnEncodeString: TCcEncodingEvent read FOnEncodeString write FOnEncodeString;
end;

 { TCcWSServerGateway = class;

  TCcNodeConnectionInfo = class
    private
      FGateway: TCcWSServerGateway;
      NodeName: String;
      RequestValue: TWSValue;
      ResponseValue: TWSValue;
      RequestAvailable : TEvent;
      ResponseAvailable: TEvent;
      LastCallTimestamp: Cardinal;
    procedure OnConnectionLost(Sender: TWSConnection);
    public
      constructor Create(AGateway: TCcWSServerGateway);
      destructor Destroy; override;
  end;

  TCcNodeConnectionEvent = procedure(Sender: TCcWSServerGateway; nodeName: String) of object;
   }

  TCcWSGateway = class (TComponent)
  private
    ConnectedNodesSection: TCriticalSection;
//    NodesByConnection: TDictionary<TWSConnection, TCcNodeConnectionInfo>;
    FConnectedNodes: TDictionary<String, TIdServerWSContext>;
//    FOnNodeConnect: TCcNodeConnectionEvent;
//    FOnNodeDisconnect: TCcNodeConnectionEvent;
    FNodeSessionTimeout: Integer;
    FNodeCleanupTimer: TTimer;
    FDeleting :Boolean;
    FPort: Integer;
    Fserver : TIdWebsocketServer;
    FRequests: TDictionary<TGuid, TIdServerWSContext>;
    procedure SetPort(const Value: Integer);
    procedure ServerMessageTextReceived(const AContext: TIdServerWSContext;
      const aText: string);
    function NewRequest(const AContext: TIdServerWSContext): TGuid;
  public
    procedure StartServer;
    procedure StopServer;
		constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
{    function NodeConnect(nodeName: String; cnx: TWSConnection): TCcNodeConnectionInfo;
    function NodeConnected(nodeName: String): Boolean;
    procedure NodeDisconnect(nodeName: String);
    property ConnectedNodes: TDictionary<String, TCcNodeConnectionInfo> read FConnectedNodes;}
  published
    property Port: Integer read FPort write SetPort;
{    property NodeSessionTimeout: Integer read FNodeSessionTimeout write FNodeSessionTimeout;
    property OnNodeConnect: TCcNodeConnectionEvent read FOnNodeConnect write FOnNodeConnect;
    property OnNodeDisconnect: TCcNodeConnectionEvent read FOnNodeDisconnect write FOnNodeDisconnect;}
end;
  {
TCcWSServerConnection = class (TCcWSAbstractClientTransport)
  private
    FGateway: TCcWSServerGateway;
    procedure SignalConnectLost;
  protected
    class function ConnectorName: String; override;
		procedure DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);override;
  published
    property Gateway : TCcWSServerGateway read FGateway write FGateway;
end;
}

TCcWSAbstractServerTransport = class (TCcServerTransport)
  protected
    procedure ProcessRequest(IOHandler: TIdIOHandlerWebsocket; const aText: String);
  public
		constructor Create(AOwner: TComponent);override;
		destructor Destroy;override;
end;

TCcWSServerTransport = class (TCcWSAbstractServerTransport)
	private
    FPort: Integer;
    procedure SetPort(const Value: Integer);
    procedure ServerMessageTextReceived(const AContext: TIdServerWSContext;
      const aText: string);
  protected
    Fserver : TIdWebsocketServer;
    procedure DoStartServer;override;
    procedure DoStopServer; override;
  public
		constructor Create(AOwner: TComponent);override;
		destructor Destroy;override;
	published
    property Port: Integer read FPort write SetPort;
end;

TCcWSAgentConnection = class (TCcWSAbstractServerTransport)
  private
    FPort: Integer;
    FRequestTimeout: Integer;
    FHost: String;
	  FWSClient: TIdHTTPWebsocketClient;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
    procedure TextReceived(const aData: string);
  protected
  public
    procedure Connect;
		constructor Create(AOwner: TComponent);override;
		destructor Destroy;override;
	published
    property Host: String read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
		property RequestTimeout: Integer read FRequestTimeout write FRequestTimeout;
end;

{
TCcWSClientGateway = class (TCcWSAbstractServerTransport)
  private
    FClientModule: TWSClientModule;
    FWSResult: TWSResult;
    procedure WSResultReturn(Sender: TWSConnection; Data, Result: TWSValue);
  protected
		procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    procedure Connect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	published
		//The server to use for our remote functions
    property ClientModule: TWSClientModule read FClientModule write FClientModule;
end;
 }
implementation

uses Variants, IdGlobal;

function parseDateTime(str: String): TDateTime;
var
  year: Integer;
  month: Integer;
  day: Integer;
  hour: Integer;
  min: Integer;
  sec: Integer;
  msec: Integer;
begin
  //Format YYYYMMDD HHMMSS.SSS

  year := StrToInt(Copy(str, 1, 4));
  month := StrToInt(Copy(str, 5, 2));
  day := StrToInt(Copy(str, 7, 2));
  hour := StrToInt(Copy(str, 10, 2));
  min := StrToInt(Copy(str, 12, 2));
  sec := StrToInt(Copy(str, 14, 2));
  msec := StrToInt(Copy(str, 17, 3));

  Result := EncodeDate(year, month, day) + EncodeTime(hour, min, sec, msec);
end;

function encodeDateTime(dt: TDateTime): String;
begin
  Result := FormatDateTime('YYYYMMDD HHNNSS.ZZZ', dt);
end;

procedure ConvertFromJSONArray(json: TlkJSONlist; CcRpcValue: TCcValue; OnDecodeString: TCcEncodingEvent);

	function GetStringValue(cVal: String): Variant;
	begin
    if Assigned(OnDecodeString) then
      OnDecodeString(cVal);

    Result := cVal;
	end;

	procedure ProcessArray(arr: TlkJSONlist; CcRoot: TCcValue);
	var
		i : Integer;
		CcVal: TCcValue;
    dataType: String;
    data: String;
	begin
		for I := 0 to arr.Count - 1 do begin
			CcVal := CcRoot.AsArray.Add;
			case arr.Child[i].SelfType of
        jsNull:
          CcVal.Value := Null;
				jsString: begin
					CcVal.Value := GetStringValue(arr.getString(i))
				end;
				jsObject: begin
          dataType := arr.Child[i].Field['Type'].Value;
          data := arr.Child[i].Field['Data'].Value;
          if dataType = 'BLOB' then
            CcVal.SetValueAsType(MimeDecodeString(data), ftBlob)
          else if dataType = 'DT' then
            CcVal.SetValueAsType(parseDateTime(data), ftDateTime);
        end;
        jsNumber, jsBoolean:
					CcVal.Value := arr.Child[i].Value;
				jsList:
					ProcessArray(arr.Child[i] as TlkJSONlist, CcVal)
			else
				raise Exception.Create('Unsupported JSON data-type ' + IntToStr(Integer(arr.Child[i].SelfType)) + '!');
			end;
		end;
	end;

begin
  ProcessArray(json, CcRpcValue);
end;

procedure ConvertToJSONArray(CcRpcArray: TCcArray; json: TlkJSONlist; OnEncodeString: TCcEncodingEvent);

  procedure ProcessArray(arr: TCcArray; rootJson: TlkJSONlist);
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
    list: TlkJSONlist;
    obj: TlkJSONobject;
    s: String;
  begin
    for i := 0 to arr.Count - 1 do begin
      with arr[i] do begin
        if ValueType = vtArray then begin
          list := TlkJSONlist.Create;
          rootJson.Add(list);
          ProcessArray(arr[i].AsArray, list);
        end
        //If the value is null, just ignore it
        else if ValueType = vtField then
          if AsField.IsNull or VarIsNull(Value) or VarIsClear(Value) then begin
            rootJson.Add(TlkJSONnull.Create)
          end
          else begin
            case AsField.DataType of
              ftString, ftGuid, ftFixedChar, ftWideString, {$IFDEF CC_D2K6} ftFixedWideChar, ftWideMemo, {$ENDIF} ftMemo, ftFmtMemo:
              begin
                cVal := Value;

                if Assigned(OnEncodeString) then
                  OnEncodeString(cVal);

                rootJson.Add(TlkJSONstring.Generate(cVal));
              end;
              ftSmallint, ftInteger, ftWord, ftAutoInc, ftCurrency, ftBCD, ftFloat {$IFDEF CC_D6}, ftFMTBcd, ftLargeint {$ENDIF}:
                rootJson.Add(TlkJSONnumber.Generate(Value));
              ftBoolean:
                rootJson.Add(TlkJSONboolean.Generate(Value));
              ftDateTime, ftDate, ftTime, ftTimeStamp: begin
                obj := TlkJSONobject.Create();
                obj.Add('Type', 'DT');
                obj.Add('Data', encodeDateTime(Value));
              end;
              ftBlob, ftGraphic, ftBytes, ftVarBytes, ftArray, ftTypedBinary:
              begin
                {$IFDEF CC_D2K9}
                val := Value;
                if VarIsArray(val) then begin
                  Size := VarArrayHighBound (Val, 1) - VarArrayLowBound(Val, 1) + 1;
                  Data := VarArrayLock(Val);
                  try
                    SetString(s, PChar(Data[0]), Size div SizeOf(Char));
                  finally
                    VarArrayUnlock(Val);
                  end;
                end else begin
                {$ENDIF}
                  s := Value;
                {$IFDEF CC_D2K9}
                end;
                {$ENDIF}

                obj := TlkJSONobject.Create();
                obj.Add('Type', 'BLOB');
                obj.Add('Data', s);
               end
              else
                raise Exception.Create('Unsupported JSON data-type ' + IntToStr(Integer(arr[i].AsField.DataType)) + '!');
            end;
          end;
      end;
    end;
  end;

var
  list: TlkJSONlist;

begin
  ProcessArray(CcRpcArray, json);
end;

procedure WriteJSON(IOHandler: TIdIOHandlerWebsocket; json: TlkJSONobject);
var
  strm : TStringStream;
begin
  IOHandler.Write(TlkJSON.GenerateText(json), IndyTextEncoding_UTF8);
end;

procedure SendError(IOHandler: TIdIOHandlerWebsocket; error: String);
var
  strm : TStringStream;
  json: TlkJSONobject;
begin
  json := TlkJSONobject.Create;
  json.Add('MessageType', 'RESPONSE');
  json.Add('ResponseType', 'ERROR');
  json.Add('ErrorMessage', error);
  IOHandler.Write(TlkJSON.GenerateText(json), IndyTextEncoding_UTF8);
end;

class function TCcWSClientTransport.ConnectorName: String;
begin
  Result := 'WS';
end;

procedure TCcWSClientTransport.TextReceived(const aData: string);
begin
  FReceivedData := aData;
  FResponseReceived.SetEvent;
end;

procedure TCcWSClientTransport.DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);
var
	call: TlkJSONobject;
  arr: TlkJSONlist;
  JSONResult: TlkJSONobject;
begin
  if not FWSClient.CheckConnection then begin
    FWSClient.Port := FPort;
    FWSClient.Host := FHost;
    FWSClient.Connect;
    FWSClient.UpgradeToWebsocket;
    FWSClient.OnTextData := TextReceived;
  end;

  arr := TlkJSONlist.Create;
  if Assigned(Params) then
		ConvertToJSONArray(Params.AsArray, arr, OnEncodeString);

  call := TlkJSONObject.Create;
  call.Add('MessageType', 'REQUEST');
  call.Add('NodeName', DatabaseAlias);
  call.Add('FunctionName', functionName);
  call.Add('Params', arr);

  FReceivedData := '';
  FResponseReceived.ResetEvent;
  FWSClient.IOHandler.Write(TlkJSON.GenerateText(call), IndyTextEncoding_UTF8);
  FResponseReceived.WaitFor(FRequestTimeout);

	if (FReceivedData = '') then begin
		SignalConnectLost;
	end
  else begin
    JSONResult := TlkJSON.ParseText(FReceivedData) as TlkJSONobject;
    FReceivedData := '';

    if JSONResult.Field['ResponseType'].Value = 'ERROR' then
      raise Exception.Create(JSONResult.Field['ErrorMessage'].value);

    Assert(JSONResult.Field['ResponseType'].Value = 'OK');
    arr := JSONResult.Field['Data'] as TlkJSONlist;
		ConvertFromJSONArray(arr, Result, FOnDecodeString);
	end
end;

procedure TCcWSClientTransport.SetHost(const Value: String);
begin
  if not FWSClient.Connected then
    FHost := Value;
end;

procedure TCcWSClientTransport.SetPort(const Value: Integer);
begin
  if not FWSClient.Connected then
    FPort := Value;
end;

destructor TCcWSClientTransport.Destroy;
begin
  FWSClient.Free;
  FResponseReceived.Free;
  inherited;
end;

procedure TCcWSClientTransport.DoCleanup;
begin
end;


{ TCcWSClientTransport }

constructor TCcWSClientTransport.Create(AOwner: TComponent);
begin
  inherited;
  FRequestTimeout := 0;
	FConnectionLost := False;
  FWSClient := TIdHTTPWebsocketClient.Create(Self);
  FWSClient.SocketIOCompatible := False;
//  FWSClient.NoAsyncRead := True;
  FResponseReceived := TEvent.Create;
end;



constructor TCcWSServerTransport.Create(AOwner: TComponent);
begin
  inherited;
  Fserver := TIdWebsocketServer.Create(Self);
end;

destructor TCcWSServerTransport.Destroy;
begin
  Fserver.Free;
  inherited;
end;

procedure TCcWSServerTransport.DoStartServer;
begin
  if Trim(DatabaseAlias) = '' then
    raise Exception.Create('A database alias must be assigned before starting the server');

  Fserver.DefaultPort := FPort;
  Fserver.OnMessageText := ServerMessageTextReceived;
  Fserver.Active      := True;
end;

constructor TCcWSAbstractServerTransport.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCcWSAbstractServerTransport.Destroy;
begin

  inherited;
end;

procedure TCcWSServerTransport.ServerMessageTextReceived(const AContext: TIdServerWSContext; const aText: string);
begin
  ProcessRequest(AContext.IOHandler, aText);
end;

procedure TCcWSAbstractServerTransport.ProcessRequest(IOHandler: TIdIOHandlerWebsocket; const aText: String);
var
  strm: TStringStream;
	Params, Res: TCcValue;
	Result : TlkJSONobject;
  func: TlkJSONobject;
  jsonParams: TlkJSONlist;
  cFunctionName: String;
  ResultData: TlkJSONlist;
  reqID: String;
begin
  Result := TlkJSONobject.Create;
  try
    Params := TCcValue.Create;
    Res := TCcValue.Create;
    try
      //JSON object should contain function name and a list of parameters
      func := TlkJSON.ParseText(aText) as TlkJSONobject;
      cFunctionName := func.Field['FunctionName'].Value;
      jsonParams := func.Field['Params'] as TlkJSONlist;
      if func.Field['RequestID'] <> nil then
        reqID := func.Field['RequestID'].Value;
      ConvertFromJSONArray(jsonParams, Params, nil);

      //Execute the function
      if cFunctionName = ProcedureName then
        ExecuteFunction(Params, Res)
      else
        Login(Params.AsArray, Res);

      //Create new JSON result object and return the results to the client
      ResultData := TlkJSONlist.Create;
      ConvertToJSONArray(Res.AsArray, ResultData, nil);
      Result.Add('MessageType', 'RESPONSE');
      Result.Add('ResponseType', 'OK');
      Result.Add('Data', ResultData);
    finally
      Params.Free;
      Res.Free;
    end;
  except on E: Exception do
    begin
      Result.Add('MessageType', 'RESPONSE');
      Result.Add('ResponseType', 'ERROR');
      Result.Add('ErrorMessage', E.Message);
    end;
  end;

  if reqID <> '' then
    Result.Add('RequestID', reqID);
  IOHandler.Write(TlkJSON.GenerateText(Result), IndyTextEncoding_UTF8);
end;


procedure TCcWSServerTransport.SetPort(const Value: Integer);
begin
  if not FServer.Active then
    FPort := Value;
end;

procedure TCcWSServerTransport.DoStopServer;
begin
  FServer.Active := False;
end;


constructor TCcWSGateway.Create(AOwner: TComponent);
begin
  inherited;
  Fserver := TIdWebsocketServer.Create;
  FConnectedNodes := TDictionary<String, TIdServerWSContext>.Create;
  FRequests := TDictionary<TGuid, TIdServerWSContext>.Create;
end;

destructor TCcWSGateway.Destroy;
begin
  Fserver.Free;
  FConnectedNodes.Free;
  FRequests.Free;
  inherited;
end;

function TCcWSGateway.NewRequest(const AContext: TIdServerWSContext): TGuid;
begin
  CreateGUID(Result);
  FRequests.Add(Result, AContext);
end;

procedure TCcWSGateway.ServerMessageTextReceived(
  const AContext: TIdServerWSContext; const aText: string);
var
  reqID: TGuid;
  call: TlkJSONobject;
  cMessageType : String;
  cNodeName: String;
begin
  try
    call := TlkJSON.ParseText(aText) as TlkJSONobject;
    cMessageType := call.Field['MessageType'].Value;
    if cMessageType = 'NODE_CONNECT' then begin
      cNodeName := call.Field['NodeName'].Value;
      if FConnectedNodes.ContainsKey(cNodeName) then
        SendError(AContext.IOHandler, 'Node ' + cNodeName + ' already connected to the gateway')
      else
        FConnectedNodes.Add(cNodeName, AContext);
    end
    else if cMessageType = 'REQUEST' then begin
      reqID := NewRequest(AContext);
      call.Add('RequestID', GuidToString(reqID));
      cNodeName := call.Field['NodeName'].Value;
      if FConnectedNodes.ContainsKey(cNodeName) then
        WriteJSON(FConnectedNodes[cNodeName].IOHandler, call)
      else
        SendError(AContext.IOHandler, 'Node ' + cNodeName + ' is not connected to the gateway')
    end
    else if cMessageType = 'RESPONSE' then begin
      reqID := StringToGuid(call.Field['RequestID'].Value);
      WriteJSON(FRequests[reqID].IOHandler, call);
      FRequests.Remove(reqID);
    end;
  except on E: Exception do
    begin
      SendError(AContext.IOHandler, E.Message);
    end;
  end;
end;

procedure TCcWSGateway.SetPort(const Value: Integer);
begin
  if not FServer.Active then
    FPort := Value;
end;

procedure TCcWSGateway.StartServer;
begin
  Fserver.DefaultPort := FPort;
  Fserver.OnMessageText := ServerMessageTextReceived;
  Fserver.Active      := True;
end;


procedure TCcWSGateway.StopServer;
begin
  Fserver.Active := False;
end;

{ TCcWSAgentConnection }

procedure TCcWSAgentConnection.Connect;
var
  json : TlkJSONobject;
begin
  FWSClient.Host := FHost;
  FWSClient.Port := FPort;
  FWSClient.Connect;
  FWSClient.UpgradeToWebsocket;
  FWSClient.OnTextData := TextReceived;

  json := TlkJSONobject.Create;
  json.Add('MessageType', 'NODE_CONNECT');
  json.Add('NodeName', DatabaseAlias);
  WriteJSON(FWSClient.IOHandler, json);
end;

constructor TCcWSAgentConnection.Create(AOwner: TComponent);
begin
  inherited;
  FWSClient := TIdHTTPWebsocketClient.Create;
end;

destructor TCcWSAgentConnection.Destroy;
begin
  FWSClient.Free;
  inherited;
end;

procedure TCcWSAgentConnection.SetHost(const Value: String);
begin
  if not FWSClient.Connected then
    FHost := Value;
end;

procedure TCcWSAgentConnection.SetPort(const Value: Integer);
begin
  if not FWSClient.Connected then
    FPort := Value;
end;

procedure TCcWSAgentConnection.TextReceived(const aData: string);
begin
  ProcessRequest(FWSClient.IOHandler, aData);
end;

end.



