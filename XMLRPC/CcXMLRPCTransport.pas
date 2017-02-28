unit CcXMLRPCTransport;

interface

{$I ..\CC.INC} 

//{$R 'CopyCat.DCR'}

uses Classes, DB, CcDB, CcTransports, CcProviders, CcXMLRPCServer, CcXmlRpcClient, CcXmlRpcTypes;

const
  NULL_STRING = '!!!XML-RPC-NULL-STRING!!!';
  NULL_VALUE = '!!!XML-RPC-NULL!!!';

type

TCcXmlRpcClientTransport = class (TCcClientTransport)
  private
    FRpcCaller: TRpcCaller;
    FHostPort: Integer;
    FHostName: String;
    FEndPoint: String;
    FModuleName: String;
    FEncodeStringsBase64: Boolean;
    FUseHTMLEntities: Boolean;
  protected
    procedure DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);override;
    procedure DoCleanup;override;
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
  published
    property HostName: String read FHostName write FHostName;
    property HostPort: Integer read FHostPort write FHostPort;
    property ModuleName: String read FModuleName write FModuleName;
    property EndPoint : String read FEndPoint write FEndPoint;
    property EncodeStringsBase64 : Boolean read FEncodeStringsBase64 write FEncodeStringsBase64;
    property UseHTMLEntities : Boolean read FUseHTMLEntities write FUseHTMLEntities;
end;

TCcXmlRpcServerTransport = class (TCcServerTransport)
  private
    FServer: TCcXmlRpcServer;
    FEncodeStringsBase64: Boolean;
    FUseHTMLEntities: Boolean;
    procedure SetServer(const Value: TCcXmlRpcServer);
    procedure MethodExecute(thread: TRpcThread; const MethodName: string; List:
      TList; Return: TRpcReturn);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure DoStartServer;override;
    procedure DoStopServer;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property Server :TCcXmlRpcServer read FServer write SetServer;
    property EncodeStringsBase64 : Boolean read FEncodeStringsBase64 write FEncodeStringsBase64;
    property UseHTMLEntities : Boolean read FUseHTMLEntities write FUseHTMLEntities;
end;

procedure Register;

implementation

uses
  Sysutils {$IFDEF CC_D6} , Variants {$ENDIF}, CcDIMime, htmlentities;


procedure ConvertFromXMLDataType(XMLRpcValue: TRpcCustomItem; CcRpcValue: TCcValue; EncodeStringsBase64: Boolean = True; UseHTMLEntities :Boolean = False);
var
  i : Integer;
  cVal: String;
  memStr: TMemoryStream;

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

begin
  case XMLRpcValue.DataType of
    dtFloat:
      CcRpcValue.Value := XMLRpcValue.AsFloat;
    dtInteger:
      CcRpcValue.Value := XMLRpcValue.AsInteger;
    dtInt64:
      CcRpcValue.Value := XMLRpcValue.AsInt64;
    dtString:
    begin
      if XMLRpcValue.AsString = NULL_VALUE then
        CcRpcValue.Value := Null
      else begin
        cVal := XMLRpcValue.AsString;

        if (EncodeStringsBase64) then
          cVal := MimeDecodeString(cVal);

        if (UseHTMLEntities) then
          cVal := HTMLToStr(cVal);

        if cVal = NULL_STRING then
          cVal := '';
        CcRpcValue.Value := cVal;
      end;
    end;
    dtBoolean:
      CcRpcValue.Value := XMLRpcValue.AsBoolean;
    dtDateTime:
      CcRpcValue.Value := XMLRpcValue.AsDateTime;
    dtBase64: begin
      memStr := TMemoryStream.Create;
      try
        XMLRpcValue.Base64StrSaveToStream(memStr);
        CcRpcValue.SetValueAsType(GetByteStreamAsVariant(memStr), ftBlob);
      finally
        memStr.Free;
      end;
    end;
    dtArray:
    begin
      for I := 0 to XMLRpcValue.AsArray.Count - 1 do
        ConvertFromXMLDataType(XMLRpcValue.AsArray[i], CcRpcValue.AsArray.Add, EncodeStringsBase64, UseHTMLEntities)
    end;
    else
      raise Exception.Create('Unsupported XML-RPC data-type!' + IntToStr(Integer(XMLRpcValue.DataType)));
  end;
  XMLRpcValue := nil;
end;

function VariantToInt64(v: Variant): Int64;
begin
  result := StrToInt64(Format('%20s',[v]));
end;

procedure ConvertToXMLDataType(CcRpcArray: TCcArray; XMLRpcArray: TRpcCustomArray; EncodeStringsBase64: Boolean = True; UseHTMLEntities :Boolean = False);
var
  i : Integer;
  arr: TRpcArray;
  cVal: String;
  val: Variant;
  memstr: TMemoryStream;
  Data: PByteArray;
  Size: integer;
begin
  for i := 0 to CcRpcArray.Count - 1 do begin
    with CcRpcArray[i] do begin
      if ValueType = vtArray then begin
        arr := TRpcArray.Create;
        ConvertToXMLDataType(CcRpcArray[i].AsArray, arr, EncodeStringsBase64, UseHTMLEntities);
        XmlRpcArray.AddItem(arr);
      end
      //If the value is null, just ignore it
      else if ValueType = vtField then
        if AsField.IsNull or (Value = Null) or (VarIsNull(Value)) or (VarIsEmpty(Value)) then begin
          XMLRpcArray.AddItem(NULL_VALUE);
        end
        else begin
          case AsField.DataType of
            ftString, ftWideString, ftMemo, ftWideMemo, ftFixedChar:
            begin
              cVal := Value;
              //Hack to avoid trouble with empty strings...
              if cVal = '' then
                cVal := NULL_STRING;

              if (UseHTMLEntities) then
                cVal := StrToHTML(cVal);

              if (EncodeStringsBase64) then
                cVal := MimeEncodeString(cVal);

              XMLRpcArray.AddItem(cVal);
            end;
            ftFloat, ftCurrency, ftBCD {$IFDEF CC_D6}, ftFMTBcd {$ENDIF}: begin
              XMLRpcArray.AddItem(Double(Value));
            end;
            ftSmallint, ftInteger: begin
              XMLRpcArray.AddItem(Integer(Value));
            end;
            ftLargeint :
              XMLRpcArray.AddItem(VariantToInt64(Value));
            ftBoolean: begin
              XMLRpcArray.AddItem(Boolean(Value));
            end;
            ftDateTime, ftDate, ftTime {$IFDEF CC_D6}, ftTimeStamp{$ENDIF}: begin
              XMLRpcArray.AddItemDateTime(Value);
            end;
{            ftMemo:
            begin
              cVal := Value;
              if cVal = '' then
                cVal := NULL_STRING;
              XMLRpcArray.AddItemBase64Str(cVal);
            end;}
            ftBlob, ftGraphic, ftBytes, ftArray:
            begin
              Val := Value;
              memstr := TMemoryStream.Create;
              Size := VarArrayHighBound (Val, 1) - VarArrayLowBound(Val, 1) + 1;
              Data := VarArrayLock(Val);
              try
                memstr.Position := 0;
                memstr.WriteBuffer(Data^, Size);
              finally
                VarArrayUnlock(Val);
              end;
              XMLRpcArray.AddItemBase64StrFromStream(memstr);
            end;
            else
              raise Exception.Create('Unsupported XML-RPC data-type: ' + IntToStr(Integer(AsField.DataType)));
          end;
        end;
    end;
  end;



//  if CcRpcValue.ValueType = vtArray then begin
//    arr := TRpcArray.Create;
//    for i := 0 to CcRpcValue.AsArray.Count - 1 do
//      ConvertToXMLDataType(CcRpcValue.AsArray[i], arr as IRpcCustomArray);
//    XmlRpcArray.AddItem(arr);
//  end
//  else
//  case CcRpcValue.AsField.DataType of
//    cftString:
//      XMLRpcArray.AddItem(String(CcRpcValue.Value));
//    cftFloat:
//      XMLRpcArray.AddItem(Double(CcRpcValue.Value));
//    cftSmallint, cftInteger:
//      XMLRpcArray.AddItem(Integer(CcRpcValue.Value));
//    cftBoolean:
//      XMLRpcArray.AddItem(Boolean(CcRpcValue.Value));
//    cftDateTime, cftDate, cftTime:
//      XMLRpcArray.AddItemDateTime(CcRpcValue.Value);
//    cftMemo, cftBlob, cftArray:
//      XMLRpcArray.AddItemBase64Str(CcRpcValue.Value);
//    else
//      raise Exception.Create('Unsupported XML-RPC data-type!');
//  end;
end;

{ TCcXMLRPCClient }

class function TCcXmlRpcClientTransport.ConnectorName: String;
begin
  Result := 'XMLRPC';
end;

constructor TCcXmlRpcClientTransport.Create(AOwner: TComponent);
begin
  inherited;
  FRpcCaller := TRpcCaller.Create;
  HostPort := 8080;
  FEncodeStringsBase64 := True;
end;

procedure TCcXmlRpcClientTransport.DoCleanup;
begin
  FRpcCaller.Free;
end;

procedure TCcXmlRpcClientTransport.DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);
var
  RpcResult: TRpcResult;
  FRpcFunction: TRpcFunction;
  arr: TRpcArray;
  cProcName: String;
begin
  FRpcCaller.HostName := HostName;
  FRpcCaller.HostPort := HostPort;
  FRpcCaller.EndPoint := EndPoint;
  FRpcCaller.FixEmptyStrings := True;

  if Trim(ModuleName) <> '' then
    cProcName := Trim(ModuleName) + '.' + functionName
  else
    cProcName := functionName;

  RpcResult := nil;
  arr := TRpcArray.Create;
  FRpcFunction := TRpcFunction.Create;
  try
    FRpcFunction.ObjectMethod := cProcName;

    if Assigned(Params) then
      ConvertToXMLDataType(Params.AsArray, arr, EncodeStringsBase64, UseHTMLEntities);
    FRpcFunction.AddItem(arr);

    try
      RpcResult := FRpcCaller.Execute(FRpcFunction as IRpcFunction);
    except
      SignalConnectLost;
    end;

    if RpcResult.IsError then
      raise Exception.Create(RpcResult.ErrorMsg)
    else if Assigned(Result) then begin
      ConvertFromXMLDataType(RpcResult, Result, EncodeStringsBase64, UseHTMLEntities)
    end;
  finally
    //arr.Free;
//    FRpcFunction.Free;
    if RpcResult <> nil then
      RpcResult.Free;
  end;
end;

{ TCcXMLRPCServer }

constructor TCcXmlRpcServerTransport.Create(AOwner: TComponent);
begin
  inherited;
  FEncodeStringsBase64 := True;
end;

destructor TCcXmlRpcServerTransport.Destroy;
begin
 // FServer.Free;
  inherited;
end;

procedure TCcXmlRpcServerTransport.DoStartServer;
var
  RpcMethodHandler: TRpcMethodHandler;
begin
  if FServer = nil then
    raise Exception.Create('XMLRPC server not assigned');
             
  try
    RpcMethodHandler := TRpcMethodHandler.Create;
    RpcMethodHandler.Name := ProcedureName;
    RpcMethodHandler.Method := MethodExecute;
    FServer.RegisterMethodHandler(RpcMethodHandler);
    RpcMethodHandler := nil;
  finally
    RpcMethodHandler.Free;
  end;

  try
    RpcMethodHandler := TRpcMethodHandler.Create;
    RpcMethodHandler.Name := LoginProcName;
    RpcMethodHandler.Method := MethodExecute;
    FServer.RegisterMethodHandler(RpcMethodHandler);
    RpcMethodHandler := nil;
  finally
    RpcMethodHandler.Free;
  end;

  FServer.Active := True;
end;

procedure TCcXmlRpcServerTransport.DoStopServer;
begin
  FServer.Active := False;
  FServer.ClearMethodHandlers;
end;

procedure TCcXmlRpcServerTransport.MethodExecute(thread: TRpcThread;
  const MethodName: string; List: TList; Return: TRpcReturn);
var
  i: Integer;
  Params, Result: TCcValue;
  param: TRpcParameter;
  arr: TRpcArray;
begin
  if List.Count <> 1 then
    Exit;
//    raise Exception.Create('Incorrect parameters for remote function call!');

  Params := TCcValue.Create;
  Result := TCcValue.Create;

  try
    //First and only parameter is an array of parameters
    param := TRpcParameter(List[0]);
    ConvertFromXMLDataType(param, Params, EncodeStringsBase64, UseHTMLEntities);
    param := nil;
    List[0] := nil;

    //Execute the function
    if MethodName = ProcedureName then
      ExecuteFunction(Params, Result)
    else
      Login(Params.AsArray, Result);

    //Return the results to the client
    arr := TRpcArray.Create;
    ConvertToXMLDataType(Result.AsArray, arr, EncodeStringsBase64, UseHTMLEntities);
    Return.AddItem(arr);
  finally
    Params.Free;
    Result.Free;
  end;
end;

procedure TCcXmlRpcServerTransport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FServer then
      FServer := nil;
  end;
  inherited;
end;

procedure TCcXmlRpcServerTransport.SetServer(const Value: TCcXmlRpcServer);
begin
  FServer := Value;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcXmlRpcServer, TCcXmlRpcClientTransport, TCcXmlRpcServerTransport]);
end;

initialization
  RegisterDBConnector(TCcXmlRpcClientTransport, TCcXmlRpcClientTransport.ConnectorName);

end.
