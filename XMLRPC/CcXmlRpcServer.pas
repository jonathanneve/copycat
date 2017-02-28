
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ XmlRpcServer.pas                                      }
{                                                       }
{ for Delphi 6, 7                                       }
{ Release 2.0.0                                         }
{ Copyright (c) 2001-2003 by Team-DelphiXml-Rpc         }
{ e-mail: team-dxmlrpc@dwp42.org                        }
{ www: http://sourceforge.net/projects/delphixml-rpc/   }
{                                                       }
{ The initial developer of the code is                  }
{   Clifford E. Baeseman, codepunk@codepunk.com         }
{                                                       }
{ This file may be distributed and/or modified under    }
{ the terms of the GNU Lesser General Public License    }
{ (LGPL) version 2.1 as published by the Free Software  }
{ Foundation and appearing in the included file         }
{ license.txt.                                          }
{                                                       }
{*******************************************************}
{
  $Header: /cvsroot/delphixml-rpc/dxmlrpc/source/XmlRpcServer.pas,v 1.1.1.1 2003/12/03 22:37:41 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: XmlRpcServer.pas,v $
  Revision 1.1.1.1  2003/12/03 22:37:41  iwache
  Initial import of release 2.0.0

  ----------------------------------------------------------------------------
}
unit CcXmlRpcServer;

{$I ..\CC.INC}
{$I CcXmlRpc.inc}

interface

uses
  SysUtils, Classes, {$IFDEF HAS_CONTNRS_UNIT} Contnrs, {$ELSE} System.Generics.Collections,{$ENDIF} SyncObjs,
  {$IFNDEF INDY9}
  IdContext,
  {$ENDIF}
  // #iwa-2003-11-09: IdCustomHTTPServer added for D7 support
//{$IFDEF CC_D7}  also need for D5
  IdCustomHTTPServer,
//{$ENDIF}
//{$IFDEF BCB}
//   IdCustomHTTPServer,
//{$ENDIF}
  IdHTTPServer,
  IdTCPServer,
  CcXmlRpcCommon,
  CcLibXmlParser,
  CcXmlRpcTypes;

type
  {$IFDEF INDY9}
  TRpcThread = TIdPeerThread;
  {$ELSE}
  TRpcThread = TIdContext;
  {$ENDIF}

  { method handler procedure type }
  TRPCMethod = procedure(Thread: TRpcThread; const MethodName: string;
      List: TList; Return: TRpcReturn) of object;

  { method pointer object }
  TRpcMethodHandler = class(TObject)
    Name: string;
    Method: TRPCMethod;
    Help: string;
    Signature: string;
  end;

  { server params parser }
  TRpcServerParser = class(TObject)
  private
    FParser: TCcXmlParser;
    FStack: TStack{$IFNDEF HAS_CONTNRS_UNIT}<TObject>{$ENDIF};
    FLastTag: string;
    FName: string;
    FMethodName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property RequestName: string read FMethodName write FMethodName;
    procedure Parse(const Data: string);
    function GetParameters: TObjectList;
    procedure StartTag;
    procedure EndTag;
    procedure DataTag;
  end;

  { the actual server object }
  TCcXmlRpcServer = class(TComponent)
  private
    FServer: TIdHttpServer;
    FPort: Integer;
    // Take TObjectList instead of TList;
    // A free to TObjectList frees also its items.  14.8.2003 / mko
    FMethodList: TObjectList;
    FSParser: TRpcServerParser;
    FActive: Boolean;
    FLock: TCriticalSection;
    FIntrospect: Boolean;
    {introspection extension methods }
    procedure SystemListMethods(Thread: TRpcThread; const MethodName: string;
        List: TList; Return: TRpcReturn);
    procedure SystemMethodHelp(Thread: TRpcThread; const MethodName: string;
        List: TList; Return: TRpcReturn);
    procedure SystemMethodSignature(Thread: TRpcThread;
        const MethodName: string; List: TList; Return: TRpcReturn);
    { end introspection extension methods }
    procedure DataPosted(Thread: TRpcThread; RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo);
    procedure SetActive(const Value: Boolean);
    procedure StartServer;
    procedure StopServer;
    function GetParser: TRpcServerParser;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure RegisterMethodHandler(MethodHandler: TRpcMethodHandler);
    procedure ClearMethodHandlers;
    function ExecuteFunctionCall(Thread: TRpcThread; RequestName: String; List: TList; RpcReturn: TRpcReturn): Boolean;
  published
    property EnableIntrospect: Boolean read FIntrospect write FIntrospect;
    property ListenPort: Integer read FPort write FPort;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

uses IdGlobal;

{------------------------------------------------------------------------------}

constructor TRpcServerParser.Create;
begin
  FParser := TCcXmlParser.Create;
  FStack := TStack{$IFNDEF HAS_CONTNRS_UNIT}<TObject>{$ENDIF}.Create;
end;

{------------------------------------------------------------------------------}

destructor TRpcServerParser.Destroy;
begin
  if Assigned(FStack) then
    FStack.Free;
  if Assigned(FParser) then
    FParser.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}

procedure TRpcServerParser.DataTag;
var
  Data: string;
begin
  Data := FParser.CurContent;

  { empty not allowed }
  if Trim(Data) = '' then
    Exit;

  if (FLastTag = 'METHODNAME') then
    FMethodName := Data;

  {this will handle the default
   string pain in the ass}
  if FLastTag = 'VALUE' then
    FLastTag := 'STRING';

  {ugly null string hack}
  if (FLastTag = 'STRING') then
    if (Data = '[NULL]') then
      Data := '';

  {if the tag was a struct name we will
   just store it for the next pass    }
  if FLastTag = 'NAME' then
  begin
    FName := Data;
    Exit;
  end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TRpcStruct) then
    begin
      if (FLastTag = 'STRING') then
        TRpcStruct(FStack.Peek).LoadRawData(dtString, FName, Data);
      if (FLastTag = 'INT') then
        TRpcStruct(FStack.Peek).LoadRawData(dtInteger, FName, Data);
      if (FLastTag = 'INT4') then
        TRpcStruct(FStack.Peek).LoadRawData(dtDateTime, FName, Data);
      if (FLastTag = 'DOUBLE') then
        TRpcStruct(FStack.Peek).LoadRawData(dtFloat, FName, Data);
      if (FLastTag = 'DATETIME.ISO8601') then
        TRpcStruct(FStack.Peek).LoadRawData(dtDateTime, FName, Data);
      if (FLastTag = 'BASE64') then
        TRpcStruct(FStack.Peek).LoadRawData(dtBase64, FName, Data);
      if (FLastTag = 'BOOLEAN') then
        TRpcStruct(FStack.Peek).LoadRawData(dtBoolean, FName, Data);
    end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TRpcArray) then
    begin
      if (FLastTag = 'STRING') then
        TRpcArray(FStack.Peek).LoadRawData(dtString, Data);
      if (FLastTag = 'INT') then
        TRpcArray(FStack.Peek).LoadRawData(dtInteger, Data);
      if (FLastTag = 'INT4') then
        TRpcArray(FStack.Peek).LoadRawData(dtInteger, Data);
      if (FLastTag = 'DOUBLE') then
        TRpcArray(FStack.Peek).LoadRawData(dtFloat, Data);
      if (FLastTag = 'DATETIME.ISO8601') then
        TRpcArray(FStack.Peek).LoadRawData(dtDateTime, Data);
      if (FLastTag = 'BASE64') then
        TRpcArray(FStack.Peek).LoadRawData(dtBase64, Data);
      if (FLastTag = 'BOOLEAN') then
        TRpcArray(FStack.Peek).LoadRawData(dtBoolean, Data);
    end;

  {here we are just getting a single value}
  if FStack.Count > 0 then
    if (TObject(FStack.Peek) is TRpcParameter) then
    begin
      if (FLastTag = 'STRING') then
        TRpcParameter(FStack.Peek).AsRawString := Data;
      if (FLastTag = 'INT') then
        TRpcParameter(FStack.Peek).AsInteger := StrToInt(Data);
      if (FLastTag = 'INT4') then
        TRpcParameter(FStack.Peek).AsInteger := StrToInt(Data);
      if (FLastTag = 'DOUBLE') then
        TRpcParameter(FStack.Peek).AsFloat := StrToFloat(Data);
      if (FLastTag = 'DATETIME.ISO8601') then
        TRpcParameter(FStack.Peek).AsDateTime := IsoToDateTime(Data);
      if (FLastTag = 'BASE64') then
        TRpcParameter(FStack.Peek).AsBase64Raw := Data;
      if (FLastTag = 'BOOLEAN') then
        TRpcParameter(FStack.Peek).AsBoolean := StrToBool(Data);
    end;
end;

{------------------------------------------------------------------------------}

procedure TRpcServerParser.EndTag;
var
  Tag: string;
  rpcArray: TRpcArray;
  rpcStruct: TRpcStruct;
begin
  FLastTag := '';
  Tag := UpperCase(Trim(string(FParser.CurName)));

  if (Tag = 'ARRAY') then
    if (TObject(FStack.Peek) is TRpcArray) then
    begin
      rpcArray := TRpcArray(FStack.Pop);
      if (TObject(FStack.Peek) is TRpcParameter) then
        TRpcParameter(FStack.Peek).AsArray := rpcArray;
      if (TObject(FStack.Peek) is TRpcArray) then
        TRpcArray(FStack.Peek).AddItem(rpcArray);
      if (TObject(FStack.Peek) is TRpcStruct) then
        TRpcStruct(FStack.Peek).AddItem(FName, rpcArray);
    end;

  if (Tag = 'STRUCT') then
    if (TObject(FStack.Peek) is TRpcStruct) then
    begin
      rpcStruct := TRpcStruct(FStack.Pop);
      if (TObject(FStack.Peek) is TRpcParameter) then
        TRpcParameter(FStack.Peek).AsStruct := rpcStruct;
      if (TObject(FStack.Peek) is TRpcArray) then
        TRpcArray(FStack.Peek).AddItem(rpcStruct);
      if (TObject(FStack.Peek) is TRpcStruct) then
        TRpcStruct(FStack.Peek).AddItem(FName, rpcStruct);
    end;
end;

{------------------------------------------------------------------------------}

function TRpcServerParser.GetParameters: TObjectList;
var
  Index: Integer;
begin
  Result := TObjectList.Create;
  {we need to reverse the order of the items in the list
   to make the order appear correct at the called method}
  Result.Count := FStack.Count;
  for Index := FStack.Count - 1 downto 0 do
    Result[Index] := TRpcParameter(FStack.Pop);
end;

{------------------------------------------------------------------------------}

procedure TRpcServerParser.Parse(const Data: string);
begin
  FParser.LoadFromBuffer(PChar(Data));
  FParser.StartScan;
  FParser.Normalize := False;
  while FParser.Scan do
  begin
    case FParser.CurPartType of
      ptStartTag:
        StartTag;
      ptContent:
        DataTag;
      ptEndTag:
        EndTag;
    end;
  end;
end;

{------------------------------------------------------------------------------}

procedure TRpcServerParser.StartTag;
var
  Tag: string;
  rpcParameter: TRpcParameter;
  rpcStruct: TRpcStruct;
  rpcArray: TRpcArray;
begin
  Tag := UpperCase(Trim(string(FParser.CurName)));
  if (Tag = 'PARAM') then
  begin
    rpcParameter := TRpcParameter.Create;
    FStack.Push(rpcParameter);
  end;
  if (Tag = 'ARRAY') then
  begin
    rpcArray := TRpcArray.Create;
    FStack.Push(rpcArray);
  end;
  if (Tag = 'STRUCT') then
  begin
    rpcStruct := TRpcStruct.Create;
    FStack.Push(rpcStruct);
  end;
  FLastTag := Tag;
end;

{------------------------------------------------------------------------------}

constructor TCcXmlRpcServer.Create(AOwner: TComponent);
begin
  inherited;
  EnableIntrospect := False;
  FPort := 80;
  FServer := TIdHTTPServer.Create(nil);
  FSParser := TRpcServerParser.Create;
  // 14.8.2003 / mko
  // FMethodList := TList.Create; Take TObjectList instead of TList;
  FMethodList := TObjectList.Create;
//  FMethodList.OwnsObjects := True;
  FLock := TCriticalSection.Create;
end;

{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.DataPosted(Thread: TRpcThread; RequestInfo:
  TIdHTTPRequestInfo; ResponseInfo: TIdHTTPResponseInfo);
var
  Index: Integer;
  RpcReturn: TRpcReturn;
  Found: Boolean;
  List: TList;
  RequestName: string;
  Parser: TRpcServerParser;
  cPostData: String;
begin
  Found := False;
  RpcReturn := TRpcReturn.Create;
  try
    try
      cPostData := RequestInfo.UnparsedParams;
      if cPostData = '' then begin
        cPostData := ReadStringFromStream(RequestInfo.PostStream);
      end;

      { PMM alteration }
      FLock.Acquire;
      try
        {
          This section is not thread-safe, as all values going to and from
          the parser are not passed in and out on the stack
          everything else is on the stack, or presumed safe to use.
          If the parser where some kind of factory object, that merely supplied
          processing functionality, the former approach would be fine
          (and very straightforward) however, the results are in the internal
          state of the parser, thus in principle, if threadA and threadB call
          into this, the parser will become very upset.

          There are several possible approaches for fixes :
          [a] Lock the parser object for the duration that it's state needs to
              be protected. This means not only for while the parse results are
              being used, but when it's being cleared etc. or anything else
              - this could get messy real quick, and one has to be damn confident
                everything is threadsafe
              - V hard to do
          [b] forget making the parser threadsafe, and make the access to the
              parser(s) used threadsafe. This means either serialise all
              requests - poor performance or never give the same parser to
              different threads.

          Note that I've done [a] as it *seems* to be OK. This approach is fine,
          as long as parsing in not the major job or processing the request,
          and normally, it shouldn't be. If parsing is intensive, then GetParser
          needs to be implemented to allow per-thread parser allocation.
        }
        Parser := GetParser;
        Parser.Parse(cPostData);
        { PMM alteration }
        List := Parser.GetParameters;
        RequestName := Parser.RequestName;
        { note that the parser lock can be dropped after here, in principle }
      finally
        FLock.Release;
      end;

      Found := ExecuteFunctionCall(Thread, RequestName, List, RpcReturn);

      if not Found then
      begin
        RpcReturn.SetError(999,
          'Requested method was not registered on the server');
        ResponseInfo.ContentType := 'text/xml';
        ResponseInfo.ServerSoftware := 'DELPHI XMLRPC SERVER';
        ResponseInfo.ContentText := RpcReturn.ResponseXML;
        Exit;
      end;

      ResponseInfo.ContentType := 'text/xml';
      ResponseInfo.ServerSoftware := 'DELPHI XMLRPC SERVER';
      ResponseInfo.ContentText := RpcReturn.ResponseXML;
    except
      on E: Exception do
      begin
        RpcReturn.SetError(999, E.Message);
        ResponseInfo.ContentType := 'text/xml';
        ResponseInfo.ServerSoftware := 'DELPHI XMLRPC SERVER';
        ResponseInfo.ContentText := RpcReturn.ResponseXML;
      end;
    end;
  finally
    RpcReturn.Free;
  end;
end;

{------------------------------------------------------------------------------}
function TCcXmlRpcServer.ExecuteFunctionCall(Thread: TRpcThread; RequestName: String; List: TList; RpcReturn: TRpcReturn): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to FMethodList.Count - 1 do
    if (TRpcMethodHandler(FMethodList[Index]).Name = RequestName) then
    begin
      TRpcMethodHandler(FMethodList[Index]).Method(Thread,
          TRpcMethodHandler(FMethodList[Index]).Name, List, RpcReturn);
      List.Free;
      Result := True;
    end;
end;
{------------------------------------------------------------------------------}

destructor TCcXmlRpcServer.Destroy;
begin
  FLock.Free;
  FMethodList.Free;
  FSParser.Free;
  FServer.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}

function TCcXmlRpcServer.GetParser: TRpcServerParser;
begin
  { dummy allocator }
  Result := FSParser;
end;

{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.RegisterMethodHandler(MethodHandler: TRpcMethodHandler);
begin
  FMethodList.Add(MethodHandler);
end;

procedure TCcXmlRpcServer.ClearMethodHandlers;
begin
  FMethodList.Clear;
end;
{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.SetActive(const Value: Boolean);
begin
  if (Factive <> Value) then
  begin
    if not (csDesigning in ComponentState) then begin
      if Value then
        StartServer
      else
        StopServer;
    end;
    FActive := Value;
  end;
end;

{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.StartServer;
var
  MethodHandler: TRpcMethodHandler;
begin
  FServer.DefaultPort := FPort;
  FServer.OnCommandGet := DataPosted;
  FServer.SessionTimeOut := 10000;
  {introspection extension}
  if FIntrospect then
  begin
    {list methods}
    MethodHandler := TRpcMethodHandler.Create;
    MethodHandler.Name := 'system.listMethods';
    MethodHandler.Method := SystemListMethods;
    MethodHandler.Signature := 'array (no params)';
    MethodHandler.Help :=
      'Returns a list  of all methods registered with the server';
    RegisterMethodHandler(MethodHandler);
    {system help}
    MethodHandler := TRpcMethodHandler.Create;
    MethodHandler.Name := 'system.methodHelp';
    MethodHandler.Method := SystemMethodHelp;
    MethodHandler.Signature := 'string (string method_name)';
    MethodHandler.Help := 'Returns any help text supplied for this method';
    RegisterMethodHandler(MethodHandler);
    {system signature}
    MethodHandler := TRpcMethodHandler.Create;
    MethodHandler.Name := 'system.methodSignature';
    MethodHandler.Method := SystemMethodSignature;
    MethodHandler.Signature := 'array of arrays (string method_name)';
    MethodHandler.Help := 'Returns a array of know signatures for this method';
    RegisterMethodHandler(MethodHandler);
  end;
  FServer.Active := True;
end;

{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.StopServer;
begin
  FServer.Active := False;
end;

{------------------------------------------------------------------------------}
{server introspection extensions}

procedure TCcXmlRpcServer.SystemListMethods(Thread: TRpcThread; const MethodName:
    string; List: TList; Return: TRpcReturn);
var
  RpcArray: IRpcArray;
  Index: Integer;
begin
  RpcArray := TRpcArray.Create;
  for Index := 0 to FMethodList.Count - 1 do
    RpcArray.AddItem(TRpcMethodHandler(FMethodList[Index]).Name);
  Return.AddItem(RpcArray);
end;

{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.SystemMethodHelp(Thread: TRpcThread; const MethodName:
    string; List: TList; Return: TRpcReturn);
var
  Data: string;
  Index: Integer;
begin
  Data := Trim(TRpcParameter(List[0]).AsString);
  for Index := 0 to FMethodList.Count - 1 do
    if (TRpcMethodHandler(FMethodList[Index]).Name = Data) then
      Data := TRpcMethodHandler(FMethodList[Index]).Help;
  Return.AddItem(Data);
end;

{------------------------------------------------------------------------------}

procedure TCcXmlRpcServer.SystemMethodSignature(Thread: TRpcThread; const
    MethodName: string; List: TList; Return: TRpcReturn);
var
  S: string;
  RpcArray: IRpcArray;
  Signatures: IRpcArray;
  Index: Integer;
begin
  S := Trim(TRpcParameter(List[0]).AsString);
  RpcArray := TRpcArray.Create;
  Signatures := TRpcArray.Create;
  for Index := 0 to FMethodList.Count - 1 do
    if (TRpcMethodHandler(FMethodList[Index]).Name = S) then
      Signatures.AddItem(TRpcMethodHandler(FMethodList[Index]).Signature);
  RpcArray.AddItem(Signatures);
  Return.AddItem(RpcArray);
end;

end.



