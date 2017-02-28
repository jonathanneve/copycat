
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ XmlRpcClient.pas                                      }
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
  $Header: /cvsroot/delphixml-rpc/dxmlrpc/source/XmlRpcClient.pas,v 1.1.1.1 2003/12/03 22:37:51 iwache Exp $
  ----------------------------------------------------------- -----------------

  $Log: XmlRpcClient.pas,v $
  Revision 1.1.1.1  2003/12/03 22:37:51  iwache
  Initial import of release 2.0.0

  ----------------------------------------------------------------------------
}
unit CcXmlRpcClient;

{$I ..\CC.INC}
{$I CcXmlRpc.inc}

interface

uses
  SysUtils, Classes, {$IFDEF HAS_CONTNRS_UNIT} Contnrs, {$ELSE} System.Generics.Collections,{$ENDIF} CcXmlRpcTypes, CcXmlRpcCommon,
  IdHTTP, CcXmlRpcServer,
  IdSSLOpenSSL,
{$IFDEF INDY9}
{$IFDEF CC_D7}
  IdHashMessageDigest,
  IdHash,
{$ENDIF}
{$ENDIF}
  CcLibXmlParser;

type
  TRpcClientParser = class(TObject)
  private
    FStack: {$IFDEF HAS_CONTNRS_UNIT}TObjectStack{$ELSE}TStack<TObject>{$ENDIF};
    FStructNames: TStringList;
    FRpcResult: TRpcResult;
    FParser: TCcXmlParser;
    FLastTag: string;
    FFixEmptyStrings: Boolean;
    FServerParser :TRpcServerParser;
    FServer: TCcXmlRpcServer;
    procedure PushStructName(const Name: string);
    function PopStructName: string ;
    function GetServerParser: TRpcServerParser;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Data: string);
    procedure StartTag;
    procedure EndTag;
    procedure DataTag;
    property FixEmptyStrings: Boolean read FFixEmptyStrings
        write FFixEmptyStrings;
    property Server: TCcXmlRpcServer read FServer write FServer;
  end;

  TRpcCaller = class(TRpcClientParser)
  private
    FHostName: string;
    FHostPort: Integer;
    FProxyName: string;
    FProxyPort: Integer;
    FProxyUserName: string;
    FProxyPassword: string;
    FSSLEnable: Boolean;
    FSSLRootCertFile: string;
    FSSLCertFile: string;
    FSSLKeyFile: string;
    FEndPoint: string;
    FProxyBasicAuth: Boolean;
    function Post(const RawData: string): string;
  public
    constructor Create;
    property EndPoint: string read FEndPoint write FEndPoint;
    property HostName: string read FHostName write FHostName;
    property HostPort: Integer read FHostPort write FHostPort;
    property ProxyName: string read FProxyName write FProxyName;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyUserName: string read FProxyUserName write FProxyUserName;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
    property ProxyBasicAuth: Boolean read FProxyBasicAuth write FProxyBasicAuth;
    property SSLEnable: Boolean read FSSLEnable write FSSLEnable;
    property SSLRootCertFile: string read FSSLRootCertFile write
      FSSLRootCertFile;
    property SSLCertFile: string read FSSLCertFile write FSSLCertFile;
    property SSLKeyFile: string read FSSLKeyFile write FSSLKeyFile;
{$IFDEF INDY9}
{$IFDEF CC_D7}
    function Execute(RpcFunction: IRpcFunction; Ttl: Integer): TRpcResult; overload;
{$ENDIF}
{$ENDIF}
    function Execute(const XmlRequest: string): TRpcResult; overload;
    function Execute(Value: IRpcFunction): TRpcResult; overload;
    procedure DeleteOldCache(Ttl: Integer);
  end;

const
  ERROR_EMPTY_RESULT = 600;
  ERROR_EMPTY_RESULT_MESSAGE = 'The xml-rpc server returned a empty response';
  ERROR_INVALID_RESPONSE = 601;
  ERROR_INVALID_RESPONSE_MESSAGE =
    'Invalid payload received from xml-rpc server';

implementation

{------------------------------------------------------------------------------}
{ RPC PARSER CONSTRUCTOR                                                       }
{------------------------------------------------------------------------------}

constructor TRpcClientParser.Create;
begin
  inherited Create;
end;

destructor TRpcClientParser.Destroy;
begin
  //CLINTON - 16/9/2003
  FStructNames.Free;
  FStack.Free;
  FParser.Free;
  inherited Destroy;
end;

//CLINTON 16/9/2003
// push/pop StructName used to store prior struct member name
procedure TRpcClientParser.PushStructName(const Name: String);
begin
  FStructNames.Add(Name);
end ;

function TRpcClientParser.PopStructName: string ;
var 
  I: Integer ;
begin
  I := FStructNames.Count - 1;
  Result := fStructNames[I];
  FStructNames.Delete(I);
end ;


function TRpcClientParser.GetServerParser: TRpcServerParser;
begin
  if not Assigned(FServerParser) then
    FServerParser := TRpcServerParser.Create;
  Result := FServerParser;
end;

{------------------------------------------------------------------------------}
{ RETURN THE RESULT OBJECT  tastes great less filling ;)                       }
{------------------------------------------------------------------------------}

procedure TRpcClientParser.Parse(Data: string);
var
  nPos : Integer;
  List: TList;
  RequestName: String;
  ServerParser: TRpcServerParser;
  RpcReturn: TRpcReturn;
begin
  FRpcResult := TRpcResult.Create;

  { empty string fix }
  if (FFixEmptyStrings) then
    Data := FixEmptyString(Data);
  {simple error check}
  nPos := Pos('<?xml', Trim(Data));
  if not (nPos > 0) then
  begin
    FRpcResult.SetError(ERROR_INVALID_RESPONSE, ERROR_INVALID_RESPONSE_MESSAGE + Data);
    Exit;
  end;

  if (nPos > 1) then
    FRpcResult.FExtraInfo := Copy(Trim(Data), 1, nPos-1)
  else
    FRpcResult.FExtraInfo := '';

  {empty response}
  if (Trim(Data) = '') then
  begin
    FRpcResult.SetError(ERROR_EMPTY_RESULT, ERROR_EMPTY_RESULT_MESSAGE);
    Exit;
  end;

 { if Assigned(Server) and (Pos('<methodResponse>', Data) > 0) then begin
    ServerParser := GetServerParser;
    ServerParser.Parse(Data);
    List := ServerParser.GetParameters;
    RequestName := ServerParser.RequestName;
    RpcReturn := TRpcReturn.Create;
    try
      Server.ExecuteFunctionCall(nil, RequestName, List, RpcReturn);
      if RpcReturn.IsError then
        FRpcResult.SetError(RpcReturn.ErrorCode, RpcReturn.ErrorMsg)
      else begin
        FRpcResult.AsArray := RpcReturn;
      end;
    finally
      RpcReturn.Free;
    end;
    Exit;
  end;      }

  if not Assigned(FParser) then
    FParser := TCcXmlParser.Create;
  if not Assigned(FStack) then
    FStack := {$IFDEF HAS_CONTNRS_UNIT}TObjectStack{$ELSE}TStack<TObject>{$ENDIF}.Create;
  //CLINTON - 16/9/2003  
  if not Assigned(FStructNames) then
    FStructNames := TStringList.Create;

  FRpcResult.Clear;
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
{ CACHED WEB CALL Time To Live calculated in minutes                           }
{------------------------------------------------------------------------------}

{$IFDEF INDY9}
  {$IFDEF CC_D7}

function TRpcCaller.Execute(RpcFunction: IRpcFunction; Ttl: Integer):
    TRpcResult;
var
  Strings: TStrings;
  XmlResult: string;
  XmlRequest: string;
  Hash: string;
  HashMessageDigest: TIdHashMessageDigest5;
begin
  XmlRequest := RpcFunction.RequestXML;
  HashMessageDigest := TIdHashMessageDigest5.Create;
  try
    { determine the md5 digest hash of the request }
		Hash := Hash128AsHex(HashMessageDigest.HashValue(XmlRequest));
  finally
    HashMessageDigest.Free;
  end;
  Strings := TStringList.Create;
  try
    { if we have a cached file from a previous request
      that has not expired then load it }
    if FileExists(GetTempDir + Hash + '.csh') then
    begin
      if not FileIsExpired(GetTempDir + Hash + '.csh', Ttl) then
      begin
        Strings.LoadFromFile(GetTempDir + Hash + '.csh');
        Parse(Strings.Text);
      end;
    end
    else
    begin
      { ok we got here so we where expired or did not exist
        make the call and cache the result this time }
      XmlResult := Post(XmlRequest);
      Parse(XmlResult);

      { save XmlResult in to the cache }
      Strings.Text := XmlResult;
      Strings.SaveToFile(GetTempDir + Hash + '.csh');
    end;
  finally
    Strings.Free;
  end;
  RpcFunction.Clear;
end;

{$ENDIF}
{$ENDIF}

{------------------------------------------------------------------------------}
{ NON - CACHED WEB CALL with IFunction parameter                                                     }
{------------------------------------------------------------------------------}

function TRpcCaller.Execute(Value: IRpcFunction): TRpcResult;
begin
  Result := Execute(Value.RequestXML);
  Value.Clear;
end;

{------------------------------------------------------------------------------}
{ NON - CACHED WEB CALL with XML string parameter                                                     }
  {------------------------------------------------------------------------------}

function TRpcCaller.Execute(const XmlRequest: string): TRpcResult;
var
  XmlResponse: string;
begin
  XmlResponse := Post(XmlRequest);
  Parse(XmlResponse);
  Result := FRpcResult;
end;

{------------------------------------------------------------------------------}
{ DELETE ALL TEMPORARY EXPIRED DATA                                            }
{------------------------------------------------------------------------------}

procedure TRpcCaller.DeleteOldCache(Ttl: Integer);
var
  SearchRec: TSearchRec;
begin
  if FindFirst(GetTempDir + '*.csh', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory = 0) then
        if FileIsExpired(GetTempDir + SearchRec.Name, Ttl) then
          DeleteFile(GetTempDir + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

{------------------------------------------------------------------------------}
{ POST THE REQUEST TO THE RPC SERVER                                           }
{------------------------------------------------------------------------------}

function TRpcCaller.Post(const RawData: string): string;
var
  SendStream: TStream;
  ResponseStream: TStream;
  Session: TIdHttp;
  {$IFDEF INDY9}
  IdSSLIOHandlerSocket: TIdSSLIOHandlerSocket;
  {$ELSE}
  IdSSLIOHandlerSocket: TIdSSLIOHandlerSocketOpenSSL;
  {$ENDIF}
begin
  SendStream := nil;
  ResponseStream := nil;
  IdSSLIOHandlerSocket := nil;
  try
    SendStream := TMemoryStream.Create;
    ResponseStream := TMemoryStream.Create;
    StringToStream(RawData, SendStream); { convert to a stream }
    SendStream.Position := 0;
    Session := TIdHttp.Create(nil);
    try
      if (FSSLEnable) then
      begin
        {$IFDEF INDY9}
        IdSSLIOHandlerSocket := TIdSSLIOHandlerSocket.Create(nil);
        {$ELSE}
        IdSSLIOHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
        {$ENDIF}
        IdSSLIOHandlerSocket.SSLOptions.RootCertFile := FSSLRootCertFile;
        IdSSLIOHandlerSocket.SSLOptions.CertFile := FSSLCertFile;
        IdSSLIOHandlerSocket.SSLOptions.KeyFile := FSSLKeyFile;
        Session.IOHandler := IdSSLIOHandlerSocket;
      end;

      { proxy setup }
      if (FProxyName <> '') then
      begin
        {proxy basic auth}
        if (FProxyBasicAuth) then
          Session.ProxyParams.BasicAuthentication := True;

        Session.ProxyParams.ProxyServer := FProxyName;
        Session.ProxyParams.ProxyPort := FProxyPort;
        Session.ProxyParams.ProxyUserName := FProxyUserName;
        Session.ProxyParams.ProxyPassword := FProxyPassword;
      end;

      Session.Request.Accept := '*/*';
      Session.Request.ContentType := 'text/xml';
      Session.Request.Connection := 'Keep-Alive';
      Session.Request.ContentLength := Length(RawData);
      if not FSSLEnable then
        if FHostPort = 80 then
          Session.Post('http://' + FHostName + FEndPoint, SendStream,
            ResponseStream)
        else
          Session.Post('http://' + FHostName + ':' + IntToStr(FHostPort) +
            FEndPoint, SendStream, ResponseStream);

      if FSSLEnable then
        Session.Post('https://' + FHostName + ':' + IntToStr(FHostPort) +
          FEndPoint, SendStream, ResponseStream);

      Result := StreamToString(ResponseStream);
    finally
      Session.Free;
    end;
  finally
    IdSSLIOHandlerSocket.Free;
    ResponseStream.Free;
    SendStream.Free;
  end;
end;

{------------------------------------------------------------------------------}

constructor TRpcCaller.Create;
begin
  inherited Create;
  FHostPort := 80;
  FSSLEnable := False;
  FProxyBasicAuth := False;
end;

{------------------------------------------------------------------------------}

procedure TRpcClientParser.DataTag;
var
  Data: string;
begin
  Data := FParser.CurContent;
  { should never be empty }
  if not (Trim(Data) <> '') then
    Exit;
  { last tag empty ignore }
  if (FLastTag = '') then
    Exit;

  { struct name store for next pass}
  if (FLastTag = 'NAME') then
    if not (Trim(Data) <> '') then
      Exit;

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
  if (FLastTag = 'NAME') then
  begin
    // CLINTON 16/9/2003
    PushStructName(Data);
    Exit;
  end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TRpcStruct) then
    begin
      if (FLastTag = 'STRING') then
        TRpcStruct(FStack.Peek).LoadRawData(dtString, PopStructName, Data);
      if (FLastTag = 'INT') then
        TRpcStruct(FStack.Peek).LoadRawData(dtInteger, PopStructName, Data);
      if (FLastTag = 'I4') then
        TRpcStruct(FStack.Peek).LoadRawData(dtInteger, PopStructName, Data);
      if (FLastTag = 'DOUBLE') then
        TRpcStruct(FStack.Peek).LoadRawData(dtFloat, PopStructName, Data);
      if (FLastTag = 'DATETIME.ISO8601') then
        TRpcStruct(FStack.Peek).LoadRawData(dtDateTime, PopStructName, Data);
      if (FLastTag = 'BASE64') then
        TRpcStruct(FStack.Peek).LoadRawData(dtBase64, PopStructName, Data);
      if (FLastTag = 'BOOLEAN') then
        TRpcStruct(FStack.Peek).LoadRawData(dtBoolean, PopStructName, Data);
    end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TRpcArray) then
    begin
      if (FLastTag = 'STRING') then
        TRpcArray(FStack.Peek).LoadRawData(dtString, Data);
      if (FLastTag = 'INT') then
        TRpcArray(FStack.Peek).LoadRawData(dtInteger, Data);
      if (FLastTag = 'I4') then
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
  if FStack.Count = 0 then
  begin
    if (FLastTag = 'STRING') then
      FRpcResult.AsRawString := Data;
    if (FLastTag = 'INT') then
      FRpcResult.AsInteger := StrToInt(Data);
    if (FLastTag = 'I4') then
      FRpcResult.AsInteger := StrToInt(Data);
    if (FLastTag = 'DOUBLE') then
      FRpcResult.AsFloat := StrToFloat(Data);
    if (FLastTag = 'DATETIME.ISO8601') then
      FRpcResult.AsDateTime := IsoToDateTime(Data);
    if (FLastTag = 'BASE64') then
      FRpcResult.AsBase64Raw := Data;
    if (FLastTag = 'BOOLEAN') then
      FRpcResult.AsBoolean := StrToBool(Data);
  end;

  FLastTag := '';
end;

{------------------------------------------------------------------------------}

procedure TRpcClientParser.EndTag;
var
  RpcStruct: TRpcStruct;
  RpcArray: TRpcArray;
  Tag: string;
begin
  Tag := UpperCase(Trim(string(FParser.CurName)));

  {if we get a struct closure then
   we pop it off the stack do a peek on
   the item before it and add  it}
  if (Tag = 'STRUCT') then
  begin
    {last item is a struct}
    if (TObject(FStack.Peek) is TRpcStruct) then
      if (FStack.Count > 0) then
      begin
        RpcStruct := TRpcStruct(FStack.Pop);
        if (FStack.Count > 0) then
        begin
          if (TObject(FStack.Peek) is TRpcArray) then
            TRpcArray(FStack.Peek).AddItem(RpcStruct);
          if (TObject(FStack.Peek) is TRpcStruct) then
            TRpcStruct(FStack.Peek).AddItem(PopStructName, RpcStruct)
        end
        else
          FRpcResult.AsStruct := RpcStruct;
        Exit;
      end;

    {last item is a array}
    if (TObject(FStack.Peek) is TRpcArray) then
      if (FStack.Count > 0) then
      begin
        RpcArray := TRpcArray(FStack.Pop);
        if (FStack.Count > 0) then
        begin
          if (TObject(FStack.Peek) is TRpcArray) then
            TRpcArray(FStack.Peek).AddItem(RpcArray);
          if (TObject(FStack.Peek) is TRpcStruct) then
            TRpcStruct(FStack.Peek).AddItem(PopStructName, RpcArray);
        end
        else
          FRpcResult.AsArray := RpcArray;
        Exit;
      end;
  end;

  if (Tag = 'ARRAY') then
  begin
    if (TObject(FStack.Peek) is TRpcArray) then
      if (FStack.Count > 0) then
      begin
        RpcArray := TRpcArray(FStack.Pop);
        if (FStack.Count > 0) then
        begin
          if (TObject(FStack.Peek) is TRpcStruct) then
            TRpcStruct(FStack.Peek).AddItem(PopStructName, RpcArray);
          if (TObject(FStack.Peek) is TRpcArray) then
            TRpcArray(FStack.Peek).AddItem(RpcArray);
        end
        else
          FRpcResult.AsArray := RpcArray;
        Exit;
      end;
  end;

  {if we get the params closure then we will pull the array
   and or struct and add it to the final result then clean up}
  if (Tag = 'PARAMS') then
    if (FStack.Count > 0) then
    begin
      if (TObject(FStack.Peek) is TRpcStruct) then
        FRpcResult.AsStruct := TRpcStruct(FStack.Pop);
      if (TObject(FStack.Peek) is TRpcArray) then
        FRpcResult.AsArray := TRpcArray(FStack.Pop);

      //CLINTON 16/9/2003
      {free the stack and the stack of the Struct names}
      FreeAndNil(FStack);
      FreeAndNil(FStructNames);
    end;
end;

{------------------------------------------------------------------------------}

procedure TRpcClientParser.StartTag;
var
  Tag: string;
  RpcStruct: TRpcStruct;
  RpcArray: TRpcArray;
begin
  Tag := UpperCase(Trim(string(FParser.CurName)));

  if (Tag = 'STRUCT') then
  begin
    RpcStruct := TRpcStruct.Create;
    try
      FStack.Push(RpcStruct);
      RpcStruct := nil;
    finally
      RpcStruct.Free;
    end;
  end;

  if (Tag = 'ARRAY') then
  begin
    RpcArray := TRpcArray.Create;
    try
      FStack.Push(RpcArray);
      RpcArray := nil;
    finally
      RpcArray.Free;
    end;
  end;
  FLastTag := Tag;
end;

end.

