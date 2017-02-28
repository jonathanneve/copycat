unit CcDataSnapTransport;

interface

uses Classes, CcTransports, CcDB, CcProviders,
  Data.SQLExpr, DB, Data.DBXCommon, EncdDecd;

type

TCcDSClientTransport = class (TCcClientTransport)
  private
    FSQLConnection: TSQLConnection;
    FRemoteCallCommand: TDBXCommand;
    FLoginCommand: TDBXCommand;
    procedure SetSQLConnection(const Value: TSQLConnection);
    procedure ConnectEvent(Sender: TObject; Connecting: Boolean);
  protected
    procedure DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);override;
    procedure DoCleanup;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property SQLConnection: TSQLConnection read FSQLConnection write SetSQLConnection;
end;

function ValToString(const val: TCcValue): String;
function StrToVar(const S: String; fieldType: TFieldType): Variant;
procedure DecodeValue(str: String; Result: TCcValue);
function EncodeValue(val: TCcValue): String;

procedure Register;

implementation

uses
  Sysutils, Variants, System.Generics.Collections;

var
  fs : TFormatSettings;

function CcBoolToStr(b: Boolean):String;
begin
  if b then
    Result := 'Y'
  else
    Result := 'N';
end;

function CcStrToBool(str: String): Boolean;
begin
  if (str = 'Y') or (str = 'True') then
    Result := True
  else if (str = 'N') or (str = 'False') then
    Result := False
  else
    raise Exception.Create('Error converting string "' + str + '" to boolean');
end;

function VariantToBase64(const V: Variant): String;
var
  p: Pointer;
begin
  p := VarArrayLock(v);
  try
    Result := EncodeBase64(p, VarArrayHighBound(v, 1)+1);
  finally
    VarArrayUnlock(v);
  end;
end;

function ValToString(const val: TCcValue): String;
begin
  if val.AsField.Value = Null then
    Result := ''
  else if val.AsField.DataType = ftBlob then
    Result := VariantToBase64(val.AsField.Value)
  else
    Result := VarToStr(val.AsField.Value);
end;

procedure DoParseValue(val: TCcValue; s: String; var currentPos: Integer);

  function GetNextToken: String;
  var
    InQuote, lastCharWasQuote: Boolean;
  begin
    Result := '';
    InQuote := False;
    lastCharWasQuote := False;
    while (currentPos < High(s)) do begin

      //End current quotation if last char was a quote mark
      // not followed by another one (ie, not escaped)
      if InQuote and lastCharWasQuote and (s[currentPos] <> '''') then begin
        InQuote := False;
        lastCharWasQuote := False;
      end;

      if not InQuote and ((s[currentPos] = ';') or (s[currentPos] = '}')) then
        break
      else if (s[currentPos] = '''') then begin
        if not InQuote then begin
          //First quote mark detected : enter quotation
          InQuote := True;
          Inc(currentPos);
          continue;
        end
        else if not lastCharWasQuote then begin
          //Quote mark detected within a quote
          //Continue to next character to see if this quote marks
          //end of quote or escapes a quote mark
          lastCharWasQuote := True;
          Inc(currentPos);
          continue;
        end
        else
          //escaped quote mark detected : reset lastCharWasQuote
          //  and add quote mark to Result
          lastCharWasQuote := False;
      end;
      Result := Result + s[currentPos];
      Inc(currentPos);
    end;
    //Skip last character (";" or "}")
    if currentPos < High(s) then
      Inc(currentPos);
  end;

var
  firstChar: Char;
  f: TCcFieldInfo;
  arr: TCcArray;
begin
  firstChar := s[currentPos];
  Inc(currentPos);
  if firstChar = '{' then begin
    f := val.AsField;
    f.FieldName := GetNextToken;
    f.DataType := TFieldType(StrToInt(GetNextToken));
    f.Size := StrToInt(GetNextToken);
    f.IsNull := CcStrToBool(GetNextToken);
    if f.IsNull then begin
      f.Value := Null;
      GetNextToken; //We have to call GetNextToken anyway, so that the parsing won't get messed up
    end
    else
      f.Value := StrToVar(GetNextToken, f.DataType);
  end
  else if firstChar = '[' then begin
    arr := val.AsArray;
    while s[currentPos] <> ']' do begin
      DoParseValue(arr.Add, s, currentPos);
    end;
    Inc(currentPos);
  end;
end;

procedure DecodeValue(str: String; Result: TCcValue);
var
  nStartPos: Integer;
begin
  nStartPos := Low(str);
  DoParseValue(Result, str, nStartPos);
end;

function EncodeValue(val: TCcValue): String;
var
  I: Integer;
begin
  if val.ValueType = vtField then
    Result := '{' + QuotedStr(val.AsField.FieldName) + ';'
                  + IntToStr(Integer(val.AsField.DataType)) + ';'
                  + IntToStr(val.AsField.Size) + ';'
                  + CcBoolToStr(val.AsField.IsNull) + ';'
                  + QuotedStr(ValToString(val))
                  + '}'
  else begin
    Result := '[';
    for I := 0 to val.AsArray.Count-1 do
      Result := Result + EncodeValue(val.AsArray[I]);
    Result := Result + ']';
  end;
end;

{ TCcDSClient }

procedure TCcDSClientTransport.ConnectEvent(Sender: TObject;
  Connecting: Boolean);
begin
  if not Connecting then begin
    FreeAndNil(FRemoteCallCommand);
    FreeAndNil(FLoginCommand);
  end;
end;

class function TCcDSClientTransport.ConnectorName: String;
begin
  Result := 'DataSnap';
end;

constructor TCcDSClientTransport.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCcDSClientTransport.Destroy;
begin
  FreeAndNil(FLoginCommand);
  FreeAndNil(FRemoteCallCommand);
  inherited;
end;

procedure TCcDSClientTransport.DoCleanup;
begin

end;

procedure TCcDSClientTransport.DoRemoteCall(functionName: String; Params: TCcValue; Result: TCcValue);

  procedure PrepareCalls;
  begin
    if not SQLConnection.Connected then begin
      SQLConnection.Open;
      FreeAndNil(FRemoteCallCommand);
      FreeAndNil(FLoginCommand);
    end;
    if FRemoteCallCommand = nil then begin
      FRemoteCallCommand := SQLConnection.DBXConnection.CreateCommand;
      FRemoteCallCommand.CommandType := TDBXCommandTypes.DSServerMethod;
      FRemoteCallCommand.Text := 'TCcDSServerMethods.RemoteCall';
      FRemoteCallCommand.Prepare;
    end;
    if FLoginCommand = nil then begin
      FLoginCommand := SQLConnection.DBXConnection.CreateCommand;
      FLoginCommand.CommandType := TDBXCommandTypes.DSServerMethod;
      FLoginCommand.Text := 'TCcDSServerMethods.Login';
      FLoginCommand.Prepare;
    end;
  end;

var
  cmd : TDBXCommand;
  DSParams: TCcValue;
begin
  PrepareCalls;
  if Pos('RemoteCall', functionName) > 0 then
    cmd := FRemoteCallCommand
  else
    cmd := FLoginCommand;

  DSParams := TCcValue.Create;
  try
    //We have to add the database alias as the first parameter of the DataSnap call
    //so that on the server side, we can know which TCcDSServerTransport to use
    DSParams.AsArray.Add.Value := DatabaseAlias;
    DSParams.AsArray.AddValue(Params);
    cmd.Parameters[0].Value.SetString(EncodeValue(DSParams));

//    cmd.Parameters[0].Value.SetString(EncodeValue(DSParams));
    cmd.ExecuteUpdate;
    DecodeValue(cmd.Parameters[1].Value.AsString, Result);
  finally
    DSParams.Free;
  end;
end;

procedure TCcDSClientTransport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = SQLConnection then
      SQLConnection := nil;
  end;
  inherited;
end;

procedure TCcDSClientTransport.SetSQLConnection(const Value: TSQLConnection);
begin
  if FSQLConnection <> Value  then begin
    if Assigned(FSQLConnection) then
      FSQLConnection.RemoveConnectNotification(Self);

    FSQLConnection := Value;

    if Assigned(FSQLConnection) then
      FSQLConnection.AddConnectNotification(Self, ConnectEvent);
  end;
end;

function StrToVar(const S: String; fieldType: TFieldType): Variant;
var
  str : String;
begin

  case fieldType of
    ftSmallint, ftInteger, ftShortInt, ftByte, ftWord:
      Result := StrToInt(S);
    ftFloat, ftSingle, ftExtended:
    begin
      str := S;
      if Pos(',', S) > 0 then
        str := StringReplace(S, ',', '.', [rfReplaceAll]);
      Result := StrToFloat(str, fs);
    end;
    ftCurrency, ftBCD:
    begin
      str := S;
      if Pos(',', S) > 0 then
        str := StringReplace(S, ',', '.', [rfReplaceAll]);
      Result := StrToCurr(str, fs);
    end;
    ftDate:
      Result := StrToDate(s);
    ftDateTime, ftTimeStamp:
      Result := StrToDateTime(s);
    ftTime:
      Result := StrToTime(s);
    ftBoolean:
      Result := StrToBool(S);
    ftLargeint:
      Result := StrToInt64(S);
    ftString, ftGuid, ftMemo, ftWideString, ftFixedChar, ftFixedWideChar, ftWideMemo, ftOraClob:
      Result := S;
    ftBlob:
      Result := DecodeBase64(S);
  end;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcDSClientTransport]);
end;

initialization
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';

  RegisterDBConnector(TCcDSClientTransport, TCcDSClientTransport.ConnectorName);

end.

