unit CcDataSnapServer;

interface

uses Classes, CcTransports, CcDB, CcProviders,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  Data.SQLExpr, DB, Data.DBXCommon;

type

TCcDSServerTransportLink = class (TComponent)
  private
    procedure OnGetServerClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
    procedure SetServer(const Value: TDSCustomServer);
    function GetServer: TDSCustomServer;
  protected
    FServerClass: TDSServerClass;
  published
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    property Server :TDSCustomServer read GetServer write SetServer;
end;

TCcDSServerTransport = class (TCcServerTransport)
  protected
    procedure SetDatabaseAlias(const Value: String);override;
    procedure Loaded;override;
  public
    destructor Destroy; override;
end;

{$METHODINFO ON}
TCcDSServerMethods = class(TPersistent)
  private
    function FindTransportByAlias(DBAlias: String): TCcDSServerTransport;
  public
    function Login(Params: String): String;
    function RemoteCall(Params: String): String;
end;
{$METHODINFO OFF}

procedure Register;

implementation

uses
  Sysutils, Variants, System.Generics.Collections, CcDataSnapTransport;

var
  CcDSServerTransportList: TDictionary<String, TCcDSServerTransport>;

{ TCcDSServerTransport }

destructor TCcDSServerTransport.Destroy;
begin
  StopServer;
  inherited;
end;

procedure TCcDSServerTransport.Loaded;
begin
  StartServer;
end;

procedure TCcDSServerTransport.SetDatabaseAlias(const Value: String);
begin
  if DatabaseAlias <> '' then
    CcDSServerTransportList.Remove(DatabaseAlias);

  inherited;

  if DatabaseAlias <> '' then
    CcDSServerTransportList.Add(DatabaseAlias, Self);
end;

{ TCcDSServerTransportLink }

constructor TCcDSServerTransportLink.Create(AOwner: TComponent);
begin
  inherited;
  FServerClass := TDSServerClass.Create(Self);
  FServerClass.OnGetClass := OnGetServerClass;
end;

destructor TCcDSServerTransportLink.Destroy;
begin
  FreeAndNil(FServerClass);
  inherited;
end;

function TCcDSServerTransportLink.GetServer: TDSCustomServer;
begin
  Result := FServerClass.Server;
end;

procedure TCcDSServerTransportLink.OnGetServerClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := TCcDSServerMethods;
end;

procedure TCcDSServerTransportLink.SetServer(const Value: TDSCustomServer);
begin
  FServerClass.Server := Value;
end;

{ TCcDSServerMethods }

function TCcDSServerMethods.FindTransportByAlias(DBAlias: String): TCcDSServerTransport;
begin
  Result := CcDSServerTransportList[DBAlias];
  if not Assigned(Result) then
    raise Exception.Create('CopyCat database alias ' + DBAlias + ' not found on server');
end;

function TCcDSServerMethods.RemoteCall(Params: String): String;
var
  CcParams, CcResult: TCcValue;
  DBAlias: String;
begin
  CcResult := TCcValue.Create;
  CcParams := TCcValue.Create;
  try
    DecodeValue(Params, CcParams);
    DBAlias := CcParams.AsArray[0].Value;
    FindTransportByAlias(DBAlias).ExecuteFunction(CcParams.AsArray[1], CcResult);
    Result := EncodeValue(CcResult);
  finally
    CcResult.Free;
    CcParams.Free;
  end;
end;

function TCcDSServerMethods.Login(Params: String): String;
var
  CcParams, CcResult: TCcValue;
  DBAlias: String;
begin
  CcResult := TCcValue.Create;
  CcParams := TCcValue.Create;
  try
    DecodeValue(Params, CcParams);
    DBAlias := CcParams.AsArray[0].Value;
    FindTransportByAlias(DBAlias).Login(CcParams.AsArray[1].AsArray, CcResult);
    Result := EncodeValue(CcResult);
  finally
    CcResult.Free;
    CcParams.Free;
  end;
end;


procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcDSServerTransportLink, TCcDSServerTransport]);
end;

initialization
  CcDSServerTransportList := TDictionary<String, TCcDSServerTransport>.Create;

finalization
  CcDSServerTransportList.Free;

end.
