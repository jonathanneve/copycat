unit dtConnector;

interface

uses
  System.SysUtils, System.Classes, Forms, CcProviders;

type

TdmtConnector = class;

TCctConnectorList = class(TList)
  private
    function GetConnector(nIndex: Integer): TdmtConnector;
  public
    property Connector[nIndex: Integer]: TdmtConnector read GetConnector;
end;

TdmtConnector = class(TDataModule)
  protected
    function GetConnection: TCcConnection; virtual;
    function GetDescription: String;virtual;
    class function GetRandomDBName: String;
  public
    procedure Reinit;virtual;
    procedure GetSupportedFieldTypes(list: TStringList);virtual;
    property Description: String read GetDescription;
    property Connection: TCcConnection read GetConnection;
    class function CreateTestsByVersion: TCctConnectorList; virtual;
end;

implementation

uses DB;

class function TdmtConnector.CreateTestsByVersion: TCctConnectorList;
begin
  Result := TCctConnectorList.Create;
  Result.Add(Self.Create(Application));
end;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TCctConnectorList }

function TCctConnectorList.GetConnector(nIndex: Integer): TdmtConnector;
begin
  Result := TdmtConnector(Items[nIndex]);
end;

function TdmtConnector.GetConnection: TCcConnection;
begin
  Result := nil;
end;

function TdmtConnector.GetDescription: String;
begin
  Result := ClassName;
end;

class function TdmtConnector.GetRandomDBName: String;
var
  Uid: TGuid;
begin
  if CreateGuid(Uid) = S_OK then
     Result := GuidToString(Uid);
end;

procedure TdmtConnector.GetSupportedFieldTypes(list: TStringList);
begin
//  list.Add('VARCHAR(100)=' + IntToStr(Integer(ftString)));
//  list.Add('CHAR(100)=' + IntToStr(Integer(ftString)));
//  list.Add('NUMERIC(15, 4)=' + IntToStr(Integer(ftFloat)));
//  list.Add('INTEGER=' + IntToStr(Integer(ftInteger)));
//  list.Add('TIMESTAMP=' + IntToStr(Integer(ftTimeStamp)));
end;

procedure TdmtConnector.Reinit;
begin

end;

end.
