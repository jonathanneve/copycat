unit dtConnectorIBX;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders,
  CcInterbaseConn, CcProvIBX;

type
  TdmtConnectorIBX = class(TdmtConnector)
    CcConnectionIBX: TCcConnectionIBX;
  protected
    function GetDescription: String; override;
    function GetConnection: TCcConnection; override;
    procedure Init(versionName: String; dialect: Integer);
  public
    procedure GetSupportedFieldTypes(list: TStringList); override;
    class function CreateTestsByVersion: TCctConnectorList; override;
    destructor Destroy; override;
  end;

implementation

uses DB;

{$R *.dfm}

class function TdmtConnectorIBX.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorIBX;
begin
  Result := TCctConnectorList.Create;

  conn := Self.Create(Application);
  conn.Init('IB6.0', 1);
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('IB6.0', 3);
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('FB2.5', 1);
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('FB2.5', 3);
  Result.Add(conn);
end;

destructor TdmtConnectorIBX.Destroy;
begin
  if Assigned(CcConnectionIBX) and CcConnectionIBX.Connected then
    CcConnectionIBX.Disconnect;
  DeleteFile(CcConnectionIBX.DBName);
end;

function TdmtConnectorIBX.GetConnection: TCcConnection;
begin
  Result := CcConnectionIBX;
end;

function TdmtConnectorIBX.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionIBX.DBVersion
    + ' - dialect ' + IntToStr(CcConnectionIBX.SQLDialect);
end;

procedure TdmtConnectorIBX.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
  list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

procedure TdmtConnectorIBX.Init(versionName: String; dialect: Integer);
begin
  CcConnectionIBX.DBVersion := versionName;
  CcConnectionIBX.SQLDialect := dialect;
  CcConnectionIBX.DBName := 'c:\temp\' + GetRandomDBName + '.fdb';
  CcConnectionIBX.CreateDatabase;
  if not CcConnectionIBX.Connected then
    CcConnectionIBX.Connect;
end;

end.
