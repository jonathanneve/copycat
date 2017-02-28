unit dtConnectorIBO;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders,
  CcInterbaseConn, CcProvIBO;

type
  TdmtConnectorIBO = class(TdmtConnector)
    CcConnectionIBO: TCcConnectionIBO;
  private
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

{ TdmtConnectorIBO }

function TdmtConnectorIBO.GetConnection: TCcConnection;
begin
  Result := CcConnectionIBO;
end;

function TdmTConnectorIBO.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionIBO.DBVersion
    + ' - dialect ' + IntToStr(CcConnectionIBO.SQLDialect);
end;

procedure TdmTConnectorIBO.Init(versionName: String; dialect: Integer);
begin
  CcConnectionIBO.DBVersion := versionName;
  CcConnectionIBO.SQLDialect := dialect;
  CcConnectionIBO.DBName := 'c:\temp\' + GetRandomDBName + '.fdb';
  CcConnectionIBO.IBDatabase.Path := CcConnectionIBO.DBName;
  CcConnectionIBO.IBDatabase.Username := 'SYSDBA';
  CcConnectionIBO.IBDatabase.Password := 'masterkey';
  CcConnectionIBO.IBDatabase.SQLDialect := dialect;
  CcConnectionIBO.IBDatabase.CreateDatabase;
end;

destructor TdmTConnectorIBO.Destroy;
begin
  if Assigned(CcConnectionIBO) and CcConnectionIBO.Connected then
    CcConnectionIBO.Disconnect;
  DeleteFile(CcConnectionIBO.DBName);
  inherited;
end;

class function TdmtConnectorIBO.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorIBO;
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

procedure TdmtConnectorIBO.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
  list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

end.
