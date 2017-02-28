unit dtConnectorZeos;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders, CcProvZeos;

type
  TdmtConnectorZeos = class(TdmtConnector)
    CcConnectionZeos: TCcConnectionZeos;
  protected
    function GetDescription: String; override;
    function GetConnection: TCcConnection; override;
    procedure Init(dbType, versionName, zeosProtocol, cUser, cPassword, dbName: String; dialect: Integer);
  public
    procedure GetSupportedFieldTypes(list: TStringList); override;
    class function CreateTestsByVersion: TCctConnectorList; override;
    destructor Destroy; override;
  end;

var
  dmtConnectorZeos: TdmtConnectorZeos;

implementation

uses DB;

{$R *.dfm}

{ TdmtConnectorZeos }

class function TdmtConnectorZeos.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorZeos;
  dbName: String;
begin
  Result := TCctConnectorList.Create;

  conn := Self.Create(Application);
  conn.Init('Interbase', 'IB6.0', 'interbase-6', 'SYSDBA', 'masterkey', 'c:\temp\' + GetRandomDBName + '.fdb', 1);
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('Interbase', 'IB6.0', 'interbase-6', 'SYSDBA', 'masterkey', 'c:\temp\' + GetRandomDBName + '.fdb', 3);
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('Interbase', 'FB2.5', 'firebirdd-2.5', 'SYSDBA', 'masterkey', 'c:\temp\' + GetRandomDBName + '.fdb', 1);
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('Interbase', 'FB2.5', 'firebirdd-2.5', 'SYSDBA', 'masterkey', 'c:\temp\' + GetRandomDBName + '.fdb', 3);
  Result.Add(conn);
end;

destructor TdmtConnectorZeos.Destroy;
var
  dbName : String;
begin
  dbName := CcConnectionZeos.Database;
  if Assigned(CcConnectionZeos) and CcConnectionZeos.Connected then begin
    CcConnectionZeos.Disconnect;
    CcConnectionZeos.Database := '';
  end;
  inherited;
  DeleteFile(dbName);
end;

function TdmtConnectorZeos.GetConnection: TCcConnection;
begin
  Result := CcConnectionZeos;
end;

function TdmtConnectorZeos.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionZeos.DBVersion
    + ' - dialect ' + CcConnectionZeos.Properties.Text;
end;

procedure TdmtConnectorZeos.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
  list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

procedure TdmtConnectorZeos.Init(dbType, versionName, zeosProtocol, cUser, cPassword, dbName: String; dialect: Integer);
begin
  CcConnectionZeos.DBType := dbType;
  CcConnectionZeos.DBVersion := versionName;
  CcConnectionZeos.Protocol := zeosProtocol;
  if dialect <> -1 then
  CcConnectionZeos.Properties.Values['SQLDialect'] := IntToStr(dialect);
  CcConnectionZeos.User := cUser;
  CcConnectionZeos.Password := cPassword;
  CcConnectionZeos.database := dbName;//'c:\temp\' + GetRandomDBName + '.fdb';
  if dbType = 'Interbase' then
    CcConnectionZeos.Properties.Add ('CreateNewDatabase=CREATE DATABASE ' +
      QuotedStr (dbName) + ' USER ' + QuotedStr (cUser) + ' PASSWORD ' + QuotedStr (cPassword));
  CcConnectionZeos.Connect;
end;

end.
