unit dtConnectorFIB;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders,
  CcInterbaseConn, CcProvFIBPlus;

type
  TdmtConnectorFIB = class(TdmtConnector)
    CcConnectionFIB: TCcConnectionFIB;
  private
  protected
    function GetConnection: TCcConnection;override;
    function GetDescription: String;override;
    procedure Init(versionName: String; dialect: Integer);
    { Private declarations }
  public
    procedure GetSupportedFieldTypes(list: TStringList);override;
    class function CreateTestsByVersion: TCctConnectorList;override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  dmtConnectorFIB: TdmtConnectorFIB;

implementation

uses DB;

{$R *.dfm}

function TdmtConnectorFIB.GetConnection: TCcConnection;
begin
  Result := CcConnectionFIB;
end;

function TdmtConnectorFIB.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionFIB.DBVersion
    + ' - dialect ' + IntToStr(CcConnectionFIB.SQLDialect);
end;

procedure TdmtConnectorFIB.Init(versionName: String; dialect: Integer);
begin
  CcConnectionFIB.DBVersion := versionName;
  CcConnectionFIB.SQLDialect := dialect;
  CcConnectionFIB.DBName := 'localhost:c:\temp\' + GetRandomDBName + '.fdb';
  CcConnectionFIB.UserLogin := 'SYSDBA';
  CcConnectionFIB.UserPassword := 'masterkey';
  CcConnectionFIB.CreateDatabase;
end;

destructor TdmtConnectorFIB.Destroy;
begin
  if Assigned(CcConnectionFIB) and CcConnectionFIB.Connected then begin
    CcConnectionFIB.Disconnect;
  end;
  DeleteFile(CcConnectionFIB.DBName);
  inherited;
end;

class function TdmtConnectorFIB.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorFIB;
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

procedure TdmtConnectorFIB.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
  list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

end.
