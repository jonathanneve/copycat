unit dtConnectorUIB;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders, CcProvUIB,
  CcInterbaseConn;

type
  TdmtConnectorUIB = class(TdmtConnector)
    CcConnectionUIB: TCcConnectionUIB;
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

var
  dmtConnectorUIB: TdmtConnectorUIB;

implementation

uses DB;

{$R *.dfm}

function TdmtConnectorUIB.GetConnection: TCcConnection;
begin
  Result := CcConnectionUIB;
end;

function TdmtConnectorUIB.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionUIB.DBVersion
    + ' - dialect ' + IntToStr(CcConnectionUIB.SQLDialect);
end;

procedure TdmtConnectorUIB.Init(versionName: String; dialect: Integer);
begin
  CcConnectionUIB.DBType := 'Interbase';
  CcConnectionUIB.DBVersion := versionName;
  CcConnectionUIB.SQLDialect := dialect;
  CcConnectionUIB.UserLogin := 'SYSDBA';
  CcConnectionUIB.UserPassword := 'masterkey';
  CcConnectionUIB.DBName := 'c:\temp\' + GetRandomDBName + '.fdb';
  CcConnectionUIB.CreateDatabase;
  CcConnectionUIB.Connect;
end;

destructor TdmtConnectorUIB.Destroy;
begin
  if Assigned(CcConnectionUIB) then
    if CcConnectionUIB.Connected then
      CcConnectionUIB.Disconnect;
  DeleteFile(CcConnectionUIB.DBName);
  inherited;
end;

class function TdmtConnectorUIB.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorUIB;
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

procedure TdmtConnectorUIB.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
//  list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

end.
