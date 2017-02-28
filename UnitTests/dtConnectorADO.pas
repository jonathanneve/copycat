unit dtConnectorADO;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders, CcConnADO,
  Data.DB, Data.Win.ADODB;

type
  TdmtConnectorADO = class(TdmtConnector)
    CcConnectionADO: TCcConnectionADO;
    ADOConnection: TADOConnection;
    qCreateDB: TADOCommand;
    qDeleteDB: TADOCommand;
  private
    cDBName: string;
    currentDbType, currentVersionName:String;
  protected
    function GetConnection: TCcConnection;override;
    function GetDescription: String;override;
    procedure Init(dbType, versionName: String);
    procedure Reinit;override;
    { Private declarations }
  public
    procedure GetSupportedFieldTypes(list: TStringList); override;
    class function CreateTestsByVersion: TCctConnectorList; override;
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

{$R *.dfm}


function TdmtConnectorADO.GetConnection: TCcConnection;
begin
  Result := CcConnectionADO;
end;

function TdmtConnectorADO.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionADO.DBType + ' / '
  + CcConnectionADO.DBVersion;
end;

procedure TdmtConnectorADO.Init(dbType, versionName: String);
begin
  currentDbType := dbType;
  currentVersionName := versionName;
{  ADOConnection.ConnectionString := 'Provider=SQLNCLI10.1;Integrated Security=SSPI;Persist Security Info=False;User ID="";Initial Catalog=master;Data Source=KANGAROO\SQLEXPRESS;Initial File Name="";Server SPN=KANGAROO\Jonathan';
  ADOConnection.Open;

  cDBName := 'mssqlTestDB';
  qCreateDB.CommandText := 'create database ' + cDBName;
  qCreateDB.Execute;
                     }
  cDBName := 'mssqlTestDB' + FormatDateTime('ddhhnnss', Now) + IntToStr(Random(100));
  CcConnectionADO.DBType := dbType;
  CcConnectionADO.DBVersion := versionName;
  CcConnectionADO.ConnectionString := 'Provider=SQLNCLI10.1;Integrated Security=SSPI;Persist Security Info=False;User ID="";Initial Catalog=master;Data Source=KANGAROO\SQLEXPRESS;Initial File Name="";Server SPN=KANGAROO\Jonathan';
  CcConnectionADO.Connect;
  CcConnectionADO.CreateDatabase(cDBName);
  CcConnectionADO.Disconnect;
  CcConnectionADO.ConnectionString := 'Provider=SQLNCLI10.1;Integrated Security=SSPI;Persist Security Info=False;User ID="";Initial Catalog=' + cDBName + ';Data Source=KANGAROO\SQLEXPRESS;Initial File Name="";Server SPN=KANGAROO\Jonathan';
  CcConnectionADO.Connect;
{  with CcConnectionADO.Query['createdb'] do begin
    SQL.Text := 'create database ' + cDBName;
    Exec;
    SQL.Text := 'use ' + cDBName;
    Exec;
  end;}
end;

procedure TdmtConnectorADO.Reinit;
begin
  Init(currentDbType, currentVersionName);
end;

destructor TdmtConnectorADO.Destroy;
begin
// Can't manage to drop databases automatically
// Execute this instead :
{
EXEC sp_MSforeachdb '
IF DB_ID(''?'') > 4
BEGIN
ALTER DATABASE [?] SET SINGLE_USER WITH ROLLBACK IMMEDIATE
DROP DATABASE [?]
END'
}
//  CcConnectionADO.Connect;
//  CcConnectionADO.DropDatabase(cDBName);
{  with CcConnectionADO.Query['dropdb'] do begin
    SQL.Text := 'use master';
    Exec;
    SQL.Text := 'drop database ' + cDBName;
    Exec;
  end;    }
  if Assigned(CcConnectionADO) and CcConnectionADO.Connected then begin
    CcConnectionADO.Disconnect;
  end;
{  ADOConnection.Close;
  ADOConnection.Open;
  qDeleteDB.CommandText := 'drop database ' + cDBName;
  qDeleteDB.Execute;}
  inherited;
end;

class function TdmtConnectorADO.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorADO;
begin
  Result := TCctConnectorList.Create;

  conn := Self.Create(Application);
  conn.Init('MSSQL', 'MSSQL2000');
  Result.Add(conn);
end;

procedure TdmtConnectorADO.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
  list.Add('varbinary(max)=' + IntToStr(Integer(ftBlob)));
//  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

end.
