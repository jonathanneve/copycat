unit dtconnectorFireDAC;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase, FireDAC.Comp.UI, FireDAC.Comp.Client,
  Data.DB, CcProviders, CcProvFireDAC, dtConnector, FireDAC.Phys.Oracle,
  FireDAC.Phys.PG;

type
  TdmtConnectorFireDAC = class(TdmtConnector)
    FDConnection: TFDConnection;
    FDTransaction1: TFDTransaction;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    CcConnectionFireDAC: TCcConnectionFireDAC;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
  private
    cDBName: String;
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
  end;

var
  dmtConnectorFireDAC: TdmtConnectorFireDAC;

implementation

uses Forms;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TdmtConnectorFireDAC }

class function TdmtConnectorFireDAC.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorFireDAC;
begin
  Result := TCctConnectorList.Create;

  conn := Self.Create(Application);
  conn.Init('Interbase', 'FB2.5');
  Result.Add(conn);

{  conn := Self.Create(Application);
  conn.Init('Oracle', '11.2');
  Result.Add(conn);

  conn := Self.Create(Application);
  conn.Init('Postgres', '9.3');
  Result.Add(conn);
}
end;

destructor TdmtConnectorFireDAC.Destroy;
var
  oldUserName : String;
begin
  CcConnectionFireDAC.Disconnect;

  if CcConnectionFireDAC.dbType = 'Interbase' then begin
  FDConnection.Params.Values['DropDatabase'] := 'Yes';
  CcConnectionFireDAC.Connect;
  CcConnectionFireDAC.Disconnect;
  end;

  if CcConnectionFireDAC.dbType = 'Postgres' then begin
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := 'postgres';
    FDConnection.Params.Values['password'] := 'cetorcim';
    CcConnectionFireDAC.Connect;
    FDConnection.ExecSQL('DROP SCHEMA "' + cDBName + '" CASCADE');
    FDConnection.ExecSQL('DROP OWNED BY "' + cDBName + '"');
    FDConnection.ExecSQL('DROP ROLE "' + cDBName + '"');
    CcConnectionFireDAC.Disconnect;
  end;

  if CcConnectionFireDAC.dbType = 'Oracle' then begin
    oldUserName := FDConnection.Params.Values['User_Name'];
    FDConnection.Params.Values['User_Name'] := 'system';
    FDConnection.Params.Values['password'] := 'cetorcim';
    CcConnectionFireDAC.Connect;
    FDConnection.ExecSQL('DROP USER ' + oldUserName + ' CASCADE');
    CcConnectionFireDAC.Disconnect;
  end;

  inherited;
end;

function TdmtConnectorFireDAC.GetConnection: TCcConnection;
begin
  Result := CcConnectionFireDAC;
end;

function TdmtConnectorFireDAC.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionFireDAC.DBType + ' / '
  + CcConnectionFireDAC.DBVersion;
end;

procedure TdmtConnectorFireDAC.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;


  if (CcConnectionFireDAC.DBType = 'Interbase') then begin
//    list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
//    list.Add('VARCHAR(100)=' + IntToStr(Integer(ftWideString)));
//    list.Add('FLOAT=' + IntToStr(Integer(ftFloat)));
//    list.Add('DOUBLE PRECISION=' + IntToStr(Integer(ftFloat)));
//    list.Add('BLOB SUB_TYPE 1=' + IntToStr(Integer(ftMemo)));
//  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
//    list.Add('DATE=' + IntToStr(Integer(ftDate)));
//    list.Add('TIME=' + IntToStr(Integer(ftTIME)));
//    list.Add('NUMERIC(15,2)=' + IntToStr(Integer(ftCurrency)));
    list.Add('BIGINT=' + IntToStr(Integer(ftLargeInt)));
  end;

  if (CcConnectionFireDAC.DBType = 'Oracle') then begin
//    list.Add('BLOB=' + IntToStr(Integer(ftOraBlob)));
//    list.Add('LONG RAW=' + IntToStr(Integer(ftBlob)));
//    list.Add('NVARCHAR2(100)=' + IntToStr(Integer(ftWideString)));
//    list.Add('BINARY_FLOAT=' + IntToStr(Integer(ftFloat)));
//    list.Add('BINARY_DOUBLE=' + IntToStr(Integer(ftFloat)));
//    list.Add('CLOB=' + IntToStr(Integer(ftMemo)));
//    list.Add('DATE=' + IntToStr(Integer(ftDateTime)));
//    list.Add('DATE=' + IntToStr(Integer(ftDate)));
//    list.Add('NUMERIC(15,2)=' + IntToStr(Integer(ftCurrency)));
  list.Add('NUMBER(38,0)=' + IntToStr(Integer(ftLargeInt)));
  end;

  if (CcConnectionFireDAC.DBType = 'Postgres') then begin
//    list.Add('oid=' + IntToStr(Integer(ftOraBlob)));
//    list.Add('bytea=' + IntToStr(Integer(ftBlob)));
//    list.Add('VARCHAR(100)=' + IntToStr(Integer(ftWideString)));
//    list.Add('real=' + IntToStr(Integer(ftFloat)));
//    list.Add('double precision=' + IntToStr(Integer(ftFloat)));
//    list.Add('text=' + IntToStr(Integer(ftMemo)));
//    list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
//    list.Add('DATE=' + IntToStr(Integer(ftDate)));
//    list.Add('time=' + IntToStr(Integer(ftTime)));
//    list.Add('money=' + IntToStr(Integer(ftCurrency)));
    list.Add('bigint=' + IntToStr(Integer(ftLargeInt)));
  end;
end;

procedure TdmtConnectorFireDAC.Reinit;
begin
  //Drop the database and recreate it


  if CcConnectionFireDAC.dbType = 'Interbase' then begin
  CcConnectionFireDAC.Disconnect;
  FDConnection.Params.Values['DropDatabase'] := 'Yes';
  CcConnectionFireDAC.Connect;
  CcConnectionFireDAC.Disconnect;
  FDConnection.Params.Values['CreateDatabase'] := 'Yes';
  FDConnection.Params.Values['DropDatabase'] := 'No';
  CcConnectionFireDAC.Connect;
  CcConnectionFireDAC.Disconnect;
  FDConnection.Params.Values['CreateDatabase'] := 'No';
  CcConnectionFireDAC.Connect;
  end;

  if CcConnectionFireDAC.dbType = 'Postgres' then begin
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := 'postgres';
    FDConnection.Params.Values['password'] := 'cetorcim';
    CcConnectionFireDAC.Connect;
    FDConnection.ExecSQL('DROP SCHEMA "' + cDBName + '" CASCADE');
    FDConnection.ExecSQL('DROP OWNED BY "' + cDBName + '"');
    FDConnection.ExecSQL('DROP ROLE "' + cDBName + '"');
    FDConnection.ExecSQL('CREATE USER "' + cDBName + '" WITH PASSWORD ''blop''');
    FDConnection.ExecSQL('CREATE SCHEMA AUTHORIZATION "' + cDBName + '"');
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := cDBName;
    FDConnection.Params.Values['password'] := 'blop';
    CcConnectionFireDAC.Connect;
  end;

  if CcConnectionFireDAC.dbType = 'Oracle' then begin
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := 'system';
    FDConnection.Params.Values['password'] := 'cetorcim';
    CcConnectionFireDAC.Connect;
    FDConnection.ExecSQL('DROP USER ' + cDBName + ' CASCADE');
    FDConnection.ExecSQL('CREATE USER ' + cDBName + ' IDENTIFIED BY blop');
    FDConnection.ExecSQL('GRANT connect, resource TO ' + cDBName);
    FDConnection.ExecSQL('GRANT UNLIMITED TABLESPACE TO ' + cDBName);
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := cDBName;
    FDConnection.Params.Values['password'] := 'blop';
    CcConnectionFireDAC.Connect;
  end;

end;

procedure TdmtConnectorFireDAC.Init(dbType, versionName: String);
begin
  cDBName := 'fireDACTestDB' + FormatDateTime('ddhhnnss', Now) + IntToStr(Random(100));
  CcConnectionFireDAC.DBType := dbType;
  CcConnectionFireDAC.DBVersion := versionName;

  if dbType = 'Interbase' then begin
    FDConnection.DriverName := 'FB';
    FDConnection.Params.Values['CreateDatabase'] := 'Yes';
    FDConnection.Params.Values['Database'] := ExtractFileDir(Application.ExeName) + '\' + cDBName  + '.fdb';
    FDConnection.Params.Values['User_Name'] := 'SYSDBA';
    FDConnection.Params.Values['password'] := 'masterkey';
    CcConnectionFireDAC.Connect;
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['CreateDatabase'] := 'No';
    CcConnectionFireDAC.Connect;
  end;

  if dbType = 'Oracle' then begin
    FDConnection.DriverName := 'Ora';
    FDConnection.Params.Values['User_Name'] := 'system';
    FDConnection.Params.Values['password'] := 'cetorcim';
  CcConnectionFireDAC.Connect;
    FDConnection.ExecSQL('CREATE USER ' + cDBName + ' IDENTIFIED BY blop');
    FDConnection.ExecSQL('GRANT connect, resource TO ' + cDBName);
    FDConnection.ExecSQL('GRANT UNLIMITED TABLESPACE TO ' + cDBName);
  CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := cDBName;
    FDConnection.Params.Values['password'] := 'blop';
  CcConnectionFireDAC.Connect;
  end;

  if dbType = 'Postgres' then begin
    FDConnection.DriverName := 'PG';
    FDConnection.Params.Values['User_Name'] := 'postgres';
    FDConnection.Params.Values['password'] := 'cetorcim';
    FDConnection.Params.Values['Database'] := 'postgres';
    CcConnectionFireDAC.Connect;
    FDConnection.ExecSQL('CREATE USER "' + cDBName + '" WITH PASSWORD ''blop''');
    FDConnection.ExecSQL('CREATE SCHEMA AUTHORIZATION "' + cDBName + '"');
    CcConnectionFireDAC.Disconnect;
    FDConnection.Params.Values['User_Name'] := cDBName;
    FDConnection.Params.Values['password'] := 'blop';
    CcConnectionFireDAC.Connect;
  end;

end;

end.
