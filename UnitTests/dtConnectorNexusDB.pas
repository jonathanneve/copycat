unit dtConnectorNexusDB;

interface

uses
  System.SysUtils, System.Classes, dtConnector, CcProviders, nxdb,
  nxsrSqlEngineBase, nxsqlEngine, nxsdServerEngine, nxsrServerEngine,
  nxllComponent, CcProvNexusDB, nxSqlTriggerExtender;

type
  TdmtConnectorNexusDB = class(TdmtConnector)
    nxSession1: TnxSession;
    nxServerEngine1: TnxServerEngine;
    nxSqlEngine1: TnxSqlEngine;
    nxDatabase: TnxDatabase;
    CcConnectionNexusDB: TCcConnectionNexusDB;
    nxSqlTriggerMonitor: TnxSqlTriggerMonitor;
  private
    cDBName : String;
  protected
    function GetConnection: TCcConnection; override;
    function GetDescription: String; override;
    procedure Init;
  public
    class function CreateTestsByVersion: TCctConnectorList; override;
    procedure GetSupportedFieldTypes(list: TStringList);override;
    destructor Destroy; override;
    { Déclarations publiques }
  end;

var
  dmtConnectorNexusDB: TdmtConnectorNexusDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses Forms, nxseAllEngines;

procedure DeleteDirectory(const Name: string);
var
  F: TSearchRec;
begin
  if FindFirst(Name + '\*', faAnyFile, F) = 0 then begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then begin
          if (F.Name <> '.') and (F.Name <> '..') then begin
            DeleteDirectory(Name + '\' + F.Name);
          end;
        end else begin
          DeleteFile(Name + '\' + F.Name);
        end;
      until FindNext(F) <> 0;
      RemoveDir(Name);
    finally
      FindClose(F);
    end;
  end;
end;

function TdmtConnectorNexusDB.GetConnection: TCcConnection;
begin
  Result := CcConnectionNexusDB;
end;

function TdmtConnectorNexusDB.GetDescription: String;
begin
  Result := ClassName + ' / ' + CcConnectionNexusDB.DBType + ' / '
  + CcConnectionNexusDB.DBVersion;
end;

procedure TdmtConnectorNexusDB.Init;
begin
  cDBName := 'c:\temp\' + GetRandomDBName;
  if DirectoryExists(cDBName) then
    DeleteDirectory(cDBName);
  ForceDirectories(cDBName);

  nxSession1.Connect;
  nxSession1.AddAlias('CopyCatUnitTests', cDBName);
  nxSqlTriggerMonitor.Connect;
  nxDatabase.AliasName := 'CopyCatUnitTests';
  CcConnectionNexusDB.Connect;
end;

destructor TdmtConnectorNexusDB.Destroy;
begin
  if Assigned(CcConnectionNexusDB) and CcConnectionNexusDB.Connected then begin
    CcConnectionNexusDB.Disconnect;
  end;
  if DirectoryExists(cDBName) then
    DeleteDirectory(cDBName);
  inherited;
end;

class function TdmtConnectorNexusDB.CreateTestsByVersion: TCctConnectorList;
var
  conn: TdmtConnectorNexusDB;
begin
  Result := TCctConnectorList.Create;

  conn := Self.Create(Application);
  conn.Init;
  Result.Add(conn);
end;

procedure TdmtConnectorNexusDB.GetSupportedFieldTypes(list: TStringList);
begin
  inherited;
//  list.Add('BLOB=' + IntToStr(Integer(ftBlob)));
//  list.Add('TIMESTAMP=' + IntToStr(Integer(ftDateTime)));
end;

end.
