unit CcDBISAM;

interface

uses Classes, Sysutils, CCat, DB, CcProviders, CcKeys, dbisamtb;

type

{$I CC.INC}

TCcDBISamAdaptor = class (TCcDBAdaptor)
  private
    function GetDBISAMDatabase: TDBISAMDatabase;

  protected
    FKeys : TCcKeyRing;
		function GetQuoteMetadata: Boolean;override;

		procedure DoListTables(list: TStringList); override;
		procedure DoListTableFields(cTableName: String; list: TStringList);override;
		procedure DoListUpdatableTableFields(cTableName: String; list: TStringList);override;
		procedure DoListPrimaryKeys(cTableName: String; list: TStringList);override;
		procedure DoListTriggers(list: TStringList);override;
		procedure DoListProcedures(list: TStringList);override;
		procedure DoListAllProcedures(list: TStringList);override;

    function MaxDDLNameLength: Integer;override;
    function GetUseRowsAffected: Boolean; override;

    //Called after a new connection has been started
    procedure InitConnection;override;
    function GetCurrentTimeStampSQL:String;override;
  public
    property DBISAMDatabase: TDBISAMDatabase read GetDBISAMDatabase;
    function SupportsGenerators: Boolean;override;
    class function GetAdaptorName: String;override;
    function SQLFormatValue(Data: Variant; FieldType :TFieldType): String;override;
    procedure CheckCustomMetadata;override;

    function ConvertValue(Val: Variant; DataType: TFieldType): Variant;override;
		function DeclareField(FieldName: String; FieldType: TFieldType;
			Length: Integer; NotNull: Boolean; PK: Boolean; AutoInc: Boolean): String;override;
		function DeclarePK(FieldNames: String): String;override;

    function GenDeclared(GenName:String): Boolean;override;
		procedure RemoveTriggers(qTable: TCcQuery);override;
		procedure GenerateTriggers(qTable:TCcQuery; qTableConf: TCcQuery; FailIfNoPK: Boolean);override;

    constructor Create(Conn: TCcConnection);override;
    destructor Destroy;override;
end;

implementation

uses {$IFDEF CC_D6}Variants, {$ENDIF}Math, CcConf, CcProvDBISAM;



{ TCcDBISamAdaptor }

procedure TCcDBISamAdaptor.CheckCustomMetadata;
begin
  inherited;

end;

function TCcDBISamAdaptor.ConvertValue(Val: Variant;
  DataType: TFieldType): Variant;
begin

end;

constructor TCcDBISamAdaptor.Create(Conn: TCcConnection);
begin
  inherited;

end;

function TCcDBISamAdaptor.DeclareField(FieldName: String; FieldType: TFieldType;
  Length: Integer; NotNull, PK, AutoInc: Boolean): String;
begin

end;

function TCcDBISamAdaptor.DeclarePK(FieldNames: String): String;
begin

end;

destructor TCcDBISamAdaptor.Destroy;
begin

  inherited;
end;

procedure TCcDBISamAdaptor.DoListAllProcedures(list: TStringList);
begin
  inherited;

end;

procedure TCcDBISamAdaptor.DoListPrimaryKeys(cTableName: String;
  list: TStringList);
begin
  inherited;

end;

procedure TCcDBISamAdaptor.DoListProcedures(list: TStringList);
begin
  inherited;

end;

procedure TCcDBISamAdaptor.DoListTableFields(cTableName: String;
  list: TStringList);
var
  table : TDBISAMTable;
begin
  table.create(nil);
  table.DatabaseName := DBISAMDatabase.DatabaseName;
  table.TableName := cTableName;
  table.Open;
  table.GetFieldNames(list);
  table.Close;
  table.destroy;
end;

procedure TCcDBISamAdaptor.DoListTables(list: TStringList);
begin
  Session.GetTableNames(DBISAMDatabase.DatabaseName,list);
end;

procedure TCcDBISamAdaptor.DoListTriggers(list: TStringList);
begin
  inherited;

end;

procedure TCcDBISamAdaptor.DoListUpdatableTableFields(cTableName: String;
  list: TStringList);
begin
  DoListTableFields(cTableName,list);
end;

function TCcDBISamAdaptor.GenDeclared(GenName: String): Boolean;
begin

end;

procedure TCcDBISamAdaptor.GenerateTriggers(qTable, qTableConf: TCcQuery;
  FailIfNoPK: Boolean);
begin

end;

class function TCcDBISamAdaptor.GetAdaptorName: String;
begin

end;

function TCcDBISamAdaptor.GetCurrentTimeStampSQL: String;
begin

end;

function TCcDBISamAdaptor.GetDBISAMDatabase: TDBISAMDatabase;
begin
  Result := (FConnection as TCcConnectionDBISAM).dbisamDatabase;
end;

function TCcDBISamAdaptor.GetQuoteMetadata: Boolean;
begin

end;

function TCcDBISamAdaptor.GetUseRowsAffected: Boolean;
begin

end;

procedure TCcDBISamAdaptor.InitConnection;
begin
  inherited;

end;

function TCcDBISamAdaptor.MaxDDLNameLength: Integer;
begin

end;

procedure TCcDBISamAdaptor.RemoveTriggers(qTable: TCcQuery);
begin
  inherited;

end;

function TCcDBISamAdaptor.SQLFormatValue(Data: Variant;
  FieldType: TFieldType): String;
begin

end;

function TCcDBISamAdaptor.SupportsGenerators: Boolean;
begin

end;

//initialization
// 	CcAvailableAdaptors.Add(TCcDBISamAdaptor);

end.
