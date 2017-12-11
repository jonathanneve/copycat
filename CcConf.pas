//CopyCat replication suite<p/>
//Copyright (c) 2014 Microtec Communications<p/>
//For any questions or technical support, contact us at contact@copycat.fr
unit CcConf;

{$I CC.INC}

interface

uses Classes, DB, CcDB, CCat, CcProviders, CcKeys;

type

TConfirmEvent = procedure(Sender:TObject; var CanContinue:Boolean) of object;
TCcScriptEvent = procedure(Sender: TObject; Script: TStrings) of object;

//Summary:
//\Parameters of a stored procedure.
//Description:
//TCcProcParams represents the parameters of a stored procedure. It is only used in
//conjunction with the TCcConfig component, when generating primary key
//synchronization SQL based on a stored procedure.<p/>
//<p/>
//The most common way to use TCcProcParams (for holding input parameters) is as
//follows:
//  1. Create an instance of TCcProcParams.
//  2. Pass it as a parameter to the TCcConfig.GetProcParams method, in order to
//     fill it with the list of parameters of a given stored procedure, along with the
//     data-type and field length of each one.
//  3. Fill in the values of the parameters, using the PARAM_VALUE field.
//  4. Call TCcConfig.GetProcGenerator, passing the TCcProcParams object in order
//     to provide input parameter values.
//
//TCcProcParams is a TDataSet descendant, that defines the following fields:
//<table 25c%, 15c%>
//Field name       Data type        \Description
//===============  ===============  -----------------------------
//PARAM_NAME       <align center>   Name of the stored procedure
//                  String           parameter.
//                  </align>
//<align center>   String           Value of the parameter.
// PARAM_VALUE
// </align>
//FIELD_TYPE       <align center>   Data type.
//                  Integer
//                  </align>
//FIELD_LENGTH     Integer          Field size.
//</table>
TCcProcParams = class(TCcMemoryTable)
  protected
    procedure LoadFields;
    procedure Loaded;override;
  public
		constructor Create(AOwner: TComponent); override;
end;

TCcStringList = class(TStringList)
  private
    function GetSQLCommaText: String;
    procedure SetSQLCommaText(const Value: String);
  public
    property SQLCommaText : String read GetSQLCommaText write SetSQLCommaText;
end;

TCcConfigTable = class(TCollectionItem)
	private
		FTableName: String;
		FFieldsIncluded: TStringList;
		FFieldsExcluded: TStringList;
		FCondition: TStringList;
		FDeleteCondition: TStringList;
		FUpdateCondition: TStringList;
		FInsertCondition: TStringList;
		FPriority: Integer;
    FPKSyncStatements: TCcStringList;
    FSyncStatements: TCcStringList;
    FSyncFieldNames: TCcStringList;
		procedure SetFieldsExcluded(const Value: TStringList);
		procedure SetFieldsIncluded(const Value: TStringList);
		procedure SetCondition(const Value: TStringList);
		procedure SetDeleteCondition(const Value: TStringList);
		procedure SetInsertCondition(const Value: TStringList);
		procedure SetUpdateCondition(const Value: TStringList);
    procedure SetPKSyncStatements(const Value: TCcStringList);
    procedure SetSyncFieldNames(const Value: TCcStringList);
    procedure SetSyncStatements(const Value: TCcStringList);
	protected
		function GetDisplayName: string; override;
	public
		constructor Create(Collection: TCollection); override;
		destructor Destroy; override;
	published
		property Priority: Integer read FPriority write FPriority;
		property TableName: String read FTableName write FTableName;

		//SQL statements for synchronizing primary key values (one row per statement, one statement for each field of the PK)
		property PKSyncStatements: TCcStringList read FPKSyncStatements write SetPKSyncStatements;

 		//Names of fields to be sychronized using the SQL statements in SyncStatements (one row per field)
		property SyncFieldNames: TCcStringList read FSyncFieldNames write SetSyncFieldNames;

 		//SQL statements for synchronizing key values that aren't part of the PK
    //Description: The list of fields to synchronise is in the SyncFieldNames property.
    //Put one row per statement, corresponding to the fields listed in SyncFieldNames
		property SyncStatements: TCcStringList read FSyncStatements write SetSyncStatements;

		//SQL condition to be used in the replication triggers
		property Condition: TStringList read FCondition write SetCondition;

		//SQL condition to be used in the update replication triggers
		property UpdateCondition: TStringList read FUpdateCondition write SetUpdateCondition;

		//SQL condition to be used in the insert replication triggers
		property InsertCondition: TStringList read FInsertCondition write SetInsertCondition;

		//SQL condition to be used in the delete replication triggers
		property DeleteCondition: TStringList read FDeleteCondition write SetDeleteCondition;

		//Fields to be taken into account for replicating this table.
		//Description: If nothing is set, the default behaviour is to always replicate any row that is changed, 
		//without comparing the field values. Therefore, even if none if the fields actually changed at all, 
		//the row will be replicated because the triggers were fired.<p/>
		//If you explicitly set a list of fields to include in replication, then CopyCat will check those fields 
		//in the triggers and will only fire replication for the row if at least one of the included fields was changed.
		//That means that by default, any new field will be ignored unlesss it's added to this list.
		property FieldsIncluded: TStringList read FFieldsIncluded write SetFieldsIncluded;

		//Fields to be ignored in the replication triggers.
		//Description: If you set fields in the FieldsExcluded property, CopyCat will put the list of all fields
		//except the excluded ones into the FieldsIncluded property. Thus, if you add a field in your table 
		//and don't want to exclude it), you need to call TCcConfig.GenerateConfig again in order to refresh the 
		//FieldsIncluded property and update the triggers.
		property FieldsExcluded: TStringList read FFieldsExcluded write SetFieldsExcluded;
end;

TCcConfigTables = class (TCollection)
 private
   FOwner: TPersistent;
 	 function GetItem(Index: Integer): TCcConfigTable;
 protected
   function GetOwner: TPersistent; override;
 public
   constructor Create(AOwner: TPersistent);
	 function FindTable(tableName: String): TCcConfigTable;
	 function Add: TCcConfigTable;
	 property Item[Index: Integer]: TCcConfigTable read GetItem; default;
 end;

//Summary:
//Component for generating replication meta-data.
//Description:
//Before any replication can take place, the appropriate meta-data must be created
//in both databases. TCcConfig allows you to generate this meta-data (based on
//settings specific to each setup), and apply it to the database.<p/>
//<p/>
//Here's the simplest way to configure your databases :<p/>
//   1. Setup the list of tables that you want to replicate using the Tables property.<p/>
//   2. Fill out the list of nodes in the Nodes property. You need to set one node name
//     for every replication node towards which you want to send the selected database's data.<p/> 
//   3. Set ConfigName to a short, descriptive name. This allows you to have several different 
//      configurations, with different lists of tables and nodes, and different conditions and options.
//      If you give each a different configuration name, they will be able to fit together seamlessly
//      in the same database.<p/>
//   4. Call GenerateConfig to apply the above setting to the database. The settings are also stored in 
//      the database, so they can be checked, and the triggers will only be recreated if the options have
//      changed.
//
//Note: If you ever need to force CopyCat to recreate a trigger, for whatever reason, simply set the CREATED field 
//of RPL$TABLES_CONFIG to 'N' for the row corresponding to the table and configuration that you need to update. This 
//setting will force CopyCat to recreate the trigger next time you call GenerateConfig.
//[ComponentPlatformsAttribute( PidWin32 Or PidiOSDevice Or PidiOSSimulator )]

{ TCcConfig }

TCcConfig = class(TComponent)
  private
    FPrevDestroyQueries: TNotifyEvent;
    FQueriesInitiated: Boolean;
    FOnQueryReady :TCcScriptEvent;
    Query: TStringList;
//    FTerminator: String;
    FScript: TStrings;
    FFieldNames: TStringList;
    FOnScriptReady: TCcNotifyEvent;
    qProcedure: TCcQuery;
    FConnection: TCcConnection;
		qTable: TCcQuery;
		qTableConfig: TCcQuery;
    FPrevExecQuery: TNotifyEvent;
    FTables: TCcConfigTables;
    FConfigName: String;
    FNodes: TStringList;
    FFailIfNoPK: Boolean;
    FTrackFieldChanges: Boolean;
    FOnProgress :TNotifyEvent;
    procedure AddUser(cUserName: String);
    function GetConnection :TCcConnection;
    procedure QueryReady;
    procedure QueryExecuted(Sender: TObject);
    procedure DestroyQueries(Sender: TObject);
    procedure RemoveUser(cUserName: String);
		procedure SetConnection(const Value: TCcConnection);
		function FindTable(TableName: String) :Boolean;

    procedure GetFieldNames(TableName: String);
    procedure CheckConnected;
    procedure SetScript(const Value: TStrings);
    procedure ScriptReady;
    procedure CheckMetaData;
    procedure SetTables(const Value: TCcConfigTables);
		procedure SetNodes(const Value: TStringList);
    procedure SetConfigName(const Value: String);
    procedure DropTable(cTableName: String);
    function ExistsInList(str: String; list: TStringList): Boolean;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure RefreshDisplay;
    function ListRPLTables: String;
    function ListRPLTablesConfig: String;
    function CalcTriggerName(cTableName: String): String;
protected
	procedure FillTables;virtual;
	procedure Notification(AComponent: TComponent; Operation: TOperation);override;
public
  procedure CheckUsers;
  procedure DropAllTriggers;
  procedure ConnectAndRemoveConfig;
  function TableNameWithConfig(cTableName: String): String;
  function ListRPLTablesConfigWithTriggerNames: String;

{*********************************************************************************
 Summary:
 Connect to the database.
 Description:
 Call Connect to open a connection to the database, using the parameters specified
 in the ConnectParams property. The Tables and Procedures properties must be
 assigned before calling Connect.<p/>
 <p/>
 After connecting, TCcConfig loads the list of tables and procedures into the
 Tables and Procedures properties, and then checks that the necessary replication
 meta-data exists in the database. See OnCreateMetaData for more information.
 See Also:
 Disconnect
 *********************************************************************************}
  procedure Connect;

{********************************************************
 Summary:
 Disconnect from the database.
 Description:
 Call Disconnect to close the connection to the database.
 See Also:
 Connect
 ********************************************************}
  procedure Disconnect;

{********************************************************************************
 Summary:
 Build SQL statement for synchronizing a primary key or generator field, based on
 a generator.
 Parameters:
 GenName :    Name of the generator to use.
 Increment :  Amount to increment the generator by.
 Returns:
 The generated SQL statement, in a format suitable to be put in a 'PKn_GEN' or
 'GENn_VALUE' field of Tables.
 See Also:
 GetProcGenerator, Tables
 Note:
 This function does not alter the Script property, it merely returns the SQL
 statement.
 ********************************************************************************}
  function GetGenerator(GenName: String; Increment: Integer): String;

//Summary:
//Get the list of parameters for a stored procedure.
//Parameters:
//ProcName :    Name of the stored procedure.
//Params :      Empty TCcProcParams object to fill.
//InputParam :  Fill Params with input or output parameters of the procedure?
//See Also:
//GetProcGenerator
//  procedure GetProcParams(ProcName: String; Params: TCcProcParams; InputParam: Boolean = True);

  {********************************************************************************
 Summary:
 Build SQL statement for synchronizing a primary key or generator field of the
 selected table, based on a stored procedure.
 Returns:
 The generated SQL statement, in a format suitable to be put in a 'PKn_GEN' or
 'GENn_VALUE' field of Tables.
 Parameters:
 ProcName :     Name of the stored procedure to select from.
 Params :       \Input parameters for the procedure, with their values filled in.
 OutputParam :  \Output parameter to be selected from the procedure.
 See Also:
 GetGenerator, Tables
 Note:
 This function does not alter the Script property, it merely returns the SQL
 statement.
 ********************************************************************************}
//  function GetProcGenerator(TableName, ProcName: String; Params: TCcProcParams; OutputParam: String): String;

  {*****************************************************************************
 Summary:
 Generate the replication triggers for the selected table.
 Description:
 Call this method to create the necessary replication triggers for the current
 table, based on the options set in Tables and Procedures.
 See Also:
 RemoveTriggers
 *****************************************************************************}
  procedure GenerateTriggers(TableName:String);

(*
{********************************************************************************
 Summary:
 Generate the replication meta-data for the selected procedure.
 Description:
 Use GenerateProcedure to generate the SQL necessary for creating the replication
 meta-data for the current procedure.
 See Also:
 RemoveProcedure
 ********************************************************************************}
  procedure GenerateProcedure(ProcName:String);
*)
{*****************************************************************************
 Summary:
 Remove replication triggers from the selected table.
 Description:
 Use RemoveTriggers to generate the SQL necessary for removing the replication
 triggers from the "TableName" table.
 See Also:
 GenerateTriggers
 Parameters:
 TableName :  Name of the table from which to remove triggers
 *****************************************************************************}
  procedure RemoveTriggers(TableName:String);
(*
{*******************************************************************************
 Summary:
 Remove the replication meta-data for the selected procedure.
 Description:
 Use RemoveProcedure to generate the SQL necessary for removing the replication
 meta-data for the selected procedure. The procedure itself is not removed, only
 the meta-data necessary for replicating it.
 Parameters:
 ProcName :  Name of the stored procedure to remove
 See Also:
 GenerateProcedure
 *******************************************************************************}
	procedure RemoveProcedure(ProcName:String);
  *)
	constructor Create(AOwner: TComponent);override;
	destructor Destroy;override;

//	procedure PrepareBatchImport(tableName: String; sqlCondition: String; destinationNode: String);

 {*******************************************************************************
 Summary:
 Creates triggers and / or replication nodes according to the configuration
 Description:
 Call GenerateConfig to create all the replication configuration based on the Nodes and Tables properties
	If the list of tables to replicate is given, we automatically create triggers for all the tables.
 If any table missing from the list (Tables property) its triggers are deleted.
 If a list of nodes is given in the Nodes property, it is used to fill the RPL$USERS table.
 *******************************************************************************}
	procedure GenerateConfig(lForceRecreateTriggers: Boolean = False; lCheckUsers: Boolean = True);
published
  //Summary: Set TrackFieldChanges to true in order to track the values of fields changed to RPL$LOG_VALUES
  //Description: This option must be set if you plan to use TCcReplicator.ReplicateOnlyChangedFields
  property TrackFieldChanges: Boolean read FTrackFieldChanges write FTrackFieldChanges;

  //Indicates whether tables with no primary key are accepted or not
  //If FailIfNoPK is true (the default value), then an error will be raised
  //if a table is detected with no primary key.
  //If FailIfNoPK is false, the full list of field values (excluding blobs and string fields over 50 chars)
  //will be used instead
  //ONLY USE THIS OPTION FOR ONE-WAY REPLICATION, otherwise, it could cause conflicts.
  property FailIfNoPK: Boolean read FFailIfNoPK write FFailIfNoPK;

    //Name of the current configuration.
	property ConfigName: String read FConfigName write SetConfigName;

	//The list of replication nodes (databases) towards which the data in the current database should be sent,
	//for the specified configuration. Since each configuration can have a different list of nodes, you could 
	//very well send different tables to different nodes, or perhaps send part of one table to all nodes and the 
	//whole table to some other node.
	property Nodes: TStringList read FNodes write SetNodes;

  {***************************************************************************
   Summary:
   Terminator character.
   Description:
   Terminator is the character used for separating statements in the meta-data
   script.
   See Also:
   Script
   ***************************************************************************}
//  property Terminator: String read FTerminator write FTerminator;
  {******************************************************************************
   Summary:
   Last meta-data script generated.
   Description:
	 Script is filled (and overwritten) every time GenerateTriggers,
   GenerateProcedure, RemoveTriggers or RemoveProcedure are called. It represents
   the last generated Script.
   See Also:
   Terminator
	 ******************************************************************************}
  property Script: TStrings read FScript write SetScript;
  {********************************************************************************
   Summary:
   Database connection parameters.
   Description:
   Connection defines the necessary parameters for connecting to the database to
   be configured.
   ********************************************************************************}
  property Connection: TCcConnection read FConnection write SetConnection;
	{********************************************************************************
   Summary:
   Fired when an entire meta-data Script has been prepared.
   Description:
   When an script has been generated by TCcConfig, it is placed in the Script
   property, and the OnScriptReady event is fired to give the application the
   opportunity to either execute it, or stores it. The individual statements in the
   script are separated using the Terminator character.
   See Also:
   OnQueryReady
   ********************************************************************************}
  property OnScriptReady: TCcNotifyEvent read FOnScriptReady write FOnScriptReady;
	{*********************************************************************************
	 Summary:
   Fired for every individual meta-data query.
   Description:
   The purpose of OnQueryReady is to give the application the possibility to execute
   the meta-data statements created by TCcConfig, one statement at a time.
   See Also:
   OnScriptReady
   *********************************************************************************}
  property OnQueryReady :TCcScriptEvent read FOnQueryReady write FOnQueryReady;
  property OnProgress :TNotifyEvent read FOnProgress write FOnProgress;

  //The Tables property hold the list of to be replicated for the current configuration
  //You can also set various options.
  //See also: TCcConfigTables
  property Tables: TCcConfigTables read FTables write SetTables;
  property Version: String read GetVersion write SetVersion;
end;

implementation

uses SysUtils;

function TCcConfig.TableNameWithConfig(cTableName: String): String;
var
  test: String;
begin
  //test := concat('_', PChar(cTableName));
  //Result := test;
	if (ConfigName <> '') then
   //        Result := '_' + cTableName
		Result := concat(PChar(FConfigName), '_', PChar(cTableName))
	else
		Result := Trim(cTableName);
end;

procedure TCcConfig.RemoveTriggers(TableName: String);
var
  cSQL: String;
begin
	if not FindTable(TableName) then Exit;

	if (ConfigName = '') then begin
		FConnection.DBAdaptor.RemoveTriggers(qTable);
		cSQL := 'update RPL$TABLES set created = ''N'' where %upper_case(table_name) = %upper_case(:table_name)';
	end else begin
		FConnection.DBAdaptor.RemoveTriggers(qTableConfig);
		cSQL := 'update RPL$TABLES_CONFIG set created = ''N'' where %upper_case(table_name) = %upper_case(:table_name) and config_name = ' + QuotedStr(ConfigName);
	end;
	with GetConnection.UpdateQuery['TCcConfig_RemoveTriggers'] do begin
    Close;
    SQL.Text := cSQL;
		if GetConnection.DBAdaptor.QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';
    Param['table_name'].Value := TableName;
    Exec;
  end;
end;

function TCcConfig.FindTable(TableName: String): Boolean;
begin
	qTable := GetConnection.SelectQuery['TCcConfig_qTable'];
	with qTable do begin
		Close;
		SQL.Text := 'select t.* from RPL$TABLES t where %upper_case(t.table_name) = %upper_case(:table_name)';
		if GetConnection.DBAdaptor.QuoteMetadata then
			Macro['upper_case'].Value := ''
		else
			Macro['upper_case'].Value := 'upper';

		Param['table_name'].AsString := TableName;
		Exec;
		Result := (RecordCount > 0);
	end;
	if (ConfigName <> '') then begin
		qTableConfig := GetConnection.SelectQuery['TCcConfig_qTableConfig'];
		with qTableConfig do begin
			Close;
			SQL.Text := 'select tc.* from RPL$TABLES_CONFIG tc where %upper_case(tc.table_name) = %upper_case(:table_name) and upper(config_name) = ' + QuotedStr(ConfigName);
			if GetConnection.DBAdaptor.QuoteMetadata then
				Macro['upper_case'].Value := ''
			else
				Macro['upper_case'].Value := 'upper';

			Param['table_name'].AsString := TableName;
			Exec;
		end;
	end else
	  qTableConfig := nil;
end;

procedure TCcConfig.ScriptReady;
begin
  if Assigned(FOnScriptReady) then
    FOnScriptReady(Self);
end;

procedure TCcConfig.QueryExecuted(Sender: TObject);
var
  qQuery: TCcQuery;
begin
  if Assigned(FPrevExecQuery) then
    FPrevExecQuery(Sender);

  qQuery := (Sender as TCcQuery);
  if qQuery.Name = 'qConfigQuery' then
  begin
    Query.Text := qQuery.SQL.Text;
    Script.Add(Query.Text);
//    Script.Add(FTerminator);
    try
      GetConnection.CommitRetaining;
      if Assigned(FOnQueryReady) then
        FOnQueryReady(Self, Query);
    finally
      Query.Clear;
    end;
  end;
end;

procedure TCcConfig.QueryReady;
begin
	try
    with GetConnection.UpdateQuery['TCcConfig_qQuery'] do begin
      Close;
      SQL.Text := Query.Text;
      Exec;
    end;
    if Assigned(FOnQueryReady) then begin
//      Query.Add(FTerminator);
      FOnQueryReady(Self, Query);
    end;
  finally
    Query.Clear;
  end;
  RefreshDisplay;
end;

(*
procedure TCcConfig.RemoveProcedure(ProcName:String);
begin
  if not FindProcedure(ProcName) then Exit;
  if GetConnection.DBAdaptor.ProcedureExists(qProcedure.Field['NEW_PROCEDURE_NAME'].AsString) then begin
    Query.Add('DROP PROCEDURE ' + GetConnection.DBAdaptor.MetaQuote(qProcedure.Field['NEW_PROCEDURE_NAME'].AsString));
    QueryReady;
    Query.Add('update RPL$PROCEDURES set created = ''N'' where procedure_name = ' + QuotedStr(ProcName));
    QueryReady;
    ScriptReady;
  end;
end;
*)

{procedure TCcConfig.GenerateProcedure(ProcName:String);
var
  Params: TCcProcParams;
  NewProcName: String;
begin
  if not FindProcedure(ProcName) then Exit;

  NewProcName := qProcedure.Field['NEW_PROCEDURE_NAME'].AsString;

  if Trim(NewProcName) = '' then
    raise Exception.Create('You must give the name for the new procedure you want to create!'#13#10'This new procedure will allow you to replicate all calls to the selected stored procedure.');
  if qProcedure.Field['PRIORITY'].Value <= 0 then
    raise Exception.Create('You must provide the priority of this procedure in the replication process.');

  //Check if procedure already exists, and if so, remove it
  RemoveProcedure(ProcName);

  if not GetConnection.DBAdaptor.GenDeclared(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_' + copy(NewProcName, 1, 27))) then
    GetConnection.DBAdaptor.DeclareGenerator(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_' + copy(NewProcName, 1, 27)));

  Params := TCcProcParams.Create(Self);
  GetProcParams(ProcName, Params);

  GetConnection.DBAdaptor.GenerateProcedure(qProcedure, Params);

  Query.Add('update RPL$PROCEDURES set created = ''Y'' where procedure_name = ' + QuotedStr(ProcName));
  QueryReady;

  ScriptReady;
end;   }

{
//This procedure checks a trigger exists, and if so, removes it.
//RemoveTrigger is called internally by RemoveTriggers and GenerateTriggers,
//in order to avoid duplicate trigger creation or deletion.
//Parameters:
//cTriggerName: Name of the trigger to remove
procedure TCcConfig.RemoveTrigger(cTriggerName:String);
begin
	if GetConnection.DBAdaptor.TriggerExists(cTriggerName) then begin
		Query.Add('DROP TRIGGER ' + GetConnection.DBAdaptor.MetaQuote(cTriggerName));
		QueryReady;
	end;
end;                                                  }

procedure TCcConfig.GenerateTriggers(TableName: String);
var
	cTableName, cQuotedTableName: String;
  nNumberTriggers: Integer;
begin
	cTableName := Trim(TableName);
  cQuotedTableName := GetConnection.DBAdaptor.MetaQuote(cTableName);

	if not FindTable(cTableName) then Exit;
//TriggerName := Trim(qTable.Field['TRIG_BASE_NAME'].AsString);

	//If primary or unique key synchronization is configured, create the RPL$LOCAL field in the table
	if ((Trim(qTable.Field['PRIMARY_KEY_SYNC'].AsString) <> '') or (Trim(qTable.Field['UNIQUE_KEY_SYNC'].AsString) <> ''))
		and not GetConnection.DBAdaptor.FieldExists(cTableName, GetConnection.DBAdaptor.UnQuotedIdentifier('RPL$LOCAL')) then begin
		Query.Add('alter table ' + cQuotedTableName + ' add RPL$LOCAL char(1) default ''N''');
		QueryReady;
	end;

	if Assigned(qTableConfig) then
		FConnection.DBAdaptor.RemoveTriggers(qTableConfig)
	else
	  FConnection.DBAdaptor.RemoveTriggers(qTable);

//	GetConnection.CommitRetaining;

 	nNumberTriggers := GetConnection.DBAdaptor.GenerateTriggers(qTable, qTableConfig, FailIfNoPK, TrackFieldChanges);

	if (ConfigName <> '') then begin
		Query.Add('update RPL$TABLES_CONFIG set created = ''Y'', number_of_triggers = ' + IntToStr(nNumberTriggers) + ' where table_name = ' + QuotedStr(cTableName) + ' and config_name = ' + QuotedStr(ConfigName));
   	QueryReady;
  end;

  Query.Add('update RPL$TABLES set created = ''Y'' where table_name = ' + QuotedStr(cTableName));
	QueryReady;

	GetConnection.CommitRetaining;
	ScriptReady;
end;

function TCcConfig.GetGenerator(GenName: String;
	Increment: Integer): String;
begin
	Result := GetConnection.DBAdaptor.GetGenerator(Trim(GenName), Increment);
end;

function TCcConfig.GetVersion: String;
begin
  Result := VersionNumber;
end;

(*
function TCcConfig.GetProcGenerator(TableName, ProcName: String; Params: TCcProcParams; OutputParam: String): String;
begin
	if not FindTable(TableName) then Exit;

  GetFieldNames(qTable.Field['TABLE_NAME'].AsString);
  Result := GetConnection.DBAdaptor.GetProcGenerator(ProcName, Params, OutputParam, FFieldNames);
end;
*)
procedure TCcConfig.GetFieldNames(TableName: String);
begin
	CheckConnected;
	FFieldNames.Assign(FConnection.ListTableFields(TableName));

{	with GetConnection.MetaQuery[sqlTableFields] do begin
		Close();
		Param['table_name'].AsString := TableName;
		Exec;

		FFieldNames.Clear();
		while (not Eof) do begin
			FFieldNames.Add(Trim(Field['field_name'].AsString));
			Next;
		end;
  end;}
end;

function TCcConfig.GetConnection: TCcConnection;
begin
  if Assigned(FConnection) then
    Result := FConnection
  else
    raise Exception.Create('Database connection not assigned!');
end;

(*procedure TCcConfig.GetProcParams(ProcName: String; Params: TCcProcParams; InputParam: Boolean = True);
begin
  CheckConnected;
  GetConnection.DBAdaptor.GetProcParams(ProcName, Params, InputParam);
end;
  *)

constructor TCcConfig.Create(AOwner: TComponent);
begin
  inherited;
  FTables := TCcConfigTables.Create(Self);

	FQueriesInitiated := False;

  FNodes := TStringList.Create;
  Query := TStringList.Create;
  FFieldNames := TStringList.Create;
  FScript := TStringList.Create;

//  Terminator := '§';
end;

destructor TCcConfig.Destroy;
begin
  Query.Free;
	FNodes.Free;
	FTables.Free; 

  FFieldNames.Free;
  FScript.Free;
  inherited;
end;

procedure TCcConfig.CheckConnected;
begin
  if not GetConnection.Connected then
    Connect;
end;

procedure TCcConfig.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

{
procedure TCcConfig.LoadConfig;
begin

  if not Assigned(ConfigStorage) then Exit;

  if Assigned(Connection) then
    Connection.Disconnect;

  if DatabaseNode = dnLocal then
    Connection := ConfigStorage.LocalDB.Connection
  else
    Connection := ConfigStorage.RemoteDB.Connection;
end;
}
procedure TCcConfig.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if AComponent = FConnection then
      SetConnection(nil);
  end;
  inherited;
end;

{
procedure TCcConfig.SetConfigStorage(const Value: TCcConfigStorage);
begin
  inherited;
end;}

procedure TCcConfig.SetConnection(const Value: TCcConnection);
begin
  if FConnection = Value then Exit;

  if FConnection <> nil then begin
    FConnection.OnDestroyQueries := FPrevDestroyQueries;
    FConnection.OnQueryExecute := FPrevExecQuery;
    FConnection.RemoveFreeNotification(Self);
  end;
  FConnection := Value;
  if FConnection <> nil then begin
    FPrevDestroyQueries := FConnection.OnDestroyQueries;
    FConnection.OnDestroyQueries := DestroyQueries;
    FPrevExecQuery := FConnection.OnQueryExecute;
    FConnection.OnQueryExecute := QueryExecuted;
    FConnection.FreeNotification(Self);
  end
  else
    DestroyQueries(nil);
end;

constructor TCcProcParams.Create(AOwner: TComponent);
begin
  inherited;
  if not (csLoading in ComponentState) then
    LoadFields;
end;

procedure TCcConfig.Connect;
begin
  GetConnection.Connect;
  RefreshDisplay;

  CheckMetaData;
  RefreshDisplay;

  GetConnection.Disconnect;
  GetConnection.Connect;

  FillTables;
  RefreshDisplay;

  RefreshDisplay;
end;

procedure TCcConfig.DropTable(cTableName:String);
begin
  if GetConnection.DBAdaptor.TableExists(GetConnection.DBAdaptor.UnQuotedIdentifier(cTableName)) then begin
    Query.Clear;
    Query.Add('DROP TABLE ' + cTableName);
    QueryReady;
    GetConnection.CommitRetaining;
  end;
end;

procedure TCcConfig.DropAllTriggers;
var
  slTriggers: TStringList;
  I: Integer;
  cTriggerName: string;
begin
  slTriggers := GetConnection.ListTriggers;
  for I:=0 to slTriggers.Count-1 do begin
    cTriggerName := Trim(slTriggers[I]);
    if Uppercase(Copy(cTriggerName, 1, 4)) = 'RPL$' then
      GetConnection.ExecQuery('DROP TRIGGER ' + GetConnection.DBAdaptor.MetaQuote(cTriggerName));
  end;
end;

procedure TCcConfig.ConnectAndRemoveConfig;
begin
  try
    GetConnection.Connect;

    DropAllTriggers;
    if GetConnection.DBAdaptor.TableExists(GetConnection.DBAdaptor.UnQuotedIdentifier('RPL$TABLES_CONFIG')) then
      GetConnection.ExecQuery('UPDATE RPL$TABLES_CONFIG SET CREATED = ''N''');
    GetConnection.CommitRetaining;

    GetConnection.DBAdaptor.RemoveExtraCustomMetadata;
    GetConnection.CommitRetaining;

    GetConnection.DBAdaptor.DropProcedures;
    GetConnection.CommitRetaining;

    DropTable('RPL$TABLES');
    DropTable('RPL$TABLES_CONFIG');
    DropTable('RPL$ERRORS');
    DropTable('RPL$PROCEDURES');
    DropTable('RPL$LOG');
    DropTable('RPL$CONFLICTS');
    DropTable('RPL$TRACE');
    DropTable('RPL$USERS');
    DropTable('RPL$LOG_VALUES');

    GetConnection.DBAdaptor.DropGenerator(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_RPL$LOG'));
    GetConnection.DBAdaptor.DropGenerator(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_RPL$LOG_VALUES'));
    GetConnection.DBAdaptor.DropGenerator(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_RPL$CONFLICTS'));
    GetConnection.DBAdaptor.DropGenerator(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_RPL$ERRORS'));
    GetConnection.DBAdaptor.DropGenerator(GetConnection.DBAdaptor.UnQuotedIdentifier('GEN_RPL$TRACE'));

    GetConnection.DBAdaptor.RemoveCustomMetadata;

    GetConnection.CommitRetaining;
  finally
    if GetConnection.InTransaction then
      GetConnection.RollbackRetaining;
  end;
end;

procedure TCcConfig.Disconnect;
begin
  if Assigned(FConnection) then
    GetConnection.Disconnect;
end;

procedure TCcConfig.RefreshDisplay;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

(*
procedure TCcConfig.FillProcedures;
var
//	qProcedures: TCcQuery;
	slProcs, slRPLProcs: TStringList;
	I: Integer;
begin
	if GetConnection.Connected then
	begin
		slProcs := GetConnection.ListProcedures;
    slRPLProcs := TStringList.Create;
    try
      slRPLProcs.CommaText := ListRPLProcedures;
      for I:=0 to slProcs.Count-1 do begin
        if slRPLProcs.IndexOf(slProcs[i]) = -1 then
          with GetConnection.UpdateQuery['TCcConfig_qFillProcedures'] do begin
            Close;
            if SQL.Text = '' then
              SQL.Text := 'insert into RPL$PROCEDURES (procedure_name, new_procedure_name, priority) values (:procedure_name, :new_procedure_name, :priority)';
            Param['PROCEDURE_NAME'].AsString := Copy(Trim(slProcs[i]), 1, 50);
            Param['NEW_PROCEDURE_NAME'].AsString := Copy('RPL$' + slProcs[i], 1, 29);
            Param['PRIORITY'].Value := 0;
            Exec;
          end;
        RefreshDisplay;
      end;
    finally
      slRPLProcs.Free;
    end;

{		qProcedures := GetConnection.MetaQuery[sqlProcedures];
		qProcedures.Close;
		try
			qProcedures.Exec;
		except
			Exit;
		end;
		while not qProcedures.Eof do
		begin
			if not FindProcedure(qProcedures.Field['PROCEDURE_NAME'].AsString) then
				with GetConnection.Query['TCcConfig_qFillProcedures'] do begin
					Close;
					if SQL.Text = '' then
						SQL.Text := 'insert into RPL$PROCEDURES (procedure_name, new_procedure_name, priority) values (:procedure_name, :new_procedure_name, :priority)';
					Param['PROCEDURE_NAME'].AsString := Trim(qProcedures.Field['PROCEDURE_NAME'].AsString);
					Param['NEW_PROCEDURE_NAME'].AsString := Copy('RPL$' + Param['PROCEDURE_NAME'].AsString, 1, 29);
					Param['PRIORITY'].Value := 0;
					Exec;
				end;
			qProcedures.Next;
		end;}

		GetConnection.CommitRetaining;
	end;
end; *)

function TCcConfig.ListRPLTables: String;
begin
  Result := '';
	qTable := GetConnection.SelectQuery['TCcConfig_qRPLTables'];
	with qTable do begin
		Close;
		SQL.Text := 'select t.TABLE_NAME from RPL$TABLES t';
		Exec;
    while not Eof do begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + Field['TABLE_NAME'].AsString;
      Next;
    end;
	end;
end;

{function TCcConfig.ListRPLProcedures: String;
begin
	qTable := GetConnection.SelectQuery['TCcConfig_qRPLProcedures'];
	with qTable do begin
		Close;
		SQL.Text := 'select p.procedure_name from RPL$PROCEDURES P';
		Exec;
    while not Eof do begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + Field['procedure_name'].AsString;
      Next;
    end;
	end;
end;
 }
function TCcConfig.ListRPLTablesConfig: String;
begin
  Result := '';
	qTable := GetConnection.SelectQuery['TCcConfig_qRPLTables_Config'];
	with qTable do begin
		Close;
		SQL.Text := 'select t.TABLE_NAME from RPL$TABLES_CONFIG t where t.config_name = :config_name';
    Param['config_name'].Value := Trim(ConfigName);
		Exec;

    while not Eof do begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + Field['TABLE_NAME'].AsString;
      Next;
    end;
	end;
end;

function TCcConfig.ListRPLTablesConfigWithTriggerNames: String;
begin
  Result := '';
	qTable := GetConnection.SelectQuery['TCcConfig_qRPLTables_Config'];
	with qTable do begin
		Close;
		SQL.Text := 'select t.TABLE_NAME, t.TRIG_BASE_NAME from RPL$TABLES_CONFIG t where t.config_name = :config_name';
    Param['config_name'].Value := Trim(ConfigName);
		Exec;

    while not Eof do begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + Field['TABLE_NAME'].AsString + '=' + Field['TRIG_BASE_NAME'].AsString;
      Next;
    end;
	end;
end;

function TCcConfig.CalcTriggerName(cTableName: String):String;
var
  cTableNameStub: String;
  nNum: Integer;
  cNum: String;
begin
  if ConfigName = '' then
    Result := Trim(Copy('RPL$' + cTableName, 1, GetConnection.MaxDDLNameLength-2))
  else begin
    cTableNameStub := Copy(TableNameWithConfig(cTableName), 1, GetConnection.MaxDDLNameLength-8);
    with Connection.SelectQuery['TCcConfig.CalcTriggerName_CheckDuplicate'] do begin
      Close;
      SQL.Text := 'select count(*) as cnt, max(trig_base_name) as max_name from RPL$TABLES_CONFIG where '
        + GetConnection.DBAdaptor.SubStringFunction('config_name ' + GetConnection.DBAdaptor.ConcatenationOperator + '''_''' + GetConnection.DBAdaptor.ConcatenationOperator + ' table_name', 1, GetConnection.MaxDDLNameLength-8)
        + ' = :table_name_stub and config_name = :config_name and table_name <> :table_name';
      Param['table_name_stub'].Value := cTableNameStub;
      Param['table_name'].Value := cTableName;
      Param['config_name'].Value := ConfigName;
      Exec;
      if Field['cnt'].AsInteger = 0 then
        Result := 'RPL$' + cTableNameStub
      else
      begin
        nNum := StrToIntDef(Copy(Field['max_name'].AsString, Length('RPL$'+cTableNameStub)+1, 2), 0);
        Inc(nNum);
        if nNum < 10 then
          cNum := '0' + IntToStr(nNum)
        else
          cNum := IntToStr(nNum);
        Result := 'RPL$' + cTableNameStub + cNum;
      end;
    end;
  end;
end;

procedure TCcConfig.FillTables;
var
	cTableName: String;
//	qTables: TCcQuery;
	slTables,slRPLTables,slRPLTablesConfig: TStringList;
	I: Integer;
  cTriggerName, cRPLTablesConfig: String;
begin
	if GetConnection.Connected then
	begin
		slTables := GetConnection.ListTables;
    slRPLTables := TStringList.Create;
    slRPLTablesConfig := TStringList.Create;
    try
      slRPLTables.CommaText := ListRPLTables;
      cRPLTablesConfig := ListRPLTablesConfig;
      slRPLTablesConfig.CommaText := cRPLTablesConfig;
      for I:=0 to slTables.Count-1 do begin
        cTableName := slTables[i];
        cTriggerName := CalcTriggerName(cTableName);
        if slRPLTables.IndexOf(cTableName) = -1 then
        with GetConnection.UpdateQuery['TCcConfig_qFillTables'] do
        begin
          Close;
          if SQL.Text = '' then
            SQL.Text := 'insert into RPL$TABLES(table_name, TRIG_BASE_NAME, priority, REPL_INSERTS, REPL_UPDATES, REPL_DELETES) values (:table_name, :TRIG_BASE_NAME, :priority, ''Y'', ''Y'', ''Y'')';
          Param['TRIG_BASE_NAME'].AsString := cTriggerName;
          Param['priority'].AsInteger := 0;
          Param['table_name'].AsString := cTableName;
          Exec;
        end;
        if (ConfigName <> '') and (slRPLTablesConfig.IndexOf(cTableName) = -1) then
          with GetConnection.UpdateQuery['TCcConfig_qFillTablesConfig'] do
          begin
            Close;
            if SQL.Text = '' then
              SQL.Text := 'insert into RPL$TABLES_CONFIG(table_name, config_name, TRIG_BASE_NAME, REPL_INSERTS, REPL_UPDATES, REPL_DELETES, PRIORITY) values (:table_name, :config_name, :TRIG_BASE_NAME, ''Y'', ''Y'', ''Y'', 0)';
            Param['TRIG_BASE_NAME'].AsString := cTriggerName;
            Param['table_name'].AsString := cTableName;
            if (ConfigName = '') then
              Param['config_name'].Clear
            else
              Param['config_name'].Value := ConfigName;
            Exec;
          end;
          RefreshDisplay;
      end;
		finally
      slRPLTables.Free;
      slRPLTablesConfig.Free;
    end;

{		qTables := GetConnection.MetaQuery[sqlTables];
		qTables.Close;
		try
			qTables.Exec;
		except
			Exit;
		end;
		while not qTables.Eof do
		begin
			cTableName := qTables.Field['table_name'].AsString;
			if not FindTable(cTableName) then
			with GetConnection.Query['TCcConfig_qFillTables'] do
			begin
				Close;
				if SQL.Text = '' then
					SQL.Text := 'insert into RPL$TABLES(table_name, TRIG_BASE_NAME, priority, REPL_INSERTS, REPL_UPDATES, REPL_DELETES) values (:table_name, :TRIG_BASE_NAME, :priority, ''Y'', ''Y'', ''Y'')';
				Param['table_name'].AsString := cTableName;
				Param['TRIG_BASE_NAME'].AsString := 'RPL$' + Trim(Copy(cTableName, 1, GetConnection.MaxDDLNameLength-6));
				Param['priority'].Value := 0;
				Exec;
			end;
			qTables.Next;
		end;}
		GetConnection.CommitRetaining;
	end;
end;

procedure TCcConfig.CheckMetaData;
var
  slTables:TStringList;

function MyTableExists(TableName: String): Boolean;
var
  I: Integer;
begin
  if slTables.Count = 0 then
    slTables.Assign(FConnection.ListAllTables);

  Result := False;
  for i := 0 to slTables.Count - 1 do
  begin
    if GetConnection.DBAdaptor.QuoteMetadata then
      Result := AnsiSameStr(slTables[i], TableName)
    else
      Result := AnsiSameText(slTables[i], TableName);
    if Result then
      Break;
  end;

end;

begin
  slTables := TStringList.Create;
  try

	with GetConnection.DBAdaptor do begin
		CheckCustomMetadata;

		if not GenDeclared(UnQuotedIdentifier('GEN_RPL$LOG')) then
			DeclareGenerator(UnQuotedIdentifier('GEN_RPL$LOG'));

		if not GenDeclared(UnQuotedIdentifier('GEN_RPL$LOG_CHANGE_NUMBER')) then
			DeclareGenerator(UnQuotedIdentifier('GEN_RPL$LOG_CHANGE_NUMBER'));

		if not GenDeclared(UnQuotedIdentifier('GEN_RPL$CONFLICTS')) then
			DeclareGenerator(UnQuotedIdentifier('GEN_RPL$CONFLICTS'));

{		if not GenDeclared(UnQuotedIdentifier('GEN_RPL$ERRORS')) then
			DeclareGenerator(UnQuotedIdentifier('GEN_RPL$ERRORS'));
 }
    if not GenDeclared(UnQuotedIdentifier('GEN_RPL$TRACE')) then
      DeclareGenerator(UnQuotedIdentifier('GEN_RPL$TRACE'));

(*  if not TableExists('RPL$CONFIG') then begin
      with Query do
      begin
        Clear;
        Add('CREATE TABLE RPL$CONFIG (');
        Add(DeclareField('COPYCAT_VERSION', ftString, 10, True, True, False) + ',');
        Add(DeclareField('DBTYPE', ftString, 20, False, False, False) + ',');
        Add(DeclareField('DBVERSION', ftString, 20, False, False, False) + ',');
        Add(')');
      end;
      QueryReady;
    end;*)

    if not MyTableExists(UnQuotedIdentifier('RPL$TABLES')) then begin
      with Query do
      begin
        Clear;
        Add('CREATE TABLE RPL$TABLES (');
				Add(DeclareField('TABLE_NAME', ftString, 100, True, True, False) + ',');
        Add(DeclareField('PRIMARY_KEY_SYNC', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('UNIQUE_KEY_NAMES', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('UNIQUE_KEY_SYNC', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('TRIG_BASE_NAME', ftString, 50, False, False, False) + ',');
        Add(DeclareField('REF_TABLE', ftString, 50, False, False, False) + ',');
        Add(DeclareField('REF_TABLE_KEY', ftString, 50, False, False, False) + ',');
        Add(DeclareField('CREATED', ftString, 1, False, False, False) + ',');
        Add(DeclareField('REPL_INSERTS', ftString, 1, False, False, False) + ',');
        Add(DeclareField('REPL_UPDATES', ftString, 1, False, False, False) + ',');
        Add(DeclareField('REPL_DELETES', ftString, 1, False, False, False) + ',');
        Add(DeclareField('CONDITION_FIELD', ftString, 50, False, False, False) + ',');
        Add(DeclareField('CONDITION', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('PRIORITY', ftInteger, 0, False, False, False));
				Add(')');
//        Add('ENGINE=MyISAM');
			end;
      QueryReady;
      GrantRightsToTable('RPL$TABLES');
    end;
    RefreshDisplay;

		if not MyTableExists(UnQuotedIdentifier('RPL$TABLES_CONFIG')) then begin
			with Query do
			begin
				Clear;
				Add('CREATE TABLE RPL$TABLES_CONFIG (');
				Add(DeclareField('TABLE_NAME', ftString, 100, True, False, False) + ',');
				Add(DeclareField('CONFIG_NAME', ftString, 100, True, False, False)+ ',');
				Add(DeclareField('TRIG_BASE_NAME', ftString, 50, False, False, False) + ',');
				Add(DeclareField('CREATED', ftString, 1, False, False, False) + ',');
				Add(DeclareField('REPL_INSERTS', ftString, 1, False, False, False) + ',');
				Add(DeclareField('REPL_UPDATES', ftString, 1, False, False, False) + ',');
				Add(DeclareField('REPL_DELETES', ftString, 1, False, False, False) + ',');
				Add(DeclareField('CONDITION', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('INSERT_CONDITION', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('UPDATE_CONDITION', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('DELETE_CONDITION', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('INCLUDED_FIELDS', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('EXCLUDED_FIELDS', ftMemo, 0, False, False, False) + ',');
				Add(DeclareField('PRIORITY', ftInteger, 0, False, False, False) + ',');
        Add(DeclareField('TRACKED_FIELDS', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('NUMBER_OF_TRIGGERS', ftInteger, 0, False, False, False) + ',');
        Add(DeclareField('PRIMARY_KEY_SYNC', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('UNIQUE_KEY_NAMES', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('UNIQUE_KEY_SYNC', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('MASTER_TABLE_NAME', ftString, 50, False, False, False) + ',');
        Add(DeclareField('MASTER_TABLE_KEYS', ftString, 200, False, False, False) + ',');
        Add(DeclareField('DETAIL_TABLE_NAME', ftString, 200, False, False, False) + ',');
				Add(DeclarePK('TABLE_NAME, CONFIG_NAME'));
				Add(')');
//        Add('ENGINE=MyISAM');
			end;
			QueryReady;
      GrantRightsToTable('RPL$TABLES_CONFIG');
		end
    else if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$TABLES_CONFIG'), UnQuotedIdentifier('EXCLUDED_FIELDS')) then begin
      with Query do
      begin
        Clear;
				Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('EXCLUDED_FIELDS', ftMemo, 0, False, False, False));
      end;
      QueryReady;
    end
    else if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$TABLES_CONFIG'), UnQuotedIdentifier('PRIORITY')) then begin
      with Query do
      begin
        Clear;
				Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('PRIORITY', ftInteger, 0, False, False, False));
      end;
      QueryReady;
    end;
    if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$TABLES_CONFIG'), UnQuotedIdentifier('PRIMARY_KEY_SYNC')) then begin
      with Query do
      begin
        Clear;
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('PRIMARY_KEY_SYNC', ftMemo, 0, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('UNIQUE_KEY_NAMES', ftMemo, 0, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('UNIQUE_KEY_SYNC', ftMemo, 0, False, False, False));
        QueryReady;
       end;
    end;
    if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$TABLES_CONFIG'), UnQuotedIdentifier('NUMBER_OF_TRIGGERS')) then begin
      with Query do
      begin
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('NUMBER_OF_TRIGGERS', ftInteger, 0, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('TRACKED_FIELDS', ftMemo, 0, False, False, False));
        QueryReady;
     end;
    end;
    if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$TABLES_CONFIG'), UnQuotedIdentifier('MASTER_TABLE_NAME')) then begin
      with Query do
      begin
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('MASTER_TABLE_NAME', ftString, 50, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('MASTER_TABLE_KEYS', ftString, 200, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$TABLES_CONFIG ');
        Add('ADD ' + DeclareField('DETAIL_TABLE_NAME', ftString, 200, False, False, False));
        QueryReady;
     end;
    end;

    if not MyTableExists(UnQuotedIdentifier('RPL$LOG')) then
    begin
      with Query do
      begin
        Clear;
        Add('CREATE TABLE RPL$LOG (');
        Add(DeclareField('CODE', ftInteger, 0, True, True, True) + ',');
        Add(DeclareField('LOGIN', ftString, 50, False, False, False) + ',');
        Add(DeclareField('SENT_FROM', ftString, 50, False, False, False) + ',');
        Add(DeclareField('OPERATION_DATE', ftDateTime, 0, False, False, False) + ',');
        Add(DeclareField('TABLE_NAME', ftString, 100, False, False, False) + ',');
        Add(DeclareField('PRIMARY_KEY_VALUES', ftString, 500, False, False, False) + ',');
        Add(DeclareField('PRIMARY_KEY_SYNC', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('UNIQUE_KEY_SYNC', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('CONFLICT', ftString, 1, False, False, False) + ',');
        Add(DeclareField('PROCEDURE_STATEMENT', ftString, 200, False, False, False) + ',');
        Add(DeclareField('REPLICATION_STATE', ftString, 20, False, False, False) + ',');
        Add(DeclareField('ERROR_MESSAGE', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('OPERATION_TYPE', ftFixedChar, 1, False, False, False) + ',');
        Add(DeclareField('ERROR_CONTEXT', ftString, 50, False, False, False) + ',');       
        Add(DeclareField('TRANSACTION_NUMBER', ftInteger, 0, False, False, False) + ',');
        Add(DeclareField('CHANGE_NUMBER', ftInteger, 0, False, False, False));
        Add(')');
//        Add('ENGINE=MyISAM');
      end;
      QueryReady;
      GrantRightsToTable('RPL$LOG');
    end
    else begin
      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('CHANGE_NUMBER')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('CHANGE_NUMBER', ftInteger, 0, False, False, False));
        end;
        QueryReady;
      end;

      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('SENT_FROM')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('SENT_FROM', ftString, 50, False, False, False));
        end;
        QueryReady;
      end;
      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('REPLICATION_STATE')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('REPLICATION_STATE', ftString, 20, False, False, False));
        end;
        QueryReady;
      end;
      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('ERROR_MESSAGE')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('ERROR_MESSAGE', ftMemo, 0, False, False, False));
        end;
        QueryReady;
      end;
      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('ERROR_CONTEXT')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('ERROR_CONTEXT', ftString, 50, False, False, False));
        end;
        QueryReady;
      end;
      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('OPERATION_TYPE')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('OPERATION_TYPE', ftFixedChar, 1, False, False, False));
        end;
        QueryReady;
      end;
      if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG'), UnQuotedIdentifier('TRANSACTION_NUMBER')) then begin
        with Query do
        begin
          Clear;
          Add('ALTER TABLE RPL$LOG ');
          Add('ADD ' + DeclareField('TRANSACTION_NUMBER', ftInteger, 0, False, False, False));
        end;
        QueryReady;
      end;
    end;

    if not MyTableExists(UnQuotedIdentifier('RPL$LOG_VALUES')) then
    begin
      with Query do
      begin
        Clear;
        Add('CREATE TABLE RPL$LOG_VALUES (');
        Add(DeclareField('NODE_NAME', ftString, 50, True, False, False) + ',');
        Add(DeclareField('CHANGE_NUMBER', ftInteger, 0, True, False, False) + ',');
        Add(DeclareField('OLD_VALUE', ftString, 250, False, False, False) + ',');
        Add(DeclareField('OLD_VALUE_BLOB', ftBlob, 0, False, False, False) + ',');
        Add(DeclareField('NEW_VALUE', ftString, 250, False, False, False) + ',');
        Add(DeclareField('NEW_VALUE_BLOB', ftBlob, 0, False, False, False) + ',');
        Add(DeclareField('FIELD_NAME', ftString, 50, True, False, False) + ',');
        Add(DeclareField('FIELD_TYPE', ftInteger, 0, False, False, False) + ',');
        Add(DeclareField('OLD_BLOB_NULL', ftFixedChar, 1, False, False, False) + ',');
        Add(DeclareField('NEW_BLOB_NULL', ftFixedChar, 1, False, False, False) + ',');
				Add(DeclarePK('NODE_NAME, CHANGE_NUMBER, FIELD_NAME'));
        Add(')');
//        Add('ENGINE=MyISAM');
      end;
      QueryReady;
      GrantRightsToTable('RPL$LOG_VALUES');
    end
    else if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$LOG_VALUES'), UnQuotedIdentifier('OLD_BLOB_NULL')) then begin
      with Query do
      begin
        Clear;
        Add('ALTER TABLE RPL$LOG_VALUES ');
        Add('ADD ' + DeclareField('OLD_BLOB_NULL', ftFixedChar, 1, False, False, False) + ',');
        Add('ADD ' + DeclareField('NEW_BLOB_NULL', ftFixedChar, 1, False, False, False));
        QueryReady;
      end;
    end;

    if not MyTableExists(UnQuotedIdentifier('RPL$CONFLICTS')) then
    begin
      with Query do
      begin
        Clear;
        Add('CREATE TABLE RPL$CONFLICTS (');
        Add(DeclareField('CODE', ftInteger, 0, True, True, True) + ',');
        Add(DeclareField('USER1', ftString, 50, False, False, False) + ',');
        Add(DeclareField('USER2', ftString, 50, False, False, False) + ',');
        Add(DeclareField('CONFLICT_DATE', ftDateTime, 0, False, False, False) + ',');
        Add(DeclareField('TABLE_NAME', ftString, 100, False, False, False) + ',');
        Add(DeclareField('REMOTE_TABLE_NAME', ftString, 100, False, False, False) + ',');
        Add(DeclareField('PRIMARY_KEY_VALUES', ftString, 500, False, False, False) + ',');
        Add(DeclareField('CHOSEN_USER', ftString, 50, False, False, False) + ',');
        Add(DeclareField('CHANGED_FIELDS1', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('CHANGED_FIELDS2', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('SQL1', ftMemo, 0, False, False, False) + ',');
        Add(DeclareField('SQL2', ftMemo, 0, False, False, False));
        Add(')');
//        Add('ENGINE=MyISAM');
      end;
      QueryReady;
      GrantRightsToTable('RPL$CONFLICTS');
    end
    else if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$CONFLICTS'), UnQuotedIdentifier('CHANGED_FIELDS1')) then begin
      with Query do
      begin
        Clear;
        Add('ALTER TABLE RPL$CONFLICTS ');
        Add('ADD ' + DeclareField('REMOTE_TABLE_NAME', ftString, 100, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$CONFLICTS ');
        Add('ADD ' + DeclareField('CHANGED_FIELDS1', ftMemo, 0, False, False, False));
        QueryReady;
        Add('ALTER TABLE RPL$CONFLICTS ');
        Add('ADD ' + DeclareField('CHANGED_FIELDS2', ftMemo, 0, False, False, False));
        QueryReady;
      end;
    end;

    if not MyTableExists(UnQuotedIdentifier('RPL$USERS')) then
    begin
			with Query do
      begin
        Clear;
        Add('CREATE TABLE RPL$USERS (');
        Add(DeclareField('LOGIN', ftString, 50, True, True, False) + ',');
        Add(DeclareField('PASSWRD', ftString, 100, False, False, False) + ',');
        Add(DeclareField('LIBELLE', ftString, 50, False, False, False) + ',');
        Add(DeclareField('CONDITION_VALUE', ftString, 200, False, False, False) + ',');
        Add(DeclareField('CONFIG_NAME', ftString, 100, False, False, False));
				Add(')');
//        Add('ENGINE=MyISAM');
      end;
      QueryReady;
      GrantRightsToTable('RPL$USERS');
      ScriptReady;
    end
    else if not Connection.DBAdaptor.FieldExists(UnQuotedIdentifier('RPL$USERS'), UnQuotedIdentifier('CONFIG_NAME')) then begin
      with Query do
      begin
        Clear;
        Add('ALTER TABLE RPL$USERS ');
        Add('ADD ' + DeclareField('CONFIG_NAME', ftString, 100, False, False, False));
      end;
      QueryReady;
    end;

		GetConnection.CommitRetaining;

    CreateProcedures;

	CheckExtraCustomMetadata;
  end;
  GetConnection.CommitRetaining;
  ScriptReady;

  finally
    slTables.Free;
  end;
end;

procedure TCcProcParams.Loaded;
begin
  inherited;
  LoadFields;
end;

procedure TCcConfig.DestroyQueries(Sender: TObject);
begin
  if Assigned(qTable) then
    qTable := nil;
  if Assigned(qProcedure) then
    qProcedure := nil;
end;

procedure TCcProcParams.LoadFields;
  procedure AddField(name: String; dt: TFieldType; s: Integer);
  begin
    with Fields.Add(name) do begin
      DataType := dt;
      Size := s;
    end;
  end;

begin
  AddField('PARAM_NAME', ftString, 50);
  AddField('PARAM_VALUE', ftString, 500);
  AddField('FIELD_DECL', ftString , 100);
	AddField('FIELD_TYPE', ftInteger, 0);
  AddField('FIELD_LENGTH', ftInteger, 0);
  AddField('FIELD_SCALE', ftInteger, 0);
end;

{ TCcConfigTables }

function TCcConfigTables.Add: TCcConfigTable;
begin
	result := inherited Add as TCcConfigTable;
end;

procedure TCcConfig.SetTables(const Value: TCcConfigTables);
begin
	FTables.Assign(Value);
end;

procedure TCcConfig.SetVersion(const Value: String);
begin

end;

{procedure TCcConfig.PrepareBatchImport(tableName, sqlCondition,
	destinationNode: String);
var
	I: Integer;
	keys: TCcKeyRing;
	cKeys, pkvals: String;
	q, qInsert: TCcQuery;
	slKeys: TStringList;
begin
	keys := TCcKeyRing.Create(GetConnection);
  keys.FailIfNoPK := FailIfNoPK;
	try
		keys.LoadKeys(TableName, '', '', '', '', '', '');

		if (sqlCondition = '') then
			sqlCondition := '0=0';

		slKeys := TStringList.Create;
		try
			for i:=0 to keys.Count -1 do
				slKeys.Add(keys[i].KeyName);
			cKeys := slKeys.CommaText;
		finally
			slKeys.Free;
		end;

		GetConnection.Connect;
		q := GetConnection.SelectQuery['TCcConfig.PrepareBatchImport_Select'];
		q.close;
		q.SQL.Text := 'select ' + cKeys + ' from ' + tableName + ' t where ' + sqlCondition;
		q.Exec;
		while not q.Eof do begin
			pkvals := keys.LoadKeysFromDataSet(tableName, q, '', '');
			qInsert := GetConnection.UpdateQuery['TCcConfig.PrepareBatchImport_Insert'];
				qInsert.close;
			qInsert.SQL.Text := 'execute procedure rpl$generate_log(:table_name, :pkvals, '''', '''', :dest_node, '''', '''')';
			qInsert.Param['table_name'].Value := tableName;
			qInsert.Param['pkvals'].Value := pkvals;
			qInsert.Param['dest_node'].Value := destinationNode;
			qInsert.Exec;
			q.Next;
		end;
		GetConnection.CommitRetaining;
	finally
		keys.Free;
	end;
end; }

constructor TCcConfigTables.Create(AOwner: TPersistent);
begin
  inherited Create(TCcConfigTable);
   FOwner := AOwner;
end;

function TCcConfigTables.FindTable(tableName: String): TCcConfigTable;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count -1 do begin
	 if (UpperCase(Trim(Item[I].TableName)) = UpperCase(Trim(tableName))) then begin
		 Result := Item[I] as TCcConfigTable;
		 Exit;
	 end;
	end;
end;

function TCcConfigTables.GetItem(Index: Integer): TCcConfigTable;
begin
	result := inherited Items[Index] as TCcConfigTable;
end;


function TCcConfigTables.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TCcConfigTable }

constructor TCcConfigTable.Create(Collection: TCollection);
begin
	inherited;
	FFieldsExcluded := TStringList.Create;
	FFieldsIncluded := TStringList.Create;
	FCondition := TStringList.Create;
	FInsertCondition := TStringList.Create;
	FUpdateCondition := TStringList.Create;
	FDeleteCondition := TStringList.Create;
  FSyncFieldNames := TCcStringList.Create;
  FSyncStatements:= TCcStringList.Create;
  FPKSyncStatements := TCcStringList.Create;
end;

destructor TCcConfigTable.Destroy;
begin
	FDeleteCondition.Free;
	FUpdateCondition.Free;
	FInsertCondition.Free;
	FFieldsExcluded.Free;
	FFieldsIncluded.Free;
	FCondition.Free;
  FSyncFieldNames.Free;
  FSyncStatements.Free;
  FPKSyncStatements.Free;
	inherited;
end;

function TCcConfigTable.GetDisplayName: string;
begin
  Result := TableName; 
end;

procedure TCcConfigTable.SetCondition(const Value: TStringList);
begin
	if Assigned(Value) then
		FCondition.Assign(Value);
end;

procedure TCcConfigTable.SetDeleteCondition(const Value: TStringList);
begin
	if Assigned(Value) then
		FDeleteCondition.Assign(Value);
end;

procedure TCcConfigTable.SetFieldsExcluded(const Value: TStringList);
begin
	if Assigned(Value) then
		FFieldsExcluded.Assign(Value);
end;

procedure TCcConfigTable.SetFieldsIncluded(const Value: TStringList);
begin
	if Assigned(Value) then
		FFieldsIncluded.Assign(Value);
end;

procedure TCcConfig.SetNodes(const Value: TStringList);
begin
	if Assigned(Value) then
		FNodes.Assign(Value);
end;

function TCcConfig.ExistsInList(str: String; list: TStringList) : Boolean;
var
  i:Integer;
begin
  Result := False;
  for I := 0 to list.Count-1 do
    if list[I] = str then begin
      Result := True;
      Exit;
    end;
end;

procedure TCcConfig.RemoveUser(cUserName: String);
var
  qDeleteUser: TCcQuery;
begin
  qDeleteUser := FConnection.UpdateQuery['TCcConfig_qDeleteUser'];
  qDeleteUser.Close;
  qDeleteUser.SQL.Text := 'delete from rpl$users where login = :user_name';
  qDeleteUser.Param['user_name'].Value := cUserName;
  qDeleteUser.Exec();
end;

procedure TCcConfig.AddUser(cUserName: String);
var
  qAddUser: TCcQuery;
begin
  qAddUser := FConnection.UpdateQuery['TCcConfig_qAddUser'];
  qAddUser.Close;
  qAddUser.SQL.Text := 'insert into RPL$USERS (config_name, login, condition_value) values (:config_name, :login, :login)';
  qAddUser.Param['login'].Value := cUserName;
  if (ConfigName <> '') then
	  qAddUser.Param['config_name'].Value := ConfigName
  else
	  qAddUser.Param['config_name'].Clear;
  qAddUser.Exec();
end;

procedure TCcConfig.CheckUsers;
var
  slExistingUsers: TStringList;
  cUserName: String;
  i: Integer;
  qUsers: TCcQuery;
begin
  slExistingUsers := TStringList.Create;
  qUsers := TCcQuery.Create(Self);
  try
    qUsers.SelectStatement := True;
    qUsers.Close;
    qUsers.Connection := FConnection;
    qUsers.SQL.Text := 'select login from RPL$USERS where %config_name';
    if (ConfigName <> '') then
      qUsers.Macro['config_name'].Value := 'config_name = ' + QuotedStr(ConfigName)
    else
      qUsers.Macro['config_name'].Value := '0=0';
    qUsers.Exec;
    while (not qUsers.Eof) do begin
      slExistingUsers.Add(Trim(qUsers.Field['LOGIN'].AsString));
      qUsers.Next;
    end;
    for i:=0 to slExistingUsers.Count-1 do begin
      cUserName := slExistingUsers[i];
      if (Nodes.IndexOf(cUserName) = -1) then
	RemoveUser(cUserName);
    end;
    for i:=0 to Nodes.Count-1 do begin
      cUserName := Nodes[i];
      if (slExistingUsers.IndexOf(cUserName) = -1) then
	AddUser(cUserName);
    end;
  finally
    qUsers.Free;
    slExistingUsers.Free;
    FConnection.CommitRetaining;
  end;
end;

procedure TCcConfig.GenerateConfig(lForceRecreateTriggers: Boolean; lCheckUsers: Boolean);

 	function GetIncludedFields (tableConfig: TCcConfigTable): String;
	var
		slIncludedFields: TStringList;
		I: Integer;
		nFieldIndex: Integer;
	begin
		Result := '';
		slIncludedFields := TStringList.Create;
		try
      if not TrackFieldChanges then begin
        if Trim(tableConfig.FieldsExcluded.Text) <> '' then begin
          slIncludedFields.Assign(FConnection.ListTableFields(tableConfig.TableName));
          for I := 0 to tableConfig.FieldsExcluded.Count -1 do begin
            nFieldIndex := slIncludedFields.IndexOf(tableConfig.FieldsExcluded[i]);
            if (nFieldIndex <> -1) then
              slIncludedFields.Delete(nFieldIndex);
          end;
        end else if Trim(tableConfig.FieldsIncluded.Text) <> '' then
          slIncludedFields.Assign(tableConfig.FieldsIncluded);
      end
      else
        slIncludedFields.Assign(tableConfig.FieldsIncluded);

			Result := slIncludedFields.CommaText;
		finally
			slIncludedFields.Free;
		end;
	end;

	function GetExcludedFields(tableConfig: TCcConfigTable): String;
	begin
		Result := '';
    if TrackFieldChanges then
      Result := tableConfig.FieldsExcluded.CommaText
	end;


	procedure EmptyLog(cTableName, cNode: String);
	var
		qEmptyLog: TCcQuery;
	begin
		qEmptyLog := FConnection.UpdateQuery['TCcConfig_qEmptyTable'];
		qEmptyLog.Close;
		qEmptyLog.SQL.Text := 'delete from rpl$log where %table_name and %user_name';

		if (cTableName <> '') then
			qEmptyLog.Macro['table_name'].Value := 'table_name = ' + QuotedStr(cTableName)
		else
			qEmptyLog.Macro['table_name'].Value := '0=0';

		if (cNode <> '') then
			qEmptyLog.Macro['user_name'].Value := 'login = ' + QuotedStr(cNode)
		else
			qEmptyLog.Macro['user_name'].Value := '0=0';
		qEmptyLog.Exec;
	end;

	procedure DropTriggers(cTableName: String);
	begin
//		if Assigned(OnCreateTriggers) then
//			OnCreateTriggers(Self, cTableName);
//		LogMessage('Suppression triggers pour la table ' + cTableName, cDest);

		RemoveTriggers(cTableName);
		EmptyLog(cTableName, '');
	end;

  function GetTrackedFields(tableConfig: TCcConfigTable): String;
  var
    slTrackedFields: TStringList;
  begin
    if TrackFieldChanges then begin
      slTrackedFields := TStringList.Create;
      try
        slTrackedFields.Assign(FConnection.ListTableFields(tableConfig.TableName));
        Result := slTrackedFields.CommaText;
      finally
        slTrackedFields.Free;
      end;
    end
    else
      Result := '';
  end;

	procedure CreateTriggers(cTableName: String);
	var
		qUpdateTable , qUpdateTableConf: TCcQuery;
		tableConfig : TCcConfigTable;
	begin
//		if Assigned(OnCreateTriggers) then
//			OnCreateTriggers(Self, cTableName);
//		LogMessage('Création des triggers pour la table ' + cTableName, cDest);

		tableConfig := Tables.FindTable(cTableName);
		qUpdateTable := FConnection.UpdateQuery['TCcConfig_qUpdateTable'];
		qUpdateTable.Close;
		qUpdateTable.SQL.Text := 'update RPL$TABLES set priority = :priority where table_name = :table_name';
		qUpdateTable.Param['table_name'].Value := cTableName;
		qUpdateTable.Param['priority'].Value := tableConfig.Priority;
		qUpdateTable.Exec();

		qUpdateTableConf := FConnection.UpdateQuery['TCcConfig_qUpdateTableConf'];
		qUpdateTableConf.Close;
		qUpdateTableConf.SQL.Text := 'update RPL$TABLES_CONFIG set repl_updates = :repl_updates, repl_inserts = :repl_inserts, repl_deletes = :repl_deletes,  '
		 + ' condition = :condition, update_condition = :update_condition , insert_condition = :insert_condition , delete_condition = :delete_condition, '
     + ' included_fields = :included_fields, excluded_fields = :excluded_fields, priority = :priority, TRACKED_FIELDS = :TRACKED_FIELDS, primary_key_sync = :primary_key_sync,  unique_key_sync = :unique_key_sync, unique_key_names = :unique_key_names'
		 + ' where table_name = :table_name and config_name = ' + QuotedStr(ConfigName);

		qUpdateTableConf.Param['table_name'].Value := cTableName;
		qUpdateTableConf.Param['repl_updates'].Value := 'Y';
		qUpdateTableConf.Param['repl_inserts'].Value := 'Y';
		qUpdateTableConf.Param['repl_deletes'].Value := 'Y';
		qUpdateTableConf.Param['condition'].Value := tableConfig.Condition.Text;
		qUpdateTableConf.Param['primary_key_sync'].Value := tableConfig.PKSyncStatements.SQLCommaText;
		qUpdateTableConf.Param['unique_key_sync'].Value := tableConfig.SyncStatements.SQLCommaText;
		qUpdateTableConf.Param['unique_key_names'].Value := tableConfig.SyncFieldNames.SQLCommaText;
		qUpdateTableConf.Param['delete_condition'].Value := tableConfig.DeleteCondition.Text;
		qUpdateTableConf.Param['insert_condition'].Value := tableConfig.InsertCondition.Text;
		qUpdateTableConf.Param['update_condition'].Value := tableConfig.UpdateCondition.Text;
		qUpdateTableConf.Param['included_fields'].Value := GetIncludedFields(tableConfig);
		qUpdateTableConf.Param['excluded_fields'].Value := GetExcludedFields(tableConfig);
		qUpdateTableConf.Param['priority'].Value := tableConfig.Priority;
    qUpdateTableConf.Param['TRACKED_FIELDS'].Value := GetTrackedFields(tableConfig);

		qUpdateTableConf.Exec();
		GenerateTriggers(cTableName);
	end;

	procedure CheckTriggers;
	var
		slExistingTables: TStringList;
		cTableName: String;
		i: Integer;
		qTables :TCcQuery;
		tableConfig: TCcConfigTable;
    lCreated: Boolean;
    cTriggerName: string;
    slTriggers: TStringList;

    function MyTriggerExists(TrigName:String):Boolean;
    var
      I:Integer;
    begin
      for i := 0 to slTriggers.Count - 1 do begin
        if FConnection.DBAdaptor.QuoteMetadata then
          Result := AnsiSameStr(slTriggers[i], TrigName)
        else
          Result := AnsiSameText(slTriggers[i], TrigName);
        if Result then
          Break;
      end;
    end;

	begin
		//Autocreate triggers, if needed
		slExistingTables := TStringList.Create;
    slTriggers := TStringList.Create;
    slTriggers.Assign(FConnection.ListTriggers);

		qTables := TCcQuery.Create(Self);
		try
      qTables.SelectStatement := True;
			qTables.Close;
			qTables.Connection := FConnection;
			qTables.SQL.Text := 'select t.* from RPL$TABLES_CONFIG t where t.config_name = :config_name';
      qTables.Param['config_name'].Value := ConfigName;
      qTables.Exec;

      RefreshDisplay;
      if not lForceRecreateTriggers then
      begin
        while (not qTables.Eof) do begin
          RefreshDisplay;
          cTableName := Trim(UpperCase(qTables.Field['TABLE_NAME'].AsString));
          lCreated := False;
          cTriggerName := qTables.Field['TRIG_BASE_NAME'].AsString;
          if TrackFieldChanges then begin
            if MyTriggerExists(cTriggerName) then begin
              lCreated := True;
              for I := 2 to qTables.Field['NUMBER_OF_TRIGGERS'].AsInteger do
                if MyTriggerExists(cTriggerName + IntToStr(i)) then
                  lCreated := True
                else begin
                  lCreated := False;
                  Break;
                end;
            end;
          end
          else begin
            if MyTriggerExists(cTriggerName + '_I') and MyTriggerExists(cTriggerName + '_U')
              and MyTriggerExists(cTriggerName + '_D') then
              lCreated := True;
          end;

          if lCreated then begin
            slExistingTables.Add(cTableName);
            tableConfig := Tables.FindTable(cTableName);
            if (tableConfig = nil) then
              //Delete existing triggers that should not be there
              DropTriggers(cTableName)
            else if (tableConfig.Condition.Text <> qTables.Field['CONDITION'].AsString)
                or (tableConfig.Priority <> qTables.Field['PRIORITY'].AsInteger)
                or (tableConfig.UpdateCondition.Text <> qTables.Field['UPDATE_CONDITION'].AsString)
                or (tableConfig.InsertCondition.Text <> qTables.Field['INSERT_CONDITION'].AsString)
                or (tableConfig.DeleteCondition.Text <> qTables.Field['DELETE_CONDITION'].AsString)
                or (tableConfig.SyncFieldNames.SQLCommaText <> qTables.Field['UNIQUE_KEY_NAMES'].AsString)
                or (tableConfig.SyncStatements.SQLCommaText <> qTables.Field['UNIQUE_KEY_SYNC'].AsString)
                or (tableConfig.PKSyncStatements.SQLCommaText <> qTables.Field['PRIMARY_KEY_SYNC'].AsString)
                or (GetIncludedFields(tableConfig) <> qTables.Field['INCLUDED_FIELDS'].AsString)
                or (GetTrackedFields(tableConfig) <> qTables.Field['TRACKED_FIELDS'].AsString) then
                //Delete the table from the list of existing tables so as to force recreation of triggers
                slExistingTables.Delete(slExistingTables.IndexOf(cTableName));
          end;

          qTables.Next;
        end;
      end;

			//Create triggers that are missing
			for i:=0 to Tables.Count-1 do begin
				cTableName := Tables[i].TableName;
				if (not ExistsInList(cTableName, slExistingTables)) then
					CreateTriggers(cTableName);
        RefreshDisplay;
  			FConnection.CommitRetaining;
			end;
		finally
			qTables.Free;
      slTriggers.Free;
			slExistingTables.Free;
			FConnection.CommitRetaining;
		end;
	end;

begin
	if (Tables.Count > 0) then
		CheckTriggers;

	if lCheckUsers and (Nodes.Count > 0) then
		CheckUsers;
end;

procedure TCcConfig.SetConfigName(const Value: String);
begin
	if (Assigned(FConnection) and FConnection.Connected) then
    raise Exception.Create('Configuration name must be set before connecting to the database')
  else
		FConfigName := UpperCase(Trim(Value));
end;

procedure TCcConfigTable.SetInsertCondition(const Value: TStringList);
begin
	if Assigned(Value) then
		FInsertCondition.Assign(Value);
end;

procedure TCcConfigTable.SetPKSyncStatements(const Value: TCcStringList);
begin
	if Assigned(Value) then
		FPKSyncStatements.Assign(Value);
end;

procedure TCcConfigTable.SetSyncFieldNames(const Value: TCcStringList);
begin
	if Assigned(Value) then
		FSyncFieldNames.Assign(Value);
end;

procedure TCcConfigTable.SetSyncStatements(const Value: TCcStringList);
begin
	if Assigned(Value) then
		FSyncStatements.Assign(Value);
end;

procedure TCcConfigTable.SetUpdateCondition(const Value: TStringList);
begin
	if Assigned(Value) then
		FUpdateCondition.Assign(Value);
end;

{ TCcStringList }

function TCcStringList.GetSQLCommaText: String;
var
  I: Integer;
begin
  for I := 0 to Count-1 do begin
    if Result <> '' then Result := Result + ';';
    Result := Result + QuotedStr(Strings[i]);
  end;
end;

procedure TCcStringList.SetSQLCommaText(const Value: String);
var
  I: Integer;
  txt: String;
begin
  while True do
  begin
    txt := ParseKeyValues(Value, I, False);
    if txt <> '' then
      Add(txt)
    else
      Break;
  end;
end;

end.

