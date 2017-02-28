unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, CheckLst, ComCtrls, CcProviders, CcInterbaseConn,
	CcConf, CcReplicator, CcDB, ExtCtrls, Buttons, DB;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    clbTables: TCheckListBox;
    clbTablesRemote: TCheckListBox;
    TabSheet2: TTabSheet;
    Replicator: TCcReplicator;
    RemoteConfig: TCcConfig;
    LocalConfig: TCcConfig;
    tsIntroduction: TTabSheet;
    Label2: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Image2: TImage;
    Label7: TLabel;
    Label8: TLabel;
    Image3: TImage;
    Label9: TLabel;
    Label10: TLabel;
    Image4: TImage;
    Label11: TLabel;
    Label12: TLabel;
    Image5: TImage;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    btConnect: TSpeedButton;
    btDisconnect: TSpeedButton;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label3: TLabel;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    btConfigure: TBitBtn;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    btReplicate: TBitBtn;
    memLog: TMemo;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Image9: TImage;
    Image10: TImage;
    Label27: TLabel;
    procedure Label21Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
    procedure btConfigureClick(Sender: TObject);
    procedure btReplicateClick(Sender: TObject);
    procedure ReplicatorException(Sender: TObject; e: Exception);
    procedure ReplicatorReplicationResult(Sender: TObject);
    procedure ReplicatorProgress(Sender: TObject);
    procedure ReplicatorConnectionLost(Sender: TObject;
      Database: TCcConnection);
    procedure ReplicatorConflict(Sender: TObject);
    procedure ReplicatorResolveConflict(Sender: TObject;
      var Conflict: TConflictRecord);
    procedure FormShow(Sender: TObject);
    procedure ReplicatorRowReplicated(Sender: TObject; TableName: string;
      Fields: TCcMemoryFields; QueryType: TCcQueryType);
    procedure ReplicatorReplicationError(Sender: TObject; e: Exception;
      var CanContinue, SkipToRemote: Boolean);
  private
    LocalConnection, RemoteConnection: TCcConnection;
    procedure Init;
    procedure LoadTables;
    function VariantToStr(v: Variant): String;
  public
    { Public declarations }
  end;

var
	MainForm: TMainForm;

implementation

uses ShellAPI;

{$R *.dfm}

procedure TMainForm.Label21Click(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar('http://copycat.fr/wordpress/discussions/'), '', '', SW_SHOWNORMAL);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
//	LocalConnection :=
//	RemoteConnection :=

	Init;
end;

procedure TMainForm.Init;
begin
	LocalConfig.Connection := LocalConnection;
	RemoteConfig.Connection := RemoteConnection;

	//The Nodes property defines the list of nodes towards which the current database
	//is to replicate its tables
	//
	//Here we are hard-coding the list of nodes to make the local node send its data to
	//the remote node and vice versa, thus enabling bi-directional replication between the two databases
	//
	//If we set an empty list of nodes for one of the two databases, the changes made
	//to that database would be sent nowhere : thus, we would have a one-way replication config
	//
	//Lastly, if you were to set several nodes for one of the databases (one per line),
	//the changes made to that database would be dispatched to all the databases in the list.
	//Such a configuration can make sense for instance for the central server hub in a star-shaped configuration

	LocalConfig.Nodes.Text := 'REMOTE';
	RemoteConfig.Nodes.Text := 'LOCAL';

	//Here we name the two nodes LOCAL and REMOTE
	//You can use any name that has a meaning to you, but it must match the
	//names used above in the list of nodes

	Replicator.LocalNode.Connection := LocalConnection;
	Replicator.LocalNode.Name := 'LOCAL';
	Replicator.RemoteNode.Connection := RemoteConnection;
	Replicator.RemoteNode.Name := 'REMOTE';
end;

procedure TMainForm.btConnectClick(Sender: TObject);
begin
	//We call the Connect method on the TCcConfig component because it doesn't merely connect
	//to the database, it also creates the basic meta-data (the RPL$... tables) in your database
	//that are needed before replication can be configured for any tables
	try
		LocalConfig.Connect;
	except on E:Exception do begin
      raise Exception.Create('Error connecting to local database : ' + E.Message);
		end;
	end;
	try
		RemoteConfig.Connect;
	except on E:Exception do begin
      raise Exception.Create('Error connecting to remote database : ' + E.Message);
		end;
	end;

	//Load the list of database tables into each of the two CheckListBoxes
	LoadTables;
end;

procedure TMainForm.LoadTables;
begin
  clbTables.Items.Assign(LocalConnection.ListTables);
	clbTablesRemote.Items.Assign(RemoteConnection.ListTables);
end;

procedure TMainForm.btDisconnectClick(Sender: TObject);
begin
	LocalConfig.Disconnect;
	RemoteConfig.Disconnect;
end;


procedure TMainForm.btConfigureClick(Sender: TObject);
var
	i: Integer;
	configTable: TCcConfigTable;
begin
	if (not LocalConnection.Connected) or (not RemoteConnection.Connected) then
		raise Exception.Create('You need to connect to the databases before you can configure them for replication');

	//Load the list of tables to be replicated from the local database...
	LocalConfig.Tables.Clear;
	for i := 0 to clbTables.Items.Count -1 do begin
		if clbTables.Checked[I] then begin
			configTable := LocalConfig.Tables.Add;
			configTable.TableName := clbTables.Items[I];
		end;
	end;

	//...and same thing for the remote database
	RemoteConfig.Tables.Clear;
	for i := 0 to clbTablesRemote.Items.Count -1 do begin
		if clbTablesRemote.Checked[I] then begin
			configTable := RemoteConfig.Tables.Add;
			configTable.TableName := clbTablesRemote.Items[I];
		end;
	end;

	//The GenerateConfig method checks the configuration in the database, compares it with
	//the configuration set in the component (in the Nodes and Tables properties)
	//and adjusts the database to match the configuration set in the component
	//In practice, that means that GenerateConfig will :
	//   - Create triggers and nodes if they are missing in the database
	//   - Remove them from the database if they have been removed from the component
	//   - Drop and recreate triggers that were created using different options
	//   - Do nothing if everything is up to date
	//You can therefore call GenerateConfig systematically (at program startup for example)
	//to ensure that the database is always up-to-date. If nothing has changed, the database
	//triggers and nodes will not be modified.
	//
	//Please note that there are variousoptions that can be set for each table, which we do not use
	//in this demo. Check out the properties of the TCcConfigTable objects created above to see
	//what the different possibilites are. If you change any of these options after creating the triggers
	//for a certain table, GenerateConfig will drop and recreate the triggers for that table to make
	//sure that they take the new options into account.

	LocalConfig.GenerateConfig;
	RemoteConfig.GenerateConfig;

	ShowMessage('Databases configured successfully!');
end;

procedure TMainForm.btReplicateClick(Sender: TObject);
begin
	//The databases must both be disconnected before starting replication with the
	//TCcReplicator.Replicate method. The Replicate method connects to the databases
	//automatically, and then initializes some parameters that are important
	//for replication to function correctly.

	if LocalConnection.Connected then
		LocalConnection.Disconnect;
	if RemoteConnection.Connected then
		RemoteConnection.Disconnect;

	Replicator.Replicate;
end;

function TMainForm.VariantToStr(v: Variant): String;
begin
  if VarIsNull(v) or VarIsClear(v) then
    Result := '<null>'
  else
    Result := v;
end;

procedure TMainForm.ReplicatorException(Sender: TObject; e: Exception);
begin
	//This event is fired only for exceptions that occur outside the scope of
	//the replication of one particular row, for database connection losses,
	//if you set CanContinue := False in the OnReplicationError event, or
	//if you call TCcReplicator.AbortReplication.
	//
	//This is an unrecoverable exception, so replication of the following rows does
	//not continue after this point.

	memLog.Lines.Add('Replication aborted with the following error:');
	memLog.Lines.Add(e.Message);
end;

procedure TMainForm.ReplicatorReplicationError(Sender: TObject; e: Exception;
  var CanContinue, SkipToRemote: Boolean);
var
	i:Integer;
	primaryKeys: String;
begin
	//This event fires when a row cannot be replicated, for whatever reason
	//Typically, this is not an unrecoverable exception, but rather a problem stopping
	//the row from being able to be replicated (a foreign key violation for instance)
	//By default, replication of the following rows will continue, but you can choose to abort
	//replication by setting the CanContinue parameter to false

  //For explanations about the code below, see the RowReplicated event above
	for i:=0 to Replicator.Log.Keys.Count-1 do begin
		if primaryKeys <> '' then
			primaryKeys := primaryKeys + ', ';
		primaryKeys := Replicator.Log.Keys[I].KeyName + ' = ' + String(Replicator.Log.Keys[I].KeyValue);
	end;

	memLog.Lines.Add('Row replication failed for table "' + Replicator.Log.TableName + '" (' + PrimaryKeys + ')');
	memLog.Lines.Add(e.Message);
end;

procedure TMainForm.ReplicatorReplicationResult(Sender: TObject);
begin
	//This event is fired at the end of a replication cycle, when the result of the
	//replication cycle is known. You can access that result using the TCcReplicator.LastResult
	//property. See documentation of that property for the list of different possible values.

	memLog.Lines.Add('Replication finished : '
		+ IntToStr(Replicator.LastResult.RowsReplicated) + ' rows replicated, '
		+ IntToStr(Replicator.LastResult.RowsConflicted) + ' conflicts, '
		+ IntToStr(Replicator.LastResult.RowsErrors) + ' rows with errors');
end;

procedure TMainForm.ReplicatorProgress(Sender: TObject);
begin
	//This event is fired at every possible occasion, in order to give the user interface
	//a chance to refresh, if applicable.
	//It can also be used to show the current progress, using a TProgressBar for instance.
	//To do that, you can use the TCcReplicator.Log.LineCount and TCcReplicator.Log.CurrentLine
	//properties.

	//Here we just call Application.ProcessMessages to keep the GUI responsive
	Application.ProcessMessages;
end;

procedure TMainForm.ReplicatorConnectionLost(Sender: TObject;
	Database: TCcConnection);
var
	nodeName: String;
begin
	//As its name implies, this event is fired when a connection is lost to one of
	//the databases. In this example, we will simply compare the Database connection
	//object that's passed as a parameter with the LocalNode and RemoteNode properties
	//to see which node just lost its connection

	if Database = Replicator.LocalNode.Connection then
		nodeName := Replicator.LocalNode.Name
	else
	  nodeName := Replicator.RemoteNode.Name;

	memLog.Lines.Add('Connection lost to ' + nodeName + ' database');
end;

procedure TMainForm.ReplicatorConflict(Sender: TObject);
begin
	//This event is fired after a conflict has been detected and logged to RPL$CONFLICTS
	//In order to resolve the conflict, you need to set the CHOSEN_USER field of that
	//table to the name of the node whose version of the row you would like to keep.
	//Have a look at the OnResolveConflict event below, as an alternative way to
	//handle conflicts.
	//
	//There's more information about conflict resolution in the documentation,
	//here we'll just log the conflict to the GUI...

	memLog.Lines.Add('Conflict detected!')
end;

procedure TMainForm.ReplicatorResolveConflict(Sender: TObject;
  var Conflict: TConflictRecord);
begin
	//This event is fired when a conflict has been detected, but before the OnConflict
	//event above, and before anything is put into RPL$CONFLICTS.
	//This event can be very useful if you have any automatic conflict resolution
	//policies that you would like to implement. To resolve the conflict, simply set
	//the ChosenNode property of the Conflict parameter to the name of the node
	//whose version of the row you would like to keep. Replication will then proceed as
	//normal.
	//
	//If you don't do anything here, the conflict will get recorded in RPL$CONFLICTS
	//and the OnConflict event above will be fired.
	//The conflict will stay in RPL$CONFLICTS until it is resolved through human intervention.
end;

procedure TMainForm.ReplicatorRowReplicated(Sender: TObject; TableName: string;
  Fields: TCcMemoryFields; QueryType: TCcQueryType);
var
	i: Integer;
	primaryKeys: String;
begin
	//This event is fired after the current row has been successfully replicated
	//The Fields parameter contains the full list of fields that were replicated
	//along with their values

	//This event is often useful for logging the replication process
	//Here's an example of how to extract information about the current row
	//in order to display it in the log :

	//The TCcReplicator.Log property contains all the information about the row being
	//replicated (taken from RPL$LOG). You can use this property from within any of
	//the event handlers that are fired during replication.
	//You can find more information about this table by looking at the class documentation of TCcLog.

	//The TCcLog.Keys property contains the list of primary key fields with their values
	//These values are extracted from the RPL$LOG.PRIMARY_KEY_VALUES field,
	//but since primary keys may be composed of several fields (multi-segment PKs),
	//the RPL$LOG.PRIMARY_KEY_VALUES field contains the values of all the PK fields
	//bunched together. The TCcLog.Keys property parses this field value in order to give easy access
	//to the list of PK fields with their values

	//Get the list of PK fields with their names and values, separated by commas
	for i:=0 to Replicator.Log.Keys.Count-1 do begin
		if primaryKeys <> '' then
			primaryKeys := primaryKeys + ', ';
		primaryKeys := primaryKeys + Replicator.Log.Keys[I].KeyName + ' = ' + VariantToStr(Replicator.Log.Keys[I].KeyValue);
	end;

	//Add this information to the log
	memLog.Lines.Add('Row replicated successfully : table "' + TableName + '" (' + PrimaryKeys + ')');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
	if not Assigned(LocalConnection) or not Assigned(RemoteConnection) then begin
		ShowMessage('You need to set the LocalConnection and RemoteConnection variables to point to your database connection components in the FormCreate method!');
		Application.Terminate;
	end;
	PageControl.ActivePage := tsIntroduction;
end;

end.
