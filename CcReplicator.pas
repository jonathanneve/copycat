// CopyCat replication suite<p/>
// Copyright (c) 2015 Microtec Communications<p/>
// For any questions or technical support, contact us at contact@copycat.fr
unit CcReplicator;

{$I CC.INC}

interface

uses
  Classes, DB, SysUtils, CCat, CcProviders, CcKeys, SyncObjs, CcDB, {$IFDEF FPC}fptimer{$ELSE}
{$IFDEF MSWINDOWS}{$IFDEF CC_D2K14}Vcl.ExtCtrls{$ELSE}ExtCtrls{$ENDIF}{$ELSE}FMX.Types{$ENDIF}{$ENDIF};

type
  TCcTraceType = (ttNoTrace, ttLocalOnly, ttRemoteOnly, ttEachSeparately,
    ttBothTogether);
  TCcQueryResult = (qrOK, qrError, qrNoneAffected, qrCanceled);

  TCcReplicationResultType = (rtNone, rtReplicatorBusy,
    rtErrorCheckingConflicts, rtErrorLoadingLog, rtException,
    rtNothingToReplicate, rtReplicated, rtErrorConnectLocal, rtErrorConnectRemote, rtReplicationAborted);

  TCcReplicationResult = record
    ResultType: TCcReplicationResultType;
    // Outcome of the replication session.<p/>
    // rtReplicated indicates that the replication session was able to proceed (though some rows may not have replicated)<p/>
    // rtNothingToReplicate indicates that the replication session could not proceed because there was no data to replicate <p/>
    // rtReplicatorBusy indicates that replication could not proceed because another call to Replicate was already in progress<p/>
    // The other error codes indicate errors that occured during replication setup, not while replicating data. In such cases,
    // the error message is reported in ExceptionMessage
    ExceptionMessage: string; // Error message, if applicable
    RowsReplicated: Integer; // Number of rows successfully replicated
    RowsConflicted: Integer; // Number of conflictual rows detected
    RowsErrors: Integer;
    // Number of rows that could not be replicated because of errors.
    // Such rows will be left in RPL$LOG and will be attempted again upon next replication
  end;

  TCcSyncDirection = (sdBoth, sdRemoteToLocal, sdLocalToRemote);

  // TConflictRecord is a record for holding information about a conflict that has occured.
  TConflictRecord = record
    Node1: string; // Name of first replication node in the conflict
    Node2: string; // Name of second replication node in the conflict

    ChangedFields1: TStringList;
    ChangedFields2: TStringList;
    ConflictingFields: TStringList;

    FirstOperationDate1: TDateTime;
    // Date and time of earliest change made in the first node to the conflicting record.
    FirstOperationDate2: TDateTime;
    // Date and time of earliest change made in the second node to the conflicting record.

    LastOperationDate1,
    // Date and time of latest change made in the first node to the conflicting record.
    LastOperationDate2: TDateTime;
    // Date and time of latest change made in the second node to the conflicting record.

    NumChanges1,
    // Number of changes made to the conflicting record in the first node
    NumChanges2: Integer;
    // Number of changes made to the conflicting record in the second node
    ChosenNode: string; // Name of the node chosen to resolve the conflict.
    // Empty by default, but can be set in the OnResolveConflict event.
  end;

  TConflictEvent = procedure(Sender: TObject; var Conflict: TConflictRecord)
    of object;

    // Type of SQL query. Provided in the OnQueryDone event.
  TCcQueryType = (qtSelect, // SELECT query
    qtDelete, // DELETE query
    qtUpdate, // UPDATE query
    qtInsert // INSERT query
    );
  TCcQueryTypes = set of TCcQueryType;
  TCcCanReplicateEvent = procedure(Sender: TObject; TableName: string;
    Fields: TCcMemoryFields; QueryType: TCcQueryType; var CanReplicate: Boolean;
    var RetryLater: Boolean) of object;
  TCcReplicationEvent = procedure(Sender: TObject; TableName: string;
    Fields: TCcMemoryFields; var ReplicateRow: Boolean;
    var AbortAndTryLater: Boolean) of object;
  TCcAfterReplicationEvent = procedure(Sender: TObject; TableName: string; Fields: TCcMemoryFields; QueryType: TCcQueryType) of object;
  TCcReplicationQueryEvent = procedure(Sender: TObject; TableName: string;
    Fields: TCcMemoryFields; QueryType: TCcQueryType; var Done: Boolean)
    of object;
  TCcReplicateRowErrorEvent = procedure(Sender: TObject; TableName: string;
    Fields: TCcMemoryFields; E: Exception; var Retry: Boolean; var ShouldAbortReplication: Boolean) of object;
  TCcQueryEvent = procedure(Sender: TObject; QueryType: TCcQueryType;
    Rows: Integer) of object;
  TGenReplEvent = procedure(Sender: TObject; Name: string; NewValue: Variant)
    of object;
  TCcFieldValueEvent = procedure(TableName: string; Fields: TCcMemoryFields;
    FieldName: string; var Value: Variant) of object;

  TCcCustomLog = class;
  TCcReplicator = class;

  // Description:
  // Fires the OnPeriod event every Frequency seconds after Start has been called.
  TPeriodicity = class(TPersistent)
  private
    FFrequency: Integer;
    FOwner: TComponent;
    Timer: {$IFDEF FPC}TFPTimer{$ELSE}TTimer{$ENDIF};
  protected
    fEnabled: Boolean;
    OnPeriod: TCcNotifyEvent;
  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    { ******************************
      * Summary:                   *
      * Timer frequency in seconds *
      ****************************** }
    property Frequency: Integer read FFrequency write FFrequency;
  end;

  // Summary:
  // Defines automatic replication properties.
  // Description:
  // Use TAutoRepl to automatically launch a replication session periodically, if
  // there is none already in progress.<p/>
  // <p/>
  // To enable automatic replication, set Enabled to True, and call the Start method
  // when you are ready to start the timer. Automatic replication can be temporarily
  // disabled at any time, by calling the Stop method.
  // See Also:
  // TCcReplicator.AutoReplicate
  TAutoRepl = class(TPeriodicity)
  published
    // Summary:
    // Enables or disables automatic replication.
    // Description:
    // Set Enabled to enable or disable automatic replication is a general way. If
    // auto-replication is enabled, the Start method must still be called however, in
    // order to start the timer.
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

  // Summary:
  // Represents auto-commit properties.
  // Description:
  // The TCcAutoCommit object allows to periodically commit the transactions after
  // Frequency seconds of replication. Use the CommitType property to select the type
  // of commit that you want to perform.<p/>
  // <p/>
  // It is not necessary to call the Start nor Stop methods, as this is handled
  // internally be TCcReplicator. Setting CommitType to ctNone means that no automatic
  // commit will be performed.
  // See Also:
  // TCcReplicator.AutoCommit
  TCcAutoCommit = class(TPeriodicity)
  private
    FCommitType: TCcCommitType;
    procedure SetCommitType(CommType: TCcCommitType);
  published
    // Summary:
    // Type of automatic commit to perform.
    // Description:
    // Set CommitType to determine how you wish to commit the replication transactions.
    property CommitType: TCcCommitType read FCommitType write SetCommitType;
  end;

  TCcCustomConflictMgr = class(TComponent)
  protected
    FReplicator: TCcReplicator;
    procedure LogConflict(Conflict: TConflictRecord); virtual; abstract;
    procedure CheckConflicts; virtual; abstract;
    procedure DeleteLogValues(TableName, cRemoteTableName: String;
      cPrimaryKeyValues: String; ConflictingFields: TStringList;
      ChosenNode: String); virtual; abstract;
    procedure DeleteConflictLog(TableName, RemoteTableName: string;
      cPrimaryKeyValues, ChosenUser: string; ReplicationStateCondition: String); virtual; abstract;
    procedure ResolveConflict(cTableName, cRemoteTableName, cPrimaryKeyValues
      : String; Conflict: TConflictRecord; ReplicationStateCondition: String); virtual; abstract;
  end;

  TCcGenerator = class
  protected
    Table, Field: String;
    OldValue, NewValue: Variant;
  end;

  // Summary:
  // Represents a replication node.
  // Description:
  // A TCcNode object holds a reference to the TCcConnection object to be used for
  // connecting to this node, as well as the name of the node.
  // See Also:
  // <link Replication nodes>
  TCcNode = class(TPersistent)
  private
    FReplicator: TCcReplicator;
    FPrevConnLost: TCcExceptionNotifyEvent;
    FName: string;
    FPassword: string;
    FConnection: TCcConnection;
    FConfigName: string;
    procedure SetConnection(const Value: TCcConnection);
    function GetCurrentTableName: string;
  public
    procedure FindConfigName(node: TCcNode);

    // Name of the CopyCat configuration that this node is attached to in the other node
    property ConfigName: string read FConfigName;
    constructor Create(AOwner: TCcReplicator);
  published
    property Connection: TCcConnection read FConnection write SetConnection;

    // Name of the node. This corresponds to RPL$USERS.LOGIN
    property Name: string read FName write FName;

    // Summary: Password to identify and authenticate the node on the remote server
    // Description: This password is used when connecting to a Java or PHP CopyCat server.
    // It is not a database connection password
    property Password: string read FPassword write FPassword;

    property CurrentTableName: string read GetCurrentTableName;
  end;

  // Summary:
  // Holds the two necessary TCcNode objects: LocalNode and RemoteNode.
  // See Also:
  // <link Replication nodes>
  TCcNodes = class(TPersistent)
  private
    FLocalNode: TCcNode;
    FRemoteNode: TCcNode;
  public
    constructor Create(AOwner: TCcReplicator);
    destructor Destroy; override;
  published
    // Summary:
    // Replication name of the local node.
    property LocalNode: TCcNode read FLocalNode write FLocalNode;
    // Summary:
    // Replication name of the remote node.
    property RemoteNode: TCcNode read FRemoteNode write FRemoteNode;
  end;

  ECcReplicationAborted = class(Exception)
  private
    FReplicator: TCcReplicator;
  public
    constructor Create(repl: TCcReplicator);
    property Replicator: TCcReplicator read FReplicator;
  end;

  // Summary:
  // TCcReplicator provides the core replication functionality of CopyCat.
  // Description:
  // \ \ <p/>
  // <b>How to use TCcReplicator:</b>
  // 1. Fill in the Nodes.LocalNode.Connector and Nodes.RemoteNode.Connector
  // properties with the connectors of your choice for accessing the local and remote
  // databases
  // 2. Provide the names of the local and remote nodes via the Nodes.LocalNode.Name
  // and Nodes.RemoteNode.Name properties. See Nodes for more information.
  // 3. Call the Replicate method.
  // 4. Optionnaly, implement a few of the events, in order to provide user feedback
  // at every stage of the replication process.
  // 5. Optionnaly, you may set up automatic timer-based replication, using the
  // AutoReplicate property.
  //
  // <align justify>
  // If an exception occurs during replication, an OnReplicationError event is fired,
  // where you can choose to either abort the whole replication process, or to
  // continue trying to replicate the next record. The default behaviour depends on
  // the AbortOnError property.<p/>
  // If an exception occurs at any other point (e.g. during database connection), an
  // OnException event is fired, and replication is aborted.<p/>
  // TCcReplicator frequently fires the OnProgress event, in order to allow the
  // application to refresh user interface, process any pending user messages, and
  // possibly, update a progress bar. It is also possible to abort the replication at
  // any time, using the AbortReplication method. Refer to the provided example
  // projects for an illustration of the use of most of the events and properties.
  // </align>
  // See Also:
  // AbortOnError, OnProgress
  TCcReplicator = class(TComponent)
  private
    FConfigurationName: string;
    FFieldList: TStringList;
    FTableFieldList: TStringList;
    CS: TCriticalSection;
    ConnectionCS: TCriticalSection;
    FGenerators: TStringList;
    FBusy: Boolean;
    FClosing: Boolean;
    FFinished: Boolean;
    lAutoCommitPending: Boolean;
    FCommitOnFinished: TCcCommitType;
    FAbortOnError: Boolean;
    FKeepConnection: Boolean;
    FSelect: TCcMemoryTable;
    FLog: TCcCustomLog;
    FAutoReplicate: TAutoRepl;
    FAutoCommit: TCcAutoCommit;
    FOnFinished: TCcNotifyEvent;
    FOnRowReplicating: TCcReplicationEvent;
    FOnRowReplicated: TCcAfterReplicationEvent;
    FOnConflict: TCcNotifyEvent;
    FOnEmptyLog: TCcNotifyEvent;
    FOnTableBegin: TCcNameEvent;
    FOnTableEnd: TCcNameEvent;
    FOnGetFields: TCcGetFieldsEvent;
    FOnReplicationError: TCcErrorEvent;
    FOnException: TCcExceptionEvent;
    FOnGenReplError: TCcErrorEvent;
    FOnGenReplicating: TCcNotifyEvent;
    FOnKeySynchronized: TGenReplEvent;
    FOnReplicationAborted: TCcNotifyEvent;
    FOnConnectionLost: TCcLostConnectEvent;
    FOnAutoCommit: TCcNotifyEvent;
    FOnConnectLocal: TCcNotifyEvent;
    FOnConnectRemote: TCcNotifyEvent;
    FOnDisconnect: TCcNotifyEvent;
    FOnCommit: TCcNotifyEvent;
    FOnProgress: TCcNotifyEvent;
    FOnReplicateProc: TCcNameEvent;
    FConflictMgr: TCcCustomConflictMgr;
    FNodes: TCcNodes;
    FHarmonizeFields: Boolean;
		FOnQueryDone: TCcQueryEvent;
		FOnLogLoaded: TCcNotifyEvent;
		FBeforeReplicate: TCcNotifyEvent;
		FOnResolveConflict: TConflictEvent;
		FOnCanReplicate: TCcCanReplicateEvent;
		FOnRowBeforeReplicate: TCcNotifyEvent;
		FRecordChunks: Integer;
		FOnReplicationResult: TNotifyEvent;
		FTableMapping: TStrings;
		FAutoPriority: Boolean;
		FAutoClearMetadata: Boolean;
		FTableFields: TStringList;
		FTraceEvents: TCcTraceType;
		FOnRowReplicatingQuery: TCcReplicationQueryEvent;
		FOnGetExtraFieldValue: TCcFieldValueEvent;
		FExtraFields: TStringList;
		FOnRowReplicatingError: TCcReplicateRowErrorEvent;
		FTrimCharFields: Boolean;
		FEmptyStringsToNull: Boolean;
		FOnRowReplFinishing: TCcAfterReplicationEvent;
		FUsePKSynchronization: Boolean;
		FFailIfNoPK: Boolean;
		FReplicateOnlyChangedFields: Boolean;
		FDirection: TCcSyncDirection;
		FMergeChangedFieldsOnConflict: Boolean;
		FKeepRowsInLog: Boolean;
		slInconsistentDeletes: TStringList;
		FTrackInconsistentDeletes: Boolean;
    LastRowAborted: Boolean;
		procedure HarmonizeLists(slFirst, slSecond: TStringList);
		function CanReplicate(QueryType: TCcQueryType; TableName: string;
      Fields: TCcMemoryFields; var RetryLater: Boolean): Boolean;
    procedure SetBusy(Value: Boolean);
    function GetBusy: Boolean;
    procedure ClearGeneratorList;
    procedure OnAutoReplicateTimer(Sender: TObject);
    procedure OnAutoCommitTimer(Sender: TObject);
    procedure DoAutoCommit;
		function ReplicateRecord(OrigTableName, DestTableName, OrigSQLConditions,
      DestSQLConditions: string; lCrushAll: Boolean): TCcQueryType;
    procedure ReplicateProcedure(conn: TCcConnection);
    procedure ReplicateGenerators(TableName: string);
    procedure GetFields(lHarmonizeFields: Boolean; cTableName: string);
    function GetFieldsSQL(slFields: TStringList; cOperationType: string;
      node: TCcNode): string;
    function GetFieldValue(lLocal: Boolean; cFieldName: string;
      DB: TCcConnection): Variant;
    function BuildGenUpdateFields: string;
    function GetGenerator(TableName, cKeyName: string): Variant;
    function SignalError(E: Exception): Boolean;
    procedure SignalReplicationFinished;
    function GetLocalNode: TCcNode;
    function GetRemoteNode: TCcNode;
    procedure HarmonizeFieldList(Query: TCcMemoryTable; slList: TStringList);
    procedure SetKeepConnection(Value: Boolean);
    procedure SetCommitOnFinished(Value: TCcCommitType);
    function DoReplicate: Boolean;
    procedure DoGetFields(conn: TCcConnection; cTableName: string;
      slFields: TStringList);
    procedure DoCommitOnFinished;
    procedure GetGenUpdateFields;
    procedure GetGenUpdateParams(qQuery: TCcQuery);
    function GetLocalDB: TCcConnection;
    function GetRemoteDB: TCcConnection;
    procedure ClearLastResult;
    procedure LogError(E: Exception);
    procedure SetTableMapping(const Value: TStrings);
    function CheckRowExists(conn: TCcConnection;
      TableName, Condition: string): Boolean;
    procedure SetAutoPriority(const Value: Boolean);
    procedure SetRecordChunks(const Value: Integer);
    function GetTableFields(TableName: string): TStringList;
    function BuildWhere(DB: TCcConnection): string;
    procedure UpdateReplicationFields;
    procedure RowReplicatingQuery(qt: TCcQueryType; var lQueryDone: Boolean);
    function GetFieldType(cFieldName: string): TFieldType;
    function ValueByKey(list: TStrings; key: string): string;
    function KeyByValue(list: TStrings; Value: string): string;
    function GetVersion: String;
    procedure SetVersion(Value: String);
    procedure UpdateRplLogWithNewKeys;
    function SanitizeParamName(paramName: String): String;

  protected
    FReplicationQueryContext: string;
    FLogErrors: Boolean;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure LogConflict(var Conflict: TConflictRecord);
    procedure ConnectionLost(Sender: TObject; var RaiseException: Boolean);

    property TableFields[TableName: string]: TStringList read GetTableFields;
  public
    // LastResult holds the result of the last call to Replicate.
    // See also: OnReplicationResult
    LastResult: TCcReplicationResult;

    procedure RefreshDisplay;

    // If this property is set, empty string values are set to null for field types other than ftString
    // This is useful only for database types (such as SQLite) with loose data type management, such that
    // it may be possible to get a string value out of an integer or date column. If EmptyStringsToNull is
    // set, the field will be set to null in such cases.
    property EmptyStringsToNull: Boolean read FEmptyStringsToNull
      write FEmptyStringsToNull;

    // Use ExtraFields to hold the list of fields which are needed in the destination database but do not exist in the source database.
    // ExtraFields is cleared before each new row, so you can use OnRowReplicating to fill it.
    // The OnGetExtraFieldValues event is fired in order to get the values for the extra fields.
    // See also :
    // TCcReplicator.OnGetExtraFieldValues
    property ExtraFields: TStringList read FExtraFields;

    // Returns the table name in the remote database corresponding to the supplied local table name
    // See also: TableMapping
    function GetRemoteTableName(cLocalTableName: string): string;

    // Returns the table name in the local database corresponding to the supplied remote table name
    // See also: TableMapping
    function GetLocalTableName(cRemoteTableName: string): string;

    // Summary: Clear list of fields for each table
    // Description: Use ClearMetadataCache in order to explicitly clear the meta-data cache,
    // for instance, if you are have AutoClearMetadata set to false and you need a metadata change
    // (a new field, or a field dropped) to be taken into account.
    // See also:
    // AutoClearMetadata
    procedure ClearMetadataCache;

    // Name of the currently selected configuration, coming either from a TCcReplicatorList, or from a ConfigStorage component
    property ConfigurationName: string read FConfigurationName;

    // Overview: Connect to both local and remote databases.
    // Description: Connect establishes a connection with both the remote and the local databases,
    // if there was no existing connection, and starts both transactions. If the database connections
    // and/or transactions were already active, no error is produced.
    //
    // Before establishing the connection, the OnConnectLocal or the OnConnectRemote events are fired.
    procedure Connect;

    (* ************************************************************************************
      Summary:
      Call Replicate to start replicating the two databases.
      Description:
      If no replication process is already in progress, calling the Replicate method
      initiates the replication.<p/>
      First a BeforeReplicate event is fired, after which, the database connections are
      established, and the replication log is loaded. If the same record is found in
      both the remote and the local logs, a conflict is detected, and the OnConflict
      event occurs. This does not stop the other records from replicating, it merely
      means that the conflictual record will be put "in hold" until the user resolves
      the conflict. See TCcConflictMgr for more information about conflict management.<p/>
      <p/>
      If the log is empty, an OnEmptyLog event is fired. If not, an OnLogLoaded event
      is fired, and replication begins.<p/>
      If AutoPriority is false, then all the records to be replicated are grouped up by
      table priority , so all the records of the same table replicate together (whether
      they from the local or remote databases). Each table is enclosed by the
      OnTableBegin and OnTableEnd events.<p/>
      If AutoPriority is true, then the rows are replicated in the order the changes occured,
      without taking table priority into account. However, if the same record was changed several
      times, the duplicate rows in RPL$LOG will still be grouped up together and only replicated
      once, which means that there can be situations where the order can be wrong (for example, if
      a row is inserted into a table, then row into a different table, and then the first row gets
      updated to point to the row from the second table, with a foreign key constraint : a situation
      like that will cause a foreign key violation because the first table will be replicated first).
      That means that it can sometimes be normal to get foreign key violations during replication,
      but it has little incidence as the affected rows will be replicated successfully on the next
      attempt.<p/>
      The OnRowReplicating and OnRowReplicated
      events are fired before and after each record. For further detail, the
      OnQueryDone event is fired after every query (SELECT, UPDATE, INSERT or DELETE)
      that is performed during the replication of one record.<p/>
      <p/>
      Once replication has started, you may abort it at any time using the
      AbortReplication method. If you choose to do so, an OnReplicationAborted event will be fired
      as soon as the replication process is actually stopped.<p/>
      <p/>
      If an exception occurs during the replication of a record, the OnReplicationError
      event is fired, allowing to either resume replication, or to abort. If an
      exception occurs at any other point, an OnException event is fired, and
      replication is stopped.<p/>
      <p/>
      If the log was not empty, an OnFinished event is fired after replicating the
      records, even if an error occured and replication was aborted. OnFinished merely
      indicates that the replication process is no longer in progress, and can be used
      for such things as refreshing the user display and status labels (for example).
      The OnFinished event is only fire if the log was not empty however : for an event
      that always fires whatever the outcome of replication, you can use OnReplicationResult,
      and from there examine the LastResult property.
      ************************************************************************************ *)
    procedure Replicate;

    (* *******************************************************************************
      Summary:
      Call AbortReplication to stop the current replication process.
      Description:
      Once replication has started, you may abort it at any time using the
      AbortReplication method. AbortReplication doesn't actually stop replicating
      immediately (because the database component will in most cases be busy, with no
      way of interrupting them). As soon as the replication process has been aborted,
      the OnReplicationAborted event is called.
      See Also:
      OnReplicationAborted
      ******************************************************************************* *)
    procedure AbortReplication;

    // Overview: Disconnect local and remote databases.
    // Description: Call Disconnect to cut the connection to both local and remote databases.
    //
    // If the transactions are active, Disconnect first commits them. After this,
    // the OnDisconnect event is fired, and the database connections are cut.
    procedure Disconnect;

    { *******************************************************************************
      Summary:
      Replication log.
      Description:
      Use the Log property to access the TCcLog component used by the replicator.<p/>
      <p/>
      This component encapsulates the replication log, and provides properties for
      accessing and manipulating its data. Until the OnLogLoaded event, the
      log will be empty.<p/>
      The Log property is what you need to use when implementing events in order
      to detemine which row is currently being replicated. The TCcLog object holds
      the contents of the replication log (RPL$LOG table) and you can therefore access
      all the information from the log using the fields of TCcLog. You can also use the
      Keys property to access the value(s) of the primary key field(s) of the row being
      replicated.
      ******************************************************************************* }
    property Log: TCcCustomLog read FLog;

    // Short cut for Nodes.LocalNode
    property LocalNode: TCcNode read GetLocalNode;

    // Short cut for Nodes.RemoteNode
    property RemoteNode: TCcNode read GetRemoteNode;

    // Overview: Indicates that replication is finished.
    property Finished: Boolean read FFinished;

    // Overview: Indicates whether the replicator component is available or not.
    // Description: Busy is true from the moment the Replicate procedure is called,
    // until replication is either finished or aborted.<p/>
    // If the AutoReplicate timer fires when a previous replication cycle is still
    // in progress, it checks the Busy property and the timer is ignored until
    // the first replication cycle is finished.
    property Busy: Boolean read GetBusy;

    // Overview: Indicates that the replication in the process of closing.
    // Description: Closing is set to true whenever the user chooses to abort the replication
    // process (whether by the AbortReplication method, or in response to an OnReplicationError
    // event). As soon as an appropriate moment is found, replication then stops, and Closing
    // is set back to false.
    property Closing: Boolean read FClosing;
    constructor Create(Owner: TComponent; ConfigName: string); overload;
    constructor Create(Owner: TComponent); overload; override;
    destructor Destroy; override;

    // Short cut for Nodes.LocalNode.Connection
    property LocalDB: TCcConnection read GetLocalDB;
    // Short cut for Nodes.RemoteNode.Connection
    property RemoteDB: TCcConnection read GetRemoteDB;

    // Summary: Synchronize one or several rows between local and remote databases
    // Description: Call SynchronizeRows to explicitly synchronize one or several rows
    // of either the remote or the local database. The difference between SynchronizeRows
    // and Replicate is that Replicate synchronizes only records that changed since last
    // replication, whereas SynchronizeRows allows you to explicitly select one or several
    // records to synchronize.
    //
    // TableName is the name of the table to synchronize
    // SQLConditions is an optional SQL condition clause that can be used to limit the records to synchronize
    // Direction is the direction of synchronization.
    procedure SynchronizeRows(TableName, SQLConditions: string;
      Direction: TCcSyncDirection; lCrushAll: Boolean);

    // Summary: Synchronize a batch of records
    // Description: Use BatchImport to quickly synchronize a batch of records between the two databases
    // Unlike Replicate or SynchronizeRows, BatchImport loads all the records selected by the SQLConditions supplied together,
    // and then synchronizes the rows all together towards the destination database. This makes things much faster for large amounts of data.
    procedure BatchImport(TableName: string; Direction: TCcSyncDirection;
      SQLConditionSource: string; SQLConditionDest: string;
      DeleteAndReInsert: Boolean);

    procedure GetWhereValues(qQuery: TCcQuery);
	published
		property TrackInconsistentDeletes: Boolean read FTrackInconsistentDeletes write FTrackInconsistentDeletes;

		//Summary: Merges values of changed fields from both nodes, so as to avoid conflicts where possible
    //Description: MergeChangedFieldsOnConflict allows many conflicts to be automatically resolved by merging the changes from both
    //nodes, in cases where none of the same fields were changed on both sides. In cases that are still conflictual, the OnResolveConflict
    //event and the RPL$CONFLICTS table provide the list of fields changed on each side as well as the list of fields changed on both
    //sides, and it is also possible to find out for each field the value that it was changed from and to on each side, making conflict
   	//resolution much easier
    //
    //If the MergeChangedFieldsOnConflict property is set to True and a conflict is detected, the fields that were
    //changed on only one of the nodes will be merged. For instance, if the first node changed fields A, B and C while the seconde node
    //changed fields C and D, if the conflict is resolved in favor of the second node, the values of A and B from the first node will be
    //kept, as they were not conflictual with the change from the second node. Thus, conflict resolution can be much finer-grained.
    property MergeChangedFieldsOnConflict: Boolean read FMergeChangedFieldsOnConflict write FMergeChangedFieldsOnConflict;

    //Set Direction to sdLocalToRemote or sdRemoteToLocal to enable various performance optimizations when replicating only one way
    property Direction: TCcSyncDirection read FDirection write FDirection;

    //Summary: Set ReplicateOnlyChangedFields to True to replicate only the fields that were changed, as opposed to whole records
    //Description: If this option is set to true, then the changes will be replicated exactly as they occured, with no bunching of records, and
    //replicating every time only the fields that were changed. In terms of performance, the new mode will be better for some situations (particularly tables with many fields or with blob
    //fields, as those fields will only be sent over the wire when they actually change, rather than every time the row was touched.
    //
    //This new mode also allows improved consistency, as there should never be any foreign key violations (sometimes inevitable with
    //the old system, due to record bunching), which means that it becomes possible (and recommended) to run the whole replication cycle
    //within a single transaction, and to rollback if an error occurs.
    //Note: This option works together with TCcConfig.TrackFieldChanges
    property ReplicateOnlyChangedFields: Boolean read FReplicateOnlyChangedFields write FReplicateOnlyChangedFields;

    //Summary: Clear cached metadata after every replication cycle
    //Description: If AutoClearMetadata is false, the list of fields for each table will be kept in memory,
    //which improves performance as it avoids having to fetch them again next replication. If a field is added or dropped
    //however, ClearMetadataCache will need to be called so that the field lists can be reinitialized.
    //Setting AutoClearMetadata to true means that the field lists will automatically be cleared before replication begins.
    //Note: This does not apply to BatchImport nor to SynchronizeRows. For these methods, the metadata should be clear manually.
    property AutoClearMetadata: Boolean read FAutoClearMetadata
      write FAutoClearMetadata;

    // Indicates whether tables with no primary key are accepted or not
    // If FailIfNoPK is true (the default value), then an error will be raised
    // if a table is detected with no primary key.
    // If FailIfNoPK is false, the full list of field values (excluding blobs and string fields over 50 chars)
    // will be used instead
    // ONLY USE THIS OPTION FOR ONE-WAY REPLICATION, otherwise, it could cause conflicts.
    property FailIfNoPK: Boolean read FFailIfNoPK write FFailIfNoPK;

    // Summary:
    // Indicates whether primary key synchronization features are activated or not.
    // Description:
    // UsePKSynchronization allows you to activate CopyCat's primary key synchronization features.
    // This is implemented primarily for backwards compatibility, as this functionality is deprecated.
    property UsePKSynchronization: Boolean read FUsePKSynchronization
      write FUsePKSynchronization default false;

    // Indicates whether or not char fields (data type ftString or ftMemo) should be trimmed during replication
    property TrimCharFields: Boolean read FTrimCharFields write FTrimCharFields;

    // This event fires during replication if ExtraFields are defined.
    // ExtraFields are fields that are missing in one of the two nodes and that therefore cannot be replicated
    // normally, but if you implement this event, you can supply a value for them to be used when inserting or
    // updating the destination database.
    property OnGetExtraFieldValue: TCcFieldValueEvent read FOnGetExtraFieldValue
      write FOnGetExtraFieldValue;

    // Summary:
    // Indicates whether you would like to trace the replication events to the database or not
    // Description:
    // TraceEvents allows you to keep a trace of all the database operations that take place during replication, including
    // the full SQL with data, for every UPDATE, INSERT or DELETE statement that is applied. This enables you to have a precice
    // log of everything that happens to your database, and to know exactly what changes were applied when, to which database, and coming from
    // which node.<p/>
    // This information will be logged to the RPL$TRACE table, which is created automatically by simply connection a TCcConfig component
    // to your database. If you are upgrading your database from a previous version of CopyCat, you need to make sure you do so before trying
    // to use the TraceEvents property, as it cannot work if the RPL$TRACE table does not exist.<p/>
    // Depending on the setting you use, the trace information will be logged either to the local node only, to the remote node only,
    // to each separately (that is, each node keeps a trace only of the SQL statements that were applied to itself), or both together (meaning that
    // both nodes keep a full trace of all the SQL that was applied to both sides).<p/>
    // The lines created in RPL$TRACE have no functional use in CopyCat, they are merely informative. The table can therefore be cleared periodically
    // or you can activate tracing for a short time only (for debugging purposes for example).
    property TraceEvents: TCcTraceType read FTraceEvents write FTraceEvents
      default ttNoTrace;

    // Summary:
    // Specify whether to handle priorities automatically or not
    // Description:
    // If AutoPriority is True, CopyCat will use the natural order of events as the
    // replication order. What this means is that records will be replicated in the same
    // order as they were created or modified. This avoids most problems with foreign
    // key violations, though there can be complex situations where they could still
    // occur.<p/>
    // <p/>
    // If AutoPriority is set to False, then the table priorities must be set in
    // RPL$TABLES.
    property AutoPriority: Boolean read FAutoPriority write SetAutoPriority;

    // Summmary: Map table names between local and remote databases
    // Description: TableMapping allows you to define any tables whose names differ between local and remote databases.
    // The tables should simply defined as follows : <p/>
    // LOCAL_TABLE_NAME=REMOTE_TABLE_NAME <p/>
    // Only table names that differ need to be included in this list, tables that are not found in the list
    // are assumed to have the same name.
    property TableMapping: TStrings read FTableMapping write SetTableMapping;

    // Summary: Fired at the end of the replication, whatever the outcome
    // Description: OnReplicationResult is fired after every call to Replicate, whatever the outcome. The result of the replication
    // session is summarized in the LastResult object.
    property OnReplicationResult: TNotifyEvent read FOnReplicationResult
      write FOnReplicationResult;

    { *********************************************************************************
      Summary:
      Harmonize field list between local and remote databases.
      Description:
      If HarmonizeFields is true, only the fields that exist in both local and remote
      databases will be replicated. In order to find the list of fields, the local
      database is queried and (if necessary) the remote database also. The OnGetFields
      event is then fired, in order to let the developer change the list of fields.<p/>
      This implies that in certain cases, CopyCat will issue an additionnal query
      against the remote database to find the list of fields for each table. Therefore,
      setting HarmonizeFields to false will slightly improve performance, but will
      produce an exception if ever a field is missing.<p/>
      Actually, if there are lines in the log coming from the remote database, there is
      no need to explicitly fetch the list of fields from the remote database, since
      the data will be SELECTed from it. This optimizes performance by avoiding (part
      of the time) an extra query against the remote database, and it also means that
      in such cases, the field list is always harmonized, regardless of the value of
      the HarmonizeFields property.
      See Also:
      OnGetFields
      ********************************************************************************* }
    property HarmonizeFields: Boolean read FHarmonizeFields
      write FHarmonizeFields;

    // Overview: Keep the database connections open after replication ends.
    // Description: If KeepConnection is false, the connections will be closed after every call to
    // Replicate, and reopened upon the next replication. If KeepConnection is true,
    // the database connections will be kept open. This is especially useful when performing
    // automatic, periodic replication.
    property KeepConnection: Boolean read FKeepConnection
      write SetKeepConnection;

    // Summary:
    // Connection parameters for the local and remote nodes
    // See Also:
    // <link Replication nodes>
    property Nodes: TCcNodes read FNodes write FNodes;
    { *********************************************************************************
      Summary:
      Handles automatic, timer-based replication.
      Description:
      AutoReplicate allows you to enable/disable automatic replication. If Enabled is
      true, the replication process will automatically be launched every Frequency
      seconds, unless another process is already in progress.<p/>
      <p/>
      In order to start the auto-replication timer, you must call the Start method.
      Thus, Enabled should be set to true to enable auto-replication is a general way,
      and Start must be called when the application is ready to start auto-replicating.
      Use Stop to end the auto-replication timer.
      See Also:
      TAutoRepl.Start, TAutoRepl.Stop, Replicate, Busy
      ********************************************************************************* }
    property AutoReplicate: TAutoRepl read FAutoReplicate write FAutoReplicate;

    { *********************************************************************************
      Summary:
      Handles automatic, timer-based committing during replication.
      Description:
      AutoCommit allows you to enable/disable automatic committing. If CommitType \<\>
      ctNone, the transactions will automatically be committed every Frequency seconds,
      using the commit type specified in CommitType.<p/>
      If the commit timer fires during replication of a record, the commit will only
      actually be performed once the record has been fully replicated. This timer is
      reset every time the Replicate method is called.
      See Also:
      Replicate
      ********************************************************************************* }
    property AutoCommit: TCcAutoCommit read FAutoCommit write FAutoCommit;

    // Overview: Determines how the transaction is ended after replication.
    // Description: Possible options are Commit, CommitRetaining, or none.
    property CommitOnFinished: TCcCommitType read FCommitOnFinished
      write SetCommitOnFinished;

    { ********************************************************************************
      Summary:
      Determines whether or not to abort replication when an error occurs.
      Description:
      When an error occurs during replication, the OnReplicationError event is fired,
      to determine whether replication may resume (i.e. skip the current record, and
      and continue with the subsequent ones), or must be aborted. AbortOnError is used
      as a default behaviour in case this event is not implemented.
      See Also:
      OnReplicationError
      ******************************************************************************** }
    property AbortOnError: Boolean read FAbortOnError write FAbortOnError;

    { ********************************************************************************
      Summary:
      Fired when the replication process is finished.
      Description:
      If the log was not empty, an OnFinished event is fired after replicating the
      records, even if an error occured and replication was aborted. OnFinished merely
      indicates that the replication process is no longer in progress, and can be used
      for such things as refreshing the user display and status labels (for example).
      See Also:
      OnLogLoaded, OnEmptyLog
      ******************************************************************************** }
    property OnFinished: TCcNotifyEvent read FOnFinished write FOnFinished;

    // Overview: Fired after a record has been replicated.
    property OnRowReplicated: TCcAfterReplicationEvent read FOnRowReplicated
      write FOnRowReplicated;

    // Overview: Fired after a record has been replicated, just before removing row from RPL$LOG
    // Description: OnRowReplFinishing can be used for any action that needs to be part of the replication of a row.
    // If an exception is raised during execution of this event, the row will not be removed from RPL$LOG, and
    // replication will therefore be retried in the next replication cycle
    property OnRowReplFinishing: TCcAfterReplicationEvent
      read FOnRowReplFinishing write FOnRowReplFinishing;

    // Overview: Fired upon starting to replicate a new record.
    property OnRowBeforeReplicate: TCcNotifyEvent read FOnRowBeforeReplicate
      write FOnRowBeforeReplicate;

    // Overview: Fired during replication of each record
    // Description: The Fields property provides the values of all the fields of the current row
    // from the source database. You can remove fields from the list if you don't want them to be replicated
    // or you can change the field values that will be sent (note that in such a case, the changed value will
    // not be sent back to the source database).
    // If ReplicateRow is set to False and AbortAndTryLater is also false, the row will not be replicated
    // to destination database, but replicationg will be considered to have proceeded correctly (and therefore,
    // the row will be removed from RPL$LOG.
    // If ReplicatERow is True and AbortAndTryLater is false, the row will be replicated normally.
    // If AbortAndTryLater is True, replication of the row will be aborted (and OnReplicationAborted will be called),
    // and the row will be left in RPL$LOG and therefore retried at the next replication cycle.
    property OnRowReplicating: TCcReplicationEvent read FOnRowReplicating
      write FOnRowReplicating;

    // Overview: Fired for every query executed during replication of a record.
    // Description: The queries are SELECT from the source database, and UPDATE, INSERT or DELETE from destination database
    // This event provides the number of rows affected by the query.
    // If Done is set to True, the query will not be executed, so as to allow you to provide a customized replication
    // for the row when needed.
    property OnRowReplicatingQuery: TCcReplicationQueryEvent
      read FOnRowReplicatingQuery write FOnRowReplicatingQuery;

    // Overview: Fired when a conflict is detected.
    // Description: OnConflict is called
    // as soon as a conflict is detected. This does not stop replication of the other records from
    // continuing, it simply means that the conflictual record will not be replicated.
    // SeeAlso:TCcConflictMgr
    property OnConflict: TCcNotifyEvent read FOnConflict write FOnConflict;

    (* ********************************************************************************
      Summary:
      Fired when an error occurs while records are being replicated.
      Description:
      When an error occurs during replication, the OnReplicationError event is fired,
      to determine whether replication may resume (i.e. skip the current record, and
      and continue with the subsequent ones), or must be aborted. AbortOnError is used
      as a default behaviour in case this event is not implemented.
      See Also:
      Replicate, OnException
      ******************************************************************************** *)
    property OnReplicationError: TCcErrorEvent read FOnReplicationError
      write FOnReplicationError;

    // This event fires if one of the SQL statements used during replication (INSERT, UPDATE or DELETE) fails.
    // You can set the Retry parameter to true if you would like the query to be reexecuted. If you leave Retry
    // false, the row will be abandoned and the OnReplicationError will be fired.
    property OnRowReplicatingError: TCcReplicateRowErrorEvent
      read FOnRowReplicatingError write FOnRowReplicatingError;

    { *********************************************************************************
      Summary:
      Fired when an error occurs while no records are being replicated.
      Description:
      When an error occurs outside the scope of any replication (e.g. when checking for
      resolved conflicts, or when loading the log), the OnException event is fired. In
      such cases, it's of course impossible to resume replication (as with the
      OnReplicationError event), it is simply aborted.
      See Also:
      Replicate
      ********************************************************************************* }
    property OnException: TCcExceptionEvent read FOnException
      write FOnException;

    { *******************************************************************************
      Summary:
      Fired when the replication is aborted
      Description:
      OnReplicationAborted is fired whenever replication is aborted. This may be because:

      1. The application called the AbortReplication method.
      2. A replication error occured, and the application chose to abort (using the
      OnReplicationError event and/or the AbortOnError property).
      3. The database connection was lost during processing.
      See Also:
      Replicate, OnConnectionLost
      ******************************************************************************* }
    property OnReplicationAborted: TCcNotifyEvent read FOnReplicationAborted
      write FOnReplicationAborted;

    // Overview: Fired when neither local nor remote replication logs contain any records.
    property OnEmptyLog: TCcNotifyEvent read FOnEmptyLog write FOnEmptyLog;

    { ********************************************************************************
      Summary:
      Fired after creating the list of fields to be replicated for a table
      Description:
      This event is called in order to let the developer access, and possibly alter,
      the list of fields that are to be replicated for a certain table.<p/>
      Every time a new table is encountered in the replication log, the list of
      existing fields is loaded from the local database, and (if necessary) from the
      remote database also. If Harmonize fields is true, the field list is harmonized,
      and then OnGetFields is fired. Thus, the developer can override field list
      created by CopyCat.
      See Also:
      HarmonizeFields
      ******************************************************************************** }
    property OnGetFields: TCcGetFieldsEvent read FOnGetFields
      write FOnGetFields;

    (* ******************************************************************************
      Summary:
      Fired upon beginning a new table.
      Description:
      The lines in the replication log are grouped up by table (due to dependencies
      between tables). Therefore, this event is called every time records from a new
      table start replicating.
      See Also:
      OnTableEnd
      ****************************************************************************** *)
    property OnTableBegin: TCcNameEvent read FOnTableBegin write FOnTableBegin;

    (* ********************************************************************************
      Summary:
      Fired upon finishing a table.
      Description:
      The lines in the replication log are grouped up by table (due to dependencies
      between tables). Therefore, this event is called whenever all the records from a
      table have finished replicating.
      See Also:
      OnTableBegin
      ******************************************************************************** *)
    property OnTableEnd: TCcNameEvent read FOnTableEnd write FOnTableEnd;

    { ********************************************************************************
      Summary:
      Fired if an error occurs when trying to update the local value of a generator
      field.
      Description:
      This event is fired if an error occurs while updating the value generator field
      locally. By implementing the event, you can choose whether to re-try the update,
      or to abort. In the latter case, the exception is re-raised, and the
      OnReplicationError event is fired.
      See Also:
      TCcConfig
      ******************************************************************************** }
    property OnGenReplError: TCcErrorEvent read FOnGenReplError
      write FOnGenReplError;

    // Overview: Fired just before replicating the generator fields of a record.
    // Description: This event occurs before the generator fields of the current record get fetched
    // from the remote server and applied locally.
    // SeeAlso: TCcConfig
    property OnGenReplicating: TCcNotifyEvent read FOnGenReplicating
      write FOnGenReplicating;

    // Overview: Fired after a generator field was replicated.
    // Description: This event occurs every time a generator field has been replicated, and
    // it provides the name of the field as well as its new value. This occurs once for every
    // generator field of a record, but before the fields are updated in the local database.
    // SeeAlso: TCcConfig
    property OnKeySynchronized: TGenReplEvent read FOnKeySynchronized
      write FOnKeySynchronized;

    { ******************************************************************************
      Summary:
      Fired whenever the replication transactions are auto-committed.
      Description:
      This event occurs when the AutoCommit property is enabled, and the replication
      transactions are automatically committed. See AutoCommit for more information.
      See Also:
      AutoCommit
      ****************************************************************************** }
    property OnAutoCommit: TCcNotifyEvent read FOnAutoCommit
      write FOnAutoCommit;

    { **********************************************
      Summary:
      Fired before connecting to the local database.
      See Also:
      Connect
      ********************************************** }
    property OnConnectLocal: TCcNotifyEvent read FOnConnectLocal
      write FOnConnectLocal;

    { ***********************************************
      Summary:
      Fired before connecting to the remote database.
      See Also:
      Connect
      *********************************************** }
    property OnConnectRemote: TCcNotifyEvent read FOnConnectRemote
      write FOnConnectRemote;

    // Summary:
    // Fired before disconnecting from both local and remote database.
    // Description:
    // \Note that this event is not fired when the connection is unintentionally lost,
    // but only if the application calls the Disconnect method, or if KeepConnection is
    // set to False.
    // See Also:
    // OnConnectionLost
    property OnDisconnect: TCcNotifyEvent read FOnDisconnect
      write FOnDisconnect;

    { *********************************************************************************
      Summary:
      Fired just before the transaction is commited at the end of the replication
      process.
      Description:
      This event occurs at the end of the replication, when the CommitOnFinished
      property is set to something other than ctNone (that is, ctCommit or
      ctRetaining).<p/>
      \Note that this is different from the OnAutoCommit event: OnCommit only occurs
      when replication is finished, not when performing automatic, timer-based commits.
      ********************************************************************************* }
    property OnCommit: TCcNotifyEvent read FOnCommit write FOnCommit;

    (* ***********************************************************************************
      Summary:
      Frequently fired to give the application a chance to refresh the display.
      Description:
      This event is primarily designed to avoid the application freezing while
      replication is in progress. OnProgress is fired after every potentially slow
      operation (that is, mainly, remote database operations), and it is therefore a
      perfect place to process user-interface messages (using
      Application.ProcessMessages for instance), or to otherwise refresh the display.<p/>
      <p/>
      It is also called every time the current record changes, during replication, so
      it can be used for refreshing a progress bar.<p/>
      *********************************************************************************** *)
    property OnProgress: TCcNotifyEvent read FOnProgress write FOnProgress;

    // Overview: Fired every time a stored procedure is replicated.
    // Description: The OnReplicateProc event occurs every time a stored procedure is replicated,
    // providing the name of the procedure. The complete SQL text of the EXECUTE PROCEDURE
    // query is available using the PROCEDURE_STATEMENT field of the Log property.
    // SeeAlso: TCcLog
    property OnReplicateProc: TCcNameEvent read FOnReplicateProc
      write FOnReplicateProc;

    (* *********************************************************************************
      Summary:
      Fired for every query that is executing while replicating a record.
      Description:
      The OnQueryDone event provides the type of query that was executed (SELECT,
      UPDATE, INSERT or DELETE), as well as number of rows affected or retrieved.<p/>
      <p/>
      This allows you to know exactly what database operations were performed, which is
      helpful for diagnosing problems, or simply understanding how CopyCat works.
      ********************************************************************************* *)
    property OnQueryDone: TCcQueryEvent read FOnQueryDone write FOnQueryDone;

    { *********************************************************************************
      Summary:
      Fired after a non-empty replication log has been loaded
      Description:
      This event occurs immediately after the log has been loaded, and only if there is
      something to replicate. It can therefore be used (for example) for displaying how
      many records there are to replicate, and for initializing a progress bar. Before
      this point, the log is empty.
      See Also:
      Replicate, TCcLog
      ********************************************************************************* }
    property OnLogLoaded: TCcNotifyEvent read FOnLogLoaded write FOnLogLoaded;

    { ***********************************************************************
      Summary:
      Fired just before replication begins.
      Description:
      BeforeReplicate occurs as soon as the Replicate method is called or the
      auto-replicate timer is fired, before any replication takes place.
      See Also:
      Replicate, AutoReplicate
      *********************************************************************** }
    property BeforeReplicate: TCcNotifyEvent read FBeforeReplicate
      write FBeforeReplicate;

    { *********************************************************************************
      Summary:
      Fired when a database connection is lost.
      Description:
      When CopyCat detects that a database connection has been dropped, an
      OnConnectionLost event is fired. All remaning database connections are then cut,
      and replication is aborted (firing an OnReplicationAborted event). This means that if the
      remote database becomes temporarily unavailable while replication is in progress,
      the application will abort the replication gracefully without throwing any
      \exceptions (only events). As soon as the remote database is back online,
      replication will be able to resume.
      ********************************************************************************* }
    property OnConnectionLost: TCcLostConnectEvent read FOnConnectionLost
      write FOnConnectionLost;

    // Summary:
    // Fired as soon as a conflict has been detected;
    // Description:
    // Implement OnResolveConflict in order to resolve the conflict. OnResolveConflict
    // occurs before the conflict gets logged to the database, so if you resolve it
    // right away, it won't ever go into RPL$CONFLICTS, and the record will get
    // replicated immediately.<p/><p/>
    // The Conflict parameter gives all the information about the conflict (information
    // about the current record can be found in Log).<p/><p/>
    // To resolve the conflict, set Conflict.ChosenNode to the node the name of the node
    // whose changes are to be kept.
    property OnResolveConflict: TConflictEvent read FOnResolveConflict
      write FOnResolveConflict;

    // Summary: Event fired in order to determine whether the current record should be replicated or not
    // Description: Set the CanReplicate parameter to indicate whether or not to replicate the record.
    // The default value of CanReplicate is determined by the the AllowedOperations for the current record.
    // See also: TCcLog.AllowedOperations
    property OnCanReplicate: TCcCanReplicateEvent read FOnCanReplicate
      write FOnCanReplicate;

    // Summary: The number of records to replicate at a time
    // Description: If RecordChunks is set to any value above 0, replication will be divided up into chunck of
    // RecordChunks records. After every chunck, OnFinished is fired and the transactions are committed (depending on
    // the CommitOnFinished property), and OnLogLoaded is fired again before every new chunk.<p/><p/>
    // This functionality allows more granular commits and much better performance when dealing with large record sets.
    property RecordChunks: Integer read FRecordChunks write SetRecordChunks
      default 0;
    property Version: String read GetVersion write SetVersion;
    property KeepRowsInLog: Boolean read FKeepRowsInLog write FKeepRowsInLog;
  end;

  TCcCustomLog = class(TComponent)
  protected
    FReplicator: TCcReplicator;
    procedure ClearKeyRing; virtual; abstract;
    function GetKeys: TCcKeyRing; virtual; abstract;
    function GetDest: TCcNode; virtual; abstract;
    function GetOrigin: TCcNode; virtual; abstract;
    function GetLocalMode: Boolean; virtual; abstract;
    procedure SetLocalMode(Value: Boolean); virtual; abstract;
    function GetRemoteTableName: string; virtual; abstract;
    function GetPrimaryKeys: string; virtual; abstract;
    function GetTableName: string; virtual; abstract;
    function GetEof: Boolean; virtual; abstract;
    function GetCurrentLine: Integer; virtual; abstract;
    function GetLineCount: Integer; virtual; abstract;
    procedure SetReplicator(repl: TCcReplicator); virtual; abstract;
    function LoadFromDatabase: Boolean; virtual; abstract;
    procedure LogConflict(var Conflict: TConflictRecord);
    property Replicator: TCcReplicator read FReplicator write SetReplicator;
    procedure Init; virtual; abstract;
    function GetAllowedOperations: TCcQueryTypes; virtual; abstract;
    procedure LoadFromDataSet(cTableName: string; DataSet: TCcQuery);
      virtual; abstract;
    procedure LoadKeys(cTableName, PrimaryValues, PrimarySync, UniqueSync,
      GenericPrimarySync, UniqueNames, GenericUniqueSync: string);
      virtual; abstract;
    procedure RecordReplicated(Sender: TObject); virtual; abstract;
    procedure SignalError(Sender: TObject; e: Exception;
       var CanContinue: Boolean); virtual; abstract;
    function GetRecordID: string; virtual; abstract;
    function GetOldRecordID: string; virtual; abstract;
    function RowAlreadyDone: Boolean; virtual; abstract;
    function RowInError: Boolean;virtual;abstract;
    procedure LoadLogValues(DataSet: TCcMemoryTable); virtual; abstract;
    procedure SkipToRemote;virtual; abstract;
  public
    procedure DeleteLogLine(cCodes: String);overload;virtual;abstract;
    procedure DeleteLogLine(nCode: Integer);overload;virtual;abstract;
    procedure DeleteLogLine;overload;virtual; abstract;
    property RecordID: string read GetRecordID;
    property OldRecordID: string read GetOldRecordID;
    property PrimaryKeys: string read GetPrimaryKeys;
    property TableName: string read GetTableName;
    { ****************************************************************
      Summary:
      Retrieve field value as a string.
      Description:
      FBN(FieldName) is equivalent to FieldByName(FieldName).AsString.
      **************************************************************** }
    function FBN(cFieldName: string): string; virtual; abstract;
    { *********************************************************************************
      Summary:
      Retieve field object by name.
      Description:
      Use field name to access the TField object, in order to read or write the value
      of a field.<p/>
      Please note however, that application shouldn't normally change the values of any
      fields of the replication log, and doing so may cause unpredicted misbehavior.
      See Also:
      FBN
      ********************************************************************************* }
    function FieldByName(cFieldName: string): TCcField; virtual; abstract;
    function FieldExists(cFieldName: string): Boolean; virtual; abstract;
    { ***************************************************************************
      Summary:
      Go to the next record of the log, if any.
      Description:
      If the current record is already the last, the Eof property is set to True.
      *************************************************************************** }
    procedure Next(lRowSkipped: Boolean = False); virtual; abstract;
    // Eof is true on the last record of the log
    property Eof: Boolean read GetEof;
    // Total number of lines in the log
    property LineCount: Integer read GetLineCount;
    // Current line number
    property CurrentLine: Integer read GetCurrentLine;

    { Summary:
      Source database for the current record.
      Description:
      dpOrigin corresponds to the source database of the current
      record, that is, the database from which the current record
      originates. SeeAlso TCcCustomLog.dpDest }
    property Origin: TCcNode read GetOrigin;

    // Overview: Destination database for the current record.
    // Description: dpDest corresponds to the destination database of the current record, that is,
    // the database towards which the current record is to be replicated.
    // SeeAlso TCcCustomLog.dpOrigin
    property Dest: TCcNode read GetDest;

    // Indicates whether the record comes from the local database.
    property LocalMode: Boolean read GetLocalMode;

    // List of primary keys for the current record, with their values
    // Description
    // Use this property to access the list of primary keys for the current record.
    // The keys are stored in Name=Value format, so the keys can be accessed by using
    // the the Names and Values properties of TStringList.
    property Keys: TCcKeyRing read GetKeys;

    // List of operations allowed for the current record in the log.
    // Description: This list is based on the REPL_INSERTS, REPL_UPDATES and REPL_DELETES
    // fields in RPL$TABLES
    property AllowedOperations: TCcQueryTypes read GetAllowedOperations;

    property RemoteTableName: string read GetRemoteTableName;
  end;

  EReplError = class(Exception)
  end;

implementation

uses CcLog, CcConflictMgr
{$IFNDEF FPC}{$IFDEF MSWINDOWS} , Windows {$IFDEF CC_D2K14}, Vcl.Dialogs{$ELSE},
  Dialogs{$ENDIF} {$ELSE}, FMX.Dialogs {$ENDIF}{$ENDIF}
{$IFDEF CC_UseVariants}, Variants{$ENDIF};

{ TCcReplicator }

procedure TCcCustomLog.LogConflict(var Conflict: TConflictRecord);
begin
  FReplicator.LogConflict(Conflict);
end;

procedure TCcReplicator.ConnectionLost(Sender: TObject;
  var RaiseException: Boolean);
var
  Database: TCcConnection;
begin
  // Only handle the connection loss if a replication cycle is in progress
  if Busy then
  begin
    Database := (Sender as TCcConnection);
    if Assigned(OnConnectionLost) then
      OnConnectionLost(Self, Database);
    if Database = GetLocalDB then
      GetRemoteDB.Disconnect
    else
      GetLocalDB.Disconnect;
    RaiseException := true;
//    AbortReplication;
  end;
end;

procedure TCcReplicator.AbortReplication;
begin
  // AutoReplicate.Stop;
  if (Busy) then begin
    FClosing := true; // Replication will be aborted at the next opportunity
    if Assigned(FOnReplicationAborted) then
      FOnReplicationAborted(Self);
  end;
end;

function TCcReplicator.BuildGenUpdateFields: string;
var
  I: Integer;
begin
  Result := 'RPL$LOCAL = :RPL$LOCAL';

  for I := 0 to FLog.Keys.Count - 1 do
    with FLog.Keys[I] do
    begin
      if (SyncStatement <> '') then
        Result := Result + ', ' + GetLocalDB.DBAdaptor.MetaQuote(KeyName) +
          ' = :' + GetLocalDB.DBAdaptor.MetaQuote(KeyName);
    end;
end;

function TCcReplicator.BuildWhere(DB: TCcConnection): string;
var
  I: Integer;
  cKeyName: string;
begin
  Result := '';
  // On met un ''W_CC_'' afin de pouvoir différencier entre le paramètre pour le WHERE et la macro pour le UPDATE / INSERT
  for I := 0 to FLog.Keys.Count - 1 do
    if FLog.Keys[I].PrimaryKey then
    begin
      if I > 0 then
        Result := Result + ' and ';

      //The keys in FLog.Keys are calculated based on the LocalDB, so when we are replicating towards
      //the remote DB, we need to adapt the key names to work in the remote DB
      cKeyName := FLog.Keys[I].KeyName;
      if (DB = RemoteNode.Connection) and
         ((not LocalDB.DBAdaptor.QuoteMetadata) or (LocalDB.DBAdaptor.UnQuotedIdentifier(FLog.Keys[I].KeyName) = cKeyName)) then
        cKeyName := DB.DBAdaptor.UnQuotedIdentifier(FLog.Keys[I].KeyName);

      Result := Result + DB.DBAdaptor.MetaQuote(cKeyName) + ' %' + DB.DBAdaptor.MetaQuote('W_CC_' + FLog.Keys[I].KeyName);
    end;
end;

procedure TCcReplicator.Connect;
begin
  ConnectionCS.Enter;
  try
    // Automatically disconnect if the connection is already open without the right ReplicatingNode
    if (GetLocalDB.Connected) and
      (GetLocalDB.ReplicatingNode <> Nodes.RemoteNode.Name) then
      GetLocalDB.Disconnect;
    if (GetRemoteDB.Connected) and
      (GetRemoteDB.ReplicatingNode <> Nodes.LocalNode.Name) then
      GetRemoteDB.Disconnect;

    if (not GetLocalDB.Connected) then
    begin
      if Assigned(FOnConnectLocal) then
        FOnConnectLocal(Self);
      GetLocalDB.ConnectAsNode(Nodes.RemoteNode.Name,
        Nodes.RemoteNode.Password);
      RefreshDisplay;
    end;
    if (not GetRemoteDB.Connected) then
    begin
      if Assigned(FOnConnectRemote) then
        FOnConnectRemote(Self);
      GetRemoteDB.ConnectAsNode(Nodes.LocalNode.Name, Nodes.LocalNode.Password);
      RefreshDisplay;
    end;
  finally
    ConnectionCS.Leave;
  end;
end;

constructor TCcReplicator.Create(Owner: TComponent; ConfigName: string);
begin
	Create(Owner);
	FConfigurationName := ConfigName;
end;

constructor TCcReplicator.Create(Owner: TComponent);
begin
	inherited;
	FTrackInconsistentDeletes := False;
	slInconsistentDeletes := TStringList.Create;

	FMergeChangedFieldsOnConflict := false;
	UsePKSynchronization := false;
	FEmptyStringsToNull := false;
  FTrimCharFields := false;
  FExtraFields := TStringList.Create;

  FTraceEvents := ttNoTrace;
  FTableFields := TStringList.Create;
  FAutoClearMetadata := true;
  FTableMapping := TStringList.Create;

  FLogErrors := false;

  FFieldList := TStringList.Create;
  // FTableFieldList := TStringList.Create;
  CS := SyncObjs.TCriticalSection.Create;
  ConnectionCS := SyncObjs.TCriticalSection.Create;

  FNodes := TCcNodes.Create(Self);

  FGenerators := TStringList.Create;
  FSelect := TCcMemoryTable.Create(Self);
  FAutoReplicate := TAutoRepl.Create(Self);
  FAutoReplicate.OnPeriod := OnAutoReplicateTimer;
  FAutoCommit := TCcAutoCommit.Create(Self);
  FAutoCommit.OnPeriod := OnAutoCommitTimer;

  FLog := TCcLog.Create(Self);
  FLog.Replicator := Self;

  FConflictMgr := TCcConflictMgr.Create(Self);
  FConflictMgr.FReplicator := Self;

  FRecordChunks := 0;
  FAutoPriority := true;
  FReplicateOnlyChangedFields := false;
end;

destructor TCcReplicator.Destroy;
begin
  slInconsistentDeletes.Free;

  ClearMetadataCache;
  FTableFields.Free;

  FExtraFields.Free;
  FFieldList.Free;
  FGenerators.Free;
  FLog.Free;
  FAutoReplicate.Free;
  FAutoCommit.Free;
  FConflictMgr.Free;
  FSelect.Free;
  ConnectionCS.Free;
  CS.Free;
  FNodes.Free;
  FTableMapping.Free;
  inherited;
end;

procedure TCcReplicator.Disconnect;
begin
  ConnectionCS.Enter;
  try
    if GetRemoteDB.Connected or GetLocalDB.Connected then
    begin
      // Commit both transactions if they're still open
      // And then disconnect
      if Assigned(FOnDisconnect) then
        FOnDisconnect(Self);
      GetLocalDB.Disconnect;
      RefreshDisplay;
      GetRemoteDB.Disconnect;
    end;
  finally
    ConnectionCS.Leave;
  end;
end;

procedure TCcReplicator.DoAutoCommit;
begin
  if Assigned(FOnAutoCommit) then
    FOnAutoCommit(Self);

  if GetLocalDB.InTransaction then
  begin
    if (AutoCommit.CommitType = ctRetaining) then
      GetLocalDB.CommitRetaining
    else if (AutoCommit.CommitType = ctCommit) then
      GetLocalDB.Commit;
  end;
  if GetRemoteDB.InTransaction then
  begin
    if (AutoCommit.CommitType = ctRetaining) then
      GetRemoteDB.CommitRetaining
    else if (AutoCommit.CommitType = ctCommit) then
      GetRemoteDB.Commit;
  end;
  lAutoCommitPending := false;
end;

procedure TCcReplicator.DoGetFields(conn: TCcConnection; cTableName: string;
  slFields: TStringList);
begin
  slFields.Assign(conn.ListUpdatableTableFields(cTableName));
  { with conn.MetaQuery[sqlUpdatableFields] do begin
    Close;
    Param['table_name'].Value := cTableName;
    Exec;
    while (not Eof) do begin
    slFields.Add(Trim(Field['FIELD_NAME'].AsString));
    Next;
		end;
		end; }
end;

{procedure TCcReplicator.ClearErrorLog;
var
	qClearErrorLog: TCcQuery;
begin
	if FLogErrors then
	begin
		qClearErrorLog := GetLocalDB.UpdateQuery['qClearErrorLog'];
		qClearErrorLog.Close;
		qClearErrorLog.SQL.Text := 'delete from RPL$ERRORS';
		qClearErrorLog.Exec;
	end;
end;
 }
function TCcReplicator.DoReplicate: Boolean;
var
	lHarmonize: Boolean;
	cOldTableName, cTableName: string;
	// slFields :TStringList;
	lRefField: Boolean;
	I: Integer;
	cSQLConditions: string;
	cDestSQLConditions: string;
	cRefTable: string;
	queryPerformed: TCcQueryType;
	nCode: Integer;
begin
	cOldTableName := '';
	ClearGeneratorList;
	if FLog.LineCount > 0 then
	begin
		Result := true;
		if Assigned(FOnLogLoaded) then
			FOnLogLoaded(Self);
		try
			if FAutoClearMetadata then
				ClearMetadataCache;

			while (not FLog.Eof) do
			begin
				if ReplicateOnlyChangedFields and UsePKSynchronization then
				begin
					for I := 0 to FGenerators.Count - 1 do
						with TCcGenerator(FGenerators.Objects[I]) do
						begin
							if (Table = FLog.TableName) and (FLog.Keys.FindKey(Field) <> nil)
								and (OldValue = FLog.Keys.FindKey(Field).KeyValue) then
							begin
								FLog.Keys.FindKey(Field).KeyValue := NewValue;
							end;
						end;
				end;

        if FLog.RowAlreadyDone and (slInconsistentDeletes.IndexOfName(FLog.recordID) = -1)
          and (not ReplicateOnlyChangedFields or FLog.RowInError) then
        begin
 					if not FLog.RowInError then
						FLog.RecordReplicated(Self);

            RefreshDisplay;
            FLog.Next(True);
            continue;
        end;

        cTableName := FLog.FBN('Table_Name');
				if (cTableName <> cOldTableName) and not ReplicateOnlyChangedFields then
        begin
          if Assigned(FOnTableEnd) and (cOldTableName <> '') then
            FOnTableEnd(Self, cOldTableName);
          cOldTableName := cTableName;
          lHarmonize := FHarmonizeFields;
					GetFields(lHarmonize, cTableName);
					if Assigned(FOnTableBegin) then
						FOnTableBegin(Self, cTableName);
				end;
				try
					if Assigned(FOnRowBeforeReplicate) then
						FOnRowBeforeReplicate(Self);

					if (Trim(FLog.FBN('Procedure_Statement')) <> '') then
					begin
						ReplicateProcedure(FLog.Dest.Connection);
						RefreshDisplay;
					end
					else
					begin
						queryPerformed := ReplicateRecord(FLog.Origin.CurrentTableName, FLog.Dest.CurrentTableName, BuildWhere(FLog.Origin.Connection), BuildWhere(FLog.Dest.Connection), false);
						if slInconsistentDeletes.IndexOfName(FLog.recordID) > -1 then begin
              FLog.DeleteLogLine(slInconsistentDeletes.Values[FLog.recordID]);
              //slInconsistentDeletes.Delete(slInconsistentDeletes.IndexOfName(FLog.recordID));
						end;
					end;

					if Assigned(FOnRowReplFinishing) then
						FOnRowReplFinishing(Self, FLog.TableName, FSelect.Fields, queryPerformed);

					// Signal that row was replicated correctly and delete line from RPL$LOG
					FLog.RecordReplicated(Self);

					Inc(LastResult.RowsReplicated);

					if Assigned(FOnRowReplicated) then
						FOnRowReplicated(Self, FLog.TableName, FSelect.Fields, queryPerformed);

					if (lAutoCommitPending) then
						DoAutoCommit;

          RefreshDisplay;
        except
          // If a connection is lost, there's no point continuing, so replication is aborted
          on E: ECcLostConnection do
          begin
            raise;
          end;
          on E: ECcReplicationAborted do
          begin
            raise;
          end;
          on E: Exception do
          begin
            try
              LogError(E);
            except
              on E2: Exception do
              begin
                if not SignalError(Exception.Create('Error logging error!' + #13#10 + E2.Message + #13#10 + 'Original error was: ' + E.Message)) then
                  FClosing := true;
              end;
            end;
            if not SignalError(E) then
            // Doit-on continuer ou non après cette erreur?
              FClosing := true;
            // The current operation will get aborted at the next opportune moment (RefreshDisplay)
          end;
        end;
        // end;
        RefreshDisplay;
        FLog.Next;
      end;
      if Assigned(FOnTableEnd) then
        FOnTableEnd(Self, cTableName);
    finally
			SignalReplicationFinished;
    end;
  end
  else
  begin
    Result := false;
    if Assigned(FOnEmptyLog) then
    begin
      FOnEmptyLog(Self);
      if GetLocalDB.InTransaction then
        GetLocalDB.Rollback;
      if GetRemoteDB.InTransaction then
        GetRemoteDB.Rollback;
    end;
  end;
end;

procedure TCcReplicator.ClearMetadataCache;
var
  I: Integer;
begin
  for I := FTableFields.Count - 1 downto 0 do
    FTableFields.Objects[I].Free;
  FTableFields.Clear;
end;

procedure TCcReplicator.GetFields(lHarmonizeFields: Boolean;
  cTableName: string);
var
  // i:Integer;
  slRemoteFields: TStringList;
begin
  if not ReplicateOnlyChangedFields then
  begin
    FTableFieldList := TableFields[cTableName];
    if (FTableFieldList.Count = 0) then
    begin
      DoGetFields(GetLocalDB, cTableName, FTableFieldList);
      if (lHarmonizeFields) then
      begin
        slRemoteFields := TStringList.Create;
        try
          DoGetFields(GetRemoteDB, GetRemoteTableName(cTableName),
            slRemoteFields);
          HarmonizeLists(FTableFieldList, slRemoteFields);
        finally
          slRemoteFields.Free;
        end;
      end;

      RemoveItemsInCommon(FTableFieldList,
        LocalDB.ListKeywordsForbiddenAsFieldNames);
      RemoveItemsInCommon(FTableFieldList,
        RemoteDB.ListKeywordsForbiddenAsFieldNames);

      if Assigned(FOnGetFields) then
        FOnGetFields(Self, cTableName, FTableFieldList);
    end;
  end;
end;

function TCcReplicator.SanitizeParamName(paramName: String): String;
begin
  Result := ReplaceString(paramName, ' ', '_');
end;

function TCcReplicator.GetFieldsSQL(slFields: TStringList;
  cOperationType: string; node: TCcNode): string;
var
  I: Integer;
  cFields, cParams, cFieldName: string;
  key: TCcKey;
  lLocal: Boolean;
begin
  for I := 0 to slFields.Count - 1 do
  begin
    cFieldName := slFields.Strings[I];

    if (Trim(cFields) <> '') then
      cFields := cFields + ', ';
    if (Trim(cParams) <> '') then
      cParams := cParams + ', ';
    // cFields := cFields + node.Connection.DBAdaptor.QuoteIdentifier(cFieldName);
    cFields := cFields + node.Connection.DBAdaptor.MetaQuote(cFieldName{$IFDEF CC_D6},FLog.Origin.Connection.DBAdaptor{$ENDIF});

    // Créer des macros pour les champs susceptibles d'être affectés par un générateur
    // if FLog.Keys.KeyExists(cFieldName) then
    // key := FLog.Keys.FindKey(cFieldName);

    // lLocal := (FSelect.FindField('RPL$LOCAL') <> nil) and (FSelect.FieldByName('RPL$LOCAL').AsString = 'Y');

    // if key.PrimaryKey and UsePKSynchronization and (key.SyncStatement <> '') and (not lLocal) and (Trim(FLog.FBN('Ref_Field')) <> '') then
    // cParams := cParams + '%' + node.Connection.DBAdaptor.MetaQuote(slFields.Strings[I])
    // else
    cParams := cParams + ':' + SanitizeParamName(cFieldName);//slFields.Strings[I];

    if (cOperationType = 'U') then
    begin
      // S''il s''agit d''un UPDATE, on remet à zéro cParams, de sorte que cParams ne contienne
      // à chaque itération que la nom du paramètre.
      cFields := cFields + ' = ' + cParams;
      cParams := '';
    end
  end;
  if (cOperationType = 'U') then
    Result := cFields
  else
    Result := '(' + cFields + ')'#13#10'values'#13#10'(' + cParams + ')';
end;

function TCcReplicator.GetFieldValue(lLocal: Boolean; cFieldName: string;
  DB: TCcConnection): Variant;
var
  key: TCcKey;

  function GetFSelectFieldValue: Variant;
  var
    f: TCcMemoryField;
  begin
    Result := Null;
    f := FSelect.FindField(cFieldName);
    if Assigned(f) then
      Result := f.Value;
  end;

begin
  key := FLog.Keys.FindKey(cFieldName);

  // Dans le cas où RPL$LOCAL est vrai, la valeur du générateur a déjà été prise, donc il ne faut pas le refaire ici
  if (cFieldName = 'RPL$LOCAL') then
    Result := 'N'
  else if FExtraFields.IndexOf(cFieldName) > -1 then
  begin
    Result := GetFSelectFieldValue;
    if Assigned(FOnGetExtraFieldValue) then
      FOnGetExtraFieldValue(FLog.TableName, FSelect.Fields, cFieldName, Result);
  end
  else
    Result := GetFSelectFieldValue;
end;

function TCcReplicator.GetGenerator(TableName, cKeyName: string): Variant;
var
  Gen: TCcGenerator;
  I: Integer;
  cParamName: string;
  key: TCcKey;
  qGetGen: TCcQuery;
begin
  key := FLog.Keys.FindKey(cKeyName);
  if Assigned(key) then
  begin
    if (key.SyncStatement <> '') then
    begin
      qGetGen := GetRemoteDB.SelectQuery['TCcReplicator_qGetGen'];
      qGetGen.Close;
      qGetGen.SQL.Text := 'select ' + key.SyncStatement +
        ' as code_value from rdb$database';

      for I := 0 to qGetGen.ParamCount - 1 do
      begin
        cParamName := qGetGen.ParamByIndex[I].FieldName;
        if FLog.Keys.KeyExists(cParamName) then
          qGetGen.Param[cParamName].Value :=
            FLog.Keys.FindKey(cParamName).KeyValue
        else if FSelect.FindField(cParamName) <> nil then
          qGetGen.Param[cParamName].Value :=
            FSelect.FieldByName(cParamName).Value;
      end;

      qGetGen.Exec;
      Result := qGetGen.Field['CODE_VALUE'].Value;

      Gen := TCcGenerator.Create;
      Gen.Table := TableName;
      Gen.Field := key.KeyName; // FLog.FBN('PK' + IntToStr(nGenNum) + '_NAME');
      Gen.OldValue := key.KeyValue;
      // FLog.FBN('PK' + IntToStr(nGenNum) + '_VALUE');
      Gen.NewValue := Result;
      FGenerators.AddObject(Gen.Field, Gen);
      key.KeyValue := Result;
    end
    else
      Result := key.KeyValue;
  end;
end;

procedure TCcReplicator.GetGenUpdateParams(qQuery: TCcQuery);
var
  I: Integer;
begin
  for I := 0 to FLog.Keys.Count - 1 do
  begin
    with FLog.Keys[I] do
      if (SyncStatement <> '') then
        qQuery.Param[KeyName].Value := KeyValue;
  end;
  qQuery.Param['RPL$LOCAL'].Value := 'N';
end;

procedure TCcReplicator.UpdateRplLogWithNewKeys;
var
  I: Integer;
  cPKVals: String;
begin
  for I := 0 to FLog.Keys.Count - 1 do
  begin
    if FLog.Keys[I].PrimaryKey then
      cPKVals := cPKVals + QuotedStr(FLog.Keys[I].KeyValue) + ';';
  end;
  if cPKVals <> FLog.Keys.PrimaryKeyValues then
  begin
    with FLog.Origin.Connection.UpdateQuery
      ['TCcReplicator_UpdateRplLogWithNewKeys'] do
    begin
      Close;
      SQL.Text :=
        'update rpl$log set primary_key_values = :primary_key_values where node_name = :node_name and table_name = :table_name and primary_key_values = :old_primary_key_values';
      Param['table_name'].Value := FLog.TableName;
      Param['primary_key_values'].Value := cPKVals;
      Param['primary_key_values'].Value := FLog.Keys.PrimaryKeyValues;
      Param['node_name'].Value := FLog.Dest.Name;
      Exec;
    end;
  end;
end;

procedure TCcReplicator.GetGenUpdateFields;
var
  I: Integer;
begin
  for I := 0 to FLog.Keys.Count - 1 do
  begin
    with FLog.Keys[I] do
      if SyncStatement <> '' then
        FSelect.FieldByName(KeyName).Value := KeyValue;
  end;

  FSelect.FieldByName('RPL$LOCAL').Value := 'N';
end;

function TCcReplicator.GetLocalDB: TCcConnection;
begin
  Result := Nodes.LocalNode.FConnection;
  if not Assigned(Result) then
    raise Exception.Create('Local database connection not assigned!');
end;

// function TCcReplicator.GetLocalSYSDBA: TCcUser;
// begin
// Result := Users.LocalSYSDBA;
// end;

function TCcReplicator.GetLocalNode: TCcNode;
begin
  Result := Nodes.LocalNode;
end;

function TCcReplicator.GetRemoteDB: TCcConnection;
begin
  Result := Nodes.RemoteNode.FConnection;
  if not Assigned(Result) then
    raise Exception.Create('Remote database connection not assigned!');
end;

function TCcReplicator.GetRemoteNode: TCcNode;
begin
  Result := Nodes.RemoteNode;
end;

procedure TCcReplicator.GetWhereValues(qQuery: TCcQuery);
var
  I: Integer;
  Field: TCcMemoryField;
begin
  // Cette fonction renseigne les valeurs des paramètres de la clause WHERE de la requête fournie
  for I := 0 to FLog.Keys.Count - 1 do
    with FLog.Keys[I] do
      if PrimaryKey and qQuery.MacroExists('W_CC_' + KeyName) then
      begin
        if VarIsNull(KeyValue) then
          qQuery.Macro['W_CC_' + KeyName].Value := 'is null'
        else
          qQuery.Macro['W_CC_' + KeyName].Value := '= :' + 'W_CC_' + KeyName;
      end;

  for I := 0 to FLog.Keys.Count - 1 do
  begin
    if FLog.Keys[I].PrimaryKey and
      qQuery.ParamExists('W_CC_' + FLog.Keys[I].KeyName) then
    begin
      Field := FSelect.FindField(FLog.Keys[I].KeyName);
      with qQuery.Connection.DBAdaptor do
      begin
        if not FReplicateOnlyChangedFields and FSelect.Active and (Field <> nil)
          and (not Field.IsNull) then
          qQuery.Param['W_CC_' + FLog.Keys[I].KeyName].SetValueAsType
            (Field.Value, Field.DataType)
        else
          qQuery.Param['W_CC_' + FLog.Keys[I].KeyName].SetValueAsType
            (FLog.Keys[I].KeyValue, FLog.Keys[I].DataType);
      end;
    end;
  end;
end;

// This method harmonizes FFieldList against a TDataSet
// Any fields of FFieldList not found in the query are removed
procedure TCcReplicator.HarmonizeFieldList(Query: TCcMemoryTable;
  slList: TStringList);
var
  I: Integer;
  f: TCcMemoryField;
begin
  for I := slList.Count - 1 downto 0 do
  begin
    f := Query.FindField(slList.Strings[I]);
    if (f = nil) or (not f.Visible) then
      slList.Delete(I);
  end;
end;

// This method harmonizes two TStringLists
// Any items of the first string list not found in the second are removed
procedure TCcReplicator.HarmonizeLists(slFirst, slSecond: TStringList);
var
  I: Integer;
begin
  for I := slFirst.Count - 1 downto 0 do
    if (slSecond.IndexOf(slFirst.Strings[I]) = -1) then
      slFirst.Delete(I);
end;

procedure TCcReplicator.LogConflict(var Conflict: TConflictRecord);
var
  changedFields: TStringList;
begin
  if FMergeChangedFieldsOnConflict and FReplicateOnlyChangedFields and
    (Conflict.ConflictingFields.Count = 0) then
    Conflict.ChosenNode := 'REPLICATE_BOTH_WAYS';

  if (Assigned(OnResolveConflict)) then
    OnResolveConflict(Self, Conflict);

  if Conflict.ChosenNode <> 'REPLICATE_BOTH_WAYS' then
  begin
    if (Conflict.ChosenNode <> Conflict.Node1) and
      (Conflict.ChosenNode <> Conflict.Node2) then
    begin
      FConflictMgr.LogConflict(Conflict);
      if Assigned(FOnConflict) then
        FOnConflict(Self);
    end
    else
    begin
      FConflictMgr.ResolveConflict(FLog.TableName, FLog.RemoteTableName,
        FLog.FBN('PRIMARY_KEY_VALUES'), Conflict, ' is null');
    end;
  end;
end;

procedure TCcReplicator.LogError(E: Exception);
var
  qLogError: TCcQuery;
  cCodeField: string;
  cCodeValue: string;
begin
  Inc(LastResult.RowsErrors);
  qLogError := FLog.Origin.Connection.UpdateQuery['qLogError'];
  qLogError.Close;
  qLogError.SQL.Text := 'update RPL$LOG set error_message = :error_message, error_context = :error_context where code = :code';
  qLogError.Param['code'].Value := FLog.FBN('CODE');
  qLogError.Param['error_message'].Value := E.Message;
  qLogError.Param['error_context'].Value := FReplicationQueryContext;
  qLogError.Exec;
end;

procedure TCcReplicator.OnAutoCommitTimer(Sender: TObject);
begin
  lAutoCommitPending := true;
end;

procedure TCcReplicator.OnAutoReplicateTimer(Sender: TObject);
begin
  Replicate;
end;

procedure TCcReplicator.RefreshDisplay;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
  if (FClosing) then
  begin
    FClosing := false;
    raise ECcReplicationAborted.Create(Self);
  end
end;

procedure TCcReplicator.ClearLastResult;
begin
  with LastResult do
  begin
    ResultType := rtNone;
    ExceptionMessage := '';
    RowsReplicated := 0;
    RowsConflicted := 0;
    RowsErrors := 0;
  end;
end;

procedure TCcReplicator.Replicate;
var
  lMoreLogRows: Boolean;
begin
  ClearLastResult;
  if (not Busy) then
  begin
    SetBusy(true);
    try
      if Assigned(FBeforeReplicate) then
        FBeforeReplicate(Self);
      try
        FLog.Init;
        FLog.Keys.FailIfNoPK := FailIfNoPK;

        try
          Connect;
        except
          on E: Exception do
          begin
            if LocalDB.Connected then
              LastResult.ResultType := rtErrorConnectRemote
            else
              LastResult.ResultType := rtErrorConnectLocal;
            LastResult.ExceptionMessage := E.Message;
            raise;
          end;
        end;

        FFinished := false;
        RefreshDisplay;
        FAutoCommit.Start;
        try
          try
            FConflictMgr.CheckConflicts;
          except
            on E: Exception do
            begin
              LastResult.ResultType := rtErrorCheckingConflicts;
              LastResult.ExceptionMessage := E.Message;
              raise Exception.Create('Error checking conflicts!' + #13#10 +
                E.Message);
            end;
          end;

          RefreshDisplay;

          try
            FLog.LoadFromDatabase;
          except
            on E: Exception do
            begin
              LastResult.ResultType := rtErrorLoadingLog;
              LastResult.ExceptionMessage := E.Message;
              raise Exception.Create('Error loading RPL$LOG!' + #13#10 +
                E.Message);
            end;
          end;

          try
            if DoReplicate then
              LastResult.ResultType := rtReplicated
            else
              LastResult.ResultType := rtNothingToReplicate;
          except
            on E: ECcReplicationAborted do begin
              LastResult.ResultType := rtReplicationAborted;
            end;
            on E: Exception do
            begin
              LastResult.ResultType := rtException;
              LastResult.ExceptionMessage := E.Message;
              raise;
            end;
          end;
        finally
          FAutoCommit.Stop;
          if (not FKeepConnection) then
            Disconnect;
          SetBusy(false);
        end;
      except
        on E: ECcLostConnection do
        begin
          if E.Connection = LocalDB then
            LastResult.ResultType := rtErrorConnectLocal
          else
            LastResult.ResultType := rtErrorConnectRemote;
          LastResult.ExceptionMessage := E.Message;
          if Assigned(FOnException) and not Assigned(FOnConnectionLost) then
            FOnException(Self, E);
        end;
        on E: Exception do
        begin
          LastResult.ExceptionMessage := E.Message;
          if Assigned(FOnException) then
            FOnException(Self, E);
        end;
      end;
    finally
      SetBusy(false);
      //RefreshDisplay;
    end;
  end
  else
    LastResult.ResultType := rtReplicatorBusy;

  if Assigned(FOnReplicationResult) then
    FOnReplicationResult(Self);
end;

procedure TCcReplicator.ReplicateGenerators(TableName: string);
var
  lCanContinue, lSkip: Boolean;
  cFields: string;
  qGenUpdate: TCcQuery;
  I: Integer;
  lSyncStatement: Boolean;
begin
  if Assigned(FOnGenReplicating) then
    FOnGenReplicating(Self);

  lSyncStatement := false;
  for I := 0 to FLog.Keys.Count - 1 do
    if FLog.Keys[I].SyncStatement <> '' then
    begin
      lSyncStatement := true;
      Break;
    end;

  if lSyncStatement then
  begin
    qGenUpdate := GetLocalDB.UpdateQuery['TCcReplicator_qGenUpdate'];
    qGenUpdate.Close;
    cFields := BuildGenUpdateFields;
    qGenUpdate.SQL.Text := 'update %Cc_table_name_macro set '#13#10'' + cFields
      + ''#13#10' where ' + BuildWhere(GetLocalDB);
    qGenUpdate.Macro['Cc_table_name_macro'].Value := GetLocalDB.DBAdaptor.MetaQuote(TableName);
    GetWhereValues(qGenUpdate);

    for I := 0 to FLog.Keys.Count - 1 do
      with FLog.Keys[I] do
      begin
        KeyValue := GetGenerator(TableName, Trim(KeyName));
        if Assigned(FOnKeySynchronized) then
          FOnKeySynchronized(Self, KeyName, KeyValue);
      end;

    GetGenUpdateParams(qGenUpdate);

    while (true) do
    begin
      try
        qGenUpdate.Exec;
        Break;
      except
        on E: Exception do
        begin
          lCanContinue := false;
          if Assigned(FOnGenReplError) then
            FOnGenReplError(Self, Exception(E), lCanContinue, lSkip);
          if (not lCanContinue) then
            raise
          else
            continue;
        end;
      end;
    end;
    // On rafaîchait FSelect de sorte que tout se passe comme si la ligne avait été loggée avec ce code.
    GetGenUpdateFields;
    if ReplicateOnlyChangedFields then
      UpdateRplLogWithNewKeys;
  end;
end;

procedure TCcReplicator.ReplicateProcedure(conn: TCcConnection);
begin
  if Assigned(FOnReplicateProc) then
    FOnReplicateProc(Self, FLog.FBN('TABLE_NAME'));
  with conn.UpdateQuery['TCcReplicator_qExecuteProcedure'] do
  begin
    Close;
    SQL.Text := FLog.FBN('PROCEDURE_STATEMENT');
    Exec;
  end;
end;

function TCcReplicator.CanReplicate(QueryType: TCcQueryType; TableName: string;
  Fields: TCcMemoryFields; var RetryLater: Boolean): Boolean;
begin
  RetryLater := false;
  Result := (QueryType in FLog.AllowedOperations);
  if Assigned(FOnCanReplicate) then
    FOnCanReplicate(Self, TableName, Fields, QueryType, Result, RetryLater);
end;

procedure CopyFieldsFromDataset(tbl: TCcMemoryTable; slList: TStringList);
var
  I: Integer;
  f: TCcMemoryField;
begin
  slList.Clear;
  for I := 0 to tbl.Fields.Count - 1 do
  begin
    f := tbl.Fields[I];
    if (f.Visible) then
      slList.Add(f.FieldName);
  end;
end;

procedure TCcReplicator.UpdateReplicationFields;
var
  I: Integer;
begin
  if ReplicateOnlyChangedFields then
    CopyFieldsFromDataset(FSelect, FFieldList)
  else
  begin
    FFieldList.Assign(FTableFieldList);
    HarmonizeFieldList(FSelect, FFieldList);
  end;

  FFieldList.AddStrings(FExtraFields);

  for I := 0 to FFieldList.Count-1 do begin
    if (not FLog.Origin.Connection.DBAdaptor.QuoteMetadata)
      or (FLog.Origin.Connection.DBAdaptor.UnQuotedIdentifier(FFieldList[i]) = FFieldList[i])
    then
      FFieldList[i] := FLog.Dest.Connection.DBAdaptor.UnQuotedIdentifier(FFieldList[i]);
  end;
end;

function TCcReplicator.GetFieldType(cFieldName: string): TFieldType;
begin
  Result := ftUnknown;
  if Assigned(FSelect.FindField(cFieldName)) then
    Result := FSelect.FieldByName(cFieldName).DataType;
end;

function TCcReplicator.ReplicateRecord(OrigTableName, DestTableName,
  OrigSQLConditions, DestSQLConditions: string; lCrushAll: Boolean): TCcQueryType;
var
  I: Integer;
  // lRefField,
  lLocal, lReplicateRow, lAbortAndTryLater, RetryLater, lQueryDone: Boolean;
  cWhere, cFieldName: string;
  qDelete: TCcQuery;
  qSelect: TCcQuery;
  qUpdate: TCcQuery;
  qInsert: TCcQuery;
  lNeedInsert, lNeedUpdate: Boolean;
  DestTable, OriginTable: string;
  lRetry : Boolean;
  nSelected, nDeleted, nInserted, nUpdated, nCanceled: Integer;
  cErrorMessage: string;
  qr: TCcQueryResult;
  cOperationType: string;
  queryPerformed: TCcQueryType;
  LocalTable: string;
  // cPKVals: string;

  function GetFullSQL(qry: TCcQuery): string;
  var
    fieldVal: string;
    I: Integer;
    Param: TCcField;
  begin
    if (qry.Prepared) then
    begin
      Result := qry.RealSQL + #13#10;
      for I := 0 to qry.ParamCount - 1 do
      begin
        Param := qry.ParamByIndex[I];
        if Param.IsNull then
          fieldVal := '<null>'
        else if Param.DataType = ftBlob then
          fieldVal := '<Blob>'
        else
          fieldVal := Param.AsString;
        Result := Result + Param.FieldName + '=' + fieldVal + #13#10;
      end;
    end
    else
      Result := qry.SQL.Text;
  end;

  function TraceExec(qry: TCcQuery): Boolean;
  var
    SQL: string;
    lAbortReplication: Boolean;
    lRetry: Boolean;
    procedure InsertTrace(node: TCcNode);
    var
      qTrace: TCcQuery;
      cCodeField: string;
      cCodeValue: string;
    begin
      qTrace := node.Connection.UpdateQuery['TCcReplicator_qTrace'];

      if GetLocalDB.DBAdaptor.SupportsGenerators then
      begin
        cCodeField := 'CODE,';
        cCodeValue := IntToStr(GetLocalDB.Gen_Id('GEN_RPL$TRACE', 1)) + ',';
      end
      else
      begin
        cCodeField := '';
        cCodeValue := '';
      end;

      qTrace.SQL.Text := 'insert into RPL$TRACE (' + cCodeField +
        ' from_node, to_node, OPERATION_DATE, REPLICATION_DATE, TABLE_NAME, PRIMARY_KEY_VALUES, SQL_STATEMENT) '
        + ' values (' + cCodeValue +
        ' :from_node, :to_node, :OPERATION_DATE, :REPLICATION_DATE, :TABLE_NAME, :PRIMARY_KEY_VALUES, :SQL_STATEMENT)';
      qTrace.Close;
      qTrace.Param['from_node'].Value := FLog.Origin.Name;
      qTrace.Param['to_node'].Value := FLog.Dest.Name;
      qTrace.Param['operation_date'].Value := FLog.FieldByName('OPERATION_DATE').Value;
      qTrace.Param['replication_date'].Value := Now;
      qTrace.Param['table_name'].Value := FLog.TableName;
      qTrace.Param['PRIMARY_KEY_VALUES'].Value := FLog.PrimaryKeys;
      qTrace.Param['SQL_STATEMENT'].Value := SQL;
      qTrace.Exec;
    end;

  begin
    if (TraceEvents <> ttNoTrace) then
    begin
      SQL := GetFullSQL(qry);
      if ((TraceEvents = ttEachSeparately) and (qry.Connection = LocalDB)) or
        (TraceEvents = ttLocalOnly) or (TraceEvents = ttBothTogether) then
        InsertTrace(Nodes.LocalNode);
      if ((TraceEvents = ttEachSeparately) and (qry.Connection = RemoteDB)) or
        (TraceEvents = ttRemoteOnly) or (TraceEvents = ttBothTogether) then
        InsertTrace(Nodes.RemoteNode);
    end;

    // The result returned indicates whether to retry the query or not
    Result := False;
    try
      qry.Exec;
    except
      on E: Exception do
      begin
        Result := False;
        lAbortReplication := False;
        if Assigned(OnRowReplicatingError) then
          OnRowReplicatingError(Self, DestTableName, FSelect.Fields, E, Result, lAbortReplication);
        LastRowAborted := lAbortReplication;

        if (not Result) then
          raise Exception.Create(E.Message);
      end;
    end;
  end;

  procedure RaiseQueryError(qry: TCcQuery; E: Exception);
  begin
    if Assigned(qry) then
      raise Exception.Create('Error performing ' + FReplicationQueryContext +
        ' statement :' + #13#10 + GetFullSQL(qry) + #13#10#13#10 + E.Message)
    else
      raise Exception.Create('Error performing ' + FReplicationQueryContext +
        ' - no CcQuery' + #13#10 + E.Message);
  end;

  function PerformQuery(qt: TCcQueryType): TCcQueryResult;
  var
    q: TCcQuery;
    I: Integer;
    slFieldList: TStringList;
  begin
    if CanReplicate(qt, DestTable, FSelect.Fields, RetryLater) then
    begin
      RowReplicatingQuery(qt, lQueryDone);
      if not lQueryDone then
      begin
        UpdateReplicationFields;
        slFieldList := TStringList.Create;
        try
          repeat
            q := nil;
						if qt = qtUpdate then
              FReplicationQueryContext := 'UPDATE'
            else
              FReplicationQueryContext := 'INSERT';

						slFieldList.Assign(FFieldList);
						FLog.Dest.Connection.DBAdaptor.ExecutingReplicationQuery(DestTable, FReplicationQueryContext, slFieldList);
            try
              try
                q := FLog.Dest.Connection.UpdateQuery['TCcReplicator_q' + FReplicationQueryContext + DestTable];
                q.Close;

                // TODO add counter for insert or updates to distinguish them
                if FLog.Dest.Connection.DBAdaptor.SupportsInsertOrUpdate
                  and (not ReplicateOnlyChangedFields or (qt = qtInsert))
                then
                  q.SQL.Text := FLog.Dest.Connection.DBAdaptor.GetInsertOrUpdateSQL(slFieldList, FLog.Origin.Connection.DBAdaptor, FLog.Keys, LocalTable)
                else if qt = qtUpdate then
                  q.SQL.Text := 'update %cc_table_name_macro set '#13#10'' + GetFieldsSQL(slFieldList, 'U', FLog.Dest) + ''#13#10' where ' + cWhere
                else
                  q.SQL.Text := 'insert into %Cc_table_name_macro '#13#10' ' + GetFieldsSQL(slFieldList, 'I', FLog.Dest);

                FLog.Dest.Connection.DBAdaptor.ExecutingReplicationQuerySQL(DestTable, FReplicationQueryContext, q);

                q.Macro['cc_table_name_macro'].Value := DestTable;

                // The macros must be set first
                for I := 0 to FLog.Keys.Count - 1 do
                  if FLog.Keys[I].PrimaryKey and
                    q.MacroExists(FLog.Keys[I].KeyName) then
                      q.Macro[FLog.Keys[I].KeyName].Value := GetFieldValue(lLocal, FLog.Keys[I].KeyName, Log.Dest.Connection);

                GetWhereValues(q);
                for I := 0 to slFieldList.Count - 1 do
                begin
                  cFieldName := slFieldList.Strings[I];
                  if q.ParamExists(cFieldName) then
                    q.Param[SanitizeParamName(cFieldName)].SetValueAsType(GetFieldValue(lLocal, cFieldName, Log.Dest.Connection), GetFieldType(cFieldName));
                end;
                lRetry := TraceExec(q);
              except
                on E: Exception do
                  RaiseQueryError(q, E);
              end;
            finally
              FLog.Dest.Connection.DBAdaptor.ExecutedReplicationQuery(DestTable, FReplicationQueryContext, slFieldList);
            end;

            if Assigned(FOnQueryDone) then
              FOnQueryDone(Self, qt, q.RowsAffected);
            RefreshDisplay;

            if (FLog.Dest.Connection.CanUseRowsAffected) then
            begin
              if (q.RowsAffected = 0) then
                Result := qrNoneAffected
              else
                Result := qrOK;
            end
            else
              // If we can't use RowsAffected, we have either called CheckRowExists before the update,
              // or we have called INSERT OR UPDATE if supported.
              // Either way, we can consider it a success and needn't insert.
              Result := qrOK;

          until (not lRetry);
        finally
          slFieldList.Free;
        end;
      end
      else
        Result := qrCanceled;
    end
    else
    begin
      Result := qrCanceled;
      if RetryLater then
        raise Exception.Create('Can''t replicate UPDATE statement on table ' +
          DestTable + #13#10 + 'Disallowed by configuration');
    end;
  end;

  procedure DeleteRecord;
  begin
    if CanReplicate(qtDelete, DestTable, FSelect.Fields, RetryLater) then
    begin
      RowReplicatingQuery(qtDelete, lQueryDone);
      if not lQueryDone then
      begin
        repeat
          qDelete := nil; // avoid compiler warning in except block below
          FReplicationQueryContext := 'DELETE';

          try
            qDelete := FLog.Dest.Connection.UpdateQuery
              ['TCcReplicator_qDelete' + DestTable];
            qDelete.Close;
            qDelete.SQL.Text := 'delete from %cc_table_name_macro where ' +
              DestSQLConditions;
            qDelete.Macro['cc_table_name_macro'].Value := DestTable;
            GetWhereValues(qDelete);
            lRetry := TraceExec(qDelete);
          except
            on E: Exception do
              RaiseQueryError(qDelete, E);
          end;

          if Assigned(FOnQueryDone) then
            FOnQueryDone(Self, qtDelete, qDelete.RowsAffected);

          queryPerformed := qtDelete;

          if FLog.Dest.Connection.CanUseRowsAffected then
            nDeleted := nDeleted + qDelete.RowsAffected
          else
            nDeleted := nDeleted + 1;

        until (not lRetry);
      end;

      RefreshDisplay;
    end
    else
    begin
      if RetryLater then
        raise Exception.Create('Can''t replicate DELETE statement on table ' +
           DestTable + #13#10 + 'Disallowed by configuration');
    end;
  end;

  function FindDeleteRowInSourceLog: Boolean;
  begin
    with FLog.Origin.Connection.SelectQuery['FindDeleteRowInSourceLog'] do begin
      Close;
      SQL.Text := 'select count(*) from rpl$log where primary_key_values = :primary_key_values and table_name = :table_name and operation_type = ''D'' and replication_state is null';
      Param['primary_key_values'].Value := FLog.FBN('PRIMARY_KEY_VALUES');
      Param['table_name'].Value := FLog.TableName;
      Exec;
      Result := (Field['count'].AsInteger > 0);
    end;
  end;
begin
	lRetry := false;
  FExtraFields.Clear;

  nUpdated := 0;
  nInserted := 0;
  nCanceled := 0;
  nDeleted := 0;
  nSelected := 0;
  FSelect.Active := false;

  FReplicationQueryContext := '';
  OriginTable := FLog.Origin.Connection.DBAdaptor.MetaQuote(OrigTableName);
  DestTable := FLog.Dest.Connection.DBAdaptor.MetaQuote(DestTableName);
  if FLog.Origin = LocalNode then
    LocalTable := OrigTableName
  else
    LocalTable := DestTableName;

  if ReplicateOnlyChangedFields then
  begin
    cOperationType := FLog.FBN('Operation_type');
    if cOperationType = 'D' then
      DeleteRecord
    else
      FLog.LoadLogValues(FSelect);
  end
  else
  begin
    FReplicationQueryContext := 'SELECT';
    qSelect := FLog.Origin.Connection.SelectQuery['TCcReplicator_qSelect' + OriginTable];
    qSelect.Close;
    qSelect.SQL.Text := 'select * from %cc_table_name_macro where ' + OrigSQLConditions;
    qSelect.Macro['cc_table_name_macro'].Value := OriginTable;
    GetWhereValues(qSelect);
    qSelect.Exec;

    // Copy qSelect into FSelect
    FSelect.LoadFromDataSet(qSelect, FTrimCharFields, true, FEmptyStringsToNull);
    Assert(FSelect.RecordCount = qSelect.RecordCount);

    if Assigned(FOnQueryDone) then
			FOnQueryDone(Self, qtSelect, FSelect.RecordCount);
    RefreshDisplay;

    nSelected := FSelect.RecordCount;

    // If the record doesn't exist in the originating database, it means it has been deleted or the primary key value has been changed.
    // Either way, we delete the row from the destination database, if there is one.
    if (FSelect.RecordCount = 0) or lCrushAll then begin
      if (FLog.FBN('OPERATION_TYPE') = 'D') or (FLog.FBN('OPERATION_TYPE') = '') or not FTrackInconsistentDeletes then
        DeleteRecord
      else begin
        if slInconsistentDeletes.Values[FLog.recordID] <> '' then
          slInconsistentDeletes.Values[FLog.recordID] := slInconsistentDeletes.Values[FLog.recordID] + ',' + FLog.FBN('code')
        else
        slInconsistentDeletes.Values[FLog.recordID] := FLog.FBN('code');
        if not FindDeleteRowInSourceLog then
        raise Exception.Create('Row not found in source database for operation type ' + QuotedStr(FLog.FBN('OPERATION_TYPE')));
      end;
    end;
  end;

  FReplicationQueryContext := '';

  if (FSelect.RecordCount > 0) then
  begin
    if not ReplicateOnlyChangedFields then
    begin
      // Harmonize FTableFieldList with the fields in FSelect
      HarmonizeFieldList(FSelect, FTableFieldList);
    end;
    UpdateReplicationFields;

    FSelect.First;
    while not FSelect.Eof do
    begin
      if not ReplicateOnlyChangedFields then
      begin
        if UsePKSynchronization then
          FLog.Keys.LoadKeysFromDataSet(FLog.TableName, FSelect,
            FLog.FBN('PRIMARY_KEY_SYNC'), FLog.FBN('UNIQUE_KEY_SYNC'))
				else
          FLog.Keys.LoadKeysFromDataSet(FLog.TableName, FSelect, '', '');
      end;

      cWhere := BuildWhere(FLog.Dest.Connection);

      lReplicateRow := true;
      lAbortAndTryLater := false;
      if Assigned(FOnRowReplicating) then
        FOnRowReplicating(Self, FLog.TableName, FSelect.Fields, lReplicateRow,
          lAbortAndTryLater);

      if lAbortAndTryLater then
        Abort;

      if lReplicateRow then
      begin
        // We harmonize FFieldList again after the OnRowReplicating event, so as to allow users to delete a field from FSelect in the event
        UpdateReplicationFields;

        // Cette variable sert ici, et plus tard, dans GetFieldValue. Si on change de code, il ne faut pas la réinitialiser.
        lLocal := (FSelect.FindField('RPL$LOCAL') <> nil) and
          (FSelect.FieldByName('RPL$LOCAL').AsString = 'Y');

        if UsePKSynchronization then
        begin
          if lLocal then
          begin
            ReplicateGenerators(FLog.TableName);
            RefreshDisplay;
          end;
        end;

        if ReplicateOnlyChangedFields then
          lNeedUpdate := (cOperationType = 'U')
        else if lCrushAll then
          lNeedUpdate := false
        else if FLog.Dest.Connection.DBAdaptor.SupportsInsertOrUpdate then
          lNeedUpdate := true
        else if (not FLog.Dest.Connection.CanUseRowsAffected) then
          lNeedUpdate := CheckRowExists(FLog.Dest.Connection, DestTable, cWhere)
        else
          lNeedUpdate := true;

        lNeedInsert := not lNeedUpdate;

        if lNeedUpdate then
        begin
          qr := PerformQuery(qtUpdate);
          if (qr = qrNoneAffected) and not ReplicateOnlyChangedFields then
            lNeedInsert := true
          else if qr = qrCanceled then
            Inc(nCanceled)
          else if qr = qrOK then begin
            Inc(nUpdated);
            queryPerformed := qtUpdate;
          end;
        end;

        if (lNeedInsert) then
        begin
          qr := PerformQuery(qtInsert);
          if qr = qrCanceled then
            Inc(nCanceled)
          else begin
            Inc(nInserted);
            queryPerformed := qtInsert;
          end;
        end;
      end
      else
        Inc(nCanceled);
      FSelect.Next;
    end;

    if (FLog.Dest.Connection.CanUseRowsAffected) then
    begin
      if nSelected > 0 then
      begin
        if (nUpdated + nInserted + nCanceled) < nSelected then
        begin
          cErrorMessage := 'Selected ' + IntToStr(nSelected) +
            ' rows from source database' + #13#10 + IntToStr(nUpdated) +
            ' were updated and ' + IntToStr(nInserted) + ' were inserted';
          if (nCanceled > 0) then
            cErrorMessage := cErrorMessage + ' and ' + IntToStr(nCanceled) +
              ' were not replicated (canceled by OnRowReplicating event)';
          cErrorMessage := cErrorMessage + ' into the destination DB';
          raise Exception.Create(cErrorMessage);
        end;
      end
      else if nDeleted = 0 then
      begin
        // Issue a warning : this can be normal (if a row was inserted and then deleted before being replicated),
        // but it could perhaps be a bug
      end;
    end;
  end;

  Result := queryPerformed;
end;

procedure TCcReplicator.RowReplicatingQuery(qt: TCcQueryType;
  var lQueryDone: Boolean);
begin
  lQueryDone := false;
  if Assigned(FOnRowReplicatingQuery) then
    FOnRowReplicatingQuery(Self, FLog.TableName, FSelect.Fields, qt, lQueryDone);
end;

function TCcReplicator.CheckRowExists(conn: TCcConnection; TableName: string;
  Condition: string): Boolean;
var
  qCheckRecordExists: TCcQuery;
begin
  // Check if the record already exists, and if so we can insert it
  // This is unnecessary when updates are enabled, as we can use the RowsAffected property instead
  // (except for databases for which we can't use it, like MySQL)

  FReplicationQueryContext := 'CHECKRECORDEXISTS';
  qCheckRecordExists := conn.SelectQuery['TCcReplicator_qCheckRecordExists' +
    TableName];
  qCheckRecordExists.Close;
  qCheckRecordExists.SQL.Text :=
    'select count(*) as rec_count from %cc_table_name_macro '#13#10' where ' +
    Condition;
  qCheckRecordExists.Macro['cc_table_name_macro'].Value := TableName;
  GetWhereValues(qCheckRecordExists);
  qCheckRecordExists.Exec;
  Result := (qCheckRecordExists.Field['rec_count'].Value > 0);
end;

procedure TCcReplicator.SetCommitOnFinished(Value: TCcCommitType);
begin
  FCommitOnFinished := Value;
  if (Value <> ctCommit) then
    FKeepConnection := true;
end;

procedure TCcReplicator.SetKeepConnection(Value: Boolean);
begin
  FKeepConnection := Value;
  if (not Value) then
    FCommitOnFinished := ctCommit;
end;

function TCcReplicator.SignalError(E: Exception): Boolean;
var
  lSkipToRemote: Boolean;
begin
  lSkipToRemote := False;
  Result := not (AbortOnError or LastRowAborted);

  FLog.SignalError(Self, E, Result);
  if Assigned(FOnReplicationError) then
    FOnReplicationError(Self, E, Result, lSkipToRemote);

  if lSkipToRemote then begin
    FLog.SkipToRemote;
    Result := True;
  end;
end;

procedure TCcReplicator.SignalReplicationFinished;
begin
  DoCommitOnFinished;
  RefreshDisplay;
  FFinished := true;
  if Assigned(FOnFinished) then
    FOnFinished(Self);
end;

procedure TCcReplicator.DoCommitOnFinished;
begin
  if (FCommitOnFinished = ctCommit) or (FCommitOnFinished = ctRetaining) then
  begin
    if Assigned(FOnCommit) then
      FOnCommit(Self);

    if GetLocalDB.Connected and GetLocalDB.InTransaction then
    begin
      if FCommitOnFinished = ctCommit then
        GetLocalDB.Commit
      else
        GetLocalDB.CommitRetaining;
    end;
    RefreshDisplay;
    if GetRemoteDB.Connected and GetRemoteDB.InTransaction then
    begin
      if FCommitOnFinished = ctCommit then
        GetRemoteDB.Commit
      else
        GetRemoteDB.CommitRetaining;
    end;
  end;
end;

procedure TCcReplicator.ClearGeneratorList;
var
  I: Integer;
begin
  for I := FGenerators.Count - 1 downto 0 do
    TCcGenerator(FGenerators.Objects[I]).Free;
  FGenerators.Clear;
end;

(* procedure TCcReplicator.LoadConfig;
  begin
  inherited;
  { if Assigned(Nodes.LocalNode.Connection) and Assigned(Nodes.RemoteNode.Connection) then begin
  Disconnect;
  with ConfigStorage do
  begin
  with Nodes.LocalNode do begin
  Connection.CharSet := FieldByName('LocalCharSet').AsString;
  Connection.DBName := FieldByName('LocalDBName').AsString;
  Connection.UserLogin := FieldByName('LocalUserName').AsString;
  Connection.UserPassword := FieldByName('LocalPassword').AsString;
  end;
  with Nodes.RemoteNode do begin
  Connection.CharSet := FieldByName('RemoteCharSet').AsString;
  Connection.DBName := FieldByName('RemoteDBName').AsString;
  Connection.UserLogin := FieldByName('RemoteUserName').AsString;
  Connection.UserPassword := FieldByName('RemotePassword').AsString;
  end; }
  if not FBusy then
  begin
  if Assigned(Nodes.LocalNode.Connection) then
  Nodes.LocalNode.Connection.Disconnect;
  if Assigned(Nodes.RemoteNode.Connection) then
  Nodes.RemoteNode.Connection.Disconnect;

  FConfigurationName := ConfigStorage.FieldByName('ConfigName').AsString;
  Nodes.LocalNode.Connection := ConfigStorage.LocalDB.Connection;
  Nodes.RemoteNode.Connection := ConfigStorage.RemoteDB.Connection;
  FAutoReplicate.Frequency := ConfigStorage.FieldByName('AutoReplicateFrequency').AsInteger;
  FAutoReplicate.Enabled := (FAutoReplicate.Frequency > 0);
  end;
  end;

  procedure TCcReplicator.SaveConfig;
  begin
  { inherited;
  with ConfigStorage do
  begin
  Edit;
  FieldByName('LocalUserName').Value := Users.LocalUser.Name;
  FieldByName('LocalPassword').Value := Users.LocalUser.Password;
  FieldByName('LocalSYSDBAName').Value := Users.LocalSYSDBA.Name;
  FieldByName('LocalSYSDBAPassword').Value := Users.LocalSYSDBA.Password;
  FieldByName('RemoteUserName').Value := Users.RemoteUser.Name;
  FieldByName('RemotePassword').Value := Users.RemoteUser.Password;
  FieldByName('LocalCharSet').Value := LocalDB.CharSet;
  FieldByName('LocalDBName').Value := LocalDB.DBName;
  FieldByName('LocalSQLDialect').Value := LocalDB.SQLDialect;
  FieldByName('RemoteCharSet').Value := RemoteDB.CharSet;
  FieldByName('RemoteDBName').Value := RemoteDB.DBName;
  FieldByName('RemoteSQLDialect').Value := RemoteDB.SQLDialect;
  FieldByName('AutoReplicateFrequency').Value := FAutoReplicate.Frequency;
  end; }
  end; *)

procedure TCcAutoCommit.SetCommitType(CommType: TCcCommitType);
begin
  fEnabled := (CommType <> ctNone);
  FCommitType := CommType;
end;

function TCcReplicator.GetBusy: Boolean;
begin
  CS.Enter;
  try
    Result := FBusy;
  finally
    CS.Leave;
  end;
end;

procedure TCcReplicator.SetBusy(Value: Boolean);
begin
  CS.Enter;
  try
    FBusy := Value;
  finally
    CS.Leave;
  end;
end;

procedure TCcReplicator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Assigned(Nodes) and (AComponent <> Self) then
    begin
      if AComponent = Nodes.LocalNode.Connection then
        Nodes.LocalNode.SetConnection(nil);
      if AComponent = Nodes.RemoteNode.Connection then
        Nodes.RemoteNode.SetConnection(nil);
    end;
  end;
  inherited;
end;

constructor TPeriodicity.Create(Owner: TComponent);
begin
  inherited Create;
  FOwner := Owner;
  Timer := {$IFDEF FPC}TFPTimer{$ELSE}TTimer{$ENDIF}.Create(Owner);
  FFrequency := 30;
  fEnabled := false;
end;

destructor TPeriodicity.Destroy;
begin
  Timer.Free;
  inherited;
end;

// Summary:
// Start the timer.
// Description:
// Call Start to initiate the timer.
procedure TPeriodicity.Start;
begin
  if (fEnabled and Assigned(OnPeriod)) then
  begin
    Timer.Interval := FFrequency * 1000;
    Timer.OnTimer := OnPeriod;
    Timer.Enabled := true;
  end
end;

{ ****************************************
  * Summary:                             *
  * Stop the timer.                      *
  * Description:                         *
  * Call Stop to end the periodic timer. *
  **************************************** }
procedure TPeriodicity.Stop;
begin
  Timer.Enabled := false;
  // On arrête le compteur, mais on ne touche pas à la propriété Enabled
end;

constructor TCcNodes.Create(AOwner: TCcReplicator);
begin
  inherited Create;
  LocalNode := TCcNode.Create(AOwner);
  RemoteNode := TCcNode.Create(AOwner);
end;

destructor TCcNodes.Destroy;
begin
  LocalNode.Free;
  RemoteNode.Free;
  inherited;
end;

procedure TCcReplicator.SetAutoPriority(const Value: Boolean);
begin
  FAutoPriority := Value;
  if (FAutoPriority) then
    RecordChunks := 0;
end;

procedure TCcReplicator.SetRecordChunks(const Value: Integer);
begin
  FRecordChunks := Value;
  if (FRecordChunks > 0) then
    AutoPriority := false;
end;

function TCcReplicator.GetTableFields(TableName: string): TStringList;
var
  index: Integer;
begin
  index := FTableFields.IndexOf(TableName);
  if (index = -1) then
    index := FTableFields.AddObject(TableName, TStringList.Create);
  Result := TStringList(FTableFields.Objects[index]);
end;

function TCcReplicator.GetVersion: String;
begin
  Result := VersionNumber;
end;

{ TCcNode }

constructor TCcNode.Create(AOwner: TCcReplicator);
begin
  FReplicator := AOwner;
end;

procedure TCcNode.SetConnection(const Value: TCcConnection);
begin
  if FConnection = Value then
    Exit;

  if FConnection <> nil then
  begin
    FConnection.RemoveFreeNotification(FReplicator);
    FConnection.OnConnectionLost := FPrevConnLost;
  end;
  FConnection := Value;
  if (FConnection <> nil) then
  begin
    FPrevConnLost := FConnection.OnConnectionLost;
    FConnection.OnConnectionLost := FReplicator.ConnectionLost;
    FConnection.FreeNotification(FReplicator);
  end;

  if FReplicator.LocalNode = Self then
  begin
    FReplicator.Log.ClearKeyRing;
  end;
end;

procedure TCcNode.FindConfigName(node: TCcNode);
begin
  with node.Connection.SelectQuery['TCcNode.FindConfigName'] do
  begin
    Close;
    SQL.Text := 'select config_name from RPL$USERS where login = :login';
    Param['login'].AsString := Self.Name;
    Exec;
    FConfigName := Trim(Field['config_name'].AsString);
  end;
end;

function TCcNode.GetCurrentTableName: string;
begin
  if (Self = FReplicator.RemoteNode) then
    Result := FReplicator.Log.RemoteTableName
  else
    Result := FReplicator.Log.FBN('TABLE_NAME');
end;

procedure TCcReplicator.SynchronizeRows(TableName: string;
  SQLConditions: string; Direction: TCcSyncDirection; lCrushAll: Boolean);
var
  lHarmonize: Boolean;
  qKeys: TCcQuery;
  slLines: TStringList;
  I: Integer;
  cSQL: string;
  cOrigTableName, cDestTableName: string;
begin
  FLog.Keys.FailIfNoPK := FailIfNoPK;

  // lHarmonize := FHarmonizeFields;
  if Direction = sdRemoteToLocal then
  begin
    FLog.SetLocalMode(false);
    lHarmonize := false;
    cOrigTableName := GetRemoteTableName(TableName);
    cDestTableName := TableName;
  end
  else
  begin
    FLog.SetLocalMode(true);
    lHarmonize := true;
    cOrigTableName := TableName;
    cDestTableName := GetRemoteTableName(TableName);
  end;

  GetFields(lHarmonize, TableName);

  cSQL := 'select ' + FLog.Keys.TableKeys[TableName].CommaText + ' from ' +
    FLog.Origin.Connection.DBAdaptor.MetaQuote(cOrigTableName) + ' t where ' +
    SQLConditions;
  qKeys := TCcQuery.Create(Self);
  slLines := TStringList.Create;
  try
    qKeys.SelectStatement := true;
    qKeys.SQL.Text := cSQL;
    qKeys.Connection := FLog.Origin.Connection;
    qKeys.Exec;
    while not qKeys.Eof do
    begin
      FLog.LoadFromDataSet(TableName, qKeys);
      if lCrushAll and (slLines.IndexOf(FLog.PrimaryKeys) = -1) then
        slLines.Add(FLog.PrimaryKeys)
      else
        ReplicateRecord(cOrigTableName, cDestTableName,
          BuildWhere(FLog.Origin.Connection),
          BuildWhere(FLog.Dest.Connection), false);
      qKeys.Next;
    end;
    if lCrushAll then
    begin
      qKeys.Connection := FLog.Dest.Connection;
      qKeys.SQL.Text := cSQL;
      qKeys.Exec;
      while not qKeys.Eof do
      begin
        FLog.LoadFromDataSet(TableName, qKeys);
        if (slLines.IndexOf(FLog.PrimaryKeys) = -1) then
          slLines.Add(FLog.PrimaryKeys);
        qKeys.Next;
      end;
      for I := 0 to slLines.Count - 1 do
      begin
        FLog.LoadKeys(TableName, slLines[I], '', '', '', '', '');
        ReplicateRecord(cOrigTableName, cDestTableName,
          BuildWhere(FLog.Origin.Connection),
          BuildWhere(FLog.Dest.Connection), false);
      end;
    end;
  finally
    qKeys.Free;
    slLines.Free;
  end;
end;

procedure TCcReplicator.BatchImport(TableName: string;
  Direction: TCcSyncDirection; SQLConditionSource, SQLConditionDest: string;
  DeleteAndReInsert: Boolean);
var
  lHarmonize: Boolean;
  cOrigTableName, cDestTableName: string;
begin
  FLog.Keys.FailIfNoPK := FailIfNoPK;

  // lHarmonize := FHarmonizeFields;
  if Direction = sdRemoteToLocal then
  begin
    FLog.SetLocalMode(false);
    lHarmonize := false;
    cOrigTableName := GetRemoteTableName(TableName);
    cDestTableName := TableName;
  end
  else
  begin
    FLog.SetLocalMode(true);
    lHarmonize := true;
    cOrigTableName := TableName;
    cDestTableName := GetRemoteTableName(TableName);
  end;

  Connect;
  GetFields(lHarmonize, TableName);

  FLog.LoadKeys(TableName, '', '', '', '', '', '');
  ReplicateRecord(cOrigTableName, cDestTableName, SQLConditionSource,
    SQLConditionDest, DeleteAndReInsert);
end;

function TCcReplicator.ValueByKey(list: TStrings; key: string): string;
var
  I: Integer;
  line: string;
begin
  for I := 0 to list.Count - 1 do
  begin
    line := Trim(list[I]);
    if Pos(key + '=', line) = 1 then
    begin
      Result := Copy(line, length(key) + 2, length(line));
      Exit;
    end;
  end;
end;

function TCcReplicator.KeyByValue(list: TStrings; Value: string): string;
var
  I, nPosEqual: Integer;
  line: string;
begin
  for I := 0 to list.Count - 1 do
  begin
    line := Trim(list[I]);
    nPosEqual := length(line) - length(Value);
    if Pos('=' + Value, line) = nPosEqual then
    begin
      Result := Copy(line, 1, nPosEqual - 1);
      Exit;
    end;
  end;
end;

function TCcReplicator.GetLocalTableName(cRemoteTableName: string): string;
var
  val: string;
begin
  if (LocalDB <> nil) and (LocalDB.DBAdaptor <> nil) and (RemoteDB <> nil) and
    (RemoteDB.DBAdaptor <> nil) then
  begin
    val := KeyByValue(FTableMapping, cRemoteTableName);
    if (val <> '') then
      Result := val
    else
    begin
      //IF the remote database doesn't support case-sensitive meta-data,
      //OR IF the remote table name is formatted as case-insensitive (uppercase for most, lowercase for Postgres)
      //THEN we need to convert the tablename to the format that the local DB understands as being case-insensive
      //
      //Thus: "MY_TABLE" in Firebird should become "my_table" in PostgreSQL
      //  and My_Table in SQLite should become "MY_TABLE" in Firebird
      //Of course, none of this matters if the local database doesn't use quoted metadata...
      if (not RemoteDB.DBAdaptor.QuoteMetadata) or (RemoteDB.DBAdaptor.UnQuotedIdentifier(cRemoteTableName) = cRemoteTableName)
      then
        Result := LocalDB.DBAdaptor.UnQuotedIdentifier(cRemoteTableName)
      else
        Result := cRemoteTableName;
    end;
  end;

  { for I:=0 to FTableMapping.Count-1 do begin
    cName := FTableMapping.Names[I];
    if (FTableMapping.Values[cName] = cRemoteTableName) then begin
    Result := cName;
    Break;
    end;
    end; }
end;

function TCcReplicator.GetRemoteTableName(cLocalTableName: string): string;
var
  val: string;
begin
  if (LocalDB <> nil) and (LocalDB.DBAdaptor <> nil) and (RemoteDB <> nil) and
    (RemoteDB.DBAdaptor <> nil) then
  begin
    val := ValueByKey(FTableMapping, cLocalTableName);
    if (val <> '') then
      Result := val
    else
    begin
      //IF the local database doesn't support case-sensitive meta-data,
      //OR IF the local table name is formatted as case-insensitive (uppercase for most, lowercase for Postgres)
      //THEN we need to convert the tablename to the format that the destination DB understands as being case-insensive
      //
      //Thus: "MY_TABLE" in Firebird should become "my_table" in PostgreSQL
      //  and My_Table in SQLite should become "MY_TABLE" in Firebird
      //Of course, none of this matters if the remote database doesn't use quoted metadata...
      if (not LocalDB.DBAdaptor.QuoteMetadata) or (LocalDB.DBAdaptor.UnQuotedIdentifier(cLocalTableName) = cLocalTableName)
      then
        Result := RemoteDB.DBAdaptor.UnQuotedIdentifier(cLocalTableName)
      else
        Result := cLocalTableName;
    end;
  end;
end;

procedure TCcReplicator.SetTableMapping(const Value: TStrings);
begin
  FTableMapping.Assign(Value);
end;

procedure TCcReplicator.SetVersion(Value: String);
begin

end;

{ ECcReplicationAborted }

constructor ECcReplicationAborted.Create(repl: TCcReplicator);
begin
  inherited Create('Replication aborted!');
  FReplicator := repl;
end;

end.
