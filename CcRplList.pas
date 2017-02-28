//CopyCat replication suite<p/>
//Copyright (c) 2005 Microtec Communications<p/>
//For any questions or technical support, contact us at contact@copycat.fr
unit CcRplList;

interface

{$I CC.INC}

uses Classes, CCat, CcConfStorage, CcReplicator, CcProviders,CcMemDS, DB;

type
  //TCcReplicatorList is a component for holding a list of TCcReplicator components,
  //automatically creating one per configuration, and assigning to each one its
  //properties from the configuration settings.
  TCcReplicatorList = class(TCcComponent)
  private
    FReplicatorList:TStringList;
    FOnReplicationError: TCcErrorEvent;
    FOnGenReplError: TCcErrorEvent;
    FOnException: TCcExceptionEvent;
    FOnGetFields: TCcGetFieldsEvent;
    FOnConnectionLost: TCcLostConnectEvent;
    FOnTableBegin: TCcNameEvent;
    FOnReplicateProc: TCcNameEvent;
    FOnTableEnd: TCcNameEvent;
    FOnEmptyLog: TCcNotifyEvent;
    FOnDisconnect: TCcNotifyEvent;
    FOnLogLoaded: TCcNotifyEvent;
    FOnCommit: TCcNotifyEvent;
    FOnRowReplicating: TCcReplicationEvent;
    FOnConnectRemote: TCcNotifyEvent;
    FOnProgress: TCcNotifyEvent;
    FOnConflict: TCcNotifyEvent;
    FOnRowReplicated: TCcAfterReplicationEvent;
    FOnConnectLocal: TCcNotifyEvent;
    FOnGenReplicating: TCcNotifyEvent;
    FBeforeReplicate: TCcNotifyEvent;
    FOnAutoCommit: TCcNotifyEvent;
    FOnAbort: TCcNotifyEvent;
    FOnFinished: TCcNotifyEvent;
    FOnQueryDone: TCcQueryEvent;
    FOnGenReplicated: TGenReplEvent;
    FOnRowBeforeReplicate: TCcNotifyEvent;
    procedure FreeReplicator(repl: TCcReplicator);
    function GetReplicator(ConfigID:Integer): TCcReplicator;
    procedure SetReplicator(ConfigID:Integer; Repl: TCcReplicator);
    function GetReplicatorCount: Integer;
    function GetCurrentReplicator: TCcReplicator;
    function GetConfigNames(Repl: TCcReplicator): String;
  protected
    procedure LoadConfig;override;
    procedure SaveConfig;override;
    procedure DeleteConfig;override;
    procedure EditConfig;override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property CurrentReplicator: TCcReplicator read GetCurrentReplicator;
    property Replicators[ConfigName:Integer]: TCcReplicator read GetReplicator write SetReplicator;
    property ReplicatorCount: Integer read GetReplicatorCount;
    property ConfigNames[Repl:TCcReplicator]: String read GetConfigNames;
  published
    property OnFinished :TCcNotifyEvent read FOnFinished write FOnFinished;
    property OnRowReplicated :TCcAfterReplicationEvent read FOnRowReplicated write FOnRowReplicated;
    property OnRowBeforeReplicate :TCcNotifyEvent read FOnRowBeforeReplicate write FOnRowBeforeReplicate;
    property OnRowReplicating :TCcReplicationEvent read FOnRowReplicating write FOnRowReplicating;
    property OnConflict :TCcNotifyEvent read FOnConflict write FOnConflict;
    property OnReplicationError :TCcErrorEvent read FOnReplicationError write FOnReplicationError;
    property OnException :TCcExceptionEvent read FOnException write FOnException;
    property OnAbort :TCcNotifyEvent read FOnAbort write FOnAbort;
    property OnEmptyLog :TCcNotifyEvent read FOnEmptyLog write FOnEmptyLog;
    property OnGetFields :TCcGetFieldsEvent read FOnGetFields write FOnGetFields;
    property OnTableBegin :TCcNameEvent read FOnTableBegin write FOnTableBegin;
    property OnTableEnd :TCcNameEvent read FOnTableEnd write FOnTableEnd;
    property OnGenReplError :TCcErrorEvent read FOnGenReplError write FOnGenReplError;
    property OnGenReplicating :TCcNotifyEvent read FOnGenReplicating write FOnGenReplicating;
    property OnGenReplicated :TGenReplEvent read FOnGenReplicated write FOnGenReplicated;
    property OnAutoCommit :TCcNotifyEvent read FOnAutoCommit write FOnAutoCommit;
    property OnConnectLocal :TCcNotifyEvent read FOnConnectLocal write FOnConnectLocal;
    property OnConnectRemote :TCcNotifyEvent read FOnConnectRemote write FOnConnectRemote;
    property OnDisconnect :TCcNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnCommit :TCcNotifyEvent read FOnCommit write FOnCommit;
    property OnProgress :TCcNotifyEvent read FOnProgress write FOnProgress;
    property OnReplicateProc :TCcNameEvent read FOnReplicateProc write FOnReplicateProc;
    property OnQueryDone :TCcQueryEvent read FOnQueryDone write FOnQueryDone;
    property OnLogLoaded: TCcNotifyEvent read FOnLogLoaded write FOnLogLoaded;
    property BeforeReplicate: TCcNotifyEvent read FBeforeReplicate write FBeforeReplicate;
    property OnConnectionLost: TCcLostConnectEvent read FOnConnectionLost write FOnConnectionLost;
  end;

  TCcLogFileList = class;

  TCcLogConfigListener = class(TCcComponent)
  private
    FLogList:TCcLogFileList;
  protected
    procedure LoadConfig;override;
    procedure SaveConfig;override;
    procedure DeleteConfig;override;
    constructor Create(LogList:TCcLogFileList);
  end;

  //A log file dataset, created automatically by TCcLogFileList
  TCcLogFile = class(TCcMemoryData)
  private
    FLogFileList:TCcLogFileList;
    FStreamed: Boolean;
    procedure LoadFields;
  protected
    procedure Loaded;override;
    procedure InternalPost;override;
  public
    constructor Create(AOwner: TComponent);override;
  end;

  TCcLogListEvent = procedure(Sender:TCcLogFile; ConfigName:Integer) of object;

  //TCcLogFileList is a TDataSet descendant designed to internally hold a list
  //of replication log files (datasets), while displaying at all times the
  //contents of the currently selected configuration in the associated TCcConfigStorage.
  TCcLogFileList = class(TCcMemoryData)
  private
    FLogs :TStringList;
    FConfigName: Integer;
    FConfigListener :TCcLogConfigListener;
    FOnLogLine: TCcLogListEvent;
    procedure SetConfigName(const Value: Integer);
    function GetLog(ConfigName: Integer): TCcLogFile;
    function GetCurrentLog: TCcLogFile;
    function GetConfigStorage: TCcConfigStorage;
    procedure SetConfigStorage(const Value: TCcConfigStorage);
    function GetConfigNames(Log: TCcLogFile): String;
  protected
    procedure DeleteConfig;
    property CurrentLog: TCcLogFile read GetCurrentLog;
  protected
    procedure AddLogFile(nConfigName:Integer);
  public
    procedure Clear;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property ConfigName: Integer read FConfigName write SetConfigName;
    property Logs[ConfigName:Integer]: TCcLogFile read GetLog;
    property ConfigNames[Log: TCcLogFile]:String read GetConfigNames;
  published
    property Active;
    property ConfigStorage: TCcConfigStorage read GetConfigStorage write SetConfigStorage;
    property OnLogLine:TCcLogListEvent read FOnLogLine write FOnLogLine;
  end;

  procedure Register;

implementation

uses Sysutils;

constructor TCcReplicatorList.Create(AOwner:TComponent);
begin
  inherited;
  FReplicatorList := TStringList.Create();
end;

destructor TCcReplicatorList.Destroy;
var
  i:Integer;
begin
  for i:=FReplicatorList.Count-1 downto 0 do
    if Assigned(FReplicatorList.Objects[i]) then begin
        FreeReplicator(TCcReplicator(FReplicatorList.Objects[i]));
        FReplicatorList.Objects[i] := nil;
      end;
  FReplicatorList.Free;
  inherited;
end;

function TCcReplicatorList.GetReplicator(ConfigID:Integer): TCcReplicator;
var
  i:Integer;
begin
  Result := nil;
  i:=FReplicatorList.IndexOf(IntToStr(ConfigID));
  if i>-1 then
    Result := (FReplicatorList.Objects[i] as TCcReplicator);
end;

procedure TCcReplicatorList.SetReplicator(ConfigID:Integer; Repl:TCcReplicator);
var
  i:Integer;
begin
  if Assigned(Repl) then begin
    with Repl do begin
      OnFinished := Self.OnFinished;
      OnRowReplicated := Self.OnRowReplicated;
      OnRowBeforeReplicate := Self.OnRowBeforeReplicate;
      OnRowReplicating := Self.OnRowReplicating;
      OnConflict := Self.OnConflict;
      OnReplicationError := Self.OnReplicationError;
      OnException := Self.OnException;
      OnAbort := Self.OnAbort;
      OnEmptyLog := Self.OnEmptyLog;
      OnGetFields := Self.OnGetFields;
      OnTableBegin := Self.OnTableBegin;
      OnTableEnd := Self.OnTableEnd;
      OnGenReplError := Self.OnGenReplError;
      OnGenReplicating := Self.OnGenReplicating;
      OnKeySynchronized := Self.OnGenReplicated;
      OnAutoCommit := Self.OnAutoCommit;
      OnConnectLocal := Self.OnConnectLocal;
      OnConnectRemote := Self.OnConnectRemote;
      OnDisconnect := Self.OnDisconnect;
      OnCommit := Self.OnCommit;
      OnProgress := Self.OnProgress;
      OnReplicateProc := Self.OnReplicateProc;
      OnQueryDone := Self.OnQueryDone;
      OnLogLoaded := Self.OnLogLoaded;
      BeforeReplicate := Self.BeforeReplicate;
      OnConnectionLost := Self.OnConnectionLost;
//      RemoteDB := Self.RemoteDB;
//      RemoteDB := Self.LocalDB;
    end;
    FReplicatorList.AddObject(IntToStr(ConfigID), Repl);
  end
  else begin
    i := FReplicatorList.IndexOf(IntToStr(ConfigID));
    if i > -1 then
      FreeReplicator(TCcReplicator(FReplicatorList.Objects[i]));
      FReplicatorList.Delete(i);
  end;
end;

procedure TCcReplicatorList.LoadConfig;

  procedure SetConnection(node: TCcNode; cnx: TCcConnectionConfig);
  begin
    if not Assigned(cnx.Connection) then Exit;
    
    if Assigned(node.Connection) and not node.Connection.ClassNameIs(cnx.Connection.ClassName) then begin
      node.Connection.Free;
      node.Connection := nil;
    end;

    if not Assigned(node.Connection) then
      node.Connection := cnx.LoadConnectionFromField(nil)
    else
      cnx.LoadConnectionFromField(node.Connection);
      //TCcConnectionClass(conn.ClassType).Create(Self);
//    node.Connection.Assign(conn);
  end;

begin
  inherited;
  if ConfigStorage.ConfigID = 0 then Exit;
  if not Assigned(CurrentReplicator) then
    Replicators[ConfigStorage.ConfigID] := TCcReplicator.Create(Self, ConfigStorage.FieldByName('ConfigName').AsString);

//  with CurrentReplicator do
//  begin
//    if Assigned(DBProvider) then
//      Disconnect;
    SetConnection(CurrentReplicator.LocalNode, ConfigStorage.LocalDB);
    CurrentReplicator.LocalNode.Name := ConfigStorage.FieldByName('LocalNodeName').AsString;
    SetConnection(CurrentReplicator.RemoteNode, ConfigStorage.RemoteDB);
    CurrentReplicator.RemoteNode.Name := ConfigStorage.FieldByName('RemoteNodeName').AsString;
    CurrentReplicator.AutoReplicate.Frequency := ConfigStorage.FieldByName('AutoReplicateFrequency').AsInteger;
    CurrentReplicator.AutoReplicate.Enabled := (CurrentReplicator.AutoReplicate.Frequency > 0);
    CurrentReplicator.RecordChunks := ConfigStorage.FieldByName('RecordChunks').AsInteger;
		CurrentReplicator.AutoPriority := false;
		CurrentReplicator.HarmonizeFields := True;
//  end;
end;

procedure TCcReplicatorList.SaveConfig;
begin
  inherited;
//  if (PreviousConfigName <> ConfigStorage.ConfigName) and (PreviousConfigName <> '') then
//    with FReplicatorList do
//      Strings[IndexOf(PreviousConfigName)] := ConfigStorage.ConfigName;
  LoadConfig;
end;

function TCcReplicatorList.GetReplicatorCount: Integer;
begin
  Result := FReplicatorList.Count;
end;

function TCcReplicatorList.GetCurrentReplicator: TCcReplicator;
begin
  Result := Replicators[ConfigStorage.ConfigID];
end;

procedure TCcReplicatorList.EditConfig;
begin
//  PreviousConfigName := ConfigStorage.ConfigName;
end;

procedure TCcReplicatorList.DeleteConfig;
var
  nIndex:Integer;
begin
  with FReplicatorList do begin
    nIndex := IndexOf(IntToStr(ConfigStorage.ConfigID));
    if nIndex > -1 then begin
      if (Objects[nIndex] as TCcReplicator).Busy then
        raise Exception.Create('Cannot delete a configuration during replication!');
      FreeReplicator(TCcReplicator(Objects[nIndex]));
      Delete(nIndex);
    end;
  end;
end;

procedure TCcReplicatorList.FreeReplicator(repl: TCcReplicator);
begin
  if Assigned(repl) then
    with repl do begin
      if Assigned(LocalNode.Connection) then
        LocalNode.Connection.Free;
      if Assigned(RemoteNode.Connection) then
        RemoteNode.Connection.Free;
    end;
end;

function TCcReplicatorList.GetConfigNames(Repl: TCcReplicator): String;
var
  nIndex :Integer;
begin
  nIndex := FReplicatorList.IndexOfObject(Repl);
  if nIndex > -1 then
    Result := FReplicatorList[nIndex]
  else
    Result := '';
end;

{ TCcLogFileList }

constructor TCcLogFileList.Create(AOwner: TComponent);
begin
  inherited;
  FLogs := TStringList.Create;
  FConfigListener := TCcLogConfigListener.Create(Self);
end;

destructor TCcLogFileList.Destroy;
var
  i:Integer;
begin
  FConfigListener.Free;
  for i:=FLogs.Count-1 downto 0 do
    if Assigned(FLogs.Objects[i]) then begin
        FLogs.Objects[i].Free;
        FLogs.Objects[i] := nil;
      end;
  FLogs.Free;
  inherited;
end;

function TCcLogFileList.GetConfigNames(Log: TCcLogFile): String;
var
  nIndex :Integer;
begin
  nIndex := FLogs.IndexOfObject(Log);
  if nIndex > -1 then
    Result := FLogs[nIndex]
  else
    Result := '';
end;

procedure TCcLogFileList.SetConfigName(const Value: Integer);
begin
  if Assigned(Logs[Value]) then begin
    FConfigName := Value;
    LoadFromDataSet(CurrentLog, 0, lmCopy);
  end;
end;

function TCcLogFileList.GetConfigStorage: TCcConfigStorage;
begin
  Result := FConfigListener.ConfigStorage;
end;

procedure TCcLogFileList.SetConfigStorage(const Value: TCcConfigStorage);
begin
  FConfigListener.ConfigStorage := Value;
end;

procedure TCcLogFileList.DeleteConfig;
var
  nIndex:Integer;
begin
  with FLogs do begin
    nIndex := IndexOf(IntToStr(ConfigStorage.ConfigID));
    if nIndex > -1 then begin
      Objects[nIndex].Free;
      Delete(nIndex);
    end;
  end;
end;

//Return the current log file dataset, that is, the one that's visible to TDataSources.
//The other datasets are still accessible programatically, using the Logs property.
function TCcLogFileList.GetCurrentLog: TCcLogFile;
begin
  Result := Logs[ConfigName];
end;

function TCcLogFileList.GetLog(ConfigName: Integer): TCcLogFile;
var
  nIndex:Integer;
begin
  nIndex := FLogs.IndexOf(IntToStr(ConfigName));
  if nIndex > -1 then
    Result := FLogs.Objects[nIndex] as TCcLogFile
  else
    Result := nil;
end;

procedure TCcLogFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to FLogs.Count-1 do 
    with (FLogs.Objects[i] as TCcLogFile) do begin
      Close;
      Open;
    end;
  Close;
  Open;
end;

procedure TCcLogFileList.AddLogFile(nConfigName: Integer);
var
  LogFile: TCcLogFile;
begin
  LogFile := TCcLogFile.Create(Self);
  LogFile.Active := True;
  FLogs.AddObject(IntToStr(nConfigName), LogFile);
end;

{ TCcLogConfigListener }

constructor TCcLogConfigListener.Create(LogList: TCcLogFileList);
begin
  inherited Create(LogList);
  FLogList := LogList;
end;

procedure TCcLogConfigListener.DeleteConfig;
begin
  inherited;
  FLogList.DeleteConfig;
end;

procedure TCcLogConfigListener.LoadConfig;
begin
  if ConfigStorage.ConfigID = 0 then Exit;
  with FLogList do begin
    if not Assigned(Logs[Self.ConfigStorage.ConfigID]) then
      AddLogFile(Self.ConfigStorage.ConfigID);
    ConfigName := Self.ConfigStorage.ConfigID;
  end;
end;

procedure TCcLogConfigListener.SaveConfig;
begin
end;

{ TCcLogFile }

constructor TCcLogFile.Create(AOwner: TComponent);
begin
  inherited;
  FLogFileList := (AOwner as TCcLogFileList);
  if csLoading in ComponentState then
    FStreamed := True
  else
    LoadFields;
end;

procedure TCcLogFile.InternalPost;
var
  i:Integer;
begin
  inherited;
  with FLogFileList do begin
    if Assigned(OnLogLine) then
      OnLogLine(Self, StrToInt(ConfigNames[Self]));
    if CurrentLog = Self then begin
      Append;
      for i:=0 to Fields.Count-1 do
        Fields[i].Value := Self.Fields[i].Value;
      Post;
    end;
  end;
end;

procedure TCcLogFile.Loaded;
begin
  inherited;
  if FStreamed then LoadFields;
  FStreamed := False;
end;

procedure Register;
begin
  RegisterComponents('CopyCat', [TCcReplicatorList, TCcLogFileList]);
end;

procedure TCcLogFile.LoadFields;
begin
  AddField('Config_Name', ftInteger, 0);
  AddField('Table_Name', ftString, 50);
  AddField('Origin', ftString, 50);
  AddField('Operation_Type', ftString, 50);
	AddField('Description', ftMemo, 0);
	AddField('Log_Date', ftDateTime, 0);
end;

end.
