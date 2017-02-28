//CopyCat replication suite<p/>
//Copyright (c) 2014 Microtec Communications<p/>
//For any questions or technical support, contact us at contact@copycat.fr
unit CcConflictMgr;

{$I CC.INC}

interface

uses
  Classes, SysUtils, CCat, CcProviders, CcReplicator;

type

{ *****************************************************************
  * Summary:                                                      *
  * CopyCat conflict management component                         *
  * Description:                                                  *
  * The TCcConflictMgr component handles conflicts that occur     *
  * during replication. It isn't currently registered on the      *
  * component palette (it's used internally by TCcReplicator),    *
  * because there are no options for the developer to set. In     *
  * future versions however, other alternative conflict           *
  * management strategies will probably be devised, and           *
  * TCcConflictMgr will then be registered.                       *
  *                                                               *
  *                                                               *
  *                                                               *
  * Before replication of two databases starts, both local and    *
  * remote replication logs are loaded, and if the same record is *
  * present in both, it is considered a conflict. This conflict   *
  * management scheme has been carefully designed so as to work   *
  * seamlessly even in setups containing multiple replication     *
  * nodes.                                                        *
  ***************************************************************** }
TCcConflictMgr = class(TCcCustomConflictMgr)
private
protected
  procedure LogConflict(Conflict: TConflictRecord);override;
  procedure CheckConflicts;override;
  procedure DeleteConflictLog(TableName, RemoteTableName: String; cPrimaryKeyValues, ChosenUser: String; ReplicationStateCondition: String);override;
  procedure DeleteLogValues(TableName, cRemoteTableName: String; cPrimaryKeyValues: String; ConflictingFields: TStringList; ChosenNode: String); override;
    procedure ResolveConflict(cTableName, cRemoteTableName, cPrimaryKeyValues: String;
      Conflict: TConflictRecord; ReplicationStateCondition: String);override;
end;

implementation

uses CcLog;

procedure TCcConflictMgr.DeleteLogValues(TableName, cRemoteTableName: String; cPrimaryKeyValues: String; ConflictingFields: TStringList; ChosenNode: String);
var
  conn: TCcConnection;
  qDeleteConflictFields: TCcQuery;
  I: Integer;
  cFields: String;
begin
  if ConflictingFields.Count > 0 then
  begin
    for I := 0 to ConflictingFields.Count-1 do begin
      if cFields <> '' then cFields := cFields + ', ';
      cFields := cFields + QuotedStr(ConflictingFields[I]);
    end;

    if ChosenNode = FReplicator.RemoteNode.Name then
      conn := FReplicator.LocalDB
    else if ChosenNode = FReplicator.LocalNode.Name then
      conn := FReplicator.RemoteDB
    else
      exit;

    qDeleteConflictFields := conn.UpdateQuery['TCcConflictMgr_qDeleteConflictLog'];
    qDeleteConflictFields.Close;
    qDeleteConflictFields.SQL.Text := 'delete from RPL$LOG_VALUES where change_number in ' +
      '(select change_number from rpl$log where login = :table_name and table_name = :table_name and primary_key_values = :primary_key_values and replication_state is null)'+
      ' and node_name = :node_name and field_name in (%fields)';
    qDeleteConflictFields.Macro['fields'].Value := cFields;
    qDeleteConflictFields.Param['primary_key_values'].Value := cPrimaryKeyValues;
    if ChosenNode = FReplicator.RemoteNode.Name then begin
      qDeleteConflictFields.Param['table_name'].Value := TableName;
      qDeleteConflictFields.Param['node_name'].Value := FReplicator.RemoteNode.Name
    end
    else begin
      qDeleteConflictFields.Param['table_name'].Value := cRemoteTableName;
      qDeleteConflictFields.Param['node_name'].Value := FReplicator.LocalNode.Name;
    end;
    qDeleteConflictFields.Exec;
  end;
end;

procedure TCcConflictMgr.DeleteConflictLog(TableName, RemoteTableName: String; cPrimaryKeyValues: String; ChosenUser: String; ReplicationStateCondition: String);
var
  DBParams, DBParams2: TCcConnection;
  qDeleteConflictLog, qUpdateConflictLog: TCcQuery;
  cTableName, cTableName2 :String;
begin
  if Trim(ChosenUser) = FReplicator.RemoteNode.Name then begin
    DBParams2 := FReplicator.RemoteDB;
    DBParams := FReplicator.LocalDB;
    cTableName := TableName;
    cTableName2 := RemoteTableName;
  end
  else if Trim(ChosenUser) = FReplicator.LocalNode.Name then begin
    DBParams2 := FReplicator.LocalDB;
    DBParams := FReplicator.RemoteDB;
    cTableName := RemoteTableName;
    cTableName2 := TableName;
  end
  else
    exit;

  qDeleteConflictLog := DBParams.UpdateQuery['TCcConflictMgr_qDeleteConflictLog'];
  qDeleteConflictLog.Close;
  if FReplicator.KeepRowsInLog then
    qDeleteConflictLog.SQL.Text := 'update RPL$LOG set replication_state = ''CONFLICT_OVERWRITTEN'' where table_name = :table_name and primary_key_values = :primary_key_values and replication_state ' + ReplicationStateCondition
  else
    qDeleteConflictLog.SQL.Text := 'delete from RPL$LOG where table_name = :table_name and primary_key_values = :primary_key_values and replication_state ' + ReplicationStateCondition;
  qDeleteConflictLog.Param['table_name'].Value := cTableName;
  qDeleteConflictLog.Param['primary_key_values'].Value := cPrimaryKeyValues;
  qDeleteConflictLog.Exec;

  qUpdateConflictLog := DBParams2.UpdateQuery['TCcConflictMgr_qUpdateConflictLog'];
  qUpdateConflictLog.Close;
  qUpdateConflictLog.SQL.Text := 'update RPL$LOG set replication_state = null where table_name = :table_name and primary_key_values = :primary_key_values and replication_state ' + ReplicationStateCondition;
  qUpdateConflictLog.Param['table_name'].Value := cTableName2;
  qUpdateConflictLog.Param['primary_key_values'].Value := cPrimaryKeyValues;
  qUpdateConflictLog.Exec;
end;

procedure TCcConflictMgr.CheckConflicts;
var
  DBParams :TCcConnection;
  lRemote :Boolean;
  qCheckConflicts: TCcQuery;
  qDeleteConflict: TCcQuery;
  qConflictLogLines: TCcQuery;
  Conflict: TConflictRecord;
  cTableName: string;

  procedure LoadLogParams(Query: TCcQuery);
  begin
    with FReplicator do begin
      Query.Param['primary_key_values'].Value := qCheckConflicts.Field['primary_key_values'].Value
    end;
  end;

  procedure UpdateLogLines(lRemote :Boolean);
  begin
    //On restaure les lignes de RPL$LOG dans la base qu'on veut garder, de sorte qu'elles se répliquent
      if (lRemote) then begin
        DBParams := FReplicator.RemoteDB;
        cTableName := qCheckConflicts.Field['remote_table_name'].AsString;
      end
      else begin
        cTableName := qCheckConflicts.Field['table_name'].AsString;
        DBParams := FReplicator.LocalDB;
      end;

      qConflictLogLines := DBParams.UpdateQuery['TCcConflictMgr_qConflictLogLines'];
      qConflictLogLines.Close;
      if qConflictLogLines.SQL.Text = '' then
        qConflictLogLines.SQL.Text := 'update RPL$LOG set conflict = :conflict, replication_state = null where table_name = :table_name and primary_key_values = :primary_key_values and replication_state = ''CONFLICT''';

      qConflictLogLines.Param['table_name'].Value := cTableName;
      qConflictLogLines.Param['conflict'].Value := 'N';
      LoadLogParams(qConflictLogLines);
      qConflictLogLines.Exec;
  end;

begin
  //On vérifie s'il y a des conflits résolus, etç si oui, on rétablit les lignes correspondantes dans RPL$LOG
  qCheckConflicts := FReplicator.LocalDB.SelectQuery['TCcConflictMgr_qCheckConflicts'];
  qCheckConflicts.Close;
  qCheckConflicts.SQL.Text := 'select * from RPL$CONFLICTS where chosen_user is not null';
  qCheckConflicts.Exec;
  while (not qCheckConflicts.Eof) do begin
    if (qCheckConflicts.Field['USER1'].AsString = FReplicator.LocalNode.Name) or
       (qCheckConflicts.Field['USER2'].AsString = FReplicator.LocalNode.Name) then
    begin
      Conflict.ChosenNode := Trim(qCheckConflicts.Field['CHOSEN_USER'].AsString);

      if (Conflict.ChosenNode <> FReplicator.RemoteNode.Name) and (Conflict.ChosenNode <> FReplicator.LocalNode.Name) then
      begin
        qCheckConflicts.Next;
        continue;
      end;

      Conflict.ChangedFields1 := TStringList.Create;
      Conflict.ChangedFields2 := TStringList.Create;
      Conflict.ConflictingFields := TStringList.Create;
      try
        Conflict.ChangedFields1.CommaText := qCheckConflicts.Field['CHANGED_FIELDS1'].AsString;
        Conflict.ChangedFields2.CommaText := qCheckConflicts.Field['CHANGED_FIELDS2'].AsString;
        ExtractItemsInCommon(Conflict.ChangedFields1, Conflict.ChangedFields2, Conflict.ConflictingFields);
        ResolveConflict(qCheckConflicts.Field['Table_name'].AsString, qCheckConflicts.Field['remote_Table_name'].AsString, qCheckConflicts.Field['primary_key_values'].AsString, Conflict, '= ''CONFLICT''');
      finally
        Conflict.ChangedFields1.Free;
        Conflict.ChangedFields2.Free;
        Conflict.ConflictingFields.Free;
      end;

      //Delete the conflict from RPL$CONFLICTS in both local and remote DBs
      qDeleteConflict := FReplicator.RemoteDB.UpdateQuery['TCcConflictMgr_qDeleteConflict'];
      qDeleteConflict.Close;
      qDeleteConflict.SQL.Text := 'delete from RPL$CONFLICTS where code = :code';
      qDeleteConflict.Param['code'].Value := qCheckConflicts.Field['code'].Value;
      qDeleteConflict.Exec;

      qDeleteConflict := FReplicator.LocalDB.UpdateQuery['TCcConflictMgr_qDeleteConflict'];
      qDeleteConflict.Close;
      qDeleteConflict.SQL.Text := 'delete from RPL$CONFLICTS where code = :code';
      qDeleteConflict.Param['code'].Value := qCheckConflicts.Field['code'].Value;
      qDeleteConflict.Exec;

      FReplicator.LocalDB.CommitRetaining;
      FReplicator.RemoteDB.CommitRetaining;
    end;

    qCheckConflicts.Next;
  end
end;

procedure TCcConflictMgr.ResolveConflict(cTableName, cRemoteTableName, cPrimaryKeyValues: String; Conflict: TConflictRecord; ReplicationStateCondition: String);
begin
  if FReplicator.MergeChangedFieldsOnConflict and FReplicator.ReplicateOnlyChangedFields then begin
    //We simply remove any fields that are conflictual and then replicate both ways
    DeleteLogValues(cTableName, cRemoteTableName, cPrimaryKeyValues, Conflict.ConflictingFields, Conflict.ChosenNode);

    //Now that we have removed conflictual fields, we can replicate remaining fields as if there were no conflict
    Conflict.ChosenNode := 'REPLICATE_BOTH_WAYS';
  end else
    //If MergeChangedFieldsOnConflict option is not set, remove the line from RPL$LOG
    DeleteConflictLog(cTableName, cRemoteTableName, cPrimaryKeyValues, Conflict.ChosenNode, ReplicationStateCondition);
end;

procedure TCcConflictMgr.LogConflict(Conflict: TConflictRecord);
var nCode: Integer;
  qCopyConflict: TCcQuery;

  function BuildWhere(DB: TCcConnection): String;
  var
    I:Integer;
  begin
    Result := '';
      for I := 0 to FReplicator.Log.Keys.Count - 1 do
        if FReplicator.Log.Keys[i].PrimaryKey then begin
          if i > 0 then Result := Result + ' and ';
          Result := Result + DB.DBAdaptor.MetaQuote(FReplicator.Log.Keys[i].KeyName) + ' = :' + DB.DBAdaptor.MetaQuote('W_CC_' + FReplicator.Log.Keys[i].KeyName);
        end;
  end;

  function FindFieldValues(node: TCcNode): String;
  var
    qGetSQL: TCcQuery;
    i: Integer;
    f: TCcField;
  begin
    Result := '';
    qGetSQL := node.Connection.SelectQuery['TCcConflictMgr_qGetSQL'];
    qGetSQL.SQL.Text := 'select * from %table_name where ' + BuildWhere(node.Connection);
    qGetSQL.Close;
    qGetSQL.Macro['table_name'].Value := FReplicator.Log.FieldByName('table_name').Value;
    FReplicator.GetWhereValues(qGetSQL);
    qGetSQL.Exec;
    while (not qGetSQL.Eof) do begin
      for i:=0 to qGetSQL.FieldCount-1 do begin
        f := qGetSQL.FieldByIndex[i];
        Result := Result + f.FieldName + '=' + f.AsString + #13#10;
      end;

      qGetSQL.Next;
      if qGetSQL.RecordCount > 1 then
        Result := Result + '-----------------------------' + #13#10;
    end;
  end;

  procedure LoadConflictParams;
  var
    cCodeField, cCodeValue: String;//(NowString: String);
  begin
    with FReplicator do begin
	    if nCode = -1 then begin
        cCodeField := '';
        cCodeValue := '';
      end
      else begin
        cCodeField := 'CODE,';
        cCodeValue := ':CODE,';
      end;

			qCopyConflict.SQL.Text := 'insert into RPL$CONFLICTS ('+ cCodeField +
      'USER1, USER2, CONFLICT_DATE, TABLE_NAME, REMOTE_TABLE_NAME, PRIMARY_KEY_VALUES, SQL1, SQL2, CHANGED_FIELDS1, CHANGED_FIELDS2) '+
      'values ('+ cCodeValue +
      ':USER1, :USER2, :conflict_date, :TABLE_NAME, :REMOTE_TABLE_NAME, :PRIMARY_KEY_VALUES, :SQL1, :SQL2, :CHANGED_FIELDS1, :CHANGED_FIELDS2)';

      qCopyConflict.Param['conflict_date'].Value := Now;
      if nCode <> -1 then
        qCopyConflict.Param['code'].Value := nCode;

      qCopyConflict.Param['user1'].Value := RemoteNode.Name;
      qCopyConflict.Param['user2'].Value := LocalNode.Name;
      qCopyConflict.Param['primary_key_values'].Value := Log.FieldByName('primary_key_values').Value;
      qCopyConflict.Param['table_name'].Value := Log.FieldByName('table_name').Value;
      qCopyConflict.Param['remote_table_name'].Value := Log.RemoteTableName;

      qCopyConflict.Param['CHANGED_FIELDS1'].Value := Conflict.ChangedFields1.CommaText;
      qCopyConflict.Param['CHANGED_FIELDS2'].Value := Conflict.ChangedFields2.CommaText;

      qCopyConflict.Param['sql1'].Value := FindFieldValues(RemoteNode);
			qCopyConflict.Param['sql2'].Value := FindFieldValues(LocalNode);
		end;
  end;
  procedure ConflictLogLines(conn: TCcConnection; remoteNodeName: String);
  var
    qConflictLogLines: TCcQuery;
  begin
    with FReplicator do begin
      qConflictLogLines := conn.UpdateQuery['TCcConflictMgr_qConflictLogLines'];
      qConflictLogLines.Close;
      if qConflictLogLines.SQL.Text = '' then
        qConflictLogLines.SQL.Text := 'update RPL$LOG set conflict = :conflict, replication_state = ''CONFLICT'' where table_name = :table_name and primary_key_values = :primary_key_values and login = :login and replication_state is null';

      if conn = FReplicator.RemoteDB then
        qConflictLogLines.Param['table_name'].Value := Log.RemoteTableName
      else
        qConflictLogLines.Param['table_name'].Value := Log.TableName;
      qConflictLogLines.Param['primary_key_values'].Value := Log.FieldByName('primary_key_values').Value;
    end;
    qConflictLogLines.Param['conflict'].Value := 'Y';
    qConflictLogLines.Param['login'].Value := remoteNodeName;
    qConflictLogLines.Exec;
  end;
begin
  with FReplicator do begin
    //Create the conflict on the server
    if RemoteDB.DBAdaptor.SupportsGenerators then
      nCode := RemoteDB.Gen_Id('GEN_RPL$CONFLICTS', 1)
    else
      nCode := -1;
    qCopyConflict := RemoteDB.UpdateQuery['TCcConflictMgr_qCopyConflict'];
    qCopyConflict.Close;
    LoadConflictParams;//(RemoteDB.DBAdaptor.CurrentTimeStampSQL);
    qCopyConflict.Exec;

    //Create the conflict locally, using the same generator code
		qCopyConflict := LocalDB.UpdateQuery['TCcConflictMgr_qCopyConflict'];
    qCopyConflict.Close;
    LoadConflictParams;//(LocalDB.DBAdaptor.CurrentTimeStampSQL);
    qCopyConflict.Exec;

    //Passer les lignes correspondantes de RPL$LOG en conflit, pour qu'elles ne soient plus répliquées
    ConflictLogLines(RemoteDB, LocalNode.Name);
    ConflictLogLines(LocalDB, RemoteNode.Name);

  end;
end;

end.
