unit CctReplicator;

interface

uses Classes, TestFramework, CctConnection, Sysutils, CcReplicator, CcConf, Forms, dtConnector;

type

TReplTestProc = reference to procedure(connLocal: TdmtConnector; connRemote: TdmtConnector);

TCctReplicator = class(TCcTestCase)
  private
    CcReplicator: TCcReplicator;
    configLocal, configRemote : TCcConfig;
    id: Integer;
    procedure DoReplicationTest(proc: TReplTestProc);
    function InsertRandomData(connector: TdmtConnector): Variant;
    procedure CheckDataEqual(connector: TdmtConnector; testData: Variant);
    procedure OnException(Sender: TObject; e: Exception);
    procedure OnReplicationError(Sender :TObject; e :Exception; var CanContinue :Boolean);
//    procedure ExecuteReplTestForAllConnections(proc: TReplTestProc);
    procedure SetupDB(conn: TdmtConnector; cNodes: String);
  public
    procedure SetUp;override;
    procedure TearDown;override;
  published
    procedure TestReplicate;
end;

implementation

{ TCctReplicator }

uses CCat, CcProviders, CctConnectors, DB, Variants;

procedure TCctReplicator.SetUp;
begin
  inherited;
  CcReplicator := TCcReplicator.Create(Application);
  CcReplicator.HarmonizeFields := True;
end;

procedure TCctReplicator.TearDown;
begin
  CcReplicator.Free;
  inherited;
end;

{
procedure TCctReplicator.ExecuteReplTestForAllConnections(proc :TReplTestProc);
var
  i, j, k, l: Integer;
  dmConn, dmConnLocal, dmConnRemote : TdmtConnector;
  connList: TCctConnectorList;
  ConnectionListLocal, ConnectionListRemote : TCctConnectorLists;
begin
  ConnectionListLocal := CreateArrayOfConnectors;
  ConnectionListRemote := CreateArrayOfConnectors;
  try
    for I := 0 to ConnectionListLocal.Count -1 do begin
      for J := 0 to ConnectionListLocal.ConnectorList[I].Count-1 do begin
        dmConnLocal := ConnectionListLocal.ConnectorList[I].Connector[J];
        for K := 0 to ConnectionListRemote.Count -1 do begin
          for L := 0 to ConnectionListRemote.ConnectorList[K].Count-1 do begin
            dmConnRemote := ConnectionListRemote.ConnectorList[K].Connector[L];
            try
              proc(dmConnLocal, dmConnRemote);
            except
              on E: Exception do begin
                Fail('Error while testing for connector ' + dmConn.Description + #13#10 + e.Message);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    //Delete databases and free everything
    for i:= ConnectionListLocal.Count-1 downto 0 do begin
      connList := ConnectionListLocal[i];
      for j:= connList.Count-1 downto 0 do begin
        dmconn := connList[j];
        dmconn.Free;
        connList.Delete(j);
      end;
      connList.Free;
      ConnectionListLocal.Delete(i);
    end;
    ConnectionListLocal.Free;

    for i:= ConnectionListRemote.Count-1 downto 0 do begin
      connList := ConnectionListRemote[i];
      for j:= connList.Count-1 downto 0 do begin
        dmconn := connList[j];
        dmconn.Free;
        connList.Delete(j);
      end;
      connList.Free;
      ConnectionListRemote.Delete(i);
    end;
    ConnectionListRemote.Free;
  end;
end;
 }

procedure TCctReplicator.SetupDB(conn: TdmtConnector; cNodes: String);
var
  conf: TCcConfig;
begin
  conn.Connection.Connect;
  DoCreateTable(conn);
  conn.Connection.Disconnect;

  conf := TCcConfig.Create(Application);
  conf.Connection := conn.Connection;
  conf.ConfigName := conn.Connection.DBAdaptor.UnQuotedIdentifier('TEST');
  conf.Connect;
  conf.Tables.Clear;
  conf.Tables.Add.TableName := conn.Connection.DBAdaptor.UnQuotedIdentifier('TEST');
  conf.Nodes.Text := cNodes;
  conf.GenerateConfig;
  conf.Disconnect;
  conf.Free;
end;

procedure TCctReplicator.DoReplicationTest(proc: TReplTestProc);
begin
{  ExecuteReplTestForAllConnections(
    procedure (connLocal, connRemote: TdmtConnector)
    begin
      SetupDB(connLocal, 'REMOTE');
      SetupDB(connRemote, 'LOCAL');
      proc(connLocal, connRemote);
    end
  ); }

  ExecuteTestForAllConnections(
    procedure (connLocal: TdmtConnector)
    var
      config: TCcConfig;
    begin
      ExecuteTestForAllConnections(
        procedure (connRemote: TdmtConnector)
        begin
          connLocal.Reinit;
          SetupDB(connLocal, 'REMOTE');
          SetupDB(connRemote, 'LOCAL');
          proc(connLocal, connRemote);
        end
      );
    end
  );
end;


function TCctReplicator.InsertRandomData(connector: TdmtConnector): Variant;
var
  qInsert : TCcQuery;
  cSQL :String;
  i : Integer;
  slDataTypes :TStringList;
begin
  slDataTypes := TStringList.Create;
  try
    connector.GetSupportedFieldTypes(slDataTypes);
    Result := CreateTestDataArray(slDataTypes);

    qInsert := TCcQuery.Create(Application);
    qInsert.Connection := connector.Connection;
    try
      cSQL := 'insert into TEST (id_field';
      for I := 0 to slDataTypes.Count-1 do begin
        cSQL := cSQL + ', field_' + slDataTypes.Values[slDataTypes.Names[i]];
      end;
      cSQL := cSQL + ') values (:id_field';
      for I := 0 to slDataTypes.Count-1 do begin
        cSQL := cSQL + ', :field_' + slDataTypes.Values[slDataTypes.Names[i]];
      end;
      cSQL := cSQL + ')';

      qInsert.SQL.Text := cSQL;

      id := GetTestData(ftInteger);
      qInsert.Param['id_field'].Value := id;
      for I := 0 to slDataTypes.Count-1 do begin
        qInsert.Param['field_' + slDataTypes.Values[slDataTypes.Names[i]]].SetValueAsType(Result[i], TFieldType(StrToInt(slDataTypes.Values[slDataTypes.Names[i]])));
      end;
      qInsert.Exec;
      connector.Connection.Commit;
    finally
      qInsert.Free;
    end;
  finally
    slDataTypes.Free;
  end;
end;

procedure TCctReplicator.CheckDataEqual(connector: TdmtConnector; testData: Variant);
var
  qSelect : TCcQuery;
  cSQL :String;
  i: Integer;
  ft: TVarType;
  b: Boolean;
  val, val2 : Variant;
  slDataTypes :TStringList;
  str1, str2: String;
begin
  slDataTypes := TStringList.Create;
  try
    connector.GetSupportedFieldTypes(slDataTypes);
    connector.Connection.Connect;
    qSelect := TCcQuery.Create(Application);
    qSelect.SelectStatement := True;
    qSelect.Connection := connector.Connection;
    try
      qSelect.SQL.Text := 'select * from test';
      qSelect.Exec;
      CheckEquals(1, qSelect.RecordCount, 'RecordCount incorrect after select');
      CheckEquals(id, qSelect.Field['id_field'].Value, 'Incorrect data retreived from table for field id');
      for I := 0 to slDataTypes.Count-1 do begin
        ft := VarType(testData[i]);
        val := qSelect.Field['field_' + slDataTypes.Values[slDataTypes.Names[i]]].Value;
        if (qSelect.Field['field_' + slDataTypes.Values[slDataTypes.Names[i]]].DataType = ftBlob) or (qSelect.Field['field_' + slDataTypes.Values[slDataTypes.Names[i]]].DataType = ftOraBlob) then begin
          str1 := val;
          str2 := testData[i];
          b := (VarCompareValue(str2, str1) = vrEqual);
        end
        else if (ft = varDouble) or (ft = varDate) then begin
          val2 := testData[i];
          b := Abs(double(val2) - double(val)) < double(val2)/10e6 ;
        end
        else if ft = varSingle then begin
          val2 := testData[i];
          b := Abs(val2 - val) < val2/10e6 ;
        end
        else begin
          val2 := testData[i];
          b := (VarCompareValue(val2,val) = vrEqual);
        end;
        Check(b, 'Incorrect data retreived from table for field type ' + slDataTypes.Names[i]
          + #13#10 + 'Expected : ' + VarToStr(testData[i]) + #13#10 + 'Actual : ' + qSelect.Field['field_' + slDataTypes.Values[slDataTypes.Names[i]]].AsString);
      end;
    finally
      qSelect.Free;
    end;
  finally
    slDataTypes.Free;
  end;
end;

procedure TCctReplicator.OnException(Sender :TObject; e :Exception);
begin
  Fail(e.Message);
end;

procedure TCctReplicator.OnReplicationError(Sender :TObject; e :Exception; var CanContinue :Boolean);
begin
  Fail(e.Message);
end;

procedure TCctReplicator.TestReplicate;
begin
  DoReplicationTest(
    procedure (connLocal, connRemote: TdmtConnector)
    var
      testData: Variant;
    begin
      connLocal.Connection.Connect;
      with connLocal.Connection.SelectQuery['checkrplusers'] do begin
        Close;
        SQL.Text := 'select count(*) as cnt from rpl$users';
        Exec;
        CheckEquals(1, Field['cnt'].AsInteger, 'Wrong number of rows in RPL$USERS');
      end;

      testData := InsertRandomData(connLocal);

      with connLocal.Connection.SelectQuery['checkrpllog'] do begin
        Close;
        SQL.Text := 'select count(*) as cnt from rpl$log';
        Exec;
        CheckEquals(1, Field['cnt'].AsInteger, 'Wrong number of rows in RPL$LOG');
      end;
      connLocal.Connection.Disconnect;

      CcReplicator.LocalNode.Connection := connLocal.Connection;
      CcReplicator.LocalNode.Name := 'LOCAL';
      CcReplicator.RemoteNode.Connection := connRemote.Connection;
      CcReplicator.RemoteNode.Name := 'REMOTE';
      CcReplicator.CommitOnFinished := ctCommit;
      CcReplicator.OnException := OnException;
      CcReplicator.OnReplicationError := OnReplicationError;

      CcReplicator.Replicate;

      Check(CcReplicator.LastResult.RowsReplicated = 1, 'RowsReplicated = ' + IntToStr(CcReplicator.LastResult.RowsReplicated));
      Check(CcReplicator.LastResult.RowsConflicted = 0);
      Check(CcReplicator.LastResult.RowsErrors = 0);
      CheckDataEqual(connRemote, testData);

      //Replicate again to make sure there's no bouncing
      CcReplicator.Replicate;
      Check(CcReplicator.LastResult.RowsReplicated = 0);
      Check(CcReplicator.LastResult.RowsConflicted = 0);
      Check(CcReplicator.LastResult.RowsErrors = 0);

      //TODO : Check updates and deletes as well
    end
  );
end;

initialization
  RegisterTest(TCctReplicator.Suite);

end.
