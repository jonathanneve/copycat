unit CcTest;

interface

uses DB, CcProviders, CcConf, CcReplicator;

type

TCcRecordCountTest = (rcZero, rcZeroOrOne, rcExactlyOne, rcAtLeastOne, rcAny);

TCcTest = class
  private
    FConnection: TCcConnection;
    procedure DoCheckQuery(qry: TCcQuery; paramNames: array of String;
      paramTypes: array of TFieldType; paramValues: array of Variant; fieldNames: array of String;
      fieldTypes: array of TFieldType; recCount: TCcRecordCountTest);
    procedure CheckQuery(cQryName: String; paramNames: array of String; paramTypes: array of TFieldType;
      paramValues: array of Variant; fieldNames: array of String; fieldTypes: array of TFieldType; recCount: TCcRecordCountTest);
    procedure CheckMetaQuery(qryType: TCcMetaSQL; paramNames: array of String;
      paramTypes: array of TFieldType; paramValues: array of Variant; fieldNames: array of String;
      fieldTypes: array of TFieldType; recCount: TCcRecordCountTest);
  public
    procedure TestReplication(conf: TCcConfig; repl: TCcReplicator);
    procedure TestConnection;
    procedure TestBlob(Data: String);
    procedure TestConfig(conf: TCcConfig);
    constructor Create(Conn: TCcConnection);
end;

implementation

uses Sysutils;

const
  TestData: String = 'AZFE a"é)à0ç*ùm:és???¶???&?ezfß!Ó%????';

{ TCcConnectionTest }

constructor TCcTest.Create(Conn: TCcConnection);
begin
  FConnection := Conn;
end;

procedure TCcTest.CheckMetaQuery(qryType: TCcMetaSQL; paramNames : array of String;
  paramTypes: array of TFieldType; paramValues: array of Variant; fieldNames : array of String;
  fieldTypes: array of TFieldType; recCount: TCcRecordCountTest);
begin
  DoCheckQuery(FConnection.MetaQuery[qryType], paramNames, paramTypes, paramValues, fieldNames, fieldTypes, recCount);
end;

procedure TCcTest.CheckQuery(cQryName: String; paramNames : array of String;
  paramTypes: array of TFieldType; paramValues: array of Variant; fieldNames : array of String;
  fieldTypes: array of TFieldType; recCount: TCcRecordCountTest);
begin
  DoCheckQuery(FConnection.Query[cQryName], paramNames, paramTypes, paramValues, fieldNames, fieldTypes, recCount);
end;

procedure TCcTest.DoCheckQuery(qry :TCcQuery; paramNames : array of String;
  paramTypes: array of TFieldType; paramValues: array of Variant; fieldNames : array of String;
  fieldTypes: array of TFieldType; recCount: TCcRecordCountTest);
var
  i: Integer;
  cContext: String;
begin
  if High(fieldNames) <> High(fieldTypes) then
    raise Exception.Create('Field names and field types must have same number of elements!');
  if High(paramNames) <> High(paramTypes) then
    raise Exception.Create('Param names and param types must have same number of elements!');

  cContext := 'CopyCat query test failed!'#13#10'Query: ' + qry.Name;
  cContext := cContext + ''#13#10'Fields: ';
  for i := 0 to High(fieldNames) do
    cContext := cContext + fieldNames[i] + '(' + IntToStr(Integer(fieldTypes[i])) + '); ';
  cContext := cContext + ''#13#10'Params: ';
  for i := 0 to High(paramNames) do
    cContext := cContext + paramNames[i] + '(' + IntToStr(Integer(paramTypes[i])) + '); ';
  cContext := cContext + ''#13#10'Expected record count: ';
  if recCount = rcZero then
    cContext := cContext + '0'
  else if recCount = rcZeroOrOne then
    cContext := cContext + '0 or 1'
  else if recCount = rcExactlyOne then
    cContext := cContext + '1'
  else if recCount = rcAtLeastOne then
    cContext := cContext + '>=1'
  else if recCount = rcAny then
    cContext := cContext + 'any';

  cContext := cContext + #13#10#13#10;

  with qry do begin
    if High(paramNames) = -1 then
      Assert(ParamCount = 0, cContext + 'Invalid param count ' + IntToStr(ParamCount))
    else
      for i := 0 to High(paramNames)  do begin
        Assert(ParamExists(paramNames[i]), cContext + 'Undefined parameter: ' + paramNames[i]);
        Assert(Param[paramNames[i]].DataType = paramTypes[i], cContext + 'Param ' + paramNames[i] + ' is of wrong datatype (' + IntToStr(Integer(paramTypes[i])) + ')');
        Param[paramNames[i]].Value := paramValues[i];
      end;

    Exec;

    if High(fieldNames) = -1 then
      Assert(FieldCount = 0, cContext + 'Invalid field count ' + IntToStr(FieldCount))
    else
      for i := 0 to High(fieldNames)  do begin
        Assert(FieldExists(fieldNames[i]), cContext + 'Undefined field: ' + fieldNames[i]);
        Assert(Field[fieldNames[i]].DataType = fieldTypes[i], cContext + 'Field ' + fieldNames[i] + ' is of wrong datatype (' + IntToStr(Integer(Field[fieldNames[i]].DataType)) + ')');
      end;

    if recCount = rcZero then
      Assert(RecordCount = 0, cContext + 'Incorrect RecordCount = ' + IntToStr(RecordCount))
    else if recCount = rcZeroOrOne then
      Assert((RecordCount = 0) or (RecordCount = 1), cContext + 'Incorrect RecordCount = ' + IntToStr(RecordCount))
    else if recCount = rcExactlyOne then
      Assert(RecordCount = 1, cContext + 'Incorrect RecordCount = ' + IntToStr(RecordCount))
    else if recCount = rcAtLeastOne then
      Assert(RecordCount >=1, cContext + 'Incorrect RecordCount = ' + IntToStr(RecordCount));
    Close;
  end;
end;

procedure TCcTest.TestBlob(Data: String);
begin
 FConnection.Connect;
  with FConnection.Query['blob_test'] do begin
    Close;
    SQL := 'create table rpl$test (data image)';
    Exec;

    FConnection.Commit;
    FConnection.StartTransaction;

    try
      Close;
      SQL := 'insert into rpl$test (data) values (:data)';
      Param['data'].Value := Data;
      Exec;

      Close;
      SQL := 'select data from rpl$test';
      Exec;
      Assert(Field['data'].Value = Data, 'Blob data read back incorrectly!');

      FConnection.Rollback;
    finally
      FConnection.Disconnect;
      FConnection.Connect;
      FConnection.StartTransaction;
      Close;
      SQL := 'drop table rpl$test';
      Exec;
      FConnection.Commit;
      FConnection.StartTransaction;
    end;
  end;
end;

procedure TCcTest.TestConfig(conf: TCcConfig);
begin
  conf.Connection := FConnection;
  conf.Connect;
  conf.GenerateTriggers('rpl$conflicts');
//  conf.GrantAll('SYSDBA');
  conf.Disconnect;
end;

procedure TCcTest.TestConnection;
var
  nTest: Integer;
begin
  with FConnection do begin
    Connect;

    Query['recordCountTest'].SQL := 'select table_name from rpl$tables where 1=0';
    CheckQuery('recordCountTest', [], [], [], ['table_name'], [ftString], rcZero);

    Query['recordCountTest2'].SQL := 'select table_name from rpl$tables';
    CheckQuery('recordCountTest2', [], [], [], ['table_name'], [ftString], rcAtLeastOne);

    with Query['paramTest'] do begin
      Close;
      SQL := 'select * from rpl$log where login = :login and operation_date = :op_date';
      Prepare;

      Assert(ParamCount = 2);
      Assert(Param['login'].DataType = ftString);
      Assert(Param['op_date'].DataType in [ftDate, ftTime, ftDateTime]);

      Param['login'].Value := TestData;
      Assert(Param['login'].Value = TestData);
      UnPrepare;
    end;

    CheckMetaQuery(sqlTables, [], [], [], ['table_name'], [ftString], rcAtLeastOne);
    CheckMetaQuery(sqlTableFields, ['table_name'], [ftString], ['RPL$TABLES'], ['field_name'], [ftString], rcAtLeastOne);
    CheckMetaQuery(sqlUpdatableFields, ['table_name'], [ftString], ['RPL$TABLES'], ['field_name'], [ftString], rcAtLeastOne);
    CheckMetaQuery(sqlTablePKs, ['table_name'], [ftString], ['RPL$TABLES'], ['pk_name'], [ftString], rcAtLeastOne);
    CheckMetaQuery(sqlProcedures, [], [], [], ['procedure_name'], [ftString], rcAny);
//    CheckMetaQuery(sqlGetGenValue, [], [], [], ['code_value'], [ftInteger], rcExactlyOne);
//    CheckMetaQuery(sqlGenerators, [], [], [], ['generator_name'], [ftString], rcAny);
    CheckMetaQuery(sqlFindTrigger, ['trigger_name'], [ftString], ['RPL$CONFLICTS'], ['rec_count'], [ftInteger], rcZeroOrOne);
    CheckMetaQuery(sqlFindTableField, ['table_name', 'field_name'], [ftString, ftString], ['RPL$TABLES', 'table_name'], ['rec_count'], [ftInteger], rcExactlyOne);
    CheckMetaQuery(sqlFindProc, ['proc_name'], [ftString], ['RPL$EXECUTE_PROCEDURE'], ['rec_count'], [ftInteger], rcExactlyOne);
    CheckMetaQuery(sqlFindTable, ['table_name'], [ftString], ['RPL$TABLES'], ['rec_count'], [ftInteger], rcZeroOrOne);

    nTest := Gen_ID('GEN_RPL$CONFLICTS', 1);// 'select table_name from rpl$tables where 1=0';

    Disconnect;
  end;
end;

procedure TCcTest.TestReplication(conf: TCcConfig; repl: TCcReplicator);

  procedure InitReplicationTest(nKey: Integer);
  begin
    if not conf.Connection.DBAdaptor.TableExists('replication_test') then
      with conf.Connection.Query['ReplTest.CreateTable'] do begin
        Close;
        SQL := 'create table replication_test (' +
          conf.Connection.DBAdaptor.DeclareField('code', ftInteger, 0, True, True, False) + ', ' +
          conf.Connection.DBAdaptor.DeclareField('blobdata', ftBlob, 0, False, False, False) + ')';
        Exec;
      end;

    conf.Connection.Commit;
    conf.GenerateTriggers('replication_test');
    with conf.Connection.Query['ReplTest.DeleteData'] do begin
      Close;
      SQL := 'delete from replication_test';
      Exec;
    end;

    with conf.Connection.Query['ReplTest.InsertData'] do begin
      Close;
      SQL := 'insert into replication_test (code, blobdata) values (:code, :blobdata)';
      Param['code'].Value := nKey;
      Param['blobdata'].Value := TestData;
      Exec;
    end;
  end;

begin
  conf.Disconnect;
  conf.Connection := repl.LocalNode.Connection;
  conf.Connect;
  InitReplicationTest(1);

  conf.Disconnect;
  conf.Connection := repl.RemoteNode.Connection;
  conf.Connect;
  InitReplicationTest(2);

  conf.Connection.Commit;
  conf.Connection.Disconnect;

  repl.Replicate;
end;

end.
