unit CctConnection;

interface

uses Classes, TestFramework, CcProviders, dtConnector, CctConnectors;

type

TTestProc = reference to procedure(connection: TdmtConnector);

TCcTestCase = class(TTestCase)
  private
  protected
    procedure ExecuteTestForAllConnections(proc: TTestProc);
    procedure DoCreateTable(conn: TdmtConnector);
  public
    procedure SetUp;override;
    procedure TearDown;override;
end;

TCctConnection = class(TCcTestCase)
  published
    procedure TestConnect;
    procedure TestCreateTable;
    procedure TestWriteAndReadBack;
end;

implementation

uses Sysutils, Forms, DB, Variants;

{ TCctConnection }

procedure TCcTestCase.SetUp;
begin
  inherited;
end;

procedure TCcTestCase.TearDown;
begin
  inherited;
end;

procedure TCcTestCase.DoCreateTable(conn: TdmtConnector);
var
  q: TCcQuery;
  I: Integer;
  slFieldTypes: TStringList;
begin
  q := TCcQuery.Create(Application);
  q.Connection := conn.Connection;
  q.SQL.Text := 'Create table test (id_field integer not null';

  slFieldTypes := TStringList.Create;
  try
    conn.GetSupportedFieldTypes(slFieldTypes);
    for I := 0 to slFieldTypes.Count-1 do begin
      q.SQL.Text := q.SQL.Text + ', field_' + slFieldTypes.Values[slFieldTypes.Names[i]] + ' ' + slFieldTypes.Names[i];
    end;
  finally
    slFieldTypes.Free;
  end;
  q.SQL.Text := q.SQL.Text + ', primary key(id_field))';
  q.Exec;
  conn.Connection.Commit;
end;

procedure TCcTestCase.ExecuteTestForAllConnections(proc :TTestProc);
var
  i, j: Integer;
  dmConn : TdmtConnector;
  connList: TCctConnectorList;
  ConnectionList : TCctConnectorLists;
begin
  ConnectionList := CreateArrayOfConnectors;
  try
    for I := 0 to ConnectionList.Count -1 do begin
      for J := 0 to ConnectionList.ConnectorList[I].Count-1 do begin
        dmConn := ConnectionList.ConnectorList[I].Connector[J];
        try
          proc(dmConn);
        except
          on E: Exception do begin
            Fail('Error while testing for connector ' + dmConn.Description + #13#10 + e.Message);
          end;
        end;
      end;
    end;
  finally
    //Delete databases and free everything
    for i:= ConnectionList.Count-1 downto 0 do begin
      connList := ConnectionList[i];
      for j:= connList.Count-1 downto 0 do begin
        dmconn := connList[j];
        dmconn.Free;
        connList.Delete(j);
      end;
      connList.Free;
      ConnectionList.Delete(i);
    end;
    ConnectionList.Free;
  end;
end;

procedure TCctConnection.TestConnect;
begin
  ExecuteTestForAllConnections(
    procedure (connector: TdmtConnector)
    begin
      with connector.Connection do begin
        Connect;
        CheckTrue(Connected, 'Connected not set after connecting : ' + connector.Description);
        Disconnect;
        CheckFalse(Connected, 'Connected not reset after disconnecting : ' + connector.Description);
      end;
    end
  );
end;

procedure TCctConnection.TestCreateTable;
begin
  ExecuteTestForAllConnections(
    procedure (connector: TdmtConnector)
    begin
      connector.Connection.Connect;
      DoCreateTable(connector);
      connector.Connection.Disconnect;
    end
  );
end;

procedure TCctConnection.TestWriteAndReadBack;
begin
  ExecuteTestForAllConnections(
    procedure (connector: TdmtConnector)
    var
      testData: Variant;
      slDataTypes :TStringList;
      id: Integer;

      procedure InsertData;
      var
        qInsert : TCcQuery;
        cSQL :String;
        i : Integer;
      begin
        qInsert := TCcQuery.Create(Application);
        qInsert.Connection := connector.Connection;
        try
          cSQL := 'insert into test (id_field';
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
            qInsert.Param['field_' + slDataTypes.Values[slDataTypes.Names[i]]].SetValueAsType(testData[i], TFieldType(StrToInt(slDataTypes.Values[slDataTypes.Names[i]])));
          end;
          qInsert.Exec;
          connector.Connection.Commit;
          CheckEquals(1, qInsert.RowsAffected, 'RowsAffected incorrect after insert');
        finally
          qInsert.Free;
        end;
      end;

      procedure SelectData;
      var
        qSelect : TCcQuery;
        cSQL :String;
        i: Integer;
        ft: TVarType;
        b: Boolean;
        val, val2 : Variant;
        str1, str2:String;
      begin
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
            else
              b := (VarCompareValue(testData[i],val) = vrEqual);
            Check(b, 'Incorrect data retreived from table for field type ' + slDataTypes.Names[i]
              + #13#10 + 'Expected : ' + VarToStr(testData[i]) + #13#10 + 'Actual : ' + qSelect.Field['field_' + slDataTypes.Values[slDataTypes.Names[i]]].AsString);
          end;
        finally
          qSelect.Free;
        end;
      end;

    begin
      slDataTypes := TStringList.Create;
      try
        connector.Connection.Connect;

        //Create table
        DoCreateTable(connector);

        //Get data types supported by connector
        slDataTypes.Clear;
        connector.GetSupportedFieldTypes(slDataTypes);

        //Create test data for each data type
        testData := CreateTestDataArray(slDataTypes);

        //Insert the data into the table
        InsertData;

        //Read it back and check that it's correct
        SelectData;

        connector.Connection.Disconnect;
      finally
        slDataTypes.Free;
      end;
    end
  );
end;

initialization
  RegisterTest(TCctConnection.Suite);

end.
