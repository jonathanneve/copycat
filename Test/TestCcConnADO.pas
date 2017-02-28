unit TestCcConnADO;

interface

uses
  TestFramework, Classes, DB, CcProviders, ADODB, Dialogs, Sysutils, CcConnADO,
  TestCcReplicator, TestCcProviders;
type
  TestTCcConnectionADO = class(TestTCcConnection)
  private
    FConnection: TCcConnection;
    function CreateConnection: TCcConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
  end;

  // Test methods for class TCcConnectionADO
  TestTCcReplicatorADO = class(TestTCcReplicator)
  published
    procedure TestReplicate;
  end;

implementation

uses
  Forms, CcSQLServer;

{ TestTCcConnectionADO }

function TestTCcConnectionADO.CreateConnection: TCcConnection;
var
  conn: TCcConnectionADO;
begin
  conn := TCcConnectionADO.Create(Application);
  conn.DBType := 'MSSQL';
  conn.DBVersion := 'MSSQL2000';
  conn.ConnectionString := 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CopyCatTestsLocal;Data Source=KANGAROO\SQLEXPRESS';
  Result := conn;
end;

procedure TestTCcConnectionADO.SetUp;
begin
  FConnection := CreateConnection;
  ConfigDB(FConnection);
end;

procedure TestTCcConnectionADO.TearDown;
begin
  FConnection.Disconnect;
  FConnection.Free;
end;

procedure TestTCcConnectionADO.TestConnect;
begin
  TestConnection(FConnection);
end;

{ TestTCcReplicatorADO }

procedure TestTCcReplicatorADO.TestReplicate;
var
  conn: TCcConnectionADO;
begin
  conn := TCcConnectionADO.Create(Application);
  conn.DBType := 'MSSQL';
  conn.DBVersion := 'MSSQL2000';
  conn.ConnectionString := 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CopyCatTestsLocal;Data Source=KANGAROO\SQLEXPRESS';
  FCcReplicator.LocalNode.Connection := conn;

  conn := TCcConnectionADO.Create(Application);
  conn.DBType := 'MSSQL';
  conn.DBVersion := 'MSSQL2000';
  conn.ConnectionString := 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=CopyCatTestsRemote;Data Source=KANGAROO\SQLEXPRESS';
  FCcReplicator.RemoteNode.Connection := conn;

  TestReplication;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCcConnectionADO.Suite);
  RegisterTest(TestTCcReplicatorADO.Suite);
end.

