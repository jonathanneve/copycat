unit TestCcConnIBX;

interface

uses
  TestFramework, Classes, DB, CcProviders, Dialogs, Sysutils, CcProvIBX,
  TestCcReplicator, TestCcProviders, TestCcInterbaseConn;
type
  TestTCcConnectionIBX = class(TestTCcInterbaseConnection)
  protected
    function CreateConnectionDialect1: TCcConnection;override;
    function CreateConnectionDialect3: TCcConnection;override;
  end;

  TestTCcReplicatorIBX = class(TestTCcInterbaseReplicator)
  protected
    function CreateConnectionDialect1(lLocal: Boolean): TCcConnection;override;
    function CreateConnectionDialect3(lLocal: Boolean): TCcConnection;override;
  end;

implementation

uses
  Forms;

function TestTCcConnectionIBX.CreateConnectionDialect1: TCcConnection;
var
  conn: TCcConnectionIBX;
begin
  conn := TCcConnectionIBX.Create(Application);
  conn.SQLDialect := 1;
  conn.DBName := 'localhost/3052:c:\projects\copycat\test\data\TEST_DIALECT1.FDB';
  conn.UserLogin := 'SYSDBA';
  conn.UserPassword := 'masterkey';
  conn.DBType := 'Interbase';
  conn.DBVersion := 'FB1.5';
  Result := conn;
end;

function TestTCcConnectionIBX.CreateConnectionDialect3: TCcConnection;
var
  conn: TCcConnectionIBX;
begin
  conn := TCcConnectionIBX.Create(Application);
  conn.SQLDialect := 3;
  conn.DBName := 'localhost/3052:c:\projects\copycat\test\data\TEST_DIALECT3.FDB';
  conn.UserLogin := 'SYSDBA';
  conn.UserPassword := 'masterkey';
  conn.DBType := 'Interbase';
  conn.DBVersion := 'FB1.5';
  Result := conn;
end;

function TestTCcReplicatorIBX.CreateConnectionDialect1(lLocal: Boolean): TCcConnection;
var
  conn: TCcConnectionIBX;
begin
  conn := TCcConnectionIBX.Create(Application);
  conn.SQLDialect := 1;
  if lLocal then
    conn.DBName := 'localhost/3052:c:\projects\copycat\test\data\TEST_DIALECT1.FDB'
  else
    conn.DBName := 'localhost/3052:c:\projects\copycat\test\data\TEST_DIALECT1_REMOTE.FDB';
  conn.UserLogin := 'SYSDBA';
  conn.UserPassword := 'masterkey';
  conn.DBType := 'Interbase';
  conn.DBVersion := 'FB1.5';
  Result := conn;
end;

function TestTCcReplicatorIBX.CreateConnectionDialect3(lLocal: Boolean): TCcConnection;
var
  conn: TCcConnectionIBX;
begin
  conn := TCcConnectionIBX.Create(Application);
  conn.SQLDialect := 3;
  if lLocal then
    conn.DBName := 'localhost/3052:c:\projects\copycat\test\data\TEST_DIALECT3.FDB'
  else
    conn.DBName := 'localhost/3052:c:\projects\copycat\test\data\TEST_DIALECT3_REMOTE.FDB';
  conn.UserLogin := 'SYSDBA';
  conn.UserPassword := 'masterkey';
  conn.DBType := 'Interbase';
  conn.DBVersion := 'FB1.5';
  Result := conn;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCcConnectionIBX.Suite);
  RegisterTest(TestTCcReplicatorIBX.Suite);
end.

