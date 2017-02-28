unit TestCcXMLRPCTransport;

interface

uses
  TestFramework, Classes, DB, CcProviders, Dialogs, Sysutils, CcXMLRPCTransport,
  TestCcReplicator, TestCcProviders;
type
  TestTCcXMLRPCTransport = class(TestTCcConnection)
  private
    FConnection: TCcConnection;
    function CreateConnection: TCcConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
  end;

  // Test methods for class TestTCcXMLRPCTransport
  TestTCcReplicatorXMLRPC = class(TestTCcReplicator)
  published
    procedure TestReplicate;
  end;

implementation

uses
  Forms;

function TestTCcXMLRPCTransport.CreateConnection: TCcConnection;
var
  conn: TCcXmlRpcClientTransport;
begin
  conn := TCcXmlRpcClientTransport.Create(Application);
  conn.HostName := 'localhost';
  conn.HostPort := 8080;
  conn.DatabaseAlias := 'COPYCAT_LOCAL';
  conn.Password := 'microtec';
  Result := conn;
end;

procedure TestTCcXMLRPCTransport.SetUp;
begin
  FConnection := CreateConnection;
  ConfigDB(FConnection);
end;

procedure TestTCcXMLRPCTransport.TearDown;
begin
  FConnection.Disconnect;
  FConnection.Free;
end;

procedure TestTCcXMLRPCTransport.TestConnect;
begin
  TestConnection(FConnection);
end;

procedure TestTCcReplicatorXMLRPC.TestReplicate;
var
  conn: TCcXmlRpcClientTransport;
begin
  conn := TCcXmlRpcClientTransport.Create(Application);
  conn.HostName := 'localhost';
  conn.HostPort := 8080;
  conn.DatabaseAlias := 'COPYCAT_LOCAL';
  conn.Password := 'microtec';
  FCcReplicator.LocalNode.Connection := conn;

  conn := TCcXmlRpcClientTransport.Create(Application);
  conn.HostName := 'localhost';
  conn.HostPort := 8080;
  conn.DatabaseAlias := 'COPYCAT_REMOTE';
  conn.Password := 'microtec';
  FCcReplicator.RemoteNode.Connection := conn;

  TestReplication;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCcXMLRPCTransport.Suite);
  RegisterTest(TestTCcReplicatorXMLRPC.Suite);
end.

