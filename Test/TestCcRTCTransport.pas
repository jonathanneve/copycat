unit TestCcRTCTransport;

interface

uses
  TestFramework, Classes, DB, CcProviders, Dialogs, Sysutils, CcRTCTransport,
  rtcConn, rtcInfo, rtcCliModule, TestCcReplicator, TestCcProviders, rtcHttpCli;
type
  TestTCcRTCTransport = class(TestTCcConnection)
  private
    FConnection: TCcConnection;
    FClient: TRtcHttpClient;
    FClientModule: TRtcClientModule;
    function CreateConnection: TCcConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
  end;

  // Test methods for class TestTCcRTCTransport
  TestTCcReplicatorRTC = class(TestTCcReplicator)
  private
    FClientModule: TRtcClientModule;
    FClient: TRtcHttpClient;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReplicate;
  end;

implementation

uses
  Forms, rtcDataCli;

function TestTCcRTCTransport.CreateConnection: TCcConnection;
var
  conn: TCcRTCClientTransport;
begin
  conn := TCcRTCClientTransport.Create(Application);
  conn.ClientModule := FClientModule;
  conn.DatabaseAlias := 'COPYCAT_LOCAL';
  conn.Password := 'microtec';
  Result := conn;
end;

procedure TestTCcRTCTransport.SetUp;
begin
  FClient := TRtcHttpClient.Create(Application);
  FClient.ServerAddr := 'localhost';
  FClient.ServerPort := '8080';

  FClientModule := TRtcClientModule.Create(Application);
  FClientModule.Client := FClient;
  FClientModule.DataFormat := fmt_XMLRPC;
  FClientModule.ModuleFileName := '/';

  FConnection := CreateConnection;
  ConfigDB(FConnection);
end;

procedure TestTCcRTCTransport.TearDown;
begin
  FConnection.Disconnect;
  FConnection.Free;
  FClientModule.Free;
  FClient.Free;
end;

procedure TestTCcRTCTransport.TestConnect;
begin
  TestConnection(FConnection);
end;

procedure TestTCcReplicatorRTC.SetUp;
begin
  inherited;
  FClient := TRtcHttpClient.Create(Application);
  FClient.ServerAddr := 'localhost';
  FClient.ServerPort := '8080';

  FClientModule := TRtcClientModule.Create(Application);
  FClientModule.Client := FClient;
  FClientModule.DataFormat := fmt_XMLRPC;
  FClientModule.ModuleFileName := '/';
end;

procedure TestTCcReplicatorRTC.TearDown;
begin
  FClientModule.Free;
  FClient.Free;
  FCcReplicator.LocalNode.Connection.Free;
  FCcReplicator.LocalNode.Connection := nil;
  FCcReplicator.RemoteNode.Connection.Free;
  FCcReplicator.RemoteNode.Connection := nil;
  inherited;
end;

procedure TestTCcReplicatorRTC.TestReplicate;
var
  conn: TCcRTCClientTransport;
begin
  conn := TCcRTCClientTransport.Create(Application);
  conn.ClientModule := FClientModule;
  conn.DatabaseAlias := 'COPYCAT_LOCAL';
  conn.Password := 'microtec';
  FCcReplicator.LocalNode.Connection := conn;

  conn := TCcRTCClientTransport.Create(Application);
  conn.ClientModule := FClientModule;
  conn.DatabaseAlias := 'COPYCAT_REMOTE';
  conn.Password := 'microtec';
  FCcReplicator.RemoteNode.Connection := conn;

  TestReplication;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCcRTCTransport.Suite);
  RegisterTest(TestTCcReplicatorRTC.Suite);
end.
