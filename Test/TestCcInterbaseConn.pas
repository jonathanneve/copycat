unit TestCcInterbaseConn;

interface

uses
  TestFramework, Classes, DB, CcProviders, Dialogs, Sysutils,
  TestCcReplicator, TestCcProviders;
type
  TestTCcInterbaseConnection = class(TestTCcConnection)
  private
    FConnDialect1: TCcConnection;
    FConnDialect3: TCcConnection;
  protected
    function CreateConnectionDialect1: TCcConnection;virtual;abstract;
    function CreateConnectionDialect3: TCcConnection;virtual;abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDialect1;
    procedure TestDialect3;
  end;

  TestTCcInterbaseReplicator = class(TestTCcReplicator)
  protected
    function CreateConnectionDialect1(lLocal: Boolean): TCcConnection;virtual;abstract;
    function CreateConnectionDialect3(lLocal: Boolean): TCcConnection;virtual;abstract;
  published
    procedure TestReplicateDialect1;
    procedure TestReplicateDialect3;
  end;

implementation

uses
  Forms;

{ TestTCcInterbaseConnection }

procedure TestTCcInterbaseConnection.SetUp;
begin
  FConnDialect1 := CreateConnectionDialect1;
  ConfigDB(FConnDialect1);

  FConnDialect3 := CreateConnectionDialect3;
  ConfigDB(FConnDialect3);
end;

procedure TestTCcInterbaseConnection.TearDown;
begin
  FConnDialect3.Disconnect;
  FConnDialect1.Disconnect;
  FConnDialect3.Free;
  FConnDialect1.Free;
end;

procedure TestTCcInterbaseConnection.TestDialect1;
begin
  TestConnection(FConnDialect1);
end;

procedure TestTCcInterbaseConnection.TestDialect3;
begin
  TestConnection(FConnDialect3);
end;

procedure TestTCcInterbaseReplicator.TestReplicateDialect1;
begin
  FCcReplicator.LocalNode.Connection := CreateConnectionDialect1(True);
  FCcReplicator.RemoteNode.Connection := CreateConnectionDialect1(False);
  TestReplication;
end;

procedure TestTCcInterbaseReplicator.TestReplicateDialect3;
begin
  FCcReplicator.LocalNode.Connection := CreateConnectionDialect3(True);
  FCcReplicator.RemoteNode.Connection := CreateConnectionDialect3(False);
  TestReplication;
end;

end.

