unit TestConnection;

interface

uses
  TestFramework, CcProviders, TestCcReplicator;

implementation

type
  TestTCcConnection = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

{ TestTCcConnection }

procedure TestTCcConnection.SetUp;
begin
  inherited;

end;

procedure TestTCcConnection.TearDown;
begin
  inherited;

end;

procedure TestTCcConnection.TestAbortReplication;
begin

end;

procedure TestTCcConnection.TestConnect;
begin

end;

procedure TestTCcConnection.TestDisconnect;
begin

end;

procedure TestTCcConnection.TestReplicate;
begin

end;

end.
