unit DaemonUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type

  { TDaemon1 }

  TDaemon1 = class(TDaemon)
    procedure DataModuleExecute(Sender: TCustomDaemon);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Daemon1: TDaemon1;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TDaemon1)
end;

{$R *.lfm}

{ TDaemon1 }

procedure TDaemon1.DataModuleExecute(Sender: TCustomDaemon);
begin
  Write('Test');
end;


initialization
  RegisterDaemon;
end.

