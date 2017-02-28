program CopyCat_2006UnitTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses

  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestCcReplicator in 'TestCcReplicator.pas',
  CcReplicator in '..\CcReplicator.pas',
  TestCcProviders in 'TestCcProviders.pas',
  CcProviders in '..\CcProviders.pas',
  TestCcInterbaseConn in 'TestCcInterbaseConn.pas';
//  TestCcRTCTransport in 'TestCcRTCTransport.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

