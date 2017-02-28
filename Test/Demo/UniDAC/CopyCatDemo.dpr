program CopyCatDemo;

uses
  Forms,
  main in 'main.pas' {MainForm};

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
//  FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
