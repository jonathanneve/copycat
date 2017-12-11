program BasicConfig;

uses
  ExceptionLog,
  Forms,
  main in '..\main.pas' {MainForm},
  genproc in '..\genproc.pas' {fmGenProc},
  tableconfig in '..\tableconfig.pas' {fmTables},
  subnode in '..\subnode.pas' {fmSubNode},
  fConnectParams in '..\..\Common\FIBPlus\fConnectParams.pas' {frConnectParams: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CopyCat BasicConfig';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.