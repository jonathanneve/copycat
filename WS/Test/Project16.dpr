program Project16;

uses
  EMemLeaks,
  Vcl.Forms,
  Unit15 in 'Unit15.pas' {Form15},
  CcDIMime in '..\CcDIMime.pas',
  CcWSTransport in '..\CcWSTransport.pas',
  uLkJSON in '..\uLkJSON.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm15, Form15);
  Application.Run;
end.
