unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CcProviders, CcInterbaseConn, CcProvUIB,
  Vcl.StdCtrls, CcConfStorage, CcConf;

type
  TForm1 = class(TForm)
    CcConfig: TCcConfig;
    Button1: TButton;
    FirebirdDB: TCcConnectionUIB;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CcConfig.Connect;
  CcConfig.GenerateTriggers('CUSTOMERS');
  CcConfig.Disconnect;
  Application.MessageBox('Triggers created successfully', 'Success!')
end;

end.
