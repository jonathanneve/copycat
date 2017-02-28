unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CcConf, CcConfStorage, CcReplicator, CcProviders,
  CcInterbaseConn, CcProvIBX;

type
  TForm1 = class(TForm)
    CcConnectionIBX1: TCcConnectionIBX;
    CcReplicator1: TCcReplicator;
    CcConfig1: TCcConfig;
    CcConfig2: TCcConfig;
    Button1: TButton;
    Button2: TButton;
    CcConnectionIBX2: TCcConnectionIBX;
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
begin
	CcConfig1.Connect;
	CcConfig1.GenerateConfig;
	CcConfig1.Disconnect;

	CcConfig2.Connect;
	CcConfig2.GenerateConfig;
	CcConfig2.Disconnect;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
CcReplicator1.Replicate;
end;

end.
