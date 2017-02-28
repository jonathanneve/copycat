unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfmDBLogin = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    edPassword: TEdit;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    edUserName: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    cPassword, cUserName:String;
  end;

var
  fmDBLogin: TfmDBLogin;

implementation

{$R *.dfm}

procedure TfmDBLogin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOk then begin
    cUserName := Trim(edUserName.Text);
    cPassword := Trim(edPassword.Text);
  end;
end;

end.
