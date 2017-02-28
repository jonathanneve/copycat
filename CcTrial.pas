//CopyCat replication suite<p/>
//Copyright (c) 2005 Microtec Communications<p/>
//For any questions or technical support, contact us at copycat@microtec.fr
unit CcTrial;

interface

{$I CC.INC}

uses
  Windows, Messages, SysUtils {$IFDEF CC_D6}, Variants {$ENDIF}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfmTrial = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    Image1: TImage;
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses ShellApi;

{$R *.dfm}

procedure TfmTrial.Label4Click(Sender: TObject);
begin
  ShellExecute(0,'open','iexplore.exe','http://www.microtec.fr/copycat',nil,SW_SHOW);
end;

initialization
{$IFDEF CC_TRIAL}
  with TfmTrial.Create(Application) do
    try
      ShowModal;
      if FindWindow('TAppBuilder', nil) <=0 then
        Application.Terminate;
    finally
      Free;
    end;
{$ENDIF}

end.

