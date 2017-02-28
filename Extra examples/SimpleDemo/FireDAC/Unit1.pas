unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, Vcl.StdCtrls, CcConf,
  CcConfStorage, CcReplicator, FireDAC.Comp.Client, Data.DB, CcProviders,
  CcProvFireDAC, FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI;

type
  TForm1 = class(TForm)
    FDConnection2: TFDConnection;
    FDTransaction2: TFDTransaction;
    FDConnection1: TFDConnection;
    FDTransaction1: TFDTransaction;
    CcReplicator1: TCcReplicator;
    CcConfig1: TCcConfig;
    CcConfig2: TCcConfig;
    Button1: TButton;
    Button2: TButton;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CcConfig1.Connect;
  CcConfig1.GenerateConfig;
  CcConfig1.Disconnect;

  CcConfig2.Connect;
  CcConfig2.GenerateConfig;
  CcConfig2.Disconnect;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CcReplicator1.Replicate;
end;

end.
