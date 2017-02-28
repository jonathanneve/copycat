unit Unit6;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DBXDataSnap, IPPeerClient,
  Data.DBXCommon, Vcl.StdCtrls, Data.DB, Data.SqlExpr;

type
  TForm6 = class(TForm)
    SQLConnection: TSQLConnection;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
  SQLConnection.Open;
  with SQLConnection.DBXConnection.CreateCommand do begin
    CommandType := TDBXCommandTypes.DSServerMethod;
    Text := 'TServerMethods1.EchoString';
    Prepare;
    Parameters[0].Value.SetWideString(Edit1.Text);
    ExecuteUpdate;
    Label1.Caption := Parameters[1].Value.GetWideString;
    DisposeOf;
  end;
end;

end.
