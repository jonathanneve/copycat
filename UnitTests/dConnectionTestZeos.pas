unit dConnectionTestZeos;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dtConnector, CcProviders, CcProvZeos;

type
  TdmtConnectorZeos = class(TdmtConnector)
    CcConnectionZeos1: TCcConnectionZeos;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmtConnectorZeos: TdmtConnectorZeos;

implementation

{$R *.dfm}

end.
