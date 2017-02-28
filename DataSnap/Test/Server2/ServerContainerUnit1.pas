unit ServerContainerUnit1;

interface

uses System.SysUtils, System.Classes,
  Datasnap.DSTCPServerTransport,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  Datasnap.DSAuth, IPPeerServer, CcProviders, CcProvFireDAC, CcTransports,
  CcDataSnapTransport, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  Data.DB, FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  FireDAC.Comp.UI, CcDB, CcDataSnapServer;

type
  TServerContainer1 = class(TDataModule)
    DSServer1: TDSServer;
    DSTCPServerTransport1: TDSTCPServerTransport;
    CcDSServerTransportLink1: TCcDSServerTransportLink;
    CcDSServerTransport1: TCcDSServerTransport;
    CcConnectionFireDAC1: TCcConnectionFireDAC;
    FDConnection1: TFDConnection;
    FDTransaction1: TFDTransaction;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
  end;

var
  ServerContainer1: TServerContainer1;

implementation

uses Winapi.Windows, Dialogs;

{$R *.dfm}

procedure TServerContainer1.DataModuleCreate(Sender: TObject);
//var
//  val:TCcValue;
begin
//  val := DecodeValue('[{;1;50;N;''C''''est un test ; eh oui !!!''}{;1;40;Y;}]');
//  ShowMessage(val.AsArray[0].Value);
end;

end.

