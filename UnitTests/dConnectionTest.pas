unit dConnectionTest;

interface

uses
  System.SysUtils, System.Classes;

type
  TdmConnectionTest = class(TDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmConnectionTest: TdmConnectionTest;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
