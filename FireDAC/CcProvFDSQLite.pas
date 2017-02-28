unit CcProvFDSQLite;

interface

uses Classes, CcProviders, CcProvFireDAC, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper;

type
TCcConnectionFDSQLite = class(TCcAbstractConnectionFireDAC)
  private
    userFunction: TFDSQLiteFunction;
    FDriverLink: TFDPhysSQLiteDriverLink;
  protected
    procedure ReplicatingNodeCalculate(AFunc: TSQLiteFunctionInstance;
      AInputs: TSQLiteInputs; AOutput: TSQLiteOutput; var AUserData: TObject);
    procedure DoConnect; override;
  public
    class function ConnectorName: String;override;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property DriverLink: TFDPhysSQLiteDriverLink read FDriverLink write FDriverLink;
    property DBVersion;
end;

procedure Register;

implementation

uses CcSQLite;

{ TCcConnectionFDSQLite }

class function TCcConnectionFDSQLite.ConnectorName: String;
begin
  Result := 'FDSQLite';
end;

constructor TCcConnectionFDSQLite.Create(AOwner: TComponent);
begin
  inherited;
  AddDBAdaptor(TCcSQLiteAdaptor);
  userFunction := TFDSQLiteFunction.Create(Self);
  DBType := 'SQLite';
end;

destructor TCcConnectionFDSQLite.Destroy;
begin
  userFunction.Free;
  inherited;
end;

procedure TCcConnectionFDSQLite.DoConnect;
begin
  inherited;
  userFunction.DriverLink := DriverLink;
  userFunction.FunctionName := 'replicating_node';
  userFunction.ArgumentsCount := 0;
  userFunction.OnCalculate := ReplicatingNodeCalculate;
  userFunction.Active := True;
end;

procedure TCcConnectionFDSQLite.ReplicatingNodeCalculate(
  AFunc: TSQLiteFunctionInstance; AInputs: TSQLiteInputs;
  AOutput: TSQLiteOutput; var AUserData: TObject);
begin
  if ReplicatingNode = '' then
    AOutput.Clear
  else
    AOutput.AsString := ReplicatingNode;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionFDSQLite]);
end;

initialization
  RegisterDBConnector(TCcConnectionFDSQLite, TCcConnectionFDSQLite.ConnectorName);

end.
