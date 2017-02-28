unit CcProvIBDAC_Design;

interface

uses CcProvIBDAC, Classes, Sysutils, DB, DesignEditors, DesignIntf, IBC;

type

  TCcConnectionIBDACProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

{ TCcConnectionIBDACProperty }

function TCcConnectionIBDACProperty.GetValue: string;
var
  Component: TComponent;
  FIbcConnectionAssigned: TIBCConnection;
  FIbcConnectionDefault: TIBCConnection;
begin
  Component := GetComponent(0) as TComponent;
  FIbcConnectionAssigned := nil;
  FIbcConnectionDefault := nil;
  if Component is TCcConnectionIBDAC then
  begin
    FIbcConnectionAssigned := TCcConnectionIBDAC(Component).FIbcConnectionAssigned;
    FIbcConnectionDefault := TCcConnectionIBDAC(Component).FIbcConnectionDefault;
  end
  else
    Assert(False);

  if (Component is TCcConnectionIBDAC) then
  begin
    if ((FIbcConnectionAssigned = nil) and (FIbcConnectionDefault <> nil)) then
      Result := '<Default>'
    else
      Result := inherited GetValue;
  end
end;

procedure TCcConnectionIBDACProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  if Value = '<Default>' then
  begin
    Component := GetComponent(0) as TComponent;
    if Component is TCcConnectionIBDAC then
    begin
      TCcConnectionIBDAC(Component).FIbcConnectionAssigned := nil;
    end
    else
      Assert(False);
    Modified;
  end
  else
    inherited SetValue(Value);
end;

procedure TCcConnectionIBDACProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('<Default>');
  inherited GetValues(Proc);
end;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TIBCConnection), TCcConnectionIBDAC, 'IbcConnection', TCcConnectionIBDACProperty);
end;

end.
