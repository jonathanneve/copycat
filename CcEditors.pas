unit CcEditors;

{$I CC.INC}

interface

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
uses
  Classes
  {$IFDEF CC_D6}
    ,DesignEditors,DesignIntf

  {$else}
     ,DsgnIntf
  {$endif}
  ;

type



TCcDBAdaptorsEditor = class(TStringProperty)
  function GetAttributes:TPropertyAttributes;override;
  procedure GetValues(Proc: TGetStrProc);override;
end;

TCcDBAdaptorVersionsEditor = class(TStringProperty)
  function GetAttributes:TPropertyAttributes;override;
  procedure GetValues(Proc: TGetStrProc);override;
  procedure SetValue(const Value: string);override;
end;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}

uses CcProviders (*{$IFDEF CC_ADO}, CcConnADO {$ENDIF}*), CcConf;
//  CcTablesConfigEditor;


{ TCcDBAdaptorsEditor }

function TCcDBAdaptorsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

procedure TCcDBAdaptorsEditor.GetValues(Proc: TGetStrProc);
var
  slValues:TStringList;
  i:Integer;
begin
  slValues := TStringList.Create;
  try
    TCcConnection(GetComponent(0)).DatabaseTypes(slValues);
    for i:=0 to slValues.Count-1 do
      Proc(slValues[i]);
  finally
    slValues.Free;
  end;
end;

{ TCcDBAdaptorVersionsEditor }

function TCcDBAdaptorVersionsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

procedure TCcDBAdaptorVersionsEditor.GetValues(Proc: TGetStrProc);
var
  slValues:TStringList;
  i:Integer;
begin
  slValues := TStringList.Create;
  try
    TCcConnection(GetComponent(0)).DatabaseVersions(slValues);
    for i:=0 to slValues.Count-1 do
      Proc(slValues[i]);
  finally
    slValues.Free;
  end;
end;

procedure TCcDBAdaptorVersionsEditor.SetValue(const Value: string);
begin
  if Assigned(TCcConnection(GetComponent(0)).DBAdaptor) then
    inherited;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(String), TCcConnection, 'DBType', TCcDBAdaptorsEditor);
  RegisterPropertyEditor(TypeInfo(String), TCcConnection, 'DBVersion', TCcDBAdaptorVersionsEditor);
//  RegisterPropertyEditor(TypeInfo(TCcConfigTables), TCcConfig, 'Tables', TCcConfigTablesEditor);
(*{$IFDEF CC_ADO}
  RegisterPropertyEditor(TypeInfo(String), TCcConnectionADO, 'ConnectionString', TCcADOConnectionStringEditor);
{$ENDIF}*)
end;

{$IFDEF CC_ADO}

{ TCcADOConnectionStringEditor }

(*
procedure TCcADOConnectionStringEditor.Edit;
begin
  TCcConnectionADO(GetComponent(0)).EditConnection;
end;

function TCcADOConnectionStringEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;
*)
{$ENDIF}


{ TCcConfigTablesEditor }

//procedure TCcConfigTablesEditor.Edit;
//var
//  fmConfigTablesEditor : TfmConfigTablesEditor;
//  conf: TCcConfig;
//begin
{  conf := TCcConfig(GetComponent(0));
  fmConfigTablesEditor := TfmConfigTablesEditor.Create(conf);
  fmConfigTablesEditor.Config := conf;
  if (fmConfigTablesEditor.ShowModal = mrOk) and (fmConfigTablesEditor.Modified) then begin
    Designer.Modified := true;
    fmConfigTablesEditor.AssignTableConfig(conf.Tables);
  end;
  conf.Free;}
//end;

{function TCcConfigTablesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;}

{function TCcConfigTablesEditor.GetValue: string;
begin
  Result := '(Configuration...)';
end;}

{$ENDIF}
{$ENDIF}

end.
