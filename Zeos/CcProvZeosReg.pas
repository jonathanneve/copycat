unit CcProvZeosReg;

interface

{$I ../CC.INC}

uses Classes, Sysutils, Types, CcProvZeos,
{$IFDEF CC_D2K12} Vcl.Dialogs{$ELSE}Dialogs{$ENDIF}, ZDbcIntfs, ZPropertyEditor, ZClasses, DesignIntf;

type

{** Implements a property editor for ZConnection.Protocol property. }
TCcZProtocolPropertyEditor = class(TZStringProperty)
public
  function  GetValue: string; override;
  procedure GetValueList(List: TStrings); override;
  procedure SetValue(const Value: string); override;
end;

{** Implements a property editor for ZConnection.Database property. }
TCcZDatabasePropertyEditor = class(TZStringProperty)
public
  function  GetAttributes: TPropertyAttributes; override;
  function  GetValue: string; override;
  procedure Edit; override;
  procedure GetValueList(List: TStrings); override;
  procedure SetValue(const Value: string); override;
end;

{** Implements a property editor for ZConnection.Catalog property. }
TCcZCatalogPropertyEditor = class(TZStringProperty)
public
  function  GetValue: string; override;
  procedure GetValueList(List: TStrings); override;
  procedure SetValue(const Value: string); override;
end;

procedure Register;

implementation

function TCcZProtocolPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TCcZProtocolPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetComponent(0) is TCcConnectionZeos then
    (GetComponent(0) as TCcConnectionZeos).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TCcZProtocolPropertyEditor.GetValueList(List: TStrings);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      List.Append(Protocols[J]);
  end;
end;

{ TCcZDatabasePropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TCcZDatabasePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetComponent(0) is TCcConnectionZeos then
  begin
    if ((GetComponent(0) as TCcConnectionZeos).Protocol = 'mssql') or
    ((GetComponent(0) as TCcConnectionZeos).Protocol = 'sybase') then
      Result := inherited GetAttributes
    else
      Result := [paDialog];
  end;
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TCcZDatabasePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TCcZDatabasePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetComponent(0) is TCcConnectionZeos then
    (GetComponent(0) as TCcConnectionZeos).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TCcZDatabasePropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetComponent(0) is TCcConnectionZeos then
  try
    if (GetComponent(0) as TCcConnectionZeos).Port = 0 then
      Url := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TCcConnectionZeos).Protocol,
        (GetComponent(0) as TCcConnectionZeos).HostName,
        '',
        (GetComponent(0) as TCcConnectionZeos).User,
        (GetComponent(0) as TCcConnectionZeos).Password])
    else
      Url := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TCcConnectionZeos).Protocol,
        (GetComponent(0) as TCcConnectionZeos).HostName,
        (GetComponent(0) as TCcConnectionZeos).Port,
        '',
        (GetComponent(0) as TCcConnectionZeos).User,
        (GetComponent(0) as TCcConnectionZeos).Password]);

    (GetComponent(0) as TCcConnectionZeos).ZConnection.ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetComponent(0) as TCcConnectionZeos).Properties);

      with DbcConnection.GetMetadata.GetCatalogs do
      try
        while Next do
          List.Append(GetStringByName('TABLE_CAT'));
      finally
        Close;
      end;

    finally
      (GetComponent(0) as TCcConnectionZeos).ZConnection.HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;

{**
  Brings up the proper database property editor dialog.
}
procedure TCcZDatabasePropertyEditor.Edit;
var
  OD: TOpenDialog;
begin
  if GetComponent(0) is TCcConnectionZeos then
  begin
    if ((GetComponent(0) as TCcConnectionZeos).Protocol = 'mssql') or
    ((GetComponent(0) as TCcConnectionZeos).Protocol = 'sybase') then
      inherited
{$IFNDEF UNIX}
{$IFNDEF FPC}
{$IFDEF ENABLE_ADO}
    else
    if ((GetComponent(0) as TCcConnectionZeos).Protocol = 'ado') then
      (GetComponent(0) as TCcConnectionZeos).Database := PromptDataSource(Application.Handle,
        (GetComponent(0) as TCcConnectionZeos).Database)
{$ENDIF}
{$ENDIF}
{$ENDIF}
    else
    begin
      OD := TOpenDialog.Create(nil);
      try
        OD.InitialDir := ExtractFilePath((GetComponent(0) as TCcConnectionZeos).Database);
        if OD.Execute then
          (GetComponent(0) as TCcConnectionZeos).Database := OD.FileName;
      finally
        OD.Free;
      end;
    end;
  end
  else
    inherited;
end;

{ TCcZCatalogPropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
function TCcZCatalogPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TCcZCatalogPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TCcZCatalogPropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetComponent(0) is TCcConnectionZeos then
  try
    if (GetComponent(0) as TCcConnectionZeos).Port = 0 then
      Url := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TCcConnectionZeos).Protocol,
        (GetComponent(0) as TCcConnectionZeos).HostName,
        '',
        (GetComponent(0) as TCcConnectionZeos).User,
        (GetComponent(0) as TCcConnectionZeos).Password])
    else
      Url := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TCcConnectionZeos).Protocol,
        (GetComponent(0) as TCcConnectionZeos).HostName,
        (GetComponent(0) as TCcConnectionZeos).Port,
        '',
        (GetComponent(0) as TCcConnectionZeos).User,
        (GetComponent(0) as TCcConnectionZeos).Password]);

    (GetComponent(0) as TCcConnectionZeos).ZConnection.ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetComponent(0) as TCcConnectionZeos).Properties);

      with DbcConnection.GetMetadata.GetCatalogs do
      try
        while Next do
          List.Append(GetStringByName('TABLE_CAT'));
      finally
        Close;
      end;

    finally
      (GetComponent(0) as TCcConnectionZeos).ZConnection.HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;

procedure Register;
begin
  RegisterComponents('CopyCat Connectors', [TCcConnectionZeos]);

  RegisterPropertyEditor(TypeInfo(string), TCcConnectionZeos, 'Protocol',
    TZProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TCcConnectionZeos, 'Database',
    TZDatabasePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TCcConnectionZeos, 'Catalog',
    TZCatalogPropertyEditor);
end;


end.
