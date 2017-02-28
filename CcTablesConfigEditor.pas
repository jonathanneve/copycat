unit CcTablesConfigEditor;

{$I CC.INC}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF CC_D6}Variants,{$ENDIF} Classes,
  {$IFDEF CC_D2K14}Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs{$ELSE}Graphics, Controls, Forms, Dialogs{$ENDIF} ,
  DB, CcMemDS, CcConf;

type
  TfmConfigTablesEditor = class(TForm)
	private
		{ Private declarations }
	public
		{ Public declarations }
//    procedure AssignTableConfig(tableConfig: TCcConfigTables);
  end;

implementation

{$R *.dfm}

{ TfmConfigTablesEditor }

{procedure TfmConfigTablesEditor.AssignTableConfig(tableConfig: TCcConfigTables);
var
  table: TCcConfigTable;
  nIndex: Integer;
begin
  while (not CcMemoryData.Eof) do begin
    nIndex := CcMemoryData.FieldByName('Index').AsInteger;
    if nIndex = -1 then
      nIndex := tableConfig.AddTable(CcMemoryData.FieldByName('Name').AsString);

    table := tableConfig[nIndex];
//    table.Condition := CcMemoryData.FieldByName('Condition').AsString;
//    table. := CcMemoryData.FieldByName('Name').AsString;
//    table.Name := CcMemoryData.FieldByName('Name').AsString;
//    table.Name := CcMemoryData.FieldByName('Name').AsString;
    CcMemoryData.Next;
  end;
end;
    }
end.
