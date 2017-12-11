unit conflict;

interface

uses
  CcReplicator,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfmConflict = class(TForm)
    rbNode1: TRadioButton;
    rbNode2: TRadioButton;
    Label1: TLabel;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label2: TLabel;
    lbTableName: TLabel;
    Label3: TLabel;
    lbKeys: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ResolveConflict(var Conflict: TConflictRecord);
  end;

var
  fmConflict: TfmConflict;

implementation

uses
  Unit1;

{$R *.dfm}

procedure TfmConflict.ResolveConflict(var Conflict: TConflictRecord);
begin
  with Conflict do begin
    rbNode1.Caption := Conflict.Node1 + ' (' + FormatDateTime('dd/mm/yy hh:nn:ss',  Conflict.LastOperationDate1) + ')';
    rbNode2.Caption := Conflict.Node2 + ' (' + FormatDateTime('dd/mm/yy hh:nn:ss',  Conflict.LastOperationDate2) + ')';
    lbTableName.Caption := fmMain.Replicator.Log.TableName;
    lbKeys.Caption := fmMain.Replicator.Log.Keys.PrimaryKeyValues;
    if ShowModal = mrOk then begin
      if rbNode1.Checked then
        ChosenNode := Node1
      else
        ChosenNode := Node2;
    end;
  end;
end;

end.
