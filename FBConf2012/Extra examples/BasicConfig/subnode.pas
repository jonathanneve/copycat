unit subnode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Mask, DBCtrls;

type
  TfmSubNode = class(TForm)
    Label1: TLabel;
    edName: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    DBEdit1: TDBEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSubNode: TfmSubNode;

implementation

uses main, DB;

{$R *.dfm}

procedure TfmSubNode.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOk then begin
    if Trim(edName.Text) = '' then begin
      Application.MessageBox('You must provide a name for this sub-node!', 'Name missing!', MB_ICONERROR + MB_OK);
      CanClose := False;
    end else if MainForm.RPLUsers.State <> dsBrowse then begin
      MainForm.RPLUsers.Post;

      //Refresh node registration. This is necessary after a new node is created directly in RPL$USERS
      MainForm.Connection.RefreshNodes;
      //Grant rights to all replicated tables for the selected user
//      MainForm.CcConfig.GrantAll(Trim(edName.Text));
      if MainForm.Connection.InTransaction then
        MainForm.Connection.Commit;
    end;
  end
  else if MainForm.RPLUsers.State <> dsBrowse then
    MainForm.RPLUsers.Cancel;
end;

end.
