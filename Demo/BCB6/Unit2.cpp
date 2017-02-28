//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfmDBLogin *fmDBLogin;
//---------------------------------------------------------------------------
__fastcall TfmDBLogin::TfmDBLogin(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmDBLogin::FormClose(TObject *Sender, TCloseAction &Action)
{
  if (ModalResult == mrOk) {
    cUserName = edUserName->Text.Trim();
    cPassword = edPassword->Text.Trim();
  }
}
//---------------------------------------------------------------------------
