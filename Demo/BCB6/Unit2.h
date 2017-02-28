//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfmDBLogin : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label2;
        TLabel *Label1;
        TPanel *Panel1;
        TBitBtn *BitBtn1;
        TBitBtn *BitBtn2;
        TEdit *edPassword;
        TEdit *edUserName;
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
public:		// User declarations
        String cUserName, cPassword;
        __fastcall TfmDBLogin(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmDBLogin *fmDBLogin;
//---------------------------------------------------------------------------
#endif
