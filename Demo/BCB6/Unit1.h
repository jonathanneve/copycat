//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <Menus.hpp>
#include <Db.hpp>
#include "CcReplicator.hpp"
#include <DB.hpp>
#include "CcMemDS.hpp"
#include <DBCtrls.hpp>
#include "CcConfStorage.hpp"
#include "CcDBParams.hpp"
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <DBTables.hpp>
#include "CcConf.hpp"
#include <Mask.hpp>
#include "IB_Services.hpp"
#include "syncobjs.hpp"
#include <Dialogs.hpp>
#include <Graphics.hpp>
#include "CCDBParams.hpp"
#include "CcProviders.hpp"
#include "CcConf.hpp"
#include "CcConfStorage.hpp"
#include "CcMemDS.hpp"
#include "CcReplicator.hpp"
#include "CcProvIBX.hpp"
#include <IBCustomDataSet.hpp>
#include <IBDatabase.hpp>
#include <IBServices.hpp>
#include <IBSQL.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel2;
        TBitBtn *btReactiver;
        TBitBtn *btDesactiver;
        TBitBtn *btReplicate;
        TBitBtn *btArreter;
        TBitBtn *BitBtn1;
        TPanel *Panel1;
        TLabel *lbReplicationEnCours;
        TLabel *RzLabel1;
        TImage *Image1;
        TLabel *Label3;
        TPageControl *PageControl1;
        TTabSheet *TabSheet1;
        TSplitter *Splitter2;
        TSplitter *Splitter1;
        TMemo *edLog;
        TProgressBar *ProgressBar;
        TPanel *Panel4;
        TPanel *Panel6;
        TLabel *Label1;
        TEdit *edLocalUser;
        TDBNavigator *RzDBNavigator3;
        TDBGrid *DBGrid1;
        TPanel *Panel7;
        TDBNavigator *RzDBNavigator2;
        TPanel *Panel5;
        TLabel *Label2;
        TEdit *edRemoteUser;
        TDBGrid *DBGrid2;
        TTabSheet *TabSheet2;
        TDBGrid *DBGrid3;
        TPanel *Panel3;
        TDBNavigator *RzDBNavigator1;
        TCcReplicator *Replicator;
        TDataSource *LocalDataSetDS;
        TDataSource *RemoteDataSetDS;
        TIBDataSet *qLocalDataSet;
        TIntegerField *qLocalDataSetCUST_NO;
        TStringField *qLocalDataSetNAME;
        TCcTables *LocalTables;
        TIBSQL *qScript;
        TIBDataSet *qConflicts;
        TDataSource *qConflictsDS;
        TIBRestoreService *RestoreService;
        TIBSecurityService *SecurityService;
        TIBDatabase *LocalDB;
        TIBTransaction *LocalTR;
        TIBSQL *qGrant;
        TCcProcedures *LocalProcs;
        TCcConfig *RplConfig;
        TTimer *Timer;
        TIBDatabase *RemoteDB;
        TIBTransaction *RemoteTR;
        TIBSQL *qLocalUpdateRPLLog;
        TIBSQL *qLocalUpdateRPLUser;
        TIBSQL *qLocalInsertRPLUser;
        TIBSQL *qRemoteUpdateRPLLog;
        TIBSQL *qRemoteUpdateRPLUser;
        TIBSQL *qRemoteInsertRPLUser;
        TIBSQL *qGetLocalUser;
        TIBSQL *qGetRemoteUser;
        TCcTables *RemoteTables;
        TCcProcedures *RemoteProcs;
        TIBDataSet *qRemoteDataSet;
        TIntegerField *qRemoteDataSetCUST_NO;
        TStringField *qRemoteDataSetNAME;
        TIBSQL *IBScript;
        TCcProviderIBX *CcProviderIBX1;
        void __fastcall btReplicateClick(TObject *Sender);
        void __fastcall btArreterClick(TObject *Sender);
        void __fastcall btDesactiverClick(TObject *Sender);
        void __fastcall btReactiverClick(TObject *Sender);
        void __fastcall ReplicatorTableBegin(TObject *Sender,
          AnsiString Name);
        void __fastcall ReplicatorConflict(TObject *Sender);
        void __fastcall ReplicatorRowReplicating(TObject *Sender);
        void __fastcall ReplicatorReplicateProc(TObject *Sender,
          AnsiString Name);
        void __fastcall ReplicatorReplicationError(TObject *Sender,
          Exception *e, bool &CanContinue);
        void __fastcall ReplicatorException(TObject *Sender,
          Exception *e);
        void __fastcall ReplicatorAutoCommit(TObject *Sender);
        void __fastcall ReplicatorAbort(TObject *Sender);
        void __fastcall ReplicatorGenReplError(TObject *Sender,
          Exception *e, bool &CanContinue);
        void __fastcall ReplicatorQueryDone(TObject *Sender,
          TCcQueryType QueryType, int Rows);
        void __fastcall ReplicatorProgress(TObject *Sender);
        void __fastcall ReplicatorEmptyLog(TObject *Sender);
        void __fastcall ReplicatorFinished(TObject *Sender);
        void __fastcall ReplicatorLogLoaded(TObject *Sender);
        void __fastcall ReplicatorGenReplicated(TObject *Sender,
          AnsiString Name, AnsiString NewValue);
        void __fastcall RplConfigCreateMetadata(TObject *Sender,
          bool &CanContinue);
        void __fastcall RplConfigQueryReady(TObject *Sender,
          TStrings *Script);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall RestoreServiceTextNotify(TObject *Sender,
          const AnsiString Text);
        void __fastcall ReplicatorBeforeReplicate(TObject *Sender);
        void __fastcall TimerTimer(TObject *Sender);
        void __fastcall edLocalUserEnter(TObject *Sender);
        void __fastcall edLocalUserExit(TObject *Sender);
        void __fastcall edRemoteUserExit(TObject *Sender);
        void __fastcall edRemoteUserEnter(TObject *Sender);
        void __fastcall DBGrid1Enter(TObject *Sender);
        void __fastcall DBGrid2Enter(TObject *Sender);
        void __fastcall ReplicatorConnectLocal(TObject *Sender);
        void __fastcall ReplicatorConnectRemote(TObject *Sender);
        void __fastcall ReplicatorDisconnect(TObject *Sender);
        void __fastcall qRemoteDataSetAfterPost(TDataSet *DataSet);
        void __fastcall qRemoteDataSetAfterDelete(TDataSet *DataSet);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall BitBtn1Click(TObject *Sender);
        void __fastcall btHelpClick(TObject *Sender);
        void __fastcall ReplicatorConnectionLost(TObject *Sender,
          TCCDatabase *Database);
        void __fastcall LocalDBLogin(TIBDatabase *Database,
          TStrings *LoginParams);
private:	// User declarations
        TEvent* RestoreEof;
        String cOldLocalUser;
        String cOldRemoteUser;
        String cSYSDBA;
        String cSYSDBAPassword;
        void __fastcall CheckMetadata();
        void __fastcall CreateMetadata(TCcTables* RplTables, String cTable);
        void __fastcall GrantTable(TIBDatabase* DB, TIBTransaction* TR, String cTableName);
        void __fastcall LogEvent(String cLine);
        void __fastcall CheckConnected();
        void __fastcall CheckRestore(String cFileName);
        void __fastcall CheckUsers();
        void __fastcall CheckUser(String cUserName);
        void __fastcall OpenLocalTable();
        void __fastcall OpenRemoteTable();
        void __fastcall ReOpenTables();
public:		// User declarations
        __fastcall TForm2(TComponent* Owner);
        __fastcall ~TForm2();
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
