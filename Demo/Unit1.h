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
#include "FIBQuery.hpp"
#include "pFIBQuery.hpp"
#include <Menus.hpp>
#include "FIBDatabase.hpp"
#include "FIBSQLMonitor.hpp"
#include "FIBDataSet.hpp"
#include "pFIBDataSet.hpp"
#include <Db.hpp>
#include <DB.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <DBTables.hpp>
#include "DBCtrlsEh.hpp"
#include "DBGridEh.hpp"
#include "RzButton.hpp"
#include "RzPanel.hpp"
#include "RzStatus.hpp"
#include "RzTabs.hpp"
#include "RzSplit.hpp"
#include "RzLabel.hpp"
#include "RzDBNav.hpp"
#include "RzEdit.hpp"
#include <Mask.hpp>
#include "IB_Services.hpp"
#include "syncobjs.hpp"
#include "FIBDatabase.hpp"
#include "pFIBDatabase.hpp"
#include <Dialogs.hpp>
#include "pFIBScript.hpp"
#include "CcConf.hpp"
#include "CcConfStorage.hpp"
#include "CCDBParams.hpp"
#include "CcMemDS.hpp"
#include "CcReplicator.hpp"
#include "CcProvFIBPlus.hpp"
#include "CcProviders.hpp"
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
        TRzPanel *Panel2;
        TRzBitBtn *btReactiver;
        TRzBitBtn *btDesactiver;
        TRzBitBtn *btReplicate;
        TRzBitBtn *btArreter;
        TPanel *Panel1;
        TRzLabel *lbReplicationEnCours;
        TRzLabel *RzLabel1;
        TCcReplicator *Replicator;
        TDataSource *LocalDataSetDS;
        TDataSource *RemoteDataSetDS;
        TpFIBDataSet *qLocalDataSet;
        TCcConfig *RplConfig;
        TCcTables *LocalTables;
        TpFIBQuery *qScript;
        TRzStatusBar *StatusBar;
        TRzPageControl *PageControl1;
        TRzTabSheet *TabSheet1;
        TMemo *edLog;
        TRzTabSheet *TabSheet2;
        TDBGridEh *DBGrid3;
        TpFIBDataSet *qConflicts;
        TDataSource *qConflictsDS;
        TProgressBar *ProgressBar;
        TRzBitBtn *BitBtn1;
        TRzStatusPane *RzStatusPane1;
        TRzSizePanel *RzSizePanel1;
        TPanel *Panel5;
        TLabel *Label2;
        TRzEdit *edRemoteUser;
        TDBGridEh *DBGrid2;
        TRzSizePanel *RzSizePanel2;
        TPanel *Panel6;
        TLabel *Label1;
        TRzEdit *edLocalUser;
        TDBGridEh *DBGrid1;
        TRzStatusPane *StatusPane;
        TRzPanel *Panel3;
        TRzDBNavigator *RzDBNavigator1;
        TRzDBNavigator *RzDBNavigator2;
        TRzDBNavigator *RzDBNavigator3;
        TRzStatusPane *AutoReplPane;
        TpFIBRestoreService *RestoreService;
        TpFIBSecurityService *SecurityService;
        TpFIBDatabase *LocalDB;
        TpFIBTransaction *LocalTR;
        TpFIBQuery *qGrant;
        TCcProcedures *LocalProcs;
        TTimer *Timer;
        TpFIBDatabase *RemoteDB;
        TpFIBTransaction *RemoteTR;
        TpFIBQuery *qLocalUpdateRPLLog;
        TpFIBQuery *qLocalUpdateRPLUser;
        TpFIBQuery *qLocalInsertRPLUser;
        TpFIBQuery *qRemoteUpdateRPLLog;
        TpFIBQuery *qRemoteUpdateRPLUser;
        TpFIBQuery *qRemoteInsertRPLUser;
        TpFIBQuery *qGetLocalUser;
        TpFIBQuery *qGetRemoteUser;
        TCcTables *RemoteTables;
        TCcProcedures *RemoteProcs;
        TFIBIntegerField *qLocalDataSetCUST_NO;
        TFIBStringField *qLocalDataSetNAME;
        TpFIBDataSet *qRemoteDataSet;
        TFIBIntegerField *qRemoteDataSetCUST_NO;
        TFIBStringField *qRemoteDataSetNAME;
        TCcProviderFIBPlus *CcProviderFIBPlus1;
        TImage *Image1;
        TLabel *Label3;
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
        void __fastcall ReplicatorQueryDone(TObject *Sender,
          TCcQueryType QueryType, int Rows);
        void __fastcall ReplicatorConnectionLost(TObject *Sender,
          TCCDatabase *Database);
        void __fastcall Image1Click(TObject *Sender);
        void __fastcall BitBtn1Click(TObject *Sender);
private:	// User declarations
        TEvent* RestoreEof;
        String cOldLocalUser;
        String cOldRemoteUser;
        String cSYSDBA;
        String cSYSDBAPassword;
        void __fastcall CheckMetadata();
        void __fastcall CreateMetadata(TCcTables* RplTables, String cTable);
        void __fastcall GrantTable(TFIBDatabase* DB, TFIBTransaction* TR, String cTableName);
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
