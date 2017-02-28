//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "FIBQuery"
#pragma link "pFIBQuery"
#pragma link "FIBDatabase"
#pragma link "FIBSQLMonitor"
#pragma link "FIBDataSet"
#pragma link "pFIBDataSet"
#pragma link "DBGridEh"
#pragma link "DBGridEh"
#pragma link "RzButton"
#pragma link "RzPanel"
#pragma link "RzStatus"
#pragma link "RzTabs"
#pragma link "RzSplit"
#pragma link "RzLabel"
#pragma link "RzDBNav"
#pragma link "RzEdit"
#pragma link "IB_Services"
#pragma link "pFIBDatabase"
#pragma link "pFIBDatabase"
#pragma link "CcConf"
#pragma link "CcConfStorage"
#pragma link "CCDBParams"
#pragma link "CcMemDS"
#pragma link "CcReplicator"
#pragma link "CcProvFIBPlus"
#pragma link "CcProviders"
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
        : TForm(Owner)
{
//  SendInteger("Config DB: ", (int)RplConfig->DB);
  RestoreEof = new TEvent(NULL, false, false, "RestoreServiceEof");
  cSYSDBA = "SYSDBA";
}
//---------------------------------------------------------------------------
__fastcall TForm2::~TForm2()
{
  delete RestoreEof;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::btReplicateClick(TObject *Sender)
{
  Replicator->Users->LocalSYSDBA->Name = LocalDB->ConnectParams->UserName;
  Replicator->Users->LocalSYSDBA->Password = LocalDB->ConnectParams->Password;
  Replicator->Users->LocalUser->Name = edLocalUser->Text.Trim();
  Replicator->Users->LocalUser->Password = "CopyCat";
  Replicator->Users->RemoteUser->Name = edRemoteUser->Text.Trim();
  Replicator->Users->RemoteUser->Password = "CopyCat";
  Replicator->Replicate();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::btArreterClick(TObject *Sender)
{
  Replicator->AbortReplication();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::btDesactiverClick(TObject *Sender)
{
  Replicator->AutoReplicate->Stop();
  AutoReplPane->Caption = "Manual replication";
  btReactiver->Enabled = true;
  btDesactiver->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::btReactiverClick(TObject *Sender)
{
  String cFrequency = InputBox("Auto-replication", "Auto-replication frequency (seconds):", "");
  if (cFrequency.Trim() != "") {
    AutoReplPane->Caption = "Auto-replication (" + cFrequency.Trim() + " sec)";
    Replicator->AutoReplicate->Frequency = cFrequency.Trim().ToInt();
    Replicator->AutoReplicate->Start();
    btReactiver->Enabled = false;
    btDesactiver->Enabled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::LogEvent(String cLine)
{
  TDateTime dNow = Now();
  unsigned short nHours, nMinutes, nSeconds, nMilliSeconds;
  String cDateTime;

  dNow.DecodeTime(&nHours, &nMinutes, &nSeconds, &nMilliSeconds);
  cDateTime = dNow.FormatString("dd/mm/yyyy hh:mm:ss") + "." + FormatFloat("000", nMilliSeconds);
  cLine = cDateTime + " : " + cLine;
  edLog->Lines->Add(cLine);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorTableBegin(TObject *Sender,
      AnsiString Name)
{
  LogEvent("Table: " + Name);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorConflict(TObject *Sender)
{
  LogEvent("\tConflict!");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorRowReplicating(TObject *Sender)
{
  String cMessage = "\tReplicating from " + Replicator->Log->FBN("Origine") + " ";
  if (Replicator->Log->FBN("REF_FIELD") != "")
    cMessage = cMessage + Replicator->Log->FBN("REF_FIELD") + " = " + Replicator->Log->FBN("REF_VALUE");
  else
    cMessage = cMessage + Replicator->Log->FBN("PK1_NAME") + " = " + Replicator->Log->FBN("PK1_VALUE") + ", " + Replicator->Log->FBN("PK2_NAME") + " = " + Replicator->Log->FBN("PK2_VALUE") + ", " + Replicator->Log->FBN("PK3_NAME") + " = " + Replicator->Log->FBN("PK3_VALUE");
  LogEvent(cMessage);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorReplicateProc(TObject *Sender,
      AnsiString Name)
{
  LogEvent("Procedure: " + Replicator->Log->FBN("PROCEDURE_STATEMENT"));
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorReplicationError(TObject *Sender,
      Exception *e, bool &CanContinue)
{
  LogEvent("Replication error: " + e->Message);
  CanContinue = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorException(TObject *Sender, Exception *e)
{
  LogEvent("Error: " + e->Message);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorAutoCommit(TObject *Sender)
{
  LogEvent("Auto-commiting...");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorAbort(TObject *Sender)
{
  LogEvent("Replication aborted!");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorGenReplError(TObject *Sender,
      Exception *e, bool &CanContinue)
{
  EFIBError* f = (EFIBError*)e;
  if ((f->SQLCode == -903) || (f->SQLCode == -901)) { 
    LogEvent("Local record locked!!");    
    Application->ProcessMessages();
    CanContinue = false;
  }
  else {
    LogEvent("Error synchronizing the primary keys!" + e->Message);
    CanContinue = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorProgress(TObject *Sender)
{
  if (Replicator->Busy) {
    ProgressBar->Max = Replicator->Log->LineCount;
    ProgressBar->Position = Replicator->Log->CurrentLine;
  }
  else
    ProgressBar->Position = 0;
  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorEmptyLog(TObject *Sender)
{
  LogEvent("Nothing to replicate!");
  ReOpenTables(); 
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorFinished(TObject *Sender)
{
  ProgressBar->Position = 0;
  LogEvent("Repliction finished");
  Application->ProcessMessages();

  btReplicate->Enabled = true;
  btArreter->Enabled = false;

  lbReplicationEnCours->Blinking = false;
  lbReplicationEnCours->Visible = false;

  ReOpenTables();
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorLogLoaded(TObject *Sender)
{
  LogEvent("Replication started: " + String(Replicator->Log->LineCount) + " rows");

  btReplicate->Enabled = false;
  btArreter->Enabled = true;

  lbReplicationEnCours->Visible = true;
  lbReplicationEnCours->Blinking = true;
  Screen->Cursor = crHourGlass;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorGenReplicated(TObject *Sender,
      AnsiString Name, AnsiString NewValue)
{
  LogEvent("Replicated generator : " + Name + " => " + NewValue);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::RplConfigCreateMetadata(TObject *Sender,
      bool &CanContinue)
{
  CanContinue = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::RplConfigQueryReady(TObject *Sender,
      TStrings *Script)
{
  qScript->Close();
  qScript->Database = ((TCcDatabaseFIBPlus*)RplConfig->DB)->FIBDatabase;
  qScript->Transaction = ((TCcTransactionFIBPlus*)RplConfig->TR)->FIBTransaction;
  qScript->SQL->Text = Script->Text;
  qScript->ExecQuery();                                                       
  qScript->Transaction->Commit();
  qScript->Transaction->StartTransaction();
//  SQLScript->Database = ((TCcDatabaseFIBPlus*)RplConfig->DB)->FIBDatabase;
//  SQLScript->Transaction = ((TCcTransactionFIBPlus*)RplConfig->TR)->FIBTransaction;
//  SQLScript->SQL->Text = Script->Text + RplConfig->Terminator;
//  SQLScript->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::OpenLocalTable()
{
  CheckConnected();
  qLocalDataSet->Close();
  qLocalDataSet->Open();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::GrantTable(TFIBDatabase* DB, TFIBTransaction* TR, String cTableName)
{
  qGrant->Close();
  qGrant->Database = DB;
  qGrant->Transaction = TR;
  qGrant->SQL->Text = "GRANT ALL ON " + cTableName + " TO PUBLIC";
  qGrant->ExecQuery();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::OpenRemoteTable()
{
  CheckConnected();
  qRemoteDataSet->Close();
  qRemoteDataSet->Open();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CheckRestore(String cFileName)
{
  if (!FileExists(cFileName)) {
    //Retore
    StatusPane->Caption = "Restoring database...";
    String cBackup = ChangeFileExt(cFileName, ".gbk");
    RestoreService->BackupFile->Text = cBackup;
    RestoreService->DatabaseName->Text = cFileName;
    RestoreService->Params->Values["USER_NAME"] = cSYSDBA;
    try {
      RestoreService->Active = true;
    }
    catch(EFIBClientError &e) { //User canceled
      Application->Terminate();
      Abort();
    }
    RestoreService->ServiceStart();
    RestoreService->Active = false;
    RestoreEof->WaitFor(600000);
    RestoreService->LoginPrompt = false;
    cSYSDBA = RestoreService->Params->Values["USER_NAME"];
    cSYSDBAPassword = RestoreService->Params->Values["PASSWORD"];
    StatusPane->Caption = "";
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormShow(TObject *Sender)
{
  String cAppDir = ExtractFileDir(Application->ExeName);
  if (! ((FileExists(cAppDir + "\\local.gdb") && FileExists(cAppDir + "\\remote.gdb")) ||
         (FileExists(cAppDir + "\\local.gbk") && FileExists(cAppDir + "\\remote.gbk"))) ) {
    Application->MessageBox("Demonstration database files missing!\nReinstalling CopyCat demo should correct the problem.", "Error", MB_ICONERROR + MB_OK);
    Application->Terminate();
  }
  if (! (FileExists(cAppDir + "\\local.gdb") && FileExists(cAppDir + "\\remote.gdb"))) {
    if (Application->MessageBox("For compatability reasons, the example databases are supplied as backups only,\nand must therefore be restored. Restore now?", "Confirmation", MB_ICONQUESTION + MB_YESNO) == IDYES) {
      CheckRestore(cAppDir + "\\local.gdb");
      CheckRestore(cAppDir + "\\remote.gdb");
    }
    else {
      Application->MessageBox("Both local and remote databases must be present in the application directory\nin order for the demo to function correctly.", "Error", MB_ICONERROR + MB_OK);
      Application->Terminate();
    }
  }
  Replicator->LocalDB->DBName = cAppDir + "\\local.gdb";
  Replicator->RemoteDB->DBName = cAppDir + "\\remote.gdb";
  RemoteTables->Path = cAppDir + "\\" + RemoteTables->Path;
  RemoteProcs->Path = cAppDir + "\\" + RemoteProcs->Path;
  LocalTables->Path = cAppDir + "\\" + LocalTables->Path;
  LocalProcs->Path = cAppDir + "\\" + LocalProcs->Path;
  Timer->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CheckUsers()
{
  if ((edLocalUser->Text.Trim() != "") && (edRemoteUser->Text.Trim() != "")) {
    SecurityService->Params->Clear();
    SecurityService->Params->Add("USER_NAME=" + LocalDB->ConnectParams->UserName);
    SecurityService->Params->Add("PASSWORD=" + LocalDB->ConnectParams->Password);
    SecurityService->Active = true;
    try {
      CheckUser(edLocalUser->Text.Trim());
      CheckUser(edRemoteUser->Text.Trim());
    }
    __finally {
      SecurityService->Active = false;
    }
  }
  else {
    Application->MessageBox("Both usernames must have a value before replication can start!", "User name required", MB_ICONERROR + MB_OK);
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CheckUser(String cUserName)
{
  SecurityService->Params->Clear();
  SecurityService->Params->Add("USER_NAME=" + LocalDB->ConnectParams->UserName);
  SecurityService->Params->Add("PASSWORD=" + LocalDB->ConnectParams->Password);
  SecurityService->Active = true;
  try {
    SecurityService->DisplayUser(cUserName);
    if (!SecurityService->UserInfoCount) {
      if (Application->MessageBox(("User '" + cUserName + "' does not exist. Create it now?").c_str(), "Confirmation", MB_ICONQUESTION + MB_YESNO) == IDYES) {
        SecurityService->UserName = cUserName;
        SecurityService->FirstName = "";
        SecurityService->Password = "CopyCat";
        SecurityService->AddUser();
      }
      else Abort();
    }
  }
  __finally {
    SecurityService->Active = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::RestoreServiceTextNotify(TObject *Sender,
      const AnsiString Text)
{
  if (RestoreService->Eof) {
    RestoreService->Active = false;
    RestoreEof->SetEvent();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorBeforeReplicate(TObject *Sender)
{
  if ((edLocalUser->Text.Trim() == "") || (edRemoteUser->Text.Trim() == "")) {
    Application->MessageBox("Both usernames must have a value before replication can start!", "User name required", MB_ICONERROR + MB_OK);
    Abort();
  }
//  CheckConnected();
//  CheckMetadata();
  if (qLocalDataSet->Active && (qLocalDataSet->State != dsBrowse))
    qLocalDataSet->Post();
  if (qRemoteDataSet->Active && (qRemoteDataSet->State != dsBrowse))
    qRemoteDataSet->Post();
  if (qConflicts->Active && (qConflicts->State != dsBrowse))
    qConflicts->Post();
    
  if (LocalTR->InTransaction)
    LocalTR->Commit();
  LocalDB->Connected = false;
  if (RemoteTR->InTransaction)
    RemoteTR->Commit();
  RemoteDB->Connected = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CheckConnected()
{
  if (!LocalDB->Connected) {
    LocalDB->DBName = Replicator->LocalDB->DBName;
    if (cSYSDBAPassword != "") {
      LocalDB->ConnectParams->UserName = cSYSDBA;
      LocalDB->ConnectParams->Password = cSYSDBAPassword;
      LocalDB->UseLoginPrompt = false;
    }
    try {
      LocalDB->Connected = true;
    }
    catch(EFIBClientError &e) { //User canceled
      Application->Terminate();
      Abort();
    }
    LocalDB->UseLoginPrompt = false;
    LocalTR->Active = true;
  }
  if (!RemoteDB->Connected) {
    RemoteDB->DBName = Replicator->RemoteDB->DBName;
    RemoteDB->ConnectParams->UserName = LocalDB->ConnectParams->UserName;
    RemoteDB->ConnectParams->Password = LocalDB->ConnectParams->Password;
    RemoteDB->Connected = true;
    RemoteTR->Active = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CheckMetadata()
{
  TLocateOptions lo;
  bool lCreated = false;
  StatusPane->Caption = "Checking meta-data...";
  RplConfig->Disconnect();
  RplConfig->Procedures = LocalProcs;
  RplConfig->Tables = LocalTables;
  RplConfig->ConnectParams->CharSet = Replicator->LocalDB->CharSet;
  RplConfig->ConnectParams->SQLDialect = Replicator->LocalDB->SQLDialect;
  RplConfig->ConnectParams->DBName = Replicator->LocalDB->DBName;
  RplConfig->ConnectParams->UserName = LocalDB->ConnectParams->UserName; //SYSDBA user
  RplConfig->ConnectParams->Password = LocalDB->ConnectParams->Password;
  RplConfig->Connect();
  LocalTables->Locate("TABLE_NAME", "CUSTOMERS", lo);
  if (LocalTables->FieldByName("CREATED")->AsString == "N") {
    CreateMetadata(LocalTables, "CUSTOMERS");
    GrantTable(((TCcDatabaseFIBPlus*)RplConfig->DB)->FIBDatabase, ((TCcTransactionFIBPlus*)RplConfig->TR)->FIBTransaction, "CUSTOMERS");
    lCreated = true;
  }

  RplConfig->Disconnect();
  RplConfig->Procedures = RemoteProcs;
  RplConfig->Tables = RemoteTables;
  RplConfig->ConnectParams->CharSet = Replicator->RemoteDB->CharSet;
  RplConfig->ConnectParams->SQLDialect = Replicator->RemoteDB->SQLDialect;
  RplConfig->ConnectParams->DBName = Replicator->RemoteDB->DBName;
  RplConfig->ConnectParams->UserName = RemoteDB->ConnectParams->UserName;//SYSDBA user
  RplConfig->ConnectParams->Password = RemoteDB->ConnectParams->Password;
  RplConfig->Connect();
  RemoteTables->Locate("TABLE_NAME", "CUSTOMERS", lo);
  if (RemoteTables->FieldByName("CREATED")->AsString == "N") {
    CreateMetadata(RemoteTables, "CUSTOMERS");
    GrantTable(((TCcDatabaseFIBPlus*)RplConfig->DB)->FIBDatabase, ((TCcTransactionFIBPlus*)RplConfig->TR)->FIBTransaction, "CUSTOMERS");
    lCreated = true;
  }
  StatusPane->Caption = "";

  if (lCreated)
    Application->MessageBox("The replication meta-data has been successfully created.\n\nYou may now make changes to either or both databases, and then click on\nthe \"Replicate now\" button to synchronize.\n\nNote: Only changes made after this point will be synchronized.\nTo replicate changes made previously, simply touch the record again (in any way),\nand the current version of the record will be replicated to the other database.", "Meta-data created", MB_ICONINFORMATION + MB_OK);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CreateMetadata(TCcTables* RplTables, String cTable)
{
  StatusPane->Caption = "Creating meta-data for table " + cTable + "...";
  RplTables->Edit();
  RplTables->FieldByName("PRIORITY")->Value = 1;

  RplConfig->GenerateTriggers(cTable);
  RplTables->Edit();
  RplTables->FieldByName("CREATED")->AsString = "Y";
  RplTables->Post();
  StatusPane->Caption = "";
}
//---------------------------------------------------------------------------
void __fastcall TForm2::TimerTimer(TObject *Sender)
{
  Timer->Enabled = false;
  CheckConnected();
  CheckMetadata();
  ReOpenTables();

  qGetLocalUser->Close();
  qGetLocalUser->ExecQuery();
  if (qGetLocalUser->FN("LOGIN")->AsString.Trim() != "")
    edLocalUser->Text = qGetLocalUser->FN("LOGIN")->AsString.Trim();

  qGetRemoteUser->Close();
  qGetRemoteUser->ExecQuery();
  if (qGetRemoteUser->FN("LOGIN")->AsString.Trim() != "")
    edRemoteUser->Text = qGetRemoteUser->FN("LOGIN")->AsString.Trim();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::edLocalUserEnter(TObject *Sender)
{
  cOldLocalUser = edLocalUser->Text.Trim();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::edLocalUserExit(TObject *Sender)
{
  if (edLocalUser->Text.Trim() != "") {
    if (cOldLocalUser != edLocalUser->Text.Trim()) {
      CheckUser(edLocalUser->Text.Trim());
      if (cOldLocalUser != "") {
        qRemoteUpdateRPLUser->Close();
        qRemoteUpdateRPLUser->ParamByName("old_user")->Value = cOldLocalUser;
        qRemoteUpdateRPLUser->ParamByName("new_user")->Value = edLocalUser->Text.Trim();
        qRemoteUpdateRPLUser->ExecQuery();

        qRemoteUpdateRPLLog->Close();
        qRemoteUpdateRPLLog->ParamByName("old_user")->Value = cOldLocalUser;
        qRemoteUpdateRPLLog->ParamByName("new_user")->Value = edLocalUser->Text.Trim();
        qRemoteUpdateRPLLog->ExecQuery();
      }
      else {
        qRemoteInsertRPLUser->Close();
        qRemoteInsertRPLUser->ParamByName("new_user")->Value = edLocalUser->Text.Trim();
        qRemoteInsertRPLUser->ExecQuery();
      }
      GrantTable(RemoteDB, RemoteTR, "CUSTOMERS");
      RemoteTR->CommitRetaining();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::edRemoteUserExit(TObject *Sender)
{
  if (edRemoteUser->Text.Trim() != "") {
    if (cOldRemoteUser != edRemoteUser->Text.Trim()) {
      CheckUser(edRemoteUser->Text.Trim());
      if (cOldRemoteUser != "") {
        qLocalUpdateRPLUser->Close();
        qLocalUpdateRPLUser->ParamByName("old_user")->Value = cOldRemoteUser;
        qLocalUpdateRPLUser->ParamByName("new_user")->Value = edRemoteUser->Text.Trim();
        qLocalUpdateRPLUser->ExecQuery();

        qLocalUpdateRPLLog->Close();
        qLocalUpdateRPLLog->ParamByName("old_user")->Value = cOldRemoteUser;
        qLocalUpdateRPLLog->ParamByName("new_user")->Value = edRemoteUser->Text.Trim();
        qLocalUpdateRPLLog->ExecQuery();
      }
      else {
        qLocalInsertRPLUser->Close();
        qLocalInsertRPLUser->ParamByName("new_user")->Value = edRemoteUser->Text.Trim();
        qLocalInsertRPLUser->ExecQuery();
      }
      GrantTable(LocalDB, LocalTR, "CUSTOMERS");
      LocalTR->CommitRetaining();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::edRemoteUserEnter(TObject *Sender)
{
  cOldRemoteUser = edRemoteUser->Text.Trim();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::DBGrid1Enter(TObject *Sender)
{
  if (edRemoteUser->Text.Trim() == "") {
    ActiveControl = edLog;
    Application->MessageBox("You must provide a user name before editing any data.", "Information", MB_ICONINFORMATION + MB_OK);
    ActiveControl = edRemoteUser;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::DBGrid2Enter(TObject *Sender)
{
  if (edLocalUser->Text.Trim() == "") {
    ActiveControl = edLog;
    Application->MessageBox("You must provide a user name before editing any data.", "Information", MB_ICONINFORMATION + MB_OK);
    ActiveControl = edLocalUser;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorConnectLocal(TObject *Sender)
{
  LogEvent("Connecting to local database...");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorConnectRemote(TObject *Sender)
{
  LogEvent("Connecting to remote database...");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorDisconnect(TObject *Sender)
{
  LogEvent("Disconnecting from databases...\r\n");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReOpenTables()
{
  Application->ProcessMessages();
  OpenLocalTable();
  Application->ProcessMessages();
  OpenRemoteTable();
  Application->ProcessMessages();
  qConflicts->Close();
  qConflicts->Open();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::qRemoteDataSetAfterPost(TDataSet *DataSet)
{
  ((TpFIBDataSet*)DataSet)->UpdateTransaction->CommitRetaining();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::qRemoteDataSetAfterDelete(TDataSet *DataSet)
{
  ((TpFIBDataSet*)DataSet)->UpdateTransaction->CommitRetaining();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormClose(TObject *Sender, TCloseAction &Action)
{
  if (qConflicts->Active && (qConflicts->State != dsBrowse))
    qConflicts->Post();
  if (qLocalDataSet->Active && (qLocalDataSet->State != dsBrowse))
    qLocalDataSet->Post();
  if (qRemoteDataSet->Active && (qRemoteDataSet->State != dsBrowse))
    qRemoteDataSet->Post();
}
//---------------------------------------------------------------------------

void __fastcall TForm2::ReplicatorQueryDone(TObject *Sender,
      TCcQueryType QueryType, int Rows)
{
  String cQueryType;
  switch (QueryType) {
    case qtSelect: cQueryType = "SELECT"; break;
    case qtDelete: cQueryType = "DELETE"; break;
    case qtInsert: cQueryType = "INSERT"; break;
    case qtUpdate: cQueryType = "UPDATE"; break;
  }
  LogEvent("\t" + cQueryType + ": " + String(Rows) + " rows");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReplicatorConnectionLost(TObject *Sender,
      TCCDatabase *Database)
{
  LogEvent("Connection lost to database: " + Database->DBName + "!");
  qLocalDataSet->Database = ((TCcDatabaseFIBPlus*)Replicator->RemoteDB->DB)->FIBDatabase;
  qLocalDataSet->Transaction = ((TCcTransactionFIBPlus*)Replicator->RemoteDB->TR)->FIBTransaction;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Image1Click(TObject *Sender)
{
  Application->HelpJump("Introduction");
}
//---------------------------------------------------------------------------
void __fastcall TForm2::BitBtn1Click(TObject *Sender)
{
  Application->Terminate();
}
//---------------------------------------------------------------------------

