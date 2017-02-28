unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fConnectParams, StdCtrls, CcConfStorage, CcReplicator,
  ComCtrls, Buttons, ExtCtrls, CcProviders;

type
  TfmMain = class(TForm)
    Panel2: TPanel;
    btReactiver: TBitBtn;
    btDesactiver: TBitBtn;
    btFermer: TBitBtn;
    btReplicate: TBitBtn;
    btArreter: TBitBtn;
    Panel1: TPanel;
    lbReplicationEnCours: TLabel;
    edLog: TMemo;
    ProgressBar: TProgressBar;
    Replicator: TCcReplicator;
    LocalNodeNameLabel: TLabel;
    RemoteNodeNameLabel: TLabel;
    RemoteNodeNameEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LocalNodeNameEdit: TEdit;
    frLocalParams: TfrConnectParams;
    frRemoteParams: TfrConnectParams;
    procedure ReplicatorAutoCommit(Sender: TObject);
    procedure ReplicatorConflict(Sender: TObject);
    procedure ReplicatorEmptyLog(Sender: TObject);
    procedure ReplicatorException(Sender: TObject; e: Exception);
    procedure ReplicatorFinished(Sender: TObject);
    procedure ReplicatorGenReplError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
    procedure ReplicatorLogLoaded(Sender: TObject);
    procedure ReplicatorProgress(Sender: TObject);
    procedure ReplicatorQueryDone(Sender: TObject;
      QueryType: TCcQueryType; Rows: Integer);
    procedure ReplicatorReplicateProc(Sender: TObject; Name: String);
    procedure ReplicatorReplicationError(Sender: TObject; e: Exception;
      var CanContinue: Boolean);
    procedure ReplicatorTableBegin(Sender: TObject; Name: String);
    procedure btReplicateClick(Sender: TObject);
    procedure btReactiverClick(Sender: TObject);
    procedure btArreterClick(Sender: TObject);
    procedure btDesactiverClick(Sender: TObject);
    procedure ReplicatorResolveConflict(Sender: TObject;
      var Conflict: TConflictRecord);
    procedure ReplicatorBeforeReplicate(Sender: TObject);
    procedure btFermerClick(Sender: TObject);
    procedure ReplicatorKeySynchronized(Sender: TObject; Name: String;
      NewValue: Variant);
    procedure ReplicatorRowBeforeReplicate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReplicatorReplicationAborted(Sender: TObject);
  private
    procedure LogEvent(cLine: String);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  conflict;

{$R *.dfm}

procedure TfmMain.ReplicatorAutoCommit(Sender: TObject);
begin
  LogEvent('Auto-commiting...');
end;

procedure TfmMain.ReplicatorConflict(Sender: TObject);
begin
  LogEvent(''#9'Conflict!');
end;

procedure TfmMain.ReplicatorEmptyLog(Sender: TObject);
begin
  LogEvent('Nothing to replicate!');
end;

procedure TfmMain.ReplicatorException(Sender: TObject; e: Exception);
begin
  LogEvent('Error: ' + e.Message);
end;

procedure TfmMain.ReplicatorFinished(Sender: TObject);
begin
  ProgressBar.Position := 0;
  LogEvent('Repliction finished');
  Application.ProcessMessages();

  btReplicate.Enabled := true;
  btArreter.Enabled := false;

  lbReplicationEnCours.Visible := false;
end;

procedure TfmMain.ReplicatorGenReplError(Sender: TObject; e: Exception;
  var CanContinue: Boolean);
begin
    LogEvent('Error synchronizing the primary keys!' + e.Message);
    CanContinue := false;
end;

procedure TfmMain.ReplicatorLogLoaded(Sender: TObject);
begin
  edLog.Lines.Add('');
  LogEvent('Replication started: ' + IntToStr(Replicator.Log.LineCount) + ' rows');

  btReplicate.Enabled := false;
  btArreter.Enabled := true;

  lbReplicationEnCours.Visible := true;
end;

procedure TfmMain.ReplicatorProgress(Sender: TObject);
begin
	if (Replicator.Busy) then begin
		ProgressBar.Max := Replicator.Log.LineCount;
    ProgressBar.Position := Replicator.Log.CurrentLine;
  end
  else
    ProgressBar.Position := 0;
  Application.ProcessMessages();
end;

procedure TfmMain.ReplicatorQueryDone(Sender: TObject;
  QueryType: TCcQueryType; Rows: Integer);
var cQueryType: String;
begin
  case (QueryType) of
    qtSelect: cQueryType := 'SELECT';
    qtDelete: cQueryType := 'DELETE';
    qtInsert: cQueryType := 'INSERT';
    qtUpdate: cQueryType := 'UPDATE';
  end;
  LogEvent(#9 + cQueryType + ': ' + IntToStr(Rows) + ' rows');
end;

procedure TfmMain.ReplicatorReplicateProc(Sender: TObject; Name: String);
begin
  LogEvent('Procedure: ' + Replicator.Log.FBN('PROCEDURE_STATEMENT'));
end;

procedure TfmMain.ReplicatorReplicationError(Sender: TObject; e: Exception;
  var CanContinue: Boolean);
begin
  LogEvent('Replication error: ' + e.Message);
  CanContinue := true;
end;

procedure TfmMain.ReplicatorTableBegin(Sender: TObject; Name: String);
begin
  LogEvent('Table: ' + Name);
end;

procedure TfmMain.btReplicateClick(Sender: TObject);
begin
   Replicator.Replicate();
end;

procedure TfmMain.btReactiverClick(Sender: TObject);
var
  cInterval: String;
begin
  cInterval := IntToStr(Replicator.AutoReplicate.Frequency);
  if (InputQuery('Automatic replication', 'Interval (sec.) :', cInterval))
    and (StrToIntDef(cInterval, 0) <> 0) then begin
    Replicator.AutoReplicate.Frequency := StrToInt(cInterval);
    Replicator.AutoReplicate.Start();
    btReactiver.Enabled := false;
    btDesactiver.Enabled := true;
  end;
end;

procedure TfmMain.btArreterClick(Sender: TObject);
begin
  Replicator.AbortReplication();
end;

procedure TfmMain.btDesactiverClick(Sender: TObject);
begin
  Replicator.AutoReplicate.Stop();
  btReactiver.Enabled := true;
  btDesactiver.Enabled := false;
end;

procedure TfmMain.LogEvent(cLine: String);
var dNow:TDateTime;
    cDateTime: String;
    nHours, nMinutes, nSeconds, nMilliSeconds: Word;
begin
  dNow := Now();

  DecodeTime(dNow, nHours, nMinutes, nSeconds, nMilliSeconds);
  cDateTime := FormatDateTime('dd/mm/yyyy hh:mm:ss', dNow) + '.' + FormatFloat('000', nMilliSeconds);
  cLine := cDateTime + ' : ' + cLine;
  edLog.Lines.Add(cLine);
end;

procedure TfmMain.ReplicatorResolveConflict(Sender: TObject;
  var Conflict: TConflictRecord);
begin
  with TfmConflict.Create(Self) do try
    ResolveConflict(Conflict);
  finally
    Free;
  end;
end;

procedure TfmMain.ReplicatorBeforeReplicate(Sender: TObject);
begin
  // Only set things up here. Do not connect here.
  // Connection is done automatically by TCcReplicator.Replicate
  if not frLocalParams.Connection.Connected then begin
    Screen.Cursor := crHourGlass;
    try
      frLocalParams.SetConnectParams;
      Replicator.Nodes.LocalNode.Connection := frLocalParams.Connection;
      Replicator.Nodes.LocalNode.Name := LocalNodeNameEdit.Text;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

  if not frRemoteParams.Connection.Connected then begin
    Screen.Cursor := crHourGlass;
    try
      frRemoteParams.SetConnectParams;
      Replicator.Nodes.RemoteNode.Connection := frRemoteParams.Connection;
      Replicator.Nodes.RemoteNode.Name := RemoteNodeNameEdit.Text;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfmMain.btFermerClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.ReplicatorKeySynchronized(Sender: TObject; Name: String;
  NewValue: Variant);
begin
  LogEvent('Synchronized key : ' + Name + ' => ' + NewValue);
end;

procedure TfmMain.ReplicatorRowBeforeReplicate(Sender: TObject);
var
  cMessage:String;
  I :Integer;
begin
  cMessage := ''#9'Replicating from ' + Replicator.Log.FBN('Origin') + ' ';
  if (Replicator.Log.FBN('REF_FIELD') <> '') then
    cMessage := cMessage + Replicator.Log.FBN('REF_FIELD') + ' = ' + Replicator.Log.FBN('REF_VALUE')
  else for I:=0 to Replicator.Log.Keys.Count -1 do
    with Replicator.Log.Keys[i] do
      if PrimaryKey then
        cMessage := cMessage + KeyName + ' = ' + KeyValue;
  LogEvent(cMessage);
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  frRemoteParams.Init;
  frLocalParams.Init;
end;

procedure TfmMain.ReplicatorReplicationAborted(Sender: TObject);
begin
  LogEvent('Replication aborted!');
end;

end.
