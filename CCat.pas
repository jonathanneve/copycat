//CopyCat replication suite<p/>
//Copyright (c) 2014 Microtec Communications<p/>
//For any questions or technical support, contact us at contact@copycat.fr
unit CCat;

{$I CC.INC}

interface

uses Classes,

{$IFDEF FPC}
LazarusPackageIntf,
{$ENDIF}

IniFiles, SysUtils, DB{$IFDEF CC_TRIAL}, CcTrial{$ENDIF}, CcProviders
 {$IFDEF CC_UseVariants}, Variants {$ENDIF};


const
  VersionNumber = '3.9.4';

type

(*********************************************************
  Summary:
  Type of commit to perform.
  Description:
  <table>
  Value         Meaning
  ------------  ------------------------------------------
  ctNone        No commit
  ctCommit      Perform commit (closing transaction)
  ctRetaining   Commit Retaining (leave transaction open)
  </table>
  *********************************************************)
TCcCommitType = (ctNone, ctCommit, ctRetaining);

(*********************************************************
  Summary:
  Current state of the a record in the replication log.
  Description:
  <table>
  Value         Meaning
  ------------  ------------------------------------------
  lsNone        Non-replicated record.
  lsOk          Record replicated successfully.
  lsError       Replication of the current record failed with an error.
  lsConflict    Replication of the current record failed because of a conflict.
  </table>
  *********************************************************)
TCcLogState = (lsNone, lsOk, lsError, lsConflict);

TCcErrorEvent = procedure(Sender :TObject; e :Exception; var CanContinue :Boolean; var SkipToRemote :Boolean) of object;
TCcExceptionEvent = procedure(Sender :TObject; e :Exception) of object;
TCcNameEvent = procedure(Sender :TObject; Name :String) of object;
TCcOperationEvent = procedure(Sender :TObject; var Handled :Boolean) of object;
TCcGetFieldsEvent = procedure(Sender :TObject; Table :String; Fields :TStringList) of object;
TCcLostConnectEvent = procedure(Sender: TObject; Database: TCcConnection) of object;
TCcNotifyEvent = procedure(Sender: TObject) of object;

function ReplaceString(S:String;sub1:String;sub2:String):String;
function ReadBinaryStream(ini: TCustomIniFile; const Section, Name: string;
  Value: TStream): Integer;
procedure WriteBinaryStream(ini: TCustomIniFile; const Section, Name: string;
  Value: TStream);

procedure Register;

procedure RemoveItemsInCommon(slFirst, slSecond: TStringList);
procedure ExtractItemsInCommon(slFirst, slSecond, slDest: TStringList);

implementation

uses CcConf, CcReplicator {$IFDEF CC_D2K12},CcComparer {$ENDIF}{$IFNDEF FPC}, CcMemDS, CcDB {$ELSE} , CcProvLazSQLDB, LResources{$ENDIF};

procedure Register;
begin
	RegisterComponents('CopyCat', [TCcReplicator, TCcConfig, {$IFDEF CC_D2K12}TCcComparer, {$ENDIF}TCcQuery{$IFNDEF NEXTGEN}{$IFNDEF FPC}, TCcMemoryData, TCcDataSet{$ELSE} , TCcConnectionSQLDB{$ENDIF}{$ENDIF}]);
end;

function ReplaceString(S:String; sub1:String; sub2:String) : String;
var
  pos1:integer;
  s2,res:String;
begin
  s2 := s;
  pos1 := pos(UpperCase(sub1), UpperCase(s2));

  res:='';

  while (pos1<>0) do
  begin
    res:=res+copy(s2,1,pos1-1)+sub2;
    s2:=copy(s2,pos1+length(sub1),length(s2)+1-(pos1+length(sub1))); //le fait sauf au dernier passage
    pos1:=pos(UpperCase(sub1),UpperCase(s2));
  end;
  res:=res+s2;
  Result:=res;
end;

function ReadBinaryStream(ini: TCustomIniFile; const Section, Name: string;
  Value: TStream): Integer;
var
  Text: string;
  Stream: TMemoryStream;
  Pos: Integer;
{$IFDEF NEXTGEN}
  buf, bufOut: TBytes;
{$ENDIF}
begin
  Text := ini.ReadString(Section, Name, '');
  if Text <> '' then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      Pos := Stream.Position;
      Stream.SetSize(Int64(Stream.Size + Length(Text) div 2));
{$IFNDEF NEXTGEN}
      HexToBin(PChar(Text), PChar(Integer(Stream.Memory) + Stream.Position), Length(Text) div 2);
{$ELSE}
    SetLength(buf, Length(Text) * SizeOf(Char));
    if Text <> '' then Move(Text[1], buf[0], Length(buf));
    HexToBin(buf, 1, bufOut, 0, Length(Text) div 2);
    Move(bufOut[0], PChar(Integer(Stream.Memory) + Stream.Position)[0], Length(bufOut));
{$ENDIF}
      Stream.Position := Pos;
      if Value <> Stream then
        Value.CopyFrom(Stream, Length(Text) div 2);
      Result := Stream.Size - Pos;
    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end
  else
    Result := 0;
end;

procedure WriteBinaryStream(ini: TCustomIniFile; const Section, Name: string;
  Value: TStream);
var
  Text: string;
  Stream: TMemoryStream;
{$IFDEF NEXTGEN}
  buf, bufOut: TBytes;
{$ENDIF}
begin
  SetLength(Text, (Value.Size - Value.Position) * 2);
  if Length(Text) > 0 then
  begin
    if Value is TMemoryStream then
      Stream := TMemoryStream(Value)
    else
      Stream := TMemoryStream.Create;

    try
      if Stream <> Value then
      begin
        Stream.CopyFrom(Value, Value.Size - Value.Position);
        Stream.Position := 0;
      end;

{$IFNDEF NEXTGEN}
      BinToHex(PChar(Integer(Stream.Memory) + Stream.Position), PChar(Text),
        Stream.Size - Stream.Position);
{$ELSE}
    SetLength(buf, Stream.Size - Stream.Position);
    Move(PChar(Integer(Stream.Memory) + Stream.Position)[0], buf[0], Length(buf));
    BinToHex(buf, 0, bufOut, 0, Length(buf));
    Move(bufOut[0], Text[1], Length(bufOut));
{$ENDIF}


    finally
      if Value <> Stream then
        Stream.Free;
    end;
  end;
  ini.WriteString(Section, Name, Text);
end;

//This method removes items in common between two TStringLists
//Any items of found in both string lists are removed from the first
procedure RemoveItemsInCommon(slFirst, slSecond: TStringList);
var
  i:Integer;
begin
  for I := slFirst.Count - 1 downto 0 do
    if (slSecond.IndexOf(slFirst.Strings[i]) <> -1) then
      slFirst.Delete(i);
end;

//This method lists items in common between two TStringLists into a third TStringList
//Any items of found in both string lists are removed from the first
procedure ExtractItemsInCommon(slFirst, slSecond, slDest: TStringList);
var
  i:Integer;
begin
  for I := slFirst.Count - 1 downto 0 do
    if (slSecond.IndexOf(slFirst.Strings[i]) <> -1) then
      slDest.Add(slFirst.Strings[i]);
end;

{$IFDEF FPC}
initialization
  {$I copycat.lrs}
  RegisterPackage( 'CopyCat', @Register );
{$ENDIF}

end.
