unit CctConnectors;

interface

uses Classes, Forms, dtConnector, DB , System.Math , Data.SqlTimSt;

type

TCctConnectorLists = class(TList)
  private
    function GetConnectorList(nIndex: Integer): TCctConnectorList;
  public
    property ConnectorList[nIndex: Integer]: TCctConnectorList read GetConnectorList;
end;

function CreateArrayOfConnectors: TCctConnectorLists;
function CreateTestDataArray(slDataTypes: TStringList): Variant;
function GetTestData(dataType: TFieldType): Variant;

implementation

uses Variants, Sysutils, dtconnectorFireDAC, dtConnectorADO;

function CreateArrayOfConnectors: TCctConnectorLists;
begin
  Result := TCctConnectorLists.Create;
  Result.Add(TdmtConnectorFireDAC.CreateTestsByVersion);
//  Result.Add(TdmtConnectorNexusDB.CreateTestsByVersion);
//  Result.Add(TdmtConnectorIBX.CreateTestsByVersion);
//  Result.Add(TdmtConnectorIBO.CreateTestsByVersion);
  //Result.Add(TdmtConnectorADO.CreateTestsByVersion);
//  Result.Add(TdmtConnectorFIB.CreateTestsByVersion);

  //UIB has some problem with BLOB or MEMO type parameters...
//  Result.Add(TdmtConnectorUIB.CreateTestsByVersion);

  //Zeos doesn't seem to support RowsAffected nor RecordCount...
//  Result.Add(TdmtConnectorZeos.CreateTestsByVersion);
end;


{ TCctConnectorLists }

function TCctConnectorLists.GetConnectorList(
  nIndex: Integer): TCctConnectorList;
begin
  Result := TCctConnectorList(Items[nIndex]);
end;

function GetTestData(dataType: TFieldType): Variant;
begin
  Randomize;

  case dataType of
    ftInteger: Result := random(1000);
    ftLargeInt: Result := random(1000000000000000000);
    ftFloat,ftSingle  : Result := RoundTo(random*1000,-3);
    ftCurrency  : Result := RoundTo(random*1000,-2);
    ftString,ftWideString,ftMemo,ftWideMemo: Result := ' ,zaef çaez0 PFRELBUzçàbtZR0Tvbse rvrlkee rqg !!az!:e; faez,:f; ezf! az:!f ';
    ftBlob,ftOraBlob   : begin
      Result := VarArrayCreate([0, 3], varByte);
      Result[0] := random(256);
      Result[1] := random(256);
      Result[2] := random(256);
      Result[3] := random(256);
    end;
    ftDateTime : Result := Now;
    ftTimeStamp : Result := VarSQLTimeStampCreate(Now);
    ftDate   : Result := Date;
    ftTime   : Result := Time;
    ftBoolean   : Result := 1;
    else
      Result := 0;
  end;
end;

function CreateTestDataArray(slDataTypes: TStringList): Variant;
var
  i: Integer;
begin
  Result := VarArrayCreate([0, slDataTypes.Count-1], varVariant);
  for I := 0 to slDataTypes.Count-1 do begin
    Result[I] := GetTestData(TFieldType(StrToInt(slDataTypes.Values[slDataTypes.Names[i]])));
  end;
end;

end.
