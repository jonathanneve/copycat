{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit copycat;

interface

uses
  CCat, CcConf, CcConflictMgr, CcDB, CcEditors, CcInterbase, CcKeys, CcLog, 
  CcMySQL, CcNexusDB, CcOracle, CcPostgres, CcProviders, CcReplicator, 
  CcSQLite, CcSQLServer, CcTransports, CcProvLazSQLDB, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CCat', @CCat.Register);
end;

initialization
  RegisterPackage('copycat', @Register);
end.
