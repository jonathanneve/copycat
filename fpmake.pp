{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for CopyCat 0.0

   This file was generated on 23/03/15
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_CopyCat(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('copycat');
    P.Version:='0.0';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.IncludePath.Add('.');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('copycat.pas');
    t.Dependencies.AddUnit('CCat');
    t.Dependencies.AddUnit('CcConf');
    t.Dependencies.AddUnit('CcConflictMgr');
    t.Dependencies.AddUnit('CcDB');
    t.Dependencies.AddUnit('CcEditors');
    t.Dependencies.AddUnit('CcInterbase');
    t.Dependencies.AddUnit('CcKeys');
    t.Dependencies.AddUnit('CcLog');
    t.Dependencies.AddUnit('CcMySQL');
    t.Dependencies.AddUnit('CcNexusDB');
    t.Dependencies.AddUnit('CcOracle');
    t.Dependencies.AddUnit('CcPostgres');
    t.Dependencies.AddUnit('CcProviders');
    t.Dependencies.AddUnit('CcReplicator');
    t.Dependencies.AddUnit('CcSQLite');
    t.Dependencies.AddUnit('CcSQLServer');
    t.Dependencies.AddUnit('CcTransports');

    T:=P.Targets.AddUnit('CCat.pas');
    T:=P.Targets.AddUnit('CcConf.pas');
    T:=P.Targets.AddUnit('CcConflictMgr.pas');
    T:=P.Targets.AddUnit('CcDB.pas');
    T:=P.Targets.AddUnit('CcEditors.pas');
    T:=P.Targets.AddUnit('CcInterbase.pas');
    T:=P.Targets.AddUnit('CcKeys.pas');
    T:=P.Targets.AddUnit('CcLog.pas');
    T:=P.Targets.AddUnit('CcMySQL.pas');
    T:=P.Targets.AddUnit('CcNexusDB.pas');
    T:=P.Targets.AddUnit('CcOracle.pas');
    T:=P.Targets.AddUnit('CcPostgres.pas');
    T:=P.Targets.AddUnit('CcProviders.pas');
    T:=P.Targets.AddUnit('CcReplicator.pas');
    T:=P.Targets.AddUnit('CcSQLite.pas');
    T:=P.Targets.AddUnit('CcSQLServer.pas');
    T:=P.Targets.AddUnit('CcTransports.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('CopyCat.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_CopyCat('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
