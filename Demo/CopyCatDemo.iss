[Files]
Source: C:\Projects\Replicator\Demo\CopyCat.exe; DestDir: {app}
Source: C:\Projects\Replicator\Demo\local.gbk; DestDir: {app}
Source: C:\Projects\Replicator\Demo\CopyCatDemo.hlp; DestDir: {app}
Source: C:\Projects\Replicator\Demo\remote.gbk; DestDir: {app}
Source: C:\Projects\Replicator\Demo\CopyCatDemo.cnt; DestDir: {app}
Source: C:\Projects\Replicator\Demo\RemoteProcs.ini; DestDir: {app}
Source: C:\Projects\Replicator\Demo\RemoteTables.ini; DestDir: {app}
Source: C:\Projects\Replicator\Demo\LocalProcs.ini; DestDir: {app}
Source: C:\Projects\Replicator\Demo\LocalTables.ini; DestDir: {app}
[Setup]
DefaultDirName={pf}\CopyCat Demo
ShowLanguageDialog=yes
OutputDir=c:\projects\replicator\demo
OutputBaseFilename=CCDemoSetup
AppCopyright=Microtec Communications
AppName=CopyCat Demo
AppVerName=Microtec CopyCat v.1.0
[_ISTool]
UseAbsolutePaths=true
[Run]
Filename: {app}\CopyCat.exe; Flags: nowait shellexec postinstall; Description: Launch CopyCat Demo
Filename: {app}\CopyCatDemo.hlp; Flags: nowait shellexec postinstall; Description: Show CopyCat help file
