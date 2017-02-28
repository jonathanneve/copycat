inherited dmtConnectorNexusDB: TdmtConnectorNexusDB
  object nxSession1: TnxSession
    ServerEngine = nxServerEngine1
    Left = 136
    Top = 160
  end
  object nxServerEngine1: TnxServerEngine
    SqlEngine = nxSqlEngine1
    ServerName = ''
    Options = []
    TableExtension = 'nx1'
    Left = 136
    Top = 112
  end
  object nxSqlEngine1: TnxSqlEngine
    StmtLogging = False
    StmtLogTableName = 'QueryLog'
    UseFieldCache = False
    Left = 136
    Top = 64
  end
  object nxDatabase: TnxDatabase
    Session = nxSession1
    Left = 136
    Top = 208
  end
  object CcConnectionNexusDB: TCcConnectionNexusDB
    nxDatabase = nxDatabase
    DBType = 'NexusDB'
    DBVersion = '3.12'
    Left = 136
    Top = 264
  end
  object nxSqlTriggerMonitor: TnxSqlTriggerMonitor
    DisplayName = 'SQL Triggers'
    DisplayCategory = 'SQL Engine'
    ServerEngine = nxServerEngine1
    SqlEngine = nxSqlEngine1
    Left = 288
    Top = 104
  end
end
