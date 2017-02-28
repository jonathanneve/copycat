inherited dmtConnectorIBO: TdmtConnectorIBO
  OldCreateOrder = True
  object CcConnectionIBO: TCcConnectionIBO
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'fdl'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    LockWait = False
    RecVersion = False
    Isolation = tiConcurrency
    Left = 48
    Top = 32
  end
end
