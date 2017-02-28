inherited dmtConnectorZeos: TdmtConnectorZeos
  object CcConnectionZeos1: TCcConnectionZeos
    TransactIsolationLevel = tiReadCommitted
    DBType = 'Interbase'
    User = 'SYSDBA'
    Protocol = 'firebirdd-2.5'
    Password = 'masterkey'
    Port = 0
    Catalog = 'fdl'
    Left = 24
    Top = 8
  end
end
