inherited dmtConnectorIBX: TdmtConnectorIBX
  OldCreateOrder = True
  object CcConnectionIBX: TCcConnectionIBX
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Left = 200
    Top = 168
  end
end
