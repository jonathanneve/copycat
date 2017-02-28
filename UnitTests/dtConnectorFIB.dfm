inherited dmtConnectorFIB: TdmtConnectorFIB
  OldCreateOrder = True
  object CcConnectionFIB: TCcConnectionFIB
    DBType = 'Interbase'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 176
    Top = 120
  end
end
