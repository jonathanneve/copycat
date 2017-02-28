inherited dmtConnectorUIB: TdmtConnectorUIB
  OldCreateOrder = True
  object CcConnectionUIB: TCcConnectionUIB
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Options = [tpNowait, tpWrite]
    LibraryName = 'gds32.dll'
    Left = 200
    Top = 168
  end
end
