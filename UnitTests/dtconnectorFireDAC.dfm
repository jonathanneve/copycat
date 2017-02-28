inherited dmtConnectorFireDAC: TdmtConnectorFireDAC
  OldCreateOrder = True
  Height = 335
  Width = 595
  object FDConnection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey')
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 144
    Top = 24
  end
  object FDTransaction1: TFDTransaction
    Connection = FDConnection
    Left = 232
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 288
    Top = 96
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 424
    Top = 96
  end
  object FDPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 440
    Top = 192
  end
  object CcConnectionFireDAC: TCcConnectionFireDAC
    FDConnection = FDConnection
    FDTransaction = FDTransaction1
    Left = 48
    Top = 24
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 288
    Top = 224
  end
end
