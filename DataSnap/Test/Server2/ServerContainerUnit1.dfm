object ServerContainer1: TServerContainer1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 358
  Width = 415
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSTCPServerTransport1: TDSTCPServerTransport
    Server = DSServer1
    Filters = <>
    Left = 96
    Top = 73
  end
  object CcDSServerTransportLink1: TCcDSServerTransportLink
    Server = DSServer1
    Left = 200
    Top = 8
  end
  object CcDSServerTransport1: TCcDSServerTransport
    DatabaseAlias = 'TEST'
    Connection = CcConnectionFireDAC1
    Left = 216
    Top = 128
  end
  object CcConnectionFireDAC1: TCcConnectionFireDAC
    FDConnection = FDConnection1
    FDTransaction = FDTransaction1
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 216
    Top = 184
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=fdl'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=fB')
    LoginPrompt = False
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 216
    Top = 248
  end
  object FDTransaction1: TFDTransaction
    Connection = FDConnection1
    Left = 312
    Top = 264
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 312
    Top = 112
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 344
    Top = 200
  end
end
