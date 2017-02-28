object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 477
  Height = 141
  TabOrder = 0
  object Label10: TLabel
    Left = 243
    Top = 19
    Width = 42
    Height = 13
    Caption = 'Version :'
  end
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 47
    Height = 13
    Caption = 'DB Type :'
  end
  object cbVersions: TComboBox
    Left = 291
    Top = 16
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object cbDBType: TComboBox
    Left = 83
    Top = 16
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbDBTypeChange
  end
  object Button1: TButton
    Left = 83
    Top = 56
    Width = 145
    Height = 25
    Caption = 'Configure connection'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Connection: TCcConnectionFireDAC
    FDConnection = FDConnection
    FDTransaction = FDTransaction1
    Left = 264
    Top = 80
  end
  object FDConnection: TFDConnection
    LoginPrompt = False
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 328
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 352
  end
  object FDPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 448
  end
  object FDPhysDB2DriverLink1: TFDPhysDB2DriverLink
    Left = 424
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 400
  end
  object FDPhysODBCDriverLink1: TFDPhysODBCDriverLink
    Left = 376
  end
  object FDPhysMSAccessDriverLink1: TFDPhysMSAccessDriverLink
    Left = 448
    Top = 112
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 352
    Top = 112
  end
  object FDPhysASADriverLink1: TFDPhysASADriverLink
    Left = 424
    Top = 112
  end
  object FDPhysADSDriverLink1: TFDPhysADSDriverLink
    Left = 400
    Top = 112
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 376
    Top = 112
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 320
    Top = 112
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 312
    Top = 144
  end
  object FDTransaction1: TFDTransaction
    Connection = FDConnection
    Left = 264
    Top = 48
  end
end
