object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 460
  Height = 112
  TabOrder = 0
  object Label11: TLabel
    Left = 248
    Top = 6
    Width = 41
    Height = 13
    Caption = 'Version :'
  end
  object Label12: TLabel
    Left = 8
    Top = 29
    Width = 88
    Height = 13
    Caption = 'Connection string :'
  end
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 75
    Height = 13
    Caption = 'Database type :'
  end
  object memMSConnectionStr: TMemo
    Left = 8
    Top = 48
    Width = 449
    Height = 59
    TabOrder = 0
  end
  object cbVersions: TComboBox
    Left = 299
    Top = 3
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object cbDatabaseTypes: TComboBox
    Left = 91
    Top = 3
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbDatabaseTypesChange
  end
  object Connection: TCcConnectionADO
    ConnectionTimeout = 15
    CommandTimeout = 30
    Left = 256
    Top = 24
  end
end
