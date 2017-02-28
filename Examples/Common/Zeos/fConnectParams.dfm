object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 477
  Height = 190
  TabOrder = 0
  object Label10: TLabel
    Left = 280
    Top = 13
    Width = 41
    Height = 13
    Caption = 'Version :'
  end
  object Label1: TLabel
    Left = 16
    Top = 81
    Width = 81
    Height = 13
    Caption = 'Database name :'
  end
  object SpeedButton1: TSpeedButton
    Left = 449
    Top = 79
    Width = 25
    Height = 20
    Caption = '...'
  end
  object Label2: TLabel
    Left = 16
    Top = 104
    Width = 57
    Height = 13
    Caption = 'User name :'
  end
  object Label3: TLabel
    Left = 294
    Top = 104
    Width = 52
    Height = 13
    Caption = 'Password :'
  end
  object Label4: TLabel
    Left = 280
    Top = 58
    Width = 42
    Height = 13
    Caption = 'Catalog :'
  end
  object Label9: TLabel
    Left = 16
    Top = 35
    Width = 57
    Height = 13
    Caption = 'Host name :'
  end
  object Label5: TLabel
    Left = 16
    Top = 13
    Width = 44
    Height = 13
    Caption = 'DB type :'
  end
  object Label6: TLabel
    Left = 16
    Top = 58
    Width = 45
    Height = 13
    Caption = 'Protocol :'
  end
  object Label7: TLabel
    Left = 280
    Top = 35
    Width = 25
    Height = 13
    Caption = 'Port :'
  end
  object cbVersions: TComboBox
    Left = 331
    Top = 8
    Width = 141
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object edDBName: TEdit
    Left = 107
    Top = 78
    Width = 340
    Height = 21
    TabOrder = 1
  end
  object edUserName: TEdit
    Left = 107
    Top = 101
    Width = 134
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 2
  end
  object edPassword: TEdit
    Left = 353
    Top = 101
    Width = 118
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object edHostName: TEdit
    Left = 107
    Top = 32
    Width = 150
    Height = 21
    TabOrder = 4
  end
  object cbDBType: TComboBox
    Left = 107
    Top = 8
    Width = 150
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
  end
  object cbCatalog: TComboBox
    Left = 331
    Top = 55
    Width = 141
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
  end
  object cbProtocol: TComboBox
    Left = 107
    Top = 55
    Width = 150
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
  end
  object Edit1: TEdit
    Left = 331
    Top = 32
    Width = 141
    Height = 21
    TabOrder = 8
  end
  object OpenDialog: TOpenDialog
    Filter = 'Interbase databases|*.gdb|FireBird databases|*.fdb|Any file|*.*'
    Left = 384
    Top = 73
  end
  object CcConnectionZeos1: TCcConnectionZeos
    TransactIsolationLevel = tiNone
    DBType = 'Interbase'
    Port = 0
    Left = 296
  end
end
