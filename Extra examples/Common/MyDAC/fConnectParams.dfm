object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 477
  Height = 134
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 65
    Width = 81
    Height = 13
    Caption = 'Database name :'
  end
  object Label2: TLabel
    Left = 16
    Top = 87
    Width = 57
    Height = 13
    Caption = 'User name :'
  end
  object Label3: TLabel
    Left = 294
    Top = 87
    Width = 52
    Height = 13
    Caption = 'Password :'
  end
  object Label4: TLabel
    Left = 16
    Top = 109
    Width = 25
    Height = 13
    Caption = 'Port :'
  end
  object Label10: TLabel
    Left = 16
    Top = 22
    Width = 41
    Height = 13
    Caption = 'Version :'
  end
  object Label5: TLabel
    Left = 16
    Top = 43
    Width = 37
    Height = 13
    Caption = 'Server :'
  end
  object edDBName: TEdit
    Left = 107
    Top = 62
    Width = 364
    Height = 21
    TabOrder = 1
  end
  object edUserName: TEdit
    Left = 107
    Top = 84
    Width = 134
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 2
  end
  object edPassword: TEdit
    Left = 353
    Top = 84
    Width = 118
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object edPort: TEdit
    Left = 107
    Top = 106
    Width = 134
    Height = 21
    TabOrder = 4
  end
  object cbVersions: TComboBox
    Left = 107
    Top = 16
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object edServer: TEdit
    Left = 107
    Top = 39
    Width = 364
    Height = 21
    TabOrder = 5
  end
  object Connection: TCcConnectionMyDAC
    MyConnection = MyConnection1
    DBType = 'MySQL'
    DBVersion = '5.0'
    Left = 304
    Top = 8
  end
  object MyConnection1: TMyConnection
    IsolationLevel = ilRepeatableRead
    Left = 336
    Top = 8
  end
end
