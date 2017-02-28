object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 477
  Height = 121
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 41
    Width = 81
    Height = 13
    Caption = 'Database name :'
  end
  object SpeedButton1: TSpeedButton
    Left = 448
    Top = 38
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 16
    Top = 65
    Width = 57
    Height = 13
    Caption = 'User name :'
  end
  object Label3: TLabel
    Left = 294
    Top = 65
    Width = 52
    Height = 13
    Caption = 'Password :'
  end
  object Label4: TLabel
    Left = 16
    Top = 89
    Width = 42
    Height = 13
    Caption = 'Charset :'
  end
  object Label5: TLabel
    Left = 294
    Top = 89
    Width = 39
    Height = 13
    Caption = 'Dialect :'
  end
  object Label10: TLabel
    Left = 16
    Top = 19
    Width = 41
    Height = 13
    Caption = 'Version :'
  end
  object edDBName: TEdit
    Left = 107
    Top = 38
    Width = 340
    Height = 21
    TabOrder = 1
  end
  object edUserName: TEdit
    Left = 107
    Top = 62
    Width = 134
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 2
  end
  object edPassword: TEdit
    Left = 353
    Top = 62
    Width = 118
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object cbDialect: TComboBox
    Left = 353
    Top = 85
    Width = 59
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      '1'
      '2'
      '3')
  end
  object edCharset: TEdit
    Left = 107
    Top = 86
    Width = 134
    Height = 21
    TabOrder = 4
  end
  object cbVersions: TComboBox
    Left = 107
    Top = 13
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object OpenDialog: TOpenDialog
    Filter = 'Interbase databases|*.gdb|FireBird databases|*.fdb|Any file|*.*'
    Left = 424
    Top = 80
  end
  object Connection: TCcConnectionIBX
    DBType = 'Interbase'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Left = 272
    Top = 8
  end
end
