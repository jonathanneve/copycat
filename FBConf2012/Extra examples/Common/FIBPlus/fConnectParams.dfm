object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 477
  Height = 152
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 33
    Width = 81
    Height = 13
    Caption = 'Database name :'
  end
  object SpeedButton1: TSpeedButton
    Left = 448
    Top = 30
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 16
    Top = 55
    Width = 57
    Height = 13
    Caption = 'User name :'
  end
  object Label3: TLabel
    Left = 294
    Top = 55
    Width = 52
    Height = 13
    Caption = 'Password :'
  end
  object Label4: TLabel
    Left = 16
    Top = 77
    Width = 42
    Height = 13
    Caption = 'Charset :'
  end
  object Label5: TLabel
    Left = 294
    Top = 77
    Width = 39
    Height = 13
    Caption = 'Dialect :'
  end
  object Label9: TLabel
    Left = 16
    Top = 99
    Width = 66
    Height = 13
    Caption = 'Library name :'
  end
  object SpeedButton2: TSpeedButton
    Left = 448
    Top = 96
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton2Click
  end
  object Label10: TLabel
    Left = 16
    Top = 14
    Width = 41
    Height = 13
    Caption = 'Version :'
  end
  object edDBName: TEdit
    Left = 107
    Top = 30
    Width = 340
    Height = 21
    TabOrder = 1
  end
  object edUserName: TEdit
    Left = 107
    Top = 52
    Width = 134
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 2
  end
  object edPassword: TEdit
    Left = 353
    Top = 52
    Width = 118
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object cbDialect: TComboBox
    Left = 353
    Top = 73
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
    Top = 74
    Width = 134
    Height = 21
    TabOrder = 4
  end
  object edClientDLL: TEdit
    Left = 107
    Top = 96
    Width = 340
    Height = 21
    TabOrder = 6
  end
  object cbVersions: TComboBox
    Left = 107
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object OpenDialogDLL: TOpenDialog
    Filter = 'Windows library|*.dll|Any file|*.*'
    Left = 432
    Top = 56
  end
  object OpenDialog: TOpenDialog
    Filter = 'Interbase databases|*.gdb|FireBird databases|*.fdb|Any file|*.*'
    Left = 424
    Top = 88
  end
  object Connection: TCcConnectionFIB
    DBType = 'Interbase'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 264
    Top = 8
  end
end
