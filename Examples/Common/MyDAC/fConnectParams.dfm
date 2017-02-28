object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 477
  Height = 134
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 65
    Width = 82
    Height = 13
    Caption = 'Database name :'
  end
  object Label2: TLabel
    Left = 16
    Top = 87
    Width = 58
    Height = 13
    Caption = 'User name :'
  end
  object Label3: TLabel
    Left = 294
    Top = 87
    Width = 53
    Height = 13
    Caption = 'Password :'
  end
  object Label4: TLabel
    Left = 16
    Top = 109
    Width = 27
    Height = 13
    Caption = 'Port :'
  end
  object Label10: TLabel
    Left = 16
    Top = 22
    Width = 42
    Height = 13
    Caption = 'Version :'
  end
  object Label5: TLabel
    Left = 16
    Top = 43
    Width = 39
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
    TabOrder = 0
  end
  object edServer: TEdit
    Left = 107
    Top = 39
    Width = 364
    Height = 21
    TabOrder = 5
  end
  object FDTransaction1: TFDTransaction
    Options.AutoStop = False
    Connection = FDConnection1
    Left = 288
    Top = 16
  end
  object Connection: TCcConnectionFireDAC
    FDConnection = FDConnection1
    FDTransaction = FDTransaction1
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 288
    Top = 64
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=FB'
      'User_Name=sysdba'
      'Password=masterkey')
    TxOptions.AutoStop = False
    Left = 248
    Top = 90
  end
end
