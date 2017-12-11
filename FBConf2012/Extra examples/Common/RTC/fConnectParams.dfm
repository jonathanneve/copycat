object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 446
  Height = 159
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 27
    Width = 76
    Height = 13
    Caption = 'Database alias :'
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 93
    Height = 13
    Caption = 'Keep-alive interval :'
  end
  object Label3: TLabel
    Left = 206
    Top = 51
    Width = 52
    Height = 13
    Caption = 'Password :'
  end
  object Label9: TLabel
    Left = 8
    Top = 3
    Width = 77
    Height = 13
    Caption = 'Server address :'
  end
  object Label10: TLabel
    Left = 312
    Top = 3
    Width = 58
    Height = 13
    Caption = 'Server port :'
  end
  object edDBAlias: TEdit
    Left = 107
    Top = 24
    Width = 334
    Height = 21
    TabOrder = 2
  end
  object edKeepAliveInterval: TEdit
    Left = 107
    Top = 48
    Width = 70
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 3
    Text = '10'
  end
  object edPassword: TEdit
    Left = 265
    Top = 48
    Width = 176
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
  end
  object GroupBox1: TGroupBox
    Left = 2
    Top = 69
    Width = 439
    Height = 87
    Caption = 'RTC options'
    TabOrder = 5
    object Label4: TLabel
      Left = 13
      Top = 17
      Width = 60
      Height = 13
      Caption = 'Secure key :'
    end
    object Label5: TLabel
      Left = 237
      Top = 17
      Width = 76
      Height = 13
      Caption = 'Encryption key :'
    end
    object Label6: TLabel
      Left = 13
      Top = 41
      Width = 86
      Height = 13
      Caption = 'Module file name :'
    end
    object Label7: TLabel
      Left = 13
      Top = 65
      Width = 64
      Height = 13
      Caption = 'Module host :'
    end
    object Label8: TLabel
      Left = 237
      Top = 41
      Width = 66
      Height = 13
      Caption = 'Compression :'
    end
    object edSecureKey: TEdit
      Left = 104
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edEncryptionKey: TEdit
      Left = 322
      Top = 13
      Width = 108
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object edModuleFileName: TEdit
      Left = 104
      Top = 37
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '/'
    end
    object edModuleHost: TEdit
      Left = 104
      Top = 61
      Width = 121
      Height = 21
      TabOrder = 4
    end
    object cbCompression: TComboBox
      Left = 322
      Top = 37
      Width = 110
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = 'None'
      Items.Strings = (
        'None'
        'Fast'
        'Default'
        'Max')
    end
  end
  object edServerAddress: TEdit
    Left = 107
    Top = 0
    Width = 190
    Height = 21
    TabOrder = 0
  end
  object edServerPort: TEdit
    Left = 376
    Top = 0
    Width = 65
    Height = 21
    TabOrder = 1
    Text = '80'
  end
  object Connection: TCcRtcClientTransport
    KeepAliveInterval = 30000
    ClientModule = ClientModule
    Left = 192
  end
  object HttpClient: TRtcHttpClient
    Left = 256
  end
  object ClientModule: TRtcClientModule
    Client = HttpClient
    Compression = cDefault
    ModuleFileName = '/'
    Left = 224
  end
end
