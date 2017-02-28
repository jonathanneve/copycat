object frServerOptions: TfrServerOptions
  Left = 0
  Top = 0
  Width = 466
  Height = 145
  TabOrder = 0
  object Label1: TLabel
    Left = 293
    Top = 7
    Width = 82
    Height = 13
    Caption = 'Server password:'
  end
  object Label2: TLabel
    Left = 13
    Top = 7
    Width = 70
    Height = 13
    Caption = 'Database alias'
  end
  object Label3: TLabel
    Left = 13
    Top = 31
    Width = 122
    Height = 13
    Caption = 'Session timeout (minutes):'
  end
  object edPassword: TEdit
    Left = 381
    Top = 3
    Width = 65
    Height = 21
    TabOrder = 1
  end
  object edDBAlias: TEdit
    Left = 101
    Top = 3
    Width = 156
    Height = 21
    TabOrder = 0
  end
  object edSessionTimeout: TEdit
    Left = 141
    Top = 27
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '10'
  end
  object GroupBox1: TGroupBox
    Left = 10
    Top = 49
    Width = 439
    Height = 88
    Caption = 'RTC options'
    TabOrder = 3
    object Label4: TLabel
      Left = 13
      Top = 18
      Width = 60
      Height = 13
      Caption = 'Secure key :'
    end
    object Label5: TLabel
      Left = 237
      Top = 18
      Width = 76
      Height = 13
      Caption = 'Encryption key :'
    end
    object Label6: TLabel
      Left = 13
      Top = 42
      Width = 86
      Height = 13
      Caption = 'Module file name :'
    end
    object Label7: TLabel
      Left = 13
      Top = 66
      Width = 64
      Height = 13
      Caption = 'Module host :'
    end
    object Label8: TLabel
      Left = 237
      Top = 42
      Width = 66
      Height = 13
      Caption = 'Compression :'
    end
    object ListenPortLabel: TLabel
      Left = 237
      Top = 66
      Width = 52
      Height = 13
      Caption = 'Listen port:'
    end
    object edSecureKey: TEdit
      Left = 104
      Top = 14
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edEncryptionKey: TEdit
      Left = 322
      Top = 14
      Width = 108
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object edModuleFileName: TEdit
      Left = 104
      Top = 38
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '/'
    end
    object edModuleHost: TEdit
      Left = 104
      Top = 62
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object cbCompression: TComboBox
      Left = 322
      Top = 38
      Width = 110
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = 'None'
      Items.Strings = (
        'None'
        'Fast'
        'Default'
        'Max')
    end
    object ListenPortEdit: TEdit
      Left = 322
      Top = 62
      Width = 65
      Height = 21
      TabOrder = 5
      Text = '80'
    end
  end
  object RtcHttpServer1: TRtcHttpServer
    Left = 240
    Top = 3
  end
  object RtcServerModule1: TRtcServerModule
    Server = RtcHttpServer1
    Compression = cMax
    ModuleFileName = '/'
    FunctionGroup = RtcFunctionGroup1
    Left = 272
    Top = 3
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 304
    Top = 3
  end
  object ServerTransport: TCcRtcServerTransport
    Server = RtcHttpServer1
    FunctionGroup = RtcFunctionGroup1
    DataFormat = fmt_RTC
    Left = 336
    Top = 3
  end
end
