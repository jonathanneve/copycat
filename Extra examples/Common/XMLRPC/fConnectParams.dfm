object frConnectParams: TfrConnectParams
  Left = 0
  Top = 0
  Width = 445
  Height = 77
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 27
    Width = 76
    Height = 13
    Caption = 'Database alias :'
  end
  object Label2: TLabel
    Left = 16
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
    Left = 16
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
    Left = 115
    Top = 24
    Width = 326
    Height = 21
    TabOrder = 2
  end
  object edKeepAliveInterval: TEdit
    Left = 115
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
  object edServerAddress: TEdit
    Left = 115
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
  object Connection: TCcXmlRpcClientTransport
    HostPort = 8080
    Left = 312
    Top = 8
  end
end
