object frServerOptions: TfrServerOptions
  Left = 0
  Top = 0
  Width = 439
  Height = 107
  TabOrder = 0
  object ListenPortLabel: TLabel
    Left = 269
    Top = 60
    Width = 52
    Height = 13
    Caption = 'Listen port:'
  end
  object Label1: TLabel
    Left = 269
    Top = 28
    Width = 82
    Height = 13
    Caption = 'Server password:'
  end
  object Label2: TLabel
    Left = 13
    Top = 28
    Width = 70
    Height = 13
    Caption = 'Database alias'
  end
  object Label3: TLabel
    Left = 13
    Top = 60
    Width = 122
    Height = 13
    Caption = 'Session timeout (minutes):'
  end
  object ListenPortEdit: TEdit
    Left = 357
    Top = 56
    Width = 65
    Height = 21
    TabOrder = 0
    Text = '80'
  end
  object edPassword: TEdit
    Left = 357
    Top = 24
    Width = 65
    Height = 21
    TabOrder = 1
  end
  object edDBAlias: TEdit
    Left = 101
    Top = 24
    Width = 156
    Height = 21
    TabOrder = 2
  end
  object edSessionTimeout: TEdit
    Left = 141
    Top = 56
    Width = 65
    Height = 21
    TabOrder = 3
    Text = '10'
  end
  object ServerTransport: TCcXmlRpcServerTransport
    Server = XmlRpcServer
    Left = 208
    Top = 64
  end
  object XmlRpcServer: TCcXmlRpcServer
    EnableIntrospect = False
    ListenPort = 80
    Active = False
    Left = 248
    Top = 64
  end
end
