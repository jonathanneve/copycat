object Form1: TForm1
  Left = 366
  Top = 346
  Caption = 'CopyCat Test App'
  ClientHeight = 145
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbOk: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 129
    Width = 234
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Caption = 'All tests passed OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    ExplicitWidth = 107
  end
  object lbConnectOk: TLabel
    Left = 187
    Top = 12
    Width = 15
    Height = 13
    Caption = 'Ok'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object lbBlobOk: TLabel
    Left = 187
    Top = 70
    Width = 15
    Height = 13
    Caption = 'Ok'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Button1: TButton
    Left = 57
    Top = 7
    Width = 113
    Height = 25
    Caption = 'Test connection'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 57
    Top = 65
    Width = 113
    Height = 25
    Caption = 'Test blob data'
    TabOrder = 1
    OnClick = Button2Click
  end
  object edFile: TEdit
    Left = 3
    Top = 38
    Width = 167
    Height = 21
    TabOrder = 2
  end
  object Button3: TButton
    Left = 176
    Top = 38
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 57
    Top = 96
    Width = 113
    Height = 28
    Caption = 'Replicate'
    TabOrder = 4
    OnClick = Button4Click
  end
  object OpenDialog1: TOpenDialog
    Left = 211
    Top = 65535
  end
  object CcConnectionIBX1: TCcConnectionIBX
    DBType = 'Interbase'
    DBVersion = 'IB6'
    DBName = 'localhost/3051:c:\temp\equip_local.fbdb'
    SQLDialect = 1
    TRParams.Strings = (
      'write'
      'nowait'
      'rec_version')
    Left = 203
    Top = 87
  end
  object CcXmlRpcServer1: TCcXmlRpcServer
    EnableIntrospect = False
    ListenPort = 80
    Active = True
    Left = 139
    Top = 87
  end
  object CcXmlRpcClientTransport1: TCcXmlRpcClientTransport
    DBType = 'Interbase'
    DBVersion = 'IB6'
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    HostName = 'localhost'
    HostPort = 80
    DatabaseAlias = 'EQUIP_LOCAL'
    Left = 107
    Top = 87
  end
  object CcXmlRpcServerTransport1: TCcXmlRpcServerTransport
    DatabaseAlias = 'EQUIP_LOCAL'
    Connection = CcConnectionIBX1
    Server = CcXmlRpcServer1
    Left = 171
    Top = 87
  end
  object CcReplicator1: TCcReplicator
    HarmonizeFields = False
    KeepConnection = False
    Nodes.LocalNode.Connection = CcConnectionADO1
    Nodes.LocalNode.Name = 'LOCAL'
    Nodes.RemoteNode.Connection = CcConnectionIBX2
    Nodes.RemoteNode.Name = 'EQUIPM2000'
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = False
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctCommit
    AbortOnError = False
    Left = 75
    Top = 87
  end
  object CcConnectionIBX2: TCcConnectionIBX
    DBType = 'Interbase'
    DBVersion = 'IB6'
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    DBName = 'localhost/3050:c:\temp\equip_local.fbdb'
    SQLDialect = 1
    TRParams.Strings = (
      'write'
      'nowait'
      'rec_version')
    Left = 203
    Top = 47
  end
  object CcConnectionADO1: TCcConnectionADO
    DBType = 'MSSQL'
    DBVersion = 'MSSQL2000'
    ConnectionString = 
      'Provider=SQLOLEDB;Data Source=KANGAROO\SQLExpress;Initial catalo' +
      'g=CopyCatTests;Integrated Security=SSPI'
    Left = 19
    Top = 27
  end
  object CcConfig1: TCcConfig
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = CcConnectionADO1
    Left = 19
    Top = 3
  end
end
