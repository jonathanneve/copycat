object Form1: TForm1
  Left = 304
  Top = 200
  Width = 323
  Height = 273
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 168
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Replicate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Config'
    TabOrder = 1
    OnClick = Button2Click
  end
  object CcConnectionIBX1: TCcConnectionIBX
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'data\test.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Left = 64
    Top = 64
  end
  object CcReplicator1: TCcReplicator
    Version = '3.05.0'
    TrimCharFields = False
    AutoPriority = True
    LogErrors = False
    HarmonizeFields = False
    KeepConnection = True
    Nodes.LocalNode.Connection = CcConnectionIBX2
    Nodes.LocalNode.Name = 'NODE2'
    Nodes.RemoteNode.Connection = CcConnectionIBX1
    Nodes.RemoteNode.Name = 'NODE1'
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = False
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctCommit
    AbortOnError = False
    Left = 88
    Top = 152
  end
  object CcConfig1: TCcConfig
    Version = '3.05.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'NODE2')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = CcConnectionIBX1
    Tables = <
      item
        TableName = 'TEST'
      end>
    Left = 64
    Top = 104
  end
  object CcConfig2: TCcConfig
    Version = '3.05.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'NODE1')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = CcConnectionIBX2
    Tables = <
      item
        TableName = 'TEST'
      end>
    Left = 104
    Top = 104
  end
  object CcConnectionIBX2: TCcConnectionIBX
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'data\test_2.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Left = 104
    Top = 64
  end
end
