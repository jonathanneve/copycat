object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 282
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 168
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Config'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 87
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Replicate'
    TabOrder = 1
    OnClick = Button2Click
  end
  object memLog: TMemo
    Left = 8
    Top = 121
    Width = 419
    Height = 153
    TabOrder = 2
  end
  object CcDSClientTransport1: TCcDSClientTransport
    AutoCommit = False
    DatabaseAlias = 'TEST'
    SQLConnection = SQLConnection1
    Left = 56
    Top = 24
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'DataSnap'
    KeepConnection = False
    LoginPrompt = False
    Params.Strings = (
      'DriverUnit=Data.DBXDataSnap'
      'HostName=localhost'
      'Port=211'
      'CommunicationProtocol=tcp/ip'
      'DatasnapContext=datasnap/'
      
        'DriverAssemblyLoader=Borland.Data.TDBXClientDriverLoader,Borland' +
        '.Data.DbxClientDriver,Version=19.0.0.0,Culture=neutral,PublicKey' +
        'Token=91d62ebb5b0d1b1b'
      'Filters={}')
    Left = 48
    Top = 80
  end
  object CcConfig: TCcConfig
    TrackFieldChanges = False
    FailIfNoPK = False
    ConfigName = 'TEST'
    Nodes.Strings = (
      '2')
    Connection = CcDSClientTransport1
    Tables = <
      item
        Priority = 0
        TableName = 'TEST'
      end>
    Version = '3.9.2'
    Left = 152
    Top = 24
  end
  object CcConnectionFireDAC1: TCcConnectionFireDAC
    FDConnection = FDConnection1
    FDTransaction = FDTransaction1
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 272
    Top = 96
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=fdl'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=fB')
    TxOptions.AutoStop = False
    LoginPrompt = False
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 272
    Top = 152
  end
  object FDTransaction1: TFDTransaction
    Options.AutoStop = False
    Connection = FDConnection1
    Left = 376
    Top = 158
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 352
    Top = 16
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 376
    Top = 80
  end
  object CcConfig2: TCcConfig
    TrackFieldChanges = False
    FailIfNoPK = False
    ConfigName = 'TEST'
    Nodes.Strings = (
      '1')
    Connection = CcConnectionFireDAC1
    Tables = <
      item
        Priority = 0
        TableName = 'TEST'
      end>
    Version = '3.9.2'
    Left = 216
    Top = 24
  end
  object CcReplicator: TCcReplicator
    TrackInconsistentDeletes = False
    MergeChangedFieldsOnConflict = False
    Direction = sdBoth
    ReplicateOnlyChangedFields = False
    AutoClearMetadata = True
    FailIfNoPK = False
    TrimCharFields = False
    AutoPriority = True
    HarmonizeFields = False
    KeepConnection = False
    Nodes.LocalNode.Connection = CcConnectionFireDAC1
    Nodes.LocalNode.Name = '2'
    Nodes.RemoteNode.Connection = CcDSClientTransport1
    Nodes.RemoteNode.Name = '1'
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = False
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctCommit
    AbortOnError = False
    OnRowReplicated = CcReplicatorRowReplicated
    OnReplicationError = CcReplicatorReplicationError
    OnException = CcReplicatorException
    OnEmptyLog = CcReplicatorEmptyLog
    Version = '3.9.2'
    KeepRowsInLog = False
    Left = 120
    Top = 200
  end
end
