object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 202
  ClientWidth = 447
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
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Config'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Replicate'
    TabOrder = 1
    OnClick = Button2Click
  end
  object CcConnectionFireDAC1: TCcConnectionFireDAC
    FDConnection = FDConnection1
    FDTransaction = FDTransaction1
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 80
    Top = 80
  end
  object FDConnection2: TFDConnection
    Params.Strings = (
      'Database=C:\Projects-data\fruitsduloir\OPTIMA_prod.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=fB')
    LoginPrompt = False
    Transaction = FDTransaction2
    UpdateTransaction = FDTransaction2
    Left = 256
    Top = 8
  end
  object FDTransaction2: TFDTransaction
    Connection = FDConnection2
    Left = 336
    Top = 8
  end
  object CcConnectionFireDAC2: TCcConnectionFireDAC
    FDConnection = FDConnection2
    FDTransaction = FDTransaction2
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 304
    Top = 64
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=fdl'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=fB')
    LoginPrompt = False
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 48
    Top = 16
  end
  object FDTransaction1: TFDTransaction
    Connection = FDConnection1
    Left = 128
    Top = 16
  end
  object CcReplicator1: TCcReplicator
    Version = '3.05.0'
    TrimCharFields = False
    AutoPriority = True
    LogErrors = False
    HarmonizeFields = False
    KeepConnection = True
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = False
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctNone
    AbortOnError = False
    Left = 208
    Top = 128
  end
  object CcConfig1: TCcConfig
    Version = '3.05.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'NODE2')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = CcConnectionFireDAC1
    Tables = <
      item
        TableName = 'STOCK'
      end>
    Left = 80
    Top = 128
  end
  object CcConfig2: TCcConfig
    Version = '3.05.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'NODE1')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = CcConnectionFireDAC2
    Tables = <
      item
        TableName = 'STOCK'
      end>
    Left = 304
    Top = 120
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 184
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 216
    Top = 104
  end
end
