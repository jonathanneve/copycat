object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'CopyCat Agent'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CcRtcClientGateway1: TCcRtcClientGateway
    RowBatchSize = 0
    DatabaseAlias = 'TEST'
    Connection = CcConnectionFireDAC1
    Password = 'test'
    DataFormat = fmt_RTC
    EncodeStringsAsBase64 = True
    ClientModule = RtcClientModule1
    Left = 112
    Top = 136
  end
  object RtcClientModule1: TRtcClientModule
    Client = RtcHttpClient1
    ModuleFileName = '/'
    FunctionGroup = RtcFunctionGroup1
    Left = 224
    Top = 136
  end
  object RtcHttpClient1: TRtcHttpClient
    MultiThreaded = True
    ServerAddr = 'localhost'
    ServerPort = '8080'
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    OnReconnect = RtcHttpClient1Reconnect
    Left = 224
    Top = 80
  end
  object CcConnectionFireDAC1: TCcConnectionFireDAC
    FDConnection = FDConnection1
    FDTransaction = FDTransaction1
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 112
    Top = 64
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=localhost:C:\Projects\simon\data\simon.fdb'
      'SQLDialect=1'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=FB')
    TxOptions.AutoStop = False
    LoginPrompt = False
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 296
    Top = 48
  end
  object FDTransaction1: TFDTransaction
    Options.AutoStop = False
    Connection = FDConnection1
    Left = 400
    Top = 56
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 224
    Top = 192
  end
end
