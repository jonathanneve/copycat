object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'CopyCat server'
  ClientHeight = 510
  ClientWidth = 964
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Conn: TLabel
    Left = 8
    Top = 8
    Width = 84
    Height = 13
    Caption = 'Connected nodes'
  end
  object Button1: TButton
    Left = 8
    Top = 169
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 200
    Width = 948
    Height = 302
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ListBox1: TListBox
    Left = 8
    Top = 26
    Width = 225
    Height = 137
    ItemHeight = 13
    TabOrder = 2
  end
  object btStartServer: TButton
    Left = 158
    Top = 1
    Width = 75
    Height = 25
    Caption = 'Start server'
    TabOrder = 3
    OnClick = btStartServerClick
  end
  object CcRtcServerGateway1: TCcRtcServerGateway
    ServerModule = RtcServerModule1
    KeepAliveFrequency = 2000
    NodeSessionTimeout = 5000
    OnNodeConnect = CcRtcServerGateway1NodeConnect
    OnNodeDisconnect = CcRtcServerGateway1NodeConnect
    Left = 184
    Top = 104
  end
  object CcRtcServerConnection1: TCcRtcServerConnection
    AutoCommit = False
    DatabaseAlias = 'TEST'
    Password = 'test'
    RequestTimeout = 5000
    EncodeStringsAsBase64 = True
    Gateway = CcRtcServerGateway1
    Left = 328
    Top = 112
  end
  object RtcServerModule1: TRtcServerModule
    Server = RtcHttpServer1
    ModuleFileName = '/'
    FunctionGroup = RtcFunctionGroup1
    Left = 296
    Top = 32
  end
  object RtcHttpServer1: TRtcHttpServer
    MultiThreaded = True
    ServerAddr = 'localhost'
    ServerPort = '8080'
    Left = 160
    Top = 32
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 456
    Top = 32
  end
  object CcDataSet1: TCcDataSet
    SQL.Strings = (
      'select * from article')
    Connection = CcRtcServerConnection1
    Left = 400
    Top = 168
  end
  object DataSource1: TDataSource
    DataSet = CcDataSet1
    Left = 328
    Top = 168
  end
end
