object Form1: TForm1
  Left = 354
  Top = 197
  Width = 744
  Height = 640
  Caption = 'CopyCat Database Explorer'
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
    Left = 640
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Go!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGridEh1: TDBGridEh
    Left = 24
    Top = 256
    Width = 689
    Height = 337
    DataSource = DataSource1
    FooterColor = clWindow
    FooterFont.Charset = DEFAULT_CHARSET
    FooterFont.Color = clWindowText
    FooterFont.Height = -11
    FooterFont.Name = 'MS Sans Serif'
    FooterFont.Style = []
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    Left = 24
    Top = 8
    Width = 609
    Height = 241
    TabOrder = 2
  end
  object Button2: TButton
    Left = 640
    Top = 8
    Width = 75
    Height = 25
    Caption = '(Dis)Connect'
    TabOrder = 3
    OnClick = Button2Click
  end
  object CcXmlRpcClientTransport1: TCcXmlRpcClientTransport
    DatabaseAlias = 'COPYCAT'
    KeepAliveInterval = 10
    Password = 'cetorcim'
    HostName = 'gideons.ch/dbsync/copycat/CopyCatPHP/server.php'
    HostPort = 80
    Left = 648
    Top = 80
  end
  object CcDataSet1: TCcDataSet
    Connection = CcXmlRpcClientTransport1
    Left = 648
    Top = 112
  end
  object DataSource1: TDataSource
    DataSet = CcDataSet1
    Left = 648
    Top = 144
  end
end
