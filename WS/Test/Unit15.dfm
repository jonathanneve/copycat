object Form15: TForm15
  Left = 0
  Top = 0
  Caption = 'Form15'
  ClientHeight = 299
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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Connect client'
    TabOrder = 1
    OnClick = Button2Click
  end
  object DBGrid1: TDBGrid
    Left = 89
    Top = 8
    Width = 538
    Height = 283
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Open dataset'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Start gateway'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 231
    Width = 75
    Height = 25
    Caption = 'Connect client'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button6: TButton
    Left = 8
    Top = 262
    Width = 75
    Height = 25
    Caption = 'Open dataset'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button7: TButton
    Left = 8
    Top = 203
    Width = 75
    Height = 25
    Caption = 'Connect agent'
    TabOrder = 7
    OnClick = Button7Click
  end
  object CcConnection: TCcConnectionFireDAC
    FDConnection = FDConnection1
    FDTransaction = FDTransaction1
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    Left = 184
    Top = 64
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=localhost/3050:c:\temp\medicontrol.fdb'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=FB')
    TxOptions.AutoStop = False
    LoginPrompt = False
    Transaction = FDTransaction1
    UpdateTransaction = FDTransaction1
    Left = 320
    Top = 24
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 328
    Top = 208
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 408
    Top = 168
  end
  object FDTransaction1: TFDTransaction
    Options.AutoStop = False
    Connection = FDConnection1
    Left = 320
    Top = 72
  end
  object CcDataSet: TCcDataSet
    SQL.Strings = (
      'select * from beroepen')
    Left = 40
    Top = 88
  end
  object DataSource1: TDataSource
    DataSet = CcDataSet
    Left = 40
    Top = 136
  end
end
