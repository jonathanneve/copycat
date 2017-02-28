object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 337
  ClientWidth = 527
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
    Left = 16
    Top = 56
    Width = 153
    Height = 25
    Caption = 'Configure Firebird DB'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CcConfig: TCcConfig
    Version = '3.01.0'
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = FirebirdDB
    Left = 24
    Top = 8
  end
  object FirebirdDB: TCcConnectionUIB
    CharSet = 'NONE'
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'c:\projects\copycat\ITDevCon2011\example.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Options = [tpNowait, tpWrite]
    LibraryName = 'gds32.dll'
    Left = 152
    Top = 8
  end
end
