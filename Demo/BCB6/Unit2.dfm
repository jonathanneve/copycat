object fmDBLogin: TfmDBLogin
  Left = 312
  Top = 210
  BorderStyle = bsDialog
  Caption = 'Database login'
  ClientHeight = 120
  ClientWidth = 243
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    243
    120)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 18
    Width = 54
    Height = 13
    Caption = 'User name:'
  end
  object Label1: TLabel
    Left = 8
    Top = 50
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 82
    Width = 243
    Height = 38
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 26
      Top = 6
      Width = 91
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 126
      Top = 6
      Width = 91
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object edPassword: TEdit
    Left = 77
    Top = 47
    Width = 161
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    PasswordChar = '*'
    TabOrder = 1
  end
  object edUserName: TEdit
    Left = 77
    Top = 15
    Width = 161
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    CharCase = ecUpperCase
    TabOrder = 0
  end
end
