object fmGenProc: TfmGenProc
  Left = 197
  Top = 79
  BorderStyle = bsDialog
  Caption = 'Field synchronization editor'
  ClientHeight = 430
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    516
    430)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 389
    Width = 516
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      516
      41)
    object btOK: TBitBtn
      Left = 342
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btOKClick
      Kind = bkOK
    end
    object btAnnuler: TBitBtn
      Left = 432
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 516
    Height = 29
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 1
    object Label4: TLabel
      Left = 5
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Field :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbPKName: TLabel
      Left = 35
      Top = 8
      Width = 214
      Height = 13
      AutoSize = False
      Caption = 'CODE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 250
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Table:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbTableName: TLabel
      Left = 288
      Top = 8
      Width = 217
      Height = 13
      AutoSize = False
      Caption = 'TABLE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object cbRien: TRadioButton
    Left = 10
    Top = 35
    Width = 145
    Height = 17
    Caption = 'No synchronization'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object GroupBox3: TGroupBox
    Left = 5
    Top = 133
    Width = 506
    Height = 247
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    OnEnter = GroupBox3Enter
    DesignSize = (
      506
      247)
    object Label1: TLabel
      Left = 16
      Top = 28
      Width = 117
      Height = 13
      Caption = 'Stored procedure name :'
    end
    object Label2: TLabel
      Left = 16
      Top = 52
      Width = 88
      Height = 13
      Caption = 'Output parameter :'
    end
    object Label3: TLabel
      Left = 16
      Top = 76
      Width = 85
      Height = 13
      Caption = 'Input parameters :'
    end
    object edProcedureName: TComboBox
      Left = 136
      Top = 25
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = edProcedureNameChange
    end
    object dbgParams: TDBGrid
      Left = 16
      Top = 92
      Width = 482
      Height = 141
      Anchors = [akLeft, akTop, akRight, akBottom]
      DataSource = InParamsDS
      Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'PARAM_NAME'
          ReadOnly = True
          Title.Caption = 'Parameter name'
          Width = 200
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'PARAM_VALUE'
          Title.Caption = 'Parameter value'
          Width = 230
          Visible = True
        end>
    end
    object edOutputParam: TComboBox
      Left = 136
      Top = 49
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 5
    Top = 61
    Width = 506
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnEnter = GroupBox2Enter
    object Label6: TLabel
      Left = 16
      Top = 28
      Width = 34
      Height = 13
      Caption = 'Name :'
    end
    object Label7: TLabel
      Left = 288
      Top = 28
      Width = 53
      Height = 13
      Caption = 'Increment :'
    end
    object edGeneratorName: TComboBox
      Left = 56
      Top = 25
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object edGenIncrement: TEdit
      Left = 352
      Top = 24
      Width = 105
      Height = 21
      TabOrder = 0
    end
  end
  object cbGen: TRadioButton
    Left = 10
    Top = 60
    Width = 127
    Height = 17
    Caption = 'Use a generator'
    TabOrder = 5
  end
  object cbProc: TRadioButton
    Left = 10
    Top = 131
    Width = 170
    Height = 17
    Caption = 'Use a stored procedure'
    TabOrder = 6
  end
  object InParamsDS: TDataSource
    DataSet = InParams
    Left = 71
    Top = 272
  end
  object InParams: TCcProcParams
    Left = 40
    Top = 272
  end
  object OutParams: TCcProcParams
    Left = 72
    Top = 240
  end
end
