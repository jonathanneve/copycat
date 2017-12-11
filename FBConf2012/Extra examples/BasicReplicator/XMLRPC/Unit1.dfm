object fmMain: TfmMain
  Left = 218
  Top = 179
  Width = 987
  Height = 620
  Caption = 'CopyCat basic replicator example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 545
    Width = 979
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      979
      41)
    object btReactiver: TBitBtn
      Left = 160
      Top = 8
      Width = 153
      Height = 25
      Hint = 'T'#233'l'#233'charger la nouvelle version de Neptune'
      Caption = 'Activate auto-replication'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btReactiverClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000666666886666666666666666666666666666000066666CC8866666666666
        666888666666666600006666C22C88666666666666888886666666660000666A
        2222C886666666666888888866666666000066A222222C886666666668888888
        86666666000066A222A222C8866666666888688888666666000066A22C8A222C
        866666666888868888866666000066A22C86A22C886666666888866888866666
        0000666A2C666A22C8866666668866668888666600006666AA6666A22C886666
        6666666668888666000066666666666A22C88666666666666688886600006666
        66666666A22C8666666666666668888600006666666666666A2C866666666666
        66668886000066666666666666AC666666666666666668660000666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
    end
    object btDesactiver: TBitBtn
      Left = 320
      Top = 8
      Width = 161
      Height = 25
      Hint = 'T'#233'l'#233'charger la nouvelle version de Neptune'
      Caption = 'Deactivate auto-replication'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btDesactiverClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
        33333337777FF377FF3333993370739993333377FF373F377FF3399993000339
        993337777F777F3377F3393999707333993337F77737333337FF993399933333
        399377F3777FF333377F993339903333399377F33737FF33377F993333707333
        399377F333377FF3377F993333101933399377F333777FFF377F993333000993
        399377FF3377737FF7733993330009993933373FF3777377F7F3399933000399
        99333773FF777F777733339993707339933333773FF7FFF77333333999999999
        3333333777333777333333333999993333333333377777333333}
      NumGlyphs = 2
    end
    object btFermer: TBitBtn
      Left = 877
      Top = 8
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      TabOrder = 4
      OnClick = btFermerClick
      Kind = bkCancel
    end
    object btReplicate: TBitBtn
      Left = 8
      Top = 8
      Width = 145
      Height = 25
      Caption = 'Replicate now'
      TabOrder = 0
      OnClick = btReplicateClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FF044906055B09066C0C066C0C055E0A044C06FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF05600905600908911309B01809
        B31A09B31909B11907961405680C05680CFF00FFFF00FFFF00FFFF00FFFF00FF
        0A6A150A7F150BB61C09B91A08B41807B21609B31909B41909B81A09B91A0783
        10044D06FF00FFFF00FFFF00FF0B6A150F852216BD3411B7270BB21C07B11608
        B11709B21909B21909B21909B41909BA1A07841006670CFF00FFFF00FF0B6A15
        20BE491BBD4014B7300AB21F28BC36DFF5E1EEFAEF63CE6D09B21909B21909B3
        1909BA1A06670CFF00FF0872101B9A3A2AC65B1DBB450EB4250BB31B11B4219A
        DFA0FFFFFFF7FDF85ACB6509B21909B21909B81A089413045D090872102AB65B
        2CC56522BD4D0FB4220AB21A0CB31C0AB2198DDB95FDFEFDF6FCF758CB6309B2
        1909B51A08AB17045D090F821C37C26C33C76CCDF1DAC9EFD3C7EED0C8EFD2C5
        EED0C7EECFF8FDF9FFFFFFF2FBF36FD27908B41909B31905650B138D2358CC83
        42C977FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFEFDFFFFFFFFFFFFBCEA
        C10AB41A09B319066D0D0F911D6FD2935FD38D6DD49572D69971D69872D69964
        D28C92DFA8FBFEFBFFFFFFACE5B82EBF4C11B82B08B11905610A0F911D67CC83
        9BE5BA38C67030C36938C56F38C56F70D697E8F8EEFFFFFF9FE2B120BD481AB9
        3E10BA2908A31705610AFF00FF25AE39BCEDD282DBA428C0632FC26753CD82F7
        FDF9FFFFFF9CE2B222BC4B1DBA4118B73614C0300A8517FF00FFFF00FF25AE39
        71D28CD2F4E180DAA336C46D39C56FBCECCEABE6C22DC26324BE5623BC4D1FC1
        4616AE340A8517FF00FFFF00FFFF00FF25AE3984D89FDBF7EAAFE8C66BD49352
        CC8144C97849CA7B48CB7839CB6A21B6490F7C1FFF00FFFF00FFFF00FFFF00FF
        FF00FF25AE3925AE39ADE8C5CCF2DEBAEDD1A6E7C291E2B364D4922FB1572FB1
        57FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF32B74E25AE3925
        AE3925AE3925AE3924A342FF00FFFF00FFFF00FFFF00FFFF00FF}
    end
    object btArreter: TBitBtn
      Left = 488
      Top = 8
      Width = 129
      Height = 25
      Hint = 'Arr'#234'ter la r'#233'plication en cours'
      Caption = 'Stop replication'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btArreterClick
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777770000007777700000007777770000007777011111110777770000007770
        1111111110777700000077011111111111077700000070111111111111107700
        000001FF71F17F71F111070000000111F1F1F1F1F111070000000111F1F1F1F1
        FF7107000000017F71F1F1F1F1F10700000001F111F1F1F1F1F10700000001F1
        11F1F1F1F1F107000000017FFFFF7F71FF710700000070111111111111107700
        0000770111111111110777000000777011111111107777000000777701111111
        077777000000777770000000777777000000}
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 979
    Height = 236
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object lbReplicationEnCours: TLabel
      Left = 1
      Top = 1
      Width = 977
      Height = 24
      Align = alTop
      Alignment = taCenter
      Caption = 'Replication in progress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 3816154
      Font.Height = -21
      Font.Name = 'Albany'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object GroupBox1: TGroupBox
      Left = 4
      Top = 25
      Width = 484
      Height = 194
      Caption = 'Local database'
      TabOrder = 0
      object LocalNodeNameLabel: TLabel
        Left = 25
        Top = 23
        Width = 56
        Height = 13
        Caption = 'Local node:'
      end
      object Label1: TLabel
        Left = 24
        Top = 66
        Width = 81
        Height = 13
        Caption = 'Database name :'
      end
      object SpeedButton1: TSpeedButton
        Left = 456
        Top = 63
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = SpeedButton1Click
      end
      object Label2: TLabel
        Left = 24
        Top = 88
        Width = 57
        Height = 13
        Caption = 'User name :'
      end
      object Label3: TLabel
        Left = 302
        Top = 88
        Width = 52
        Height = 13
        Caption = 'Password :'
      end
      object Label4: TLabel
        Left = 24
        Top = 110
        Width = 42
        Height = 13
        Caption = 'Charset :'
      end
      object Label5: TLabel
        Left = 302
        Top = 110
        Width = 39
        Height = 13
        Caption = 'Dialect :'
      end
      object Label9: TLabel
        Left = 24
        Top = 132
        Width = 66
        Height = 13
        Caption = 'Library name :'
      end
      object SpeedButton2: TSpeedButton
        Left = 456
        Top = 129
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = SpeedButton2Click
      end
      object Label10: TLabel
        Left = 24
        Top = 47
        Width = 41
        Height = 13
        Caption = 'Version :'
      end
      object Label6: TLabel
        Left = 281
        Top = 23
        Width = 52
        Height = 13
        Caption = 'Password :'
      end
      object LocalNodeNameEdit: TEdit
        Left = 115
        Top = 19
        Width = 134
        Height = 21
        CharCase = ecUpperCase
        TabOrder = 0
      end
      object edDBName: TEdit
        Left = 115
        Top = 63
        Width = 340
        Height = 21
        TabOrder = 1
      end
      object edUserName: TEdit
        Left = 115
        Top = 85
        Width = 134
        Height = 21
        CharCase = ecUpperCase
        TabOrder = 2
      end
      object edPassword: TEdit
        Left = 361
        Top = 85
        Width = 118
        Height = 21
        PasswordChar = '*'
        TabOrder = 3
      end
      object cbDialect: TComboBox
        Left = 361
        Top = 106
        Width = 59
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          '1'
          '2'
          '3')
      end
      object edCharset: TEdit
        Left = 115
        Top = 107
        Width = 134
        Height = 21
        TabOrder = 5
      end
      object edClientDLL: TEdit
        Left = 115
        Top = 129
        Width = 340
        Height = 21
        TabOrder = 6
      end
      object cbVersions: TComboBox
        Left = 115
        Top = 41
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 7
      end
      object edLocalPassword: TEdit
        Left = 339
        Top = 19
        Width = 134
        Height = 21
        TabOrder = 8
      end
    end
    object GroupBox2: TGroupBox
      Left = 490
      Top = 25
      Width = 484
      Height = 194
      Caption = 'Remote database'
      TabOrder = 1
      object RemoteNodeNameLabel: TLabel
        Left = 21
        Top = 23
        Width = 67
        Height = 13
        Caption = 'Remote node:'
      end
      object RemoteNodeNameEdit: TEdit
        Left = 120
        Top = 19
        Width = 134
        Height = 21
        CharCase = ecUpperCase
        TabOrder = 0
      end
      inline frRemoteParams: TfrConnectParams
        Left = 5
        Top = 43
        Width = 472
        Height = 145
        TabOrder = 1
      end
    end
  end
  object edLog: TMemo
    Left = 0
    Top = 236
    Width = 979
    Height = 292
    Align = alClient
    Color = clInfoBk
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 528
    Width = 979
    Height = 17
    Align = alBottom
    TabOrder = 2
  end
  object Replicator: TCcReplicator
    Version = '2.03.0 beta 2'
    LogErrors = False
    HarmonizeFields = True
    KeepConnection = True
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = True
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctRetaining
    AbortOnError = False
    OnFinished = ReplicatorFinished
    OnRowBeforeReplicate = ReplicatorRowBeforeReplicate
    OnConflict = ReplicatorConflict
    OnReplicationError = ReplicatorReplicationError
    OnException = ReplicatorException
    OnAbort = ReplicatorAbort
    OnEmptyLog = ReplicatorEmptyLog
    OnTableBegin = ReplicatorTableBegin
    OnGenReplError = ReplicatorGenReplError
    OnKeySynchronized = ReplicatorKeySynchronized
    OnAutoCommit = ReplicatorAutoCommit
    OnProgress = ReplicatorProgress
    OnReplicateProc = ReplicatorReplicateProc
    OnQueryDone = ReplicatorQueryDone
    OnLogLoaded = ReplicatorLogLoaded
    BeforeReplicate = ReplicatorBeforeReplicate
    OnResolveConflict = ReplicatorResolveConflict
    Left = 32
  end
  object OpenDialogDLL: TOpenDialog
    Filter = 'Windows library|*.dll|Any file|*.*'
    Left = 432
    Top = 64
  end
  object OpenDialog: TOpenDialog
    Filter = 'Interbase databases|*.gdb|FireBird databases|*.fdb|Any file|*.*'
    Left = 424
    Top = 96
  end
  object Connection: TCcConnectionFIB
    DBType = 'Interbase'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 264
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Windows library|*.dll|Any file|*.*'
    Left = 432
    Top = 64
  end
  object OpenDialog2: TOpenDialog
    Filter = 'Interbase databases|*.gdb|FireBird databases|*.fdb|Any file|*.*'
    Left = 424
    Top = 96
  end
  object ConnectionFIB: TCcConnectionFIB
    DBType = 'Interbase'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 264
    Top = 16
  end
end
