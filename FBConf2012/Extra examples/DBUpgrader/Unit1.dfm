object Form1: TForm1
  Left = 372
  Top = 294
  Width = 544
  Height = 387
  Caption = 'CopyCat database upgrade utility'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 10
    Width = 521
    Height = 344
    ActivePage = tsTables
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object lbConnected: TLabel
        Left = 128
        Top = 280
        Width = 206
        Height = 13
        Caption = 'Connection established successfully'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Visible = False
      end
      object Label6: TLabel
        Left = 8
        Top = 10
        Width = 435
        Height = 26
        Caption = 
          'Please enter below the connection parameters for the database th' +
          'at you want to upgrade to the new database structure (CopyCat ve' +
          'rsion 3.00.0). '
        WordWrap = True
      end
      object Label8: TLabel
        Left = 9
        Top = 48
        Width = 427
        Height = 39
        Caption = 
          'Please note that the existing databases are assumed to have the ' +
          'format of version 2.02.1, so please upgrade to 2.02.1 first if y' +
          'ou haven'#39't already done so.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object GroupBox1: TGroupBox
        Left = 13
        Top = 94
        Width = 484
        Height = 174
        Caption = 'Connection parameters'
        TabOrder = 0
        inline frConnectParams: TfrConnectParams
          Left = 2
          Top = 19
          Width = 477
          Height = 152
          TabOrder = 0
        end
      end
      object BitBtn1: TBitBtn
        Left = 18
        Top = 276
        Width = 94
        Height = 25
        Caption = 'Connect'
        Default = True
        TabOrder = 1
        OnClick = BitBtn1Click
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFAB
          B1B48885835258650C547B617884B3B6B9A3AFBDABB8C4FF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFADB2B580807EA7A4A8007AAF67EAFE54A5BC505C
          6C057EA73892ABFF00FFFF00FFFF00FFBCACAEAD8D8EB28B8BD8CBCB6D6D6DF2
          E9E50084B951F3FF43CAEA7085950090C585FFFF4EC5E3FF00FFFF00FFBEA4A5
          EABFBFF6CFCFEDCFCFAEA7A7686868F3EAE90ECAF837D2F5E2D5D10F93C258FF
          FF2FABC8C2CDD2FF00FFFF00FFC79E9FFFEFEFE9CDCDECD7D7A39F9F919191A9
          A6A5EAE7E6E9E1E1E2DCDB35BCE062C6E0988887FF00FFFF00FFFF00FFC6A2A2
          F7DADAEBBEBEECCFCFA49E9EDEDEDEBCBBBBA2A1A1E1E0E0E4E3E3DEDBDA8F8C
          8CAFB5B8FF00FFFF00FFFF00FFC99C9CFFE3E3F6DEDEF6ECEC959494B1B1B1FF
          FFFFFFFFFFC4C4C48F8F8F78797AB9C0C3FF00FFFF00FFFF00FFFF00FFCAA4A4
          F9E1E1E0B6B6E2BABAD3C8C8C1BFBF9F9B9BB9B5B5B3ADADB6ADADC5CDD1FF00
          FFFF00FFFF00FFFF00FFFF00FFCB9E9EFFDEDEEEC0C0DCA8A8E2BABAEBD8D8DF
          C7C7D3B9B9BDA0A08D6969FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA7A7
          F9E8E8F0DADAEAD2D2E8CBCBDBB5B5CFA2A2BD8B8BAA7777764445FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFCAA0A0FFDEDEEDC0C0DBA6A6D8A5A5CA9696BA
          8686A87575875555714243FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD1A5A5
          FFDBDBF0CACAE3B7B7E1B4B4D2A3A3BE8F8FAE7E7E834F4F6D3E3EFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFCCA5A5FFFDFDFFF1F1FFE8E8FBE0E0F9DADAF7
          D5D5E9C4C4D7ACAC805252FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC6C0C3
          B8999BBF999AC09797BD9191B98C8CB18787AC8686A38384B6B1B4FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      end
    end
    object tsTables: TTabSheet
      Caption = 'Tables'
      ImageIndex = 1
      object dbgTables: TDBGrid
        Left = 0
        Top = 65
        Width = 513
        Height = 251
        Align = alClient
        DataSource = RPLTablesDS
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'TABLE_NAME'
            Title.Alignment = taCenter
            Title.Caption = 'Table name'
            Width = 350
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'PRIORITY'
            Title.Alignment = taCenter
            Title.Caption = 'Priority'
            Width = 125
            Visible = True
          end>
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 513
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label7: TLabel
          Left = 3
          Top = 4
          Width = 376
          Height = 26
          Caption = 
            'Below is the list of replicated tables with their configuration.' +
            ' Press GO to automatically import the existing configuration for' +
            ' each table into the new format.'
          WordWrap = True
        end
        object Label9: TLabel
          Left = 0
          Top = 48
          Width = 430
          Height = 13
          Caption = 
            'All external connections to the database must be closed before p' +
            'roceeding.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Button1: TButton
          Left = 416
          Top = 8
          Width = 75
          Height = 25
          Hint = 'Launch meta-data conversion.'
          Caption = 'Go !'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object tsSQLLog: TTabSheet
      Caption = 'SQL Log'
      ImageIndex = 2
      TabVisible = False
      object Label1: TLabel
        Left = 8
        Top = 14
        Width = 263
        Height = 13
        Caption = 'Log of SQL operations executed for database upgrade :'
      end
      object SQLLog: TMemo
        Left = 4
        Top = 40
        Width = 505
        Height = 272
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object RPLTablesDS: TDataSource
    DataSet = RPLTables
    Left = 80
    Top = 160
  end
  object CcConfig: TCcConfig
    Version = '3.01.0'
    DatabaseNode = dnLocal
    Terminator = ';'
    Connection = frConnectParams.Connection
    OnQueryReady = CcConfigQueryReady
    Left = 16
    Top = 160
  end
  object RPLTables: TCcDataSet
    SQL.Strings = (
      'select * from RPL$TABLES')
    SQLDelete.Strings = (
      'delete from RPL$TABLES'
      'where table_name = :table_name')
    SQLInsert.Strings = (
      'insert into RPL$TABLES'
      '(TABLE_NAME,'
      'REF_FIELD,'
      'PRIORITY,'
      'CREATED,'
      'UNIQUE_KEY_SYNC,'
      'UNIQUE_KEY_NAMES,'
      'TRIG_BASE_NAME,'
      'REF_TABLE,'
      'REF_TABLE_KEY,'
      'PRIMARY_KEY_SYNC,'
      'REPL_INSERTS,'
      'REPL_UPDATES,'
      'REPL_DELETES,'
      'CONDITION_FIELD,'
      'CONDITION'
      ')'
      'values'
      '(:TABLE_NAME,'
      ':REF_FIELD,'
      ':PRIORITY,'
      ':CREATED,'
      ':UNIQUE_KEY_SYNC,'
      ':UNIQUE_KEY_NAMES,'
      ':TRIG_BASE_NAME,'
      ':REF_TABLE,'
      ':REF_TABLE_KEY,'
      ':PRIMARY_KEY_SYNC,'
      ':REPL_INSERTS,'
      ':REPL_UPDATES,'
      ':REPL_DELETES,'
      ':CONDITION_FIELD,'
      ':CONDITION'
      ')')
    SQLRefresh.Strings = (
      'select * from RPL$TABLES'
      'where table_name = :table_name')
    SQLUpdate.Strings = (
      'update RPL$TABLES t'
      'set'
      'TABLE_NAME = :TABLE_NAME,'
      'REF_FIELD = :REF_FIELD,'
      'PRIORITY = :PRIORITY,'
      'CREATED = :CREATED,'
      'UNIQUE_KEY_SYNC = :UNIQUE_KEY_SYNC,'
      'UNIQUE_KEY_NAMES = :UNIQUE_KEY_NAMES,'
      'TRIG_BASE_NAME = :TRIG_BASE_NAME,'
      'REF_TABLE = :REF_TABLE,'
      'REF_TABLE_KEY = :REF_TABLE_KEY,'
      'PRIMARY_KEY_SYNC = :PRIMARY_KEY_SYNC,'
      'REPL_INSERTS = :REPL_INSERTS,'
      'REPL_UPDATES = :REPL_UPDATES,'
      'REPL_DELETES = :REPL_DELETES,'
      'CONDITION_FIELD = :CONDITION_FIELD,'
      't.CONDITION = :CONDITION'
      'where'
      '  TABLE_NAME = :OLD_TABLE_NAME'
      '')
    Connection = frConnectParams.Connection
    Left = 48
    Top = 160
    object RPLTablesTRIG_BASE_NAME: TStringField
      FieldName = 'TRIG_BASE_NAME'
      Size = 50
    end
    object RPLTablesTABLE_NAME: TStringField
      FieldName = 'TABLE_NAME'
      Required = True
      Size = 100
    end
    object RPLTablesCREATED: TStringField
      FieldName = 'CREATED'
      Size = 1
    end
    object RPLTablesPRIORITY: TIntegerField
      FieldName = 'PRIORITY'
    end
  end
  object RPLProcedures: TCcDataSet
    SQL.Strings = (
      'select * from RPL$Procedures')
    Connection = frConnectParams.Connection
    Left = 48
    Top = 192
    object RPLProceduresPROCEDURE_NAME: TStringField
      FieldName = 'PROCEDURE_NAME'
      Required = True
      Size = 50
    end
    object RPLProceduresNEW_PROCEDURE_NAME: TStringField
      FieldName = 'NEW_PROCEDURE_NAME'
      Required = True
      Size = 50
    end
    object RPLProceduresCREATED: TStringField
      FieldName = 'CREATED'
      Size = 1
    end
  end
  object RPLUsersDS: TDataSource
    DataSet = RPLProcedures
    Left = 80
    Top = 192
  end
end
