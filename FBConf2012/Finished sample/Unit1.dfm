object Form1: TForm1
  Left = 247
  Top = 105
  Width = 921
  Height = 541
  Caption = 'CopyCat Head Office / Branch sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    905
    503)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 386
    Width = 889
    Height = 109
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 891
    Height = 372
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Head Office'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 883
        Height = 338
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 0
        DesignSize = (
          883
          338)
        object Splitter1: TSplitter
          Left = 457
          Top = 1
          Height = 336
        end
        object Panel7: TPanel
          Left = 1
          Top = 1
          Width = 456
          Height = 336
          Align = alLeft
          Caption = 'CUSTOMERS'
          Color = 14480832
          ParentBackground = False
          TabOrder = 0
          object DBGrid1: TDBGrid
            Left = 1
            Top = 25
            Width = 454
            Height = 310
            Align = alClient
            DataSource = qHeadCustDS
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -16
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
            Columns = <
              item
                Expanded = False
                FieldName = 'BRANCH'
                PickList.Strings = (
                  'BRANCHA'
                  'BRANCHB')
                Width = 79
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 163
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'SYNC_DATE'
                Visible = True
              end>
          end
          object Panel15: TPanel
            Left = 1
            Top = 1
            Width = 454
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            Caption = 'CUSTOMERS'
            Color = 13498545
            TabOrder = 1
          end
        end
        object Panel8: TPanel
          Left = 460
          Top = 1
          Width = 422
          Height = 336
          Align = alClient
          Caption = 'PRODUCTS'
          Color = 10218705
          ParentBackground = False
          TabOrder = 1
          object DBGrid7: TDBGrid
            Left = 1
            Top = 25
            Width = 420
            Height = 310
            Align = alClient
            DataSource = qHeadProdDS
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -16
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
            Columns = <
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 195
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'PRICE'
                Visible = True
              end>
          end
          object Panel13: TPanel
            Left = 1
            Top = 1
            Width = 420
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            Caption = 'PRODUCTS'
            Color = 11794402
            TabOrder = 1
          end
        end
        object btn1: TButton
          Left = 8
          Top = 307
          Width = 225
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Configure head office DB'
          TabOrder = 2
          OnClick = btn1Click
        end
      end
    end
    object Bra: TTabSheet
      Caption = 'Branch A'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 883
        Height = 338
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 0
        DesignSize = (
          883
          338)
        object Splitter2: TSplitter
          Left = 457
          Top = 1
          Height = 336
        end
        object Panel3: TPanel
          Left = 1
          Top = 1
          Width = 456
          Height = 336
          Align = alLeft
          Caption = 'CUSTOMERS'
          Color = 14480832
          ParentBackground = False
          TabOrder = 0
          object DBGrid2: TDBGrid
            Left = 1
            Top = 25
            Width = 454
            Height = 310
            Align = alClient
            DataSource = qBranchACustDS
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -16
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
            Columns = <
              item
                Expanded = False
                FieldName = 'BRANCH'
                PickList.Strings = (
                  'BRANCHA'
                  'BRANCHB')
                Width = 79
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 163
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'SYNC_DATE'
                Visible = True
              end>
          end
          object Panel14: TPanel
            Left = 1
            Top = 1
            Width = 454
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            Caption = 'CUSTOMERS'
            Color = 13498545
            TabOrder = 1
          end
        end
        object Panel4: TPanel
          Left = 460
          Top = 1
          Width = 422
          Height = 336
          Align = alClient
          Caption = 'PRODUCTS'
          Color = 10218705
          ParentBackground = False
          TabOrder = 1
          object DBGrid3: TDBGrid
            Left = 1
            Top = 25
            Width = 420
            Height = 310
            Align = alClient
            DataSource = qBranchAProdDS
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -16
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
            Columns = <
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 195
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'PRICE'
                Visible = True
              end>
          end
          object Panel12: TPanel
            Left = 1
            Top = 1
            Width = 420
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            Caption = 'PRODUCTS'
            Color = 11794402
            TabOrder = 1
          end
        end
        object btn3: TButton
          Left = 2
          Top = 308
          Width = 201
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Configure branch A'
          TabOrder = 2
          OnClick = btn3Click
        end
        object btn2: TButton
          Left = 209
          Top = 308
          Width = 210
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Replicate to branch A'
          TabOrder = 3
          OnClick = btn2Click
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Branch B'
      ImageIndex = 2
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 883
        Height = 338
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 0
        DesignSize = (
          883
          338)
        object Splitter3: TSplitter
          Left = 457
          Top = 1
          Height = 336
        end
        object Panel6: TPanel
          Left = 1
          Top = 1
          Width = 456
          Height = 336
          Align = alLeft
          Caption = 'CUSTOMERS'
          Color = 14480832
          ParentBackground = False
          TabOrder = 0
          object DBGrid4: TDBGrid
            Left = 1
            Top = 25
            Width = 454
            Height = 310
            Align = alClient
            DataSource = qBranchBCustDS
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -16
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
            Columns = <
              item
                Expanded = False
                FieldName = 'BRANCH'
                PickList.Strings = (
                  'BRANCHA'
                  'BRANCHB')
                Width = 79
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 163
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'SYNC_DATE'
                Visible = True
              end>
          end
          object Panel10: TPanel
            Left = 1
            Top = 1
            Width = 454
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            Caption = 'CUSTOMERS'
            Color = 13498545
            TabOrder = 1
          end
        end
        object Panel9: TPanel
          Left = 460
          Top = 1
          Width = 422
          Height = 336
          Align = alClient
          Caption = 'PRODUCTS'
          Color = 10218705
          ParentBackground = False
          TabOrder = 1
          object DBGrid5: TDBGrid
            Left = 1
            Top = 25
            Width = 420
            Height = 310
            Align = alClient
            DataSource = qBranchBProdDS
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -16
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
            Columns = <
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 195
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'PRICE'
                Visible = True
              end>
          end
          object Panel11: TPanel
            Left = 1
            Top = 1
            Width = 420
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            Caption = 'PRODUCTS'
            Color = 11794402
            TabOrder = 1
          end
        end
        object btn4: TButton
          Left = 2
          Top = 307
          Width = 210
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Configure branch B'
          TabOrder = 2
          OnClick = btn4Click
        end
        object btn5: TButton
          Left = 218
          Top = 307
          Width = 224
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Replicate to branch B'
          TabOrder = 3
          OnClick = btn5Click
        end
      end
    end
  end
  object qHeadCustDS: TDataSource
    DataSet = qHeadCust
    Left = 40
    Top = 208
  end
  object qHeadProdDS: TDataSource
    DataSet = qHeadProd
    Left = 472
    Top = 224
  end
  object qBranchACustDS: TDataSource
    DataSet = qBranchACust
    Left = 120
    Top = 208
  end
  object qBranchAProdDS: TDataSource
    DataSet = qBranchAProd
    Left = 560
    Top = 224
  end
  object qBranchBCustDS: TDataSource
    DataSet = qBranchBCust
    Left = 208
    Top = 208
  end
  object qBranchBProdDS: TDataSource
    DataSet = qBranchBProd
    Left = 648
    Top = 224
  end
  object HeadDB: TCcConnectionFIB
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'C:\Projects\CopyCat\FBConf2012\Finished sample\head_office.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 392
    Top = 336
  end
  object BranchADB: TCcConnectionFIB
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'C:\Projects\CopyCat\FBConf2012\Finished sample\branch a.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 424
    Top = 336
  end
  object BranchBDB: TCcConnectionFIB
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'C:\Projects\CopyCat\FBConf2012\Finished sample\branch b.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    ClientDLL = 'gds32.dll'
    Left = 456
    Top = 336
  end
  object FIBHeadDB: TpFIBDatabase
    DBName = 'C:\Projects\CopyCat\FBConf2012\Finished sample\head_office.fdb'
    DBParams.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    DefaultTransaction = FIBHeadTR
    DefaultUpdateTransaction = FIBHeadTR
    SQLDialect = 3
    Timeout = 0
    WaitForRestoreConnect = 0
    Left = 21
    Top = 87
  end
  object FIBHeadTR: TpFIBTransaction
    DefaultDatabase = FIBHeadDB
    TimeoutAction = TARollback
    AfterSQLExecute = FIBHeadTRAfterSQLExecute
    Left = 53
    Top = 87
  end
  object qHeadCust: TpFIBDataSet
    UpdateSQL.Strings = (
      'UPDATE CUSTOMERS'
      'SET '
      '    ID = :ID,'
      '    BRANCH = :BRANCH,'
      '    NAME = :NAME,'
      '    SYNC_DATE = :SYNC_DATE'
      'WHERE'
      '    ID = :OLD_ID'
      '    and BRANCH = :OLD_BRANCH'
      '    ')
    DeleteSQL.Strings = (
      'DELETE FROM'
      '    CUSTOMERS'
      'WHERE'
      '        ID = :OLD_ID'
      '    and BRANCH = :OLD_BRANCH'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO CUSTOMERS('
      '    ID,'
      '    BRANCH,'
      '    NAME,'
      '    SYNC_DATE'
      ')'
      'VALUES('
      '    :ID,'
      '    :BRANCH,'
      '    :NAME,'
      '    :SYNC_DATE'
      ')'
      'RETURNING ID, BRANCH')
    RefreshSQL.Strings = (
      'select * from customers'
      ''
      ' WHERE '
      '        CUSTOMERS.ID = :OLD_ID'
      '    and CUSTOMERS.BRANCH = :OLD_BRANCH'
      '    ')
    SelectSQL.Strings = (
      'select * from customers')
    Transaction = FIBHeadTR
    Database = FIBHeadDB
    DefaultFormats.NumericDisplayFormat = '########0.00'
    DefaultFormats.DisplayFormatDate = 'dd/mm/yyyy'
    DefaultFormats.DisplayFormatTime = 'hh:mm:ss'
    Left = 37
    Top = 175
    object qHeadCustID: TFIBIntegerField
      FieldName = 'ID'
    end
    object qHeadCustBRANCH: TFIBStringField
      FieldName = 'BRANCH'
      Size = 10
      EmptyStrToNull = True
    end
    object qHeadCustNAME: TFIBStringField
      FieldName = 'NAME'
      Size = 50
      EmptyStrToNull = True
    end
    object qHeadCustSYNC_DATE: TFIBDateTimeField
      FieldName = 'SYNC_DATE'
    end
  end
  object qHeadProd: TpFIBDataSet
    UpdateSQL.Strings = (
      'UPDATE PRODUCTS'
      'SET '
      '    ID = :ID,'
      '    NAME = :NAME,'
      '    PRICE = :PRICE'
      'WHERE'
      '    ID = :OLD_ID'
      '    ')
    DeleteSQL.Strings = (
      'DELETE FROM'
      '    PRODUCTS'
      'WHERE'
      '        ID = :OLD_ID'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO PRODUCTS('
      '    ID,'
      '    NAME,'
      '    PRICE'
      ')'
      'VALUES('
      '    :ID,'
      '    :NAME,'
      '    :PRICE'
      ')'
      'RETURNING ID')
    RefreshSQL.Strings = (
      'select *'
      'from products'
      ''
      ''
      ' WHERE '
      '        PRODUCTS.ID = :OLD_ID'
      '    ')
    SelectSQL.Strings = (
      'select *'
      'from products'
      '')
    Transaction = FIBHeadTR
    Database = FIBHeadDB
    DefaultFormats.NumericDisplayFormat = '########0.00'
    DefaultFormats.DisplayFormatDate = 'dd/mm/yyyy'
    DefaultFormats.DisplayFormatTime = 'hh:mm:ss'
    Left = 469
    Top = 183
  end
  object FIBBranchADB: TpFIBDatabase
    DBName = 'C:\Projects\CopyCat\FBConf2012\Finished sample\branch a.fdb'
    DBParams.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    DefaultTransaction = FIBBranchATR
    DefaultUpdateTransaction = FIBBranchATR
    SQLDialect = 3
    Timeout = 0
    WaitForRestoreConnect = 0
    Left = 109
    Top = 87
  end
  object FIBBranchATR: TpFIBTransaction
    DefaultDatabase = FIBBranchADB
    TimeoutAction = TARollback
    AfterSQLExecute = FIBHeadTRAfterSQLExecute
    Left = 141
    Top = 87
  end
  object FIBBranchBTR: TpFIBTransaction
    DefaultDatabase = FIBBranchBDB
    TimeoutAction = TARollback
    AfterSQLExecute = FIBHeadTRAfterSQLExecute
    Left = 221
    Top = 87
  end
  object FIBBranchBDB: TpFIBDatabase
    DBName = 'C:\Projects\CopyCat\FBConf2012\Finished sample\branch b.fdb'
    DBParams.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    DefaultTransaction = FIBBranchBTR
    DefaultUpdateTransaction = FIBBranchBTR
    SQLDialect = 3
    Timeout = 0
    WaitForRestoreConnect = 0
    Left = 189
    Top = 87
  end
  object qBranchACust: TpFIBDataSet
    UpdateSQL.Strings = (
      'UPDATE CUSTOMERS'
      'SET '
      '    ID = :ID,'
      '    BRANCH = :BRANCH,'
      '    NAME = :NAME,'
      '    SYNC_DATE = :SYNC_DATE'
      'WHERE'
      '    ID = :OLD_ID'
      '    and BRANCH = :OLD_BRANCH'
      '    ')
    DeleteSQL.Strings = (
      'DELETE FROM'
      '    CUSTOMERS'
      'WHERE'
      '        ID = :OLD_ID'
      '    and BRANCH = :OLD_BRANCH'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO CUSTOMERS('
      '    ID,'
      '    BRANCH,'
      '    NAME,'
      '    SYNC_DATE'
      ')'
      'VALUES('
      '    :ID,'
      '    :BRANCH,'
      '    :NAME,'
      '    :SYNC_DATE'
      ')'
      'RETURNING ID, BRANCH')
    RefreshSQL.Strings = (
      'select * from customers'
      ''
      ' WHERE '
      '        CUSTOMERS.ID = :OLD_ID'
      '    and CUSTOMERS.BRANCH = :OLD_BRANCH'
      '    ')
    SelectSQL.Strings = (
      'select * from customers')
    Transaction = FIBBranchATR
    Database = FIBBranchADB
    DefaultFormats.NumericDisplayFormat = '########0.00'
    DefaultFormats.DisplayFormatDate = 'dd/mm/yyyy'
    DefaultFormats.DisplayFormatTime = 'hh:mm:ss'
    Left = 117
    Top = 175
    object FIBIntegerField1: TFIBIntegerField
      FieldName = 'ID'
    end
    object FIBStringField1: TFIBStringField
      FieldName = 'BRANCH'
      Size = 10
      EmptyStrToNull = True
    end
    object FIBStringField2: TFIBStringField
      FieldName = 'NAME'
      Size = 50
      EmptyStrToNull = True
    end
    object qBranchACustSYNC_DATE: TFIBDateTimeField
      FieldName = 'SYNC_DATE'
    end
  end
  object qBranchBCust: TpFIBDataSet
    UpdateSQL.Strings = (
      'UPDATE CUSTOMERS'
      'SET '
      '    ID = :ID,'
      '    BRANCH = :BRANCH,'
      '    NAME = :NAME,'
      '    SYNC_DATE = :SYNC_DATE'
      'WHERE'
      '    ID = :OLD_ID'
      '    and BRANCH = :OLD_BRANCH'
      '    ')
    DeleteSQL.Strings = (
      'DELETE FROM'
      '    CUSTOMERS'
      'WHERE'
      '        ID = :OLD_ID'
      '    and BRANCH = :OLD_BRANCH'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO CUSTOMERS('
      '    ID,'
      '    BRANCH,'
      '    NAME,'
      '    SYNC_DATE'
      ')'
      'VALUES('
      '    :ID,'
      '    :BRANCH,'
      '    :NAME,'
      '    :SYNC_DATE'
      ')'
      'RETURNING ID, BRANCH')
    RefreshSQL.Strings = (
      'select'
      #9'CUSTOMERS.ID,'
      #9'CUSTOMERS.BRANCH,'
      #9'CUSTOMERS.NAME,'
      #9'CUSTOMERS.SYNC_DATE'
      'from customers'
      ''
      ' WHERE '
      '        CUSTOMERS.ID = :OLD_ID'
      '    and CUSTOMERS.BRANCH = :OLD_BRANCH'
      '    ')
    SelectSQL.Strings = (
      'select'
      #9'*'
      'from customers')
    Transaction = FIBBranchBTR
    Database = FIBBranchBDB
    DefaultFormats.NumericDisplayFormat = '########0.00'
    DefaultFormats.DisplayFormatDate = 'dd/mm/yyyy'
    DefaultFormats.DisplayFormatTime = 'hh:mm:ss'
    Left = 205
    Top = 167
    object FIBIntegerField2: TFIBIntegerField
      FieldName = 'ID'
    end
    object FIBStringField3: TFIBStringField
      FieldName = 'BRANCH'
      Size = 10
      EmptyStrToNull = True
    end
    object FIBStringField4: TFIBStringField
      FieldName = 'NAME'
      Size = 50
      EmptyStrToNull = True
    end
    object qBranchBCustSYNC_DATE: TFIBDateTimeField
      FieldName = 'SYNC_DATE'
    end
  end
  object qBranchAProd: TpFIBDataSet
    UpdateSQL.Strings = (
      'UPDATE PRODUCTS'
      'SET '
      '    ID = :ID,'
      '    NAME = :NAME,'
      '    PRICE = :PRICE'
      'WHERE'
      '    ID = :OLD_ID'
      '    ')
    DeleteSQL.Strings = (
      'DELETE FROM'
      '    PRODUCTS'
      'WHERE'
      '        ID = :OLD_ID'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO PRODUCTS('
      '    ID,'
      '    NAME,'
      '    PRICE'
      ')'
      'VALUES('
      '    :ID,'
      '    :NAME,'
      '    :PRICE'
      ')'
      'RETURNING ID')
    RefreshSQL.Strings = (
      'select *'
      'from products'
      ''
      ''
      ' WHERE '
      '        PRODUCTS.ID = :OLD_ID'
      '    ')
    SelectSQL.Strings = (
      'select *'
      'from products'
      '')
    Transaction = FIBBranchATR
    Database = FIBBranchADB
    DefaultFormats.NumericDisplayFormat = '########0.00'
    DefaultFormats.DisplayFormatDate = 'dd/mm/yyyy'
    DefaultFormats.DisplayFormatTime = 'hh:mm:ss'
    Left = 557
    Top = 175
  end
  object qBranchBProd: TpFIBDataSet
    UpdateSQL.Strings = (
      'UPDATE PRODUCTS'
      'SET '
      '    ID = :ID,'
      '    NAME = :NAME,'
      '    PRICE = :PRICE'
      'WHERE'
      '    ID = :OLD_ID'
      '    ')
    DeleteSQL.Strings = (
      'DELETE FROM'
      '    PRODUCTS'
      'WHERE'
      '        ID = :OLD_ID'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO PRODUCTS('
      '    ID,'
      '    NAME,'
      '    PRICE'
      ')'
      'VALUES('
      '    :ID,'
      '    :NAME,'
      '    :PRICE'
      ')'
      'RETURNING ID')
    RefreshSQL.Strings = (
      'select *'
      'from products'
      ''
      ''
      ' WHERE '
      '        PRODUCTS.ID = :OLD_ID'
      '    ')
    SelectSQL.Strings = (
      'select *'
      'from products'
      '')
    Transaction = FIBBranchBTR
    Database = FIBBranchBDB
    DefaultFormats.NumericDisplayFormat = '########0.00'
    DefaultFormats.DisplayFormatDate = 'dd/mm/yyyy'
    DefaultFormats.DisplayFormatTime = 'hh:mm:ss'
    Left = 645
    Top = 175
  end
  object CcReplicatorA: TCcReplicator
    Version = '3.03.0'
    TrimCharFields = False
    AutoPriority = True
    LogErrors = False
    HarmonizeFields = False
    KeepConnection = False
    Nodes.LocalNode.Connection = BranchADB
    Nodes.LocalNode.Name = 'BRANCHA'
    Nodes.RemoteNode.Connection = HeadDB
    Nodes.RemoteNode.Name = 'HEAD'
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = False
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctCommit
    AbortOnError = False
    OnRowReplicating = CcReplicatorARowReplicating
    OnReplicationError = CcReplicatorAReplicationError
    OnException = CcReplicatorAException
    OnEmptyLog = CcReplicatorAEmptyLog
    Left = 96
    Top = 408
  end
  object CcReplicatorB: TCcReplicator
    Version = '3.03.0'
    TrimCharFields = False
    AutoPriority = True
    LogErrors = False
    HarmonizeFields = False
    KeepConnection = False
    Nodes.LocalNode.Connection = BranchBDB
    Nodes.LocalNode.Name = 'BRANCHB'
    Nodes.RemoteNode.Connection = HeadDB
    Nodes.RemoteNode.Name = 'HEAD'
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = False
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctCommit
    AbortOnError = False
    OnReplicationError = CcReplicatorAReplicationError
    OnException = CcReplicatorAException
    OnEmptyLog = CcReplicatorAEmptyLog
    Left = 136
    Top = 408
  end
  object qUpdateSyncDate: TCcQuery
    ParamCheck = True
    SQL.Strings = (
      
        'update customers set sync_date = :sync_date where id = :id and b' +
        'ranch = :branch')
    Left = 544
    Top = 400
  end
  object CcConfigHead: TCcConfig
    Version = '3.03.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'BRANCHA'
      'BRANCHB')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = HeadDB
    Tables = <
      item
        TableName = 'PRODUCTS'
      end
      item
        TableName = 'CUSTOMERS'
        Condition.Strings = (
          'new.branch = u.login')
      end>
    Left = 45
    Top = 295
  end
  object CcConfigA: TCcConfig
    Version = '3.03.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'HEAD')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = BranchADB
    Tables = <
      item
        TableName = 'CUSTOMERS'
      end>
    Left = 77
    Top = 295
  end
  object CcConfigB: TCcConfig
    Version = '3.03.0'
    ConfigName = 'TEST'
    Nodes.Strings = (
      'HEAD')
    DatabaseNode = dnLocal
    Terminator = #167
    Connection = BranchBDB
    Tables = <
      item
        TableName = 'CUSTOMERS'
      end>
    Left = 109
    Top = 295
  end
end
