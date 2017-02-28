object Form1: TForm1
  Left = 142
  Top = 266
  Caption = 'CopyCat Head Office / Branch sample'
  ClientHeight = 436
  ClientWidth = 710
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnShow = FormShow
  DesignSize = (
    710
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 319
    Width = 694
    Height = 109
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 696
    Height = 305
    ActivePage = TabSheet1
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
        Width = 688
        Height = 271
        Align = alClient
        Caption = 'Panel1'
        Padding.Bottom = 35
        TabOrder = 0
        DesignSize = (
          688
          271)
        object Splitter1: TSplitter
          Left = 314
          Top = 1
          Height = 234
          ExplicitLeft = 323
          ExplicitTop = -3
          ExplicitHeight = 208
        end
        object Panel7: TPanel
          Left = 1
          Top = 1
          Width = 313
          Height = 234
          Align = alLeft
          Caption = 'CUSTOMERS'
          Color = 14480832
          Enabled = False
          Padding.Top = 20
          ParentBackground = False
          TabOrder = 0
          VerticalAlignment = taAlignTop
          object DBGrid1: TDBGrid
            Left = 1
            Top = 21
            Width = 311
            Height = 212
            Align = alClient
            DataSource = qHeadCustDS
            GradientEndColor = 15711420
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
            Touch.ParentTabletOptions = False
            Touch.TabletOptions = [toPressAndHold, toTouchUIForceOn]
            Columns = <
              item
                Expanded = False
                FieldName = 'BRANCH'
                PickList.Strings = (
                  'BRANCH A'
                  'BRANCH B')
                Width = 79
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 163
                Visible = True
              end>
          end
        end
        object Panel8: TPanel
          Left = 317
          Top = 1
          Width = 370
          Height = 234
          Align = alClient
          Caption = 'PRODUCTS'
          Color = 10218705
          Padding.Top = 20
          ParentBackground = False
          TabOrder = 1
          VerticalAlignment = taAlignTop
          object DBGrid7: TDBGrid
            Left = 1
            Top = 21
            Width = 368
            Height = 212
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
        end
        object btn1: TButton
          Left = 8
          Top = 240
          Width = 225
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Configure head office DB'
          TabOrder = 2
        end
      end
    end
    object Bra: TTabSheet
      Caption = 'Branch A'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 688
        Height = 271
        Align = alClient
        Caption = 'Panel1'
        Padding.Bottom = 35
        TabOrder = 0
        DesignSize = (
          688
          271)
        object Splitter2: TSplitter
          Left = 314
          Top = 1
          Height = 234
          ExplicitLeft = 323
          ExplicitTop = -3
          ExplicitHeight = 208
        end
        object Panel3: TPanel
          Left = 1
          Top = 1
          Width = 313
          Height = 234
          Align = alLeft
          Caption = 'CUSTOMERS'
          Color = 14480832
          Padding.Top = 20
          ParentBackground = False
          TabOrder = 0
          VerticalAlignment = taAlignTop
          object DBGrid2: TDBGrid
            Left = 1
            Top = 21
            Width = 311
            Height = 212
            Align = alClient
            DataSource = qBranchACustDS
            GradientEndColor = 15711420
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
            Touch.ParentTabletOptions = False
            Touch.TabletOptions = [toPressAndHold, toTouchUIForceOn]
            Columns = <
              item
                Expanded = False
                FieldName = 'BRANCH'
                PickList.Strings = (
                  'BRANCH A'
                  'BRANCH B')
                Width = 79
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 163
                Visible = True
              end>
          end
        end
        object Panel4: TPanel
          Left = 317
          Top = 1
          Width = 370
          Height = 234
          Align = alClient
          Caption = 'PRODUCTS'
          Color = 10218705
          Padding.Top = 20
          ParentBackground = False
          TabOrder = 1
          VerticalAlignment = taAlignTop
          object DBGrid3: TDBGrid
            Left = 1
            Top = 21
            Width = 368
            Height = 212
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
        end
        object btn3: TButton
          Left = 2
          Top = 241
          Width = 201
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Configure branch A'
          TabOrder = 2
        end
        object btn2: TButton
          Left = 209
          Top = 241
          Width = 210
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Replicate to branch A'
          TabOrder = 3
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Branch B'
      ImageIndex = 2
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 688
        Height = 271
        Align = alClient
        Caption = 'Panel1'
        Padding.Bottom = 35
        TabOrder = 0
        DesignSize = (
          688
          271)
        object Splitter3: TSplitter
          Left = 314
          Top = 1
          Height = 234
          ExplicitLeft = 323
          ExplicitTop = -3
          ExplicitHeight = 208
        end
        object Panel6: TPanel
          Left = 1
          Top = 1
          Width = 313
          Height = 234
          Align = alLeft
          Caption = 'CUSTOMERS'
          Color = 14480832
          Padding.Top = 20
          ParentBackground = False
          TabOrder = 0
          VerticalAlignment = taAlignTop
          object DBGrid4: TDBGrid
            Left = 1
            Top = 21
            Width = 311
            Height = 212
            Align = alClient
            DataSource = qBranchBCustDS
            GradientEndColor = 15711420
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
            Touch.ParentTabletOptions = False
            Touch.TabletOptions = [toPressAndHold, toTouchUIForceOn]
            Columns = <
              item
                Expanded = False
                FieldName = 'BRANCH'
                PickList.Strings = (
                  'BRANCH A'
                  'BRANCH B')
                Width = 79
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'NAME'
                Width = 163
                Visible = True
              end>
          end
        end
        object Panel9: TPanel
          Left = 317
          Top = 1
          Width = 370
          Height = 234
          Align = alClient
          Caption = 'PRODUCTS'
          Color = 10218705
          Padding.Top = 20
          ParentBackground = False
          TabOrder = 1
          VerticalAlignment = taAlignTop
          object DBGrid5: TDBGrid
            Left = 1
            Top = 21
            Width = 368
            Height = 212
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
        end
        object btn4: TButton
          Left = 2
          Top = 240
          Width = 210
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Configure branch B'
          TabOrder = 2
        end
        object Button2: TButton
          Left = 218
          Top = 240
          Width = 224
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Replicate to branch B'
          TabOrder = 3
        end
      end
    end
  end
  object Button1: TButton
    Left = 621
    Top = 8
    Width = 81
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'OPEN ALL'
    TabOrder = 2
    OnClick = Button1Click
  end
  object HeadDB: TCcConnectionUIB
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'c:\copycat_demo\head_office.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Options = [tpNowait, tpWrite]
    LibraryName = 'fbclient.dll'
    Left = 368
    Top = 352
  end
  object qHeadCust: TCcDataSet
    AfterPost = qHeadCustAfterPost
    SQL.Strings = (
      'select *'
      'from customers')
    SQLDelete.Strings = (
      'delete from customers '
      'where id=:id')
    SQLInsert.Strings = (
      'insert into customers (branch, name) values (:branch, :name)')
    SQLRefresh.Strings = (
      'select *'
      'from customers'
      'where id = :id')
    SQLUpdate.Strings = (
      'update customers set branch = :branch, name= :name '
      'where id = :id')
    Connection = HeadDB
    Left = 40
    Top = 160
    object qHeadCustID: TIntegerField
      FieldName = 'ID'
    end
    object qHeadCustBRANCH: TStringField
      FieldName = 'BRANCH'
      Size = 10
    end
    object qHeadCustNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object qHeadCustDS: TDataSource
    DataSet = qHeadCust
    Left = 40
    Top = 208
  end
  object BranchADB: TCcConnectionUIB
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'c:\copycat_demo\branch a.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Options = [tpNowait, tpWrite]
    LibraryName = 'fbclient.dll'
    Left = 424
    Top = 352
  end
  object BranchBDB: TCcConnectionUIB
    UserLogin = 'SYSDBA'
    UserPassword = 'masterkey'
    SQLDialect = 3
    DBType = 'Interbase'
    DBVersion = 'FB2.5'
    DBName = 'c:\copycat_demo\branch b.fdb'
    TRParams.Strings = (
      'write'
      'nowait'
      'concurrency')
    Options = [tpNowait, tpWrite]
    LibraryName = 'gds32.dll'
    Left = 488
    Top = 352
  end
  object qHeadProd: TCcDataSet
    AfterPost = qHeadCustAfterPost
    SQL.Strings = (
      'select *'
      'from products'
      '')
    SQLDelete.Strings = (
      'delete from products'
      'where id=:id')
    SQLInsert.Strings = (
      'insert into products (name, price) values (:name, :price)')
    SQLRefresh.Strings = (
      'select *'
      'from products'
      'where id = :id')
    SQLUpdate.Strings = (
      'update products set name= :name , price=:price'
      'where id = :id')
    Connection = HeadDB
    Left = 472
    Top = 176
    object qHeadProdID: TIntegerField
      FieldName = 'ID'
    end
    object qHeadProdNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object qHeadProdPRICE: TFloatField
      FieldName = 'PRICE'
      currency = True
    end
  end
  object qHeadProdDS: TDataSource
    DataSet = qHeadProd
    Left = 472
    Top = 224
  end
  object qBranchACust: TCcDataSet
    AfterPost = qHeadCustAfterPost
    SQL.Strings = (
      'select *'
      'from customers')
    SQLDelete.Strings = (
      'delete from customers '
      'where id=:id')
    SQLInsert.Strings = (
      'insert into customers (branch, name) values (:branch, :name)')
    SQLRefresh.Strings = (
      'select *'
      'from customers'
      'where id = :id')
    SQLUpdate.Strings = (
      'update customers set branch = :branch, name= :name '
      'where id = :id')
    Connection = BranchADB
    Left = 120
    Top = 160
    object qBranchACustID: TIntegerField
      FieldName = 'ID'
    end
    object qBranchACustBRANCH: TStringField
      FieldName = 'BRANCH'
      Size = 10
    end
    object qBranchACustNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
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
  object qBranchAProd: TCcDataSet
    AfterPost = qHeadCustAfterPost
    SQL.Strings = (
      'select *'
      'from products'
      '')
    SQLDelete.Strings = (
      'delete from products'
      'where id=:id')
    SQLInsert.Strings = (
      'insert into products (name, price) values (:name, :price)')
    SQLRefresh.Strings = (
      'select *'
      'from products'
      'where id = :id')
    SQLUpdate.Strings = (
      'update products set name= :name , price=:price'
      'where id = :id')
    Connection = BranchADB
    Left = 560
    Top = 176
    object IntegerField2: TIntegerField
      FieldName = 'ID'
    end
    object StringField7: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object FloatField1: TFloatField
      FieldName = 'PRICE'
      currency = True
    end
  end
  object qBranchBCust: TCcDataSet
    AfterPost = qHeadCustAfterPost
    SQL.Strings = (
      'select *'
      'from customers')
    SQLDelete.Strings = (
      'delete from customers '
      'where id=:id')
    SQLInsert.Strings = (
      'insert into customers (branch, name) values (:branch, :name)')
    SQLRefresh.Strings = (
      'select *'
      'from customers'
      'where id = :id')
    SQLUpdate.Strings = (
      'update customers set branch = :branch, name= :name '
      'where id = :id')
    Connection = BranchBDB
    Left = 208
    Top = 160
    object IntegerField3: TIntegerField
      FieldName = 'ID'
    end
    object StringField8: TStringField
      FieldName = 'BRANCH'
      Size = 10
    end
    object StringField9: TStringField
      FieldName = 'NAME'
      Size = 50
    end
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
  object qBranchBProd: TCcDataSet
    AfterPost = qHeadCustAfterPost
    SQL.Strings = (
      'select *'
      'from products'
      '')
    SQLDelete.Strings = (
      'delete from products'
      'where id=:id')
    SQLInsert.Strings = (
      'insert into products (name, price) values (:name, :price)')
    SQLRefresh.Strings = (
      'select *'
      'from products'
      'where id = :id')
    SQLUpdate.Strings = (
      'update products set name= :name , price=:price'
      'where id = :id')
    Connection = BranchBDB
    Left = 648
    Top = 176
    object IntegerField4: TIntegerField
      FieldName = 'ID'
    end
    object StringField14: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object FloatField2: TFloatField
      FieldName = 'PRICE'
      currency = True
    end
  end
end
