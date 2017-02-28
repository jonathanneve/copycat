object Form1: TForm1
  Left = 374
  Top = 332
  Caption = 'CopyCat Transport Server example'
  ClientHeight = 218
  ClientWidth = 512
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
  object PageControl: TPageControl
    Left = 7
    Top = 8
    Width = 497
    Height = 201
    ActivePage = tsDBConnection
    TabOrder = 0
    object tsDBConnection: TTabSheet
      Caption = 'Database connection'
      inline frConnectParams: TfrConnectParams
        Left = 0
        Top = 0
        Width = 489
        Height = 173
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 489
        ExplicitHeight = 173
        inherited Label1: TLabel
          Width = 81
          ExplicitWidth = 81
        end
        inherited Label2: TLabel
          Width = 57
          ExplicitWidth = 57
        end
        inherited Label3: TLabel
          Width = 52
          ExplicitWidth = 52
        end
        inherited Label4: TLabel
          Width = 42
          ExplicitWidth = 42
        end
        inherited Label10: TLabel
          Width = 41
          ExplicitWidth = 41
        end
      end
    end
    object tsServerOptions: TTabSheet
      Caption = 'Server options'
      ImageIndex = 1
      object lbConnected: TLabel
        Left = 122
        Top = 147
        Width = 154
        Height = 13
        Caption = 'Server started successfully'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Visible = False
      end
      inline frServerOptions: TfrServerOptions
        Left = 0
        Top = 0
        Width = 489
        Height = 145
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 489
        inherited Label1: TLabel
          Width = 82
          ExplicitWidth = 82
        end
        inherited Label3: TLabel
          Width = 122
          ExplicitWidth = 122
        end
        inherited GroupBox1: TGroupBox
          inherited Label5: TLabel
            Width = 76
            ExplicitWidth = 76
          end
          inherited Label6: TLabel
            Width = 86
            ExplicitWidth = 86
          end
          inherited Label7: TLabel
            Width = 64
            ExplicitWidth = 64
          end
          inherited Label8: TLabel
            Width = 66
            ExplicitWidth = 66
          end
          inherited ListenPortLabel: TLabel
            Width = 52
            ExplicitWidth = 52
          end
        end
        inherited ServerTransport: TCcRtcServerTransport
          RowBatchSize = 20
        end
      end
      object btConnect: TBitBtn
        Left = 12
        Top = 142
        Width = 94
        Height = 25
        Caption = 'Start server'
        Default = True
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
        TabOrder = 1
        OnClick = btConnectClick
      end
    end
  end
end
