object Form1: TForm1
  Left = 13
  Top = 59
  Width = 989
  Height = 728
  HelpContext = 1
  Caption = 'Microtec CopyCat demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 663
    Width = 981
    Height = 38
    Align = alBottom
    TabOrder = 2
    object btReactiver: TBitBtn
      Left = 126
      Top = 8
      Width = 153
      Height = 25
      Caption = 'Activate auto-replication'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      OnClick = btReactiverClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFC6CED2C2CACEB9C0C4B1B8BBACB3B7ACB3B7B1B8BBB9C1C5C3CBCFC6CE
        D2FF00FFFF00FFFF00FFFF00FFFF00FFC4CCD0BBC3C6A4ABAE868B8E6E727462
        66686266686F7476878C8FA5ACAFBBC3C6C5CDD1FF00FFFF00FFFF00FFC5CDD1
        B6BDC08F94976F707265686A686C6E64686C6266685E62634F525365696B9298
        9BB7BEC2C5CDD1FF00FFC6CED2BAC2C59393957B767A90949AB5ACA4C8B6A3C6
        B29EAAA19B7C8187686A6E56595A5C5F6192989BBBC3C6C6CED2C2CACEA1A2A4
        8E878ABCB7B1FED8ACFFE8B6FFE8BEFFE9BEFFE7B5FCD2A3A39D976D71745659
        5A65696BA5ACAFC3CBCFB6BCBF938688C7C1BAFFE4B1FFE4B8FFE1B5FFE0B4FF
        E0B6FFE6C3FFEDCCFFE1ADA69E97696C6F4F5253878C8FB9C1C5A0979AB2B2B8
        FFE0B1FFE8BCFFE4BBFFE5BDFFE9BEFFE6BBFFE0B5FFE1B9FFEECFFFD5A67D83
        885E62636E7375B1B8BB9D8A8DDCD2C6FFECBDFFE8BFFFEAC2FFF2C8EAD4AFE2
        CDA7FFEEC1FFE1B6FFE7C3FFEBBDB6A89C626569626668ACB3B7A69296EEDEC5
        FFF6DBFFEECBFFF3CAFFF8D04648503D4349F9E1B8FFEDC1FFE3B8FFECC7D3BC
        A366696D616567ACB3B7AC979AF4E5C9FFF8E0FFF8DDFFFFDDB3AB92383C432E
        30388B816DF8E0B8FFE8BBFFEDC7D8BFA7686B6E6D7173B1B8BBAC9395F5E9D9
        FFFBDFFFFFF1CFCBB201030BDFD7B6F7EAC60E1118726A5CFFEFC1FFEDC2C5B7
        A7656569868B8EB9C1C5B4A4A6E4E0E5FFF4CBE6E5D7000005C3C0ABFFFFE4FF
        FFD8F2E3BFA09480FFF3C6FFE4B395979B6E7173A4ABAEC3CBCFC5C8CBC1A5A8
        FFFFF4D7CDAFA2A49DFFFFF9FFFCE0FFF7D5FFF4CCFFF4C9FFEFBDCBC1B57C78
        7C8F9497BBC3C6C6CED2FF00FFC9BDC0D3BDC0FFFFF8FFFBD2FFFFE6FFFDE3FF
        F9DEFFF7D0FFE8B8D5CABF8F898C919294B6BEC1C5CDD1FF00FFFF00FFFF00FF
        C9BDC0C4A6A9EAE7E8FBF0DFFAEBD0F6E5CAE7DACABCB8BC948689A2A1A4BAC2
        C5C4CCD0FF00FFFF00FFFF00FFFF00FFFF00FFC5C8CBB3A3A6AC9497AF999DA8
        95999C8A8DA0989BB6BCBFC1C9CDC6CED2FF00FFFF00FFFF00FF}
    end
    object btDesactiver: TBitBtn
      Left = 286
      Top = 8
      Width = 161
      Height = 25
      Caption = 'Deactivate auto-replication'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
      OnClick = btDesactiverClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFC6CED2C2CACEB9C0C4B0B7BBABB1B5A9B0B3AAB1B4B0B7BBBAC2C5B1B8
        BBABB2B6B2BABDC1C9CDFF00FFFF00FFC4CCD0BBC3C6A4ABAE868B8E6C70735C
        6062585B5D595D5F6C7073888E906E73756063657A7F82B2BABDFF00FFC5CDD1
        B6BDC08F94976F707265686AD0D0D0B06822D57A0AB56E20444749BA7026DB7D
        08AE6C265C6062ABB1B5C6CED2BAC2C59393957B767A90949AB5ACA4F2EEEAD4
        7108FFA200E37E01B6B5B5D97400FFA200D37306585B5DA9B0B3C2CACEA1A2A4
        8E878ABCB7B1FED8ACFFE8B6FFFAF0C8660AF38D00D46F01DFDCD9CA6400F28B
        00C66606585B5DA8AFB2B6BCBF938688C7C1BAFFE4B1FFE4B8FFE1B5FFF7EEBB
        580AE37B00C86201FDEFDABF5800E07A00BC5906575B5CA8AFB2A0979AB2B2B8
        FFE0B1FFE8BCFFE4BBFFE5BDFFFAF0B55918D4720DBC5901FDF1E2B85C15D472
        0CB05004565A5BA8AFB29D8A8DDCD2C6FFECBDFFE8BFFFEAC2FFF2C8FAF5ECB6
        6B3EE0AA75C17035FDEFDEBF784BE0AA76B2612B585B5DA9B0B3A69296EEDEC5
        FFF6DBFFEECBFFF3CAFFF8D0E1E1E29D6042C38362C27A48FDEFD9CB8E66C386
        649657375C6062ABB1B5AC979AF4E5C9FFF8E0FFF8DDFFFFDDB3AB92D3D3D6D6
        D6D8DBD8D2FAF3E5FFF5E2FFFBF4F6F1EBC6C8C96C7073B1B8BBAC9395F5E9D9
        FFFBDFFFFFF1CFCBB201030BDFD7B6F7EAC60E1118726A5CFFEFC1FFEDC2C5B7
        A7656569858A8DB9C1C5B4A4A6E4E0E5FFF4CBE6E5D7000005C3C0ABFFFFE4FF
        FFD8F2E3BFA09480FFF3C6FFE4B395979B6E7173A4ABAEC3CBCFC5C8CBC1A5A8
        FFFFF4D7CDAFA2A49DFFFFF9FFFCE0FFF7D5FFF4CCFFF4C9FFEFBDCBC1B57C78
        7C8F9497BBC3C6C6CED2FF00FFC9BDC0D3BDC0FFFFF8FFFBD2FFFFE6FFFDE3FF
        F9DEFFF7D0FFE8B8D5CABF8F898C919294B6BEC1C5CDD1FF00FFFF00FFFF00FF
        C9BDC0C4A6A9EAE7E8FBF0DFFAEBD0F6E5CAE7DACABCB8BC948689A2A1A4BAC2
        C5C4CCD0FF00FFFF00FFFF00FFFF00FFFF00FFC5C8CBB3A3A6AC9497AF999DA8
        95999C8A8DA0989BB6BCBFC1C9CDC6CED2FF00FFFF00FFFF00FF}
    end
    object btReplicate: TBitBtn
      Left = 6
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Replicate now'
      TabOrder = 0
      OnClick = btReplicateClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFC4CBCFB9C0C4AFB6B9B6BDC0C3CBCFC6CED2C6CED2C6CE
        D2FF00FFFF00FFFF00FFFF00FFFF00FFC5CDD1C0C7CBBDC5C9B7BEC28E94976B
        6F71808688AFB6B9B6BDC0ADB4B7B7BEC2C4CBCFFF00FFFF00FFFF00FFC6CED2
        B7BEC29AA0A393999C8C7C7E7F4E4E6E585A55585A82878A7E82846A6E70878D
        90B8BFC3FF00FFFF00FFFF00FFC0C7CB978E91746B6D505355785D5ED0A4A481
        4E4E4E4D4E675B5D844F4F6B5A5B6A6E70AFB6BAC6CED2FF00FFFF00FFB0A9AC
        A37A7A9260607F5A5BAC7979E5C5C5B38888885555BE8B8BE0B4B48256566569
        6BA2A9ACBCC3C7C4CCD0C4CBCF9F8486DCC2C2DBBDBDD2A7A7E7C2C2DFC0C0DB
        BBBBC9A0A0E0BABAD6B0B07056574F5254727678969C9FBBC3C6B8BFC38A898B
        A57A7AE8D1D1E3C9C9E2C8C8E3C7C7E2C9C9E3C8C8DEBFBFCDA5A5774C4C6D57
        58635B5D6D7173B0B7BBA48E90A07070A47474E9D1D1E4CCCCE7D0D0DEC1C1DF
        BEBEDFC2C2E1C6C6DEBDBDC79999EDBDBD7651526D7173B0B7BBAC8283EDD3D3
        E6D1D1E3CACAECD7D7C09C9C8F7C7DB19FA1D8AFAFDFC2C2E1C5C5DEBDBDE1BF
        BF8058588C9295BBC3C6AD8282EDDEDEF0E2E2E5CCCCF1E0E08C696947494B69
        6D6FA88F91E1C4C4E1C7C7CDA8A87F5959656163959B9EBEC6CABDB5B8A6898A
        D2B4B4E6CDCDF3E5E5A27979725A5B786769BE9798E5CCCCE1C7C7CAA5A58355
        555E5D5E8F9598BDC5C9C6CED2B39799DCBFBFEAD6D6ECD9D9F1E2E2DABBBBE6
        C8C8EBD7D7E4CACAE2C6C6E7CACAC19797796D6EA2A9ACC1C9CDFF00FFCEA5A5
        FDFBFBF7F1F1E9D3D3E6CECEEFDEDEECD9D9E4CACAE3C6C6C19D9DBE9898A378
        789E9FA2BBC3C6C6CED2FF00FFC8AEB0CBA5A5BD9798CBA8A8F2E6E6E8D0D0E6
        CECEE9D4D4DCC1C17F6C6D898688AFA7AAC0C8CCC5CDD1FF00FFFF00FFFF00FF
        C7CDD1C6CED2C3A0A1FBF6F6C29E9EA58586CAA6A6E0C7C79A8182A7AEB2C1C9
        CDFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCAB2B4C79F9FB7A5A7B9
        C0C4BAA5A7AE9192B7BABDC1C9CDC6CED2FF00FFFF00FFFF00FF}
    end
    object btArreter: TBitBtn
      Left = 454
      Top = 8
      Width = 115
      Height = 25
      Caption = 'Stop replication'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 3
      OnClick = btArreterClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFC6CED2C1C9CDB7BEC2AFB6B9AAB1B4AAB1B4AFB6B9B7BEC2C1C9CDC6CE
        D2FF00FFFF00FFFF00FFFF00FFFF00FFC4CCD0B8BFC3A0A6A97F8486676B6D5A
        5E605A5E60676B6D7F8486A0A6A9B8BFC3C4CCD0FF00FFFF00FFFF00FFC4CCD0
        B3BABE838897464A8C2024980F119E0F0F9820228F3F41784B4E565F63658B90
        93B4BBBFC4CCD0FF00FFC6CED2B8BFC3686FA70B1BBA0A26D40E2AD51428CB1F
        28B6322797291E9411109647496B565A5B8B9093B9C0C4C6CED2C1C9CD717AB9
        011FD60032FF0030FA002DF8002BF40029EF0025E81D25B8392789120E984749
        6B5F63659FA5A8C1C9CDA6AEC3102ACE073CFF0639FF0538FE0336FF0133FF00
        2FF9002CF20027EF0F25CA3A288A1211964B4E577F8486B7BEC25765C12651FB
        0E42FF0E40FF0B3FFF073AFF0136FF0030FF002AFE0024F5001FF01B23B42A20
        943F417A676B6DAFB6B9243ED13C6AFF1144FF5477F25E80F76689F96D8FFA72
        92FD7193FF7493FF7A99FE4C71F93326932124935A5E60AAB1B41939E04C77FF
        1542F6F3ECD2EBE8DEF5F2E8FCF9F0FFFFF8FFFFFEFFFFFFFFFFFF4D75FE2025
        B110129F5A5E60AAB1B41A3EE3668CFF0F35E7F6F1D8F0EDE1FAF7EAFFFFF3FF
        FFFDFFFFFFFFFFFFFFFFFF4E76FF1427C80E13A5676B6DAFB6B92745DF84A5FF
        2344E03E52D13B56E13A5AEB3A5FF33D63F94066F94368F94A6FF91749FF122C
        D322289E7F8486B7BEC25B6DD76F90FD6D94FF4576FF4575FF3E6DFF3464FF28
        5AFF1C4FFF1144FF073BFF0035FF102BD3464D929FA5A8C1C9CDB1BBD42449ED
        97B4FF5C86FF4777FF4474FF3B69FF2F5FFF2455FF194BFF0E42FF0539FF0D20
        BF828798B8BFC3C6CED2FF00FF7F91DF365BF395B3FF6D91FF4675FF3A6AFF2F
        60FF2556FF1A4BFF1045FF0325DD6770ABB3BABEC4CCD0FF00FFFF00FFFF00FF
        7F91DF244BED7394FC86A7FF648AFF4B76FF3B69FF2452FA102CD6717BBCB8BF
        C3C4CCD0FF00FFFF00FFFF00FFFF00FFFF00FFB1BBD55B6ED92A49E11E44E91B
        3FE62542D85768C5A6AEC4C1C9CDC6CED2FF00FFFF00FFFF00FF}
    end
    object BitBtn1: TBitBtn
      Left = 871
      Top = 8
      Width = 103
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 4
      OnClick = BitBtn1Click
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFC6CED2C2CACEB9C0C4AFB6BAABB2B6ABB2B6AFB6BAB9C0C4C2CACEC6CE
        D2FF00FFFF00FFFF00FFFF00FFFF00FFC4CCD0BAC2C5A3A9AD83888B6B6F715F
        63655F63656B6F7183888BA3A9ADBAC2C5C4CCD0FF00FFFF00FFFF00FFC5CDD1
        B5BCC08A8F984B518B252A970E16A00E149D24278C4345754C4F546266688E94
        97B5BCC0C5CDD1FF00FFFF00FFBAC2C56E76A70E1DB60020D80023E00020D900
        19CC0011BD000BB00E119A474A66585B5D8E9497BAC2C5C6CED2C3CBCF7982BB
        031ED1002FFF002AFB002DF8002BF20029EE001FE50012CA000DB50307A0474A
        66626668A3A9ADC2CACEB2B9C3142CCB0035FF97A9E86888F40034FF002DFF00
        33FA95B1FD4570FB0019D4000DB60E10994C4F5483888BB9C0C45E6BC1254EF8
        0034FFCACDDCF5EFDEB1C0EF063DFFB1C4FAFFFFFBFFFFFF6187FD0012CA000C
        B24245756B6F71AFB6BA2C44CF3D6AFF083CFE97A3E2F4F1E1F6F3EAE4E8F3FF
        FFF8FFFFFFF3F7FF446DFD0022E70012BE2429915F6365ABB1B51E3DDC4B76FF
        2255FF0D41FE5C7AF0EFEDEBF6F6F3FFFFFBCDDAFE174AFF002AFC002CF30019
        CB0D15A55F6365ABB2B62041E06489FF2E5FFF3364FF0639FCDADCEDFBF9F3FF
        FFFBADC0FE0033FF0236FF0030FA0020DA0D16A86B7072B0B7BB2F4CDA83A2FF
        3D6CFF3C6CFF526DE9FFFBE9B2BFF3D4DAF9FFFFFF6D8FFF0033FF0032FF0023
        E2262D9C848A8CB9C0C46577D6688AFA7096FF2958F7BABEDEF5F3E71543F70E
        41FDE7ECFBFFFFFF2958FE0035FF0021DA4D548DA4AAADC3CBCFBDC5D32449E9
        97B6FF4C75F95B6CD8546BE13365FF2D5FFF1745FCC7D1FA4C73FF0135FF1023
        BC8C929BBBC3C6C6CED2FF00FF8E9DDD2C52F196B3FF6F95FF3966FA396AFF2F
        60FF2153FE083CFF0C40FF0423D6747DABB6BEC1C5CDD1FF00FFFF00FFFF00FF
        8E9EDC2347EC6688FA88A9FF6A8FFF4F7AFF3E6DFF204CF61630CF7F89BABBC3
        C6C4CCD0FF00FFFF00FFFF00FFFF00FFFF00FFBDC6D36B7CD9304FDD2546E324
        43E03049D16776C7B2BAC4C3CBCFC6CED2FF00FFFF00FFFF00FF}
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 981
    Height = 60
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object lbReplicationEnCours: TLabel
      Left = 1
      Top = 35
      Width = 979
      Height = 24
      Align = alBottom
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
    object RzLabel1: TLabel
      Left = 5
      Top = 1
      Width = 378
      Height = 29
      Caption = 'Microtec CopyCat demonstration'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13209
      Font.Height = -24
      Font.Name = 'Albany'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Image1: TImage
      Left = 828
      Top = 13
      Width = 32
      Height = 32
      Anchors = [akTop, akRight]
      AutoSize = True
      Picture.Data = {
        07544269746D6170360C0000424D360C00000000000036000000280000002000
        0000200000000100180000000000000C00000000000000000000000000000000
        0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC6CED2
        C3CBCFBBC3C6B3BABEAEB5B8AAB1B4A8AFB2A8AFB2ABB1B5AEB5B8B5BCC0BDC4
        C8C4CBCFC6CED2FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC6CED2C1C9CDB5BCC0
        A2A9AC8B9194757A7D65696B5A5E60565A5B565A5B5B5F60666A6C787D7F8E94
        97A5ACAFB6BEC1C1C9CDC6CED2FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC4CBCFB7BEC29EA4A87B8082
        6C6F71696B6C6466666161625F5F5F5C5C5C5858585B5C5C5D6061595C5D4E52
        535F63657F8486A2A9ACB9C0C4C4CCD0FF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1C9CDADB4B7888D8F7578796F6F6F
        8181818F8F8F9A9A9AA0A0A09E9E9E9B9B9B9797978C8C8C7A7A7A6666665555
        555B5D5E4C4F516165678B9093B1B8BBC3CBCFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFC0C8CCA7ADB184888A787979929292ADADAD
        ACACACA6A6A6A2A3A39E9FA19A9C9E989A9C9597989494959292929292928C8C
        8C6B6B6B51525255585A5256577D8284ABB1B5C1C9CDFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFC1C9CDA5ABAF8588898C8C8CB3B3B3B6B6B6AFB0B0
        AFB1B4BFC4CBCDC6BED8BFA4DDBC99DDBC9BD8C1AACBC7C3B3BAC09397999090
        919191918686865757575C5F604C4F51787D7FABB1B5C3CBCFFF00FFFF00FFFF
        00FFFF00FFFF00FFC4CBCFADB3B78688899C9C9CC1C1C1BCBCBCB9BBBECACAC8
        DFB388E09142DE862BDE862BDD852CDB8329DB8225DB7E1FE09345E5C09BC2C5
        C99193969090909090906161615D5F604D50517E8386B2B9BCC4CCD0FF00FFFF
        00FFFF00FFC6CED2B8BFC3939697A3A3A3C9C9C9C2C2C3C4C8CFD9B999E18E3A
        E18D39E08F3CE18F3CE18E3BE08D3ADF8C37DF8935DC8731DA842CD97E21DE88
        31E2CDB9A1A4A99191929292926060605C5E605457598F9598BBC3C6FF00FFFF
        00FFFF00FFC1C9CDA2A6A99E9E9ED0D0D0C9CACAC9CDD2DCA973E3903BE29343
        E29344E29343C67F36A86A28A76826A76724B47027D98735DD8835DB8630DA83
        2BD97A1BE6BF97A7ACB2939494929292565656545759666A6CA6ADB0C3CBCFFF
        00FFC6CED2B6BDC0979797CCCCCCCFD0D0CCCFD5DCA974E59544E4984AE4984B
        E4994CDD9345D1A086F1CCCDEFC8C7F0C8C9E7BEBAC77D2FDF8D39DD8935DB87
        31DA842DD97A1CE7BF98A0A4A89596968888885556564D5052868B8EB9C0C4FF
        00FFC4CBCFA5A9AAB6B6B6D5D5D5D0D3D6D5B595E79748E69B50E69C51E69C51
        E79C51DE9649D5A990FFE0E3FFDBDBFFDCDDF4CECDC47C31E2903EDE8D3ADD8A
        36DC8731DA842DD97B1DDECFBF96989B9B9B9B6A6A6A5B5E5F666A6CABB1B5C5
        CDD1BCC4C79E9E9FD6D6D6D6D7D8CBC9C8E89C4EE79F55E8A057E8A058E8A158
        E8A258E49C53E0AF90F9D5D1F8D2CDF8D2CDF0C9BFD38A3CE29443E0903FDF8D
        3BDD8A36DB8731DA8229DF9141BDC1C79B9B9B9191915C5D5D525657979DA0C0
        C7CBAFB4B6B4B4B4DCDCDDD5D8DCD5AF87EBA056EAA35BEAA45DEBA55EEBA55F
        ECA560ECA55ED8964FB77938B27432B37431CA853CE4984CE39648E29344E090
        3FDE8D3ADD8935DB8630D97D1FE0C6AC9D9FA0A2A2A264646456595A82878AB9
        C0C4ABAFB0C9C9C9DFDFDFCCD1D5E7A35DEBA55FECA762ECA864EDA965EDAA65
        EDA965EDA964CC996AE1C7C3DCC1BBDEC4BFCEA17FE2994FE6994EE39648E193
        43E08F3EDE8C39DC8833DA822BE3A261B1B5BAA2A2A27C7C7C5D6062727779B2
        BABDAAACACD9D9D9E0E1E2CBC2B9EFA65BEDA965EEAB68EFAD6AEFAE6CEFAE6C
        EFAE6CEFAD69CB9D75FFF0F4FFEAEBFFEDF0D4AD93E19950E99D52E59A4CE395
        47E19242DF8E3CDD8A37DB8731DB832AC6CCD1A6A6A6929292606263676C6EAF
        B6B9AEAFB0E3E3E3E3E5E6CEB8A2EFAB63EFAD6BF0AF6EF1B171F1B272F1B273
        F1B274F1B16FCCA178FFF1F4FFEAEAFFECEECDB1A4A46E36D5924CE69D51E599
        4CE29445E0903FDE8C3ADC8834DB7F23D0CAC3ABABACA0A0A0626364636769AD
        B4B7B2B2B2E8E8E8E4E6E9CEB397F1AE69F1B170F2B374F3B577F4B679F4B77B
        F5B879F3B577DDAE80FFF4F8FFF3F6FFEEEFFFEFF1F8E3E3B79279B97A3BE59A
        4EE49749E19242DF8E3CDD8A36DC8326D5C5B5AEAFB0AAAAAA63646464686AAD
        B4B7B5B5B5EAEAEAE7EAECCEB399F4B16EF2B477F4B77AF5B97DF6BB7FF6BB80
        F6BB80F6BB7EF2B579E1A669E0B690F8E8E6FFF0F2FFECEDFFF0F4D7B8ADBB7C
        3BE5984AE29546E0903FDE8C38DD8328D4C5B7B4B4B5AEAEAE6465656B6F71AF
        B6BAB7B8B9E9E9E9ECEEEFCBB9A5F5B472F4B77AF6BB7FF7BD83F8BF87F8C088
        F8BF88F7BE84F7BD82F6BA7CF3B474D89D62F2DFD8FFEEEFFFE8E9FFEDF1C49E
        82DA9246E49648E19141DE8D3ADD8429D1CDC7B8B8B9ABABAB696B6C767B7DB4
        BBBFBABCBDE5E5E5EEEFF0C8C4BEF6B674F5BA7EF7BF84F9C18AFAC38CFBC58E
        FCC68DFCC28BF9C087F7BD82F5B97CF1B172CEA47FFFF2F5FFEAEAFFE9E9EED2
        D0C9843DE59749E19242DF8E3CDE8C38CBD2D7BEBFBFA2A2A26A6D6E888E90BA
        C2C5C0C4C6DBDBDBF1F1F1D3D7DAEBB67DF7BC82F8C089FAC48DFDC893F4C18E
        CAA076DDAB7AF5BE86F9C185F9BC80EDB072BB946EFFF5F7FFEAEAFFE6E7FFE9
        EDBE7D39E5994AE19343DF8D38DCA66EC8CBCFC4C4C49393936B6F709DA3A6C1
        C9CDC4CACDCCCCCCF5F5F5EFF1F3CDB296FABD81F9C38AFBC690FFCC98E3B483
        F5EAECCCBDB1AD8E71A97E55AB7E50A17B54DFCEC8FFF0F1FFEAEAFFE7E8FBE1
        E3CA843DE4984BE19344E08A32D5C5B4C9CACCCCCCCC7C7C7C717577AFB6B9C6
        CED2C5CDD1C6C7C7EEEEEEF5F6F7CACCCDF3BB82FAC38AFCC692FFCD99E0B081
        F4E7E7FFFAFBFFFFFFFFF9FBFCF3F5FFFBFEFFF2F4FFEDEDFFEAEAFFEAEDE3BB
        A5DE9549E5974AE29241DD9D5DCFD4DACECFCFB7B7B77A7B7C8F9598BDC4C8FF
        00FFFF00FFCBD0D3D7D7D7F9F9F9F4F5F8C4B5A4FEC286FCC68FFFCA95E7B784
        F7E9EAFFF5F7FFF4F5FFF5F6FFF3F3FFF1F1FFEEEFFFEDEEFFEDF1EFCFC6D692
        4DE69B50E5974AE1913DD1CBC5D0D1D3D5D5D58C8C8C7D8183ADB4B7C4CCD0FF
        00FFFF00FFC6CED2C8C9CAEDEDEDF9F9FAE7EAEDCEB295FEC289FCC690F9C48E
        EBBF96ECCBB3F2DACEF8E6E4FDEEF0FEEEF2F9E4E3EFCFC2E0AF86DE9951E8A0
        56E69B4FE59440D2BCA7D4D7DAD6D6D6B6B6B683858699A0A3BFC7CAFF00FFFF
        00FFFF00FFFF00FFCBD1D4CDCDCDF9F9F9FAFBFBE1E5E7CCB196FDC085FAC38A
        FAC189F7BE84F1B87BEAAF72E4A869E2A464E4A361E8A55FEBA65EEAA25BE89E
        52E59848D0BAA5D6DADDDADADACFCFCF8788888E9395B8BFC3C6CED2FF00FFFF
        00FFFF00FFFF00FFFF00FFD1D6D8D3D3D3FCFCFCFBFCFCEAEEF0C3B6AAECB882
        FABD81F8BC81F6BB7EF5B87BF3B576F1B171EFAE6AEEA964ECA55DEAA051DDA1
        64CDC6C0DCDFE2DDDDDED9D9D9959595909496B3BABEC4CCD0FF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFD2D6D8D2D2D2FAFAFAFCFCFCFBFCFDD3D6D7
        C7B4A0DFB081F4B473F5B26FF4B16BF1AC66F0A85FE9A55ED8A874CDBBAAD6DB
        E0E3E4E6E2E2E2D8D8D89A9A9A979B9DB3BABEC4CBCFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFCFD4D7CCCCCCEDEDEDFFFFFFFBFCFC
        FAFCFEE3E6EACED1D2CCC5BCCEBFB0CFC0B1CDC7C1D1D4D9E0E4E8EAECEDE8E8
        E8E9E9E9C7C7C7999A9AA1A5A8B9C0C4C4CCD0FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCBD1D5C4C6C7D3D3D3EFEFEF
        FEFEFEFCFCFDFAFBFBF7F9F9F5F6F8F4F6F7F2F3F4F0F1F1EFEFEFEDEDEDD6D6
        D6AFAFAFA4A6A7AEB4B7C0C7CBC6CED2FF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC6CED2C9CFD1CFD1D2
        CCCCCCDBDBDBE5E5E5EDEDEDEFEFEFEDEDEDE6E6E6D9D9D9C8C8C8B0B1B1ADAF
        B0B3B9BBBDC5C9C4CCD0FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C7CED2C3C8CBC0C3C5BBBDBEB8BABBB6B8B9B5B7B8B3B6B7B4B9BCBBC2C6C2CA
        CEC6CED2FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
    end
    object Label3: TLabel
      Left = 868
      Top = 21
      Width = 108
      Height = 16
      Anchors = [akTop, akRight]
      Caption = 'Press F1 for help'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object PageControl1: TPageControl
    Left = 3
    Top = 63
    Width = 973
    Height = 595
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Replication'
      object Splitter1: TSplitter
        Left = 230
        Top = 0
        Width = 3
        Height = 550
        Cursor = crHSplit
      end
      object Splitter2: TSplitter
        Left = 732
        Top = 0
        Width = 3
        Height = 550
        Cursor = crHSplit
        Align = alRight
      end
      object edLog: TMemo
        Left = 233
        Top = 0
        Width = 499
        Height = 550
        Align = alClient
        BorderStyle = bsNone
        Color = clInfoBk
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object ProgressBar: TProgressBar
        Left = 0
        Top = 550
        Width = 965
        Height = 17
        Align = alBottom
        Min = 0
        Max = 100
        TabOrder = 3
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 230
        Height = 550
        Align = alLeft
        BevelInner = bvLowered
        BevelOuter = bvNone
        TabOrder = 0
        object Panel6: TPanel
          Left = 1
          Top = 1
          Width = 228
          Height = 36
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 7
            Top = 11
            Width = 52
            Height = 13
            Caption = 'Local user:'
          end
          object edLocalUser: TEdit
            Left = 69
            Top = 8
            Width = 136
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            CharCase = ecUpperCase
            Color = clWhite
            TabOrder = 0
            OnEnter = edLocalUserEnter
            OnExit = edLocalUserExit
          end
        end
        object RzDBNavigator3: TDBNavigator
          Left = 1
          Top = 524
          Width = 228
          Height = 25
          DataSource = RemoteDataSetDS
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          Align = alBottom
          TabOrder = 2
        end
        object DBGrid1: TDBGrid
          Left = 1
          Top = 37
          Width = 228
          Height = 487
          Align = alClient
          DataSource = LocalDataSetDS
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          OnEnter = DBGrid1Enter
        end
      end
      object Panel7: TPanel
        Left = 735
        Top = 0
        Width = 230
        Height = 550
        Align = alRight
        BevelInner = bvLowered
        BevelOuter = bvNone
        TabOrder = 2
        object RzDBNavigator2: TDBNavigator
          Left = 1
          Top = 524
          Width = 228
          Height = 25
          DataSource = LocalDataSetDS
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          Align = alBottom
          TabOrder = 2
        end
        object Panel5: TPanel
          Left = 1
          Top = 1
          Width = 228
          Height = 36
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label2: TLabel
            Left = 7
            Top = 11
            Width = 63
            Height = 13
            Caption = 'Remote user:'
          end
          object edRemoteUser: TEdit
            Left = 75
            Top = 8
            Width = 200
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            CharCase = ecUpperCase
            Color = clWhite
            TabOrder = 0
            OnEnter = edRemoteUserEnter
            OnExit = edRemoteUserExit
          end
        end
        object DBGrid2: TDBGrid
          Left = 1
          Top = 37
          Width = 228
          Height = 487
          Align = alClient
          DataSource = RemoteDataSetDS
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          OnEnter = DBGrid2Enter
        end
      end
    end
    object TabSheet2: TTabSheet
      HelpContext = 2
      Caption = 'Conflicts'
      ImageIndex = 1
      object DBGrid3: TDBGrid
        Left = 0
        Top = 0
        Width = 965
        Height = 534
        Align = alClient
        DataSource = qConflictsDS
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Panel3: TPanel
        Left = 0
        Top = 534
        Width = 965
        Height = 33
        Align = alBottom
        TabOrder = 1
        object RzDBNavigator1: TDBNavigator
          Left = 667
          Top = 4
          Width = 288
          Height = 25
          DataSource = qConflictsDS
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          Anchors = [akTop, akRight]
          TabOrder = 0
        end
      end
    end
  end
  object Replicator: TCcReplicator
    Version = '2.00.0'
    HarmonizeFields = False
    KeepConnection = False
    Nodes.LocalNode.Connection = CcProviderIBX1
    Nodes.RemoteNode.Connection = CcProviderIBX1
    AutoReplicate.Frequency = 30
    AutoReplicate.Enabled = True
    AutoCommit.Frequency = 30
    AutoCommit.CommitType = ctNone
    CommitOnFinished = ctCommit
    AbortOnError = False
    OnFinished = ReplicatorFinished
    OnRowReplicating = ReplicatorRowReplicating
    OnConflict = ReplicatorConflict
    OnReplicationError = ReplicatorReplicationError
    OnException = ReplicatorException
    OnAbort = ReplicatorAbort
    OnEmptyLog = ReplicatorEmptyLog
    OnTableBegin = ReplicatorTableBegin
    OnGenReplError = ReplicatorGenReplError
    OnGenReplicating = ReplicatorGenReplicating
    OnAutoCommit = ReplicatorAutoCommit
    OnConnectLocal = ReplicatorConnectLocal
    OnConnectRemote = ReplicatorConnectRemote
    OnDisconnect = ReplicatorDisconnect
    OnProgress = ReplicatorProgress
    OnReplicateProc = ReplicatorReplicateProc
    OnQueryDone = ReplicatorQueryDone
    OnLogLoaded = ReplicatorLogLoaded
    BeforeReplicate = ReplicatorBeforeReplicate
    OnConnectionLost = ReplicatorConnectionLost
    Left = 296
    Top = 96
  end
  object LocalDataSetDS: TDataSource
    DataSet = qLocalDataSet
    Left = 40
    Top = 192
  end
  object RemoteDataSetDS: TDataSource
    DataSet = qRemoteDataSet
    Left = 680
    Top = 104
  end
  object qLocalDataSet: TIBDataSet
    Database = LocalDB
    Transaction = LocalTR
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'DELETE FROM CUSTOMERS'
      'WHERE     '
      '            CUST_NO = ?OLD_CUST_NO'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO CUSTOMERS('
      '    CUST_NO,'
      '    NAME'
      ')'
      'VALUES('
      '    ?CUST_NO,'
      '    ?NAME'
      ')')
    RefreshSQL.Strings = (
      'select * from customers'
      ' WHERE '
      '    (    '
      '            customers.CUST_NO = ?OLD_CUST_NO'
      '    )')
    SelectSQL.Strings = (
      'select * from customers')
    ModifySQL.Strings = (
      'UPDATE CUSTOMERS SET '
      '    CUST_NO = ?CUST_NO,'
      '    NAME = ?NAME'
      ' WHERE     '
      '            CUST_NO = ?OLD_CUST_NO'
      '    ')
    Left = 8
    Top = 192
    object qLocalDataSetCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
    end
    object qLocalDataSetNAME: TStringField
      FieldName = 'NAME'
      Size = 200
    end
  end
  object qScript: TIBSQL
    Database = LocalDB
    ParamCheck = False
    Transaction = LocalTR
    Left = 320
    Top = 272
  end
  object qConflicts: TIBDataSet
    Database = LocalDB
    Transaction = LocalTR
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'DELETE FROM RPL$CONFLICTS'
      'WHERE     '
      '            CODE = ?OLD_CODE'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO RPL$CONFLICTS('
      '    CODE,'
      '    USER1,'
      '    USER2,'
      '    CONFLICT_DATE,'
      '    TABLE_NAME,'
      '    PK1_VALUE,'
      '    CHOSEN_USER'
      ')'
      'VALUES('
      '    ?CODE,'
      '    ?USER1,'
      '    ?USER2,'
      '    ?CONFLICT_DATE,'
      '    ?TABLE_NAME,'
      '    ?PK1_VALUE,'
      '    ?CHOSEN_USER'
      ')')
    RefreshSQL.Strings = (
      
        'select code, conflict_date, table_name, pk1_value, user1, user2,' +
        ' chosen_user'
      'from rpl$conflicts'
      ' WHERE '
      '    (    '
      '            rpl$conflicts.CODE = ?OLD_CODE'
      '    )')
    SelectSQL.Strings = (
      
        'select code, conflict_date, cast(table_name as varchar(31)) as t' +
        'able_name, pk1_value, user1, user2, chosen_user'
      'from rpl$conflicts')
    ModifySQL.Strings = (
      'UPDATE RPL$CONFLICTS SET '
      '    CODE = ?CODE,'
      '    USER1 = ?USER1,'
      '    USER2 = ?USER2,'
      '    CONFLICT_DATE = ?CONFLICT_DATE,'
      '    TABLE_NAME = ?TABLE_NAME,'
      '    PK1_VALUE = ?PK1_VALUE,'
      '    CHOSEN_USER = ?CHOSEN_USER'
      ' WHERE     '
      '            CODE = ?OLD_CODE'
      '    ')
    Left = 320
    Top = 160
  end
  object qConflictsDS: TDataSource
    DataSet = qConflicts
    Left = 352
    Top = 160
  end
  object RestoreService: TIBRestoreService
    Params.Strings = (
      'USER_NAME=SYSDBA')
    TraceFlags = []
    PageSize = 0
    PageBuffers = 0
    Left = 468
    Top = 200
  end
  object SecurityService: TIBSecurityService
    LoginPrompt = False
    TraceFlags = []
    SecurityAction = ActionAddUser
    UserID = 0
    GroupID = 0
    Left = 500
    Top = 203
  end
  object LocalDB: TIBDatabase
    DefaultTransaction = LocalTR
    IdleTimer = 0
    SQLDialect = 1
    TraceFlags = []
    OnLogin = LocalDBLogin
    Left = 324
    Top = 427
  end
  object LocalTR: TIBTransaction
    Active = False
    DefaultDatabase = LocalDB
    AutoStopAction = saNone
    Left = 356
    Top = 427
  end
  object RplConfig: TCcConfig
    Version = '2.00.0'
    DatabaseNode = dnLocal
    Terminator = '�'
    Connection = CcProviderIBX1
    Left = 320
    Top = 240
  end
  object Timer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = TimerTimer
    Left = 460
    Top = 323
  end
  object RemoteDB: TIBDatabase
    DefaultTransaction = RemoteTR
    IdleTimer = 0
    SQLDialect = 1
    TraceFlags = []
    OnLogin = LocalDBLogin
    Left = 324
    Top = 459
  end
  object RemoteTR: TIBTransaction
    Active = False
    DefaultDatabase = RemoteDB
    AutoStopAction = saNone
    Left = 356
    Top = 459
  end
  object qLocalUpdateRPLLog: TIBSQL
    Database = LocalDB
    ParamCheck = True
    SQL.Strings = (
      'update rpl$log set login = :new_user where login = :old_user')
    Transaction = LocalTR
    Left = 392
    Top = 426
  end
  object qLocalUpdateRPLUser: TIBSQL
    Database = LocalDB
    ParamCheck = True
    SQL.Strings = (
      'update rpl$users set login = :new_user where login = :old_user')
    Transaction = LocalTR
    Left = 424
    Top = 426
  end
  object qLocalInsertRPLUser: TIBSQL
    Database = LocalDB
    ParamCheck = True
    SQL.Strings = (
      'insert into rpl$users (login) values (:new_user)')
    Transaction = LocalTR
    Left = 456
    Top = 426
  end
  object qRemoteUpdateRPLLog: TIBSQL
    Database = RemoteDB
    ParamCheck = True
    SQL.Strings = (
      'update rpl$log set login = :new_user where login = :old_user')
    Transaction = RemoteTR
    Left = 392
    Top = 458
  end
  object qRemoteUpdateRPLUser: TIBSQL
    Database = RemoteDB
    ParamCheck = True
    SQL.Strings = (
      'update rpl$users set login = :new_user where login = :old_user')
    Transaction = RemoteTR
    Left = 424
    Top = 458
  end
  object qRemoteInsertRPLUser: TIBSQL
    Database = RemoteDB
    ParamCheck = True
    SQL.Strings = (
      'insert into rpl$users (login) values (:new_user)')
    Transaction = RemoteTR
    Left = 456
    Top = 458
  end
  object qGetLocalUser: TIBSQL
    Database = RemoteDB
    ParamCheck = True
    SQL.Strings = (
      'select max(login) as login from rpl$users')
    Transaction = RemoteTR
    Left = 528
    Top = 378
  end
  object qGetRemoteUser: TIBSQL
    Database = LocalDB
    ParamCheck = True
    SQL.Strings = (
      'select max(login) as login from rpl$users')
    Transaction = LocalTR
    Left = 560
    Top = 378
  end
  object qRemoteDataSet: TIBDataSet
    Database = RemoteDB
    Transaction = RemoteTR
    AfterDelete = qRemoteDataSetAfterDelete
    AfterPost = qRemoteDataSetAfterPost
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'DELETE FROM CUSTOMERS'
      'WHERE     '
      '            CUST_NO = ?OLD_CUST_NO'
      '    ')
    InsertSQL.Strings = (
      'INSERT INTO CUSTOMERS('
      '    CUST_NO,'
      '    NAME'
      ')'
      'VALUES('
      '    ?CUST_NO,'
      '    ?NAME'
      ')')
    RefreshSQL.Strings = (
      'select * from customers'
      ' WHERE '
      '    (    '
      '            customers.CUST_NO = ?OLD_CUST_NO'
      '    )')
    SelectSQL.Strings = (
      'select * from customers')
    ModifySQL.Strings = (
      'UPDATE CUSTOMERS SET '
      '    CUST_NO = ?CUST_NO,'
      '    NAME = ?NAME'
      ' WHERE     '
      '            CUST_NO = ?OLD_CUST_NO'
      '    ')
    Left = 648
    Top = 104
    object qRemoteDataSetCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
    end
    object qRemoteDataSetNAME: TStringField
      FieldName = 'NAME'
      Size = 200
    end
  end
  object CcProviderIBX1: TCcConnectionIBX
    DBType = 'Interbase'
    SQLDialect = 1
    TRParams.Strings = (
      'write'
      'nowait'
      'rec_version')
    Left = 456
    Top = 112
  end
  object qLocalTables: TIBSQL
    Database = LocalDB
    ParamCheck = True
    SQL.Strings = (
      'select * from rpl$tables where table_name = :table_name')
    Transaction = LocalTR
    Left = 352
    Top = 306
  end
  object qRemoteTables: TIBSQL
    Database = RemoteDB
    ParamCheck = True
    SQL.Strings = (
      'select * from rpl$tables where table_name = :table_name')
    Transaction = RemoteTR
    Left = 352
    Top = 274
  end
end
