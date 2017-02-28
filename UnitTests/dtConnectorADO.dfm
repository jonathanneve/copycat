inherited dmtConnectorADO: TdmtConnectorADO
  OldCreateOrder = True
  object ADOConnection: TADOConnection
    ConnectionString = 
      'Provider=SQLNCLI10.1;Integrated Security=SSPI;Persist Security I' +
      'nfo=False;User ID="";Initial Catalog=zoueieoir;Data Source=KANGA' +
      'ROO\SQLEXPRESS;Initial File Name="";Server SPN=KANGAROO\Jonathan'
    Provider = 'SQLNCLI10.1'
    Left = 200
    Top = 168
  end
  object qCreateDB: TADOCommand
    Connection = ADOConnection
    Parameters = <>
    Left = 104
    Top = 128
  end
  object qDeleteDB: TADOCommand
    Connection = ADOConnection
    Parameters = <>
    Left = 112
    Top = 184
  end
end
