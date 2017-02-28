unit htmlentities;

interface

function strToHTML(const s : string) : string;
function HTMLToStr(const s : string) : string;

implementation
 
function strToHTML(const s : string) : string;
var
  i : integer;
begin
  result := '';
  for i := 1 to length(s) do
  begin
    case s[i] of
      '&' : result := result +  '&amp;';
      '<' : result := result +  '&lt;';
      '>' : result := result +  '&gt;';
      '"' : result := result +  '&amp;quot;';
      '™' : result := result +  '&amp;trade;';
      '©' : result := result +  '&amp;copy;';
      '®' : result := result +  '&amp;reg;';
      'À' : result := result +  '&amp;Agrave;';
      'Á' : result := result +  '&amp;Aacute;';
      'Â' : result := result +  '&amp;Acirc;';
      'Ã' : result := result +  '&amp;Atilde;';
      'Ä' : result := result +  '&amp;Auml;';
      'Å' : result := result +  '&amp;Aring;';
      'Æ' : result := result +  '&amp;AElig;';
      'Ç' : result := result +  '&amp;Ccedil;';
      'È' : result := result +  '&amp;Egrave;';
      'É' : result := result +  '&amp;Eacute;';
      'Ê' : result := result +  '&amp;Ecirc;';
      'Ë' : result := result +  '&amp;Euml;';
      'Ì' : result := result +  '&amp;Igrave;';
      'Í' : result := result +  '&amp;Iacute;';
      'Î' : result := result +  '&amp;Icirc;';
      'Ï' : result := result +  '&amp;Iuml;';
      'Ð' : result := result +  '&amp;ETH;';
      'Ñ' : result := result +  '&amp;Ntilde;';
      'Ò' : result := result +  '&amp;Ograve;';
      'Ó' : result := result +  '&amp;Oacute;';
      'Ô' : result := result +  '&amp;Ocirc;';
      'Õ' : result := result +  '&amp;Otilde;';
      'Ö' : result := result +  '&amp;Ouml;';
      'Ø' : result := result +  '&amp;Oslash;';
      'Ù' : result := result +  '&amp;Ugrave;';
      'Ú' : result := result +  '&amp;Uacute;';
      'Û' : result := result +  '&amp;Ucirc;';
      'Ü' : result := result +  '&amp;Uuml;';
      'Ý' : result := result +  '&amp;Yacute;';
      'Þ' : result := result +  '&amp;THORN;';
      'ß' : result := result +  '&amp;szlig;';
      'à' : result := result +  '&amp;agrave;';
      'á' : result := result +  '&amp;aacute;';
      'â' : result := result +  '&amp;acirc;';
      'ã' : result := result +  '&amp;atilde;';
      'ä' : result := result +  '&amp;auml;';
      'å' : result := result +  '&amp;aring;';
      'æ' : result := result +  '&amp;aelig;';
      'ç' : result := result +  '&amp;ccedil;';
      'è' : result := result +  '&amp;egrave;';
      'é' : result := result +  '&amp;eacute;';
      'ê' : result := result +  '&amp;ecirc;';
      'ë' : result := result +  '&amp;euml;';
      'ì' : result := result +  '&amp;igrave;';
      'í' : result := result +  '&amp;iacute;';
      'î' : result := result +  '&amp;icirc;';
      'ï' : result := result +  '&amp;iuml;';
      'ð' : result := result +  '&amp;eth;';
      'ñ' : result := result +  '&amp;ntilde;';
      'ò' : result := result +  '&amp;ograve;';
      'ó' : result := result +  '&amp;oacute;';
      'ô' : result := result +  '&amp;ocirc;';
      'õ' : result := result +  '&amp;otilde;';
      'ö' : result := result +  '&amp;ouml;';
      'ø' : result := result +  '&amp;oslash;';
      'ù' : result := result +  '&amp;ugrave;';
      'ú' : result := result +  '&amp;uacute;';
      'û' : result := result +  '&amp;ucirc;';
      'ü' : result := result +  '&amp;uuml;';
      'ý' : result := result +  '&amp;yacute;';
      'þ' : result := result +  '&amp;thorn;';
      'ÿ' : result := result +  '&amp;yuml;';
      '¡' : result := result +  '&amp;iexcl;';
      '¢' : result := result +  '&amp;cent;';
      '£' : result := result +  '&amp;pound;';
      '¤' : result := result +  '&amp;curren;';
      '¥' : result := result +  '&amp;yen;';
      '¦' : result := result +  '&amp;brvbar;';
      '§' : result := result +  '&amp;sect;';
      '¨' : result := result +  '&amp;uml;';
      'ª' : result := result +  '&amp;ordf;';
      '«' : result := result +  '&amp;laquo;';
      '¬' : result := result +  '&amp;shy;';
      '¯' : result := result +  '&amp;macr;';
      '°' : result := result +  '&amp;deg;';
      '±' : result := result +  '&amp;plusmn;';
      '²' : result := result +  '&amp;sup2;';
      '³' : result := result +  '&amp;sup3;';
      '´' : result := result +  '&amp;acute;';
      'µ' : result := result +  '&amp;micro;';
      '·' : result := result +  '&amp;middot;';
      '¸' : result := result +  '&amp;cedil;';
      '¹' : result := result +  '&amp;sup1;';
      'º' : result := result +  '&amp;ordm;';
      '»' : result := result +  '&amp;raquo;';
      '¼' : result := result +  '&amp;frac14;';
      '½' : result := result +  '&amp;frac12;';
      '¾' : result := result +  '&amp;frac34;';
      '¿' : result := result +  '&amp;iquest;';
      '×' : result := result +  '&amp;times;';
      '÷' : result := result +  '&amp;divide;';
      '€' : result := result +  '&amp;euro;';
      else result := result + s[i];
    end;
  end;
end;

function HTMLToStr(const s : string) : string;
var
  i : integer;

  function strAtPos(str: String): Boolean;
  begin
    Result := False;
    if Copy(s, i, Length(str)) = str then begin
      Result := True;
      i := i + Length(str)-1;
    end;
  end;

begin
  result := '';
  for i := 1 to length(s) do
  begin
    if strAtPos('&amp;') then Result := result + '&'
    else if strAtPos('&amp;') then Result := result + '&'
    else if strAtPos('&amp;') then Result := result + '&'
    else if strAtPos('&lt;') then Result := result + '<'
    else if strAtPos('&gt;') then Result := result + '>'
    else if strAtPos('&amp;quot;') then Result := result + '"'
    else if strAtPos('&amp;trade;') then Result := result + '™'
    else if strAtPos('&amp;copy;') then Result := result + '©'
    else if strAtPos('&amp;reg;') then Result := result + '®'
    else if strAtPos('&amp;Agrave;') then Result := result + 'À'
    else if strAtPos('&amp;Aacute;') then Result := result + 'Á'
    else if strAtPos('&amp;Acirc;') then Result := result + 'Â'
    else if strAtPos('&amp;Atilde;') then Result := result + 'Ã'
    else if strAtPos('&amp;Auml;') then Result := result + 'Ä'
    else if strAtPos('&amp;Aring;') then Result := result + 'Å'
    else if strAtPos('&amp;AElig;') then Result := result + 'Æ'
    else if strAtPos('&amp;Ccedil;') then Result := result + 'Ç'
    else if strAtPos('&amp;Egrave;') then Result := result + 'È'
    else if strAtPos('&amp;Eacute;') then Result := result + 'É'
    else if strAtPos('&amp;Ecirc;') then Result := result + 'Ê'
    else if strAtPos('&amp;Euml;') then Result := result + 'Ë'
    else if strAtPos('&amp;Igrave;') then Result := result + 'Ì'
    else if strAtPos('&amp;Iacute;') then Result := result + 'Í'
    else if strAtPos('&amp;Icirc;') then Result := result + 'Î'
    else if strAtPos('&amp;Iuml;') then Result := result + 'Ï'
    else if strAtPos('&amp;ETH;') then Result := result + 'Ð'
    else if strAtPos('&amp;Ntilde;') then Result := result + 'Ñ'
    else if strAtPos('&amp;Ograve;') then Result := result + 'Ò'
    else if strAtPos('&amp;Oacute;') then Result := result + 'Ó'
    else if strAtPos('&amp;Ocirc;') then Result := result + 'Ô'
    else if strAtPos('&amp;Otilde;') then Result := result + 'Õ'
    else if strAtPos('&amp;Ouml;') then Result := result + 'Ö'
    else if strAtPos('&amp;Oslash;') then Result := result + 'Ø'
    else if strAtPos('&amp;Ugrave;') then Result := result + 'Ù'
    else if strAtPos('&amp;Uacute;') then Result := result + 'Ú'
    else if strAtPos('&amp;Ucirc;') then Result := result + 'Û'
    else if strAtPos('&amp;Uuml;') then Result := result + 'Ü'
    else if strAtPos('&amp;Yacute;') then Result := result + 'Ý'
    else if strAtPos('&amp;THORN;') then Result := result + 'Þ'
    else if strAtPos('&amp;szlig;') then Result := result + 'ß'
    else if strAtPos('&amp;agrave;') then Result := result + 'à'
    else if strAtPos('&amp;aacute;') then Result := result + 'á'
    else if strAtPos('&amp;acirc;') then Result := result + 'â'
    else if strAtPos('&amp;atilde;') then Result := result + 'ã'
    else if strAtPos('&amp;auml;') then Result := result + 'ä'
    else if strAtPos('&amp;aring;') then Result := result + 'å'
    else if strAtPos('&amp;aelig;') then Result := result + 'æ'
    else if strAtPos('&amp;ccedil;') then Result := result + 'ç'
    else if strAtPos('&amp;egrave;') then Result := result + 'è'
    else if strAtPos('&amp;eacute;') then Result := result + 'é'
    else if strAtPos('&amp;ecirc;') then Result := result + 'ê'
    else if strAtPos('&amp;euml;') then Result := result + 'ë'
    else if strAtPos('&amp;igrave;') then Result := result + 'ì'
    else if strAtPos('&amp;iacute;') then Result := result + 'í'
    else if strAtPos('&amp;icirc;') then Result := result + 'î'
    else if strAtPos('&amp;iuml;') then Result := result + 'ï'
    else if strAtPos('&amp;eth;') then Result := result + 'ð'
    else if strAtPos('&amp;ntilde;') then Result := result + 'ñ'
    else if strAtPos('&amp;ograve;') then Result := result + 'ò'
    else if strAtPos('&amp;oacute;') then Result := result + 'ó'
    else if strAtPos('&amp;ocirc;') then Result := result + 'ô'
    else if strAtPos('&amp;otilde;') then Result := result + 'õ'
    else if strAtPos('&amp;ouml;') then Result := result + 'ö'
    else if strAtPos('&amp;oslash;') then Result := result + 'ø'
    else if strAtPos('&amp;ugrave;') then Result := result + 'ù'
    else if strAtPos('&amp;uacute;') then Result := result + 'ú'
    else if strAtPos('&amp;ucirc;') then Result := result + 'û'
    else if strAtPos('&amp;uuml;') then Result := result + 'ü'
    else if strAtPos('&amp;yacute;') then Result := result + 'ý'
    else if strAtPos('&amp;thorn;') then Result := result + 'þ'
    else if strAtPos('&amp;yuml;') then Result := result + 'ÿ'
    else if strAtPos('&amp;iexcl;') then Result := result + '¡'
    else if strAtPos('&amp;cent;') then Result := result + '¢'
    else if strAtPos('&amp;pound;') then Result := result + '£'
    else if strAtPos('&amp;curren;') then Result := result + '¤'
    else if strAtPos('&amp;yen;') then Result := result + '¥'
    else if strAtPos('&amp;brvbar;') then Result := result + '¦'
    else if strAtPos('&amp;sect;') then Result := result + '§'
    else if strAtPos('&amp;uml;') then Result := result + '¨'
    else if strAtPos('&amp;ordf;') then Result := result + 'ª'
    else if strAtPos('&amp;laquo;') then Result := result + '«'
    else if strAtPos('&amp;shy;') then Result := result + '¬'
    else if strAtPos('&amp;macr;') then Result := result + '¯'
    else if strAtPos('&amp;deg;') then Result := result + '°'
    else if strAtPos('&amp;plusmn;') then Result := result + '±'
    else if strAtPos('&amp;sup2;') then Result := result + '²'
    else if strAtPos('&amp;sup3;') then Result := result + '³'
    else if strAtPos('&amp;acute;') then Result := result + '´'
    else if strAtPos('&amp;micro;') then Result := result + 'µ'
    else if strAtPos('&amp;middot;') then Result := result + '·'
    else if strAtPos('&amp;cedil;') then Result := result + '¸'
    else if strAtPos('&amp;sup1;') then Result := result + '¹'
    else if strAtPos('&amp;ordm;') then Result := result + 'º'
    else if strAtPos('&amp;raquo;') then Result := result + '»'
    else if strAtPos('&amp;frac14;') then Result := result + '¼'
    else if strAtPos('&amp;frac12;') then Result := result + '½'
    else if strAtPos('&amp;frac34;') then Result := result + '¾'
    else if strAtPos('&amp;iquest;') then Result := result + '¿'
    else if strAtPos('&amp;times;') then Result := result + '×'
    else if strAtPos('&amp;divide;') then Result := result + '÷'
    else if strAtPos('&amp;euro;') then Result := result + '€'
    else result := result + s[i];
  end;
end;

end.