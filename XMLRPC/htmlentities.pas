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
      '�' : result := result +  '&amp;trade;';
      '�' : result := result +  '&amp;copy;';
      '�' : result := result +  '&amp;reg;';
      '�' : result := result +  '&amp;Agrave;';
      '�' : result := result +  '&amp;Aacute;';
      '�' : result := result +  '&amp;Acirc;';
      '�' : result := result +  '&amp;Atilde;';
      '�' : result := result +  '&amp;Auml;';
      '�' : result := result +  '&amp;Aring;';
      '�' : result := result +  '&amp;AElig;';
      '�' : result := result +  '&amp;Ccedil;';
      '�' : result := result +  '&amp;Egrave;';
      '�' : result := result +  '&amp;Eacute;';
      '�' : result := result +  '&amp;Ecirc;';
      '�' : result := result +  '&amp;Euml;';
      '�' : result := result +  '&amp;Igrave;';
      '�' : result := result +  '&amp;Iacute;';
      '�' : result := result +  '&amp;Icirc;';
      '�' : result := result +  '&amp;Iuml;';
      '�' : result := result +  '&amp;ETH;';
      '�' : result := result +  '&amp;Ntilde;';
      '�' : result := result +  '&amp;Ograve;';
      '�' : result := result +  '&amp;Oacute;';
      '�' : result := result +  '&amp;Ocirc;';
      '�' : result := result +  '&amp;Otilde;';
      '�' : result := result +  '&amp;Ouml;';
      '�' : result := result +  '&amp;Oslash;';
      '�' : result := result +  '&amp;Ugrave;';
      '�' : result := result +  '&amp;Uacute;';
      '�' : result := result +  '&amp;Ucirc;';
      '�' : result := result +  '&amp;Uuml;';
      '�' : result := result +  '&amp;Yacute;';
      '�' : result := result +  '&amp;THORN;';
      '�' : result := result +  '&amp;szlig;';
      '�' : result := result +  '&amp;agrave;';
      '�' : result := result +  '&amp;aacute;';
      '�' : result := result +  '&amp;acirc;';
      '�' : result := result +  '&amp;atilde;';
      '�' : result := result +  '&amp;auml;';
      '�' : result := result +  '&amp;aring;';
      '�' : result := result +  '&amp;aelig;';
      '�' : result := result +  '&amp;ccedil;';
      '�' : result := result +  '&amp;egrave;';
      '�' : result := result +  '&amp;eacute;';
      '�' : result := result +  '&amp;ecirc;';
      '�' : result := result +  '&amp;euml;';
      '�' : result := result +  '&amp;igrave;';
      '�' : result := result +  '&amp;iacute;';
      '�' : result := result +  '&amp;icirc;';
      '�' : result := result +  '&amp;iuml;';
      '�' : result := result +  '&amp;eth;';
      '�' : result := result +  '&amp;ntilde;';
      '�' : result := result +  '&amp;ograve;';
      '�' : result := result +  '&amp;oacute;';
      '�' : result := result +  '&amp;ocirc;';
      '�' : result := result +  '&amp;otilde;';
      '�' : result := result +  '&amp;ouml;';
      '�' : result := result +  '&amp;oslash;';
      '�' : result := result +  '&amp;ugrave;';
      '�' : result := result +  '&amp;uacute;';
      '�' : result := result +  '&amp;ucirc;';
      '�' : result := result +  '&amp;uuml;';
      '�' : result := result +  '&amp;yacute;';
      '�' : result := result +  '&amp;thorn;';
      '�' : result := result +  '&amp;yuml;';
      '�' : result := result +  '&amp;iexcl;';
      '�' : result := result +  '&amp;cent;';
      '�' : result := result +  '&amp;pound;';
      '�' : result := result +  '&amp;curren;';
      '�' : result := result +  '&amp;yen;';
      '�' : result := result +  '&amp;brvbar;';
      '�' : result := result +  '&amp;sect;';
      '�' : result := result +  '&amp;uml;';
      '�' : result := result +  '&amp;ordf;';
      '�' : result := result +  '&amp;laquo;';
      '�' : result := result +  '&amp;shy;';
      '�' : result := result +  '&amp;macr;';
      '�' : result := result +  '&amp;deg;';
      '�' : result := result +  '&amp;plusmn;';
      '�' : result := result +  '&amp;sup2;';
      '�' : result := result +  '&amp;sup3;';
      '�' : result := result +  '&amp;acute;';
      '�' : result := result +  '&amp;micro;';
      '�' : result := result +  '&amp;middot;';
      '�' : result := result +  '&amp;cedil;';
      '�' : result := result +  '&amp;sup1;';
      '�' : result := result +  '&amp;ordm;';
      '�' : result := result +  '&amp;raquo;';
      '�' : result := result +  '&amp;frac14;';
      '�' : result := result +  '&amp;frac12;';
      '�' : result := result +  '&amp;frac34;';
      '�' : result := result +  '&amp;iquest;';
      '�' : result := result +  '&amp;times;';
      '�' : result := result +  '&amp;divide;';
      '�' : result := result +  '&amp;euro;';
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
    else if strAtPos('&amp;trade;') then Result := result + '�'
    else if strAtPos('&amp;copy;') then Result := result + '�'
    else if strAtPos('&amp;reg;') then Result := result + '�'
    else if strAtPos('&amp;Agrave;') then Result := result + '�'
    else if strAtPos('&amp;Aacute;') then Result := result + '�'
    else if strAtPos('&amp;Acirc;') then Result := result + '�'
    else if strAtPos('&amp;Atilde;') then Result := result + '�'
    else if strAtPos('&amp;Auml;') then Result := result + '�'
    else if strAtPos('&amp;Aring;') then Result := result + '�'
    else if strAtPos('&amp;AElig;') then Result := result + '�'
    else if strAtPos('&amp;Ccedil;') then Result := result + '�'
    else if strAtPos('&amp;Egrave;') then Result := result + '�'
    else if strAtPos('&amp;Eacute;') then Result := result + '�'
    else if strAtPos('&amp;Ecirc;') then Result := result + '�'
    else if strAtPos('&amp;Euml;') then Result := result + '�'
    else if strAtPos('&amp;Igrave;') then Result := result + '�'
    else if strAtPos('&amp;Iacute;') then Result := result + '�'
    else if strAtPos('&amp;Icirc;') then Result := result + '�'
    else if strAtPos('&amp;Iuml;') then Result := result + '�'
    else if strAtPos('&amp;ETH;') then Result := result + '�'
    else if strAtPos('&amp;Ntilde;') then Result := result + '�'
    else if strAtPos('&amp;Ograve;') then Result := result + '�'
    else if strAtPos('&amp;Oacute;') then Result := result + '�'
    else if strAtPos('&amp;Ocirc;') then Result := result + '�'
    else if strAtPos('&amp;Otilde;') then Result := result + '�'
    else if strAtPos('&amp;Ouml;') then Result := result + '�'
    else if strAtPos('&amp;Oslash;') then Result := result + '�'
    else if strAtPos('&amp;Ugrave;') then Result := result + '�'
    else if strAtPos('&amp;Uacute;') then Result := result + '�'
    else if strAtPos('&amp;Ucirc;') then Result := result + '�'
    else if strAtPos('&amp;Uuml;') then Result := result + '�'
    else if strAtPos('&amp;Yacute;') then Result := result + '�'
    else if strAtPos('&amp;THORN;') then Result := result + '�'
    else if strAtPos('&amp;szlig;') then Result := result + '�'
    else if strAtPos('&amp;agrave;') then Result := result + '�'
    else if strAtPos('&amp;aacute;') then Result := result + '�'
    else if strAtPos('&amp;acirc;') then Result := result + '�'
    else if strAtPos('&amp;atilde;') then Result := result + '�'
    else if strAtPos('&amp;auml;') then Result := result + '�'
    else if strAtPos('&amp;aring;') then Result := result + '�'
    else if strAtPos('&amp;aelig;') then Result := result + '�'
    else if strAtPos('&amp;ccedil;') then Result := result + '�'
    else if strAtPos('&amp;egrave;') then Result := result + '�'
    else if strAtPos('&amp;eacute;') then Result := result + '�'
    else if strAtPos('&amp;ecirc;') then Result := result + '�'
    else if strAtPos('&amp;euml;') then Result := result + '�'
    else if strAtPos('&amp;igrave;') then Result := result + '�'
    else if strAtPos('&amp;iacute;') then Result := result + '�'
    else if strAtPos('&amp;icirc;') then Result := result + '�'
    else if strAtPos('&amp;iuml;') then Result := result + '�'
    else if strAtPos('&amp;eth;') then Result := result + '�'
    else if strAtPos('&amp;ntilde;') then Result := result + '�'
    else if strAtPos('&amp;ograve;') then Result := result + '�'
    else if strAtPos('&amp;oacute;') then Result := result + '�'
    else if strAtPos('&amp;ocirc;') then Result := result + '�'
    else if strAtPos('&amp;otilde;') then Result := result + '�'
    else if strAtPos('&amp;ouml;') then Result := result + '�'
    else if strAtPos('&amp;oslash;') then Result := result + '�'
    else if strAtPos('&amp;ugrave;') then Result := result + '�'
    else if strAtPos('&amp;uacute;') then Result := result + '�'
    else if strAtPos('&amp;ucirc;') then Result := result + '�'
    else if strAtPos('&amp;uuml;') then Result := result + '�'
    else if strAtPos('&amp;yacute;') then Result := result + '�'
    else if strAtPos('&amp;thorn;') then Result := result + '�'
    else if strAtPos('&amp;yuml;') then Result := result + '�'
    else if strAtPos('&amp;iexcl;') then Result := result + '�'
    else if strAtPos('&amp;cent;') then Result := result + '�'
    else if strAtPos('&amp;pound;') then Result := result + '�'
    else if strAtPos('&amp;curren;') then Result := result + '�'
    else if strAtPos('&amp;yen;') then Result := result + '�'
    else if strAtPos('&amp;brvbar;') then Result := result + '�'
    else if strAtPos('&amp;sect;') then Result := result + '�'
    else if strAtPos('&amp;uml;') then Result := result + '�'
    else if strAtPos('&amp;ordf;') then Result := result + '�'
    else if strAtPos('&amp;laquo;') then Result := result + '�'
    else if strAtPos('&amp;shy;') then Result := result + '�'
    else if strAtPos('&amp;macr;') then Result := result + '�'
    else if strAtPos('&amp;deg;') then Result := result + '�'
    else if strAtPos('&amp;plusmn;') then Result := result + '�'
    else if strAtPos('&amp;sup2;') then Result := result + '�'
    else if strAtPos('&amp;sup3;') then Result := result + '�'
    else if strAtPos('&amp;acute;') then Result := result + '�'
    else if strAtPos('&amp;micro;') then Result := result + '�'
    else if strAtPos('&amp;middot;') then Result := result + '�'
    else if strAtPos('&amp;cedil;') then Result := result + '�'
    else if strAtPos('&amp;sup1;') then Result := result + '�'
    else if strAtPos('&amp;ordm;') then Result := result + '�'
    else if strAtPos('&amp;raquo;') then Result := result + '�'
    else if strAtPos('&amp;frac14;') then Result := result + '�'
    else if strAtPos('&amp;frac12;') then Result := result + '�'
    else if strAtPos('&amp;frac34;') then Result := result + '�'
    else if strAtPos('&amp;iquest;') then Result := result + '�'
    else if strAtPos('&amp;times;') then Result := result + '�'
    else if strAtPos('&amp;divide;') then Result := result + '�'
    else if strAtPos('&amp;euro;') then Result := result + '�'
    else result := result + s[i];
  end;
end;

end.