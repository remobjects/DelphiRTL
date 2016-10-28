namespace Elements.RTL.Delphi;

interface
{$GLOBALS ON}

function UpperCase(const S: DelphiString): DelphiString;
function UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function LowerCase(const S: DelphiString): DelphiString; 
function LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function CompareStr(const S1, S2: DelphiString): Integer; inline;
function CompareStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer;
function SameStr(const S1, S2: DelphiString): Boolean; inline;
function SameStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
/*
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
*/
function CompareText(const S1, S2: DelphiString): Integer; 
function CompareText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer; 
function SameText(const S1, S2: DelphiString): Boolean; inline;
function SameText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
function AnsiUpperCase(const S: DelphiString): DelphiString; inline;
function AnsiLowerCase(const S: DelphiString): DelphiString; inline;
function AnsiCompareStr(const S1, S2: DelphiString): Integer; inline;
function AnsiSameStr(const S1, S2: DelphiString): Boolean; inline;
function AnsiCompareText(const S1, S2: DelphiString): Integer; inline;
function AnsiSameText(const S1, S2: DelphiString): Boolean; inline;

implementation

function UpperCase(const S: DelphiString): DelphiString;
begin
  var lTmp := new Char[S.Length];
  for i: Integer := 0 to S.Length - 1 do
  if S.Chars[i] in ['a'..'z'] then
    lTmp[i] := Char(Integer(S[i]) xor $0020)
  else
    lTmp[i] := S[i];
  result := DelphiString.Create(lTmp);
end;

function UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; 
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := UpperCase(S)
  else
    result := S.ToUpper(TLanguages.UserDefaultLocale);
end;

function LowerCase(const S: DelphiString): DelphiString;
begin
  var lTmp := new Char[S.Length];
  for i: Integer := 0 to S.Length - 1 do
  if S.Chars[i] in ['A'..'Z'] then
    lTmp[i] := Char(Integer(S[i]) or $0020)
  else
    lTmp[i] := S[i];
  result := DelphiString.Create(lTmp);
end;

function LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := LowerCase(S)
  else
    result := S.ToLower(TLanguages.UserDefaultLocale);
end;

function CompareStr(const S1, S2: DelphiString): Integer;
begin
  result := DelphiString.CompareOrdinal(S1, S2);
end;

function CompareStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := CompareStr(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale);
end;

function SameStr(const S1, S2: DelphiString): Boolean;
begin
  result := DelphiString.Equals(S1, S2);
end;

function SameStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := SameStr(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale) = 0;
end;


function CompareText(const S1, S2: DelphiString): Integer; 
begin
  result := DelphiString.CompareOrdinal(UpperCase(S1), UpperCase(S2));
end;

function CompareText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer; 
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := CompareText(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale);
end;

function SameText(const S1, S2: DelphiString): Boolean;
begin
  result := CompareText(S1, S2) = 0;
end;

function SameText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := SameText(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale) = 0;
end;

function AnsiUpperCase(const S: DelphiString): DelphiString;
begin
  result := S.ToUpper;
end;

function AnsiLowerCase(const S: DelphiString): DelphiString;
begin
  result := S.ToLower;
end;

function AnsiCompareStr(const S1, S2: DelphiString): Integer;
begin
  result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale);  
end;

function AnsiSameStr(const S1, S2: DelphiString): Boolean;
begin
  result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale) = 0;  
end;

function AnsiCompareText(const S1, S2: DelphiString): Integer;
begin
  result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale);
end;

function AnsiSameText(const S1, S2: DelphiString): Boolean;
begin
  result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale) = 0;
end;

end.
