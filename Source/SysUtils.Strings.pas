namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL;

const
  PathDelim  = {$IF WINDOWS} '\' {$ELSE} '/' {$ENDIF};
  DriveDelim = {$IF WINDOWS} ':' {$ELSE} ''  {$ENDIF};
  PathSep    = {$IF WINDOWS} ';' {$ELSE} ':' {$ENDIF};

function UpperCase(const S: DelphiString): DelphiString;
function UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function LowerCase(const S: DelphiString): DelphiString;
function LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function CompareStr(const S1, S2: DelphiString): Integer; inline;
function CompareStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer;
function SameStr(const S1, S2: DelphiString): Boolean; inline;
function SameStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
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
function Trim(const S: DelphiString): DelphiString;
function TrimLeft(const S: DelphiString): DelphiString;
function TrimRight(const S: DelphiString): DelphiString;
function QuotedStr(const S: DelphiString): DelphiString;
function AnsiQuotedStr(const S: DelphiString; Quote: Char): DelphiString;
function AnsiDequotedStr(const S: DelphiString; aQuote: Char): DelphiString;
function IntToStr(Value: Integer): DelphiString;
function IntToStr(Value: Int64): DelphiString;
function UIntToStr(Value: Cardinal): DelphiString;
function UIntToStr(Value: UInt64): DelphiString;
function IntToHex(Value: Integer; Digits: Integer := sizeOf(Integer) * 2): DelphiString;
function IntToHex(Value: Int64; Digits: Integer := sizeOf(Int64) * 2): DelphiString;
{$IF NOT COOPER}
function IntToHex(Value: UInt64; Digits: Integer := sizeOf(UInt64) * 2): DelphiString;
{$ENDIF}
function StrToInt(const S: DelphiString): Integer;
function StrToIntDef(const S: DelphiString; aDefault: Integer): Integer;
function TryStrToInt(const S: DelphiString; out Value: Integer): Boolean;
function StrToInt64(const S: DelphiString): Int64;
function StrToInt64Def(const S: DelphiString; const aDefault: Int64): Int64;
function TryStrToInt64(const S: DelphiString; out Value: Int64): Boolean;
function StrToUInt64(const S: DelphiString): UInt64;
function StrToUInt64Def(const S: DelphiString; const aDefault: UInt64): UInt64;
function TryStrToUInt64(const S: DelphiString; out Value: UInt64): Boolean;
function StrToBool(const S: DelphiString): Boolean;
function StrToBoolDef(const S: DelphiString; const aDefault: Boolean): Boolean;
function TryStrToBool(const S: DelphiString; out Value: Boolean): Boolean;
function BoolToStr(B: Boolean; UseBoolStrs: Boolean := False): DelphiString;

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

function Trim(const S: DelphiString): DelphiString;
begin
  result := S.Trim;
end;

function TrimLeft(const S: DelphiString): DelphiString;
begin
  result := S.TrimLeft;
end;

function TrimRight(const S: DelphiString): DelphiString;
begin
  result := S.TrimRight;
end;

function QuotedStr(const S: DelphiString): DelphiString;
begin
  result := S.QuotedString;
end;

function AnsiQuotedStr(const S: DelphiString; Quote: Char): DelphiString;
begin
  result := S.QuotedString(Quote);
end;

function AnsiDequotedStr(const S: DelphiString; aQuote: Char): DelphiString;
begin
  result := S.DeQuotedString(aQuote);
end;

function IntToStr(Value: Integer): DelphiString;
begin
  result := Convert.ToString(Value);
end;

function IntToStr(Value: Int64): DelphiString;
begin
  result := Convert.ToString(Value);
end;

function UIntToStr(Value: Cardinal): DelphiString;
begin
  result := Convert.ToString(Value);
end;

function UIntToStr(Value: UInt64): DelphiString;
begin
  result := Convert.ToString(Int64(Value));
end;

function IntToHex(Value: Integer; Digits: Integer): DelphiString;
begin
  result := Convert.ToHexString(Value, Digits);
end;

function IntToHex(Value: Int64; Digits: Integer): DelphiString;
begin
  result := Convert.ToHexString(Value, Digits);
end;

{$IF NOT COOPER}
function IntToHex(Value: UInt64; Digits: Integer := sizeOf(UInt64) * 2): DelphiString;
begin
  result := Convert.ToHexString(Value, Digits);
end;
{$ENDIF}

function StrToInt(const S: DelphiString): Integer;
begin
  result := S.ToInteger;
end;

function StrToIntDef(const S: DelphiString; aDefault: Integer): Integer;
begin
  try
    result := S.ToInteger;

  except
    result := aDefault;
  end;
end;

function TryStrToInt(const S: DelphiString; out Value: Integer): Boolean;
begin
  try
    Value := S.ToInteger;
    result := true;

  except
    result := false;
  end;
end;

function StrToInt64(const S: DelphiString): Int64;
begin
  result := S.ToInt64;
end;

function StrToInt64Def(const S: DelphiString; const aDefault: Int64): Int64;
begin
  try
    result := S.ToInt64;

  except
    result := aDefault;
  end;
end;

function TryStrToInt64(const S: DelphiString; out Value: Int64): Boolean;
begin
  try
    Value := S.ToInt64;
    result := true;

  except
    result := false;
  end;
end;

function StrToUInt64(const S: DelphiString): UInt64;
begin
  result := S.ToInt64;
end;

function StrToUInt64Def(const S: DelphiString; const aDefault: UInt64): UInt64;
begin
  result := StrToInt64Def(S, aDefault);
end;

function TryStrToUInt64(const S: DelphiString; out Value: UInt64): Boolean;
begin
  try
    Value := S.ToInt64;
    result := true;

  except
    result := false;
  end;  
end;

function StrToBool(const S: DelphiString): Boolean;
begin
  result := S.ToBoolean;
end;

function StrToBoolDef(const S: DelphiString; const aDefault: Boolean): Boolean;
begin
  try
    result := S.ToBoolean;

  except
    result := aDefault;
  end;
end;

function TryStrToBool(const S: DelphiString; out Value: Boolean): Boolean;
begin
  try
    Value := S.ToBoolean;
    result := true;

  except
    result := false;
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean := False): DelphiString;
begin
  result := Convert.ToString(B);
end;

end.