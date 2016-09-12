namespace Elements.RTL.Delphi;

interface

type
  PlatformString = java.lang.String;

  DelphiString = public partial record
  private
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static; partial;
    method InternalCreate: PlatformString; static; partial;
    method InternalCreate(Value: PlatformString): PlatformString; static; partial;
    method GetChar(aIndex: Int32): Char; partial;
    method SetChar(aIndex: Int32; aValue: Char); partial;
    method StringOfChar(Value: Char; Count: Integer): DelphiString;
    method CharIsAnyOf(Value: Char; AnyOf: array of Char): Boolean;
    method ArrayToSplitRegex(Value: array of Char): PlatformString;
    method ArrayToSplitRegex(Value: array of DelphiString): PlatformString;
    method PlatformCharArrayToCharArray(Value: array of Char; StartIndex: Integer := -1; ALength: Integer := -1): TArray<Character>;
    method CreateWithChars(aChar: Char; aCount: Integer): DelphiString; partial; static; 
    method CreateFromArray(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; partial; static; 
   
  public
    method CompareOrdinal(const StrA: DelphiString; const StrB: DelphiString): Integer; static; partial;
    method CompareOrdinal(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer; static; partial;
    method CompareText(const StrA: DelphiString; const StrB: DelphiString): Integer; static; partial;
    method CompareTo(const strB: DelphiString): Integer; partial;
    method Contains(Value: DelphiString): Boolean; partial;
    method Copy(Str: DelphiString): DelphiString; static; partial;
    method CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer); partial;
    method DeQuotedString(QuoteChar: Char): DelphiString; partial;
    method EndsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean; partial;
    method GetHashCode: Integer; partial;
    method IndexOf(Value: Char): Integer; partial;
    method IndexOf(const Value: DelphiString): Integer; partial;
    method IndexOf(Value: Char; StartIndex: Integer): Integer; partial;
    method IndexOf(const Value: DelphiString; StartIndex: Integer): Integer; partial;
    method IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer; partial;
    method IndexOf(const Value: DelphiString; StartIndex: Integer; Count: Integer): Integer; partial;
    method IndexOfAny(const AnyOf: array of Char): Integer; partial;
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer; partial;
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer; partial;
    method Insert(StartIndex: Integer; const Value: DelphiString): DelphiString; partial;
    //method IsDelimiter(const Delimiters: DelphiString; Index: Integer): Boolean;
    method IsEmpty: Boolean; partial;
    method IsNullOrEmpty(const Value: DelphiString): Boolean; static; partial;
    method IsNullOrWhiteSpace(const Value: DelphiString): Boolean; static; partial;
    //method Join(const Separator: DelphiString; const Values: array of const): DelphiString; overload; static;
    method Join(Separator: DelphiString; Values: array of DelphiString): DelphiString; static; partial;
    //method Join(Separator: DelphiString; Values: IEnumerator<DelphiString>): DelphiString; overload; static;
    //method Join(Separator: DelphiString; Values: IEnumerable<DelphiString>): DelphiString; overload; static; inline;
    method Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString; static; partial;
    method LastIndexOf(Value: Char): Integer; partial;
    method LastIndexOf(const Value: DelphiString): Integer; partial;
    method LastIndexOf(Value: Char; StartIndex: Integer): Integer; partial;
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer): Integer; partial;
    method LastIndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer; partial;
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer; Count: Integer): Integer; partial;
    method LastIndexOfAny(const AnyOf: array of Char): Integer; partial;
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer; partial;
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer; partial;
    method PadLeft(TotalWidth: Integer): DelphiString; partial;
    method PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString; partial;
    method PadRight(TotalWidth: Integer): DelphiString; partial;
    method PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString; partial;
    method &Remove(StartIndex: Integer): DelphiString; partial;
    method &Remove(StartIndex: Integer; Count: Integer): DelphiString; partial;
    method Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString; partial;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString; partial;
    method Split(const Separator: array of Char): TArray<DelphiString>; partial;
    method Split(const Separator: array of Char; Count: Integer): TArray<DelphiString>; partial;
    method Split(const Separator: array of Char; Options: TStringSplitOptions): TArray<DelphiString>; partial;
    method Split(const Separator: array of Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>; partial;
    method Split(const Separator: array of DelphiString): TArray<DelphiString>; partial;
    method Split(const Separator: array of DelphiString; Count: Integer): TArray<DelphiString>; partial;
    method Split(const Separator: array of DelphiString; Options: TStringSplitOptions): TArray<DelphiString>; partial;
    method Split(const Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>; partial;

    method ToCharArray: TArray<Char>; partial;
    method ToCharArray(StartIndex: Integer; ALength: Integer): TArray<Char>; partial;
    
    method StartsWith(Value: DelphiString): Boolean; partial;
    method StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean; partial;
    method Substring(StartIndex: Integer): DelphiString; partial;
    method Substring(StartIndex: Integer; ALength: Integer): DelphiString; partial;
    method ToLower: DelphiString; partial;
    //method ToLower(LocaleID: TLocaleID): DelphiString; overload;
    method ToLowerInvariant: DelphiString; partial;
    method ToUpper: DelphiString; partial;
    //method ToUpper(LocaleID: TLocaleID): DelphiString; overload;
    method ToUpperInvariant: DelphiString; partial;
    method Trim: DelphiString; partial;
    method TrimLeft: DelphiString; partial;
    method TrimRight: DelphiString; partial;
    method Trim(const TrimChars: array of Char): DelphiString; partial;
    method TrimLeft(const TrimChars: array of Char): DelphiString; partial;
    method TrimRight(const TrimChars: array of Char): DelphiString; partial;
    method TrimEnd(const TrimChars: array of Char): DelphiString; partial;
    method TrimStart(const TrimChars: array of Char): DelphiString; partial;
  end;


implementation

method DelphiString.InternalCompare(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; LengthA: Integer; LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
var
  LTotalChars: Integer;
begin
  if StrA.Length <= StrB.Length then
    LTotalChars := StrA.Length
  else
    LTotalChars := StrB.Length;

  var Lcollator := java.text.Collator.getInstance; // TODO locale

  if ([TCompareOption.coLingIgnoreCase] in Options) or ([TCompareOption.coIgnoreCase] in Options) then
    Lcollator.setStrength(java.text.Collator.TERTIARY);
      
  if [TCompareOption.coLingIgnoreDiacritic] in Options then
   Lcollator.setStrength(java.text.Collator.PRIMARY);

  result := Lcollator.compare(StrA.SubString(IndexA, LTotalChars), StrB.SubString(IndexB, LTotalChars));
end;

method DelphiString.InternalCreate: PlatformString;
begin
  result := new java.lang.String;
end;

method DelphiString.InternalCreate(Value: PlatformString): PlatformString;
begin
  result := new java.lang.String(Value);
end;

method DelphiString.GetChar(aIndex: Int32): Char;
begin
  result := fData.charAt(aIndex);
end;

method DelphiString.SetChar(aIndex: Int32; aValue: Char);
begin
  fData := fData.substring(0, (aIndex - 1)) + aValue + fData.substring(aIndex + 1);
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := StrA.CompareTo(StrB);
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer;
begin  
  var lStrA := StrA.fData.substring(IndexA, Length);
  result := lStrA.compareTo(StrB.SubString(IndexB, Length));
end;

method DelphiString.CompareText(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := StrA.CompareTo(StrB);
end;

method DelphiString.CompareTo(strB: DelphiString): Integer;
begin
  result := fData.compareTo(strB.fData);
end;

method DelphiString.Contains(Value: DelphiString): Boolean;
begin
  result := fData.indexOf(Value.fData) <> -1;
end;

method DelphiString.Copy(Str: DelphiString): DelphiString;
begin
  result := java.lang.String(Str.fData);
end;

method DelphiString.CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer);
begin
  for i: Integer := 0 to Count - 1 do
    destination[DestinationIndex + i] := fData[SourceIndex + i];    
end;

method DelphiString.DeQuotedString(QuoteChar: Char): DelphiString;
begin  
  if (fData.length >= 2) and (fData.charAt(0) = QuoteChar) and (fData.charAt(fData.length - 1) = QuoteChar) then begin
    var lSb := new StringBuilder;
    var lInQuote: Boolean := false;
    for i: Integer := 1 to fData.length - 2 do begin
      var lChar := fData.charAt(i);
      if lChar = QuoteChar then begin
        if lInQuote then begin
          lInQuote := false;
          lSb.append(lChar);
        end
        else 
          lInQuote := true;
      end
      else begin
        if lInQuote then
          lInQuote := false;
        lSb.append(lChar);
      end;
    end;
    result := lSb.toString;
  end
  else 
    result := self;
end;

method DelphiString.EndsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    result := fData.toUpperCase.endsWith(Value.fData.toUpperCase)
  else
    result := fData.endsWith(Value);
end;

method DelphiString.GetHashCode: Integer;
begin
  result := fData.hashCode;
end;

method DelphiString.IndexOf(Value: Char): Integer;
begin
  result := fData.indexOf(Value);
end;

method DelphiString.IndexOf(Value: DelphiString): Integer;
begin
  result := fData.indexOf(Value);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := fData.indexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.indexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  var lIndex := fData.indexOf(Value, StartIndex);
  if (lIndex >= 0) and (lIndex <= StartIndex + Count) then
    result := lIndex
  else
    result := -1;
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  var lIndex := fData.indexOf(Value, StartIndex);
  if (lIndex >= 0) and (lIndex <= StartIndex + Count) then
    result := lIndex
  else
    result := -1;
end;

method DelphiString.IndexOfAny(AnyOf: array of Char): Integer;
begin
  result := IndexOfAny(AnyOf, 0, fData.length);
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  result := IndexOfAny(AnyOf, StartIndex, fData.length);
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  var lEnd: Integer;
  if (StartIndex + Count) >= fData.length then
    lEnd := fData.length
  else
    lEnd := StartIndex + Count;
  for i: Integer := StartIndex to lEnd - 1 do begin
     for each c: Char in AnyOf do begin
       if fData.charAt(i) = c then
         exit i;
     end;
  end;
  result := -1;
end;

method DelphiString.Insert(StartIndex: Integer; Value: DelphiString): DelphiString;
begin
  var sb := new StringBuilder(fData);
  sb.insert(StartIndex, Value);
  result := sb.toString;
end;

method DelphiString.IsEmpty: Boolean;
begin
  result := fData.isEmpty;
end;

method DelphiString.IsNullOrEmpty(Value: DelphiString): Boolean;
begin
  result := Value.fData.isEmpty;
end;

method DelphiString.IsNullOrWhiteSpace(Value: DelphiString): Boolean;
begin
  result := (Value.fData.isEmpty) or (Value.fData = ' ');
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString): DelphiString;
begin
  result := &Join(Separator, Values, 0, Values.length);
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString;
begin
  var sb := new StringBuilder;
  for i: Integer := StartIndex to (StartIndex + Count) - 1 do begin
     if i <> StartIndex then
      sb.append(Separator);
    sb.append(Values[i]);
  end;
  result := sb.toString;
end;

method DelphiString.LastIndexOf(Value: Char): Integer;
begin
  result := fData.lastIndexOf(Value);
end;

method DelphiString.LastIndexOf(Value: DelphiString): Integer;
begin
  result := fData.lastIndexOf(Value);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := fData.lastIndexOf(Value, StartIndex);
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.lastIndexOf(Value, StartIndex);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.lastIndexOf(Value, (StartIndex - Count));
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.lastIndexOf(Value, (StartIndex - Count));
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char): Integer;
begin
  result := LastIndexOfAny(AnyOf, 0, fData.length); 
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  result := LastIndexOfAny(AnyOf, StartIndex, fData.length);
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  var lStart: Integer;
  if (StartIndex >= 0) and (StartIndex < fData.length) then
    lStart := StartIndex
  else
    lStart := fData.length;
  var lEnd: Integer := lStart - Count + 1;
  if lEnd < 0 then
    lEnd := 0;
  
  for i: Integer := lStart downto lEnd do begin
    var lChar := fData.charAt(i);  
    for each c: Char in AnyOf do
      if lChar = c then
        exit i;
  end;
  result := -1;
end;

method DelphiString.PadLeft(TotalWidth: Integer): DelphiString;
begin
  result := PadLeft(TotalWidth, ' ');
end;

method DelphiString.PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  var lTotal := TotalWidth - fData.length;
  if lTotal < 0 then
    result := self
  else
    result := StringOfChar(PaddingChar, lTotal) + fData;
end;

method DelphiString.PadRight(TotalWidth: Integer): DelphiString;
begin
  result := PadRight(TotalWidth, ' ');
end;

method DelphiString.PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  var lTotal := TotalWidth - fData.length;
  if lTotal < 0 then
    result := self
  else
    result := fData + StringOfChar(PaddingChar, lTotal);
end;

method DelphiString.Remove(StartIndex: Integer): DelphiString;
begin
  result := &Remove(StartIndex, 1);
end;

method DelphiString.Remove(StartIndex: Integer; Count: Integer): DelphiString;
begin
  var lSb := new StringBuilder(fData);
  lSb.delete(StartIndex, Count);
  result := lSb.toString;
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  var lPattern: DelphiString;
  if TReplaceFlags.rfIgnoreCase in ReplaceFlags then
    lPattern := '(?i)' + java.util.regex.Pattern.quote(OldChar);
  if TReplaceFlags.rfReplaceAll in ReplaceFlags then
    result := fData.replaceAll(lPattern, NewChar)
  else
    result := fData.replaceFirst(lPattern, NewChar);
end;

method DelphiString.Replace(OldValue: DelphiString; NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  var lPattern: DelphiString;
  if TReplaceFlags.rfIgnoreCase in ReplaceFlags then
    lPattern := '(?i)' + java.util.regex.Pattern.quote(OldValue);
  if TReplaceFlags.rfReplaceAll in ReplaceFlags then
    result := fData.replaceAll(lPattern, NewValue)
  else
    result := fData.replaceFirst(lPattern, NewValue);
end;

method DelphiString.Split(Separator: array of Char): TArray<DelphiString>;
begin
  result := Split(Separator, -1);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer): TArray<DelphiString>;
begin
  var lSep := ArrayToSplitRegex(Separator);
  var lArray := fData.split(lSep, Count);
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of Char; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lCount: Integer;
  if Options = TStringSplitOptions.ExcludeEmpty then
    lCount := 0
  else
    lCount := -1;
  result := Split(Separator, lCount);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  if Options = TStringSplitOptions.ExcludeEmpty then begin
    var lSep := ArrayToSplitRegex(Separator);
    var lArray := fData.split(lSep, 0);
    result := PlatformArrayToStringArray(lArray, Count);
  end
  else
    result := Split(Separator, Count);
end;

method DelphiString.Split(Separator: array of DelphiString): TArray<DelphiString>;
begin
  result := Split(Separator, -1);
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer): TArray<DelphiString>;
begin
  var lSep := ArrayToSplitRegex(Separator);
  var lArray := fData.split(lSep, Count);
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of DelphiString; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lCount: Integer;
  if Options = TStringSplitOptions.ExcludeEmpty then
    lCount := 0
  else
    lCount := -1;
  result := Split(Separator, lCount);
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  if Options = TStringSplitOptions.ExcludeEmpty then begin
    var lSep := ArrayToSplitRegex(Separator);
    var lArray := fData.split(lSep, 0);
    result := PlatformArrayToStringArray(lArray, Count);
  end
  else
    result := Split(Separator, Count);
end;

method DelphiString.ToCharArray: TArray<Character>;
begin
  var lArray :=  fData.toCharArray;
  result := PlatformCharArrayToCharArray(lArray);
end;

method DelphiString.ToCharArray(StartIndex: Integer; ALength: Integer): TArray<Character>;
begin
  var lArray :=  fData.toCharArray;
  result := PlatformCharArrayToCharArray(lArray, StartIndex, ALength);
end;

method DelphiString.StartsWith(Value: DelphiString): Boolean;
begin
  result := fData.regionMatches(0, Value.fData, 0, fData.length);
end;

method DelphiString.StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  result := fData.regionMatches(IgnoreCase, 0, Value.fData, 0, fData.length)
end;

method DelphiString.Substring(StartIndex: Integer): DelphiString;
begin
  result := fData.substring(StartIndex);
end;

method DelphiString.Substring(StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := fData.substring(StartIndex, StartIndex + ALength);
end;

method DelphiString.ToLower: DelphiString;
begin
  result := fData.toLowerCase;
end;

method DelphiString.ToLowerInvariant: DelphiString;
begin
  result := fData.toLowerCase(java.util.Locale.ENGLISH);
end;

method DelphiString.ToUpper: DelphiString;
begin
  result := fData.toUpperCase;
end;

method DelphiString.ToUpperInvariant: DelphiString;
begin
  result := fData.toUpperCase(java.util.Locale.ENGLISH);
end;

method DelphiString.Trim: DelphiString;
begin
  result := fData.trim;
end;

method DelphiString.TrimLeft: DelphiString;
begin
  result := TrimLeft([' ']);
end;

method DelphiString.TrimRight: DelphiString;
begin
  result := TrimRight([' ']);
end;

method DelphiString.Trim(TrimChars: array of Char): DelphiString;
begin
  var lStr := TrimLeft(TrimChars);
  result := lStr.TrimRight(TrimChars);
end;

method DelphiString.TrimLeft(TrimChars: array of Char): DelphiString;
begin
  if (fData = nil) or (fData.length = 0) then
    exit self;
  var i: Integer := 0;
  while (i <= fData.length) and CharIsAnyOf(fData.charAt(i), TrimChars) do
    inc(i);

  result := fData.substring(i);
end;

method DelphiString.TrimRight(TrimChars: array of Char): DelphiString;
begin
  if (fData = nil) or (fData.length = 0) then
    exit self;

  var i: Integer := fData.length - 1;
  while (i >= 0) and CharIsAnyOf(fData.charAt(i), TrimChars) do
    dec(i);

  result := fData.substring(0, i);
end;

method DelphiString.TrimEnd(TrimChars: array of Char): DelphiString;
begin
  result := TrimRight(TrimChars);
end;

method DelphiString.TrimStart(TrimChars: array of Char): DelphiString;
begin
  result := TrimLeft(TrimChars);
end;

method DelphiString.StringOfChar(Value: Char; Count: Integer): DelphiString;
begin
  var sb := new StringBuilder(Count);
  for i: Integer := 0 to Count - 1 do
    sb.append(Value);
  
  result := sb.toString;
end;

method DelphiString.CharIsAnyOf(Value: Char; AnyOf: array of Char): Boolean;
begin
  for each c: Char in AnyOf do
    if c = Value then
      exit true;

  result := false;
end;

method DelphiString.ArrayToSplitRegex(Value: array of Char): PlatformString;
begin
  result := '';
  for i: Integer := 0 to Value.length - 1 do
    if i <> 0 then
      result := result + '|' + java.util.regex.Pattern.quote(Value[i])
    else 
      result := java.util.regex.Pattern.quote(Value[i])
end;

method DelphiString.ArrayToSplitRegex(Value: array of DelphiString): PlatformString;
begin
  result := '';
  for i: Integer := 0 to Value.length - 1 do
    if i <> 0 then
      result := result + '|' + java.util.regex.Pattern.quote(Value[i])
    else 
      result := java.util.regex.Pattern.quote(Value[i])
end;

method DelphiString.PlatformCharArrayToCharArray(Value: array of Char; StartIndex: Integer := -1; ALength: Integer := -1): TArray<Character>;
begin
  var lBegin: Integer;
  var lTotal: Integer;
  if StartIndex = -1 then
    lBegin := 0
  else
    lBegin := StartIndex;
  if ALength = -1 then
    lTotal := Value.length - 1
  else
    lTotal := ALength;
  result := new Character[lTotal];

  for i: Integer := lBegin to (lBegin + Length) do
    result[i - lBegin] := Value[i];
end;

method DelphiString.CreateWithChars(aChar: Char; aCount: Integer): DelphiString;
begin
  var lChars := new Char[aCount];
  for i: Integer := 0 to aCount-1 do
    lChars[i] := aChar;
  result := new java.lang.String(lChars);
end;

method DelphiString.CreateFromArray(Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString;
begin
  exit new java.lang.String(Value, StartIndex, ALength);
end;

end.
