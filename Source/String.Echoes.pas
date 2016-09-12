namespace Elements.RTL.Delphi;

interface

type
  PlatformString = System.String;

  DelphiString = public partial record
  private
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static; partial;
    method InternalCreate: PlatformString; static; partial;
    method InternalCreate(Value: PlatformString): PlatformString; static; partial;
    method GetChar(aIndex: Int32): Char; partial;
    method SetChar(aIndex: Int32; aValue: Char); partial;
    method CreateWithChars(Char: Char; Count: Integer): DelphiString; partial; static;
    method CreateFromArray(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; partial; static;
   
  public
    method CompareOrdinal(const StrA: DelphiString; const StrB: DelphiString): Integer; static; partial;
    method CompareOrdinal(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer; static; partial;
    method CompareText(const StrA: DelphiString; const StrB: DelphiString): Integer; static; partial;
    method CompareTo(const strB: DelphiString): Integer; partial;
    method Contains(Value: DelphiString): Boolean; partial;
    method Copy(Str: DelphiString): DelphiString; inline; static; partial;
    method CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer); partial;
    method DeQuotedString(QuoteChar: Char): DelphiString; partial;
    method EndsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean; partial;
    method GetHashCode: Integer; partial; override;
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

method DelphiString.Contains(Value: DelphiString): Boolean;
begin
  result := fData.Contains(Value.fData);
end;

method DelphiString.Copy(Str: DelphiString): DelphiString;
begin
  result := System.String.Copy(Str);
end;

method DelphiString.DeQuotedString(QuoteChar: Char): DelphiString;
begin
  result := fData.Trim(QuoteChar);
end;

method DelphiString.EndsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    result := fData.EndsWith(Value, StringComparison.OrdinalIgnoreCase)
  else
    result := fData.EndsWith(Value);
end;

method DelphiString.GetHashCode: Integer;
begin
  result := fData.GetHashCode;
end;

method DelphiString.IndexOf(Value: Char): Integer;
begin
  result := fData.IndexOf(Value);
end;

method DelphiString.IndexOf(Value: DelphiString): Integer;
begin
  result := fData.IndexOf(Value);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := IndexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.IndexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.IndexOf(Value, StartIndex, Count);
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.IndexOf(Value, StartIndex, Count);
end;

method DelphiString.IndexOfAny(AnyOf: array of Char): Integer;
begin
  result := fData.IndexOfAny(AnyOf);
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  result := fData.IndexOfAny(AnyOf, StartIndex);
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.IndexOfAny(AnyOf, StartIndex, Count);
end;

method DelphiString.Insert(StartIndex: Integer; Value: DelphiString): DelphiString;
begin
  result := fData.Insert(StartIndex, Value);
end;

method DelphiString.IsEmpty: Boolean;
begin
  result := fData.IsNullOrEmpty(self);
end;

method DelphiString.IsNullOrEmpty(Value: DelphiString): Boolean;
begin
  result := System.String.IsNullOrEmpty(Value);
end;

method DelphiString.IsNullOrWhiteSpace(Value: DelphiString): Boolean;
begin
  result := System.String.IsNullOrWhiteSpace(Value);
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString): DelphiString;
begin
  result := System.String.Join(Separator, Values);
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString;
begin
  result := System.String.Join(Separator, Values, StartIndex, Count);
end;

method DelphiString.LastIndexOf(Value: Char): Integer;
begin
  result := fData.LastIndexOf(Value);
end;

method DelphiString.LastIndexOf(Value: DelphiString): Integer;
begin
  result := fData.LastIndexOf(Value);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := fData.LastIndexOf(Value, StartIndex);
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.LastIndexOf(Value, StartIndex);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.LastIndexOf(Value, StartIndex, Count);
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.LastIndexOf(Value, StartIndex, Count);
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char): Integer;
begin
  result := fData.LastIndexOfAny(AnyOf);
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  result := fData.LastIndexOfAny(AnyOf, StartIndex);
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := fData.LastIndexOfAny(AnyOf, StartIndex, Count);
end;

method DelphiString.PadLeft(TotalWidth: Integer): DelphiString;
begin
  result := fData.PadLeft(TotalWidth);
end;

method DelphiString.PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  result := fData.PadLeft(TotalWidth, PaddingChar);
end;

method DelphiString.PadRight(TotalWidth: Integer): DelphiString;
begin
  result := fData.PadRight(TotalWidth);
end;

method DelphiString.PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  result := fData.PadRight(TotalWidth, PaddingChar);
end;

method DelphiString.Remove(StartIndex: Integer): DelphiString;
begin
  result := fData.Remove(StartIndex);
end;

method DelphiString.Remove(StartIndex: Integer; Count: Integer): DelphiString;
begin
  result := fData.Remove(StartIndex, Count);
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  result := Replace(String(OldChar), String(NewChar), ReplaceFlags);
end;

method DelphiString.Replace(OldValue: DelphiString; NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  var lOptions: System.Text.RegularExpressions.RegexOptions := 0;
  var lCount: Integer := 1;
  if [TReplaceFlags.rfIgnoreCase] in ReplaceFlags then
    lOptions := System.Text.RegularExpressions.RegexOptions.IgnoreCase;
  if [TReplaceFlags.rfReplaceAll] in ReplaceFlags then
    lCount := -1;

  var lRegEx := new System.Text.RegularExpressions.Regex(OldValue.fData, lOptions);
  result := lRegEx.Replace(fData, System.Text.RegularExpressions.Regex.Escape(NewValue.fData), lCount);
end;

method DelphiString.StartsWith(Value: DelphiString): Boolean;
begin
  result := fData.StartsWith(Value);
end;

method DelphiString.StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    result := fData.StartsWith(Value, StringComparison.OrdinalIgnoreCase)
  else 
    result := fData.StartsWith(Value);
end;

method DelphiString.Substring(StartIndex: Integer): DelphiString;
begin
  result := fData.Substring(StartIndex);
end;

method DelphiString.Substring(StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := fData.Substring(StartIndex, ALength);
end;

method DelphiString.ToLower: DelphiString;
begin
  result := fData.ToLower;
end;

method DelphiString.ToLowerInvariant: DelphiString;
begin
  result := fData.ToLowerInvariant;
end;

method DelphiString.ToUpper: DelphiString;
begin
  result := fData.ToUpper;
end;

method DelphiString.ToUpperInvariant: DelphiString;
begin
  result := fData.ToUpperInvariant;
end;

method DelphiString.Trim: DelphiString;
begin
  result := fData.Trim;
end;

method DelphiString.TrimLeft: DelphiString;
begin
  result := self.TrimLeft([' ']);
end;

method DelphiString.TrimRight: DelphiString;
begin
  result := self.TrimRight([' ']);
end;

method DelphiString.Trim(TrimChars: array of Char): DelphiString;
begin
  result := fData.Trim(TrimChars);
end;

method DelphiString.TrimLeft(TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimStart(TrimChars);
end;

method DelphiString.TrimRight(TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimEnd(TrimChars);
end;

method DelphiString.TrimEnd(TrimChars: array of Char): DelphiString;
begin
  result := self.TrimEnd(TrimChars);
end;

method DelphiString.TrimStart(TrimChars: array of Char): DelphiString;
begin
  result := self.TrimStart(TrimChars);
end;

method DelphiString.GetChar(aIndex: Int32): Char;
begin
  result := fData.Chars[aIndex];
end;

method DelphiString.SetChar(aIndex: Int32; aValue: Char);
begin
  fData := fData.Substring(0, aIndex - 1) + aValue + fData.Substring(aIndex + 1);
end;

method DelphiString.InternalCreate: PlatformString;
begin
  result := new System.String([]);  
end;

method DelphiString.InternalCreate(Value: PlatformString): PlatformString;
begin
  result := System.String.Copy(Value);
end;

method DelphiString.Split(Separator: array of Char): TArray<DelphiString>;
begin
  var lArray := fData.Split(Separator);
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer): TArray<DelphiString>;
begin
  var lArray := fData.Split(Separator, Count);
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of Char; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lArray: array of PlatformString;
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := fData.Split(Separator, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := fData.Split(Separator);
    
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>;
begin  
  var lArray: array of PlatformString;  
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := fData.Split(Separator, Count, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := fData.Split(Separator, Count);
    
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of DelphiString): TArray<DelphiString>;
begin
  var lSep := StringArrayToPlatformArray(Separator);
  var lArray := fData.Split(lSep, StringSplitOptions.None);
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer): TArray<DelphiString>;
begin
  var lSep := StringArrayToPlatformArray(Separator);
  var lArray := fData.Split(lSep, Count, StringSplitOptions.None);
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of DelphiString; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lArray: array of PlatformString;
  var lSep := StringArrayToPlatformArray(Separator);
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := fData.Split(lSep, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := fData.Split(lSep, StringSplitOptions.None);  
    
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lArray: array of PlatformString;
  var lSep := StringArrayToPlatformArray(Separator);
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := fData.Split(lSep, Count, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := fData.Split(lSep, Count, StringSplitOptions.None);
    
  result := PlatformArrayToStringArray(lArray);
end;

method DelphiString.ToCharArray: TArray<Char>;
begin
  result := fData.ToCharArray;
end;

method DelphiString.ToCharArray(StartIndex: Integer; ALength: Integer): TArray<Char>;
begin
  result := fData.ToCharArray(StartIndex, ALength);
end;

method DelphiString.InternalCompare(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; LengthA: Integer; LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
var  
  LOptions: System.Globalization.CompareOptions;
  LTotalChars: Integer;
begin
  LOptions := 0;
  if [TCompareOption.coLingIgnoreCase] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreCase;
  if [TCompareOption.coLingIgnoreDiacritic] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreNonSpace;
  if [TCompareOption.coIgnoreCase] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.OrdinalIgnoreCase;
  if [TCompareOption.coIgnoreKanatype] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreKanaType;
  if [TCompareOption.coIgnoreNonSpace] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreNonSpace;  
  if [TCompareOption.coIgnoreSymbols] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreSymbols;
  if [TCompareOption.coIgnoreWidth] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreWidth;
  if [TCompareOption.coDigitAsNumbers] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.StringSort;
  if [TCompareOption.coStringSort] in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.StringSort;

  if StrA.Length <= StrB.Length then
    LTotalChars := StrA.Length
  else
    LTotalChars := StrB.Length;

  result := System.String.Compare(StrA, IndexA, StrB, IndexB, LTotalChars, nil, LOptions);
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := System.String.CompareOrdinal(StrA, StrB);
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer;
begin
  result := System.String.CompareOrdinal(StrA, IndexA, StrB, IndexB, ALength);
end;

method DelphiString.CompareText(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := System.String.Compare(StrA, StrB, StringComparison.OrdinalIgnoreCase);
end;

method DelphiString.CompareTo(strB: DelphiString): Integer;
begin
  result := System.String.CompareOrdinal(fData, strB.fData);
end;

method DelphiString.CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer);
begin
  fData.CopyTo(SourceIndex, destination, DestinationIndex, Count);
end;

method DelphiString.CreateWithChars(Char: Char; Count: Integer): DelphiString;
begin
  result := new System.String(Char, Count);  
end;

method DelphiString.CreateFromArray(Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := new System.String(Value, StartIndex, ALength);  
end;

end.
