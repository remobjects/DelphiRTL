namespace RTL2.Delphi;

interface

uses
  Foundation;

type
  DelphiString = public partial record
  private
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static; partial;
    method InternalIndexOfAny(Value: array of DelphiString; StartIndex: Integer; var CurrentLength: Integer): Integer;
    method InternalCreate: PlatformString; static; partial;
    method InternalCreate(Value: PlatformString): PlatformString; static; partial;
    method GetChar(aIndex: Int32): Char; partial;
    method SetChar(aIndex: Int32; aValue: Char); partial;
    method CreateWithChars(Char: Char; Count: Integer): DelphiString; partial; static; 
    method CreateFromArray(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; partial; static; 
    method NSArrayToStringArray(Value: NSArray; Count: Integer): array of DelphiString;

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
    method Split(const Separator: array of Char): TArray<PlatformString>; partial;
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

method DelphiString.InternalCreate: PlatformString;
begin
  result := NSString.string;
end;

method DelphiString.InternalCreate(Value: PlatformString): PlatformString;
begin
  result := NSString.stringWithString(Value);
end;

method DelphiString.InternalCompare(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; LengthA: Integer; LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
var  
  lOptions: NSStringCompareOptions;
  lTotalChars: Integer;
begin
  lOptions := 0;
  if ([TCompareOption.coLingIgnoreCase] in Options) or ([TCompareOption.coIgnoreCase] in Options) then
    lOptions := lOptions or NSStringCompareOptions.NSCaseInsensitiveSearch;
  if [TCompareOption.coLingIgnoreDiacritic] in Options then
    lOptions := lOptions or NSStringCompareOptions.NSDiacriticInsensitiveSearch;
  if [TCompareOption.coIgnoreWidth] in Options then
    lOptions := lOptions or NSStringCompareOptions.NSWidthInsensitiveSearch;
  if [TCompareOption.coDigitAsNumbers] in Options then
    lOptions := lOptions or NSStringCompareOptions.NSNumericSearch;
  if [TCompareOption.coStringSort] in Options then
    lOptions := lOptions or NSStringCompareOptions.NSForcedOrderingSearch;

  if StrA.Length <= StrB.Length then
    lTotalChars := StrA.Length
  else
    lTotalChars := StrB.Length;

  { create new strings only if needed }
  var lStrA: NSString;
  if (IndexA <> 0) or (LengthA <> StrA.Length) then
    lStrA := StrA.fData.substringWithRange(NSMakeRange(IndexA, LengthA))
  else
    lStrA := StrA.fData;
    
  var lStrB: NSString;
  if (IndexB <> 0) or (LengthB <> StrB.Length) then
    lStrB := StrB.fData.substringWithRange(NSMakeRange(IndexB, LengthB))
  else
    lStrB := StrB.fData;

  result := lStrA.compare(lStrB) options(lOptions) range(NSMakeRange(0, lTotalChars));
end;

method DelphiString.GetChar(aIndex: Int32): Char;
begin
  result := fData.characterAtIndex(aIndex);
end;

method DelphiString.SetChar(aIndex: Int32; aValue: Char);
begin
  fData := fData.substringWithRange(NSMakeRange(0, aIndex - 1)) + aValue + fData.substringWithRange(NSMakeRange(aIndex + 1, fData.length - 1));
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := StrA.fData.compare(StrB.fData) options(NSStringCompareOptions.NSLiteralSearch);
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer;
begin
  var lStrA := StrA.fData.substringWithRange(NSMakeRange(IndexA, Length));
  var lStrB := StrB.fData.substringWithRange(NSMakeRange(IndexB, Length));
  result := lStrA.compare(lStrB) options(NSStringCompareOptions.NSLiteralSearch);
end;

method DelphiString.CompareText(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := StrA.fData.caseInsensitiveCompare(StrB.fData);
end;

method DelphiString.CompareTo(strB: DelphiString): Integer;
begin
  result := fData.compare(strB.fData) options(NSStringCompareOptions.NSLiteralSearch);
end;

method DelphiString.Contains(Value: DelphiString): Boolean;
begin
  var lRange := fData.rangeOfString(Value.fData) options(NSStringCompareOptions.NSLiteralSearch);
  result := (lRange.location <> NSNotFound);
end;

method DelphiString.Copy(Str: DelphiString): DelphiString;
begin
  result := new NSString(Str.fData);
end;

method DelphiString.CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer);
begin
  fData.getCharacters(@destination[DestinationIndex]) range(NSMakeRange(SourceIndex, Count));
end;

method DelphiString.DeQuotedString(QuoteChar: Char): DelphiString;
begin
  if (fData.length >= 2) and (fData.characterAtIndex(0) = QuoteChar) and (fData.characterAtIndex(fData.length - 1) = QuoteChar) then begin
    var lString := new NSMutableString(fData.length - 2);
    var lInQuote: Boolean := false;
    for i: Integer := 1 to fData.length - 2 do begin
      var lChar := fData.characterAtIndex(i);
      if lChar = QuoteChar then begin
        if lInQuote then begin
          lInQuote := false;
          lString.appendString(NSString(lChar));
        end
        else 
          lInQuote := true;
      end
      else begin
        if lInQuote then
          lInQuote := false;
        lString.appendString(NSString(lChar));
      end;
    end;
    result := NSString(lString);
  end
  else 
    result := self;
end;

method DelphiString.EndsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  if Value.Length > fData.length then
    result := false
  else begin
    var lOptions: NSStringCompareOptions := 0;
    if IgnoreCase then 
      lOptions := NSStringCompareOptions.NSCaseInsensitiveSearch;
    result := (fData.compare(Value.fData) options(lOptions) range(NSMakeRange(fData.length - Value.fData.length + 1, fData.length - 1)) = NSComparisonResult.NSOrderedSame);
  end;
end;

method DelphiString.GetHashCode: Integer;
begin
  result := fData.hash;
end;

method DelphiString.IndexOf(Value: Char): Integer;
begin
  result := IndexOf(NSString(Value));
end;

method DelphiString.IndexOf(Value: DelphiString): Integer;
begin
  var lRange:= fData.rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch);
  if lRange.location <> NSNotFound then
    result := lRange.location
  else
    result := -1;
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := IndexOf(NSString(Value), StartIndex, fData.length - StartIndex);
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := IndexOf(Value, StartIndex, fData.length - StartIndex);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := IndexOf(NSString(Value), StartIndex, Count);
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  var lRange:= fData.rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch) range(NSMakeRange(StartIndex, Count));
  if lRange.location <> NSNotFound then
    result := lRange.location
  else
    result := -1;
end;

method DelphiString.IndexOfAny(AnyOf: array of Char): Integer;
begin
  result := IndexOfAny(AnyOf, 0, fData.length);  
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  result := IndexOfAny(AnyOf, StartIndex, fData.length - StartIndex);
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  var lChars := NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@AnyOf) length(AnyOf.length));
  var lRange := fData.rangeOfCharacterFromSet(lChars) options(NSStringCompareOptions.NSLiteralSearch) range(NSMakeRange(StartIndex, Count));
  if lRange.location <> NSNotFound then
    result := lRange.location
  else
    result := -1;
end;

method DelphiString.Insert(StartIndex: Integer; Value: DelphiString): DelphiString;
begin
  if StartIndex > 0 then
    result := fData.substringToIndex(StartIndex - 1) + Value.fData + fData.substringFromIndex(StartIndex)
  else
    result := Value.fData + fData.substringFromIndex(StartIndex);
end;

method DelphiString.IsEmpty: Boolean;
begin
  result := fData.length = 0;
end;

method DelphiString.IsNullOrEmpty(Value: DelphiString): Boolean;
begin
  result := Value.fData.length = 0;
end;

method DelphiString.IsNullOrWhiteSpace(Value: DelphiString): Boolean;
begin
  result := (Value.fData.length = 0) or (Value.fData = ' ');
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString): DelphiString;
begin
  result := &Join(Separator, Values, 0, Values.length);
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString;
begin
  var lArray := new NSMutableArray(Values.length);
  for i: Integer := 0 to Values.length - 1 do
    lArray.addObject(Values[i].fData);

  result := lArray.componentsJoinedByString(Separator.fData);
end;

method DelphiString.LastIndexOf(Value: Char): Integer;
begin
  result := LastIndexOf(NSString(Value));
end;

method DelphiString.LastIndexOf(Value: DelphiString): Integer;
begin
  var lRange:= fData.rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch);
  if lRange.location <> NSNotFound then
    result := lRange.location
  else
    result := -1;  
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := LastIndexOf(NSString(Value), StartIndex);
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := LastIndexOf(Value, StartIndex, StartIndex + 1);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  result := LastIndexOf(NSString(Value), StartIndex, Count);
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  var lRange:= fData.rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch) range(NSMakeRange(StartIndex, Count));
  if lRange.location <> NSNotFound then
    result := lRange.location
  else
    result := -1;  
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char): Integer;
begin
  result := LastIndexOfAny(AnyOf, fData.length - 1, fData.length);
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  result := LastIndexOfAny(AnyOf, StartIndex, StartIndex + 1);
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  var lChars := NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@AnyOf) length(AnyOf.length));
  var lRange := fData.rangeOfCharacterFromSet(lChars) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch) range(NSMakeRange(StartIndex, Count));
  if lRange.location <> NSNotFound then
    result := lRange.location
  else
    result := -1;
end;

method DelphiString.PadLeft(TotalWidth: Integer): DelphiString;
begin
  result := PadLeft(TotalWidth, ' ');
end;

method DelphiString.PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  result := fData.stringByPaddingToLength(TotalWidth) withString(PaddingChar) startingAtIndex(0);
end;

method DelphiString.PadRight(TotalWidth: Integer): DelphiString;
begin
  result := PadRight(TotalWidth, ' ');
end;

method DelphiString.PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  result := fData.stringByPaddingToLength(TotalWidth) withString(PaddingChar) startingAtIndex(fData.length);
end;

method DelphiString.Remove(StartIndex: Integer): DelphiString;
begin
  result := &Remove(StartIndex, 1);
end;

method DelphiString.Remove(StartIndex: Integer; Count: Integer): DelphiString;
begin
  fData := fData.substringWithRange(NSMakeRange(0, StartIndex - 1)) + fData.substringWithRange(NSMakeRange(StartIndex + Count, fData.length - 1));
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  result := Replace(Foundation.NSString.stringWithFormat("%c", OldChar), Foundation.NSString.stringWithFormat("%c", NewChar), ReplaceFlags); 
end;

method DelphiString.Replace(OldValue: DelphiString; NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString;
var 
  lOptions: NSStringCompareOptions := 0;
  lRange: NSRange;
begin
  if [TReplaceFlags.rfIgnoreCase] in ReplaceFlags then
    lOptions := NSStringCompareOptions.NSCaseInsensitiveSearch;
  if not ([TReplaceFlags.rfReplaceAll] in ReplaceFlags) then begin
    lRange := fData.rangeOfString(OldValue) options(lOptions);
    if lRange.location = NSNotFound then
      exit self;   
  end
  else
    lRange := NSMakeRange(0, fData.length - 1);
  result := fData.stringByReplacingOccurrencesOfString(OldValue.fData) withString(NewValue.fData) options(lOptions) range(lRange)
end;

method DelphiString.Split(Separator: array of Char): TArray<PlatformString>;
begin
  result := Split(Separator, -1, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer): TArray<DelphiString>;
begin
  result := Split(Separator, Count, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of Char; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  result := Split(Separator, -1, Options);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lChars := NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@Separator) length(Separator.length));
  var lArray := fData.componentsSeparatedByCharactersInSet(lChars);
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := lArray.filteredArrayUsingPredicate(NSPredicate.predicateWithFormat('length > 0'));

  //result := PlatformArrayToStringArray(lArray, Count);   // TODO compiler error only Toffee
end;

method DelphiString.Split(Separator: array of DelphiString): TArray<DelphiString>;
begin
  result := Split(Separator, -1, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer): TArray<DelphiString>;
begin
  result := Split(Separator, Count, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of DelphiString; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  result := Split(Separator, -1, Options);
end;

method DelphiString.InternalIndexOfAny(Value: array of DelphiString; StartIndex: Integer; var CurrentLength: Integer): Integer;
begin
  result := -1;
  for i: Integer := 0 to Value.length - 1 do begin
    var lIndex := IndexOf(Value[i], StartIndex);
    if (lIndex >= 0) and ((lIndex < result) or (result = -1)) then begin
      result := lIndex;
      CurrentLength := Value[i].Length;
    end;
  end;
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>;
begin
  var lTotal := 0;
  var lIndex := -1;
  var lArray := new NSMutableArray(10);
  var lCurrentLength := 0;
  var lStartIndex := 0;

  lIndex := InternalIndexOfAny(Separator, lStartIndex, var lCurrentLength);
  while (lIndex >= 0) and (lTotal < Count) do
  begin
    lStartIndex := lIndex + lCurrentLength;
    var lStr := Substring(lStartIndex, lIndex - lStartIndex);
    if (lStr.IsEmpty) or ((lStr.IsEmpty) and (Options <> TStringSplitOptions.ExcludeEmpty)) then 
      lArray.addObject(lStr.fData);
    lIndex := InternalIndexOfAny(Separator, lStartIndex, var lCurrentLength);
  end;

  if (lStartIndex < Length) and (lTotal < Count) then
    lArray.addObject(Substring(lStartIndex));

  //result := NSArrayToStringArray(lArray, Count);   // TODO compiler error only Toffee
end;

method DelphiString.ToCharArray: TArray<Char>;
begin
  result := ToCharArray(0, fData.length);
end;

method DelphiString.ToCharArray(StartIndex: Integer; ALength: Integer): TArray<Char>;
begin
  //result := new Char[ALength]; // TODO compiler error only Toffee
  var lArray := new Char[ALength];
   fData.getCharacters(lArray) range(NSMakeRange(StartIndex, ALength));

  for i: Integer := 0 to lArray.length do
    result[i] := lArray[i];
end;

method DelphiString.StartsWith(Value: DelphiString): Boolean;
begin
  result := fData.hasPrefix(Value.fData);
end;

method DelphiString.StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  if Value.Length > fData.length then
    result := false
  else begin
    var lOptions: NSStringCompareOptions := 0;
    if IgnoreCase then 
      lOptions := NSStringCompareOptions.NSCaseInsensitiveSearch;
    result := (fData.compare(Value.fData) options(lOptions) range(NSMakeRange(0, Value.fData.length)) = NSComparisonResult.NSOrderedSame);
  end;
end;

method DelphiString.Substring(StartIndex: Integer): DelphiString;
begin
  result := fData.substringFromIndex(StartIndex);
end;

method DelphiString.Substring(StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := fData.substringWithRange(NSMakeRange(StartIndex, StartIndex + ALength - 1));
end;

method DelphiString.ToLower: DelphiString;
begin
  result := fData.lowercaseString;
end;

method DelphiString.ToLowerInvariant: DelphiString;
begin
  result := fData.lowercaseStringWithLocale(Sugar.Locale.Invariant)
end;

method DelphiString.ToUpper: DelphiString;
begin
  result := fData.uppercaseString;
end;

method DelphiString.ToUpperInvariant: DelphiString;
begin
  result := fData.uppercaseStringWithLocale(Sugar.Locale.Invariant);
end;

method DelphiString.Trim: DelphiString;
begin
  result := fData.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceCharacterSet);
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
  result := fData.stringByTrimmingCharactersInSet(NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@TrimChars) length(TrimChars.length)));
end;

method DelphiString.TrimLeft(TrimChars: array of Char): DelphiString;
begin
  var lCharacters := NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@TrimChars) length(TrimChars.length));
  var lFirstWanted := fData.rangeOfCharacterFromSet(lCharacters.invertedSet);
  
  if lFirstWanted.location = NSNotFound then
    result := self
  else
    result := fData.substringFromIndex(lFirstWanted.location);
end;

method DelphiString.TrimRight(TrimChars: array of Char): DelphiString;
begin
  var lCharacters := NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@TrimChars) length(TrimChars.length));
  var lLastWanted := fData.rangeOfCharacterFromSet(lCharacters.invertedSet) options(NSStringCompareOptions.NSBackwardsSearch);
                                                               
  if lLastWanted.location = NSNotFound then
    result := self
  else
    result := fData.substringToIndex(lLastWanted.location + 1);
end;

method DelphiString.TrimEnd(TrimChars: array of Char): DelphiString;
begin
  result := TrimRight(TrimChars);
end;

method DelphiString.TrimStart(TrimChars: array of Char): DelphiString;
begin
  result := TrimLeft(TrimChars);
end;

method DelphiString.CreateWithChars(Char: Char; Count: Integer): DelphiString;
begin
  result := Foundation.NSString("").stringByPaddingToLength(Count) withString(Foundation.NSString.stringWithFormat("%c", Char)) startingAtIndex(0);  
end;

method DelphiString.CreateFromArray(Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := new Foundation.NSString withCharacters(@Value[StartIndex]) length(ALength);
end;

method DelphiString.NSArrayToStringArray(Value: NSArray; Count: Integer): array of DelphiString;
begin
  result := new DelphiString[Value.count];
  for i: Integer := 0 to Count - 1 do
    result[i] := NSString(Value[i]);
end;

end.
