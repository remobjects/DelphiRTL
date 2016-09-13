namespace Elements.RTL.Delphi;

interface

type
  TCompareOption = (coLingIgnoreCase, coLingIgnoreDiacritic, coIgnoreCase,
    coIgnoreKanatype, coIgnoreNonSpace, coIgnoreSymbols, coIgnoreWidth,
    coLingCasing, coDigitAsNumbers, coStringSort) of Integer;
  TCompareOptions = public set of TCompareOption;
  TLocaleOptions = public (loInvariantLocale, loUserLocale);
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);
  TArray<T> = array of T;
  TStringSplitOptions = public (None, ExcludeEmpty);
  
  TLocaleID = public class
  private
    fWrap: Sugar.Locale;
  end;

  //76137: Delphi RTL: `DefaultStringType` needs to support records
  {$IF NOT NOUGAT AND NOT ISLAND}
  [assembly:DefaultStringType("Elements.RTL.Delphi", typeOf(Elements.RTL.Delphi.WideString))]
  {$ENDIF}

  WideString = public DelphiString;

  DelphiString = public partial record
  private
    fData: PlatformString;
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; partial; static; empty;
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static;

    method InternalCreate: PlatformString; static; partial; empty;
    method InternalCreate(Value: PlatformString): PlatformString; static; partial; empty;
    method GetChar(aIndex: Int32): Char; partial; empty;
    method SetChar(aIndex: Int32; aValue: Char); partial; empty;
    method GetOffsetChar(aIndex: Int32): Char;
    method SetOffsetChar(aIndex: Int32; aValue: Char);
    
    class var fOffset: Integer := 1;
    class method SetOffset(aOffset: Integer);
    begin
      if aOffset not in [0,1] then raise new Exception("Delphi String offset must be 0 or 1");
      fOffset := aOffset;
    end;
    
    method GetLength: Integer;
    method CreateWithChars(Char: Char; Count: Integer): DelphiString; partial; static; empty;
    method CreateFromArray(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; partial; static; empty;
    method PlatformArrayToStringArray(Value: array of PlatformString; Count: Integer := -1): TArray<DelphiString>; static;
    method StringArrayToPlatformArray(Value: TArray<DelphiString>): array of PlatformString; static;
  public
    constructor;
    constructor(Value: PlatformString); 
    method Create(C: Char; Count: Integer): DelphiString; static;
    method Create(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; partial; static;
    method Create(const Value: array of Char): DelphiString; partial; static;
    class operator Implicit(Value: Char): DelphiString;
    class operator Implicit(Value: PlatformString): DelphiString;
    class operator Implicit(Value: DelphiString): Sugar.String;
    class operator &Add(Value1: DelphiString; Value2: Char): DelphiString;
    class operator &Add(Value1: Char; Value2: DelphiString): DelphiString;
    class operator Equal(Value1, Value2: DelphiString): Boolean;
    class property Offset: Integer read fOffset write SetOffset;
    
    [ToString]
    method ToString: PlatformString;
    begin
      result := fData;
    end;
    
    method Compare(const StrA: DelphiString; const StrB: DelphiString): Integer; static;    
    method Compare(const StrA: DelphiString; const StrB: DelphiString; LocaleID: TLocaleID): Integer; static;
    method Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean): Integer; static; 
    method Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static; 
    method Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions): Integer; static;
    method Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static;
    method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer; static; 
    method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; LocaleID: TLocaleID): Integer; static;
    method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean): Integer; static; 
    method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static;
    method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions): Integer; static;    
    method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static;    
    method CompareOrdinal(const StrA: DelphiString; const StrB: DelphiString): Integer; static; partial; empty;
    method CompareOrdinal(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; Length: Integer): Integer; static; partial; empty;
    method CompareText(const StrA: DelphiString; const StrB: DelphiString): Integer; static; partial; empty;
    
    method Parse(const Value: Integer): DelphiString; static;
    method Parse(const Value: Int64): DelphiString; static; 
    method Parse(const Value: Boolean): DelphiString; static; 
    method Parse(const Value: Extended): DelphiString; static; 
    method ToBoolean(const S: DelphiString): Boolean; static; 
    method ToInteger(const S: DelphiString): Integer; static; 
    method ToInt64(const S: DelphiString): Int64; static; 
    method ToSingle(const S: DelphiString): Double; static; 
    method ToDouble(const S: DelphiString): Double; static; 
    method ToExtended(const S: DelphiString): Double; static; // TODO check, Extended and single -->Double
    method LowerCase(const S: DelphiString): DelphiString; static; partial; empty;
    method LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; static; partial; empty;
    method UpperCase(const S: DelphiString): DelphiString; static; partial; empty;
    method UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; static; partial; empty;
    method CompareTo(const strB: DelphiString): Integer; partial; empty;
    method Contains(const Value: DelphiString): Boolean; partial; empty;
    method Copy(const Str: DelphiString): DelphiString; inline; static; partial; empty;
    method CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer); partial; empty;
    method CountChar(const C: Char): Integer;     
    method DeQuotedString: DelphiString;
    method DeQuotedString(const QuoteChar: Char): DelphiString; partial; empty;    
    // method EndsText(const ASubText, AText: DelphiString): Boolean; static;  // TODO    
    method EndsWith(const Value: DelphiString): Boolean; inline;
    method EndsWith(const Value: DelphiString; IgnoreCase: Boolean): Boolean; partial; empty;    
    method Equals(const Value: DelphiString): Boolean;
    method Equals(const a: DelphiString; const b: DelphiString): Boolean; static;    
    //method Format(const Format: DelphiString; const args: array of const): DelphiString; overload; static; // TODO    
    //method GetHashCode: Integer; partial; empty;    
    method IndexOf(value: Char): Integer; partial; empty;
    method IndexOf(const Value: DelphiString): Integer; partial; empty;
    method IndexOf(Value: Char; StartIndex: Integer): Integer; partial; empty;
    method IndexOf(const Value: DelphiString; StartIndex: Integer): Integer; partial; empty;
    method IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer; partial; empty;
    method IndexOf(const Value: DelphiString; StartIndex: Integer; Count: Integer): Integer; partial; empty;    
    method IndexOfAny(const AnyOf: array of Char): Integer; partial; empty;
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer; partial; empty;
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer; partial; empty;    
    //method IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char): Integer;
    //method IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: Integer): Integer;
    //method IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: Integer; Count: Integer): Integer;    
    method Insert(StartIndex: Integer; const Value: DelphiString): DelphiString; partial; empty;
    //method IsDelimiter(const Delimiters: DelphiString; Index: Integer): Boolean; partial; empty;
    method IsEmpty: Boolean; partial; empty;    
    method IsNullOrEmpty(const Value: DelphiString): Boolean; static; partial; empty;
    method IsNullOrWhiteSpace(const Value: DelphiString): Boolean; static; partial; empty;    
    //method Join(const Separator: DelphiString; const Values: array of const): DelphiString; overload; static;
    method Join(Separator: DelphiString; Values: array of DelphiString): DelphiString; static; partial; empty;
    //method Join(Separator: DelphiString; Values: IEnumerator<DelphiString>): DelphiString; overload; static;
    //method Join(Separator: DelphiString; Values: IEnumerable<DelphiString>): DelphiString; overload; static; inline;
    method Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString; static; partial; empty;
    //method LastDelimiter(const Delims: DelphiString): Integer;
    method LastIndexOf(Value: Char): Integer; partial; empty;
    method LastIndexOf(const Value: DelphiString): Integer; partial; empty;
    method LastIndexOf(Value: Char; StartIndex: Integer): Integer; partial; empty;
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer): Integer; partial; empty;
    method LastIndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer; partial; empty;
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer; Count: Integer): Integer; partial; empty;
    method LastIndexOfAny(const AnyOf: array of Char): Integer; partial; empty;
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer; partial; empty;
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer; partial; empty;    
    method PadLeft(TotalWidth: Integer): DelphiString; partial; empty;
    method PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString; partial; empty;
    method PadRight(TotalWidth: Integer): DelphiString; partial; empty;
    method PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString; partial; empty;    
    method QuotedString: DelphiString;
    method QuotedString(const QuoteChar: Char): DelphiString;
    method &Remove(StartIndex: Integer): DelphiString; partial; empty;
    method &Remove(StartIndex: Integer; Count: Integer): DelphiString; partial; empty; 
    method Replace(OldChar: Char; NewChar: Char): DelphiString;
    method Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString; partial; empty;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString): DelphiString;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString; partial; empty;
    
    method Split(const Separator: array of Char): TArray<DelphiString>; partial; empty;     
    method Split(const Separator: array of Char; Count: Integer): TArray<DelphiString>; partial; empty;
    method Split(const Separator: array of Char; Options: TStringSplitOptions): TArray<DelphiString>; partial; empty;
    method Split(const Separator: array of Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>; partial; empty;
    method Split(const Separator: array of DelphiString): TArray<DelphiString>; partial; empty;
    method Split(const Separator: array of DelphiString; Count: Integer): TArray<DelphiString>; partial; empty;
    method Split(const Separator: array of DelphiString; Options: TStringSplitOptions): TArray<DelphiString>; partial; empty;
    method Split(const Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>; partial; empty;

    /*
    method Split(const Separator: array of Char; Quote: Char): TArray<DelphiString>; overload;
    method Split(const Separator: array of Char; QuoteStart, QuoteEnd: Char): TArray<DelphiString>; overload;
    method Split(const Separator: array of Char; QuoteStart, QuoteEnd: Char; Options: TStringSplitOptions): TArray<DelphiString>; overload;
    method Split(const Separator: array of Char; QuoteStart, QuoteEnd: Char; Count: Integer): TArray<DelphiString>; overload;
    method Split(const Separator: array of Char; QuoteStart, QuoteEnd: Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>; overload;
    method Split(const Separator: array of DelphiString; Quote: Char): TArray<DelphiString>; overload;
    method Split(const Separator: array of DelphiString; QuoteStart, QuoteEnd: Char): TArray<DelphiString>; overload;
    method Split(const Separator: array of DelphiString; QuoteStart, QuoteEnd: Char; Options: TStringSplitOptions): TArray<DelphiString>; overload;
    method Split(const Separator: array of DelphiString; QuoteStart, QuoteEnd: Char; Count: Integer): TArray<DelphiString>; overload;
    method Split(const Separator: array of DelphiString; QuoteStart, QuoteEnd: Char; Count: Integer; Options: TStringSplitOptions): TArray<DelphiString>; overload;
    */    
    method StartsWith(Value: DelphiString): Boolean; partial; empty;
    method StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean; partial; empty;
    method SubString(StartIndex: Integer): DelphiString; partial; empty;
    method SubString(StartIndex: Integer; Length: Integer): DelphiString; partial; empty;
    method ToBoolean: Boolean; 
    method ToInteger: Integer; 
    method ToInt64: Int64; 
    method ToSingle: Double; 
    method ToDouble: Double; 
    method ToExtended: Extended;    
    method ToCharArray: TArray<Char>; partial; empty;
    method ToCharArray(StartIndex: Integer; Length: Integer): TArray<Char>; partial; empty;
    method ToLower: DelphiString; partial; empty;
    //method ToLower(LocaleID: TLocaleID): DelphiString; overload;
    method ToLowerInvariant: DelphiString; partial; empty;
    method ToUpper: DelphiString; partial; empty;
    //method ToUpper(LocaleID: TLocaleID): DelphiString; overload;
    method ToUpperInvariant: DelphiString; partial; empty;    
    method Trim: DelphiString; partial; empty;    
    method TrimLeft: DelphiString; partial; empty;
    method TrimRight: DelphiString; partial; empty;
    method Trim(const TrimChars: array of Char): DelphiString; partial; empty;
    method TrimLeft(const TrimChars: array of Char): DelphiString; partial; empty;
    method TrimRight(const TrimChars: array of Char): DelphiString; partial; empty;
    method TrimEnd(const TrimChars: array of Char): DelphiString; partial; empty;
    method TrimStart(const TrimChars: array of Char): DelphiString; partial; empty;
    property Chars[Index: Integer]: Char read GetChar write SetChar;
    property Character[Index: Integer]: Char read GetOffsetChar write SetOffsetChar; default;
    property Length: Integer read GetLength;
  end;

implementation

constructor DelphiString;
begin
  fData := InternalCreate;
end;

method DelphiString.Create(C: Char; Count: Integer): DelphiString;
begin
  result := CreateWithChars(C, Count);
end;

method DelphiString.Create(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := CreateFromArray(Value, StartIndex, Length);
end;

method DelphiString.Create(const Value: array of Char): DelphiString;
begin
  result := &Create(Value, 0, Value.length);
end;

method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [], nil); // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [], LocaleID);
end;

method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [TCompareOptions.coIgnoreCase], nil);  // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, IgnoreCase, LocaleID);
end;

method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, Options, nil); // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, False, LocaleID);
end;

method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, False, nil); // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; LocaleID: TLocaleID): Integer;
begin
  result := Compare(StrA, IndexA, StrB, IndexB, ALength, nil); // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean): Integer;
begin
  result := Compare(StrA, IndexA, StrB, IndexB, ALength, IgnoreCase, nil); // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer;
begin
  result := Compare(StrA, IndexA, StrB, IndexB, ALength, IgnoreCase, LocaleID);
end;

method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, Options, nil); // TODO locale
end;

method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, Options, LocaleID);
end;

method DelphiString.InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer;
begin
  if IgnoreCase then
    result := InternalCompare(StrA, IndexA, StrB, IndexB, LengthA, LengthB, [TCompareOption.coIgnoreCase], LocaleID)
  else
    result := InternalCompare(StrA, IndexA, StrB, IndexB, LengthA, LengthB, [], LocaleID)
end;

method DelphiString.CountChar(const C: Char): Integer;
begin
  result := 0;
  for i: Integer := 0 to self.Length - 1 do
    if self[i] = C then
      inc(result);
end;

method DelphiString.DeQuotedString: DelphiString;
begin
  result := DeQuotedString('"');
end;

method DelphiString.EndsWith(const Value: DelphiString): Boolean;
begin
  result := EndsWith(Value, False);
end;

method DelphiString.Equals(const Value: DelphiString): Boolean;
begin
  result := self.fData = Value.fData;
end;

method DelphiString.Equals(const a: DelphiString; const b: DelphiString): Boolean;
begin
  result := a = b;
end;

method DelphiString.QuotedString: DelphiString;
begin
  result := QuotedString('''');
end;

method DelphiString.QuotedString(const QuoteChar: Char): DelphiString;
begin
  result := self.Substring(0);
  for i: Integer := 0 to fData.length - 1 do
    if result.Chars[i] = QuoteChar then
      result := result.Insert(i, QuoteChar);
  result := QuoteChar + result + QuoteChar;    
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char): DelphiString;
begin
  result := self.Replace(OldChar, NewChar, [TReplaceFlags.rfReplaceAll]);
end;

method DelphiString.Replace(OldValue: DelphiString; NewValue: DelphiString): DelphiString;
begin
  result := self.Replace(OldValue, NewValue, [TReplaceFlags.rfReplaceAll]);
end;

method DelphiString.Parse(Value: Integer): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

method DelphiString.Parse(Value: Int64): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

method DelphiString.Parse(Value: Boolean): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

method DelphiString.Parse(Value: Extended): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

method DelphiString.ToBoolean(S: DelphiString): Boolean;
begin
  result := Sugar.Convert.ToBoolean(S);
end;

method DelphiString.ToInteger(S: DelphiString): Integer;
begin
  result := Sugar.Convert.ToInt32(S);
end;

method DelphiString.ToInt64(S: DelphiString): Int64;
begin
  result := Sugar.Convert.ToInt64(S);
end;

method DelphiString.ToSingle(S: DelphiString): Double;
begin
  result := Sugar.Convert.ToDoubleInvariant(S);
end;

method DelphiString.ToDouble(S: DelphiString): Double;
begin
  result := Sugar.Convert.ToDoubleInvariant(S);
end;

method DelphiString.ToExtended(S: DelphiString): Double;
begin
  result := Sugar.Convert.ToDoubleInvariant(S);
end;

method DelphiString.ToBoolean: Boolean;
begin
  result := Sugar.Convert.ToBoolean(self);
end;

method DelphiString.ToInteger: Integer;
begin
  result := Sugar.Convert.ToInt32(self);
end;

method DelphiString.ToInt64: Int64;
begin
  result := Sugar.Convert.ToInt64(self);
end;

method DelphiString.ToSingle: Double;
begin
  result := Sugar.Convert.ToDoubleInvariant(self);
end;

method DelphiString.ToDouble: Double;
begin
  result := Sugar.Convert.ToDoubleInvariant(self);
end;

method DelphiString.ToExtended: Double;
begin
  result := Sugar.Convert.ToDoubleInvariant(self);
end;

operator DelphiString.Implicit(Value: PlatformString): DelphiString;
begin
  result := new DelphiString(Value);
end;

operator DelphiString.Implicit(Value: DelphiString): Sugar.String;
begin
  result := Value.fData;
end;

constructor DelphiString(Value: PlatformString);
begin
  fData := InternalCreate(Value);
end;

operator DelphiString.Add(Value1: DelphiString; Value2: Char): DelphiString;
begin
  result := new DelphiString(Value1 + Value2);
end;

operator DelphiString.Add(Value1: Char; Value2: DelphiString): DelphiString;
begin
  result := new DelphiString(Value1 + Value2);
end;

operator DelphiString.Equal(Value1: DelphiString; Value2: DelphiString): Boolean;
begin
  result := Value1.fData = Value2.fData;
end;

method DelphiString.PlatformArrayToStringArray(Value: array of PlatformString; Count: Integer := -1): TArray<DelphiString>;
begin
  var lTotal: Integer;
  if Count = -1 then
    lTotal := Value.length - 1
  else
    lTotal := Count - 1;
{$IF ECHOES OR COOPER}
  result := new DelphiString[lTotal];
  for i: Integer := 0 to lTotal do
    result[i] := Value[i];
{$ENDIF}
end;

method DelphiString.StringArrayToPlatformArray(Value: TArray<DelphiString>): array of PlatformString;
begin
  result := new PlatformString[Value.length];
  for i: Integer := 0 to Value.length - 1 do
    result[i] := Value[i];
end;

method DelphiString.GetOffsetChar(aIndex: Int32): Char;
begin
  if (aIndex = 0) and (fOffset > 0) then raise new Exception("Index ot of range: Delphi Strings are one-based.");
  result := GetChar(aIndex - fOffset);
end;

method DelphiString.SetOffsetChar(aIndex: Int32; aValue: Char);
begin
  if (aIndex = 0) and (fOffset > 0) then raise new Exception("Index ot of range: Delphi Strings are one-based.");
  SetChar(aIndex - fOffset, aValue);
end;

method DelphiString.GetLength: Integer;
begin
  result := fData.length;
end;

operator DelphiString.Implicit(Value: Char): DelphiString;
begin
  result := Value;
end;

end.
