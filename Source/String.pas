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
  PlatformString = public {$IF ECHOES}System.String{$ELSEIF TOFFEE}Foundation.NSString{$ELSEIF COOPER}java.lang.String{$ELSEIF ISLAND}RemObjects.Elements.System.String{$ENDIF};

  DelphiString = public partial record
  private
    fData: Sugar.String;
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static;
    method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static;

    method InternalCreate: PlatformString; static;
    method InternalCreate(Value: PlatformString): PlatformString; static;
    method GetChar(aIndex: Int32): Char; inline;
    method SetChar(aIndex: Int32; aValue: Char);
    method GetOffsetChar(aIndex: Int32): Char;
    method SetOffsetChar(aIndex: Int32; aValue: Char);
    
    class var fOffset: Integer := 1;
    class method SetOffset(aOffset: Integer);
    begin
      if aOffset not in [0,1] then raise new Exception("Delphi String offset must be 0 or 1");
      fOffset := aOffset;
    end;
    
    method GetLength: Integer; 
    method CreateWithChars(Char: Char; Count: Integer): DelphiString; static;
    method CreateFromArray(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; static;
    method PlatformArrayToStringArray(Value: array of PlatformString; Count: Integer := -1): array of DelphiString; static;
    method StringArrayToPlatformArray(Value: array of DelphiString): array of PlatformString; static;
    {$IF COOPER}
    method ArrayToSplitRegex(Value: array of Char): PlatformString; static;
    method ArrayToSplitRegex(Value: array of DelphiString): PlatformString; static;
    method PlatformCharArrayToCharArray(Value: array of Char; StartIndex: Integer := -1; ALength: Integer := -1): array of Char; static;
    {$ENDIF}
    {$IF TOFFEE}
    method InternalIndexOfAny(Value: array of DelphiString; StartIndex: Integer; var CurrentLength: Integer): Integer;
    method NSArrayToStringArray(Value: NSArray; Count: Integer): array of DelphiString;
    {$ENDIF}
  public
    constructor;
    constructor(Value: PlatformString); 
    method Create(C: Char; Count: Integer): DelphiString; static;
    method Create(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; static; 
    method Create(const Value: array of Char): DelphiString; static;
    class operator Implicit(Value: Char): DelphiString;
    class operator Implicit(Value: PlatformString): DelphiString;
    class operator Implicit(Value: DelphiString): Sugar.String;
    class operator &Add(Value1: DelphiString; Value2: Char): DelphiString;
    class operator &Add(Value1: Char; Value2: DelphiString): DelphiString;
    class operator &Add(Value1: DelphiString; Value2: DelphiString): not nullable DelphiString;
    //class operator &Add(Value1: DelphiString; Value2: Object): not nullable DelphiString;
    //class operator &Add(Value1: Object; Value2: DelphiString): not nullable DelphiString;
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
    method CompareOrdinal(const StrA: DelphiString; const StrB: DelphiString): Integer; static;
    method CompareOrdinal(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer; static;
    method CompareText(const StrA: DelphiString; const StrB: DelphiString): Integer; static;
    
    method Parse(const Value: Integer): DelphiString; static;
    method Parse(const Value: Int64): DelphiString; static; 
    method Parse(const Value: Boolean): DelphiString; static; 
    method Parse(const Value: Extended): DelphiString; static; 
    method ToBoolean(const S: DelphiString): Boolean; static; 
    method ToInteger(const S: DelphiString): Integer; static; 
    method ToInt64(const S: DelphiString): Int64; static; 
    method ToSingle(const S: DelphiString): Double; static; 
    method ToDouble(const S: DelphiString): Double; static; 
    method ToExtended(const S: DelphiString): Double; static;
    method LowerCase(const S: DelphiString): DelphiString; static;
    method LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; static; partial; empty;
    method UpperCase(const S: DelphiString): DelphiString; static;
    method UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; static; partial; empty;
    method CompareTo(const strB: DelphiString): Integer;
    method Contains(const Value: DelphiString): Boolean;
    method &Copy(const Str: DelphiString): DelphiString; static;
    method CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer); 
    method CountChar(const C: Char): Integer;     
    method DeQuotedString: DelphiString;
    method DeQuotedString(const QuoteChar: Char): DelphiString;
    // method EndsText(const ASubText, AText: DelphiString): Boolean; static;  // TODO    
    method EndsWith(const Value: DelphiString): Boolean; inline;
    method EndsWith(const Value: DelphiString; IgnoreCase: Boolean): Boolean;
    method Equals(const Value: DelphiString): Boolean;  
    method Equals(const a: DelphiString; const b: DelphiString): Boolean; static;
    //method Format(const Format: DelphiString; const args: array of const): DelphiString; overload; static; // TODO    
    method IndexOf(Value: Char): Integer;
    method IndexOf(const Value: DelphiString): Integer;
    method IndexOf(Value: Char; StartIndex: Integer): Integer;
    method IndexOf(const Value: DelphiString; StartIndex: Integer): Integer;
    method IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
    method IndexOf(const Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
    method IndexOfAny(const AnyOf: array of Char): Integer; 
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer; 
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer; 
    method Insert(StartIndex: Integer; const Value: DelphiString): DelphiString; 
    //method IsDelimiter(const Delimiters: DelphiString; Index: Integer): Boolean; partial; empty;
    method IsEmpty: Boolean; 
    method IsNullOrEmpty(const Value: DelphiString): Boolean; static; 
    method IsNullOrWhiteSpace(const Value: DelphiString): Boolean; static; 
    //method Join(const Separator: DelphiString; const Values: array of const): DelphiString; overload; static;
    method &Join(Separator: DelphiString; Values: array of DelphiString): DelphiString; static; 
    //method Join(Separator: DelphiString; Values: IEnumerator<DelphiString>): DelphiString; overload; static;
    //method Join(Separator: DelphiString; Values: IEnumerable<DelphiString>): DelphiString; overload; static; inline;
    method &Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString; static; 
    //method LastDelimiter(const Delims: DelphiString): Integer;
    method LastIndexOf(Value: Char): Integer; 
    method LastIndexOf(const Value: DelphiString): Integer; 
    method LastIndexOf(Value: Char; StartIndex: Integer): Integer; 
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer): Integer; 
    method LastIndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer; 
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer; Count: Integer): Integer; 
    method LastIndexOfAny(const AnyOf: array of Char): Integer; 
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer; 
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer; 
    method PadLeft(TotalWidth: Integer): DelphiString; 
    method PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString; 
    method PadRight(TotalWidth: Integer): DelphiString; 
    method PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString; 
    method QuotedString: DelphiString;
    method QuotedString(const QuoteChar: Char): DelphiString;
    method &Remove(StartIndex: Integer): DelphiString; inline;
    method &Remove(StartIndex: Integer; Count: Integer): DelphiString; 
    method Replace(OldChar: Char; NewChar: Char): DelphiString;
    method Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString; 
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString): DelphiString;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString; 
    
    method Split(const Separator: array of Char): array of DelphiString; inline;
    method Split(const Separator: array of Char; Count: Integer): array of DelphiString; inline;
    method Split(const Separator: array of Char; Options: TStringSplitOptions): array of DelphiString; inline;
    method Split(const Separator: array of Char; Count: Integer; Options: TStringSplitOptions): array of DelphiString; 
    method Split(const Separator: array of DelphiString): array of DelphiString; inline;
    method Split(const Separator: array of DelphiString; Count: Integer): array of DelphiString; 
    method Split(const Separator: array of DelphiString; Options: TStringSplitOptions): array of DelphiString; 
    method Split(const Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): array of DelphiString; 

    method StartsWith(Value: DelphiString): Boolean; 
    method StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean; 
    method SubString(StartIndex: Integer): DelphiString; 
    method SubString(StartIndex: Integer; ALength: Integer): DelphiString; 
    method ToBoolean: Boolean; 
    method ToInteger: Integer; 
    method ToInt64: Int64; 
    method ToSingle: Double; 
    method ToDouble: Double; 
    method ToExtended: Extended;    
    method ToCharArray: array of Char; 
    method ToCharArray(StartIndex: Integer; ALength: Integer): array of Char; 
    method ToLower: DelphiString; 
    //method ToLower(LocaleID: TLocaleID): DelphiString; overload;
    method ToLowerInvariant: DelphiString; 
    method ToUpper: DelphiString; 
    //method ToUpper(LocaleID: TLocaleID): DelphiString; overload;
    method ToUpperInvariant: DelphiString; 
    method Trim: DelphiString; 
    method TrimLeft: DelphiString; 
    method TrimRight: DelphiString; 
    method Trim(const TrimChars: array of Char): DelphiString; 
    method TrimLeft(const TrimChars: array of Char): DelphiString; 
    method TrimRight(const TrimChars: array of Char): DelphiString; 
    method TrimEnd(const TrimChars: array of Char): DelphiString; 
    method TrimStart(const TrimChars: array of Char): DelphiString; 
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
  for i: Integer := 0 to fData.Length - 1 do
    if result.Chars[i] = QuoteChar then
      result := result.Insert(i, QuoteChar);
  result := QuoteChar + result + QuoteChar;    
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char): DelphiString;
begin
  result := Replace(PlatformString(OldChar), PlatformString(NewChar), [TReplaceFlags.rfReplaceAll]);
end;

method DelphiString.Replace(OldValue: DelphiString; NewValue: DelphiString): DelphiString;
begin
  result := Replace(OldValue, NewValue, [TReplaceFlags.rfReplaceAll]);
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

operator DelphiString.Implicit(Value: Char): DelphiString;
begin
  {$IF COCOA}
  //76216: Toffee: can't call `Object` extension methods on value types
  result := NSString.stringWithFormat("%c", Value);
  {$ELSE}
  result := Value.ToString();
  {$ENDIF}
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

operator DelphiString.Add(Value1: DelphiString; Value2: DelphiString): not nullable DelphiString;
begin
 {$IF COOPER}
 result := (java.lang.String(Value1)+java.lang.String(Value2)) as not nullable;
 {$ELSEIF ECHOES}
 result := (System.String(Value1)+System.String(Value2)) as not nullable;
 {$ELSEIF TOFFEE}
 result := (Foundation.NSString(Value1) + Foundation.NSString(Value2)) as not nullable;
 {$ENDIF}
end;

/*
operator DelphiString.Add(Value1: DelphiString; Value2: Object): not nullable DelphiString;
begin
  //result := Value1 + Value2.ToString;  // TODO compiler error
  result := Value1;
end;

operator DelphiString.Add(Value1: Object; Value2: DelphiString): not nullable DelphiString;
begin
 //result := Value1.ToString + Value2;  // TODO compiler error
end;
*/

operator DelphiString.Equal(Value1: DelphiString; Value2: DelphiString): Boolean;
begin
  result := Value1.fData = Value2.fData;
end;

method DelphiString.PlatformArrayToStringArray(Value: array of PlatformString; Count: Integer := -1): array of DelphiString;
begin
  var lTotal: Integer;
  if Count = -1 then
    lTotal := Value.length - 1
  else
    lTotal := Count - 1;

  result := new DelphiString[lTotal];
  for i: Integer := 0 to lTotal do
    result[i] := Value[i];
end;

method DelphiString.StringArrayToPlatformArray(Value: array of DelphiString): array of PlatformString;
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
  result := fData.Length;
end;

method DelphiString.LowerCase(S: DelphiString): DelphiString;
begin
  result := S.fData.ToLower;
end;

method DelphiString.UpperCase(S: DelphiString): DelphiString;
begin
  result := S.fData.ToUpper;
end;

method DelphiString.CompareTo(strB: DelphiString): Integer;
begin
  result := fData.CompareTo(strB);
end;

method DelphiString.Contains(Value: DelphiString): Boolean;
begin
  result := fData.Contains(Value.fData);
end;

method DelphiString.Copy(Str: DelphiString): DelphiString;
begin
  {$IF COOPER}
  result := java.lang.String(Str.fData);
  {$ELSEIF ECHOES}
  result := System.String.Copy(Str.fData);
  {$ELSEIF TOFFEE}
  result := new Foundation.NSString(Foundation.NSString(Str.fData));
  {$ENDIF}
end;

method DelphiString.CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; Count: Integer);
begin 
  {$IF COOPER}
  for i: Integer := 0 to Count - 1 do
    destination[DestinationIndex + i] := self.chars[SourceIndex + i];    
  {$ELSEIF ECHOES}
  PlatformString(fData).CopyTo(SourceIndex, destination, DestinationIndex, Count);  
  {$ELSEIF TOFFEE}
  PlatformString(fData).getCharacters(@destination[DestinationIndex]) range(Foundation.NSMakeRange(SourceIndex, Count));
  {$ENDIF}
end;

method DelphiString.DeQuotedString(QuoteChar: Char): DelphiString;
begin
  if (fData.Length >= 2) and (fData.Chars[0] = QuoteChar) and (fData.Chars[fData.Length - 1] = QuoteChar) then begin
    var lSb := new Char[fData.Length - 2];
    var lTotal := 0;
    var lInQuote: Boolean := false;
    for i: Integer := 1 to fData.Length - 2 do begin
      var lChar := fData.Chars[i];
      if lChar = QuoteChar then begin
        if lInQuote then begin
          lInQuote := false;
          lSb[lTotal] := lChar;
          inc(lTotal);
        end
        else 
          lInQuote := true;
      end
      else begin
        if lInQuote then
          lInQuote := false;
        lSb[lTotal] := lChar;
        inc(lTotal);
      end;
    end;
    result := CreateFromArray(lSb, 0, lTotal);
  end
  else 
    result := self;
end;

method DelphiString.EndsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  result := fData.EndsWith(Value, IgnoreCase);
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
  result := fData.IndexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.IndexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer; Count: Integer): Integer;
begin
  {$IF COOPER}
  var lIndex := PlatformString(fData).indexOf(Value, StartIndex);
  if (lIndex >= 0) and (lIndex <= StartIndex + Count) then
    result := lIndex
  else
    result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).IndexOf(Value, StartIndex, Count);
  {$ELSEIF TOFFEE}
  result := IndexOf(Foundation.NSString(Value), StartIndex, Count);
  {$ENDIF}
end;

method DelphiString.IndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  {$IF COOPER}
  var lIndex := PlatformString(fData).indexOf(Value, StartIndex);
  if (lIndex >= 0) and (lIndex <= StartIndex + Count) then
    result := lIndex
  else
    result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).IndexOf(Value, StartIndex, Count);
  {$ELSEIF TOFFEE}
  var lRange:= PlatformString(fData).rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch) range(Foundation.NSMakeRange(StartIndex, Count));
  result := if lRange.location <> NSNotFound then lRange.location else -1;
  {$ENDIF}
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
  {$IF COOPER}
  var lEnd: Integer;
  if (StartIndex + Count) >= fData.length then
    lEnd := fData.length
  else
    lEnd := StartIndex + Count;
  for i: Integer := StartIndex to lEnd - 1 do begin
     for each c: Char in AnyOf do begin
       if fData.Chars[i] = c then
         exit i;
     end;
  end;
  result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).IndexOfAny(AnyOf, StartIndex, Count);
  {$ELSEIF TOFFEE}
  var lChars := Foundation.NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@AnyOf) length(AnyOf.length));
  var lRange := PlatformString(fData).rangeOfCharacterFromSet(lChars) options(NSStringCompareOptions.NSLiteralSearch) range(Foundation.NSMakeRange(StartIndex, Count));
  result := if lRange.location <> NSNotFound then lRange.location else -1;
  {$ENDIF}
end;

method DelphiString.Insert(StartIndex: Integer; Value: DelphiString): DelphiString;
begin
  {$IF COOPER}
  var sb := new StringBuilder(fData);
  sb.insert(StartIndex, Value);
  fData := sb.toString;
  {$ELSEIF ECHOES}
  fData := PlatformString(fData).Insert(StartIndex, Value);
  {$ELSEIF TOFFEE}
  if StartIndex > 0 then
    fData := PlatformString(fData).substringToIndex(StartIndex - 1) + Value.fData + PlatformString(fData).substringFromIndex(StartIndex)
  else
    fData := Value.fData + PlatformString(fData).substringFromIndex(StartIndex);
  {$ENDIF}
  result := fData;
end;

method DelphiString.IsEmpty: Boolean;
begin
  result := fData.Length = 0;
end;

method DelphiString.IsNullOrEmpty(Value: DelphiString): Boolean;
begin
  result := Sugar.String.IsNullOrEmpty(Value.fData);
end;

method DelphiString.IsNullOrWhiteSpace(Value: DelphiString): Boolean;
begin
  result := Sugar.String.IsNullOrWhiteSpace(Value);
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString): DelphiString;
begin
  result := &Join(Separator, Values, 0, Values.length);
end;

method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; Count: Integer): DelphiString;
begin
  {$IF COOPER}
  var sb := new StringBuilder;
  for i: Integer := StartIndex to (StartIndex + Count) - 1 do begin
     if i <> StartIndex then
      sb.append(Separator);
    sb.append(Values[i]);
  end;
  result := sb.toString;
  {$ELSEIF ECHOES}
  result := System.String.Join(Separator, Values, StartIndex, Count);
  {$ELSEIF TOFFEE}
  var lArray := new NSMutableArray(Values.length);
  for i: Integer := StartIndex to (StartIndex + Count) - 1 do
    lArray.addObject(Values[i].fData);

  result := lArray.componentsJoinedByString(Separator.fData);
  {$ENDIF}
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
  {$IF COOPER}
  result := PlatformString(fData).lastIndexOf(Value, (StartIndex - Count));
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOf(Value, StartIndex, Count);
  {$ELSEIF TOFFEE}
  result := LastIndexOf(Foundation.NSString(Value), StartIndex, Count);
  {$ENDIF}
end;

method DelphiString.LastIndexOf(Value: DelphiString; StartIndex: Integer; Count: Integer): Integer;
begin
  {$IF COOPER}
  result := PlatformString(fData).lastIndexOf(Value, (StartIndex - Count));
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOf(Value, StartIndex, Count);
  {$ELSEIF TOFFEE}
  var lRange:= PlatformString(fData).rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch) range(Foundation.NSMakeRange(StartIndex, Count));
  result := if lRange.location <> NSNotFound then lRange.location else -1;  
  {$ENDIF}
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char): Integer;
begin
  {$IF COOPER OR TOFFEE}
  result := LastIndexOfAny(AnyOf, fData.Length - 1, fData.Length); 
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOfAny(AnyOf);
  {$ENDIF}
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  {$IF COOPER OR TOFFEE}
  result := LastIndexOfAny(AnyOf, StartIndex, StartIndex + 1);
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOfAny(AnyOf, StartIndex);
  {$ENDIF}
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer; Count: Integer): Integer;
begin
  {$IF COOPER}
  var lStart: Integer;
  if (StartIndex >= 0) and (StartIndex < fData.length) then
    lStart := StartIndex
  else
    lStart := fData.length;
  var lEnd: Integer := lStart - Count + 1;
  if lEnd < 0 then
    lEnd := 0;
  
  for i: Integer := lStart downto lEnd do begin
    var lChar := fData.Chars[i];  
    for each c: Char in AnyOf do
      if lChar = c then
        exit i;
  end;
  result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOfAny(AnyOf, StartIndex, Count);
  {$ELSEIF TOFFEE}
  var lChars := Foundation.NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@AnyOf) length(AnyOf.length));
  var lRange := PlatformString(fData).rangeOfCharacterFromSet(lChars) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch) range(Foundation.NSMakeRange(StartIndex, Count));
  result := if lRange.location <> NSNotFound then lRange.location else -1;
  {$ENDIF}
end;

method DelphiString.PadLeft(TotalWidth: Integer): DelphiString;
begin
  result := fData.PadStart(TotalWidth);
end;

method DelphiString.PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  result := fData.PadStart(TotalWidth, PaddingChar);
end;

method DelphiString.PadRight(TotalWidth: Integer): DelphiString;
begin
  result := fData.PadEnd(TotalWidth);
end;

method DelphiString.PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString;
begin
  result := fData.PadEnd(TotalWidth, PaddingChar);
end;

method DelphiString.Remove(StartIndex: Integer): DelphiString;
begin
  result := &Remove(StartIndex, Length - StartIndex);
end;

method DelphiString.Remove(StartIndex: Integer; Count: Integer): DelphiString;
begin
  {$IF COOPER}
  var lSb := new StringBuilder(fData);
  lSb.delete(StartIndex, Count);
  fData := lSb.toString;
  {$ELSEIF ECHOES}
  fData := PlatformString(fData).Remove(StartIndex, Count);
  {$ELSEIF TOFFEE}
  fData := PlatformString(fData).substringWithRange(Foundation.NSMakeRange(0, StartIndex)) + PlatformString(fData).substringWithRange(Foundation.NSMakeRange(StartIndex + Count, fData.Length - 1));
  {$ENDIF}
  result := fData;
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  {$IF COOPER OR ECHOES}
  result := Replace(PlatformString(OldChar), PlatformString(NewChar), ReplaceFlags);
  {$ELSEIF TOFFEE}
  result := Replace(Foundation.NSString.stringWithFormat("%c", OldChar), Foundation.NSString.stringWithFormat("%c", NewChar), ReplaceFlags); 
  {$ENDIF}
end;

method DelphiString.Replace(OldValue: DelphiString; NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  {$IF COOPER}
  var lPattern: DelphiString := java.util.regex.Pattern.quote(OldValue);
  if TReplaceFlags.rfIgnoreCase in ReplaceFlags then
    lPattern := '(?i)' + lPattern;
  if TReplaceFlags.rfReplaceAll in ReplaceFlags then
    result := PlatformString(fData).replaceAll(lPattern, NewValue)
  else
    result := PlatformString(fData).replaceFirst(lPattern, NewValue);
  {$ELSEIF ECHOES}
  var lOptions: System.Text.RegularExpressions.RegexOptions := 0;
  var lCount: Integer := 1;
  if [TReplaceFlags.rfIgnoreCase] in ReplaceFlags then
    lOptions := System.Text.RegularExpressions.RegexOptions.IgnoreCase;
  if [TReplaceFlags.rfReplaceAll] in ReplaceFlags then
    lCount := -1;

  var lRegEx := new System.Text.RegularExpressions.Regex(OldValue.fData, lOptions);
  result := lRegEx.Replace(fData, System.Text.RegularExpressions.Regex.Escape(NewValue.fData), lCount);
  {$ELSEIF TOFFEE}
  var lOptions: NSStringCompareOptions := 0;
  var lRange: Foundation.NSRange;

  if [TReplaceFlags.rfIgnoreCase] in ReplaceFlags then
    lOptions := NSStringCompareOptions.NSCaseInsensitiveSearch;
  if not ([TReplaceFlags.rfReplaceAll] in ReplaceFlags) then begin
    lRange := PlatformString(fData).rangeOfString(OldValue) options(lOptions);
    if lRange.location = NSNotFound then
      exit self;   
  end
  else
    lRange := Foundation.NSMakeRange(0, fData.Length - 1);
  result := PlatformString(fData).stringByReplacingOccurrencesOfString(OldValue.fData) withString(NewValue.fData) options(lOptions) range(lRange)
  {$ENDIF}
end;

{$IF COOPER}
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

method DelphiString.PlatformCharArrayToCharArray(Value: array of Char; StartIndex: Integer := -1; ALength: Integer := -1): array of Char;
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
  result := new Char[lTotal];

  for i: Integer := lBegin to (lBegin + ALength) do
    result[i - lBegin] := Value[i];
end;
{$ENDIF}

method DelphiString.Split(Separator: array of Char): array of DelphiString;
begin
  result := Split(Separator, -1, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer): array of DelphiString;
begin
  result := Split(Separator, Count, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of Char; Options: TStringSplitOptions): array of DelphiString;
begin
  result := Split(Separator, -1, Options);
end;

method DelphiString.Split(Separator: array of Char; Count: Integer; Options: TStringSplitOptions): array of DelphiString;
begin
  {$IF COOPER}
  if Options = TStringSplitOptions.ExcludeEmpty then begin
    var lSep := ArrayToSplitRegex(Separator);
    var lArray := PlatformString(fData).split(lSep, 0);
    result := PlatformArrayToStringArray(lArray, Count);
  end
  else
    result := Split(Separator, Count);
  {$ELSEIF ECHOES}
  var lArray: array of PlatformString;  
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := PlatformString(fData).Split(Separator, Count, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := PlatformString(fData).Split(Separator, Count);
    
  result := PlatformArrayToStringArray(lArray);
  {$ELSEIF TOFFEE}
  var lChars := Foundation.NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@Separator) length(Separator.length));
  var lArray := PlatformString(fData).componentsSeparatedByCharactersInSet(lChars);
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := lArray.filteredArrayUsingPredicate(NSPredicate.predicateWithFormat('length > 0'));

  result := NSArrayToStringArray(lArray, Count);
  {$ENDIF}
end;

method DelphiString.Split(Separator: array of DelphiString): array of DelphiString;
begin
  result := Split(Separator, -1, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of DelphiString; Count: Integer): array of DelphiString;
begin
  result := Split(Separator, Count, TStringSplitOptions.None);
end;

method DelphiString.Split(Separator: array of DelphiString; Options: TStringSplitOptions): array of DelphiString;
begin
  result := Split(Separator, -1, Options);
end;

{$IF TOFFEE}
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

method DelphiString.NSArrayToStringArray(Value: NSArray; Count: Integer): array of DelphiString;
begin
  result := new DelphiString[Value.count];
  for i: Integer := 0 to Count - 1 do
    result[i] := NSString(Value[i]);
end;
{$ENDIF}

method DelphiString.Split(Separator: array of DelphiString; Count: Integer; Options: TStringSplitOptions): array of DelphiString;
begin
  {$IF COOPER}
  if Options = TStringSplitOptions.ExcludeEmpty then begin
    var lSep := ArrayToSplitRegex(Separator);
    var lArray := PlatformString(fData).split(lSep, 0);
    result := PlatformArrayToStringArray(lArray, Count);
  end
  else
    result := Split(Separator, Count);
  {$ELSEIF ECHOES}
  var lArray: array of PlatformString;
  var lSep := StringArrayToPlatformArray(Separator);
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := PlatformString(fData).Split(lSep, Count, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := PlatformString(fData).Split(lSep, Count, StringSplitOptions.None);
    
  result := PlatformArrayToStringArray(lArray);
  {$ELSEIF TOFFEE}
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
    lArray.addObject(PlatformString(fData).substringFromIndex(lStartIndex));

  result := NSArrayToStringArray(lArray, Count);  
  {$ENDIF}
end;

method DelphiString.StartsWith(Value: DelphiString): Boolean;
begin
  result := fData.StartsWith(Value.fData);
end;

method DelphiString.StartsWith(Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  result := fData.StartsWith(Value.fData, IgnoreCase);
end;

method DelphiString.SubString(StartIndex: Integer): DelphiString;
begin
  result := fData.Substring(StartIndex);
end;

method DelphiString.SubString(StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := fData.Substring(StartIndex, ALength);
end;

method DelphiString.ToCharArray: array of Char;
begin
  result := fData.ToCharArray;
end;

method DelphiString.ToCharArray(StartIndex: Integer; ALength: Integer): array of Char;
begin
  {$IF COOPER}
  var lArray :=  fData.toCharArray;
  result := PlatformCharArrayToCharArray(lArray, StartIndex, ALength);
  {$ELSEIF ECHOES}
  result := PlatformString(fData).ToCharArray(StartIndex, ALength);
  {$ELSEIF TOFFEE}
  result := new Char[ALength]; 
  var lArray := new Char[ALength];
   PlatformString(fData).getCharacters(lArray) range(Foundation.NSMakeRange(StartIndex, ALength));

  for i: Integer := 0 to lArray.length do
    result[i] := lArray[i];
  {$ENDIF}
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
  result := fData.TrimStart;
end;

method DelphiString.TrimRight: DelphiString;
begin
  result := fData.TrimEnd;
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
  result := fData.TrimEnd(TrimChars);
end;

method DelphiString.TrimStart(TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimStart(TrimChars);
end;

method DelphiString.InternalCreate: PlatformString;
begin
  {$IF COOPER}
  result := new java.lang.String;
  {$ELSEIF ECHOES}
  result := new System.String([]);  
  {$ELSEIF TOFFEE}
  result := Foundation.NSString.string;
  {$ENDIF}
end;

method DelphiString.InternalCreate(Value: PlatformString): PlatformString;
begin
  {$IF COOPER}
  result := new java.lang.String(Value);
  {$ELSEIF ECHOES}
  result := System.String.Copy(Value);
  {$ELSEIF TOFFEE}
  result := Foundation.NSString.stringWithString(Value);
  {$ENDIF}
end;

method DelphiString.CreateWithChars(Char: Char; Count: Integer): DelphiString;
begin
  {$IF COOPER}
  var lChars := new Char[Count];
  for i: Integer := 0 to Count-1 do
    lChars[i] := Char;
  result := new java.lang.String(lChars);
  {$ELSEIF ECHOES}
  result := new System.String(Char, Count);  
  {$ELSEIF TOFFEE}
  result := Foundation.NSString("").stringByPaddingToLength(Count) withString(Foundation.NSString.stringWithFormat("%c", Char)) startingAtIndex(0);  
  {$ENDIF}
end;

method DelphiString.CreateFromArray(Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString;
begin
  {$IF COOPER}
  result := new java.lang.String(Value, StartIndex, ALength);
  {$ELSEIF ECHOES}
  result := new System.String(Value, StartIndex, ALength);  
  {$ELSEIF TOFFEE}
  result := new Foundation.NSString withCharacters(@Value[StartIndex]) length(ALength);
  {$ENDIF}
end;

method DelphiString.GetChar(aIndex: Int32): Char;
begin
  {$IF COOPER}
  result := PlatformString(fData).charAt(aIndex);
  {$ELSEIF ECHOES}
  result := fData.Chars[aIndex];
  {$ELSEIF TOFFEE}
  result := PlatformString(fData).characterAtIndex(aIndex);
  {$ENDIF}
end;

method DelphiString.SetChar(aIndex: Int32; aValue: Char);
begin
  {$IF COOPER}
  fData := fData.substring(0, aIndex) + aValue + fData.substring(aIndex+1);
  {$ELSEIF ECHOES}
  fData := fData.Substring(0, aIndex) + aValue + fData.Substring(aIndex+1);
  {$ELSEIF TOFFEE}
  fData := PlatformString(fData).substringWithRange(Foundation.NSMakeRange(0, aIndex)) + aValue + PlatformString(fData).substringWithRange(Foundation.NSMakeRange(aIndex + 1, fData.Length - 1));
  {$ENDIF}
end;

method DelphiString.InternalCompare(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; LengthA: Integer; LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
begin
  {$IF COOPER}
  var lTotalChars: Integer;

  if StrA.Length <= StrB.Length then
    lTotalChars := StrA.Length
  else
    lTotalChars := StrB.Length;

  var Lcollator := java.text.Collator.getInstance; // TODO locale

  if ([TCompareOption.coLingIgnoreCase] in Options) or ([TCompareOption.coIgnoreCase] in Options) then
    Lcollator.setStrength(java.text.Collator.TERTIARY);
      
  if [TCompareOption.coLingIgnoreDiacritic] in Options then
   Lcollator.setStrength(java.text.Collator.PRIMARY);

  result := Lcollator.compare(StrA.SubString(IndexA, lTotalChars), StrB.SubString(IndexB, lTotalChars));
  {$ELSEIF ECHOES}
  var LOptions: System.Globalization.CompareOptions := 0;
  var LTotalChars: Integer;

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
  {$ELSEIF TOFFEE}
  var lOptions: Foundation.NSStringCompareOptions := 0;
  var lTotalChars: Integer;

  if ([TCompareOption.coLingIgnoreCase] in Options) or ([TCompareOption.coIgnoreCase] in Options) then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSCaseInsensitiveSearch;
  if [TCompareOption.coLingIgnoreDiacritic] in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSDiacriticInsensitiveSearch;
  if [TCompareOption.coIgnoreWidth] in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSWidthInsensitiveSearch;
  if [TCompareOption.coDigitAsNumbers] in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSNumericSearch;
  if [TCompareOption.coStringSort] in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSForcedOrderingSearch;

  if StrA.Length <= StrB.Length then
    lTotalChars := StrA.Length
  else
    lTotalChars := StrB.Length;

  { create new strings only if needed }
  var lStrA: Foundation.NSString;
  if (IndexA <> 0) or (LengthA <> StrA.Length) then
    lStrA := PlatformString(StrA.fData).substringWithRange(Foundation.NSMakeRange(IndexA, LengthA))
  else
    lStrA := StrA.fData;
    
  var lStrB: Foundation.NSString;
  if (IndexB <> 0) or (LengthB <> StrB.Length) then
    lStrB := PlatformString(StrB.fData).substringWithRange(Foundation.NSMakeRange(IndexB, LengthB))
  else
    lStrB := StrB.fData;

  result := lStrA.compare(lStrB) options(lOptions) range(Foundation.NSMakeRange(0, lTotalChars));
  {$ENDIF}
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := StrA.fData.CompareTo(StrB.fData);
end;

method DelphiString.CompareOrdinal(StrA: DelphiString; IndexA: Integer; StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer;
begin
  {$IF COOPER}
  var lStrA := StrA.fData.substring(IndexA, Length);
  result := lStrA.compareTo(StrB.SubString(IndexB, Length));
  {$ELSEIF ECHOES}
  result := System.String.CompareOrdinal(StrA, IndexA, StrB, IndexB, ALength);
  {$ELSEIF TOFFEE}
  var lStrA := PlatformString(StrA.fData).substringWithRange(Foundation.NSMakeRange(IndexA, Length));
  var lStrB := PlatformString(StrB.fData).substringWithRange(Foundation.NSMakeRange(IndexB, Length));
  result := lStrA.compare(lStrB) options(NSStringCompareOptions.NSLiteralSearch);
  {$ENDIF}
end;

method DelphiString.CompareText(StrA: DelphiString; StrB: DelphiString): Integer;
begin
  result := StrA.fData.CompareToIgnoreCase(StrB.fData);
end;

end.
