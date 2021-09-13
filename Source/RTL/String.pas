﻿namespace RemObjects.Elements.RTL.Delphi;

interface

uses
{$IF ISLAND}
  RemObjects.Elements.System,
{$ELSE}
  RemObjects.Elements.RTL,
{$ENDIF}
  RemObjects;

type
  TCompareOption = public enum(coLingIgnoreCase, coLingIgnoreDiacritic, coIgnoreCase,
                               coIgnoreKanatype, coIgnoreNonSpace, coIgnoreSymbols, coIgnoreWidth,
                               coLingCasing, coDigitAsNumbers, coStringSort) of Integer;
  TCompareOptions = public set of TCompareOption;
  TReplaceFlag = public enum(rfReplaceAll, rfIgnoreCase) of Integer;
  TReplaceFlags = public set of TReplaceFlag;
  //TArray<T> = array of T;
  TStringSplitOptions = public (None, ExcludeEmpty);

  [assembly:DefaultStringType("RemObjects.Elements.RTL.Delphi", typeOf(RemObjects.Elements.RTL.Delphi.WideString))]

  String = public DelphiString;
  WideString = public DelphiString;
  RTL2String = RemObjects.Elements.RTL.String;
  PlatformString = public {$IF ECHOES}System.String{$ELSEIF TOFFEE}Foundation.NSString{$ELSEIF COOPER}java.lang.String{$ELSEIF ISLAND}RemObjects.Elements.System.String{$ENDIF};

  {$IF NOT ISLAND}
  IString = public interface
    method ToString: PlatformString;
  end;
  {$ENDIF}

  DelphiString = public partial record(IString)
  private
    fData: RemObjects.Elements.RTL.String;
    class method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static;
    class method InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static;

    class method InternalCreate: PlatformString; static;
    class method InternalCreate(Value: PlatformString): PlatformString; static;
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
    class method CreateWithChars(Char: Char; aCount: Integer): DelphiString; static;
    class method CreateFromArray(Value: array of Char; StartIndex: Integer; aLength: Integer): DelphiString; static;
    class method PlatformArrayToStringArray(Value: array of PlatformString; aCount: Integer := -1): array of DelphiString; static;
    class method StringArrayToPlatformArray(Value: array of DelphiString): array of PlatformString; static;
    {$IF COOPER}
    class method ArrayToSplitRegex(Value: array of Char): PlatformString; static;
    class method ArrayToSplitRegex(Value: array of DelphiString): PlatformString; static;
    class method PlatformCharArrayToCharArray(Value: array of Char; StartIndex: Integer := -1; ALength: Integer := -1): array of Char; static;
    {$ENDIF}
    {$IF TOFFEE OR COOPER OR ISLAND}
    method InternalIndexOfAny(Value: array of DelphiString; StartIndex: Integer; var CurrentLength: Integer): Integer;
    {$ENDIF}
    {$IF TOFFEE}
    method NSArrayToStringArray(Value: NSArray; aCount: Integer): array of DelphiString;
    {$ENDIF}
  published
    constructor;
    constructor(Value: PlatformString);
    constructor(aBytes: array of Byte);
    constructor(C: Char; aCount: Integer);
    class method Create(C: Char; aCount: Integer): DelphiString; static;
    class method Create(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString; static;
    class method Create(const Value: array of Char): DelphiString; static;
    class method Create(aBytes: array of Byte): DelphiString; static;

    class operator Implicit(Value: Char): DelphiString;
    class operator Implicit(Value: PlatformString): DelphiString;
    //class operator Implicit(Value: RemObjects.Elements.RTL.String): DelphiString;
    class operator Implicit(Value: AnsiString): DelphiString;
    {$IF NOT COOPER}
    class operator Implicit(Value: PChar): DelphiString; unsafe;
    {$ENDIF}
    class operator Implicit(Value: DelphiString): PlatformString;
    //class operator Implicit(Value: DelphiString): RemObjects.Elements.RTL.String;
    class operator Implicit(Value: DelphiString): AnsiString;

    class operator &Add(Value1: DelphiString; Value2: Char): DelphiString;
    class operator &Add(Value1: Char; Value2: DelphiString): DelphiString;
    class operator &Add(Value1: DelphiString; Value2: DelphiString): not nullable DelphiString;
    //class operator &Add(Value1: DelphiString; Value2: Object): not nullable DelphiString;
    //class operator &Add(Value1: Object; Value2: DelphiString): not nullable DelphiString;
    class operator Equal(Value1, Value2: DelphiString): Boolean;
    class operator NotEqual(Value1, Value2: DelphiString): Boolean;
    class operator Greater(Value1, Value2: DelphiString): Boolean;
    class operator Less(Value1, Value2: DelphiString): Boolean;
    class operator GreaterOrEqual(Value1, Value2: DelphiString): Boolean;
    class operator LessOrEqual(Value1, Value2: DelphiString): Boolean;

    class operator Equal(Value1: Char; Value2: DelphiString): Boolean;
    begin
      exit DelphiString(Value1) = Value2;
    end;
    class operator NotEqual(Value1: Char; Value2: DelphiString): Boolean;
    begin
      exit DelphiString(Value1) <> Value2;
    end;
    class operator Greater(Value1: Char; Value2: DelphiString): Boolean;
    begin
      exit DelphiString(Value1) > Value2;
    end;
    class operator Less(Value1: Char; Value2: DelphiString): Boolean;
    begin
      exit DelphiString(Value1) < Value2;
    end;
    class operator GreaterOrEqual(Value1: Char; Value2: DelphiString): Boolean;
    begin
      exit DelphiString(Value1) >= Value2;
    end;
    class operator LessOrEqual(Value1: Char; Value2: DelphiString): Boolean;
    begin
      exit DelphiString(Value1) <= Value2;
    end;

    class operator Equal(Value2: DelphiString; Value1: Char): Boolean;
    begin
      exit DelphiString(Value1) = Value2;
    end;
    class operator NotEqual(Value2: DelphiString; Value1: Char): Boolean;
    begin
      exit DelphiString(Value1) <> Value2;
    end;
    class operator Greater(Value2: DelphiString; Value1: Char): Boolean;
    begin
      exit DelphiString(Value1) > Value2;
    end;
    class operator Less(Value2: DelphiString; Value1: Char): Boolean;
    begin
      exit DelphiString(Value1) < Value2;
    end;
    class operator GreaterOrEqual(Value2: DelphiString; Value1: Char): Boolean;
    begin
      exit DelphiString(Value1) >= Value2;
    end;
    class operator LessOrEqual(Value2: DelphiString; Value1: Char): Boolean;
    begin
      exit DelphiString(Value1) <= Value2;
    end;

    class property Offset: Integer read fOffset write SetOffset;

    [ToString]
    method ToString: PlatformString;
    begin
      result := fData;
    end;

    class method Compare(const StrA: DelphiString; const StrB: DelphiString): Integer; static;
    class method Compare(const StrA: DelphiString; const StrB: DelphiString; LocaleID: TLocaleID): Integer; static;
    class method Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean): Integer; static;
    class method Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static;
    class method Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions): Integer; static;
    class method Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static;
    class method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer; static;
    class method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; LocaleID: TLocaleID): Integer; static;
    class method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean): Integer; static;
    class method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer; static;
    class method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions): Integer; static;
    class method Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer; static;
    class method CompareOrdinal(const StrA: DelphiString; const StrB: DelphiString): Integer; static;
    class method CompareOrdinal(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; aLength: Integer): Integer; static;
    class method CompareText(const StrA: DelphiString; const StrB: DelphiString): Integer; static;

    class method Parse(Value: Integer): DelphiString; static;
    class method Parse(Value: Int64): DelphiString; static;
    class method Parse(Value: Boolean): DelphiString; static;
    class method Parse(Value: Extended): DelphiString; static;
    class method ToBoolean(const S: DelphiString): Boolean; static;
    class method ToInteger(const S: DelphiString): Integer; static;
    class method ToInt64(const S: DelphiString): Int64; static;
    class method ToSingle(const S: DelphiString): Double; static;
    class method ToDouble(const S: DelphiString): Double; static;
    class method ToExtended(const S: DelphiString): Double; static;
    class method LowerCase(const S: DelphiString): DelphiString; static;
    class method LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; static; partial; empty;
    class method UpperCase(const S: DelphiString): DelphiString; static;
    class method UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; static; partial; empty;

    {$IF COOPER}
    method hashCode: Integer; override;
    {$ELSEIF ECHOES OR ISLAND}
    method GetHashCode: Integer; override;
    {$ELSEIF TOFFEE}
    method hash: Foundation.NSUInteger;
    {$ENDIF}

    {$IF TOFFEE}
    method isEqual(Obj: id): Boolean;
    {$ELSE}
    method &Equals(Obj: Object): Boolean; override;
    {$ENDIF}

    method CompareTo(const strB: DelphiString): Integer;
    method Contains(const Value: DelphiString): Boolean;
    class method &Copy(const Str: DelphiString): DelphiString; static;
    method CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; aCount: Integer);
    method CountChar(const C: Char): Integer;
    method DeQuotedString: DelphiString;
    method DeQuotedString(const QuoteChar: Char): DelphiString;
    class method EndsText(const aSubText, aText: DelphiString): Boolean; static;
    method EndsWith(const Value: DelphiString): Boolean; inline;
    method EndsWith(const Value: DelphiString; IgnoreCase: Boolean): Boolean;
    method &Equals(const Value: DelphiString): Boolean;
    class method &Equals(Value1, Value2: DelphiString): Boolean;
    //method Format(const Format: DelphiString; const args: array of const): DelphiString; overload; static; // TODO
    method IndexOf(Value: Char): Integer;
    method IndexOf(const Value: DelphiString): Integer;
    method IndexOf(Value: Char; StartIndex: Integer): Integer;
    method IndexOf(const Value: DelphiString; StartIndex: Integer): Integer;
    method IndexOf(Value: Char; StartIndex: Integer; aCount: Integer): Integer;
    method IndexOf(const Value: DelphiString; StartIndex: Integer; aCount: Integer): Integer;
    method IndexOfAny(const AnyOf: array of Char): Integer;
    method IndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer;
    method IndexOfAny(      AnyOf: array of Char; StartIndex: Integer; aCount: Integer): Integer;
    method Insert(StartIndex: Integer; const Value: DelphiString): DelphiString;
    method IsDelimiter(const Delimiters: DelphiString; aIndex: Integer): Boolean;
    method IsEmpty: Boolean;
    class method IsNullOrEmpty(const Value: DelphiString): Boolean; static;
    class method IsNullOrWhiteSpace(const Value: DelphiString): Boolean; static;
    class method &Join(Separator: DelphiString; Values: array of DelphiString): DelphiString; static;
    class method &Join(Separator: DelphiString; Values: sequence of DelphiString): DelphiString; static;
    class method &Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; aCount: Integer): DelphiString; static;
    method LastDelimiter(const Delims: DelphiString): Integer;
    method LastIndexOf(Value: Char): Integer;
    method LastIndexOf(const Value: DelphiString): Integer;
    method LastIndexOf(Value: Char; StartIndex: Integer): Integer;
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer): Integer;
    method LastIndexOf(Value: Char; StartIndex: Integer; aCount: Integer): Integer;
    method LastIndexOf(const Value: DelphiString; StartIndex: Integer; aCount: Integer): Integer;
    method LastIndexOfAny(const AnyOf: array of Char): Integer;
    method LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer;
    method LastIndexOfAny(      AnyOf: array of Char; StartIndex: Integer; aCount: Integer): Integer;
    method PadLeft(TotalWidth: Integer): DelphiString;
    method PadLeft(TotalWidth: Integer; PaddingChar: Char): DelphiString;
    method PadRight(TotalWidth: Integer): DelphiString;
    method PadRight(TotalWidth: Integer; PaddingChar: Char): DelphiString;
    method QuotedString: DelphiString;
    method QuotedString(const QuoteChar: Char): DelphiString;
    method &Remove(StartIndex: Integer): DelphiString; inline;
    method &Remove(StartIndex: Integer; aCount: Integer): DelphiString;
    method Replace(OldChar: Char; NewChar: Char): DelphiString;
    method Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString; inline;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString): DelphiString;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString;

    method Split(const Separator: array of Char): array of DelphiString; inline;
    method Split(const Separator: array of Char; aCount: Integer): array of DelphiString; inline;
    method Split(const Separator: array of Char; Options: TStringSplitOptions): array of DelphiString; inline;
    method Split(      Separator: array of Char; aCount: Integer; Options: TStringSplitOptions): array of DelphiString;
    method Split(const Separator: array of DelphiString): array of DelphiString; inline;
    method Split(const Separator: array of DelphiString; aCount: Integer): array of DelphiString;
    method Split(const Separator: array of DelphiString; Options: TStringSplitOptions): array of DelphiString;
    method Split(const Separator: array of DelphiString; aCount: Integer; Options: TStringSplitOptions): array of DelphiString;

    method StartsWith(const Value: DelphiString): Boolean;
    method StartsWith(const Value: DelphiString; IgnoreCase: Boolean): Boolean;
    method SubString(StartIndex: Integer): DelphiString;
    method SubString(StartIndex: Integer; aLength: Integer): DelphiString;
    method ToBoolean: Boolean;
    method ToInteger: Integer;
    method ToInt64: Int64;
    method ToSingle: Double;
    method ToDouble: Double;
    method ToExtended: Extended;
    method ToCharArray: array of Char;
    method ToCharArray(StartIndex: Integer; aLength: Integer): array of Char;
    method ToByteArray: array of Byte;
    method ToByteArray(StartIndex: Integer; aLength: Integer): array of Byte;
    method ToLower: DelphiString;
    method ToLower(LocaleID: TLocaleID): DelphiString;
    method ToLowerInvariant: DelphiString;
    method ToUpper: DelphiString;
    method ToUpper(LocaleID: TLocaleID): DelphiString;
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

    const Empty = '';
  end;

  {$IF ECHOES}
  DelphiString = public partial record(/*System.Collections.ICollection, */System.Collections.Generic.ICollection<Char>)
  private

    method &Add(item: Char); empty;
    method Clear; empty;
    method &Remove(item: Char): Boolean; empty;
    private property Count: Integer read Length;
    property IsReadOnly: Boolean read true;

    method Contains(item: Char): Boolean;
    begin
      result := IndexOf(item) > -1;
    end;

    method CopyTo(&array: array of Char; arrayIndex: Integer);
    begin
      for i: Integer := 1 to Length do
        &array[arrayIndex+i] := self[i];
    end;

    [&Sequence]
    method GetSequence: sequence of Char; iterator;
    begin
      for i: Integer := 1 to Length do
        yield self[i];
    end;
  end;
  {$ENDIF}

{$IF ISLAND AND WINDOWS}
extension method RemObjects.Elements.System.String.ToLPCWSTR: rtl.LPCWSTR;
begin
  if String.IsNullOrEmpty(self) then exit nil;
  var arr := ToCharArray(true);
  exit rtl.LPCWSTR(@arr[0]);
end;

extension method DelphiString.ToLPCWSTR: rtl.LPCWSTR;
begin
  if self.Length = 0 then exit nil;
  var arr := PlatformString(self).ToCharArray(true);
  exit rtl.LPCWSTR(@arr[0]);
end;
{$ENDIF}

implementation

constructor DelphiString;
begin
  fData := InternalCreate;
end;

constructor DelphiString(aBytes: array of Byte);
begin
  fData := Encoding.UTF16LE.GetString(aBytes);
end;

class method DelphiString.Create(C: Char; aCount: Integer): DelphiString;
begin
  result := CreateWithChars(C, aCount);
end;

class method DelphiString.Create(const Value: array of Char; StartIndex: Integer; ALength: Integer): DelphiString;
begin
  result := CreateFromArray(Value, StartIndex, ALength);
end;

class method DelphiString.Create(const Value: array of Char): DelphiString;
begin
  result := &Create(Value, 0, Value.length);
end;

class method DelphiString.Create(aBytes: array of Byte): DelphiString;
begin
  result := Encoding.UTF16LE.GetString(aBytes);
end;

class method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [], TLanguages.UserDefaultLocale);
end;

class method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [], LocaleID);
end;

class method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean): Integer;
begin
  if IgnoreCase then
    result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [TCompareOptions.coIgnoreCase], TLanguages.UserDefaultLocale)
  else
    result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, [], TLanguages.UserDefaultLocale)
end;

class method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, IgnoreCase, LocaleID);
end;

class method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, Options, TLanguages.UserDefaultLocale);
end;

class method DelphiString.Compare(const StrA: DelphiString; const StrB: DelphiString; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, 0, StrB, 0, StrA.Length, StrB.Length, False, LocaleID);
end;

class method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, False, TLanguages.UserDefaultLocale);
end;

class method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, False, TLanguages.UserDefaultLocale);
end;

class method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, IgnoreCase, TLanguages.UserDefaultLocale);
end;

class method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer;
begin
  result := Compare(StrA, IndexA, StrB, IndexB, ALength, IgnoreCase, LocaleID);
end;

class method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, Options, TLanguages.UserDefaultLocale);
end;

class method DelphiString.Compare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; ALength: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
begin
  result := InternalCompare(StrA, IndexA, StrB, IndexB, ALength, ALength, Options, LocaleID);
end;

class method DelphiString.InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB, LengthA, LengthB: Integer; IgnoreCase: Boolean; LocaleID: TLocaleID): Integer;
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
    if Chars[i] = C then
      inc(result);
end;

method DelphiString.DeQuotedString: DelphiString;
begin
  result := DeQuotedString('''');
end;

class method DelphiString.EndsText(const aSubText, aText: DelphiString): Boolean;
begin
  result := aText.EndsWith(aSubText, true);
end;

method DelphiString.EndsWith(const Value: DelphiString): Boolean;
begin
  result := EndsWith(Value, false);
end;

method DelphiString.Equals(const Value: DelphiString): Boolean;
begin
  result := self.fData = Value.fData;
end;

class method DelphiString.Equals(Value1, Value2: DelphiString): Boolean;
begin
  result := Value1 = Value2;
end;

method DelphiString.QuotedString: DelphiString;
begin
  result := QuotedString('''');
end;

method DelphiString.QuotedString(const QuoteChar: Char): DelphiString;
begin
  result := DelphiString.Copy(self);
  for i: Integer := fData.Length - 1 downto 0 do
    if result.Chars[i] = QuoteChar then
      result.Insert(i, QuoteChar);
  result := QuoteChar + result + QuoteChar;
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char): DelphiString;
begin
  {$IF COOPER OR ECHOES OR ISLAND}
  result := Replace(PlatformString(OldChar), PlatformString(NewChar), [TReplaceFlags.rfReplaceAll]);
  {$ELSEIF TOFFEE}
  result := Replace(Foundation.NSString.stringWithFormat("%c", OldChar), Foundation.NSString.stringWithFormat("%c", NewChar), [TReplaceFlags.rfReplaceAll]);
  {$ENDIF}
end;

method DelphiString.Replace(const OldValue: DelphiString; const NewValue: DelphiString): DelphiString;
begin
  result := Replace(OldValue, NewValue, [TReplaceFlags.rfReplaceAll]);
end;

class method DelphiString.Parse(Value: Integer): DelphiString;
begin
  result := RemObjects.Elements.RTL.Convert.ToString(Value);
end;

class method DelphiString.Parse(Value: Int64): DelphiString;
begin
  result := RemObjects.Elements.RTL.Convert.ToString(Value);
end;

class method DelphiString.Parse(Value: Boolean): DelphiString;
begin
  result := RemObjects.Elements.RTL.Convert.ToString(Value);
end;

class method DelphiString.Parse(Value: Extended): DelphiString;
begin
  result := RemObjects.Elements.RTL.Convert.ToString(Value);
end;

class method DelphiString.ToBoolean(const S: DelphiString): Boolean;
begin
  result := RemObjects.Elements.RTL.Convert.ToBoolean(S);
end;

class method DelphiString.ToInteger(const S: DelphiString): Integer;
begin
  result := RemObjects.Elements.RTL.Convert.ToInt32(S);
end;

class method DelphiString.ToInt64(const S: DelphiString): Int64;
begin
  result := RemObjects.Elements.RTL.Convert.ToInt64(S);
end;

class method DelphiString.ToSingle(const S: DelphiString): Double;
begin
  result := RemObjects.Elements.RTL.Convert.ToDoubleInvariant(S);
end;

class method DelphiString.ToDouble(const S: DelphiString): Double;
begin
  result := RemObjects.Elements.RTL.Convert.ToDoubleInvariant(S);
end;

class method DelphiString.ToExtended(const S: DelphiString): Double;
begin
  result := RemObjects.Elements.RTL.Convert.ToDoubleInvariant(S);
end;

method DelphiString.ToBoolean: Boolean;
begin
  result := RemObjects.Elements.RTL.Convert.ToBoolean(self);
end;

method DelphiString.ToInteger: Integer;
begin
  result := RemObjects.Elements.RTL.Convert.ToInt32(self);
end;

method DelphiString.ToInt64: Int64;
begin
  result := RemObjects.Elements.RTL.Convert.ToInt64(self);
end;

method DelphiString.ToSingle: Double;
begin
  result := RemObjects.Elements.RTL.Convert.ToDoubleInvariant(self);
end;

method DelphiString.ToDouble: Double;
begin
  result := RemObjects.Elements.RTL.Convert.ToDoubleInvariant(self);
end;

method DelphiString.ToExtended: Extended;
begin
  result := RemObjects.Elements.RTL.Convert.ToDoubleInvariant(self);
end;

class operator DelphiString.Implicit(Value: PlatformString): DelphiString;
begin
  result := new DelphiString(Value);
end;

//class operator DelphiString.Implicit(Value: RemObjects.Elements.RTL.String): DelphiString;
//begin
  //result := new DelphiString(Value);
//end;

class operator DelphiString.Implicit(Value: AnsiString): DelphiString;
begin
  var lSource := Value.Data;
  var lArray := new Char[lSource.Length];
  for i: Integer := 0 to lSource.Length - 1 do
    lArray[i] := chr(lSource[i]);

  result := DelphiString.Create(lArray);
end;

{$IF NOT COOPER}
class operator DelphiString.Implicit(Value: PChar): DelphiString;
begin
  if assigned(Value) then begin
    {$IF ECHOES}
    var len := StrLen(Value);
    var lBytes := new Byte[len*2];
    var lSource := ^Byte(Value);
    for i: Integer := 0 to (len-1)*2 do
      lBytes[i] := (lSource+i)^;
    result := Encoding.UTF16LE.GetString(lBytes);
    {$ELSEIF ISLAND}
    result := PlatformString.FromPChar(Value);
    {$ELSEIF TOFFEE}
    var len := StrLen(Value);
    result := Foundation.NSString.stringWithCharacters(value) length(len);
    {$ENDIF}
  end;
end;
{$ENDIF}

operator DelphiString.Implicit(Value: Char): DelphiString;
begin
  {$IF COCOA}
  //76216: Toffee: can't call `Object` extension methods on value types
  result := NSString.stringWithFormat("%c", Value);
  {$ELSE}
  result := Value.ToString();
  {$ENDIF}
end;

class operator DelphiString.Implicit(Value: DelphiString): PlatformString;
begin
  result := Value.fData;
end;

//class operator DelphiString.Implicit(Value: DelphiString): RemObjects.Elements.RTL.String;
//begin
  //result := Value.fData;
//end;

class operator DelphiString.Implicit(Value: DelphiString): AnsiString;
begin
  result := Value.fData;
end;

constructor DelphiString(Value: PlatformString);
begin
  fData := InternalCreate(Value);
end;

constructor DelphiString(C: Char; aCount: Integer);
begin
  self := CreateWithChars(C, aCount);
end;

class operator DelphiString.Add(Value1: DelphiString; Value2: Char): DelphiString;
begin
  {$IF COOPER OR ECHOES OR ISLAND}
  result := new DelphiString(Value1 + DelphiString(Value2));
  {$ELSEIF TOFFEE}
  result := new DelphiString(NSString(Value1) + NSString.stringWithFormat("%c", Value2));
  {$ENDIF}
end;

class operator DelphiString.Add(Value1: Char; Value2: DelphiString): DelphiString;
begin
  {$IF COOPER OR ECHOES OR ISLAND}
  result := new DelphiString(DelphiString(Value1) + Value2);
  {$ELSEIF TOFFEE}
  result := new DelphiString(NSString.stringWithFormat("%c", Value1) + NSString(Value2));
  {$ENDIF}
end;

class operator DelphiString.Add(Value1: DelphiString; Value2: DelphiString): not nullable DelphiString;
begin
  {$IF COOPER}
  result := (java.lang.String(Value1) + java.lang.String(Value2)) as not nullable;
  {$ELSEIF ECHOES}
  result := (System.String(Value1) + System.String(Value2)) as not nullable;
  {$ELSEIF ISLAND}
  result := PlatformString(Value1) + PlatformString(Value2) as not nullable;
  {$ELSEIF TOFFEE}
  result := (Foundation.NSString(Value1) + Foundation.NSString(Value2)) as not nullable;
  {$ENDIF}
end;

(*
class operator DelphiString.Add(Value1: DelphiString; Value2: Object): not nullable DelphiString;
begin
  //result := Value1 + Value2.ToString;  // TODO compiler error
  result := Value1;
end;

class operator DelphiString.Add(Value1: Object; Value2: DelphiString): not nullable DelphiString;
begin
 //result := Value1.ToString + Value2;  // TODO compiler error
end;
*)

class operator DelphiString.Equal(Value1: DelphiString; Value2: DelphiString): Boolean;
begin
  result := Value1.fData = Value2.fData;
end;

class operator DelphiString.NotEqual(Value1, Value2: DelphiString): Boolean;
begin
  result := Value1.fData <> Value2.fData;
end;

class operator DelphiString.Greater(Value1, Value2: DelphiString): Boolean;
begin
  result := Value1.fData > Value2.fData;
end;

class operator DelphiString.Less(Value1, Value2: DelphiString): Boolean;
begin
  result := Value1.fData < Value2.fData;
end;

class operator DelphiString.GreaterOrEqual(Value1, Value2: DelphiString): Boolean;
begin
  result := Value1.fData >= Value2.fData;
end;

class operator DelphiString.LessOrEqual(Value1, Value2: DelphiString): Boolean;
begin
  result := Value1.fData <= Value2.fData;
end;

class method DelphiString.PlatformArrayToStringArray(Value: array of PlatformString; aCount: Integer := -1): array of DelphiString;
begin
  var lTotal: Integer;
  if aCount = -1 then
    lTotal := Value.length - 1
  else
    lTotal := aCount - 1;

  result := new DelphiString[lTotal + 1];
  for i: Integer := 0 to lTotal do
    result[i] := Value[i];
end;

class method DelphiString.StringArrayToPlatformArray(Value: array of DelphiString): array of PlatformString;
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
  result := if fData <> nil then fData.Length else 0;
end;

class method DelphiString.LowerCase(const S: DelphiString): DelphiString;
begin
  result := S.fData.ToLower;
end;

class method DelphiString.UpperCase(const S: DelphiString): DelphiString;
begin
  result := S.fData.ToUpper;
end;

method DelphiString.CompareTo(const strB: DelphiString): Integer;
begin
  result := fData.CompareTo(strB);
end;

method DelphiString.Contains(const Value: DelphiString): Boolean;
begin
  result := fData.Contains(Value.fData);
end;

class method DelphiString.Copy(const Str: DelphiString): DelphiString;
begin
  result := Str.fData as DelphiString;
end;

method DelphiString.CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; aCount: Integer);
begin
  {$IF COOPER}
  for i: Integer := 0 to aCount - 1 do
    destination[DestinationIndex + i] := self.chars[SourceIndex + i];
  {$ELSEIF ECHOES OR ISLAND}
  PlatformString(fData).CopyTo(SourceIndex, destination, DestinationIndex, aCount);
  {$ELSEIF TOFFEE}
  PlatformString(fData).getCharacters(@destination[DestinationIndex]) range(Foundation.NSMakeRange(SourceIndex, aCount));
  {$ENDIF}
end;

method DelphiString.DeQuotedString(const QuoteChar: Char): DelphiString;
begin
  if (fData.Length >= 2) and (Chars[0] = QuoteChar) and (Chars[fData.Length - 1] = QuoteChar) then begin
    var lSb := new Char[fData.Length - 2];
    var lTotal := 0;
    var lInQuote: Boolean := false;
    for i: Integer := 1 to fData.Length - 2 do begin
      var lChar := Chars[i];
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

method DelphiString.EndsWith(const Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  result := fData.EndsWith(Value, IgnoreCase);
end;

method DelphiString.IndexOf(Value: Char): Integer;
begin
  result := fData.IndexOf(Value);
end;

method DelphiString.IndexOf(const Value: DelphiString): Integer;
begin
  result := fData.IndexOf(Value);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := fData.IndexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(const Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.IndexOf(Value, StartIndex);
end;

method DelphiString.IndexOf(Value: Char; StartIndex: Integer; aCount: Integer): Integer;
begin
  {$IF COOPER OR ISLAND}
  var lIndex := PlatformString(fData).indexOf(Value, StartIndex);
  result := if (lIndex >= StartIndex) and (lIndex <= (StartIndex + aCount - 1)) then lIndex else -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).IndexOf(Value, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  result := IndexOf(DelphiString(Value), StartIndex, aCount);
  {$ENDIF}
end;

method DelphiString.IndexOf(const Value: DelphiString; StartIndex: Integer; aCount: Integer): Integer;
begin
  {$IF COOPER OR ISLAND}
  var lIndex := PlatformString(fData).indexOf(Value, StartIndex);
  result := if (lIndex >= StartIndex) and (lIndex <= (StartIndex + aCount - 1)) then lIndex else -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).IndexOf(Value, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  var lRange:= PlatformString(fData).rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch) range(Foundation.NSMakeRange(StartIndex, aCount));
  result := if lRange.location <> NSNotFound then Integer(lRange.location) else -1;
  {$ENDIF}
end;

method DelphiString.IndexOfAny(const AnyOf: array of Char): Integer;
begin
  {$IF ISLAND}
  result := IndexOfAny(AnyOf, 0, fData.Length);
  {$ELSE}
  result := fData.IndexOfAny(AnyOf);
  {$ENDIF}
end;

method DelphiString.IndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  {$IF ISLAND}
  result := IndexOfAny(AnyOf, StartIndex, fData.Length);
  {$ELSE}
  result := fData.IndexOfAny(AnyOf, StartIndex);
  {$ENDIF}
end;

method DelphiString.IndexOfAny(AnyOf: array of Char; StartIndex: Integer; aCount: Integer): Integer;
begin
  {$IF COOPER OR ISLAND}
  var lEnd: Integer;
  if (StartIndex + aCount) >= fData.length then
    lEnd := fData.length
  else
    lEnd := StartIndex + aCount;
  for i: Integer := StartIndex to lEnd - 1 do begin
     for each c: Char in AnyOf do begin
       if Chars[i] = c then
         exit i;
     end;
  end;
  result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).IndexOfAny(AnyOf, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  var lChars := Foundation.NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@AnyOf) length(AnyOf.length));
  var lRange := PlatformString(fData).rangeOfCharacterFromSet(lChars) options(NSStringCompareOptions.NSLiteralSearch) range(Foundation.NSMakeRange(StartIndex, aCount));
  result := if lRange.location <> NSNotFound then Integer(lRange.location) else -1;
  {$ENDIF}
end;

method DelphiString.Insert(StartIndex: Integer; const Value: DelphiString): DelphiString;
begin
  {$IF COOPER}
  var sb := new StringBuilder(fData);
  sb.insert(StartIndex, Value);
  fData := sb.toString;
  {$ELSEIF ECHOES OR ISLAND}
  fData := PlatformString(fData).Insert(StartIndex, Value);
  {$ELSEIF TOFFEE}
  var lString := new NSMutableString withCapacity(fData.Length);
  lString.setString(fData);
  lString.insertString(Value) atIndex(StartIndex);
  fData := lString;
  {$ENDIF}
  result := fData;
end;

method DelphiString.IsDelimiter(const Delimiters: DelphiString; aIndex: Integer): Boolean;
begin
  for i: Integer := 0 to Delimiters.Length - 1 do
    if Chars[aIndex] = Delimiters.Chars[i] then
    exit(true);

  result := false;
end;

method DelphiString.IsEmpty: Boolean;
begin
  result := fData.Length = 0;
end;

class method DelphiString.IsNullOrEmpty(const Value: DelphiString): Boolean;
begin
  result := (Value.fData = nil) or (String.IsNullOrEmpty(Value.fData));
end;

class method DelphiString.IsNullOrWhiteSpace(const Value: DelphiString): Boolean;
begin
  {$IF ISLAND}
  result := (PlatformString(Value.fData) = nil) or (Value.fData = ' ') or (Value.Length = 0);
  {$ELSE}
  result := String.IsNullOrWhiteSpace(Value);
  {$ENDIF}
end;

class method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString): DelphiString;
begin
  result := &Join(Separator, Values, 0, Values.length);
end;

class method DelphiString.Join(Separator: DelphiString; Values: sequence of DelphiString): DelphiString;
begin
  if Values <> nil then begin
    var lSb := new TStringBuilder();
    for each lValue in Values do
      if lSb.Length = 0 then
        lSb.Append(lValue)
      else begin
        lSb.Append(Separator);
        lSb.Append(lValue);
      end;
      result := lSb.ToString;
  end
  else
    result := '';
end;

class method DelphiString.Join(Separator: DelphiString; Values: array of DelphiString; StartIndex: Integer; aCount: Integer): DelphiString;
begin
  {$IF COOPER}
  var sb := new StringBuilder;
  for i: Integer := StartIndex to (StartIndex + aCount) - 1 do begin
     if i <> StartIndex then
      sb.append(Separator);
    sb.append(Values[i]);
  end;
  result := sb.toString;
  {$ELSEIF ECHOES OR ISLAND}
  var lArray := StringArrayToPlatformArray(Values);
  result := PlatformString.Join(Separator, lArray, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  var lArray := new NSMutableArray withCapacity(Values.length);
  for i: Integer := StartIndex to (StartIndex + aCount) - 1 do
    lArray.addObject(Values[i].fData);

  result := lArray.componentsJoinedByString(Separator.fData);
  {$ENDIF}
end;

method DelphiString.LastDelimiter(const Delims: DelphiString): Integer;
begin
  var lArray := Delims.ToCharArray;
  result := LastIndexOfAny(lArray);
end;

method DelphiString.LastIndexOf(Value: Char): Integer;
begin
  result := fData.LastIndexOf(Value);
end;

method DelphiString.LastIndexOf(const Value: DelphiString): Integer;
begin
  result := fData.LastIndexOf(Value);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer): Integer;
begin
  result := fData.LastIndexOf(Value, StartIndex);
end;

method DelphiString.LastIndexOf(const Value: DelphiString; StartIndex: Integer): Integer;
begin
  result := fData.LastIndexOf(Value, StartIndex);
end;

method DelphiString.LastIndexOf(Value: Char; StartIndex: Integer; aCount: Integer): Integer;
begin
  {$IF COOPER OR ISLAND}
  result := PlatformString(fData).lastIndexOf(Value, StartIndex);
  if result < (StartIndex - aCount) + 1 then
    result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOf(Value, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  result := LastIndexOf(DelphiString(Value), StartIndex, aCount);
  {$ENDIF}
end;

method DelphiString.LastIndexOf(const Value: DelphiString; StartIndex: Integer; aCount: Integer): Integer;
begin
  {$IF COOPER OR ISLAND}
  result := PlatformString(fData).lastIndexOf(Value, StartIndex);
  if result < (StartIndex - aCount) + 1 then
    result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOf(Value, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  var lRange:= PlatformString(fData).rangeOfString(Value) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch) range(Foundation.NSMakeRange((StartIndex - aCount) + 1, aCount));
  result := if lRange.location <> NSNotFound then Integer(lRange.location) else -1;
  {$ENDIF}
end;

method DelphiString.LastIndexOfAny(const AnyOf: array of Char): Integer;
begin
  {$IF COOPER OR TOFFEE OR ISLAND}
  result := LastIndexOfAny(AnyOf, fData.Length - 1, fData.Length);
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOfAny(AnyOf);
  {$ENDIF}
end;

method DelphiString.LastIndexOfAny(const AnyOf: array of Char; StartIndex: Integer): Integer;
begin
  {$IF COOPER OR TOFFEE OR ISLAND}
  result := LastIndexOfAny(AnyOf, StartIndex, StartIndex + 1);
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOfAny(AnyOf, StartIndex);
  {$ENDIF}
end;

method DelphiString.LastIndexOfAny(AnyOf: array of Char; StartIndex: Integer; aCount: Integer): Integer;
begin
  {$IF COOPER OR ISLAND}
  var lStart: Integer;
  if (StartIndex >= 0) and (StartIndex < fData.length) then
    lStart := StartIndex
  else
    lStart := fData.length;
  var lEnd: Integer := lStart - aCount + 1;
  if lEnd < 0 then
    lEnd := 0;

  for i: Integer := lStart downto lEnd do begin
    var lChar := Chars[i];
    for each c: Char in AnyOf do
      if lChar = c then
        exit i;
  end;
  result := -1;
  {$ELSEIF ECHOES}
  result := PlatformString(fData).LastIndexOfAny(AnyOf, StartIndex, aCount);
  {$ELSEIF TOFFEE}
  var lChars := Foundation.NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@AnyOf) length(AnyOf.length));
  var lRange := PlatformString(fData).rangeOfCharacterFromSet(lChars) options(NSStringCompareOptions.NSLiteralSearch or NSStringCompareOptions.NSBackwardsSearch) range(Foundation.NSMakeRange((StartIndex - aCount) + 1, aCount));
  result := if lRange.location <> NSNotFound then Integer(lRange.location) else -1;
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

method DelphiString.Remove(StartIndex: Integer; aCount: Integer): DelphiString;
begin
  if StartIndex >= fData.Length then
    exit fData;

  var lCount := if (StartIndex + aCount) > fData.Length then (fData.Length - StartIndex) else aCount;
  {$IF COOPER}
  var lSb := new StringBuilder(fData);
  lSb.delete(StartIndex, lCount);
  fData := lSb.toString;
  {$ELSEIF ECHOES OR ISLAND}
  fData := PlatformString(fData).Remove(StartIndex, lCount);
  {$ELSEIF TOFFEE}
  var lString := new NSMutableString withCapacity(fData.Length);
  lString.setString(fData);
  lString.deleteCharactersInRange(NSMakeRange(StartIndex, lCount));
  fData := lString;
  {$ENDIF}
  result := fData;
end;

method DelphiString.Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): DelphiString;
begin
  {$IF COOPER OR ECHOES OR ISLAND}
  result := Replace(PlatformString(OldChar), PlatformString(NewChar), ReplaceFlags);
  {$ELSEIF TOFFEE}
  //76424: Toffee: cannot cast char to string — once fixed, the above should work for all platforms.
  result := Replace(Foundation.NSString.stringWithFormat("%c", OldChar), Foundation.NSString.stringWithFormat("%c", NewChar), ReplaceFlags);
  {$ENDIF}
end;

method DelphiString.Replace(const OldValue: DelphiString; const NewValue: DelphiString; ReplaceFlags: TReplaceFlags): DelphiString;
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
  if TReplaceFlags.rfIgnoreCase in ReplaceFlags then
    lOptions := System.Text.RegularExpressions.RegexOptions.IgnoreCase;
  if TReplaceFlags.rfReplaceAll in ReplaceFlags then
    lCount := -1;

  var lRegEx := new System.Text.RegularExpressions.Regex(OldValue.fData, lOptions);
  result := lRegEx.Replace(fData, System.Text.RegularExpressions.Regex.Escape(NewValue.fData), lCount);
  {$ELSEIF ISLAND}
  var lOldValue: DelphiString := '';
  var lToParse: DelphiString := '';
  if TReplaceFlags.rfIgnoreCase in ReplaceFlags then begin
    lOldValue := OldValue.ToUpper;
    lToParse := fData.ToUpper;
  end
  else begin
    lOldValue := OldValue;
    lToParse := fData;
  end;

  result := self;
  var lReplaceAll := not (TReplaceFlags.rfReplaceAll in ReplaceFlags);
  var oldValue_Length := OldValue.Length;
  var lStart := 0;
  repeat
    lStart := lToParse.IndexOf(lOldValue, lStart);
    if lStart <> -1 then begin
      var lRest: DelphiString := '';
      if lStart + oldValue_Length < result.Length then lRest := result.SubString(lStart + oldValue_Length);
      result := result.SubString(0, lStart) + NewValue + lRest;
      inc(lStart, oldValue_Length);
    end
    else
       break;
  until lReplaceAll;
  {$ELSEIF TOFFEE}
  var lOptions: NSStringCompareOptions := 0;
  var lRange: Foundation.NSRange;

  if TReplaceFlags.rfIgnoreCase in ReplaceFlags then
    lOptions := NSStringCompareOptions.NSCaseInsensitiveSearch;
  if not (TReplaceFlags.rfReplaceAll in ReplaceFlags) then begin
    lRange := PlatformString(fData).rangeOfString(OldValue) options(lOptions);
    if lRange.location = NSNotFound then
      exit self;
  end
  else
    lRange := Foundation.NSMakeRange(0, fData.Length);
  result := PlatformString(fData).stringByReplacingOccurrencesOfString(OldValue.fData) withString(NewValue.fData) options(lOptions) range(lRange)
  {$ENDIF}
end;

{$IF COOPER}
class method DelphiString.ArrayToSplitRegex(Value: array of Char): PlatformString;
begin
  result := '';
  for i: Integer := 0 to Value.length - 1 do
    if i <> 0 then
      result := result + '|' + java.util.regex.Pattern.quote(Value[i])
    else
      result := java.util.regex.Pattern.quote(Value[i])
end;

class method DelphiString.ArrayToSplitRegex(Value: array of DelphiString): PlatformString;
begin
  result := '';
  for i: Integer := 0 to Value.length - 1 do
    if i <> 0 then
      result := result + '|' + java.util.regex.Pattern.quote(Value[i])
    else
      result := java.util.regex.Pattern.quote(Value[i])
end;

class method DelphiString.PlatformCharArrayToCharArray(Value: array of Char; StartIndex: Integer := -1; ALength: Integer := -1): array of Char;
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

method DelphiString.Split(const Separator: array of Char): array of DelphiString;
begin
  result := Split(Separator, -1, TStringSplitOptions.None);
end;

method DelphiString.Split(const Separator: array of Char; aCount: Integer): array of DelphiString;
begin
  result := Split(Separator, aCount, TStringSplitOptions.None);
end;

method DelphiString.Split(const Separator: array of Char; Options: TStringSplitOptions): array of DelphiString;
begin
  result := Split(Separator, -1, Options);
end;

method DelphiString.Split(Separator: array of Char; aCount: Integer; Options: TStringSplitOptions): array of DelphiString;
begin
  {$IF COOPER}
  var lSep := ArrayToSplitRegex(Separator);
  var lArray := PlatformString(fData).split(lSep, aCount);
  if Options = TStringSplitOptions.ExcludeEmpty then begin
    var lList: java.util.List := new java.util.ArrayList;
    for i: Integer := 0 to lArray.Length - 1 do begin
      if (i = aCount - 2) and (DelphiString(lArray[i + 1]).IndexOfAny(Separator) = 0) then
          lArray[i + 1] := lArray[i + 1].substring(1);

        if lArray[i] <> '' then
          lList.add(lArray[i])
      end;
    lArray := lList.ToArray(new PlatformString[0]);
  end;
  result := PlatformArrayToStringArray(lArray, aCount);
  {$ELSEIF ECHOES}
  var lArray: array of PlatformString;
  var lCount := if aCount = -1 then Consts.MaxInt32 else aCount;
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := PlatformString(fData).Split(Separator, lCount, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := PlatformString(fData).Split(Separator, lCount);

  result := PlatformArrayToStringArray(lArray);
  {$ELSEIF ISLAND}
  var lSeparators := new DelphiString[Separator.length];
  for i: Integer := 0 to Separator.length - 1 do
    lSeparators[i] := DelphiString(Separator[i]);

  result := Split(lSeparators, aCount, Options);
  {$ELSEIF TOFFEE}
  if aCount <> -1 then begin
    var lArray := new DelphiString[Separator.length];
    for i: Integer := 0 to Separator.length - 1 do
      lArray[i] := DelphiString(Separator[i]);

    result := Split(lArray, aCount, Options);
  end
  else begin
    var lChars := Foundation.NSCharacterSet.characterSetWithCharactersInString(new Foundation.NSString withCharacters(@Separator) length(Separator.length));
    var lArray := PlatformString(fData).componentsSeparatedByCharactersInSet(lChars);
    if Options = TStringSplitOptions.ExcludeEmpty then
      lArray := lArray.filteredArrayUsingPredicate(NSPredicate.predicateWithFormat('length > 0'));

    result := NSArrayToStringArray(lArray, aCount);
  end;
  {$ENDIF}
end;

method DelphiString.Split(const Separator: array of DelphiString): array of DelphiString;
begin
  result := Split(Separator, -1, TStringSplitOptions.None);
end;

method DelphiString.Split(const Separator: array of DelphiString; aCount: Integer): array of DelphiString;
begin
  result := Split(Separator, aCount, TStringSplitOptions.None);
end;

method DelphiString.Split(const Separator: array of DelphiString; Options: TStringSplitOptions): array of DelphiString;
begin
  result := Split(Separator, -1, Options);
end;

{$IF TOFFEE OR COOPER OR ISLAND}
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
{$ENDIF}

{$IF TOFFEE}
method DelphiString.NSArrayToStringArray(Value: NSArray; aCount: Integer): array of DelphiString;
begin
  var lTotal: Integer;
  if aCount = -1 then
    lTotal := Value.Count
  else
    lTotal := aCount;

  result := new DelphiString[lTotal];
  for i: Integer := 0 to lTotal - 1 do
    result[i] := NSString(Value[i]);
end;
{$ENDIF}

method DelphiString.Split(const Separator: array of DelphiString; aCount: Integer; Options: TStringSplitOptions): array of DelphiString;
begin
  {$IF COOPER}
  var lSep := ArrayToSplitRegex(Separator);
  var lArray := PlatformString(fData).split(lSep, aCount);

  if Options = TStringSplitOptions.ExcludeEmpty then begin
    var lCurrentLength := 0;
    var lList: java.util.List := new java.util.ArrayList;
    for i: Integer := 0 to lArray.Length - 1 do begin
      if (i = aCount - 2) and (DelphiString(lArray[i + 1]).InternalIndexOfAny(Separator, 0, var lCurrentLength) = 0) then
        lArray[i + 1] := lArray[i + 1].substring(lCurrentLength);

      if not lArray[i].isEmpty then
        lList.add(lArray[i]);
    end;
    lArray := lList.ToArray(new PlatformString[0]);
  end;

  result := PlatformArrayToStringArray(lArray, aCount);
  {$ELSEIF ECHOES}
  var lArray: array of PlatformString;
  var lSep := StringArrayToPlatformArray(Separator);
  var lCount := if aCount = -1 then Consts.MaxInt32 else aCount;
  if Options = TStringSplitOptions.ExcludeEmpty then
    lArray := PlatformString(fData).Split(lSep, lCount, [StringSplitOptions.RemoveEmptyEntries])
  else
    lArray := PlatformString(fData).Split(lSep, lCount, StringSplitOptions.None);

  result := PlatformArrayToStringArray(lArray);
  {$ELSEIF TOFFEE OR ISLAND}
  var lTotal := 0;
  var lIndex := -1;
  var lList := new TList<DelphiString>();
  var lCurrentLength := 0;
  var lStartIndex := 0;
  var lStr: DelphiString := '';
  var lCount := if aCount = -1 then {$IF ISLAND}Integer.MaxValue{$ELSE}Consts.MaxInt32{$ENDIF} else aCount;

  lIndex := InternalIndexOfAny(Separator, lStartIndex, var lCurrentLength);
  while (lIndex >= 0) and (lTotal < lCount) do
  begin
    if (lTotal = lCount - 1) and (not ((lIndex - lStartIndex = 0 ) and (Options = TStringSplitOptions.ExcludeEmpty))) then
      lStr := SubString(lStartIndex)
    else
      lStr := SubString(lStartIndex, lIndex - lStartIndex);
    lStartIndex := lIndex + lCurrentLength;
    if (not lStr.IsEmpty) or ((lStr.IsEmpty) and (Options <> TStringSplitOptions.ExcludeEmpty)) then begin
      lList.Add(lStr);
      inc(lTotal);
    end;
    lIndex := InternalIndexOfAny(Separator, lStartIndex, var lCurrentLength);
  end;

  if (lStartIndex < Length) and (lTotal < lCount) then
    lList.Add(SubString(lStartIndex));

  result := new DelphiString[lList.Count];
  for i: Integer := 0 to lList.Count - 1 do
    result[i] := lList[i];
  {$ENDIF}
end;

method DelphiString.StartsWith(const Value: DelphiString): Boolean;
begin
  result := fData.StartsWith(Value.fData);
end;

method DelphiString.StartsWith(const Value: DelphiString; IgnoreCase: Boolean): Boolean;
begin
  result := fData.StartsWith(Value.fData, IgnoreCase);
end;

method DelphiString.SubString(StartIndex: Integer): DelphiString;
begin
  result := SubString(StartIndex, fData.Length - StartIndex);
end;

method DelphiString.SubString(StartIndex: Integer; aLength: Integer): DelphiString;
begin
  if (StartIndex >= fData.Length) then
    exit '';

  var lCount := if StartIndex + ALength > fData.Length then fData.Length - StartIndex else ALength;
  result := fData.Substring(StartIndex, lCount);
end;

method DelphiString.ToCharArray: array of Char;
begin
  result := fData.ToCharArray;
end;

method DelphiString.ToCharArray(StartIndex: Integer; aLength: Integer): array of Char;
begin
  {$IF COOPER}
  var lArray :=  fData.toCharArray;
  result := PlatformCharArrayToCharArray(lArray, StartIndex, aLength);
  {$ELSEIF ECHOES OR ISLAND}
  result := PlatformString(fData).ToCharArray(StartIndex, aLength);
  {$ELSEIF TOFFEE}
  result := new Char[aLength];
  var lArray := new Char[aLength];
   PlatformString(fData).getCharacters(lArray) range(Foundation.NSMakeRange(StartIndex, aLength));

  for i: Integer := 0 to lArray.length do
    result[i] := lArray[i];
  {$ENDIF}
end;

method DelphiString.ToByteArray: array of Byte;
begin
  result := Encoding.UTF16LE.GetBytes(fData);
end;

method DelphiString.ToByteArray(StartIndex: Integer; aLength: Integer): array of Byte;
begin
  result := Encoding.UTF16LE.GetBytes(fData.Substring(StartIndex, aLength));
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

method DelphiString.Trim(const TrimChars: array of Char): DelphiString;
begin
  result := fData.Trim(TrimChars);
end;

method DelphiString.TrimLeft(const TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimStart(TrimChars);
end;

method DelphiString.TrimRight(const TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimEnd(TrimChars);
end;

method DelphiString.TrimEnd(const TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimEnd(TrimChars);
end;

method DelphiString.TrimStart(const TrimChars: array of Char): DelphiString;
begin
  result := fData.TrimStart(TrimChars);
end;

class method DelphiString.InternalCreate: PlatformString;
begin
  {$IF COOPER}
  result := new java.lang.String;
  {$ELSEIF ECHOES}
  result := new System.String([]);
  {$ELSEIF ISLAND}
  result := PlatformString.FromPChar(nil, 0);
  {$ELSEIF TOFFEE}
  result := Foundation.NSString.string;
  {$ENDIF}
end;

class method DelphiString.InternalCreate(Value: PlatformString): PlatformString;
begin
  if Value = nil then
    Value := '';

  {$IF COOPER}
  result := new java.lang.String(Value);
  {$ELSEIF ECHOES}
  result := System.String.Copy(Value);
  {$ELSEIF ISLAND}
  result := PlatformString.FromPChar(Value.FirstChar, Value.Length);
  {$ELSEIF TOFFEE}
  result := Foundation.NSString.stringWithString(Value);
  {$ENDIF}
end;

class method DelphiString.CreateWithChars(Char: Char; aCount: Integer): DelphiString;
begin
  {$IF COOPER}
  var lChars := new Char[aCount];
  for i: Integer := 0 to aCount-1 do
    lChars[i] := Char;
  result := new java.lang.String(lChars);
  {$ELSEIF ECHOES}
  result := new System.String(Char, aCount);
  {$ELSEIF ISLAND}
  result := PlatformString.FromRepeatedChar(Char, aCount);
  {$ELSEIF TOFFEE}
  result := Foundation.NSString("").stringByPaddingToLength(aCount) withString(Foundation.NSString.stringWithFormat("%c", Char)) startingAtIndex(0);
  {$ENDIF}
end;

class method DelphiString.CreateFromArray(Value: array of Char; StartIndex: Integer; aLength: Integer): DelphiString;
begin
  {$IF COOPER}
  result := new java.lang.String(Value, StartIndex, ALength);
  {$ELSEIF ECHOES}
  result := new System.String(Value, StartIndex, ALength);
  {$ELSEIF ISLAND}
  result := PlatformString.FromPChar(@Value[StartIndex], aLength);
  {$ELSEIF TOFFEE}
  result := new Foundation.NSString withCharacters(@Value[StartIndex]) length(aLength);
  {$ENDIF}
end;

method DelphiString.GetChar(aIndex: Int32): Char;
begin
  {$IF COOPER}
  result := PlatformString(fData).charAt(aIndex);
  {$ELSEIF ECHOES}
  result := fData.Chars[aIndex];
  {$ELSEIF ISLAND}
  result := PlatformString(fData).Item[aIndex];
  {$ELSEIF TOFFEE}
  result := PlatformString(fData).characterAtIndex(aIndex);
  {$ENDIF}
end;

method DelphiString.SetChar(aIndex: Int32; aValue: Char);
begin
  {$IF COOPER}
  fData := fData.substring(0, aIndex) + aValue + fData.substring(aIndex+1);
  {$ELSEIF ECHOES OR ISLAND}
  fData := fData.Substring(0, aIndex) + aValue + fData.Substring(aIndex+1);
  {$ELSEIF TOFFEE}
  fData := PlatformString(fData).substringWithRange(Foundation.NSMakeRange(0, aIndex)) + aValue + PlatformString(fData).substringWithRange(Foundation.NSMakeRange(aIndex + 1, fData.Length - 1 - aIndex));
  {$ENDIF}
end;

class method DelphiString.InternalCompare(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; LengthA: Integer; LengthB: Integer; Options: TCompareOptions; LocaleID: TLocaleID): Integer;
begin
  {$IF COOPER}
  var Lcollator := java.text.Collator.getInstance(LocaleID);

  if ([TCompareOption.coLingIgnoreCase] in Options) or ([TCompareOption.coIgnoreCase] in Options) then
    Lcollator.setStrength(java.text.Collator.TERTIARY);

  if [TCompareOption.coLingIgnoreDiacritic] in Options then
   Lcollator.setStrength(java.text.Collator.PRIMARY);

  { create new strings only if needed }
  var lStrA: PlatformString;
  if (IndexA <> 0) or (LengthA <> StrA.Length) then begin
    var lLastA: Integer := IndexA + LengthA;
    if lLastA > StrA.Length then
      lLastA := StrA.Length
    else
      inc(lLastA);
    lStrA := PlatformString(StrA.fData).substring(0, lLastA);
  end
  else
    lStrA := StrA.fData;

  var lStrB: PlatformString;
  if (IndexB <> 0) or (LengthB <> StrB.Length) then begin
    var lLastB: Integer := (IndexB + LengthB);
    if  lLastB > StrB.Length then
      lLastB := StrB.Length
    else
      inc(lLastB);
    lStrB := PlatformString(StrB.fData).substring(0, lLastB);
  end
  else
    lStrB := StrB.fData;

  result := Lcollator.compare(lStrA, lStrB);
  {$ELSEIF ECHOES}
  var LOptions: System.Globalization.CompareOptions := 0;
  var LTotalChars: Integer;

  if TCompareOption.coLingIgnoreCase in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreCase;
  if TCompareOption.coLingIgnoreDiacritic in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreNonSpace;
  if TCompareOption.coIgnoreCase in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.OrdinalIgnoreCase;
  if TCompareOption.coIgnoreKanatype in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreKanaType;
  if TCompareOption.coIgnoreNonSpace in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreNonSpace;
  if TCompareOption.coIgnoreSymbols in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreSymbols;
  if TCompareOption.coIgnoreWidth in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.IgnoreWidth;
  if TCompareOption.coDigitAsNumbers in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.StringSort;
  if TCompareOption.coStringSort in Options then
    LOptions := LOptions or System.Globalization.CompareOptions.StringSort;

  if LengthA <= LengthB then
    LTotalChars := LengthB
  else
    LTotalChars := LengthA;

  result := System.String.Compare(StrA, IndexA, StrB, IndexB, LTotalChars, LocaleID, LOptions);
  {$ELSEIF ISLAND}
  {$IF WINDOWS}
  if (StrA.Length = 0) or (StrB.Length = 0) then begin
    if StrA.Length > 0 then
      result := 1
    else
      result := if StrB.Length > 0 then -1 else 0;
  end
  else begin
    var lOptions: Cardinal := 0;

    if TCompareOption.coLingIgnoreCase in Options then
      lOptions := lOptions or rtl.LINGUISTIC_IGNORECASE;
    if TCompareOption.coLingIgnoreDiacritic in Options then
      lOptions := lOptions or rtl.LINGUISTIC_IGNOREDIACRITIC;
    if TCompareOption.coIgnoreCase in Options then
      lOptions := lOptions or rtl.NORM_IGNORECASE;
    if TCompareOption.coIgnoreKanatype in Options then
      lOptions := lOptions or rtl.NORM_IGNOREKANATYPE;
    if TCompareOption.coIgnoreNonSpace in Options then
      lOptions := lOptions or rtl.NORM_IGNORENONSPACE;
    if TCompareOption.coIgnoreSymbols in Options then
      lOptions := lOptions or rtl.NORM_IGNORESYMBOLS;
    if TCompareOption.coIgnoreWidth in Options then
      lOptions := lOptions or rtl.NORM_IGNOREWIDTH;
    if TCompareOption.coLingCasing in Options then
      lOptions := lOptions or rtl.NORM_LINGUISTIC_CASING;
    if TCompareOption.coDigitAsNumbers in Options then
      lOptions := lOptions or rtl.SORT_DIGITSASNUMBERS;
    if TCompareOption.coStringSort in Options then
      lOptions := lOptions or rtl.SORT_STRINGSORT;

    var lStrA := PlatformString(StrA.fData).FirstChar;
    var lStrB := PlatformString(StrB.fData).FirstChar;
    result := rtl.CompareStringW(rtl.LOCALE_USER_DEFAULT, lOptions, lStrA + IndexA, LengthA, lStrB + IndexB, LengthB) - rtl.CSTR_EQUAL;
    // TODO locales
  end
  {$ELSEIF LINUX}
  var lStrA := PlatformString(StrA.fData.Substring(IndexA, LengthA)).ToAnsiChars(true);
  var lStrB := PlatformString(StrB.fData.Substring(IndexB, LengthB)).ToAnsiChars(true);
  var lStringA := ^rtl.wchar_t(@lStrA[0]);
  var lStringB := ^rtl.wchar_t(@lStrB[0]);

  if (TCompareOption.coLingIgnoreCase in Options) then
    result := rtl.wcscasecmp(lStringA, lStringB)
  else
    result := rtl.wcscoll(lStringA, lStringB);
  {$ENDIF}
  {$ELSEIF TOFFEE}
  var lOptions: Foundation.NSStringCompareOptions := 0;
  var lTotalChars: Integer;

  if (TCompareOption.coLingIgnoreCase in Options) or (TCompareOption.coIgnoreCase in Options) then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSCaseInsensitiveSearch;
  if TCompareOption.coLingIgnoreDiacritic in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSDiacriticInsensitiveSearch;
  if TCompareOption.coIgnoreWidth in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSWidthInsensitiveSearch;
  if TCompareOption.coDigitAsNumbers in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSNumericSearch;
  if TCompareOption.coStringSort in Options then
    lOptions := lOptions or Foundation.NSStringCompareOptions.NSForcedOrderingSearch;

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

  if lStrA.length <= lStrB.length then
    lTotalChars := lStrA.length
  else
    lTotalChars := lStrB.length;

  result := lStrA.compare(lStrB) options(lOptions) range(Foundation.NSMakeRange(0, lTotalChars)) locale(LocaleID);
  {$ENDIF}
end;

class method DelphiString.CompareOrdinal(const StrA: DelphiString; const StrB: DelphiString): Integer;
begin
  result := StrA.fData.CompareTo(StrB.fData);
end;

class method DelphiString.CompareOrdinal(const StrA: DelphiString; IndexA: Integer; const StrB: DelphiString; IndexB: Integer; aLength: Integer): Integer;
begin
  {$IF COOPER}
  var lStrA := StrA.fData.substring(IndexA, aLength);
  result := lStrA.compareTo(StrB.SubString(IndexB, aLength));
  {$ELSEIF ECHOES}
  result := System.String.CompareOrdinal(StrA, IndexA, StrB, IndexB, aLength);
  {$ELSEIF ISLAND}
  var lStrA := PlatformString(StrA.fData).Substring(IndexA, aLength);
  var lStrB := PlatformString(StrB.fData).Substring(IndexB, aLength);
  result := String.Compare(lStrA, lStrB);
  {$ELSEIF TOFFEE}
  var lStrA := PlatformString(StrA.fData).substringWithRange(Foundation.NSMakeRange(IndexA, aLength));
  var lStrB := PlatformString(StrB.fData).substringWithRange(Foundation.NSMakeRange(IndexB, aLength));
  result := lStrA.compare(lStrB) options(NSStringCompareOptions.NSLiteralSearch);
  {$ENDIF}
end;

class method DelphiString.CompareText(const StrA: DelphiString; const StrB: DelphiString): Integer;
begin
  result := StrA.fData.CompareToIgnoreCase(StrB.fData);
end;

method DelphiString.ToLower(LocaleID: TLocaleID): DelphiString;
begin
  result := fData.ToLower(LocaleID);
end;

method DelphiString.ToUpper(LocaleID: TLocaleID): DelphiString;
begin
  result := fData.ToUpper(LocaleID);
end;


{$IF COOPER}
method DelphiString.hashCode: Integer;
begin
  result := fData.hashCode;
end;
{$ELSEIF ECHOES OR ISLAND}
method DelphiString.GetHashCode: Integer;
begin
  result := fData.GetHashCode;
end;
{$ELSEIF TOFFEE}
method DelphiString.hash: Foundation.NSUInteger;
begin
  result := fData.hash;
end;
{$ENDIF}

{$IF TOFFEE}
method DelphiString.isEqual(Obj: id): Boolean;
begin
  if Obj = nil then
    exit false;

  if not (Obj is DelphiString) then
    exit false;

  var lItem := DelphiString(Obj);
  result := &Equals(lItem);
end;
{$ELSE}
method DelphiString.&Equals(Obj: Object): Boolean;
begin
  if Obj = nil then
    exit false;

  if not (Obj is DelphiString) then
    exit false;

  var lItem := DelphiString(Obj);
  result := &Equals(lItem);
end;
{$ENDIF}

end.