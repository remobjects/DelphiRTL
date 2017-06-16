namespace RemObjects.Elements.RTL.Delphi;

interface

uses
{$IF ISLAND}
  RemObjects.Elements.System,
{$ELSE}
  RemObjects.Elements.RTL,
{$ENDIF}
  RemObjects;

{$GLOBALS ON}

type
  TContentType = public (UTF8, UTF16, Unknown);

  AnsiString = public record
  private
    fHashCode: Integer := 0;
    fData: array of Byte;
    class var fOffset: Integer := 1;
    class method SetOffset(aOffset: Integer);
    method GetOffsetChar(aIndex: Integer): AnsiChar;
    method SetOffsetChar(aIndex: Integer; aValue: AnsiChar);
    method GetChar(aIndex: Integer): AnsiChar; inline;
    method SetChar(aIndex: Integer; Value: AnsiChar); inline;
    method CharArrayToByteArray(aCharArray: array of Char; StartIndex: Integer; aLength: Integer): array of Byte;
    method StringToUTF8(aString: PlatformString): array of Byte;
    class method CopyArray(aSource: array of Byte; aSourceIndex: Integer; var aTarget: array of Byte; aTargetIndex: Integer; aLength: Integer); static;
  public
    constructor;
    constructor(aLength: Integer);
    constructor(Value: PlatformString; AsUTF16Bytes: Boolean := false);
    constructor(C: AnsiChar; Count: Integer);
    constructor(Value: array of Char);
    constructor(Value: array of Char; StartIndex: Integer; aLength: Integer);
    constructor(Value: array of Byte);
    constructor(Value: array of Byte; StartIndex: Integer; aLength: Integer);
    class method Create(C: AnsiChar; Count: Integer): AnsiString; static;
    class method Create(const Value: array of Char; StartIndex: Integer; aLength: Integer): AnsiString; static;
    class method Create(const Value: array of Char): AnsiString; static;
    class operator Implicit(Value: Char): AnsiString;
    class operator Implicit(Value: PlatformString): AnsiString;
    class operator Implicit(Value: AnsiString): String;
    class operator Implicit(Value: array of Char): AnsiString;
    class operator &Add(Value1: AnsiString; Value2: Char): AnsiString;
    class operator &Add(Value1: Char; Value2: AnsiString): AnsiString;
    class operator &Add(Value1: AnsiString; Value2: AnsiChar): AnsiString;
    class operator &Add(Value1: AnsiString; Value2: AnsiString): not nullable AnsiString;
    class operator Equal(Value1, Value2: AnsiString): Boolean;
    class operator NotEqual(Value1, Value2: AnsiString): Boolean;
    class operator Greater(Value1, Value2: AnsiString): Boolean;
    class operator Less(Value1, Value2: AnsiString): Boolean;
    class operator GreaterOrEqual(Value1, Value2: AnsiString): Boolean;
    class operator LessOrEqual(Value1, Value2: AnsiString): Boolean;
    class property Offset: Integer read fOffset write SetOffset;

    class method CompareStr(S1, S2: AnsiString): Integer; static; // CompareStr --> SameStr, case sensitive
    class method SameStr(S1, S2: AnsiString): Boolean; static;
    class method CompareText(S1, S2: AnsiString): Integer; static;
    class method SameText(S1, S2: AnsiString): Boolean; static; // --> CompareText, no case sensitive
    class method &Copy(aSource: AnsiString; aSourceIndex: Integer; aCount: Integer): AnsiString;

    {$IF COOPER}
    method hashCode: Integer; override;
    {$ELSEIF ECHOES OR ISLAND}
    method GetHashCode: Integer; override;
    {$ELSEIF TOFFEE}
    method hash: Foundation.NSUInteger;
    {$ENDIF}
    
    {$IF TOFFEE}
    method isEqual(Obj: id): Boolean;
    {$ELSEIF COOPER OR ECHOES}
    method &Equals(Obj: Object): Boolean; override;
    {$ELSE}
    method &Equals(Obj: Object): Boolean;
    {$ENDIF} 

    [ToString]
    method ToString: PlatformString;

    method GetData: array of Byte;
    method Insert(aIndex: Integer; aValue: AnsiString): AnsiString;
    method Insert(aIndex: Integer; Value: Char): AnsiString;
    method Insert(aIndex: Integer; Value: AnsiChar): AnsiString;
    method &Remove(StartIndex: Integer): AnsiString;
    method &Remove(StartIndex: Integer; Count: Integer): AnsiString;
    method SetLength(aLength: Integer);
    method CopyTo(SourceIndex: Integer; var Destination: array of Byte; DestinationIndex: Integer; Count: Integer);
    method CopyFrom(aSource: AnsiString; aSourceIndex: Integer; aCount: Integer);
    method ToUpper: AnsiString;
    method ToLower: AnsiString;
    method Trim: AnsiString;
    method TrimLeft: AnsiString;
    method TrimRight: AnsiString;
    method IndexOf(aSubStr: AnsiString): Integer; inline;
    method IndexOf(aSubStr: AnsiString; aIndex: Integer): Integer;
    method Contains(aSubText: AnsiString): Boolean; inline;
    method StartsWith(aSubText: AnsiString): Boolean;
    method EndsWith(aSubText: AnsiString): Boolean;
    method Replace(aFromText, aToText: AnsiString): AnsiString; inline;
    method Replace(OldPattern, NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString;
    method FillChar(aCount: Integer; aValue: AnsiChar);
    method ToNullTerminated: AnsiString;

    property Length: Integer read fData.length write SetLength;
    property Chars[aIndex: Integer]: AnsiChar read GetChar write SetChar;
    property Character[aIndex: Integer]: AnsiChar read GetOffsetChar write SetOffsetChar; default;
    property Data: array of Byte read GetData;
  end;

  // Original functions
  procedure SetLength(var aString: AnsiString; aLength: Integer);
  function UpperCase(S: AnsiString): AnsiString; inline;
  function LowerCase(S: AnsiString): AnsiString; inline;
  function CompareStr(S1, S2: AnsiString): Integer; inline;
  function SameStr(S1, S2: AnsiString): Boolean; inline;
  function CompareText(S1, S2: AnsiString): Integer; inline;
  function SameText(S1, S2: AnsiString): Boolean; inline;
  function Trim(S: AnsiString): AnsiString; inline;
  function TrimLeft(S: AnsiString): AnsiString; inline;
  function TrimRight(S: AnsiString): AnsiString; inline;
  function ContainsText(aText, aSubText: AnsiString): Boolean; inline;
  function StartsText(aSubText, aText: AnsiString): Boolean; inline;
  function EndsText(aSubText, aText: AnsiString): Boolean; inline;
  function ReplaceText(aText, aFromText, aToText: AnsiString): AnsiString; inline;
  function StringReplace(S, OldPattern, NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString; inline;
  procedure FillChar(var Dest: AnsiString; aCount: Integer; aValue: AnsiChar);
  // Pos, Copy, Delete and Insert are always 1-based for compatibility
  function Pos(SubStr, S: AnsiString): Integer;
  function &Copy(aSource: AnsiString; aIndex: Integer; aCount: Integer): AnsiString;
  procedure DeleteA(var aString: AnsiString; aIndex: Integer; aCount: Integer);
  procedure Insert(aSource: AnsiString; var aDest: AnsiString; aIndex: Integer);

implementation

constructor AnsiString;
begin
  fData := new Byte[0];
end;

constructor AnsiString(aLength: Integer);
begin
  fData := new Byte[aLength];
end;

constructor AnsiString(Value: PlatformString; AsUTF16Bytes: Boolean := false);
begin
  if AsUTF16Bytes then begin
    {$IF TOFFEE}
    var lTemp := new Char[Value.length];
    Value.getCharacters(lTemp) range(NSMakeRange(0, Value.length));
    {$ELSEIF COOPER OR ECHOES OR ISLAND}
    var lTemp := Value.ToCharArray;
    {$ENDIF}
    fData := CharArrayToByteArray(lTemp, 0, Value.Length);
  end
  else
    fData := StringToUTF8(Value);
end;

method AnsiString.SetLength(aLength: Integer);
begin
  if Length > 0 then begin
    var lTmp := new Byte[aLength];
    for i: Integer := 0 to Length - 1 do
      lTmp[i] := fData[i];
    fData := lTmp;
  end
  else
     fData := new Byte[aLength];
end;

class method AnsiString.Create(C: AnsiChar; Count: Integer): AnsiString;
begin
  result := new AnsiString(C, Count);
end;

class method AnsiString.Create(Value: array of Char; StartIndex: Integer; aLength: Integer): AnsiString;
begin
  result := new AnsiString(Value, StartIndex, aLength);
end;

class method AnsiString.Create(Value: array of Char): AnsiString;
begin
  result := Create(Value, 0, Value.Length);
end;

operator AnsiString.Implicit(Value: Char): AnsiString;
begin
  result := new AnsiString(AnsiChar(Value), 1);
end;

operator AnsiString.Implicit(Value: PlatformString): AnsiString;
begin
  result := new AnsiString(Value);
end;

operator AnsiString.Implicit(Value: AnsiString): String;
begin
  result := Value.ToString;
end;

operator AnsiString.Add(Value1: AnsiString; Value2: Char): AnsiString;
begin
  result := new AnsiString(Value1);
  result.Insert(Value1.Length, Value2);
end;

operator AnsiString.&Add(Value1: AnsiString; Value2: AnsiChar): AnsiString;
begin
  result := new AnsiString(Value1.Length + 1);
  result.CopyFrom(Value1, 0, Value1.Length);
  result.Chars[result.Length - 1] := Value2;
end;

operator AnsiString.Add(Value1: Char; Value2: AnsiString): AnsiString;
begin
  result := new AnsiString(Value2);
  result.Insert(0, Value1);
end;

operator AnsiString.Add(Value1: AnsiString; Value2: AnsiString): not nullable AnsiString;
begin
  result := new AnsiString(Value1);
  result.Insert(Value1.Length, Value2);
end;

operator AnsiString.Equal(Value1: AnsiString; Value2: AnsiString): Boolean;
begin
  if (Value1.Length <> Value2.Length) then
    exit false;

  if (Value1.Length = 0) and (Value2.Length = 0) then
    exit true;

  var i := 0;
  while (Value1.Chars[i] = Value2.Chars[i]) and (i < Value1.Length) do
    inc(i);

  result := if i = Value1.Length then true else false;
end;

operator AnsiString.NotEqual(Value1: AnsiString; Value2: AnsiString): Boolean;
begin
  result := not (Value1 = Value2);
end;

operator AnsiString.Greater(Value1: AnsiString; Value2: AnsiString): Boolean;
begin
  result := CompareStr(Value1, Value2) > 0;
end;

operator AnsiString.Less(Value1: AnsiString; Value2: AnsiString): Boolean;
begin
  result := CompareStr(Value1, Value2) < 0;
end;

operator AnsiString.GreaterOrEqual(Value1: AnsiString; Value2: AnsiString): Boolean;
begin
  result := CompareStr(Value1, Value2) >= 0;
end;

operator AnsiString.LessOrEqual(Value1: AnsiString; Value2: AnsiString): Boolean;
begin
  result := CompareStr(Value1, Value2) <= 0;
end;

class method AnsiString.SetOffset(aOffset: Integer);
begin
  if aOffset not in [0,1] then raise new Exception("Delphi AnsiString offset must be 0 or 1");
  fOffset := aOffset;
end;

method AnsiString.GetOffsetChar(aIndex: Integer): AnsiChar;
begin
  if (aIndex = 0) and (fOffset > 0) then raise new Exception("Index ot of range: Delphi AnsiStrings are one-based.");
  result := GetChar(aIndex - fOffset);
end;

method AnsiString.SetOffsetChar(aIndex: Integer; aValue: AnsiChar);
begin
  if (aIndex = 0) and (fOffset > 0) then raise new Exception("Index ot of range: Delphi AnsiStrings are one-based.");
  SetChar(aIndex - fOffset, aValue);
end;

method AnsiString.CharArrayToByteArray(aCharArray: array of Char; StartIndex: Integer; aLength: Integer): array of Byte;
begin
  result := new Byte[aLength * 2];
  for i: Integer := StartIndex to (StartIndex + aLength) - 1 do
  begin
    var lChar := Word(aCharArray[i]);
    result[2 * i] := Byte(lChar);
    result[(2 * i) + 1] := Byte(Word(lChar) shr 8);
  end;
end;

operator AnsiString.Implicit(Value: array of Char): AnsiString;
begin
  result := new AnsiString(Value);
end;

constructor AnsiString(Value: array of Char);
begin
  fData := CharArrayToByteArray(Value, 0, Value.Length);
end;

constructor AnsiString(Value: array of Char; StartIndex: Integer; aLength: Integer);
begin
  fData := CharArrayToByteArray(Value, StartIndex, aLength);
end;

constructor AnsiString(C: AnsiChar; Count: Integer);
begin
  fData := new Byte[Count];
  for i: Integer := 0 to Count - 1 do
    fData[i] := Byte(C);
end;

constructor AnsiString(Value: array of Byte);
begin
  constructor(Value, 0, Value.length);
end;

constructor AnsiString(Value: array of Byte; StartIndex: Integer; aLength: Integer);
begin
  fData := new Byte[Value.length];
  for i: Integer := StartIndex to (StartIndex + aLength) - 1 do
  fData[i - StartIndex] := Value[i];
end;

method AnsiString.ToString: PlatformString;
begin
  if fData = nil then
    exit '';
  {$IF COOPER}
  result := new PlatformString(fData);
  {$ELSEIF ECHOES OR ISLAND}
  var lArray := new Char[fData.length];
  for i: Integer := 0 to fData.length - 1 do
    lArray[i] := Chr(fData[i]);
  {$IF ECHOES}
  result := new PlatformString(lArray);
  {$ELSE}
  result := PlatformString.FromCharArray(lArray);
  {$ENDIF}
  {$ELSEIF TOFFEE}
  result := NSString.alloc.initWithBytes(@fData[0]) length(fData.length) encoding(NSStringEncoding.NSUTF8StringEncoding);
  {$ENDIF}
end;

method AnsiString.GetData: array of Byte;
begin
  result := fData;
end;

method AnsiString.GetChar(aIndex: Integer): AnsiChar;
begin
  result := AnsiChar(fData[aIndex]);
end;

method AnsiString.SetChar(aIndex: Integer; Value: AnsiChar);
begin
  fData[aIndex] := Byte(Value);
end;

method AnsiString.StringToUTF8(aString: PlatformString): array of Byte;
begin
  {$IF COOPER}
  exit aString.getBytes("UTF-8") as not nullable;
  {$ELSEIF ECHOES}
  exit System.Text.Encoding.UTF8.GetBytes(aString) as not nullable;
  {$ELSEIF ISLAND}
  exit TextConvert.StringToUTF8(aString) as not nullable;
  {$ELSEIF TOFFEE}
  var lData := Binary(aString.dataUsingEncoding(NSStringEncoding.NSUTF8StringEncoding));
  exit lData.ToArray as not nullable;
  {$ENDIF}
end;

method AnsiString.Insert(aIndex: Integer; aValue: AnsiString): AnsiString;
begin
  var lOldData := self.fData;
  fData := new Byte[lOldData.length + aValue.length];
  CopyArray(lOldData, 0, var fData, 0, aIndex);
  CopyArray(aValue.Data, 0, var fData, aIndex, aValue.Length);
  CopyArray(lOldData, aIndex, var fData, aIndex + aValue.Length, lOldData.length - aIndex);
  result := self;
end;

method AnsiString.Insert(aIndex: Integer; Value: Char): AnsiString;
begin
  result := Insert(aIndex, AnsiChar(Value));
end;

method AnsiString.Insert(aIndex: Integer; Value: AnsiChar): AnsiString;
begin
  var lOldData := fData;
  fData := new Byte[lOldData.length + 1];
  CopyArray(lOldData, 0, var fData, 0, aIndex);
  fData[aIndex] := Byte(Value);
  CopyArray(lOldData, aIndex, var fData, aIndex + 1, lOldData.length - aIndex);
  result := self;
end;

class method AnsiString.CopyArray(aSource: array of Byte; aSourceIndex: Integer; var aTarget: array of Byte; aTargetIndex: Integer; aLength: Integer);
begin
  for i: Integer := 0 to aLength - 1 do
    aTarget[aTargetIndex + i] := aSource[aSourceIndex + i];
end;

method AnsiString.&Remove(StartIndex: Integer): AnsiString;
begin
  result := &Remove(StartIndex, Length - StartIndex);
end;

method AnsiString.&Remove(StartIndex: Integer; Count: Integer): AnsiString;
begin
  var lOldData := fData;
  fData := new Byte[Length - Count];
  CopyArray(lOldData, 0, var fData, 0, StartIndex);
  CopyArray(lOldData, StartIndex + Count, var fData, StartIndex, lOldData.length - (StartIndex + Count));
  result := self;
end;

class method AnsiString.CompareStr(S1: AnsiString; S2: AnsiString): Integer;
begin
  result := S1.Length - S2.Length;
  var lMax := if result >= 0 then S2.Length else S1.Length;
  var i := 0;
  while i < lMax do begin
    var lCh1: SmallInt := Byte(S1.Chars[i]);
    var lCh2: SmallInt := Byte(S2.Chars[i]);
    var lCh := lCh1 - lCh2;
    if lCh <> 0 then
      exit if lCh > 0 then 1 else -1;
    inc(i);
  end;
end;

class method AnsiString.SameStr(S1: AnsiString; S2: AnsiString): Boolean;
begin
  result := CompareStr(S1, S2) = 0;
end;

class method AnsiString.CompareText(S1: AnsiString; S2: AnsiString): Integer;
begin
  result := S1.Length - S2.Length;
  var lMax := if result >= 0 then S2.Length else S1.Length;
  var i := 0;
  var lCh1, lCh2: AnsiChar;
  while i < lMax do begin
    lCh1 := if S1.Chars[i] in ['a'..'z'] then AnsiChar(Byte(S1.Chars[i]) xor $20) else S1.Chars[i];
    lCh2 := if S2.Chars[i] in ['a'..'z'] then AnsiChar(Byte(S2.Chars[i]) xor $20) else S2.Chars[i];

    var lChar1: SmallInt := Byte(lCh1);
    var lChar2: SmallInt := Byte(lCh2);
    var lCh := lChar1 - lChar2;
    if lCh <> 0 then
      exit if lCh > 0 then 1 else -1;
    inc(i);
  end;
end;

class method AnsiString.SameText(S1: AnsiString; S2: AnsiString): Boolean;
begin
  result := CompareText(S1, S2) = 0;
end;

class method AnsiString.&Copy(aSource: AnsiString; aSourceIndex: Integer; aCount: Integer): AnsiString;
begin
  result := new AnsiString(aCount);
  for i: Integer := 0 to aCount - 1 do
    result.Chars[i] := aSource.Chars[aSourceIndex + i];
end;

method AnsiString.CopyFrom(aSource: AnsiString; aSourceIndex: Integer; aCount: Integer);
begin
  for i: Integer := 0 to aCount - 1 do
    fData[i] := Byte(aSource.Chars[aSourceIndex + i]);
end;

method AnsiString.CopyTo(SourceIndex: Integer; var Destination: array of Byte; DestinationIndex: Integer; Count: Integer);
begin
  for i: Integer := 0 to Count - 1 do
    Destination[DestinationIndex + i] := fData[SourceIndex + i];
end;

method AnsiString.ToUpper: AnsiString;
begin
  result := new AnsiString(Length);
  var lCh: AnsiChar;
  for i: Integer := 0 to Length - 1 do begin
    lCh := AnsiChar(fData[i]);
    if lCh in ['a'..'z'] then
      result.Chars[i] := AnsiChar(Byte(lCh) xor $20)
    else
      result.Chars[i] := lCh;
  end;
end;

method AnsiString.ToLower: AnsiString;
begin
  result := new AnsiString(Length);
  var lCh: AnsiChar;
  for i: Integer := 0 to Length - 1 do begin
    lCh := AnsiChar(fData[i]);
    if lCh in ['A'..'Z'] then
      result.Chars[i] := AnsiChar(Byte(lCh) xor $20)
    else
      result.Chars[i] := lCh;
  end;
end;

method AnsiString.Trim: AnsiString;
begin
  result := self.TrimLeft.TrimRight;
end;

method AnsiString.TrimLeft: AnsiString;
begin
  var i: Integer := 0;

  while (fData[i] <= ord(' ')) and (i < Length) do
    inc(i);
  result := AnsiString.Copy(self, i, Length - i)
end;

method AnsiString.TrimRight: AnsiString;
begin
  var i: Integer := Length - 1;

  while (fData[i] <= ord(' ')) and (i >= 0) do
    dec(i);
  result := AnsiString.Copy(self, 0, i + 1);
end;

method AnsiString.IndexOf(aSubStr: AnsiString): Integer;
begin
  result := IndexOf(aSubStr, 0);
end;

method AnsiString.IndexOf(aSubStr: AnsiString; aIndex: Integer): Integer;
begin
  if (aSubStr = '') or (Length = 0) or (aSubStr.Length > Length) then
    exit -1;

  var i := aIndex;
  var lSubData := aSubStr.Data;
  while i <= Length - aSubStr.Length do begin
    if fData[i] = lSubData[0] then begin
      var j := i + 1;
      var k := 1;
      while (k < aSubStr.Length) and (fData[j] = lSubData[k]) do begin
        inc(j);
        inc(k);
      end;
      if k = aSubStr.Length then
        exit i;
    end;
    inc(i);
  end;
  result := -1;
end;

method AnsiString.Contains(aSubText: AnsiString): Boolean;
begin
  result := IndexOf(aSubText) >= 0;
end;

method AnsiString.StartsWith(aSubText: AnsiString): Boolean;
begin
  if (aSubText = '') or (Length = 0) or (aSubText.Length > Length) then
    exit false;

  var i: Integer := 0;
  var lSubData := aSubText.Data;
  while (i < aSubText.Length) and (fData[i] = lSubData[i]) do
    inc(i);

  result := if i = aSubText.Length then true else false;
end;

method AnsiString.EndsWith(aSubText: AnsiString): Boolean;
begin
  if (aSubText = '') or (Length = 0) or (aSubText.Length > Length) then
    exit false;

  var i := Length - aSubText.Length;
  var lDelta := i;
  var lSubData := aSubText.GetData;
  while (i < Length) and (fData[i] = lSubData[i - lDelta]) do
    inc(i);

  result := if i = Length then true else false;
end;

method AnsiString.Replace(aFromText: AnsiString; aToText: AnsiString): AnsiString;
begin
  result := Replace(aFromText, aToText, []);
end;

method AnsiString.Replace(OldPattern: AnsiString; NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString;
begin
  if (OldPattern = '') or (Length = 0) or (OldPattern.Length > Length) then
    exit AnsiString.Copy(self, 0, Length);

  var lData, lSubData: array of Byte;
  result := new AnsiString();

  if TReplaceFlags.rfIgnoreCase in aFlags then begin
    lData := ToLower.Data;
    lSubData := OldPattern.ToLower.Data;
  end
  else begin
    lData := fData;
    lSubData := OldPattern.Data;
  end;

  var lOnlyFirst := not (TReplaceFlags.rfReplaceAll in aFlags);
  var i := 0;
  while i < Length do begin
    if lData[i] = lSubData[0] then begin
      var j := i + 1;
      var k := 1;
      while (j < Length) and (k < OldPattern.Length) and (lData[j] = lSubData[k]) do begin
        inc(j);
        inc(k);
      end;
      if k = OldPattern.Length then begin
        result.Insert(result.Length, NewPattern);        
        inc(i, lSubData.Length);
        if lOnlyFirst then begin
            result.Insert(result.Length, &Copy(self, i, fData.length - i));
          exit;
        end;
      end
      else begin
        result.Insert(result.Length, AnsiChar(fData[i]));
        inc(i);
      end;
    end
    else begin
      result.Insert(result.Length, AnsiChar(fData[i]));
      inc(i);
    end;
  end;
end;

method AnsiString.FillChar(aCount: Integer; aValue: AnsiChar);
begin
  if Length < aCount then
    SetLength(aCount);

  for i: Integer := 0 to aCount - 1 do
    fData[i] := Byte(aValue);
end;

method AnsiString.ToNullTerminated: AnsiString;
begin
  result := new AnsiString(length + 1);
  result.CopyFrom(self, 0, Length);
  result.Chars[Length] := AnsiChar(#0);
end;

{$IF COOPER}
method AnsiString.hashCode: Integer; 
begin
  if fHashCode = 0 then begin
    var lMultiplier: Integer := 1;
    var lShift: Integer;
    for i: Integer := Length - 1 downto 0 do begin
      fHashCode := fHashCode + fData[i] * lMultiplier;
      lShift := lMultiplier shl 5;
      lMultiplier := lShift - lMultiplier;
    end;
  end;
  result := fHashCode;
end;
{$ELSEIF ECHOES OR ISLAND}
method AnsiString.GetHashCode: Integer;
begin
  if fHashCode = 0 then begin
    var lMultiplier: Integer := 1;
    var lShift: Integer;
    for i: Integer := Length - 1 downto 0 do begin
      fHashCode := fHashCode + fData[i] * lMultiplier;
      lShift := lMultiplier shl 5;
      lMultiplier := lShift - lMultiplier;
    end;
  end;
  result := fHashCode;
end;
{$ELSEIF TOFFEE}
method AnsiString.hash: Foundation.NSUInteger;
begin
  if fHashCode = 0 then begin
    var lMultiplier: Integer := 1;
    var lShift: Integer;
    for i: Integer := Length - 1 downto 0 do begin
      fHashCode := fHashCode + fData[i] * lMultiplier;
      lShift := lMultiplier shl 5;
      lMultiplier := lShift - lMultiplier;
    end;
  end;
  result := fHashCode;
end;
{$ENDIF}

{$IF TOFFEE}
method AnsiString.isEqual(Obj: id): Boolean;
begin
  if Obj = nil then
    exit false;

  if not (Obj is AnsiString) then
    exit false;

   var lItem := AnsiString(Obj);
   result := &Equals(lItem);
 end;
{$ELSEIF COOPER OR ECHOES}
method AnsiString.&Equals(Obj: Object): Boolean; 
 begin
  if Obj = nil then
    exit false;

  if not (Obj is AnsiString) then
    exit false;

   var lItem := AnsiString(Obj);
   result := &Equals(lItem);
 end;
{$ELSE}
method &AnsiString.Equals(Obj: Object): Boolean;
 begin
  if Obj = nil then
    exit false;

  if not (Obj is AnsiString) then
    exit false;

   var lItem := AnsiString(Obj);
   result := &Equals(lItem);
 end;
{$ENDIF} 


procedure SetLength(var aString: AnsiString; aLength: Integer);
begin
  aString.SetLength(aLength);
end;

function UpperCase(S: AnsiString): AnsiString;
begin
  result := S.ToUpper;
end;

function LowerCase(S: AnsiString): AnsiString;
begin
  result := S.ToLower;
end;

function CompareStr(S1, S2: AnsiString): Integer;
begin
  result := AnsiString.CompareStr(S1, S2);
end;

function SameStr(S1, S2: AnsiString): Boolean;
begin
  result := AnsiString.SameStr(S1, S2);
end;

function CompareText(S1, S2: AnsiString): Integer;
begin
  result := AnsiString.CompareText(S1, S2);
end;

function SameText(S1, S2: AnsiString): Boolean;
begin
  result := AnsiString.SameText(S1, S2);
end;

function Trim(S: AnsiString): AnsiString;
begin
  result := S.Trim;
end;

function TrimLeft(S: AnsiString): AnsiString;
begin
  result := S.TrimLeft;
end;

function TrimRight(S: AnsiString): AnsiString;
begin
  result := S.TrimRight;
end;

function Pos(SubStr, S: AnsiString): Integer;
begin
  result := S.IndexOf(SubStr) + 1;
end;

function &Copy(aSource: AnsiString; aIndex: Integer; aCount: Integer): AnsiString;
begin
  result := AnsiString.Copy(aSource, aIndex - 1, aCount);
end;

procedure DeleteA(var aString: AnsiString; aIndex: Integer; aCount: Integer);
begin
  aString.Remove(aIndex - 1, aCount);
end;

procedure Insert(aSource: AnsiString; var aDest: AnsiString; aIndex: Integer);
begin
  aDest := aDest.Insert(aIndex - 1, aSource);
end;

function ContainsText(aText, aSubText: AnsiString): Boolean;
begin
  result := aText.Contains(aSubText);
end;

function StartsText(aSubText, aText: AnsiString): Boolean;
begin
  result := aText.StartsWith(aSubText);
end;

function EndsText(aSubText, aText: AnsiString): Boolean;
begin
  result := aText.EndsWith(aSubText);
end;

function ReplaceText(aText, aFromText, aToText: AnsiString): AnsiString;
begin
  result := StringReplace(aText, aFromText, aToText, [TReplaceFlags.rfIgnoreCase, TReplaceFlags.rfReplaceAll]);
end;

function StringReplace(S, OldPattern, NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString;
begin
  result := S.Replace(OldPattern, NewPattern, aFlags);
end;

procedure FillChar(var Dest: AnsiString; aCount: Integer; aValue: AnsiChar);
begin
  Dest.FillChar(aCount, aValue);
end;

end.