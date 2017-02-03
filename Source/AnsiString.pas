namespace RemObjects.Elements.RTL.Delphi;

interface

uses
{$IF ISLAND}
  RemObjects.Elements.System;
{$ELSE}
  RemObjects.Elements.RTL;
{$ENDIF}

{$GLOBALS ON}

type
  AnsiChar = public Byte;
  TContentType = public (UTF8, UTF16, Unknown);

  AnsiString = public record
  private
    class var fOffset: Integer;
    fData: array of Byte;
    class method SetOffset(Value: Integer);
    method GetOffsetChar(aIndex: Integer): AnsiChar;
    method SetOffsetChar(aIndex: Integer; Value: AnsiChar);
    method GetChar(aIndex: Integer): AnsiChar;
    method SetChar(aIndex: integer; Value: AnsiChar);
    method GetData: array of Byte;
    method CharArrayToByteArray(aCharArray: array of Char; StartIndex: Integer; aLength: Integer): array of Byte;
    method StringToUTF8(aString: PlatformString): array of Byte;
    class method CopyArray(aSource: array of Byte; aSourceIndex: Integer; var aTarget: array of Byte; aTargetIndex: Integer; aLength: Integer); static;
  public
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
    class operator Implicit(Value: array of char): AnsiString;
    class operator &Add(Value1: AnsiString; Value2: Char): AnsiString;
    class operator &Add(Value1: Char; Value2: AnsiString): AnsiString;
    class operator &Add(Value1: AnsiString; Value2: AnsiString): not nullable AnsiString;
    class operator Equal(Value1, Value2: AnsiString): Boolean;
    class operator NotEqual(Value1, Value2: AnsiString): Boolean;
    class operator Greater(Value1, Value2: AnsiString): Boolean;
    class operator Less(Value1, Value2: AnsiString): Boolean;
    class operator GreaterOrEqual(Value1, Value2: AnsiString): Boolean;
    class operator LessOrEqual(Value1, Value2: AnsiString): Boolean;
    class property Offset: Integer read fOffset write SetOffset;

    class method CompareStr(S1, S2: AnsiString): Integer; static;
    class method SameStr(S1, S2: AnsiString): Boolean; static;
    class method CompareText(S1, S2: AnsiString): Integer; static;
    class method SameText(S1, S2: AnsiString): Boolean; static;

    [ToString]
    method ToString: PlatformString;
    begin
      //result := fData;
    end;
    
    method Insert(aIndex: Integer; Value: AnsiString): AnsiString;
    method Insert(aIndex: Integer; Value: Char): AnsiString;
    method Insert(aIndex: Integer; Value: AnsiChar): AnsiString;
    method &Remove(StartIndex: Integer): AnsiString;
    method &Remove(StartIndex: Integer; Count: Integer): AnsiString;     
    method SetLength(aLength: Integer);
    method CopyTo(SourceIndex: Integer; var Destination: array of Byte; DestinationIndex: Integer; Count: Integer);
    method ToUpper: AnsiString;
    method ToLower: AnsiString;
    method Trim: AnsiString;
    method TrimLeft: AnsiString;
    method TrimRight: AnsiString;
    method IndexOf(Substr: AnsiString): Integer;
    method Contains(aSubText: AnsiString): Boolean; inline;
    method StartsWith(aSubText: AnsiString): Boolean; inline;
    method EndsWith(aSubText: AnsiString): Boolean; inline;
    method Replace(aFromText, aToText: AnsiString): AnsiString; inline;
    method Replace(OldPattern, NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString;
    
    property Length: Integer read fData.Length write SetLength;
    property Chars[aIndex: Integer]: AnsiChar read GetChar write SetChar;
    property Character[aIndex: Integer]: AnsiChar read GetOffsetChar write SetOffsetChar; default;
    property Data: array of Byte read GetData;
  end;

  // Original functions
  procedure SetLength(var aString: AnsiString; aLength: Integer);
  // TODO
 /*
  function UpperCase(S: AnsiString): AnsiString;
  function LowerCase(S: AnsiString): AnsiString;
  function CompareStr(S1, S2: AnsiString): Integer;
  function SameStr(S1, S2: AnsiString): Boolean;
  function CompareText(S1, S2: AnsiString): Integer;
  function SameText(S1, S2: AnsiString): Boolean;
  function Trim(S: AnsiString): AnsiString;
  function TrimLeft(S: AnsiString): AnsiString;
  function TrimRight(S: AnsiString): AnsiString;
  function Pos(Substr, S: AnsiString): Integer;
  function ContainsText(aText, aSubText: AnsiString): Boolean;
  function StartsText(aSubText, aText: AnsiString): Boolean;
  function EndsText(aSubText, aText: AnsiString): Boolean;
  function ReplaceText(aText, aFromText, aToText: AnsiString): AnsiString;
  function StringReplace(S, OldPattern, NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString;
  */

implementation

procedure SetLength(var aString: AnsiString; aLength: Integer);
begin
  aString.SetLength(aLength);
end;

constructor AnsiString(Value: PlatformString; AsUTF16Bytes: Boolean := false);
begin
  if AsUTF16Bytes then begin
    var lTemp := Value.ToCharArray;  
    fData := CharArrayToByteArray(lTemp, 0, Value.Length);
  end
  else 
    fData := StringToUTF8(Value);
end;

method AnsiString.SetLength(aLength: Integer);
begin
  if aLength > 0 then begin
    var lTmp := new Byte[aLength];
    for i: Integer := 0 to aLength - 1 do
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
  result := create(Value, 0, Value.Length);
end;

operator AnsiString.Implicit(Value: Char): AnsiString;
begin
  result := new AnsiString(Byte(Value), 1);
end;

operator AnsiString.Implicit(Value: PlatformString): AnsiString;
begin
  result := new AnsiString(Value);
end;

operator AnsiString.Implicit(Value: AnsiString): String;
begin

end;

operator AnsiString.Add(Value1: AnsiString; Value2: Char): AnsiString;
begin
  result := new AnsiString(Value1);
  result.Insert(Value1.Length, Value2);
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

end;

operator AnsiString.NotEqual(Value1: AnsiString; Value2: AnsiString): Boolean;
begin

end;

operator AnsiString.Greater(Value1: AnsiString; Value2: AnsiString): Boolean;
begin

end;

operator AnsiString.Less(Value1: AnsiString; Value2: AnsiString): Boolean;
begin

end;

operator AnsiString.GreaterOrEqual(Value1: AnsiString; Value2: AnsiString): Boolean;
begin

end;

operator AnsiString.LessOrEqual(Value1: AnsiString; Value2: AnsiString): Boolean;
begin

end;

class method AnsiString.SetOffset(Value: Integer);
begin
  fOffset := Value;
end;

method AnsiString.GetOffsetChar(aIndex: Integer): AnsiChar;
begin

end;

method AnsiString.SetOffsetChar(aIndex: Integer; Value: AnsiChar);
begin

end;

method AnsiString.CharArrayToByteArray(aCharArray: array of Char; StartIndex: Integer; aLength: Integer): array of Byte;
begin
  result := new Byte[aLength * 2];
  for i: Integer := StartIndex to (StartIndex + aLength) - 1 do
  begin
    var lChar := Word(aCharArray[i]);
    result[2 * i] := low(lChar);
    result[(2 * i) + 1] := high(lChar);
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
    fData[i] := C;
end;

constructor AnsiString(Value: array of Byte);
begin
  constructor(Value, 0, Value.Length);
end;

constructor AnsiString(Value: array of Byte; StartIndex: Integer; aLength: Integer);
begin
  fData := new Byte[Value.Length];
  for i: Integer := StartIndex to (StartIndex + aLength) - 1 do
  fData[i - StartIndex] := Value[i]; 
end;

method AnsiString.GetData: array of Byte;
begin
  result := fData;
end;

method AnsiString.GetChar(aIndex: Integer): AnsiChar;
begin
  result := fData[aIndex];
end;

method AnsiString.SetChar(aIndex: Integer; Value: AnsiChar);
begin
  fData[aIndex] := Value;
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
  var Data := Binary(aString.dataUsingEncoding(NSStringEncoding.NSUTF8StringEncoding));
  exit Data.ToArray as not nullable;
  {$ENDIF}
end;

method AnsiString.Insert(aIndex: Integer; Value: AnsiString): AnsiString;
begin
  var lOldData := fData;
  fData := new Byte[lOldData.Length + Value.Length];
  CopyArray(lOldData, 0, var fData, 0, aIndex);
  CopyArray(Value.Data, 0, var fData, aIndex, Value.Length);
  CopyArray(lOldData, aIndex, var fData, aIndex + Value.Length, Length - aIndex);
  result := self;
end;

method AnsiString.Insert(aIndex: Integer; Value: Char): AnsiString;
begin
  result := Insert(aIndex, Byte(Value));
end;

method AnsiString.Insert(aIndex: Integer; Value: AnsiChar): AnsiString;
begin
  var lOldData := fData;
  fData := new Byte[lOldData.Length + 1];
  CopyArray(lOldData, 0, var fData, 0, aIndex);
  fData[aIndex] := Value;
  CopyArray(lOldData, aIndex, var fData, aIndex + 1, Length - aIndex);
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
  fData := new Byte[length - Count];
  CopyArray(lOldData, 0, var fData, 0, StartIndex);
  CopyArray(lOldData, StartIndex + Count, var fData, StartIndex, Length - (StartIndex + Count));
  result := self;
end;

class method AnsiString.CompareStr(S1: AnsiString; S2: AnsiString): Integer;
begin

end;

class method AnsiString.SameStr(S1: AnsiString; S2: AnsiString): Boolean;
begin

end;

class method AnsiString.CompareText(S1: AnsiString; S2: AnsiString): Integer;
begin

end;

class method AnsiString.SameText(S1: AnsiString; S2: AnsiString): Boolean;
begin

end;

method AnsiString.CopyTo(SourceIndex: Integer; var Destination: array of Byte; DestinationIndex: Integer; Count: Integer);
begin

end;

method AnsiString.ToUpper: AnsiString;
begin

end;

method AnsiString.ToLower: AnsiString;
begin

end;

method AnsiString.Trim: AnsiString;
begin

end;

method AnsiString.TrimLeft: AnsiString;
begin

end;

method AnsiString.TrimRight: AnsiString;
begin

end;

method AnsiString.IndexOf(Substr: AnsiString): Integer;
begin

end;

method AnsiString.Contains(aSubText: AnsiString): Boolean;
begin

end;

method AnsiString.StartsWith(aSubText: AnsiString): Boolean;
begin

end;

method AnsiString.EndsWith(aSubText: AnsiString): Boolean;
begin

end;

method AnsiString.Replace(aFromText: AnsiString; aToText: AnsiString): AnsiString;
begin

end;

method AnsiString.Replace(OldPattern: AnsiString; NewPattern: AnsiString; aFlags: TReplaceFlags): AnsiString;
begin

end;


end.
