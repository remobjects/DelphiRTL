namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL;

type
  TCharArray = public TArray<Char>;

  TStringBuilder = public class(TObject)
  private
    fData: RemObjects.Elements.RTL.StringBuilder;
    method GetChars(aIndex : Integer): Char; {$IF NOT COOPER}inline;{$ENDIF}
    method SetChars(aIndex : Integer; Value: Char); {$IF NOT COOPER AND NOT TOFFEE}inline;{$ENDIF}
  public
    constructor;
    constructor(aCapacity: Integer);
    constructor (const Value: DelphiString);
    constructor (aCapacity: Integer; aMaxCapacity: Integer);
    constructor (const Value: DelphiString; aCapacity: Integer);
    constructor (const Value: DelphiString; StartIndex: Integer; aLength: Integer; aCapacity: Integer);

    class method Create: TStringBuilder;
    class method Create(aCapacity: Integer): TStringBuilder;
    class method Create(const Value: DelphiString): TStringBuilder;
    class method Create(aCapacity: Integer; aMaxCapacity: Integer): TStringBuilder;
    class method Create(const Value: DelphiString; aCapacity: Integer): TStringBuilder;
    class method Create(const Value: DelphiString; StartIndex: Integer; aLength: Integer; aCapacity: Integer): TStringBuilder;
    class operator Equal(Value1, Value2: TStringBuilder): Boolean;
    
    method Append(const Value: Boolean): TStringBuilder;
    method Append(const Value: Byte): TStringBuilder;
    method Append(const Value: Char): TStringBuilder;
    method Append(const Value: Double): TStringBuilder;
    method Append(const Value: SmallInt): TStringBuilder;
    method Append(const Value: Integer): TStringBuilder;
    method Append(const Value: Int64): TStringBuilder;
    method Append(const Value: TObject): TStringBuilder;
    method Append(const Value: Single): TStringBuilder;
    method Append(const Value: DelphiString): TStringBuilder;
    method Append(const Value: array of Char): TStringBuilder;
    {$IF NOT COOPER}
    method Append(const Value: ShortInt): TStringBuilder;
    method Append(const Value: UInt64): TStringBuilder;
    method Append(const Value: Word): TStringBuilder;
    method Append(const Value: Cardinal): TStringBuilder;
    {$ENDIF}
    method Append(const Value: Char; RepeatCount: Integer): TStringBuilder;
    method Append(const Value: array of Char; StartIndex: Integer; CharCount: Integer): TStringBuilder;
    method Append(const Value: DelphiString; StartIndex: Integer; Count: Integer): TStringBuilder;
    method AppendFormat(const Format: DelphiString; const Args: array of Object): TStringBuilder;
    method AppendLine: TStringBuilder;
    method AppendLine(const Value: DelphiString): TStringBuilder;
    method Clear;
    method CopyTo(SourceIndex: Integer; const Destination: array of Char; DestinationIndex: Integer; aCount: Integer);
    method EnsureCapacity(aCapacity: Integer): Integer;
    method &Equals(aValue: TStringBuilder): Boolean;

    method Insert(aIndex: Integer; const Value: Boolean): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Byte): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Char): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Double): TStringBuilder;
    method Insert(aIndex: Integer; const Value: SmallInt): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Integer): TStringBuilder;
    method Insert(aIndex: Integer; const Value: array of Char): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Int64): TStringBuilder;
    method Insert(aIndex: Integer; const Value: TObject): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Single): TStringBuilder;
    method Insert(aIndex: Integer; const Value: DelphiString): TStringBuilder;
    {$IF NOT COOPER}
    method Insert(aIndex: Integer; const Value: ShortInt): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Word): TStringBuilder;
    method Insert(aIndex: Integer; const Value: Cardinal): TStringBuilder;
    method Insert(aIndex: Integer; const Value: UInt64): TStringBuilder;
    {$ENDIF}
    method Insert(aIndex: Integer; const Value: DelphiString; aCount: Integer): TStringBuilder;
    method Insert(aIndex: Integer; const Value: array of Char; startIndex: Integer; charCount: Integer): TStringBuilder;

    method &Remove(StartIndex: Integer; RemLength: Integer): TStringBuilder;

    method Replace(const OldChar: Char; const NewChar: Char): TStringBuilder; inline;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString): TStringBuilder; inline;
    method Replace(const OldChar: Char; const NewChar: Char; StartIndex: Integer; Count: Integer): TStringBuilder;
    method Replace(const OldValue: DelphiString; const NewValue: DelphiString; StartIndex: Integer; Count: Integer): TStringBuilder;

    [ToString]
    method ToString: String; override;
    method ToString(StartIndex: Integer; StrLength: Integer): DelphiString;

    property Capacity: Integer read fData.Length write fData.Length;
    property Chars[index: Integer]: Char read GetChars write SetChars; default;
    property Length: Integer read fData.Length write fData.Length;
    property MaxCapacity: Integer read Consts.MaxInt32;
  end;

implementation

constructor TStringBuilder;
begin
  fData := new StringBuilder();
end;

constructor TStringBuilder(aCapacity: Integer);
begin
  fData := new StringBuilder(aCapacity);
end;

constructor TStringBuilder(const Value: DelphiString);
begin
  fData := new StringBuilder(Value);
end;

constructor TStringBuilder(aCapacity: Integer; aMaxCapacity: Integer);
begin
  fData := new StringBuilder(aCapacity);
end;

constructor TStringBuilder(const Value: DelphiString; aCapacity: Integer);
begin
  fData := new StringBuilder(aCapacity);
  Append(Value);
end;

constructor TStringBuilder(const Value: DelphiString; StartIndex: Integer; aLength: Integer; aCapacity: Integer);
begin
  fData := new StringBuilder(aCapacity);
  Append(Value);
end;

class method TStringBuilder.Create: TStringBuilder;
begin
  result := new TStringBuilder();
end;

class method TStringBuilder.Create(aCapacity: Integer): TStringBuilder;
begin
  result := new TStringBuilder(aCapacity);
end;

class method TStringBuilder.Create(const Value: DelphiString): TStringBuilder;
begin
  result := new TStringBuilder(Value);
end;

class method TStringBuilder.Create(aCapacity: Integer; aMaxCapacity: Integer): TStringBuilder;
begin
  result := new TStringBuilder(aCapacity, aMaxCapacity);
end;

class method TStringBuilder.Create(const Value: DelphiString; aCapacity: Integer): TStringBuilder;
begin
  result := new TStringBuilder(Value, aCapacity);
end;

class method TStringBuilder.Create(const Value: DelphiString; StartIndex: Integer; aLength: Integer; aCapacity: Integer): TStringBuilder;
begin
  result := new TStringBuilder(Value, StartIndex, aLength, aCapacity);
end;

class operator TStringBuilder.Equal(Value1, Value2: TStringBuilder): Boolean;
begin
  result := Value1.Equals(Value2);
end;

method TStringBuilder.GetChars(aIndex : Integer): Char;
begin
  result := fData[aIndex];
end;

method TStringBuilder.SetChars(aIndex : Integer; Value: Char);
begin
  fData[aIndex] := Value;
end;

method TStringBuilder.Append(const Value: Boolean): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Byte): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Char): TStringBuilder;
begin
  fData.Append(Value);
  result := self;
end;

method TStringBuilder.Append(const Value: Double): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: SmallInt): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Integer): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Int64): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: TObject): TStringBuilder;
begin
  fData.Append(Value.ToString);
  result := self;
end;

method TStringBuilder.Append(const Value: Single): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: DelphiString): TStringBuilder;
begin
  fData.Append(Value);
  result := self;
end;

{$IF NOT COOPER}
method TStringBuilder.Append(const Value: ShortInt): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: UInt64): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Word): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Cardinal): TStringBuilder;
begin
  fData.Append(Convert.ToString(Value));
  result := self;
end;
{$ENDIF}

method TStringBuilder.Append(const Value: array of Char): TStringBuilder;
begin
  fData.Append(DelphiString.Create(Value));
  result := self;
end;

method TStringBuilder.Append(const Value: Char; RepeatCount: Integer): TStringBuilder;
begin
  fData.Append(Value, RepeatCount);
  result := self;
end;

method TStringBuilder.Append(const Value: array of Char; StartIndex: Integer; CharCount: Integer): TStringBuilder;
begin
  fData.Append(DelphiString.Create(Value, StartIndex, CharCount), StartIndex, CharCount);
  result := self;
end;

method TStringBuilder.Append(const Value: DelphiString; StartIndex: Integer; Count: Integer): TStringBuilder;
begin
  fData.Append(Value, StartIndex, Count);
  result := self;
end;

method TStringBuilder.AppendFormat(const Format: DelphiString; const Args: array of Object): TStringBuilder;
begin
  fData.AppendFormat(Format, Args);
  result := self;
end;

method TStringBuilder.AppendLine: TStringBuilder;
begin
  fData.AppendLine;
  result := self;
end;

method TStringBuilder.AppendLine(const Value: DelphiString): TStringBuilder;
begin
  fData.AppendLine(Value);
  result := self;
end;

method TStringBuilder.Clear;
begin
  fData.Clear;
end;

method TStringBuilder.CopyTo(SourceIndex: Integer; const Destination: array of Char; DestinationIndex: Integer; aCount: Integer);
begin
  for i: Integer := 0 to aCount - 1 do
    Destination[DestinationIndex + i] := GetChars(SourceIndex + i);
end;

method TStringBuilder.EnsureCapacity(aCapacity: Integer): Integer;
begin
  result := aCapacity;
end;

method TStringBuilder.&Equals(aValue: TStringBuilder): Boolean;
begin
  result := false;
  if Length <> aValue.Length then 
    exit;

  for i: Integer := 0 to Length - 1 do
    if GetChars(i) <> aValue[i] then
      exit;
  
  result := true;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Boolean): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Byte): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Char): TStringBuilder;
begin
  fData.Insert(aIndex, Value);
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Double): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: SmallInt): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Integer): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: array of Char): TStringBuilder;
begin
  fData.Insert(aIndex, DelphiString.Create(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Int64): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: TObject): TStringBuilder;
begin
  fData.Insert(aIndex, Value.ToString);
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Single): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: DelphiString): TStringBuilder;
begin
  fData.Insert(aIndex, Value);
  result := self;
end;

{$IF NOT COOPER}
method TStringBuilder.Insert(aIndex: Integer; const Value: ShortInt): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Word): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: Cardinal): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: UInt64): TStringBuilder;
begin
  fData.Insert(aIndex, Convert.ToString(Value));
  result := self;
end;
{$ENDIF}

method TStringBuilder.Insert(aIndex: Integer; const Value: DelphiString; aCount: Integer): TStringBuilder;
begin
  for i: Integer := 0 to aCount - 1 do
    fData.Insert(aIndex, Value);

  result := self;
end;

method TStringBuilder.Insert(aIndex: Integer; const Value: array of Char; startIndex: Integer; charCount: Integer): TStringBuilder;
begin
  fData.Insert(aIndex, DelphiString.Create(Value, startIndex, charCount));
  result := self;
end;

method TStringBuilder.&Remove(StartIndex: Integer; RemLength: Integer): TStringBuilder;
begin
  fData.Delete(StartIndex, RemLength);
  result := self;
end;

method TStringBuilder.Replace(const OldChar: Char; const NewChar: Char): TStringBuilder;
begin
  result := Replace(OldChar, NewChar, 0, Length);
end;

method TStringBuilder.Replace(const OldValue: DelphiString; const NewValue: DelphiString): TStringBuilder;
begin
  result := Replace(OldValue, NewValue, 0, Length);
end;

method TStringBuilder.Replace(const OldChar: Char; const NewChar: Char; StartIndex: Integer; Count: Integer): TStringBuilder;
begin
  for i: Integer := 0 to Count - 1 do
    if GetChars(i + StartIndex) = OldChar then
      SetChars(i + StartIndex, NewChar);

  result := self;
end;

method TStringBuilder.Replace(const OldValue: DelphiString; const NewValue: DelphiString; StartIndex: Integer; Count: Integer): TStringBuilder;
begin
  result := self;
  if (OldValue = '') or (Length = 0) or (OldValue.Length > Count) then
    exit;

  var lTemp: String;   
  var i := StartIndex;
  while i < (StartIndex + Count) do begin
    if GetChars(i) = OldValue.Chars[0] then begin
      var j := i + 1;
      var k := 1;
      while (j < Length) and (k < OldValue.Length) and (GetChars(j) = OldValue.Chars[k]) do begin
        inc(j);
        inc(k);
      end;
      if k = OldValue.Length then begin
        lTemp := lTemp + NewValue;
        inc(i, OldValue.Length);
      end
      else begin
        lTemp := lTemp + GetChars(i);
        inc(i); 
      end;    
    end
    else begin
      lTemp := lTemp + fData[i];
      inc(i);
    end;
  end;
  fData.Replace(StartIndex, Count, lTemp);
end;

method TStringBuilder.ToString: String;
begin
  result := fData.Substring(0);
end;

method TStringBuilder.ToString(StartIndex: Integer; StrLength: Integer): DelphiString;
begin
  result := fData.Substring(StartIndex, StrLength);
end;

end.
