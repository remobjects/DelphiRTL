namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL;

{$GLOBALS ON}

type
  THandle = public Int64; // Since native types are not supported on Java, THandle stores a 64 bit integer always
  TArray<T> = array of T;
  TDateTime = public Double;
  TDate = public TDateTime;
  TTime = public TDateTime;
  {$IF NOT COOPER}
  TCustomAttribute = public Attribute;
  {$ENDIF}

function Pos(SubStr: DelphiString; S: DelphiString; aOffset: Integer := 1): Integer; inline;
function Pos(SubStr: PlatformString; S: PlatformString; aOffset: Integer := 1): Integer; inline;
function Pos(SubStr: PlatformString; S: DelphiString; aOffset: Integer := 1): Integer; inline;
function Pos(SubStr: DelphiString; S: PlatformString; aOffset: Integer := 1): Integer; inline;
procedure Insert(aSource: DelphiString; var aTarget: DelphiString; aOffset: Integer); inline;
procedure Insert(aSource: PlatformString; var aTarget: PlatformString; aOffset: Integer); inline;
procedure Delete(var S: DelphiString; aIndex: Integer; aCount: Integer); inline;
function &Copy(S: DelphiString; aIndex: Integer; aCount: Integer): DelphiString; inline;
function &Copy(S: PlatformString; aIndex: Integer; aCount: Integer): DelphiString; inline;
{$IF NOT COOPER}
//function &Copy<T>(B: array of T; aIndex: Integer; aCount: Integer): array of T; public;
//function &Copy<T>(B: TArray<T>; aIndex: Integer): TArray<T>; inline; public;
//procedure Delete<T>(var Arr: TArray<T>; Start: Integer; Count: Integer);
{$ENDIF}

procedure FillChar(var Dest: DelphiString; aCount: Integer; aValue: Char);
{$IF NOT COOPER}
procedure FillChar(var Dest; aCount: Integer; aValue: Byte); unsafe;
procedure Move(const ASource; var Dest; Count: NativeInt); unsafe;
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; unsafe;
{$ENDIF COOPER}

function StringOfChar(aCh: Char; aCount: Integer): DelphiString; inline;

function Trunc(Val: Double): Integer; inline;
function Abs(Val: Double): Double; inline;
function Abs(Val: Integer): Integer; inline;
function Abs(Val: Int64): Int64; inline;
function Round(Val: Double): Int64; inline;

function Sqrt(X: Double): Double; inline;
function Frac(X: Double): Double; inline;
function Exp(X: Double): Double; inline;
function Cos(X: Double): Double; inline;
function Sin(X: Double): Double; inline;
//function Ln(const X: Double): Double; inline;
function ArcTan(X: Double): Double; inline;
function Tangent(X: Double): Double; inline;
procedure SineCosine(X: Double; var aSin, aCos: Double);

{$IF NOT COOPER}
function InterlockedIncrement(var Addend: Integer): Integer;
function InterlockedDecrement(var Addend: Integer): Integer;
{$ENDIF}

implementation

function Pos(SubStr: DelphiString; S: DelphiString; aOffset: Integer := 1): Integer;
begin
  result := S.IndexOf(SubStr, aOffset) + 1;
end;

function Pos(SubStr: PlatformString; S: PlatformString; aOffset: Integer := 1): Integer;
begin
  result := Pos(DelphiString(SubStr), DelphiString(S));
end;

function Pos(SubStr: PlatformString; S: DelphiString; aOffset: Integer := 1): Integer;
begin
  result := Pos(DelphiString(SubStr), S);
end;

function Pos(SubStr: DelphiString; S: PlatformString; aOffset: Integer := 1): Integer;
begin
  result := Pos(SubStr, DelphiString(S));
end;

procedure Insert(aSource: DelphiString; var aTarget: DelphiString; aOffset: Integer);
begin
  aTarget.Insert(aOffset - 1, aSource);
end;

procedure Insert(aSource: PlatformString; var aTarget: PlatformString; aOffset: Integer);
begin
  DelphiString(aTarget).Insert(aOffset - 1, aSource);
end;

procedure Delete(var S: DelphiString; aIndex: Integer; aCount: Integer);
begin
  S.Remove(aIndex - 1, aCount);
end;

function &Copy(S: DelphiString; aIndex: Integer; aCount: Integer): DelphiString;
begin
  result := S.SubString(aIndex - 1, aCount);
end;

function &Copy(S: PlatformString; aIndex: Integer; aCount: Integer): DelphiString;
begin
  result := DelphiString(S).SubString(aIndex - 1, aCount);
end;

{$IF NOT COOPER}
//function __Copy<T>(B: array of T; aIndex: Integer; aCount: Integer): array of T;
//begin
  //result := new T[aCount];
  //{$IF TOFFEE}
  //for i: Integer := 0 to aCount do
    //result[i] := B[aIndex + i];
  //{$ELSE}
  //system.Array.Copy(B, aIndex, result, 0, aCount);
  //{$ENDIF}
//end;

//function &Copy<T>(B: TArray<T>; aIndex: Integer): TArray<T>;
//begin
  //result := __Copy(B, aIndex, B.length);
//end;
{$ENDIF}

procedure FillChar(var Dest: DelphiString; aCount: Integer; aValue: Char);
begin
  for i: Integer := 0 to Dest.Length - 1 do
  Dest.Chars[i] := aValue;
end;

{$IF NOT COOPER}
//procedure Delete<T>(var Arr: TArray<T>; Start: Integer; Count: Integer);
//begin
  //var Source := Arr;
  //Arr := new T[Arr.Length - Count];
  //for Index: Integer := 0 to Start - 1 do
    //Arr[Index] := Source[Index];

  //for Index: Integer := Start + Count to Source.Length - 1 do
    //Arr[Index - Count] := Source[Index];
//end;
{$ENDIF}

{$IF NOT COOPER}
procedure FillChar(var Dest; aCount: Integer; aValue: Byte);
begin
  var Ptr := PByte(@Dest); pinned;
  for Idx: Integer := 0 to aCount - 1 do
  begin
    Ptr^ := aValue;
    inc(Ptr);
  end;
end;

procedure Move(const ASource; var Dest; Count: NativeInt); unsafe;
begin
  var SourcePtr := PByte(@ASource); pinned;
  var DestPtr := PByte(@Dest); pinned;
  for Idx: Integer := 0 to Count - 1 do
  begin
    DestPtr^ := SourcePtr^;
    inc(SourcePtr);
    inc(DestPtr);
  end;
  //System.Buffer.MemoryCopy(@Source, @Dest, Count, Count);
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
begin
  var Left := PByte(P1); pinned;
  var Right := PByte(P2); pinned;
  for Idx: Integer := 0 to Length - 1 do
  begin
    if Left^ <> Right^ then
      exit(False);
    inc(Left);
    inc(Right);
  end;

  exit(True);
end;
{$ENDIF COOPER}

function StringOfChar(aCh: Char; aCount: Integer): DelphiString;
begin
  result := DelphiString.Create(aCh, aCount);
end;

function Trunc(Val: Double): Integer;
begin
  result := Integer(Math.Truncate(Val));
end;

function Abs(Val: Double): Double;
begin
  result := Math.Abs(Val);
end;

function Abs(Val: Integer): Integer;
begin
  result := Math.Abs(Val);
end;

function Abs(Val: Int64): Int64;
begin
  result := Math.Abs(Val);
end;

function Round(Val: Double): Int64;
begin
  result := Math.Round(Val)
end;

function Sqrt(X: Double): Double;
begin
  result := Math.Sqrt(X);
end;

function Frac(X: Double): Double;
begin
  // TODO
end;

function Exp(X: Double): Double;
begin
  result := Math.Exp(X);
end;

function Cos(X: Double): Double;
begin
  result := Math.Cos(X);
end;

function Sin(X: Double): Double;
begin
  result := Math.Sin(X);
end;

function ArcTan(X: Double): Double;
begin
  result := Math.Atan(X);
end;

function Tangent(X: Double): Double;
begin
  result := Math.Tan(X);
end;

procedure SineCosine(X: Double; var aSin, aCos: Double);
begin
  aSin := Math.Sin(X);
  aCos := Math.Cos(X);
end;

{$IF NOT COOPER}
function InterlockedIncrement(var Addend: Integer): Integer;
begin
  interlockedInc(var Addend);
end;

function InterlockedDecrement(var Addend: Integer): Integer;
begin
  interlockedDec(var Addend);
end;
{$ENDIF}

end.