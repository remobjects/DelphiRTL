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
procedure Insert(aSource: DelphiString; var aTarget: DelphiString; aOffset: Integer); inline;
procedure Insert(aSource: PlatformString; var aTarget: PlatformString; aOffset: Integer); inline;
procedure Delete(var S: DelphiString; aIndex: Integer; aCount: Integer); inline;
function &Copy(S: DelphiString; aIndex: Integer; aCount: Integer): DelphiString; inline;
function &Copy(S: PlatformString; aIndex: Integer; aCount: Integer): DelphiString; inline;
procedure FillChar(var Dest: DelphiString; aCount: Integer; aValue: Char);
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
  result := DelphiString(S).IndexOf(DelphiString(SubStr), aOffset) + 1;
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

procedure FillChar(var Dest: DelphiString; aCount: Integer; aValue: Char);
begin
  for i: Integer := 0 to Dest.Length - 1 do
  Dest.Chars[i] := aValue;
end;

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