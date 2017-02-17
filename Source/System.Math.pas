namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL;

{$GLOBALS ON}

function ArcCos(X: Double): Double; inline;
function ArcTan2(Y, X: Double): Double; inline;
function ArcSin(X: Double): Double; inline;
function Floor(X: Double): Integer; inline;
function Log10(X: Double): Double; inline;
function Max(A, B: Integer): Integer; inline;
function Max(A, B: Int64): Int64; inline;
function Max(A, B: Double): Double; inline;
function Min(A, B: Integer): Integer; inline;
function Min(A, B: Int64): Int64; inline;
function Min(A, B: Double): Double; inline;
function Power(Base, Exponent: Double): Double; inline;

implementation

function ArcCos(X: Double): Double;
begin
  result := Math.Acos(X);
end;

function ArcTan2(Y, X: Double): Double;
begin
  result := Math.Atan2(Y, X);
end;

function ArcSin(X: Double): Double;
begin
  result := Math.Asin(X);
end;

function Floor(X: Double): Integer;
begin
  result := Integer(Math.Floor(X));
end;

function Log10(X: Double): Double;
begin
  result := Math.Log10(X);
end;

function Max(A, B: Integer): Integer;
begin
  result := Math.Max(A, B);
end;

function Max(A, B: Int64): Int64;
begin
  result := Math.Max(A, B);
end;

function Max(A, B: Double): Double;
begin
  result := Math.Max(A, B);
end;

function Min(const A, B: Integer): Integer;
begin
  result := Math.Min(A, B);
end;

function Min(const A, B: Int64): Int64;
begin
  result := Math.Min(A, B);
end;

function Min(const A, B: Double): Double;
begin
  result := Math.Min(A, B);
end;

function Power(Base, Exponent: Double): Double;
begin
  result := Math.Pow(Base, Exponent);
end;



end.
