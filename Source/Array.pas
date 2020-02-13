namespace RemObjects.Elements.RTL.Delphi;

interface

{$HIDE CPW8}

method SetLenth<T>(var aArray: array of T; aNewLength: Integer);
method FillChar(var aArray: array of Byte; aValue: Byte);
{$IFNDEF COOPER}
method FillChar(aDestination: ^Byte; aCount: Integer; aValue: Byte); unsafe;
method Move(aSource: ^Byte; aDestination: ^Byte; aCount: Integer); unsafe;
{$ENDIF}

implementation

method SetLenth<T>(var aArray: array of T; aNewLength: Integer);
begin
  if not assigned(aArray) then
    exit;
  var len: Integer := length(aArray);
  if len = aNewLength then
    exit;
  {$IF COOPER}
  aArray := java.util.Arrays.copyOf(aArray, aNewLength);
  {$ELSE}
  var lResult := new T[aNewLength];
  for i: Integer := 0 to Min(len, aNewLength)-1 do
    lResult[i] := aArray[i];
  aArray := lResult;
  {$ENDIF}
end;

method FillChar(var aArray: array of Byte; aValue: Byte);
begin
  for i: Integer := 0 to length(aArray)-1 do
    aArray[i] := aValue;
end;

{$IFNDEF COOPER}
method FillChar(aDestination: ^Byte; aCount: Integer; aValue: Byte);
begin
  for i: Integer := 0 to aCount-1 do
    (aDestination+i)^ := aValue;
end;

method Move(aSource: ^Byte; aDestination: ^Byte; aCount: Integer);
begin
  {$IF ECHOES}
  for i: Integer := 0 to aCount-1 do
    (aDestination+i)^ := (aSource+i)^;
  {$ELSEIF ISLAND}
  memcpy(aSource, aDestination, aCount);
  {$ENDIF}
end;

{$ENDIF}

end.