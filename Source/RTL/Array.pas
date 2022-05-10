namespace RemObjects.Elements.RTL.Delphi;

interface

{$HIDE CPW8}

method SetLength<T>(var aArray: array of T; aNewLength: Integer);
method FillChar(var aArray: array of Byte; aValue: Byte);
{$IFNDEF COOPER}
method FillChar(aDestination: ^Byte; aCount: Integer; aValue: Byte); unsafe;
method Move(aSource: ^Byte; aDestination: ^Byte; aCount: Integer); unsafe;
{$ENDIF}

method Insert<T>(aSource: T; var aDestination: array of T; aIndex: Integer);
method Insert<T>(aSource: array of T; var aDestination: array of T; aIndex: Integer);
method Delete<T>(var aDestination: array of T; aIndex: Integer; aCount: Integer := 1);
method Concat<T>(params aArrays: array of array of T): nullable array of T;

implementation

uses
  RemObjects.Elements.RTL;

method SetLength<T>(var aArray: array of T; aNewLength: Integer);
begin
  if not assigned(aArray) then
    exit;
  var len: Integer := length(aArray);
  if len = aNewLength then
    exit;
  {$IF COOPER}
  aArray := java.util.Arrays.copyOf(aArray, aNewLength);
  {$ELSEIF ECHOES}
  var lResult := new array of T(aNewLength);
  aArray.Copy(aArray, lResult, Min(len, aNewLength));
  aArray := lResult;
  {$ELSE}
  var lResult := new T[aNewLength];
  for i: Integer := 0 to Min(len, aNewLength)-1 do
    lResult[i] := aArray[i];
  aArray := lResult;
  {$ENDIF}
end;

method FillChar(var aArray: array of Byte; aValue: Byte);
begin
  for i: Integer := 0 to RemObjects.Elements.System.length(aArray)-1 do
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

method Insert<T>(aSource: T; var aDestination: array of T; aIndex: Integer);
begin
  Insert([aSource], var aDestination, aIndex);
end;

method Insert<T>(aSource: array of T; var aDestination: array of T; aIndex: Integer);
begin
  var lCurrentCount := length(aDestination);
  if aIndex > lCurrentCount then
    raise new ArgumentOutOfRangeException($"Index {aIndex} is out of range of destination array (0..{lCurrentCount})");
  var lAddedCount := length(aSource);
  if lAddedCount > 0 then begin
    {$IF COOPER}
    var lResult := java.util.Arrays.copyOf(aDestination, lCurrentCount+lAddedCount);
    //for i := 0 to aIndex-1 do
      //lResult[i] := aDestination[i];
    for i := 0 to lAddedCount-1 do
      lResult[aIndex+i] := aSource[i];
    for i := aIndex to lCurrentCount-1 do
      lResult[aIndex+lAddedCount+i] := aDestination[aIndex+i];
    aDestination := lResult;
    {$ELSE}
    var lResult := new T[lCurrentCount+lAddedCount];
    for i := 0 to aIndex-1 do
      lResult[i] := aDestination[i];
    for i := 0 to lAddedCount-1 do
      lResult[aIndex+i] := aSource[i];
    for i := aIndex to lCurrentCount-1 do
      lResult[lAddedCount+i] := aDestination[i];
    aDestination := lResult;
    {$ENDIF}
  end;
end;

method Delete<T>(var aDestination: array of T; aIndex: Integer; aCount: Integer := 1);
begin
  if aCount ≤ 0 then
    exit;
  if aIndex+aCount ≥ length(aDestination) then begin
    SetLength(var aDestination, Math.Min(aIndex, Int64(length(aDestination))));
  end
  else begin
    var lCurrentCount := length(aDestination);
    if lCurrentCount-aCount ≤ 0 then begin
      SetLength(var aDestination, 0);
    end
    else begin
      {$IF COOPER}
      var lResult := java.util.Arrays.copyOf(aDestination, lCurrentCount-aCount);
      //for i := 0 to aIndex-1 do
        //lResult[i] := aDestination[i];
      for i := aIndex+aCount to lCurrentCount-1 do
        lResult[i-aCount] := aDestination[i];
      aDestination := lResult;
      {$ELSE}
      var lResult := new T[lCurrentCount-aCount];
      for i := 0 to aIndex-1 do
        lResult[i] := aDestination[i];
      for i := aIndex+aCount to lCurrentCount-1 do
        lResult[i-aCount] := aDestination[i];
      aDestination := lResult;
      {$ENDIF}
    end;
  end;
end;

method Concat<T>(params aArrays: array of array of T): nullable array of T;
begin
  if length(aArrays) = 0 then
    exit nil;

  var lTotalCount := 0;
  for each a in aArrays do
    inc(lTotalCount, length(a));

  result := if defined("COOPER") then java.util.Arrays.copyOf(aArrays[0], lTotalCount) else new T[lTotalCount];
  var lOffset := 0;
  {$IF COOPER}
  inc(lOffset, length(aArrays[0]));
  for each a in aArrays.Skip(1) do begin
    for i := 0 to length(a)-1 do
      result[lOffset+i] := a[i];
    inc(lOffset, length(a));
  end;
  {$ELSE}
  for each a in aArrays do begin
    for i := 0 to length(a)-1 do
      result[lOffset+i] := a[i];
    inc(lOffset, length(a));
  end;
  {$ENDIF}
end;


end.