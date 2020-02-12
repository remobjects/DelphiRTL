namespace RemObjects.Elements.RTL.Delphi;

interface

method SetLenth<T>(var aArray: array of T; aNewLength: Integer);

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

end.