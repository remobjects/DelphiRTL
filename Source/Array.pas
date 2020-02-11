namespace RemObjects.Elements.RTL.Delphi;

interface

method SetLenth<T>(var aArray: array of T; aNewLength: Integer);

implementation

method SetLenth<T>(var aArray: array of T; aNewLength: Integer);
begin
  var len := length(aArray);
  if len = aNewLength then
    exit;
  var lResult := new T[aNewLength];
  for i: Integer := 0 to Min(len, aNewLength)-1 do
    lResult[i] := aArray[i];
  aArray := lResult;
end;

end.