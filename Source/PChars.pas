namespace RemObjects.Elements.RTL.Delphi;

interface

method StrLen(aString: PChar): Integer; public;

implementation

method StrLen(aString: PChar): Integer;
begin
  while aString^ ≠ #0 do begin
    inc(result);
    inc(aString);
  end;
end;


end.