namespace RemObjects.Elements.RTL.Delphi;

{$IF NOT COOPER}

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

{$ENDIF}


end.