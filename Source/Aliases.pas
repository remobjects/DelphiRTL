namespace RemObjects.Elements.RTL.Delphi;

type
  TClass = Sugar.Reflection.Type;
  ShortString = String;
  Pointer = {$IF COOPER}Integer{$ELSE}^Void{$ENDIF};

  TGUID = Sugar.Guid;
  
  PInterfaceEntry = Pointer;
  PInterfaceTable = Pointer;
  HResult = Int64;
  
end.
