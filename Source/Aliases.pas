namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL;

type
  //TClass = Sugar.Reflection.Type;
  TClass = Pointer;
  ShortString = String;
  Pointer = {$IF COOPER}Integer{$ELSE}^Void{$ENDIF};

  TGUID = RemObjects.Elements.RTL.Guid;

  PInterfaceEntry = Pointer;
  PInterfaceTable = Pointer;
  HResult = Int64;

end.