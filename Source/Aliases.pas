namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL;

type
  TClass = public Pointer;
  ShortString = String;
  Pointer = public {$IF COOPER}Integer{$ELSE}^Void{$ENDIF};

  TGUID = public RemObjects.Elements.RTL.Guid;

  PInterfaceEntry = public Pointer;
  PInterfaceTable = public Pointer;
  HResult = public Int64;

  DWORD = public UInt32;
  BYTEBOOL = public Boolean;

end.