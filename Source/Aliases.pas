namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL;

type
  {$IF ECHOES}
  TClass = public System.Type;
  {$ELSE}
  TClass = public Pointer;
  {$ENDIF}
  ShortString = String;
  {$HIDE CPW8}
  Pointer = public {$IF COOPER}Integer{$ELSE}^Void{$ENDIF};
  {$SHOW CPW8}

  TGUID = public RemObjects.Elements.RTL.Guid;

  {$IF NOT COOPER}
  PChar = public ^Char;
  PAnsiChar = public ^AnsiChar;
  {$ENDIF}
  PInterfaceEntry = public Pointer;
  PInterfaceTable = public Pointer;
  HResult = public Int64;

  DWORD = public UInt32;
  BYTEBOOL = public Boolean;

end.