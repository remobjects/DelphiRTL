namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL;

type
  TClass = public RemObjects.Elements.RTL.Reflection.Type;
  ShortString = String;
  {$HIDE CPW8}
  Pointer = public {$IF COOPER}Integer{$ELSE}^Void{$ENDIF};
  {$SHOW CPW8}

  TGUID = public RemObjects.Elements.RTL.Guid;
  Guid_Extension = public extension class(Guid)
  public
    class property &Empty: Guid read begin result := self.EmptyGuid; end;
  end;

  {$IF NOT COOPER}
  PChar = public ^Char;
  PAnsiChar = public ^AnsiChar;
  PByte = public ^Byte;
  {$ENDIF}
  PInterfaceEntry = public Pointer;
  PInterfaceTable = public Pointer;
  HResult = public Int64;

  DWORD = public UInt32;
  BYTEBOOL = public Boolean;

end.