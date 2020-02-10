namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL;

type
  TObject = public Object;

  IInterface = public interface
  end;

  Object__Delphi = public extension class(Object)
  public

    method Destroy; virtual; empty; // no-op for compatibility
    method Free; empty;

    class method InitInstance(Instance: Pointer): TObject; empty;
    method CleanupInstance; empty;

    method ClassType: TClass; empty;
    class method ClassName: ShortString;
    begin
      {$IF ISLAND}
      result := typeOf(self).Name;
      {$ELSE}
      result := '';
      {$ENDIF}
    end;

    method InstanceClassName: String;
    begin
      {$IF ISLAND}
        result := typeOf(self).Name;
      {$ELSE}
        result := '';
      {$ENDIF}
    end;

    class method ClassNameIs(const Name: String): Boolean; empty;
    class method ClassParent: TClass; empty;
    class method ClassInfo: Pointer; empty;
    class method InstanceSize: LongInt; empty;
    class method InheritsFrom(AClass: TClass): Boolean; empty;

    class method MethodAddress(const Name: ShortString): Pointer;
    begin
      raise new NotSupportedException("TObject.MethodAddress");
    end;

    class method MethodName(Address: Pointer): ShortString;
    begin
      raise new NotSupportedException("TObject.MethodName");
    end;

    method FieldAddress(const Name: ShortString): Pointer;
    begin
      raise new NotSupportedException("TObject.FieldAddress");
    end;

    method GetInterface(const IID: TGUID; out Obj): Boolean;
    begin
      raise new NotSupportedException("TObject.GetInterface");
    end;

    class method GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
    begin
      raise new NotSupportedException("TObject.GetInterfaceEntry");
    end;

    class method GetInterfaceTable: PInterfaceTable;
    begin
      raise new NotSupportedException("TObject.GetInterfaceTable");
    end;

    method SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
    begin
      raise new NotSupportedException("TObject.SafeCallException");
    end;

    procedure AfterConstruction; {virtual;} empty;
    procedure BeforeDestruction; {virtual;} empty;

    procedure Dispatch(var Message); {virtual;}
    begin
      raise new NotSupportedException("TObject.Dispatch");
    end;

    procedure DefaultHandler(var Message); {virtual;}
    begin
      raise new NotSupportedException("TObject.DefaultHandler");
    end;

    class method NewInstance: TObject; {virtual;}
    begin
      result := new self;
    end;

    procedure FreeInstance; {virtual;}
    begin
      {$IF ECHOES}
      IDisposable(self).Dispose();
      {$ELSEIF TOFFEE}
      {$ELSEIF COOPER}
      {$ELSEIF ISLAND}
      {$ENDIF}
    end;
  end;

{$IF ISLAND AND WINDOWS}
extension method RemObjects.Elements.System.String.ToLPCWSTR: rtl.LPCWSTR;
begin
  if String.IsNullOrEmpty(self) then exit nil;
  var arr := ToCharArray(true);
  exit rtl.LPCWSTR(@arr[0]);
end;
{$ENDIF}

end.