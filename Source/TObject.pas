namespace Elements.RTL.Delphi;

type
  TObject = Object;
  
  Object__Delphi = public extension class(Object)
  public
  
    method Destroy; empty; // no-op for compatibility
    method Free; empty;    // no-op for compatibility

    class method InitInstance(Instance: Pointer): TObject; empty;
    method CleanupInstance; empty;

    method ClassType: TClass; empty;
    class method ClassName: ShortString; empty;
    class method ClassNameIs(const Name: String): Boolean; empty;
    class method ClassParent: TClass; empty;
    class method ClassInfo: Pointer; empty;
    class method InstanceSize: LongInt; empty;
    class method InheritsFrom(AClass: TClass): Boolean; empty;
    
    class method MethodAddress(const Name: ShortString): Pointer;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.MethodAddress");
    end;
    
    class method MethodName(Address: Pointer): ShortString;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.MethodName");
    end;

    method FieldAddress(const Name: ShortString): Pointer;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.FieldAddress");
    end;

    method GetInterface(const IID: TGUID; out Obj): Boolean;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.GetInterface");
    end;
    
    class method GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.GetInterfaceEntry");
    end;

    class method GetInterfaceTable: PInterfaceTable;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.GetInterfaceTable");
    end;

    method SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
    begin
      raise new Sugar.SugarNotSupportedException("TObject.SafeCallException");
    end;

    procedure AfterConstruction; {virtual;} empty;
    procedure BeforeDestruction; {virtual;} empty;
    
    procedure Dispatch(var Message); {virtual;}
    begin
      raise new Sugar.SugarNotSupportedException("TObject.Dispatch");
    end;

    procedure DefaultHandler(var Message); {virtual;}
    begin
      raise new Sugar.SugarNotSupportedException("TObject.DefaultHandler");
    end;

    class method NewInstance: TObject; {virtual;}
    begin
      result := new self; {$HINT is this valid? can't imagine it would be}
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
  
end.
