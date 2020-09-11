namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL;

type
  TObject = public Object;

  IInterface = public interface
  end;

  Object__Delphi = public extension class(Object)
  public

    method Destroy;
    begin
      {$IF ECHOES OR ISLAND}
      IDisposable(self):Dispose;
      {$ENDIF}
      {$IF COOPER}
      AutoCloseable(self):close;
      {$ENDIF}
    end;

    method Free;
    begin
      Destroy;
    end;

    class method InitInstance(Instance: Pointer): TObject; empty;
    method CleanupInstance; empty;

    method ClassType: TClass;
    begin
      result := new RemObjects.Elements.RTL.Reflection.Type withPlatformType(typeOf(self));
    end;


    class method ClassName: ShortString;
    begin
      result := typeOf(self).Name;
    end;

    method InstanceClassName: String;
    begin
      result := ClassType.Name;
    end;

    class method ClassNameIs(const Name: String): Boolean; 
    begin
      result := RemObjects.Elements.RTL.String.EqualsIgnoringCase(ClassName, Name);
    end;

    class method ClassParent: TClass; 
    begin
      result := (new RemObjects.Elements.RTL.Reflection.Type withPlatformType(typeOf(self))).BaseType;
    end;

    class method ClassInfo: Pointer; empty;
    class method InstanceSize: LongInt; empty;

    class method InheritsFrom(AClass: TClass): Boolean; 
    begin
      {$IF COOPER}
      result := false;
      (*var SelfType := new RemObjects.Elements.RTL.Reflection.Type withPlatformType(typeOf(self));
      var altSelfType := typeOf(Self);
      result := (AClass = typeOf(self)) or altSelfType.IsSubClassOf(AClass);
      result := RemObjects.Elements.RTL.String.Equals(SelfType.Name, AClass.Name) or (SelfType.IsSubclassOf(AClass));*)
      {$ELSEIF TOFFEE}
      var SelfType := new RemObjects.Elements.RTL.Reflection.Type withPlatformType(typeOf(self));
      result := RemObjects.Elements.RTL.String.Equals(SelfType.Name, AClass.Name) or (SelfType.IsSubclassOf(AClass));
      {$ELSE}
      result := (AClass = typeOf(self)) or (typeOf(self).IsSubclassOf(AClass));
      {$ENDIF}
    end;

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

  TDelphiObject = public partial class(TObject)
  public
    procedure AfterConstruction; virtual; empty; // will be called by the compiler after a any "new", at thew call site.
    procedure BeforeDestruction; virtual; empty;

    constructor Create; virtual; empty;
    method Destroy; virtual; empty; // can't use "destructor" because this project isn't built using Delphi Compatibility. same diff though.

    method Free; inline;  // this allows writing SomeObject.Free without getting an NRE in Echoes
    begin
      self:InternalFree;
    end;

    method InternalFree;
    begin
      if assigned(self) and not fDestroyed then begin
        fDestroyed := true;
        BeforeDestruction;
        Destroy;
        {$IF ECHOES}
        GC.SuppressFinalize(self);
        {$ENDIF}
        {$IF ISLAND AND NOT WEBASSEMBLY}
        DefaultGC.SuppressFinalize(self);
        {$ENDIF}
      end;
    end;

  private
    fDestroyed: Boolean;

  end;

  {$IF ECHOES OR ISLAND}
  TDelphiObject = public partial class(IDisposable)
  protected

    finalizer;
    begin
      if not fDestroyed then begin
        fDestroyed := true;
        BeforeDestruction;
        Destroy;
      end;
    end;

    method Dispose; private;
    begin
      if not fDestroyed then begin
        fDestroyed := true;
        BeforeDestruction;
        Destroy;
        {$IF ECHOES}
        GC.SuppressFinalize(self);
        {$ELSEIF ISLAND AND NOT WEBASSEMBLY}
        DefaultGC.SuppressFinalize(self);
        {$ENDIF}
      end;
    end;

  end;
  {$ENDIF}

  {$IF COOPER}
  TDelphiObject = public partial class(AutoCloseable)
  private

    method close; //raises Exception;
    begin
      if not fDestroyed then begin
        fDestroyed := true;
        BeforeDestruction;
        Destroy;
      end;
    end;

  end;
  {$ENDIF}

end.