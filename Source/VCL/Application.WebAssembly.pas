namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

type
  TApplication = public partial class(TComponent)
  public
    method CreateForm(InstanceClass: TComponentClass; var aFormRef); partial;
    method Initialize; partial;
    method Run; partial;
    method Terminate; partial;
  end;

implementation

method TApplication.CreateForm(InstanceClass: TComponentClass; var aFormRef);
begin
  var lCtor: MethodInfo;
  var FormRef: TForm := TForm(aFormRef);

  var lCtors := InstanceClass.Methods.Where(a -> ((MethodFlags.Constructor in a.Flags) and (a.Arguments.Count > 0)));
  if lCtors.Count > 1 then begin
    for each lTemp in lCtors do begin
      var lArguments := lTemp.Arguments.ToList;
      if lArguments[0].Type = typeOf(TComponent) then begin
        lCtor := lTemp;
        break;
      end;
    end;
  end
  else
    lCtor := lCtors.FirstOrDefault;

  if lCtor = nil then raise new Exception('No default constructor could be found!');
  var lNew := DefaultGC.New(InstanceClass.RTTI, InstanceClass.SizeOfType);
  FormRef := InternalCalls.Cast<TForm>(lNew);
  //lCtor.Invoke(FormRef, [nil]);
  var lCaller := TControlCtor(lCtor.Pointer);
  lCaller(FormRef, nil);
  aFormRef := FormRef;
  //var lPtr := IntPtr(InternalCalls.Cast(aFormRef));
  //SimpleGC.ForceAddRef(lPtr);
  if fMainForm = nil then begin
    fMainForm := FormRef;
    fMainForm.Show;
  end;
end;

method TApplication.Initialize;
begin

end;

method TApplication.Run;
begin
end;

method TApplication.Terminate;
begin
  fFinished := true;
end;

{$ENDIF}

end.