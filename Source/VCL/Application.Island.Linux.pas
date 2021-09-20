﻿namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND (LINUX AND NOT ANDROID)}

interface

uses
  RemObjects.Elements.RTL.Delphi;

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
  var lCaller := TControlCtor(lCtor.Pointer);
  lCaller(FormRef, nil);
  //lCtor.Invoke(FormRef, [nil]);
  aFormRef := FormRef;
  if fMainForm = nil then begin
    fMainForm := FormRef;
  end;
end;

method TApplication.Initialize;
begin
  gtk.gtk_init(@ExternalCalls.nargs, @ExternalCalls.args);
end;

method TApplication.Run;
begin
  if fMainForm <> nil then
    fMainForm.Show;

   gtk.gtk_main();
end;

method TApplication.Terminate;
begin
  fFinished := true;
end;

{$ENDIF}

end.