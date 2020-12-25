namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT MACCATALYST AND NOT (ISLAND AND DARWIN)}

interface

uses
  AppKit, Foundation, RemObjects.Elements.RTL;

type
  TApplication = public partial class(TComponent)
  private
    fApp: NSApplication;
    fDelegate: TAppDelegate;
  public
    method CreateForm(InstanceClass: TComponentClass; var aFormRef); partial;
    method Initialize; partial;
    method Run; partial;
    method Terminate; partial;
  end;

  TAppDelegate = class(NSApplicationDelegate)
  private
  public
    method applicationDidFinishLaunching(notification: not nullable NSNotification);
    property App: TApplication;
  end;

implementation

method TApplication.CreateForm(InstanceClass: TComponentClass; var aFormRef);
begin
  {var lInstanceType := new &RemObjects.Elements.RTL.Reflection.Type withClass(InstanceClass);
  var lCtor: &RemObjects.Elements.RTL.Reflection.Method := nil;
  var lParent: &Class := nil;
  var lCurrentClass := InstanceClass;
  while lCtor = nil do begin
    var lMethods := lInstanceType.Methods;
    for each lMethod in lMethods do begin
      if lMethod.Name = 'init:' then begin
        lCtor := lMethod;
        break;
      end;
    end;
    lParent := class_getSuperclass(lCurrentClass);
    if lCurrentClass ≠ lParent then begin
      lCurrentClass := lParent;
      if lParent ≠ nil then
        lInstanceType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lParent)
      else
        break;
    end
    else
      break;
  end;
  if lCtor = nil then raise new Exception('No default constructor could be found!');

  var lNew := rtl.objc_msgSend(InstanceClass, NSSelectorFromString('alloc'));
  lNew := rtl.objc_msgSend(lNew, lCtor.Selector, [nil]);
  //lNew := lCtor.Invoke(lNew, [nil]);
  aFormRef := lNew;
  //lCtor.Invoke(FormRef, [nil]);} // todo Invoke method}
  //aFormRef := ComponentsHelper.CreateComponent(new RemObjects.Elements.RTL.Reflection.Type withClass(InstanceClass), nil);}

  var FormRef := ComponentsHelper.CreateComponent(new RemObjects.Elements.RTL.Reflection.Type withClass(InstanceClass), nil);
  aFormRef := FormRef;
  if fMainForm = nil then
    fMainForm := TForm(aFormRef);
end;

method TApplication.Initialize;
begin
  fApp := NSApplication.sharedApplication;
  fDelegate := new TAppDelegate();
  fDelegate.App := self;
  fApp.delegate := fDelegate;
  Screen := new TScreen();
end;

method TApplication.Run;
begin
  fApp.Run;
end;

method TApplication.Terminate;
begin
  fApp.Terminate(NSApp);
end;

method TAppDelegate.applicationDidFinishLaunching(notification: not nullable NSNotification);
begin
  if App.MainForm ≠ nil then
    (App.MainForm.Handle as NSWindow).makeKeyAndOrderFront(NSApp);
end;

{$ENDIF}

end.