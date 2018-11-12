namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows, System.Reflection;

type
  TApplication = public partial class(TComponent)
  private
    fApp: TInternalApp;
  public
    method CreateForm(InstanceClass: TComponentClass; var aFormRef); partial;
    method Initialize; partial;
    method Run; partial;
    method Terminate; partial;
  end;

  TInternalApp = class(Application)
  public
    method OnStartup(e: StartupEventArgs); override;
  end;

implementation

method TApplication.CreateForm(InstanceClass: TComponentClass; var aFormRef);
begin
  var FormRef: TForm := TForm(aFormRef);

  FormRef := TForm(Activator.CreateInstance(InstanceClass, [nil]));

  aFormRef := FormRef;
  if fMainForm = nil then begin
    fMainForm := FormRef;
    //fMainForm.Show; // not here, need to wait StartUp event
  end;
end;

method TApplication.Initialize;
begin
  if fApp = nil then
    fApp := new TInternalApp();
end;

method TApplication.Run;
begin
  fApp.Run;
end;

method TApplication.Terminate;
begin
  fApp.Shutdown;
end;

method TInternalApp.OnStartup(e: StartupEventArgs);
begin
  if Application.MainForm <> nil then
    Application.MainForm.Show;
end;

{$ENDIF}

end.