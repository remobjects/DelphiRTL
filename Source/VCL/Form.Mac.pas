namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS}

interface

uses
  AppKit, RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class(TNativeControl)
  protected
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;
    method PlatformSetCaption(aValue: String); override;
    method PlatformInitControl; override;

  public
    constructor(aOwner: TComponent);
  end;

  TForm = public partial class(TCustomForm)
  protected
    method CreateHandle; override;
  public
    method Show; override;
  end;

implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  {var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  if lName.ToUpper <> 'TFORM' then begin
    // the resource is on the .exe, not here!
    var lAssembly := System.Reflection.Assembly.GetEntryAssembly();
    var lResStream := lAssembly.GetManifestResourceStream(lName);
    var lBuffer := new Byte[lResStream.Length];
    lResStream.Read(lBuffer, 0, lResStream.Length);
    var lStream := new TMemoryStream();
    lStream.Write(lBuffer, lResStream.Length);
    lStream.Position := 74; // Discard header
    var lReader := new TReader(lStream, 100);
    lReader.ReadRootComponent(self);
  end;}
end;

method TCustomForm.PlatformSetTop(aValue: Integer);
begin

end;

method TCustomForm.PlatformSetLeft(aValue: Integer);
begin

end;

method TCustomForm.PlatformSetCaption(aValue: String);
begin

end;

method TCustomForm.PlatformInitControl;
begin
end;

method TForm.CreateHandle;
begin
  fHandle := (NSWindow.alloc).initWithContentRect(NSMakeRect(1.0, 1.0, 300.0, 300.0)) styleMask(AppKit.NSWindowStyleMask.NSTitledWindowMask) backing(AppKit.NSBackingStoreType.NSBackingStoreBuffered) defer(NO);
end;

method TForm.Show;
begin
  //(fHandle as NSWindow).visible := true;
end;

{$ENDIF}

end.