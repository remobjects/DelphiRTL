namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT MACCATALYST AND NOT (ISLAND AND DARWIN)}

interface

uses
  AppKit, RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class(TNativeControl)
  protected
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;
    method PlatformSetWidth(aValue: Integer); override;
    method PlatformSetHeight(aValue: Integer); override;
    method PlatformSetCaption(aValue: VCLString); override;
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

  TScreen = public partial class(TComponent)
  protected
    method PlatformGetScreenHeight: Integer; partial;
    method PlatformGetScreenWidth: Integer; partial;
  end;


implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  var lSize: {$IF __LP64__}UInt64{$ELSE}UInt32{$ENDIF};
  var lSecName := NSString('__island_res').UTF8String;
  var lData := NSString('__DATA').UTF8String;
  var lStart := rtl.getsectiondata(@rtl._mh_execute_header, lData, lSecName, @lSize);

  var lStream := new TMemoryStream();
  lStream.Write(lStart, lSize);
  lStream.Position := 100; // skip section&res header
  var lReader := new TReader(lStream, 100);
  lReader.ReadRootComponent(self);
end;

method TCustomForm.PlatformSetTop(aValue: Integer);
begin
  var lFrame := (fHandle as NSWindow).frame;
  lFrame.origin.y := Screen.Height - aValue;
  (fHandle as NSWindow).setFrame(lFrame) display(YES) animate(YES);
end;

method TCustomForm.PlatformSetLeft(aValue: Integer);
begin
  var lFrame := (fHandle as NSWindow).frame;
  lFrame.origin.x := aValue;
  (fHandle as NSWindow).setFrame(lFrame) display(YES) animate(YES);
end;

method TCustomForm.PlatformSetWidth(aValue: Integer);
begin
  var lFrame := (fHandle as NSWindow).frame;
  lFrame.size.width := aValue;
  (fHandle as NSWindow).setFrame(lFrame) display(YES) animate(YES);
end;

method TCustomForm.PlatformSetHeight(aValue: Integer);
begin
  var lFrame := (fHandle as NSWindow).frame;
  lFrame.size.height := aValue;
  (fHandle as NSWindow).setFrame(lFrame) display(YES) animate(YES);
end;

method TCustomForm.PlatformSetCaption(aValue: VCLString);
begin
  (fHandle as NSWindow).title := aValue;
end;

method TCustomForm.PlatformInitControl;
begin
end;

method TForm.CreateHandle;
begin
  fHandle := (NSWindow.alloc).initWithContentRect(NSMakeRect(1.0, 1.0, 300.0, 300.0))
    styleMask(AppKit.NSWindowStyleMask.NSTitledWindowMask or AppKit.NSWindowStyleMask.NSClosableWindowMask
    or AppKit.NSWindowStyleMask.Miniaturizable {or AppKit.NSWindowStyleMask.NSFullScreenWindowMask})
    backing(AppKit.NSBackingStoreType.NSBackingStoreBuffered) defer(NO);
  fView := (fHandle as NSWindow).contentView;
end;

method TForm.Show;
begin
  (fHandle as NSWindow).isvisible := true;
end;

method TScreen.PlatformGetScreenHeight: Integer;
begin
  result := Integer(NSScreen.mainScreen.frame.size.height);
end;

method TScreen.PlatformGetScreenWidth: Integer;
begin
  result := Integer(NSScreen.mainScreen.frame.size.width);
end;

{$ENDIF}

end.