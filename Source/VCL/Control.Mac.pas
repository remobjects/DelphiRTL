namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS}

interface

uses
  Foundation, AppKit, RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TControl = public partial class(TComponent)
  protected
    fView: NSView;
    method HandleAllocated: Boolean; virtual; partial;
    method PlatformSetWidth(aValue: Integer); virtual; partial;
    method PlatformSetHeight(aValue: Integer); virtual; partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); virtual; partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;
    method PlatformSetCaption(aValue: String); virtual; partial;
    method PlatformSetTabOrder(aValue: Integer); virtual; partial;

    method PlatformFontChanged; virtual; partial;

    method PlatformGetDefaultName: String; virtual; partial;
    method PlatformApplyDefaults; virtual; partial;
  public
  end;

implementation

method TControl.HandleAllocated: Boolean;
begin
  result := fHandle ≠ nil;
end;

method TControl.PlatformSetWidth(aValue: Integer);
begin
  var lSize := (fHandle as NSView).frame.size;
  lSize.width := aValue;
  (fHandle as NSView).setFrameSize(lSize);
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  var lSize := (fHandle as NSView).frame.size;
  lSize.height := aValue;
  (fHandle as NSView).setFrameSize(lSize);
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  var lOrigin := (fHandle as NSView).frame.origin;
  lOrigin.y := aValue;
  (fHandle as NSView).setFrameOrigin(lOrigin);
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
  var lOrigin := (fHandle as NSView).frame.origin;
  lOrigin.x := aValue;
  (fHandle as NSView).setFrameOrigin(lOrigin);
end;

method TControl.PlatformSetParent(aValue: TControl);
begin
  aValue.fView.addSubview(fHandle as NSView);
end;

method TControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin

end;

method TControl.PlatformSetOnKeyPress(aValue: TKeyPressEvent);
begin

end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin

end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin

end;

method TControl.PlatformSetCaption(aValue: String);
begin
end;

method TControl.PlatformSetTabOrder(aValue: Integer);
begin

end;

method TControl.PlatformFontChanged;
begin

end;

method TControl.PlatformGetDefaultName: String;
begin

end;

method TControl.PlatformApplyDefaults;
begin

end;
{$ENDIF}

end.