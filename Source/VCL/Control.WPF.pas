namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TControl = public partial class(TComponent)
  protected
    method HandleAllocated: Boolean; virtual; partial;
    method PlatformSetWidth(aValue: Integer); virtual; partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;
    method PlatformSetCaption(aValue: String); virtual; partial;

    method PlatformFontChanged; virtual; partial;

    method PlatformGetDefaultName: String; virtual; partial;
    method PlatformApplyDefaults; virtual; partial;
  end;

implementation

method TControl.HandleAllocated: Boolean;
begin

end;

method TControl.PlatformSetWidth(aValue: Integer);
begin

end;

method TControl.PlatformSetHeight(aValue: Integer);
begin

end;

method TControl.PlatformSetTop(aValue: Integer);
begin

end;

method TControl.PlatformSetLeft(aValue: Integer);
begin

end;

method TControl.PlatformSetParent(aValue: TControl);
begin

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