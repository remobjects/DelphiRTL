namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND LINUX}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TControl = public partial class(TComponent)
  protected
    fBox: ^gtk.GtkWidget;
    method HandleAllocated: Boolean; virtual; partial;
    method PlatformSetWidth(aValue: Integer); virtual; partial;
    method PlatformSetHeight(aValue: Integer); virtual; partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetVisible(aValue: Boolean); virtual; partial;
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
  gtk.gtk_widget_set_size_request(fHandle, aValue, fHeight);
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  gtk.gtk_widget_set_size_request(fHandle, fWidth, aValue);
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  if fParent <> nil then
    gtk.gtk_fixed_move(^gtk.GtkFixed(fParent.fBox), fHandle, fLeft, aValue);
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
  if fParent <> nil then
    gtk.gtk_fixed_move(^gtk.GtkFixed(fParent.fBox), fHandle, aValue, fTop);
end;

method TControl.PlatformSetParent(aValue: TControl);
begin
  gtk.gtk_fixed_put(^gtk.GtkFixed(aValue.fBox), fHandle, fLeft, fTop);
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

method TControl.PlatformSetVisible(aValue: Boolean);
begin
  if HandleAllocated then begin
    if aValue then
      gtk.gtk_widget_show(fHandle)
    else
      gtk.gtk_widget_hide(fHandle);
  end;
end;
{$ENDIF}

end.