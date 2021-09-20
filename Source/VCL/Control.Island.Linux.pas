namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND (LINUX AND NOT ANDROID)}

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
    property InnerBox: ^gtk.GtkWidget read fBox;
  end;

method gchandlefree(data: glib.gpointer; closure: ^gobject.GClosure); assembly;

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
  gtk.gtk_widget_show(fHandle);
end;

method TControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  var lEvent := PlatformString('clicked').ToAnsiChars(true);
  gobject.g_signal_connect_data(fHandle, @lEvent[0], glib.GVoidFunc(^Void(@internalClicked)), glib.gpointer(GCHandle.Allocate(self).Handle), @gchandlefree, gobject.GConnectFlags(0));
end;

method TControl.PlatformSetOnKeyPress(aValue: TKeyPressEvent);
begin
  gobject.g_signal_connect_data(fHandle, @PlatformString('key-press-event').ToAnsiChars(true)[0], glib.GVoidFunc(^Void(@internalKeyPressEvent)), glib.gpointer(GCHandle.Allocate(self).Handle), @gchandlefree, gobject.GConnectFlags(0));
end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin
  gobject.g_signal_connect_data(fHandle, @PlatformString('key-down-event').ToAnsiChars(true)[0], glib.GVoidFunc(^Void(@internalKeyDownEvent)), glib.gpointer(GCHandle.Allocate(self).Handle), @gchandlefree, gobject.GConnectFlags(0));
end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin
  gobject.g_signal_connect_data(fHandle, @PlatformString('key-release-event').ToAnsiChars(true)[0], glib.GVoidFunc(^Void(@internalKeyReleaseEvent)), glib.gpointer(GCHandle.Allocate(self).Handle), @gchandlefree, gobject.GConnectFlags(0));
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

method internalClicked(app: ^gtk.GtkWidget; userdata: ^Void);
begin
  var lSelf := new GCHandle(NativeInt(userdata)).Target as TControl;
  if lSelf.OnClick ≠ nil then
    lSelf.OnClick(lSelf);
end;

method getShiftState(aValue: cardinal): TShiftState;
begin
  result := [];
  if (aValue and gdk.GdkModifierType.GDK_SHIFT_MASK) ≠ 0 then
    result := result + [TShiftStateValues.ssShift];
  if (aValue and gdk.GdkModifierType.GDK_CONTROL_MASK) ≠ 0 then
    result := result + [TShiftStateValues.ssCtrl];
  if (aValue and gdk.GdkModifierType.GDK_MOD1_MASK) ≠ 0 then
    result := result + [TShiftStateValues.ssAlt];
  if (aValue and gdk.GdkModifierType.GDK_BUTTON1_MASK) ≠ 0 then
    result := result + [TShiftStateValues.ssLeft];
  if (aValue and gdk.GdkModifierType.GDK_BUTTON2_MASK) ≠ 0 then
    result := result + [TShiftStateValues.ssRight];
  if (aValue and gdk.GdkModifierType.GDK_BUTTON3_MASK) ≠ 0 then
    result := result + [TShiftStateValues.ssMiddle];
end;

method internalKeyPressEvent(app: ^gtk.GtkWidget; &event: ^gdk.GdkEvent; userdata: ^Void): glib.gboolean;
begin
  var lSelf := new GCHandle(NativeInt(userdata)).Target as TControl;
  var lKey: Char := Chr(^gdk.GdkEventKey(&event)^.keyval);
  if lSelf.OnKeyPress ≠ nil then
    lSelf.OnKeyPress(lSelf, var lKey);
  exit Convert.ToInt32(lKey = #0);
end;

method gchandlefree(data: glib.gpointer; closure: ^gobject.GClosure);
begin
  new GCHandle(NativeInt(data)).Dispose();
end;

method internalKeyDownEvent(app: ^gtk.GtkWidget; &event: ^gdk.GdkEvent; userdata: ^Void): glib.gboolean;
begin
  var lSelf := new GCHandle(NativeInt(userdata)).Target as TControl;
  var lShiftState := getShiftState(^gdk.GdkEventKey(&event)^.state);
  var lWord: Word := ^gdk.GdkEventKey(&event)^.keyval;
  if lSelf.OnKeyDown ≠ nil then
    lSelf.OnKeyDown(lSelf, var lWord, lShiftState);
  exit lWord;
end;

method internalKeyReleaseEvent(app: ^gtk.GtkWidget; &event: ^gdk.GdkEvent; userdata: ^Void): glib.gboolean;
begin
  var lSelf := new GCHandle(NativeInt(userdata)).Target as TControl;
  var lShiftState := getShiftState(^gdk.GdkEventKey(&event)^.state);
  var lWord: Word := ^gdk.GdkEventKey(&event)^.keyval;
  if lSelf.OnKeyUp ≠ nil then
    lSelf.OnKeyUp(lSelf, var lWord, lShiftState);
  exit lWord;
end;
{$ENDIF}

end.