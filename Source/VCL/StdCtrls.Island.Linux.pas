namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND LINUX}

{$GLOBALS ON}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TButton = public partial class(TNativeControl)
  protected
    method CreateHandle; partial; override;
    method PlatformSetCaption(aValue: String); partial; override;
    method PlatformSetOnClick(aValue: TNotifyEvent); override;
  end;

procedure PlatformShowMessage(aMessage: String);

implementation

procedure PlatformShowMessage(aMessage: String);
begin
  var lFlags := gtk.GtkDialogFlags.GTK_DIALOG_MODAL;
  var lParent: ^gtk.GtkWindow := if Application.MainForm ≠ nil then ^gtk.GtkWindow(Application.MainForm.Handle) else nil;
  var lMessage := gtk.gtk_message_dialog_new(lParent, lFlags, gtk.GtkMessageType.GTK_MESSAGE_INFO, gtk.GtkButtonsType.GTK_BUTTONS_OK, aMessage);

  //gtk.gtk_widget_show_all(lMessage);
  gtk.gtk_dialog_run(^gtk.GtkDialog(lMessage));
  gtk.gtk_widget_destroy(lMessage);
end;

method TButton.CreateHandle;
begin
  var lCaption := Caption.ToAnsiChars(true);
  fHandle := gtk.gtk_button_new_with_label(@lCaption[0]);
  gtk.gtk_widget_show(fHandle);
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  var lCaption := aValue.ToAnsiChars(true);
  gtk.gtk_button_set_label(^gtk.GtkButton(fHandle), @lCaption[0]);
end;

method TButton.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  gobject.g_signal_connect_data(fHandle, 'clicked', glib.GVoidFunc(^Void(@clicked)), glib.gpointer(GCHandle.Allocate(self).Handle), @gchandlefree, gobject.GConnectFlags(0));
end;

method clicked(app: ^gtk.GtkWidget; userdata: ^Void);
begin
  var lSelf := new GCHandle(NativeInt(userdata)).Target as TButton;
  if lSelf.OnClick ≠ nil then
  lSelf.OnClick(lSelf);
end;

method gchandlefree(data: glib.gpointer; closure: ^gobject.GClosure);
begin
  new GCHandle(NativeInt(data)).Dispose();
end;

{$ENDIF}

end.