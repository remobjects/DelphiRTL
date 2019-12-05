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
  var lFlags := gtk.GtkDialogFlags.GTK_DIALOG_DESTROY_WITH_PARENT;
  var lMessage := gtk.gtk_message_dialog_new(nil, lFlags, gtk.GtkMessageType.GTK_MESSAGE_INFO, gtk.GtkButtonsType.GTK_BUTTONS_OK, aMessage);

  gtk.gtk_widget_show_all(lMessage);
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
  //gobject.g_signal_connect_data(
  writeLn("Pointer 1");
  writeLn(NativeInt(fHandle));
  gobject.g_signal_connect_data(fHandle, "clicked", glib.GVoidFunc(()->ClickCallback(fHandle)), nil, nil, 0)
end;

method ClickCallback(data: ^Void);
begin
  writeLn("Callback 1");
  writeLn("Pointer 2");
  writeLn(NativeInt(data));

  var lButton := InternalCalls.Cast<TButton>(data);
  writeLn(typeOf(lButton).Name);
  writeLn("Callback 2");
  writeLn((lButton as TButton).Name);
  writeLn("Callback 3");
  (lButton as TButton).OnClick(lButton);
end;

{$ENDIF}

end.