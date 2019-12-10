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

  TLabel = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TGroupBox = public partial class(TNativeControl)
  protected
    // GTKFrame can have just a label and ONE child
    // so we add a GTkFixed and place all nedded inside.
    // fHandle = GtkFixed widget used as container
    fInternal: ^gtk.GtkFrame;
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TEdit = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetText(aValue: String); partial;
    method PlatformGetText: String; partial;
    method PlatformGetMaxLength: Integer; partial;
    method PlatformSetMaxLength(aValue: Integer); partial;
    method PlatformGetReadOnly: Boolean; partial;
    method PlatformSetReadOnly(aValue: Boolean); partial;
  end;

  TButtonControl = public partial class(TNativeControl)
  protected
    method PlatformGetChecked: Boolean; virtual; partial;
    method PlatformSetChecked(aValue: Boolean); virtual; partial;
  end;

  TCheckBox = public partial class(TButtonControl)
  protected
    method CreateHandle; override;

    method PlatformSetState(aValue: TCheckBoxState); partial;
    method PlatformSetAllowGrayed(aValue: Boolean); partial;
  end;

  TRadioButton = public class(TButtonControl)
  protected
    method CreateHandle; override;

    method Click; override;
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

method TLabel.CreateHandle;
begin
  var lCaption := Caption.ToAnsiChars(true);
  fHandle := gtk.gtk_label_new(@lCaption[0]);
  gtk.gtk_widget_show(fHandle);
end;

method TLabel.PlatformSetCaption(aValue: String);
begin
  var lCaption := aValue.ToAnsiChars(true);
  gtk.gtk_label_set_text(^gtk.GtkLabel(fHandle), @lCaption[0]);
end;

method TEdit.CreateHandle;
begin
  var lCaption := Text.ToAnsiChars(true);
  fHandle := gtk.gtk_entry_new();
  gtk.gtk_entry_set_text(^gtk.GtkEntry(fHandle), @lCaption[0]);
end;

method TEdit.PlatformSetText(aValue: String);
begin
  var lCaption := aValue.ToAnsiChars(true);
  gtk.gtk_entry_set_text(^gtk.GtkEntry(fHandle), @lCaption[0]);
end;

method TEdit.PlatformGetText: String;
begin
  var lText := gtk.gtk_entry_get_text(^gtk.GtkEntry(fHandle));
  result := String.FromPAnsiChars(lText);
end;

method TEdit.PlatformGetMaxLength: Integer;
begin
  result := gtk.gtk_entry_get_max_length(^gtk.GtkEntry(fHandle));
end;

method TEdit.PlatformSetMaxLength(aValue: Integer);
begin
  gtk.gtk_entry_set_max_length(^gtk.GtkEntry(fHandle), aValue);
end;

method TEdit.PlatformGetReadOnly: Boolean;
begin
  result := Convert.ToBoolean(gtk.gtk_editable_get_editable(^gtk.GtkEntry(fHandle)));
end;

method TEdit.PlatformSetReadOnly(aValue: Boolean);
begin
  gtk.gtk_editable_set_editable(^gtk.GtkEntry(fHandle), Convert.ToInt32(aValue));
end;

method TGroupBox.CreateHandle;
begin
  var lCaption := Caption.ToAnsiChars(true);
  fInternal := ^gtk.GtkFrame(gtk.gtk_frame_new(@lCaption[0]));
  fHandle := gtk.gtk_fixed_new();
  gtk.gtk_container_add(^gtk.GtkContainer(fInternal), fHandle);
end;

method TGroupBox.PlatformSetCaption(aValue: String);
begin
  var lCaption := aValue.ToAnsiChars(true);
  gtk.gtk_frame_set_label(fInternal, @lCaption[0]);
end;

method TButtonControl.PlatformGetChecked: Boolean;
begin
  result := Convert.ToBoolean(gtk.gtk_toggle_button_get_active(^gtk.GtkToggleButton(fHandle)));
end;

method TButtonControl.PlatformSetChecked(aValue: Boolean);
begin
  gtk.gtk_toggle_button_set_active(^gtk.GtkToggleButton(fHandle), Convert.ToInt32(aValue));
end;

method TCheckBox.CreateHandle;
begin
  var lCaption := Caption.ToAnsiChars(true);
  fHandle := gtk.gtk_check_button_new_with_label(@lCaption[0]);
end;

method TCheckBox.PlatformSetState(aValue: TCheckBoxState);
begin

end;

method TCheckBox.PlatformSetAllowGrayed(aValue: Boolean);
begin

end;

method TRadioButton.CreateHandle;
begin
  var lCaption := Caption.ToAnsiChars(true);
  fHandle := gtk.gtk_radio_button_new_with_label(nil, @lCaption[0]);
end;

method TRadioButton.Click;
begin

end;

{$ENDIF}
end.