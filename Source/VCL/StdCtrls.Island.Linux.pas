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

  TListControlItems = public partial class(TStringList)
  protected
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TListBox = public partial class(TMultiSelectListControl)
  private
    method GetFirstSelectedIter: ^gtk.GtkTreeIter;
  public
    const kListItem = 0;
    const kColumns = 1;
  protected
    var fStore: ^gtk.GtkListStore;
    method CreateHandle; override;
    method PlatformSelectAll;
    method PlatformGetSelected(aIndex: Integer): Boolean;
    method PlatformSetSelected(aIndex: Integer; value: Boolean);
    method PlatformGetSelCount: Integer;
    method PlatformGetMultiSelect: Boolean;
    method PlatformSetMultiSelect(value: Boolean);
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(value: Integer);
    method PlatformGetItemIndex: Integer;
  public
    property Store: ^gtk.GtkListStore read fStore;
  end;

  TComboBoxItems = public partial class(TStringList)
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TComboBox = public partial class(TListControl)
  protected
    method CreateHandle; override;
    method PlatformGetText: String;
    method PlatformSetText(aValue: String);
    method PlatformSetOnSelect(aValue: TNotifyEvent);
    method PlatformSetOnChange(aValue: TNotifyEvent);
    method PlatformSelectAll;
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(value: Integer);
    method PlatformGetItemIndex: Integer;
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
  case aValue of
    TCheckBoxState.cbUnChecked:
      gtk.gtk_toggle_button_set_active(^gtk.GtkToggleButton(fHandle), 0);

    TCheckBoxState.cbChecked:
    gtk.gtk_toggle_button_set_active(^gtk.GtkToggleButton(fHandle), 1);

    TCheckBoxState.cbGrayed:
      gtk.gtk_toggle_button_set_inconsistent(^gtk.GtkToggleButton(fHandle), 1);
  end;
end;

method TCheckBox.PlatformSetAllowGrayed(aValue: Boolean);
begin
  gtk.gtk_toggle_button_set_inconsistent(^gtk.GtkToggleButton(fHandle), Convert.ToInt32(aValue));
end;

method TRadioButton.CreateHandle;
begin
  var lCaption := Caption.ToAnsiChars(true);
  fHandle := gtk.gtk_radio_button_new_with_label(nil, @lCaption[0]);
end;

method TRadioButton.Click;
begin

end;

method TListControlItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lIter: ^gtk.GtkTreeIter;
  var lText := PlatformString(S).ToAnsiChars(true);

  gtk.gtk_list_store_append((ListControl as TListBox).Store, lIter);
  gtk.gtk_list_store_set((ListControl as TListBox).Store, lIter, (ListControl as TListBox).kListItem, @lText[0], -1);
end;

method TListControlItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  var lIter: ^gtk.GtkTreeIter;
  var lText := PlatformString(S).ToAnsiChars(true);
  gtk.gtk_list_store_insert((ListControl as TListBox).Store, lIter, aIndex);
  gtk.gtk_list_store_set((ListControl as TListBox).Store, lIter, (ListControl as TListBox).kListItem, @lText[0], -1);
end;

method TListControlItems.PlatformClear;
begin
  gtk.gtk_list_store_clear((ListControl as TListBox).Store);
end;

method TListControlItems.PlatformDelete(aIndex: Integer);
begin
  var lIter: ^gtk.GtkTreeIter;
  if gtk.gtk_tree_model_get_iter_first((ListControl as TListBox).Store, lIter) ≠ 0 then begin
    for i: Integer := 0 to aIndex - 1 do
      if gtk.gtk_tree_model_iter_next((ListControl as TListBox).Store, lIter) = 0 then
        exit;
  end;
  gtk.gtk_list_store_remove((ListControl as TListBox).Store, lIter);
end;

method TComboBoxItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lText := PlatformString(S).ToAnsiChars(true);
  gtk.gtk_combo_box_text_append_text(^gtk.GtkComboBoxText(ListControl.Handle), @lText[0]);
end;

method TComboBoxItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  var lText := PlatformString(S).ToAnsiChars(true);
  gtk.gtk_combo_box_text_insert_text(^gtk.GtkComboBoxText(ListControl.Handle), aIndex, @lText[0]);
end;

method TComboBoxItems.PlatformClear;
begin
  for i: Integer := 0 to Count - 1 do
    gtk.gtk_combo_box_text_remove(^gtk.GtkComboBoxText(ListControl.Handle), 0);
end;

method TComboBoxItems.PlatformDelete(aIndex: Integer);
begin
  gtk.gtk_combo_box_text_remove(^gtk.GtkComboBoxText(ListControl.Handle), aIndex);
end;

method TListBox.CreateHandle;
begin
  fHandle := gtk.gtk_tree_view_new();
  var lListItem := "List Item".ToAnsiChars(true);
  var lText := "text".ToAnsiChars(true);
  var lRenderer := gtk.gtk_cell_renderer_text_new();
  var lColumn := gtk.gtk_tree_view_column_new_with_attributes(@lListItem[0], lRenderer, [@lText[0], kListItem, nil]);
  gtk.gtk_tree_view_append_column(^gtk.GtkTreeView(fHandle), lColumn);
  fStore := gtk.gtk_list_store_new(kColumns, [gtk.GTK_TYPE_STRING]);
  gtk.gtk_tree_view_set_model(^gtk.GtkTreeView(fHandle), ^gtk.GtkTreeModel(fStore));
  gobject.g_object_unref(fStore);
end;

method TListBox.PlatformSelectAll;
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  gtk.gtk_tree_selection_select_all(lSelection);
end;

method TListBox.PlatformGetSelected(aIndex: Integer): Boolean;
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  var lText := aIndex.ToString.ToAnsiChars(true);
  var lPath := gtk.gtk_tree_path_new_from_string(@lText[0]);
  result := gtk.gtk_tree_selection_path_is_selected(lSelection, lPath) ≠ 0;
end;

method TListBox.PlatformSetSelected(aIndex: Integer; value: Boolean);
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  var lText := aIndex.ToString.ToAnsiChars(true);
  var lPath := gtk.gtk_tree_path_new_from_string(@lText[0]);
  if value then
    gtk.gtk_tree_selection_select_path(lSelection, lPath)
  else
    gtk.gtk_tree_selection_unselect_path(lSelection, lPath);
end;

method TListBox.PlatformGetSelCount: Integer;
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  gtk.gtk_tree_selection_count_selected_rows(lSelection);
end;

method TListBox.PlatformGetMultiSelect: Boolean;
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  var lMode := gtk.gtk_tree_selection_get_mode(lSelection);
  result := lMode ≠ gtk.GtkSelectionMode.GTK_SELECTION_EXTENDED;
end;

method TListBox.PlatformSetMultiSelect(value: Boolean);
begin
  var lSelectionMode := if value then gtk.GtkSelectionMode.GTK_SELECTION_EXTENDED else gtk.GtkSelectionMode.GTK_SELECTION_SINGLE;
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  gtk.gtk_tree_selection_set_mode(lSelection, lSelectionMode);
end;

method TListBox.PlatformClearSelection;
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  gtk.gtk_tree_selection_unselect_all(lSelection);
end;

method TListBox.GetFirstSelectedIter: ^gtk.GtkTreeIter;
begin
  var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
  var lList := gtk.gtk_tree_selection_get_selected_rows(lSelection, ^^gtk.GtkTreeModel(@fStore));
  var lIter: ^gtk.GtkTreeIter := nil;
  if lList ≠ nil then begin
    var lData := ^gtk.GtkTreePath(lList.data);
    if lData ≠ nil then begin
      var lChars := gtk.gtk_tree_path_to_string(lData);
      gtk.gtk_tree_model_get_iter_from_string(fStore, lIter, lChars);
    end;
  end;

  glib.g_list_foreach(lList, glib.GFunc((data, user)->gtk.gtk_tree_path_free(data)), nil);
  glib.g_list_free(lList);

  exit lIter;
end;

method TListBox.PlatformDeleteSelected;
begin
  var lIter := GetFirstSelectedIter;
  if lIter ≠ nil then
    gtk.gtk_tree_store_remove(^gtk.GtkTreeStore(fStore), lIter);
end;

method TListBox.PlatformSetItemIndex(value: Integer);
begin
  var lPath := gtk.gtk_tree_path_new_from_indices(value);
  var lColumn := gtk.gtk_tree_view_get_column(^gtk.GtkTreeView(fHandle), value);
  gtk.gtk_tree_view_row_activated(^gtk.GtkTreeView(fHandle), lPath, lColumn);
end;

method TListBox.PlatformGetItemIndex: Integer;
begin
  var lIter: ^gtk.GtkTreeIter;
  if not MultiSelect then begin
    var lSelection := gtk.gtk_tree_view_get_selection(^gtk.GtkTreeView(fHandle));
    if gtk.gtk_tree_selection_get_selected(lSelection, ^^gtk.GtkTreeModel(fStore), lIter) = 0 then
      exit -1;
  end
  else begin
    lIter := GetFirstSelectedIter;
    if lIter = nil then
      exit -1;
  end;
  var lChars := gtk.gtk_tree_model_get_string_from_iter(^gtk.GtkTreeModel(fStore), lIter);
  var lIndex := Convert.ToInt32(String.FromPAnsiChars(lChars));
  glib.g_free(lChars);
  exit lIndex;
end;

method TComboBox.CreateHandle;
begin
  if fStyle = TComboBoxStyle.csDropDown then
    fHandle := gtk.gtk_combo_box_text_new_with_entry()
  else
    fHandle := gtk.gtk_combo_box_text_new();
end;

method TComboBox.PlatformGetText: String;
begin

end;

method TComboBox.PlatformSetText(aValue: String);
begin

end;

method TComboBox.PlatformSetOnSelect(aValue: TNotifyEvent);
begin

end;

method TComboBox.PlatformSetOnChange(aValue: TNotifyEvent);
begin

end;

method TComboBox.PlatformSelectAll;
begin

end;

method TComboBox.PlatformClearSelection;
begin

end;

method TComboBox.PlatformDeleteSelected;
begin

end;

method TComboBox.PlatformSetItemIndex(value: Integer);
begin

end;

method TComboBox.PlatformGetItemIndex: Integer;
begin

end;

{$ENDIF}


end.