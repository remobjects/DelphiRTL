namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT DARWIN}

interface

uses
  Foundation, AppKit, RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TButton = public partial class(TNativeControl)
  protected
    method CreateHandle; partial; override;
    method PlatformSetCaption(aValue: String); partial; override;
    method PlatformSetOnClick(aValue: TNotifyEvent); override;
    method ButtonClick(aEvent: NSEvent);
  end;

  TLabel = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TGroupBox = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TEdit = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetText(aValue: String);
    method PlatformGetText: String;
    method PlatformGetMaxLength: Integer;
    method PlatformSetMaxLength(aValue: Integer);
    method PlatformGetReadOnly: Boolean;
    method PlatformSetReadOnly(aValue: Boolean);
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
    fController: TTableViewController;
    fTable: NSTableView;
  protected
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
  end;

  TTableViewController = class(INSTableViewDataSource, INSTableViewDelegate)
  private
    fOwnerList: TListBox;
  public
    method numberOfRowsInTableView(tableView: not nullable NSTableView): NSInteger;
    //[Optional] method tableView(tableView: not nullable NSTableView) setObjectValue(object: nullable id) forTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger);
    method tableView(tableView: not nullable NSTableView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger): nullable id;
    constructor(aOwner: TListBox);
  end;

  TComboBoxItems = public partial class(TStringList)
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TComboBox = public partial class(TListControl)
  private
    fDelegate: TComboBoxDelegate;
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

  TComboBoxDelegate = class(INSComboBoxDelegate)
  private
    fOwnerCombo: TComboBox;
  public
    method comboBoxSelectionDidChange(notification: not nullable NSNotification);
    constructor(aOwner: TComboBox);
  end;

  procedure PlatformShowMessage(aMessage: String);

implementation

procedure PlatformShowMessage(aMessage: String);
begin
  var lAlert: NSAlert := new NSAlert();
  lAlert.messageText := aMessage;
  lAlert.addButtonWithTitle('Ok');
  lAlert.runModal;
end;

method TButton.CreateHandle;
begin
  fHandle := new NSButton();
  (fHandle as NSButton).bezelStyle := NSBezelStyle.NSRoundedBezelStyle;
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  (fHandle as NSButton).title := aValue;
end;

method TButton.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  if aValue ≠ nil then begin
    (fHandle as NSButton).target := self;
    (fHandle as NSButton).action := NSSelectorFromString('ButtonClick:');
  end
  else
    (fHandle as NSButton).action := nil;
end;

method TButton.ButtonClick(aEvent: NSEvent);
begin
  if assigned(OnClick) then
    OnClick(self);
end;

method TLabel.CreateHandle;
begin
  var lHandle := new NSTextField();
  lHandle.editable := false;
  fHandle := lHandle;
end;

method TLabel.PlatformSetCaption(aValue: String);
begin
  (fHandle as NSTextField).stringValue := aValue;
end;

method TGroupBox.CreateHandle;
begin
  fHandle := new NSBox();
end;

method TGroupBox.PlatformSetCaption(aValue: String);
begin
  (fHandle as NSBox).title := aValue;
end;

method TEdit.CreateHandle;
begin
  fHandle := new NSTextView();
end;

method TEdit.PlatformSetText(aValue: String);
begin
  (fHandle as NSTextView).string := aValue;
end;

method TEdit.PlatformGetText: String;
begin
  result := (fHandle as NSTextView).string;
end;

method TEdit.PlatformGetMaxLength: Integer;
begin
  // NSTextField does not have a maxLength or similar
  result := high(Integer);
end;

method TEdit.PlatformSetMaxLength(aValue: Integer);
begin
  // NSTextField does not have a maxLength or similar
end;

method TEdit.PlatformGetReadOnly: Boolean;
begin
  result := (fHandle as NSTextView).editable;
end;

method TEdit.PlatformSetReadOnly(aValue: Boolean);
begin
  (fHandle as NSTextView).editable := aValue;
end;

method TButtonControl.PlatformGetChecked: Boolean;
begin
  result := (fHandle as NSButton).state = NSControlStateValueOn;
end;

method TButtonControl.PlatformSetChecked(aValue: Boolean);
begin
  var lValue := if aValue then NSControlStateValueOn else NSControlStateValueOff;
  (fHandle as NSButton).state := lValue;
end;

method TCheckBox.CreateHandle;
begin
  fHandle := new NSButton();
  (fHandle as NSButton).setButtonType(NSButtonType.NSSwitchButton);
end;

method TCheckBox.PlatformSetState(aValue: TCheckBoxState);
begin
  case aValue of
    TCheckBoxState.cbChecked:
      (fHandle as NSButton).state := NSControlStateValueOn;

    TCheckBoxState.cbUnChecked:
      (fHandle as NSButton).state := NSControlStateValueOff;

    TCheckBoxState.cbGrayed:
      (fHandle as NSButton).state := NSControlStateValueMixed;
  end;
end;

method TCheckBox.PlatformSetAllowGrayed(aValue: Boolean);
begin
  (fHandle as NSButton).allowsMixedState := aValue;
end;

method TRadioButton.CreateHandle;
begin
  fHandle := new NSButton();
  (fHandle as NSButton).setButtonType(NSButtonType.NSRadioButton);
end;

method TRadioButton.Click;
begin

end;

method TListControlItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  (ListControl.UnderlyingHandle as NSTableView).reloadData;
end;

method TListControlItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  (ListControl.UnderlyingHandle as NSTableView).reloadData;
end;

method TListControlItems.PlatformClear;
begin
  (ListControl.UnderlyingHandle as NSTableView).reloadData;
end;

method TListControlItems.PlatformDelete(aIndex: Integer);
begin
  (ListControl.UnderlyingHandle as NSTableView).reloadData;
end;

method TListBox.CreateHandle;
begin
  fTable := new NSTableView();
  UnderlyingHandle := fTable;
  fHandle := new NSScrollView;
  var lColumn := new NSTableColumn withIdentifier(Name + 'C1');
  fTable.addTableColumn(lColumn);
  fController := new TTableViewController(self);
  fTable.delegate := fController;
  fTable.dataSource := fController;
  fTable.setHeaderView(nil);

  (fHandle as NSScrollView).documentView := fTable;
end;

method TListBox.PlatformSelectAll;
begin
  fTable.selectAll(fTable);
end;

method TListBox.PlatformGetSelected(aIndex: Integer): Boolean;
begin
  result := fTable.isRowSelected(aIndex);
end;

method TListBox.PlatformSetSelected(aIndex: Integer; value: Boolean);
begin
  if value then begin
    var lIndex := NSIndexSet.indexSetWithIndex(aIndex);
    fTable.selectRowIndexes(lIndex) byExtendingSelection(rtl.YES);
  end
  else
    fTable.deselectRow(aIndex);
end;

method TListBox.PlatformGetSelCount: Integer;
begin
  result := fTable.numberOfSelectedRows;
end;

method TListBox.PlatformGetMultiSelect: Boolean;
begin
  result := fTable.allowsMultipleSelection;
end;

method TListBox.PlatformSetMultiSelect(value: Boolean);
begin
  fTable.allowsMultipleSelection := value;
end;

method TListBox.PlatformClearSelection;
begin
  fTable.deselectAll(fTable);
end;

method TListBox.PlatformDeleteSelected;
begin
  if ItemIndex ≥ 0 then begin
    Items.Delete(ItemIndex);
    fTable.reloadData;
  end;
end;

method TListBox.PlatformSetItemIndex(value: Integer);
begin
  var lIndex := NSIndexSet.indexSetWithIndex(value);
  fTable.selectRowIndexes(lIndex) byExtendingSelection(rtl.NO);
end;

method TListBox.PlatformGetItemIndex: Integer;
begin
  result := fTable.selectedRow;
end;

constructor TTableViewController(aOwner: TListBox);
begin
  fOwnerList := aOwner;
end;

method TTableViewController.numberOfRowsInTableView(tableView: not nullable NSTableView): NSInteger;
begin
  if fOwnerList.Items = nil then
    exit 0;

  result := fOwnerList.Items.Count;
end;

method TTableViewController.tableView(tableView: not nullable NSTableView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger): nullable id;
begin
  // Just one column
  result := NSString(fOwnerList.Items[row]);
end;

method TComboBoxItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  (ListControl.Handle as NSComboBox).addItemWithObjectValue(NSString(S));
end;

method TComboBoxItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  (ListControl.Handle as NSComboBox).insertItemWithObjectValue(NSString(S)) atIndex(aIndex);
end;

method TComboBoxItems.PlatformClear;
begin
  (ListControl.Handle as NSComboBox).removeAllItems;
end;

method TComboBoxItems.PlatformDelete(aIndex: Integer);
begin
  (ListControl.Handle as NSComboBox).removeItemAtIndex(aIndex);
end;

method TComboBox.CreateHandle;
begin
  fHandle := new NSComboBox();
  fDelegate := new TComboBoxDelegate(self);
  (fHandle as NSComboBox).delegate := fDelegate;
end;

method TComboBox.PlatformGetText: String;
begin
  result := (fHandle as NSComboBox).stringValue;
end;

method TComboBox.PlatformSetText(aValue: String);
begin
  (fHandle as NSComboBox).stringValue := aValue;
end;

method TComboBox.PlatformSetOnSelect(aValue: TNotifyEvent);
begin
  // Done in delegate
end;

method TComboBox.PlatformSetOnChange(aValue: TNotifyEvent);
begin
  // Not supported on mac
end;

method TComboBox.PlatformSelectAll;
begin
  // Nothing to do...
end;

method TComboBox.PlatformClearSelection;
begin
  (fHandle as NSComboBox).selectItemAtIndex(-1);
end;

method TComboBox.PlatformDeleteSelected;
begin
  var lIndex := (fHandle as NSComboBox).indexOfSelectedItem;
  if lIndex ≥ 0 then
    (fHandle as NSComboBox).removeItemAtIndex(lIndex);
end;

method TComboBox.PlatformSetItemIndex(value: Integer);
begin
  (fHandle as NSComboBox).selectItemAtIndex(value);
end;

method TComboBox.PlatformGetItemIndex: Integer;
begin
  result := (fHandle as NSComboBox).indexOfSelectedItem;
end;

method TComboBoxDelegate.comboBoxSelectionDidChange(notification: not nullable NSNotification);
begin
  if fOwnerCombo.OnChange ≠ nil then
    fOwnerCombo.OnChange(fOwnerCombo);
end;

constructor TComboBoxDelegate(aOwner: TComboBox);
begin
  fOwnerCombo := aOwner;
end;
{$ENDIF}

end.