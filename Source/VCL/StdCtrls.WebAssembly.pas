namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

uses
  RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TButton = public partial class(TNativeControl)
  protected
    method CreateHandle; partial; override;
    method PlatformSetCaption(aValue: String); partial; override;
  end;

  TLabel = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TGroupBox = public partial class(TNativeControl)
  private
    fLabelHandle: dynamic;
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
    fLabelHandle: dynamic;
    method internalCreateHandle(aType: String);
    method PlatformSetParent(aValue: TControl); override;
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;
    method PlatformSetWidth(aValue: Integer); override;

    method PlatformGetChecked: Boolean; virtual; partial;
    method PlatformSetChecked(value: Boolean); virtual; partial;

    method PlatformSetCaption(value: String); override;
  end;

  TCheckBox = public partial class(TButtonControl)
  protected
    method CreateHandle; override;
  end;

  TRadioButton = public class(TButtonControl)
  protected
    method CreateHandle; override;
  end;

  TListControlItems = public partial class(TStringList)
  private
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TListBox = public partial class(TMultiSelectListControl)
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

  TComboBoxItems = public partial class(TStringList)
  private
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

  TMemoStrings = partial class(TStringList)
  protected
    method PlatformGetText: String;
    method PlatformSetText(aValue: String);
  end;

  TMemo = public partial class(TControl)
  protected
    method CreateHandle; override;
  end;

  TProgressBar = public partial class(TControl)
  protected
    method CreateHandle; override;
    method PlatformSetPosition(value: Integer);
    method PlatformSetMax(value: Integer);
    method PlatformSetStyle(value: TProgressBarStyle);
  end;

  procedure PlatformShowMessage(aMessage: String);

implementation

procedure PlatformShowMessage(aMessage: String);
begin
  WebAssemblyCalls.ShowMessage(aMessage.FirstChar, aMessage.Length);
end;

method TButton.CreateHandle;
begin
  fHandle := Browser.CreateElement('BUTTON');
  var lCaption := Browser.CreateTextNode(Name);
  fHandle.style.position := "absolute";
  fHandle.appendChild(lCaption);
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  HandleNeeded;
  fHandle.innerText := aValue;
end;

method TLabel.CreateHandle;
begin
  fHandle := Browser.CreateElement('LABEL');
  var lCaption := Browser.CreateTextNode(Name);
  fHandle.appendChild(lCaption);
  Caption := Name;
  fHandle.style.position := "absolute";
end;

method TLabel.PlatformSetCaption(aValue: String);
begin
  fHandle.innerHTML := aValue;
end;

method TGroupBox.CreateHandle;
begin
  fHandle := Browser.CreateElement("FIELDSET");
  fLabelHandle := Browser.CreateElement("LEGEND");
  var lText := Browser.CreateTextNode(Name);
  fLabelHandle.appendChild(lText);
  fHandle.appendChild(fLabelHandle);
  fHandle.style.position := "absolute";
end;

method TGroupBox.PlatformSetCaption(aValue: String);
begin
  fLabelHandle.innerText := aValue;
end;

method TEdit.CreateHandle;
begin
  fHandle := Browser.CreateElement("INPUT");
  fHandle.setAttribute("type", "text");
  fHandle.style.position := "absolute";
end;

method TEdit.PlatformGetMaxLength: Integer;
begin
  result := fHandle.maxLength;
end;

method TEdit.PlatformSetMaxLength(aValue: Integer);
begin
  fHandle.maxLegth := aValue;
end;

method TEdit.PlatformGetReadOnly: Boolean;
begin
  result := fHandle.readOnly;
end;

method TEdit.PlatformSetReadOnly(aValue: Boolean);
begin
  fHandle.readOnly := aValue;
end;

method TEdit.PlatformGetText: String;
begin
  result := fHandle.value;
end;

method TEdit.PlatformSetText(aValue: String);
begin
  fHandle.value := aValue;
end;

method TCheckBox.CreateHandle;
begin
  internalCreateHandle("checkbox");
end;

method TButtonControl.PlatformSetCaption(value: String);
begin
  HandleNeeded;
  inherited;
  fLabelHandle.innerText := value;
end;

method TButtonControl.PlatformSetParent(aValue: TControl);
begin
  inherited;
  aValue.Handle.appendChild(fLabelHandle);
end;

method TButtonControl.PlatformSetTop(aValue: Integer);
begin
  inherited;
  fLabelHandle.style.top := (aValue).ToString + 'px';
end;

method TButtonControl.PlatformSetLeft(aValue: Integer);
begin
  inherited;
  fLabelHandle.style.left := (aValue + 22).ToString + 'px';
end;

method TButtonControl.PlatformSetWidth(aValue: Integer);
begin
end;

method TButtonControl.internalCreateHandle(aType: String);
begin
  fHandle := Browser.CreateElement("INPUT");
  fHandle.setAttribute("type", aType);
  fHandle.style.position := "absolute";
  fLabelHandle := Browser.CreateElement("LABEL");
  fLabelHandle.style.position := "absolute";
  fLabelHandle.innerText := Name;
end;

method TButtonControl.PlatformSetChecked(value: Boolean);
begin
  fHandle.checked := value;
end;

method TButtonControl.PlatformGetChecked: Boolean;
begin
  result := fHandle.checked;
end;

method TRadioButton.CreateHandle;
begin
  internalCreateHandle("radio");
end;

method TListControlItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  var lOption := Browser.CreateElement("OPTION");
  lOption.textContent := String(S);
  ListControl.Handle.add(lOption, aIndex);
end;

method TListControlItems.PlatformClear;
begin
  for i: Integer := ListControl.Handle.length - 1 downto 0 do
    ListControl.Handle.remove(i);
end;

method TListControlItems.PlatformDelete(aIndex: Integer);
begin
  ListControl.Handle.remove(aIndex);
end;

method TListControlItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lOption := Browser.CreateElement("OPTION");
  lOption.textContent := String(S);
  ListControl.Handle.add(Object(lOption));
end;

method TListBox.CreateHandle;
begin
  fHandle := Browser.CreateElement("SELECT");
  fHandle.setAttribute("size", 6);
  fHandle.style.position := "absolute";
end;

method TListBox.PlatformSelectAll;
begin
  for i: Integer := 0 to fHandle.options.length - 1 do
    fHandle.options[i].selected := true;
end;

method TListBox.PlatformGetSelected(aIndex: Integer): Boolean;
begin
  result := fHandle.options[aIndex].selected;
end;

method TListBox.PlatformSetSelected(aIndex: Integer; value: Boolean);
begin
  fHandle.options[aIndex].selected := value;
end;

method TListBox.PlatformGetSelCount: Integer;
begin
  result := 0; // TODO
end;

method TListBox.PlatformGetMultiSelect: Boolean;
begin
  result := fHandle.getAttribute("multiple") = "multiple";
end;

method TListBox.PlatformSetMultiSelect(value: Boolean);
begin
  if value then
    fHandle.setAttribute("multiple", "multiple")
  else
    fHandle.setAttribute("multiple", "no");
end;

method TListBox.PlatformClearSelection;
begin
  for i: Integer := 0 to fHandle.options.length - 1 do
    fHandle.options[i].selected := false;
end;

method TListBox.PlatformDeleteSelected;
begin
  fHandle.remove(fHandle.options.selectedIndex);
end;

method TListBox.PlatformGetItemIndex: Integer;
begin
  result := fHandle.selectedIndex;
end;

method TListBox.PlatformSetItemIndex(value: Integer);
begin
  fHandle.selectedIndex := value;
end;

method TComboBoxItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lOption := Browser.CreateElement("OPTION");
  lOption.textContent := String(S);
  ListControl.Handle.add(lOption);
end;

method TComboBoxItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  var lOption := Browser.CreateElement("OPTION");
  lOption.textContent := String(S);
  ListControl.Handle.add(lOption, aIndex);
end;

method TComboBoxItems.PlatformClear;
begin
  for i: Integer := ListControl.Handle.length - 1 downto 0 do
    ListControl.Handle.remove(i);
end;

method TComboBoxItems.PlatformDelete(aIndex: Integer);
begin
  ListControl.Handle.remove(aIndex);
end;

method TComboBox.CreateHandle;
begin
  fHandle := Browser.CreateElement("SELECT");
  fHandle.style.position := "absolute";
end;

method TComboBox.PlatformSetOnSelect(aValue: TNotifyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));
  fHandle.addEventListener("change", lDelegate);
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

method TComboBox.PlatformGetText: String;
begin
  result := fHandle.value;
end;

method TComboBox.PlatformSetText(aValue: String);
begin
  fHandle.value := aValue;
end;

method TComboBox.PlatformDeleteSelected;
begin
  fHandle.remove(fHandle.options.selectedIndex);
end;

method TComboBox.PlatformSetItemIndex(value: Integer);
begin
  fHandle.selectedIndex := value;
end;

method TComboBox.PlatformGetItemIndex: Integer;
begin
  result := fHandle.selectedIndex;
end;

method TMemoStrings.PlatformGetText: String;
begin
  result := fMemo.Handle.value;
end;

method TMemoStrings.PlatformSetText(aValue: String);
begin
  fMemo.Handle.value := aValue;
end;

method TMemo.CreateHandle;
begin
  fHandle := Browser.CreateElement("TEXTAREA");
  fHandle.style.position := "absolute";
end;

method TProgressBar.CreateHandle;
begin
  fHandle := Browser.CreateElement('PROGRESS');
  fHandle.style.position := "absolute";
end;

method TProgressBar.PlatformSetPosition(value: Integer);
begin
  fHandle.value := value;
end;

method TProgressBar.PlatformSetMax(value: Integer);
begin
  fHandle.max := value;
end;

method TProgressBar.PlatformSetStyle(value: TProgressBarStyle);
begin
  if value = TProgressBarStyle.Marquee then begin
    fHandle.removeAttribute('value');
    fHandle.removeAttribute('max');
  end
  else begin
    fHandle.max := fMax;
    fHandle.value := fPosition;
  end;
end;

{$ENDIF}

end.