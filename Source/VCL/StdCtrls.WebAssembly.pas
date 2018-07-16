namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

uses
  RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TForm = public partial class(TCustomForm)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  public
    method Show(aRootView: dynamic);
  end;

  TButton = public partial class(TNativeControl)
  protected
    method CreateHandle; partial; override;
    method PlatformSetCaption(aValue: String); partial; override;
  end;

  TLabel = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String);
  end;

  TPanel = public class(TControl)
  private
  protected
    method CreateHandle; override;
  public
  end;

  TGroupBox = public partial class(TControl)
  private
    fLabelHandle: dynamic;
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String);
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

    method PlatformGetChecked: Boolean; virtual; partial;
    method PlatformSetChecked(value: Boolean); virtual; partial;

    method PlatformSetCaption(value: String);
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

  TListControl = public partial abstract class(TControl)
  protected
    method internalCreateHandle(aListBoxMode: Boolean);
    method PlatformGetMultiSelect: Boolean;
    method PlatformSetMultiSelect(value: Boolean);
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(value: Integer);
    method PlatformGetItemIndex: Integer;
  end;

  TComboBox = public partial class(TListControl)
  protected
    method PlatformGetText: String;
    method PlatformSetOnSelect(aValue: TNotifyEvent);
    method CreateHandle; override;
  end;

  TListBox = public partial class(TListControl)
  protected
    method CreateHandle; override;
    method PlatformSelectAll;
    method PlatformGetSelected(aIndex: Integer): Boolean;
    method PlatformSetSelected(aIndex: Integer; value: Boolean);
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
  fHandle := WebAssembly.CreateElement('BUTTON');
  var lCaption := WebAssembly.CreateTextNode(Name);
  fHandle.style.position := "absolute";
  fHandle.appendChild(lCaption);
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  fHandle.innerText := aValue;
end;

method TLabel.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('LABEL');
  var lCaption := WebAssembly.CreateTextNode(Name);
  fHandle.appendChild(lCaption);
  Caption := Name;
  fHandle.style.position := "absolute";
end;

method TLabel.PlatformSetCaption(aValue: String);
begin
  fHandle.innerHTML := aValue;
end;

method TForm.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "relative";
  fHandle.style.margin := "0 auto";
end;

method TForm.Show(aRootView: dynamic);
begin
  var lRootView := aRootView;
  if lRootView = nil then begin
    // No parent html element provided to 'host' the main div
    var lWindow := WebAssembly.GetWindowObject;
    lRootView := WebAssembly.CreateElement('div');
    lRootView.style.margin := "0 auto";
    lWindow.document.body.appendChild(lRootView);
  end;

  lRootView.appendChild(fHandle);
end;

method TForm.PlatformSetCaption(aValue: String);
begin
end;

method TPanel.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "absolute";
end;

method TGroupBox.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement("FIELDSET");
  fLabelHandle := WebAssembly.CreateElement("LEGEND");
  var lText := WebAssembly.CreateTextNode(Name);
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
  fHandle := WebAssembly.CreateElement("INPUT");
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

method TButtonControl.internalCreateHandle(aType: String);
begin
  fHandle := WebAssembly.CreateElement("INPUT");
  fHandle.setAttribute("type", aType);
  fHandle.style.position := "absolute";
  fLabelHandle := WebAssembly.CreateElement("LABEL");
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
  var lOption := WebAssembly.CreateElement("OPTION");
  lOption.text := String(S);
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
  var lOption := WebAssembly.CreateElement("OPTION");
  lOption.text := String(S);
  ListControl.Handle.add(lOption);
end;

method TListControl.internalCreateHandle(aListBoxMode: Boolean);
begin
  fListBoxMode := aListBoxMode;
  fHandle := WebAssembly.CreateElement("SELECT");
  if fListBoxMode then
    fHandle.setAttribute("size", 6);
  fHandle.style.position := "absolute";
end;

method TListControl.PlatformGetMultiSelect: Boolean;
begin
  result := fHandle.getAttribute("multiple") = "multiple";
end;

method TListControl.PlatformSetMultiSelect(value: Boolean);
begin
  if value then
    fHandle.setAttribute("multiple", "multiple")
  else
    fHandle.setAttribute("multiple", "no");
end;

method TListControl.PlatformClearSelection;
begin
  for i: Integer := 0 to fHandle.options.length - 1 do
    fHandle.options[i].selected := false;
end;

method TListControl.PlatformDeleteSelected;
begin
  fHandle.remove(fHandle.options.selectedIndex);
end;

method TListControl.PlatformGetItemIndex: Integer;
begin
  result := fHandle.selectedIndex;
end;

method TListControl.PlatformSetItemIndex(value: Integer);
begin
  fHandle.selectedIndex := value;
end;

method TListBox.CreateHandle;
begin
  internalCreateHandle(true);
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

method TComboBox.PlatformSetOnSelect(aValue: TNotifyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));
  fHandle.addEventListener("change", lDelegate);
end;

method TComboBox.PlatformGetText: String;
begin
  result := fHandle.value;
end;

method TComboBox.CreateHandle;
begin
  internalCreateHandle(false);
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
  fHandle := WebAssembly.CreateElement("TEXTAREA");
  fHandle.style.position := "absolute";
end;

method TProgressBar.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('PROGRESS');
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