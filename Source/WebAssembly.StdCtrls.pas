namespace RemObjects.Elements.RTL.Delphi;

{$IF WEBASSEMBLY}

interface

{$GLOBALS ON}

type
  TControl = public partial class(TComponent)
  private
    method ProcessKeyboardStatus(aStatus: EcmaScriptObject; var aKey: Word): TShiftState;
    method InternalSetKeyboardEvent(aEvent: String; aValue: TKeyEvent);
  protected
    method PlatformSetWidth(aValue: Integer); partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;

    method PlatformFontSetColor(value: TColor); partial;
    method PlatformFontSetName(value: String); partial;
    method PlatformFontSetSize(value: Integer); partial;
    method PlatformFontSetStyles(value: TFontStyles); partial;

    method PlatformGetDefaultName: String; virtual; partial;
    method PlatformApplyDefaults; virtual; partial;
  end;

  TForm = public class(TControl)
  protected
    method CreateHandle; override;
  public
    method Show(aRootView: dynamic);
  end;

  TButton = public partial class(TControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); partial;
  end;

  TLabel = public partial class(TControl)
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

  TEdit = public partial class(TControl)
  protected
    method CreateHandle; override;
    method PlatformSetText(aValue: String);
    method PlatformGetText: String;
    method PlatformGetMaxLength: Integer;
    method PlatformSetMaxLength(aValue: Integer);
    method PlatformGetReadOnly: Boolean;
    method PlatformSetReadOnly(aValue: Boolean);
  end;

  TRadioCheckBox = public partial abstract class(TControl)
  protected
    fLabelHandle: dynamic;
    method internalCreateHandle(aType: String);
    method PlatformSetParent(aValue: TControl); override;
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;

    method PlatformGetChecked: Boolean;
    method PlatformSetChecked(value: Boolean);
    method PlatformSetCaption(value: String);
  end;

  TCheckBox = public class(TRadioCheckBox)
  protected
    method CreateHandle; override;
  end;

  TRadioButton = public class(TRadioCheckBox)
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
  fHandle.innerText := fCaption;
end;

method TLabel.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('LABEL');
  var lCaption := WebAssembly.CreateTextNode(Name);
  fHandle.appendChild(lCaption);
  fCaption := Name;
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

method TControl.PlatformSetWidth(aValue: Integer);
begin
  fHandle.style.width := aValue.ToString + 'px';
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  fHandle.style.height := aValue.ToString + 'px';
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  fHandle.style.top := aValue.ToString + 'px';
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
  fHandle.style.left := aValue.ToString + 'px';
end;

method TControl.PlatformSetParent(aValue: TControl);
begin
  aValue.Handle.appendChild(fHandle);
end;

method TControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));
  fHandle.addEventListener("click", lDelegate);
end;

method TControl.PlatformGetDefaultName: String;
begin
  var i := 1;
  var lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  while lObject <> nil do begin
    inc(i);
    lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  end;
  result := ClassName + i.ToString;
end;

method TControl.PlatformApplyDefaults;
begin
  fHandle.setAttribute('id', Name);
end;

method TControl.PlatformFontSetColor(value: TColor);
begin
  fHandle.style.color := value.ToString;
  fHandle.style.textDecorationColor := value.ToString;
end;

method TControl.PlatformFontSetName(value: String);
begin
  fHandle.style.fontFamily := value;
end;

method TControl.PlatformFontSetSize(value: Integer);
begin
  fHandle.style.fontSize := value.ToString + 'px';
end;

method TControl.PlatformFontSetStyles(value: TFontStyles);
begin
  if (TFontStyle.Italic in value) then fHandle.style.fontStyle := 'italic' else fHandle.style.fontStyle := 'normal';
  if (TFontStyle.Bold in value) then fHandle.style.fontWeight := 'bold' else fHandle.style.fontWeight := 'normal';
  if (TFontStyle.StrikeOut in value) or (TFontStyle.Underline in value) then begin
    if (TFontStyle.StrikeOut in value) then fHandle.style.textDecoration := 'line-throught';
    if (TFontStyle.Underline in value) then fHandle.style.textDecoration := 'underline';
  end
  else
    fHandle.style.textDecoration := 'none';
end;

method TCheckBox.CreateHandle;
begin
  internalCreateHandle("checkbox");
end;

method TRadioCheckBox.PlatformSetCaption(value: String);
begin
  fLabelHandle.innerText := value;
end;

method TRadioCheckBox.PlatformSetParent(aValue: TControl);
begin
  inherited;
  aValue.Handle.appendChild(fLabelHandle);
end;

method TRadioCheckBox.PlatformSetTop(aValue: Integer);
begin
  inherited;
  fLabelHandle.style.top := (aValue).ToString + 'px';
end;

method TRadioCheckBox.PlatformSetLeft(aValue: Integer);
begin
  inherited;
  fLabelHandle.style.left := (aValue + 22).ToString + 'px';
end;

method TRadioCheckBox.internalCreateHandle(aType: String);
begin
  fHandle := WebAssembly.CreateElement("INPUT");
  fHandle.setAttribute("type", aType);
  fHandle.style.position := "absolute";
  fLabelHandle := WebAssembly.CreateElement("LABEL");
  fLabelHandle.style.position := "absolute";
  fLabelHandle.innerText := Name;
end;

method TRadioCheckBox.PlatformSetChecked(value: Boolean);
begin
  fHandle.checked := value;
end;

method TRadioCheckBox.PlatformGetChecked: Boolean;
begin
  result := fHandle.checked;
end;

method TRadioButton.CreateHandle;
begin
  internalCreateHandle("radio");
end;

method TControl.PlatformSetOnKeyPress(aValue: TKeyPressEvent);
begin
  //'which' in case of using Mozilla FireFox browser
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lObject := new EcmaScriptObject(WebAssemblyCalls.GetArray(a.Handle, 0)); var lKey := chr(Max(Double(lObject['keyCode']), Double(lObject['which']))); aValue(self, var lKey); end);
  fHandle.addEventListener("keypress", lDelegate);
end;

method TControl.InternalSetKeyboardEvent(aEvent: String; aValue: TKeyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lObject := new EcmaScriptObject(WebAssemblyCalls.GetArray(a.Handle, 0)); var lKey: Word; var lShiftState := ProcessKeyboardStatus(lObject, var lKey); aValue(self, var lKey, lShiftState); end);
  fHandle.addEventListener(aEvent, lDelegate);
end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin
  InternalSetKeyboardEvent("keydown", aValue);
end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin
  InternalSetKeyboardEvent("keyup", aValue);
end;

method TControl.ProcessKeyboardStatus(aStatus: EcmaScriptObject; var aKey: Word): TShiftState;
begin
  result := [];
  if Boolean(aStatus['altKey']) then result := result + [TShiftState.ssAlt];
  if Boolean(aStatus['ctrlKey']) then result := result + [TShiftState.ssCtrl];
  if Boolean(aStatus['shiftKey']) then result := result + [TShiftState.ssShift];
  if Boolean(aStatus['metaKey']) then result := result + [TShiftState.ssCommand];
  aKey := Integer((Max(Double(aStatus['keyCode']), Double(aStatus['which']))));
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