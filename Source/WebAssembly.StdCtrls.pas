namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL.Delphi;

type
  INotifyPropertyChanged = public interface
    event PropertyChanged: Action<TObject, String>;
  end;

  TControl = public partial class(TComponent)
  private
    method ProcessKeyboardStatus(aStatus: EcmaScriptObject; var aKey: Word): TShiftState;
    method InternalSetKeyboardEvent(aEvent: String; aValue: TKeyEvent);
    method setFont(value: TFont);
  protected
    fHandle: dynamic;
    fFont: TFont;
    method PlatformSetWidth(aValue: Integer); partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;

    method PlatformFontSetColor(value: TColor);
    method PlatformFontSetName(value: String);
    method PlatformFontSetSize(value: Integer);
    method PlatformFontSetStyles(value: TFontStyles);

    method GetDefaultName: String; virtual;
    method CreateHandle; abstract;
    method ApplyDefaults; virtual;
    method Changed(aObject: TObject; propName: String);

    constructor(aOwner: TComponent);
  public
    property Handle: dynamic read fHandle;
    property Font: TFont read fFont write SetFont;
  end;

  TColor = Integer;
  TFontStyle = public enum(Bold, Italic, Underline, StrikeOut);
  TFontStyles = set of TFontStyle;

  TFont = public class(TPersistent, INotifyPropertyChanged)
  private
    fColor: TColor;
    fName: String;
    fSize: Integer;
    fStyles: TFontStyles;
    method setColor(value: TColor);
    method setName(value: String);
    method setSize(value: Integer);
    method setStyles(value: TFontStyles);
    method NotifyChanged(propName: String);
  public
    event PropertyChanged: Action<Object, String>;
    property Color: TColor read fColor write SetColor;
    property Name: String read fName write SetName;
    property Size: Integer read fSize write SetSize;
    property Style: TFontStyles read fStyles write SetStyles;
  end;

  TForm = public class(TControl)
  protected
    method CreateHandle; override;
  public
    method Show(aRootView: dynamic);
  end;

  TButton = public class(TControl)
  private
    fCaption: String;
    method SetCaption(aValue: String);
  protected
    method ClassName: String; override;
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String);
  public
    class method Create(AOwner: TComponent): TButton;
    property Caption: String read fCaption write SetCaption;
  end;

  TLabel = public class(TControl)
  private
    fCaption: String;
    method SetCaption(aValue: String);
 protected
    method ClassName: String; override;
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String);
  public
    class method Create(aOwner: TComponent): TLabel;
    property Caption: String read fCaption write SetCaption;
  end;

  TPanel = public class(TControl)
  private
  protected
    method CreateHandle; override;
  public
  end;

  TEdit = public class(TControl)
  private
  protected
    method CreateHandle; override;
    method PlatformSetText(aValue: String);
    method PlatformGetText: String;
    method PlatformGetMaxLength: Integer;
    method PlatformSetMaxLength(aValue: Integer);
    method PlatformGetReadOnly: Boolean;
    method PlatformSetReadOnly(aValue: Boolean);
  public
    property MaxLength: Integer read PlatformGetMaxLength write PlatformSetMaxLength;
    property &ReadOnly: Boolean read PlatformGetReadOnly write PlatformSetReadOnly;
    property Text: String read PlatformGetText write PlatformSetText;
  end;

  TRadioCheckBox = public abstract class(TControl)
  private
    fCaption: String;
    method setCaption(value: String);
  protected
    fLabelHandle: dynamic;
    method internalCreateHandle(aType: String);
    method PlatformSetParent(aValue: TControl); override;
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;

    method PlatformGetChecked: Boolean;
    method PlatformSetChecked(value: Boolean);
    method PlatformSetCaption(value: String);

  public
    property Caption: String read fCaption write PlatformSetCaption;
    property Checked: Boolean read PlatformGetChecked write PlatformSetChecked;
  end;

  TCheckBox = public class(TRadioCheckBox)
  protected
    method CreateHandle; override;
  end;

  TRadioButton = public class(TRadioCheckBox)
  protected
    method CreateHandle; override;
  end;

  TListControlItems = public class(TStringList)
  protected
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  public
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; S: DelphiString); override;
    property ListControl: TListControl read write;
  end;

  TListControl = public abstract class(TControl)
  private
    method SetItemIndex(value: Integer);
    method GetItemIndex: Integer;
    fListBoxMode: Boolean;
  protected
    fItems: TStrings;
    method internalCreateHandle(aListBoxMode: Boolean);
    method PlatformSetMultiSelect(value: Boolean);
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    constructor(aOwner: TComponent);
    method SetItems(aValue: TStrings); virtual;
  public
    method AddItem(Item: DelphiString; aObject: TObject);
    method Clear;
    method ClearSelection;
    method DeleteSelected;
    property Items: TStrings read fItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  TComboBox = public class(TListControl)
  private
    fOnSelect: TNotifyEvent;
    method SetOnSelect(aValue: TNotifyEvent);
  protected
    method PlatformGetText: String;
    method CreateHandle; override;
  public
    property Text: String read PlatformGetText;
    property OnSelect: TNotifyEvent read fOnSelect write SetOnSelect;
  end;

  TListBox = public class(TListControl)
  private
    method SetMultiSelect(value: Boolean);
    method GetSelected(aIndex: Integer): Boolean;
    method SetSelected(aIndex: Integer; value: Boolean);
    fMultiSelect: Boolean;
  protected
    method CreateHandle; override;
  public
    procedure SelectAll;
    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect;
    property Selected[aIndex: Integer]: Boolean read GetSelected write SetSelected;
  end;

  TMemoStrings = class(TStringList)
  private
    fMemo: TMemo;
  protected
    method Get(aIndex: Integer): DelphiString; override;
    method GetTextStr: DelphiString; override;
    method SetTextStr(value: DelphiString); override;
  public
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; aString: DelphiString); override;
    property Memo: TMemo read fMemo write fMemo;
  end;

  TMemo = public class(TControl)
  private
    fLines: TStrings;
  protected
    method CreateHandle; override;
  public
    constructor(aOwner: TComponent);
    property Lines: TStrings read fLines write fLines;
  end;

  TProgressBarStyle = public enum (Normal, Marquee);

  TProgressBar = public class(TControl)
  private
    fStyle: TProgressBarStyle;
    fPosition: Integer;
    fMax: Integer;
    method SetMax(value: Integer);
    method SetPosition(value: Integer);
    method SetStyle(value: TProgressBarStyle);
  protected
    method CreateHandle; override;
    method PlatformSetPosition(value: Integer);
    method PlatformSetMax(value: Integer);
    method PlatformSetStyle(value: TProgressBarStyle);
  public
    property Max: Integer read fMax write SetMax;
    property Position: Integer read fPosition write SetPosition;
    property Style: TProgressBarStyle read fStyle write SetStyle;
  end;

  procedure ShowMessage(aMessage: String);

implementation

procedure ShowMessage(aMessage: String);
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

class method TButton.Create(AOwner: TComponent): TButton;
begin
  result := new TButton(AOwner);
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  fHandle.innerText := fCaption;
end;

method TButton.SetCaption(aValue: String);
begin
  fCaption := aValue;
  PlatformSetCaption(aValue);
end;

method TButton.ClassName: String;
begin
  result := 'Button';
end;

method TLabel.setCaption(aValue: String);
begin
  fCaption := aValue;
  PlatformSetCaption(aValue);
end;

method TLabel.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('LABEL');
  var lCaption := WebAssembly.CreateTextNode(Name);
  fHandle.appendChild(lCaption);
  fCaption := Name;
  fHandle.style.position := "absolute";
end;

method TLabel.ClassName: String;
begin
  result := 'Label';
end;

class method TLabel.Create(aOwner: TComponent): TLabel;
begin
  result := new TLabel(aOwner);
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

method TListBox.CreateHandle;
begin
  internalCreateHandle(true);
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

method TControl.GetDefaultName: String;
begin
  var i := 1;
  var lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  while lObject <> nil do begin
    inc(i);
    lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  end;
  result := ClassName + i.ToString;
end;

constructor TControl(aOwner: TComponent);
begin
  Name := GetDefaultName;
  fFont := new TFont();
  fFont.PropertyChanged += @Changed;
  CreateHandle;
  ApplyDefaults;
end;

method TControl.ApplyDefaults;
begin
  fHandle.setAttribute('id', Name);
end;

method TControl.SetFont(value: TFont);
begin
  fFont := value;
  PlatformFontSetColor(fFont.Color);
  PlatformFontSetSize(fFont.Size);
  PlatformFontSetName(fFont.Name);
  PlatformFontSetStyles(fFont.Style);
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

method TControl.Changed(aObject: TObject; propName: String);
begin
  if aObject is TFont then begin
    case propName of
      'color': PlatformFontSetColor(fFont.Color);
      'size': PlatformFontSetSize(fFont.Size);
      'name': PlatformFontSetName(fFont.Name);
      'styles': PlatformFontSetStyles(fFont.Style);
    end;
  end;
end;

method TCheckBox.CreateHandle;
begin
  internalCreateHandle("checkbox");
end;

method TRadioCheckBox.PlatformSetCaption(value: String);
begin
  fLabelHandle.innerText := value;
end;

method TRadioCheckBox.setCaption(value: String);
begin
  fCaption := value;
  PlatformSetCaption(value);
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

constructor TListControl(aOwner: TComponent);
begin
  fItems := new TListControlItems();
  TListControlItems(fItems).ListControl := self;
end;

method TListControl.internalCreateHandle(aListBoxMode: Boolean);
begin
  fListBoxMode := aListBoxMode;
  fHandle := WebAssembly.CreateElement("SELECT");
  if fListBoxMode then
    fHandle.setAttribute("size", 6);
  fHandle.style.position := "absolute";
end;

method TComboBox.PlatformGetText: String;
begin
  result := fHandle.value;
end;

method TComboBox.CreateHandle;
begin
  internalCreateHandle(false);
end;

method TListBox.SetMultiSelect(value: Boolean);
begin
  fMultiSelect := value;
  PlatformSetMultiSelect(value);
end;

method TListControl.PlatformSetMultiSelect(value: Boolean);
begin
  if value then
    fHandle.setAttribute("multiple", "multiple")
  else
    fHandle.setAttribute("multiple", "no");
end;

method TMemo.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement("TEXTAREA");
  fHandle.style.position := "absolute";
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

method TListControlItems.Clear;
begin
  inherited;
  PlatformClear;
end;

method TListControlItems.Delete(aIndex: Integer);
begin
  inherited;
  PlatformDelete(aIndex);
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

method TListControlItems.Insert(aIndex: Integer; S: DelphiString);
begin
  inherited;
  PlatformInsert(aIndex, S);
end;

method TListControlItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lOption := WebAssembly.CreateElement("OPTION");
  lOption.text := String(S);
  ListControl.Handle.add(lOption);
end;

method TListControlItems.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  inherited;
  PlatformAddItem(S, aObject);
end;

method TListBox.SelectAll;
begin
  for i: Integer := 0 to fHandle.options.length - 1 do
    fHandle.options[i].selected := true;
end;

method TListControl.AddItem(Item: DelphiString; aObject: TObject);
begin
  (fItems as TListControlItems).AddObject(Item, aObject);
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

method TListControl.Clear;
begin
  (fItems as TListControlItems).Clear;
end;

method TListControl.ClearSelection;
begin
  PlatformClearSelection;
end;

method TListControl.DeleteSelected;
begin
  PlatformDeleteSelected;
end;

method TListControl.SetItems(aValue: TStrings);
begin
  fItems.Clear;
  for i: Integer := 0 to aValue.Count - 1 do
    fItems.AddObject(aValue[i], aValue.Objects[i]);
end;

method TListBox.GetSelected(aIndex: Integer): Boolean;
begin
  result := fHandle.options[aIndex].selected;
end;

method TListBox.SetSelected(aIndex: Integer; value: Boolean);
begin
  fHandle.options[aIndex].selected := value;
end;

method TComboBox.SetOnSelect(aValue: TNotifyEvent);
begin
  fOnSelect := aValue;
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));
  fHandle.addEventListener("change", lDelegate);
end;

method TListControl.GetItemIndex: Integer;
begin
  result := fHandle.selectedIndex;
end;

method TListControl.SetItemIndex(value: Integer);
begin
  fHandle.selectedIndex := value;
end;

method TFont.NotifyChanged(propName: String);
begin
  if PropertyChanged <> nil then
    PropertyChanged(self, propName);
end;

method TFont.SetColor(value: TColor);
begin
  fColor := value;
  NotifyChanged('color');
end;

method TFont.SetName(value: String);
begin
  fName := value;
  NotifyChanged('name');
end;

method TFont.SetSize(value: Integer);
begin
  fSize := value;
  NotifyChanged('size');
end;

method TFont.SetStyles(value: TFontStyles);
begin
  fStyles := value;
  NotifyChanged('styles');
end;

constructor TMemo(aOwner: TComponent);
begin
  fLines := new TMemoStrings();
  TMemoStrings(fLines).Memo := self;
end;

method TMemoStrings.Get(aIndex: Integer): DelphiString;
begin
  var lText: String := fMemo.Handle.value;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  result := lTmp[aIndex];
end;

method TMemoStrings.GetTextStr: DelphiString;
begin
  result := String(fMemo.Handle.value);
end;

method TMemoStrings.SetTextStr(value: DelphiString);
begin
  inherited;
  fMemo.Handle.value := String(value);
end;

method TMemoStrings.Clear;
begin
  inherited;
  fMemo.Handle.value := '';
end;

method TMemoStrings.Delete(aIndex: Integer);
begin
  var lText: String := fMemo.Handle.value;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  lTmp.Delete(aIndex);
  fMemo.Handle.value := String(lTmp.Text);
end;

method TMemoStrings.Insert(aIndex: Integer; aString: DelphiString);
begin
  var lText: String := fMemo.Handle.value;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  lTmp.Insert(aIndex, aString);
  fMemo.Handle.value := String(lTmp.Text);
end;

method TMemoStrings.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  var lText: String := fMemo.Handle.value;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  lTmp.AddObject(S, aObject);
  fMemo.Handle.value := String(lTmp.Text);
end;

method TProgressBar.SetMax(value: Integer);
begin
  fMax := value;
  PlatformSetMax(value);
end;

method TProgressBar.SetPosition(value: Integer);
begin
  fPosition := value;
  PlatformSetPosition(value);
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

method TProgressBar.SetStyle(value: TProgressBarStyle);
begin
  fStyle := value;
  PlatformSetStyle(value);
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

end.