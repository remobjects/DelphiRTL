namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL.Delphi;

type  
  TControl = public partial class(TComponent)
  private
    method ProcessKeyboardStatus(aStatus: EcmaScriptObject; var aKey: Word): TShiftState;
    method InternalSetKeyboardEvent(aEvent: String; aValue: TKeyEvent);
  protected
    fHandle: dynamic;
    method PlatformSetWidth(aValue: Integer); partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); partial;
    method PlatformSetLeft(aValue: Integer); partial;
    method PlatformSetParent(aValue: TControl); partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;

    method GetDefaultName: String; virtual;
    method CreateHandle; abstract;
    method ApplyDefaults; virtual;

    constructor(aOwner: TComponent);
  public
    property Handle: dynamic read fHandle;
  end;

  TForm = public class(TControl)
  private
    fRootView: dynamic;
    method setRootView(value: dynamic);
  protected
    method CreateHandle; override;
  public
    property RootView: dynamic read fRootView write setRootView;
  end;

  TButton = public class(TControl)
  private
    fCaption: String;
    method setCaption(aValue: String);    
  protected
    method ClassName: String; override;
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String);
  public
    class method Create(AOwner: TComponent): TButton;
    property Caption: String read fCaption write setCaption;
  end;

  TLabel = public class(TControl)
  private
    fCaption: String;
    method setCaption(aValue: String);
 protected
    method ClassName: String; override;
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String);
  public
    class method Create(aOwner: TComponent): TLabel;
    property Caption: String read fCaption write setCaption;
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
  public
    property Text: String read PlatformGetText write PlatformSetText;
  end;

  TRadioCheckBox = public abstract class(TControl)
  private
    fCaption: String;    
    method setCaption(value: String);
  protected
    method internalCreateHandle(aType: String);
    method PlatformGetChecked: Boolean;
    method PlatformSetChecked(value: Boolean);
  public
    property Caption: String read fCaption write setCaption;
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
  public
    method &Add(s: DelphiString): Integer; override;
    method AddObject(const S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; const S: DelphiString); override;
    property ListControl: TListControl read write;
  end;
  
  TListControl = public abstract class(TControl)
  private
    fListBoxMode: Boolean;
  protected
    fItems: TStrings;
    method internalCreateHandle(aListBoxMode: Boolean);
    method PlatformSetMultiSelect(value: Boolean);
    constructor(aOwner: TComponent);
    method SetItems(aValue: TStrings); virtual;
  public
    method AddItem(Item: DelphiString; aObject: TObject);
    method Clear;
    method ClearSelection; 
    method DeleteSelected;
    property Items: TStrings read fItems write SetItems;
  end;
    
  TComboBox = public class(TListControl)
  protected
    method CreateHandle; override;
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

  TMemo = public class(TControl)
  private
  protected
    method CreateHandle; override;
  public
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

method TButton.setCaption(aValue: String);
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

method TForm.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "relative";
  fHandle.style.margin := "0 auto";
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

method TForm.setRootView(value: dynamic);
begin
  fRootView := value;
  if fRootView = nil then begin

  end
  else begin
     value.appendChild(fHandle);
  end;
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
  CreateHandle;
  ApplyDefaults;
end;

method TControl.ApplyDefaults;
begin
  fHandle.setAttribute('id', Name);
end;

method TCheckBox.CreateHandle;
begin
  internalCreateHandle("checkbox");
end;

method TRadioCheckBox.setCaption(value: String);
begin
  fCaption := value;
  fHandle.setAttribute("value", value);
end;

method TRadioCheckBox.internalCreateHandle(aType: String);
begin
  fHandle := WebAssembly.CreateElement("INPUT");
  fHandle.setAttribute("type", aType);
  fHandle.setAttribute("value", Name);
  fHandle.style.position := "absolute";  
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

method TLabel.PlatformSetCaption(aValue: String);
begin
  fHandle.innerHTML := aValue;
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  fHandle.innerText := fCaption;
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

method TRadioCheckBox.PlatformSetChecked(value: Boolean);
begin
  fHandle.checked := value;  
end;

method TRadioCheckBox.PlatformGetChecked: Boolean;
begin
  result := fHandle.checked;
end;

method TListControlItems.Add(s: DelphiString): Integer;
begin
  inherited;
  var lOption := WebAssembly.CreateElement("OPTION");
  lOption.text := s;
  ListControl.Handle.add(lOption);
end;

method TListControlItems.Clear;
begin
  inherited;
  for i: Integer := ListControl.Handle.length - 1 downto 0 do
    ListControl.Handle.remove(i);
end;

method TListControlItems.Delete(aIndex: Integer);
begin
  inherited;
  ListControl.Handle.remove(aIndex);
end;

method TListControlItems.Insert(aIndex: Integer; const S: DelphiString);
begin
  inherited;
  var lOption := WebAssembly.CreateElement("OPTION");
  lOption.text := s;
  ListControl.Handle.add(lOption, aIndex);
end;

method TListControlItems.AddObject(const S: DelphiString; aObject: TObject): Integer;
begin
  inherited;
  var lOption := WebAssembly.CreateElement("OPTION");
  lOption.text := s;
  ListControl.Handle.add(lOption);
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

method TListControl.Clear;
begin
  (fItems as TListControlItems).Clear;
end;

method TListControl.ClearSelection;
begin
  for i: Integer := 0 to fHandle.options.length - 1 do
    fHandle.options[i].selected := false;
end;

method TListControl.DeleteSelected;
begin
  fHandle.remove(fHandle.options.selectedIndex);
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

end.