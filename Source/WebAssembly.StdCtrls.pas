namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL.Delphi;

type  
  TControl = public partial class(TComponent)
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
    method setText(value: String);
    method getText: String;
  protected
    method CreateHandle; override;
  public
    property Text: String read getText write setText;
  end;

  TRadioCheckBox = public abstract class(TControl)
  private
    method setCaption(value: String);
    fCaption: String;
  protected
    method internalCreateHandle(aType: String);
  public
    property Caption: String read fCaption write setCaption;
  end;

  TCheckBox = public class(TRadioCheckBox)
  protected
    method CreateHandle; override;  
  end;

  TRadioButton = public class(TRadioCheckBox)
  protected
    method CreateHandle; override;  
  end;

  TListControl = public abstract class(TControl)
  private
    fItems: TStrings;
    fListBoxMode: Boolean;
  protected
    method internalCreateHandle(aListBoxMode: Boolean);
    method PlatformSetMultiSelect(value: Boolean);
    constructor(aOwner: TComponent);
  public
    method AddItem(aValue: String);
  end;

  TComboBox = public class(TListControl)
  protected
    method CreateHandle; override;
  public
  end;

  TListBox = public class(TListControl)
  private
    method SetMultiSelect(value: Boolean);
    fMultiSelect: Boolean;
  protected
    method CreateHandle; override;
  public
    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect;

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

method TEdit.getText: String;
begin
  result := fHandle.value;
end;

method TEdit.setText(value: String);
begin

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
  fItems := TStringList.Create;
end;

method TListControl.AddItem(aValue: String);
begin
  fItems.Add(aValue);
  var lItem := WebAssembly.CreateElement("option");
  lItem.text := aValue;
  fHandle.add(lItem);
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
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lX := WebAssemblyCalls.GetIntValue(NativeInt(InternalCalls.Cast(a['keyCode']))); var lKey: Char; aValue(self, var lKey); end);
  fHandle.addEventListener("keypress", lDelegate);
end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lKey: Word; var lShiftState: TShiftState; aValue(self, var lKey, lShiftState); end);
  fHandle.addEventListener("keydown", lDelegate);
end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lKey: Word; var lShiftState: TShiftState; aValue(self, var lKey, lShiftState); end);
  fHandle.addEventListener("keyup", lDelegate);
end;


end.