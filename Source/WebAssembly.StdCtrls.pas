namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  
  TWebControl = public abstract class(TControl)
  protected
    fHandle: dynamic;
    method PlatformSetWidth(aValue: Integer); override;
    method PlatformSetHeight(aValue: Integer); override;
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;
    method PlatformSetParent(aValue: TControl); override;
    method PlatformSetOnClick(aValue: TNotifyEvent); override;
    method GetDefaultName: String; virtual;
    method CreateHandle; abstract;
    method ApplyDefaults; virtual;

    constructor(aOwner: TComponent);
  public
    property Handle: dynamic read fHandle;
  end;

  TForm = public class(TWebControl)
  private
    fRootView: dynamic;
    method setRootView(value: dynamic);
  protected
    method CreateHandle; override;
  public
    property RootView: dynamic read fRootView write setRootView;
  end;

  TButton = public class(TWebControl)
  private
    fCaption: String;
    method setCaption(aValue: String);    
  protected
    method ClassName: String; override;
    method CreateHandle; override;
  public
    class method Create(AOwner: TComponent): TButton;
    property Caption: String read fCaption write setCaption;
  end;

  TLabel = public class(TWebControl)
  private
    fCaption: String;
    method setCaption(aValue: String);
 protected
    method ClassName: String; override;
    method CreateHandle; override;
  public
    class method Create(aOwner: TComponent): TLabel;
    property Caption: String read fCaption write setCaption;
  end;

  TPanel = public class(TWebControl)
  private
  protected
    method CreateHandle; override;
  public
  end;

  TEdit = public class(TWebControl)
  private
    method setText(value: String);
    method getText: String;
  protected
    method CreateHandle; override;
  public
    property Text: String read getText write setText;
  end;

  TRadioCheckBox = public abstract class(TWebControl)
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

  TListControl = public abstract class(TWebControl)
  private
    fItems: TStrings;
    fListBoxMode: Boolean;
  protected
    method internalCreateHandle(aListBoxMode: Boolean);
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
  protected
    method CreateHandle; override;
  public

  end;


implementation

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
  fHandle.innerText := fCaption;
end;

method TButton.ClassName: String;
begin
  result := 'Button';
end;

method TLabel.setCaption(aValue: String);
begin
  fCaption := aValue;
  fHandle.innerHTML := aValue;
end;

method TLabel.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('LABEL');
  var lCaption := WebAssembly.CreateTextNode(Name);
  fHandle.appendChild(lCaption);
  fCaption := Name;
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

method TWebControl.PlatformSetWidth(aValue: Integer);
begin
  fHandle.style.width := aValue.ToString + 'px';
end;

method TWebControl.PlatformSetHeight(aValue: Integer);
begin
  fHandle.style.height := aValue.ToString + 'px';
end;

method TWebControl.PlatformSetTop(aValue: Integer);
begin
  fHandle.style.top := aValue.ToString + 'px';
end;

method TWebControl.PlatformSetLeft(aValue: Integer);
begin
  fHandle.style.left := aValue.ToString + 'px';
end;

method TWebControl.PlatformSetParent(aValue: TControl);
begin
  TWebControl(aValue).Handle.appendChild(fHandle);
end;

method TWebControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));
  fHandle.addEventListener("click", lDelegate);
end;

method TWebControl.GetDefaultName: String;
begin
  var i := 1;
  var lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  while lObject <> nil do begin
    inc(i);
    lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  end;
  result := ClassName + i.ToString;
end;

constructor TWebControl(aOwner: TComponent);
begin
  Name := GetDefaultName;
  CreateHandle;
  ApplyDefaults;
end;

method TWebControl.ApplyDefaults;
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



end.