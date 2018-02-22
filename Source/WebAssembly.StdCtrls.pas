namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  //TNotifyEvent = public block(Sender: dynamic);
  
  TComponent = public class
  private
    fName: String;
    fOwner: TComponent;
    method setName(aValue: String);
    method setOwner(aValue: TComponent);
  protected
    method GetDefaultName: String;
    method ClassName: String; virtual;
    method Loaded; virtual;
    constructor(aOwner: TComponent);
  public
    property Name: String read fName write setName;
    property Owner: TComponent read fOwner write setOwner;
  end;

  // TODO: move to a non WebAssembly unit
  TControl = public class(TComponent)
  private
    fParent: TWebControl;
    fText: String;
    fOnClick: TNotifyEvent;
    fWidth: Integer;
    fHeight: Integer;
    fTop: Integer;
    fLeft: Integer;
    method SetParent(aValue: TWebControl);
    method SetOnClick(aValue: TNotifyEvent);
    method SetWidth(aValue: Integer);
    method SetHeight(aValue: Integer);
    method SetTop(aValue: Integer);
    method setLeft(aValue: Integer);
  protected
    fHandle: dynamic;
    method SetText(aValue: String); virtual;
    property Text: String read fText write SetText;
    constructor(aOwner: TComponent);
    method ClassName: String; override;
    method Loaded; override;
  public
    property Handle: dynamic read fHandle;
    property Parent: TWebControl read fParent write SetParent;
    property Height: Integer read fHeight write SetHeight;
    property Width: Integer read fWidth write SetWidth;
    property Left: Integer read fLeft write setLeft;
    property Top: Integer read fTop write SetTop;
    property OnClick: TNotifyEvent read fOnClick write SetOnClick;
  end;

  TWebControl = public class(TControl)
  end;

  TForm = public class(TWebControl)
  private
    fRootView: dynamic;
    method setRootView(value: dynamic);
    fRoot: dynamic;
  public
    constructor(aOwner: TComponent);
    property RootView: dynamic read fRootView write setRootView;
  end;

  TButton = public class(TWebControl)
  private
    fCaption: String;
    method setCaption(aValue: String);    
  protected
    method ClassName: String; override;
  public
    constructor(aOwner: TComponent);
    class method Create(AOwner: TComponent): TButton;
    property Caption: String read fCaption write setCaption;
  end;

  TLabel = public class(TWebControl)
  private
    fCaption: String;
    method setCaption(aValue: String);
 protected
    method ClassName: String; override;
  public
    constructor(aOwner: TComponent);
    class method Create(aOwner: TComponent): TLabel;
    property Caption: String read fCaption write setCaption;
  end;

  TPanel = public class(TWebControl)
  private  
  public
    constructor(aOwner: TComponent);
  end;

  TEdit = public class(TWebControl)
  private
    method setText(value: String);
    method getText: String;
  public
    constructor(aOwner: TComponent);
    property Text: String read getText write setText;
  end;

  TCheckBox = public class(TWebControl)
  private
  
  public
  end;

  TRadioButton = public class(TWebControl)
  private
  
  public
  end;

  TComboBox = public class(TWebControl)
  private
  
  public
  end;

  TListBox = public class(TWebControl)
  private
    fItems: TStrings;
  public
    constructor(aOwner: TComponent);
    method AddItem(aValue: String);
  end;


implementation

method TControl.SetParent(aValue: TWebControl);
begin
  fParent := aValue;
  aValue.Handle.appendChild(fHandle);           
end;

method TControl.SetOnClick(aValue: TNotifyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));

  fHandle.addEventListener("click", lDelegate);
end;

method TControl.SetText(aValue: String);
begin
  fText := aValue;
end;

constructor TButton(aOwner: TComponent);
begin
  fHandle := WebAssembly.CreateElement('BUTTON');
  var lCaption := WebAssembly.CreateTextNode(Name);
  fHandle.setAttribute('id', Name);
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

method TComponent.setName(aValue: String);
begin
  fName := aValue;
end;

method TComponent.GetDefaultName: String;
begin
  var i := 1;
  var lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  while lObject <> nil do begin
    inc(i);
    lObject := WebAssembly.GetElementById(ClassName + i.ToString);
  end;
  result := ClassName + i.ToString;
end;

method TButton.ClassName: String;
begin
  result := 'Button';
end;

constructor TControl(aOwner: TComponent);
begin

end;

constructor TComponent(aOwner: TComponent);
begin
  fOwner := aOwner;
  Name := GetDefaultName;
end;

method TLabel.setCaption(aValue: String);
begin
  fCaption := aValue;
  fHandle.innerHTML := aValue;
end;

constructor TLabel(aOwner: TComponent);
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

method TControl.ClassName: String;
begin
  result := 'TControl';
end;

method TComponent.ClassName: String;
begin
  result := 'TComponent';
end;

method TControl.SetWidth(aValue: Integer);
begin
  fWidth := aValue;
  fHandle.style.width := aValue.ToString + 'px';
end;

method TControl.SetHeight(aValue: Integer);
begin
  fHeight := aValue;
  fHandle.style.height := aValue.ToString + 'px';
end;

constructor TForm(aOwner: TComponent);
begin  
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "relative";
  fHandle.style.margin := "0 auto";
end;

constructor TPanel(aOwner: TComponent);
begin
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "absolute";
end;

method TControl.setLeft(aValue: Integer);
begin
  fLeft := aValue;
  fHandle.style.left := aValue.ToString + 'px';
end;

method TControl.SetTop(aValue: Integer);
begin
  fTop := aValue;
  fHandle.style.top := aValue.ToString + 'px';
end;

method TComponent.Loaded;
begin
  // Nothing
end;

method TControl.Loaded;
begin
  fHandle.setAttribute('id', Name);
end;

constructor TEdit(aOwner: TComponent);
begin
  fHandle := WebAssembly.CreateElement("INPUT");
  fHandle.setAttribute("type", "text");
  fHandle.style.position := "absolute";
end;

method TComponent.setOwner(aValue: TComponent);
begin
  fOwner := aValue;
end;

method TForm.setRootView(value: dynamic);
begin
  fRoot := value;
  if fRoot = nil then begin

  end
  else begin
     value.appendChild(fHandle);
  end;
end;

constructor TListBox(aOwner: TComponent);
begin
  fItems := TStringList.Create;
  fHandle := WebAssembly.CreateElement("SELECT");
  fHandle.setAttribute("size", 6);
  fHandle.style.position := "absolute";
end;

method TListBox.AddItem(aValue: String);
begin
  fItems.Add(aValue);
  var lItem := WebAssembly.CreateElement("option");
  lItem.text := aValue;
  fHandle.add(lItem);
end;

method TEdit.getText: String;
begin
  result := fHandle.value;
end;

method TEdit.setText(value: String);
begin

end;


end.