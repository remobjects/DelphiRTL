namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TComponent = public class(TObject)
  private
    fName: String;
    fOwner: TComponent;
    method setName(aValue: String);
    method setOwner(aValue: TComponent);
  protected    
    method ClassName: String; virtual;
    method Loaded; virtual;
    constructor(aOwner: TComponent);
  public
    property Name: String read fName write setName;
    property Owner: TComponent read fOwner write setOwner;
  end;

  TShiftState = set of (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssTouch, ssPen, ssCommand, ssHorizontal);

  TKeyPressEvent = public block(Sender: TObject; var Key: Char);
  TKeyEvent = public block(Sender: TObject; var Key: Word; Shift: TShiftState);
  
  TControl = public partial class(TComponent)
  private
    fParent: TControl;
    fOnClick: TNotifyEvent;
    fWidth: Integer;
    fHeight: Integer;
    fTop: Integer;
    fLeft: Integer;
    fOnKeyPress: TKeyPressEvent;
    fOnKeyDown: TKeyEvent;
    fOnKeyUp: TKeyEvent;
    method SetWidth(aValue: Integer);
    method SetHeight(aValue: Integer);
    method SetTop(aValue: Integer);
    method SetLeft(aValue: Integer);
    method SetParent(aValue: TControl);
    method SetOnClick(aValue: TNotifyEvent);
    method setOnKeyUp(value: TKeyEvent);
    method SetOnKeyPress(value: TKeyPressEvent);
    method SetOnKeyDown(aValue: TKeyEvent);

protected
    method ClassName: String; override;
    method Loaded; override;
    
    method PlatformSetWidth(aValue: Integer); partial; empty;
    method PlatformSetHeight(aValue: Integer); partial; empty;
    method PlatformSetTop(aValue: Integer); partial; empty;
    method PlatformSetLeft(aValue: Integer); partial; empty;
    method PlatformSetParent(aValue: TControl); partial; empty;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial; empty;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial; empty;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial; empty;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial; empty;
    
  public
    property Parent: TControl read fParent write SetParent;
    property Height: Integer read fHeight write SetHeight;
    property Width: Integer read fWidth write SetWidth;
    property Left: Integer read fLeft write setLeft;
    property Top: Integer read fTop write SetTop;
    property OnClick: TNotifyEvent read fOnClick write SetOnClick;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write SetOnKeyPress;
    property OnKeyDown: TKeyEvent read fOnKeyDown write SetOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write setOnKeyUp;
  end;

implementation

method TComponent.setName(aValue: String);
begin
  fName := aValue;
end;

constructor TComponent(aOwner: TComponent);
begin
  fOwner := aOwner;
end;

method TComponent.ClassName: String;
begin
  result := 'TComponent';
end;

method TComponent.setOwner(aValue: TComponent);
begin
  fOwner := aValue;
end;

method TComponent.Loaded;
begin
  // Nothing
end;

method TControl.SetLeft(aValue: Integer);
begin
  fLeft := aValue;
  PlatformSetLeft(aValue);
end;

method TControl.SetTop(aValue: Integer);
begin
  fTop := aValue;
  PlatformSetTop(aValue);
end;

method TControl.Loaded;
begin
end;

method TControl.ClassName: String;
begin
  result := 'TControl';
end;

method TControl.SetWidth(aValue: Integer);
begin
  fWidth := aValue;
  PlatformSetWidth(aValue);
end;

method TControl.SetHeight(aValue: Integer);
begin
  fHeight := aValue;
  PlatformSetHeight(aValue);
end;

method TControl.SetParent(aValue: TControl);
begin
  fParent := aValue;
  PlatformSetParent(aValue);
end;

method TControl.SetOnClick(aValue: TNotifyEvent);
begin
  fOnClick := aValue;
  PlatformSetOnClick(aValue);
end;

method TControl.SetOnKeyPress(value: TKeyPressEvent);
begin
  fOnKeyPress := value;
  PlatformSetOnKeyPress(value);
end;

method TControl.setOnKeyUp(value: TKeyEvent);
begin
  fOnKeyUp := value;
end;

method TControl.SetOnKeyDown(aValue: TKeyEvent);
begin
  fOnKeyDown := aValue;
end;

end.