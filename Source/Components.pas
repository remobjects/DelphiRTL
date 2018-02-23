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

  TControl = public abstract class(TComponent)
  private
    fParent: TControl;
    fOnClick: TNotifyEvent;
    fWidth: Integer;
    fHeight: Integer;
    fTop: Integer;
    fLeft: Integer;        
    method SetWidth(aValue: Integer);
    method SetHeight(aValue: Integer);
    method SetTop(aValue: Integer);
    method SetLeft(aValue: Integer);
    method SetParent(aValue: TControl);
    method SetOnClick(aValue: TNotifyEvent);
protected
    method ClassName: String; override;
    method Loaded; override;
    method PlatformSetWidth(aValue: Integer); abstract;
    method PlatformSetHeight(aValue: Integer); abstract;
    method PlatformSetTop(aValue: Integer); abstract;
    method PlatformSetLeft(aValue: Integer); abstract;
    method PlatformSetParent(aValue: TControl); abstract;
    method PlatformSetOnClick(aValue: TNotifyEvent); abstract;
    
    constructor(aOwner: TComponent);
  public
    property Parent: TControl read fParent write SetParent;
    property Height: Integer read fHeight write SetHeight;
    property Width: Integer read fWidth write SetWidth;
    property Left: Integer read fLeft write setLeft;
    property Top: Integer read fTop write SetTop;
    property OnClick: TNotifyEvent read fOnClick write SetOnClick;
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

constructor TControl(aOwner: TComponent);
begin

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

end.