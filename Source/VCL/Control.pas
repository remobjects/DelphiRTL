namespace RemObjects.Elements.RTL.Delphi.VCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TNativeControl = public partial class(TControl)
  end;

  TControl = public partial class(TComponent)
  private
    fParent: TControl;
    fOnClick: TNotifyEvent;
    fWidth: Integer;
    fHeight: Integer;
    fTop: Integer;
    fLeft: Integer;
    fColor: TColor;
    fOnKeyPress: TKeyPressEvent;
    fOnKeyDown: TKeyEvent;
    fOnKeyUp: TKeyEvent;
    fVisible: Boolean := true;
    fCaption: String;
    fControls: TList<TControl> := nil;
    method GetCaption: String;
    method SetCaption(aValue: String);
    method SetWidth(aValue: Integer);
    method SetHeight(aValue: Integer);
    method SetTop(aValue: Integer);
    method SetLeft(aValue: Integer);
    method SetParent(aValue: TControl);
    method setFont(value: TFont);
    method SetOnClick(aValue: TNotifyEvent);
    method setOnKeyUp(value: TKeyEvent);
    method SetOnKeyPress(value: TKeyPressEvent);
    method SetOnKeyDown(aValue: TKeyEvent);
    method GetClientHeight: Integer;
    method SetClientHeight(aValue: Integer);
    method GetClientWidth: Integer;
    method SetClientWidth(aValue: Integer);
    method SetColor(aValue: TColor);
    method SetVisible(aValue: Boolean);

  protected
    fHandle: TPlatformHandle;
    fFont: TFont;
    method CreateHandle; virtual; partial; empty;
    method HandleNeeded; virtual;
    method Loaded; override;
    method Changed(aObject: TObject; propName: String);
    constructor(aOwner: TComponent);

    method PlatformGetCaption: String; partial; empty;
    method PlatformSetCaption(aValue: String); partial; empty;
    method PlatformSetWidth(aValue: Integer); partial; empty;
    method PlatformSetHeight(aValue: Integer); partial; empty;
    method PlatformSetTop(aValue: Integer); virtual; partial; empty;
    method PlatformSetLeft(aValue: Integer); virtual; partial; empty;
    method PlatformSetParent(aValue: TControl); virtual; partial; empty;
    method PlatformSetColor(aValue: TColor); virtual; partial; empty;
    method PlatformSetVisible(aValue: Boolean); virtual; partial; empty;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial; empty;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial; empty;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial; empty;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial; empty;

    method PlatformGetDefaultName: String; virtual; partial; empty;
    method PlatformApplyDefaults; virtual; partial; empty;
    method PlatformFontChanged; virtual; partial; empty;

    method Click; virtual;

  public
    method InsertControl(aControl: TControl);

    property Handle: TPlatformHandle read fHandle;
    property Font: TFont read fFont write setFont;
    property Parent: TControl read fParent write SetParent;
    property Caption: String read GetCaption write SetCaption;
    property Height: Integer read fHeight write SetHeight;
    property Width: Integer read fWidth write SetWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property Controls: TList<TControl> read fControls;
    //property ClientRect: RemObjects.TElements.RTL.RTLException
    property Left: Integer read fLeft write SetLeft;
    property Top: Integer read fTop write SetTop;
    property Color: TColor read fColor write SetColor;
    property Visible: Boolean read fVisible write SetVisible;
    property OnClick: TNotifyEvent read fOnClick write SetOnClick;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write SetOnKeyPress;
    property OnKeyDown: TKeyEvent read fOnKeyDown write SetOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write setOnKeyUp;
  end;
/*
TControl = public partial class(TComponent)
protected
  method CreateHandle; virtual; partial; empty;
  method HandleNeeded; virtual; partial; empty;
end;*/

/*TWinControl = public partial class(TControl)
protected
  method CreateWnd; virtual; empty;
end;

TScrollingWinControl = public partial class(TPlatformBaseControlClass)
end;*/


implementation

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

constructor TControl(aOwner: TComponent);
begin
  Name := PlatformGetDefaultName;
  fFont := new TFont();
  fFont.PropertyChanged := @Changed;
  //CreateHandle; // TODO
  PlatformApplyDefaults;
end;

method TControl.Changed(aObject: TObject; propName: String);
begin
  if aObject is TFont then
    PlatformFontChanged;
end;

method TControl.Loaded;
begin
end;

method TControl.HandleNeeded;
begin
  CreateHandle;
end;

method TControl.SetFont(value: TFont);
begin
  fFont := value;
  PlatformFontChanged;
end;

method TControl.GetCaption: String;
begin
  result := PlatformGetCaption;
end;

method TControl.SetCaption(aValue: String);
begin
  fCaption := aValue;
  PlatformSetCaption(aValue);
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
  aValue.InsertControl(self);
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

method TControl.Click;
begin
  if assigned(fOnClick) then
    fOnClick(self);
end;

method TControl.GetClientHeight: Integer;
begin
  result := fHeight;
end;

method TControl.SetClientHeight(aValue: Integer);
begin
  Height := aValue;
end;

method TControl.GetClientWidth: Integer;
begin
  result := fWidth;
end;

method TControl.SetClientWidth(aValue: Integer);
begin
  Width := aValue;
end;

method TControl.SetColor(aValue: TColor);
begin
  PlatformSetColor(aValue);
end;

method TControl.SetVisible(aValue: Boolean);
begin
  fVisible := aValue;
  PlatformSetVisible(aValue);
end;

method TControl.InsertControl(aControl: TControl);
begin
  if fControls = nil then
    fControls := new TList<TControl>();

  fControls.Add(aControl);
end;

end.