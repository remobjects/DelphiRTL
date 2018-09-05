namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS)) OR ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type

  TAlign = public enum (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom) of Integer;

  TControl = public partial class(TComponent)
  private
    fParent: TNativeControl;
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
    fTabOrder: Integer; // TODO
    fParentFont: Boolean := true;
    fAlign: TAlign;
    method GetCaption: String;
    method SetCaption(aValue: String);
    method SetWidth(aValue: Integer);
    method SetHeight(aValue: Integer);
    method SetTop(aValue: Integer);
    method SetLeft(aValue: Integer);
    method SetParent(aValue: TNativeControl);
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
    method SetParentFont(aValue: Boolean);
    method SetAlign(value: TAlign);

  protected
    fHandle: TPlatformHandle := {$IF ISLAND AND WINDOWS}0{$ELSE}nil{$ENDIF};
    fFont: TFont;
    method CreateHandle; partial; virtual; empty;
    method HandleNeeded; virtual;
    method HandleAllocated: Boolean; virtual; partial; empty;
    method Changed(aObject: TObject; propName: String);
    method RequestAlign; virtual;
    constructor(aOwner: TComponent);

    method PlatformGetCaption: String; partial; empty;
    method PlatformSetCaption(aValue: String); virtual; partial; empty;
    method PlatformSetWidth(aValue: Integer); virtual; partial; empty;
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
    method Show; virtual; partial; empty;

    property Handle: TPlatformHandle read fHandle;
    property Font: TFont read fFont write setFont;
    property Parent: TNativeControl read fParent write SetParent;
    property ParentFont: Boolean read fParentFont write SetParentFont;
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
    property TabOrder: Integer read fTabOrder write fTabOrder;
    property Align: TAlign read fAlign write SetAlign;
    property OnClick: TNotifyEvent read fOnClick write SetOnClick;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write SetOnKeyPress;
    property OnKeyDown: TKeyEvent read fOnKeyDown write SetOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write setOnKeyUp;
  end;

  TNativeControl = public partial class(TControl)
  public
    method AlignControl(aControl: TControl); virtual;
    method AlignControls(aControl: TControl; var Rect: TRect); virtual;
  end;

  TCustomControl = public class(TNativeControl)
  private
    //fCanvas: TCanvas;
  end;

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
  {$IF WEBASSEMBLY OR ECHOESWPF}
  CreateHandle;
  {$ENDIF}
  PlatformApplyDefaults;
end;

method TControl.Changed(aObject: TObject; propName: String);
begin
  if aObject is TFont then
    PlatformFontChanged;
end;

method TControl.HandleNeeded;
begin
  if not HandleAllocated then
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

method TControl.SetParent(aValue: TNativeControl);
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

method TControl.SetParentFont(aValue: Boolean);
begin
  fParentFont := aValue;
  // PlatformSetParentFont TODO
end;

method TControl.InsertControl(aControl: TControl);
begin
  if fControls = nil then
    fControls := new TList<TControl>();

  fControls.Add(aControl);
end;

method TControl.SetAlign(value: TAlign);
begin
  if value <> fAlign then
    RequestAlign;
end;

method TControl.RequestAlign;
begin
  if Parent ≠ nil then
    Parent.AlignControl(self);
end;

method TNativeControl.AlignControl(aControl: TControl);
begin
  var lRect: TRect;
  AlignControls(aControl, var lRect);
end;

method TNativeControl.AlignControls(aControl: TControl; var Rect: TRect);
begin

end;

{$ENDIF}

end.