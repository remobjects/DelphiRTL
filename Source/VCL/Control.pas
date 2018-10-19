namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS)) OR ECHOESWPF OR (MACOS AND NOT DARWIN)}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type

  TAlign = public enum (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom) of Integer;
  TMarginSize = public Cardinal;

  TMargins = public class(TPersistent)
  private
    method SetBottom(value: TMarginSize);
    method SetRight(value: TMarginSize);
    method SetTop(value: TMarginSize);
    method SetLeft(value: TMarginSize);
  protected
    fLeft: TMarginSize;
    fTop: TMarginSize;
    fRight: TMarginSize;
    fBottom: TMarginSize;
    class method InitDefaults(Margins: TMargins); virtual;
  public
    property Left: TMarginSize read fLeft write SetLeft;
    property Top: TMarginSize read fTop write SetTop;
    property Right: TMarginSize read fRight write SetRight;
    property Bottom: TMarginSize read fBottom write SetBottom;
  end;

  TPadding = public class(TMargins)
  protected
    class method InitDefaults(Margins: TMargins); override;
  public
    constructor;
  end;

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
    fMargins: TMargins;
    fAlignWithMargins: Boolean := false;
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
    method SetColor(aValue: TColor);
    method SetVisible(aValue: Boolean);
    method SetParentFont(aValue: Boolean);
    method SetAlign(value: TAlign);
    method SetMargins(aValue: TMargins);
    method SetAlignWithMargins(value: Boolean);
    method SetTabOrder(aValue: Integer);

  protected
    fHandle: TPlatformHandle := {$IF ISLAND AND WINDOWS}0{$ELSE}nil{$ENDIF};
    fFont: TFont;
    fExplicitLeft: Integer;
    fExplicitTop: Integer;
    fExplicitHeight: Integer;
    fExplicitWidth: Integer;
    fWidthDelta: Integer := 0;
    fHeightDelta: Integer := 0;
    method GetClientHeight: Integer; virtual;
    method SetClientHeight(aValue: Integer); virtual;
    method GetClientWidth: Integer; virtual;
    method SetClientWidth(aValue: Integer); virtual;
    method CreateHandle; partial; virtual; empty;
    method HandleNeeded; virtual;
    method HandleAllocated: Boolean; virtual; partial; empty;
    method Changed(aObject: TObject; propName: String);
    method RequestAlign; virtual;
    constructor(aOwner: TComponent);

    method PlatformGetCaption: String; partial; empty;
    method PlatformSetCaption(aValue: String); virtual; partial; empty;
    method PlatformSetWidth(aValue: Integer); virtual; partial; empty;
    method PlatformSetHeight(aValue: Integer); virtual; partial; empty;
    method PlatformSetTop(aValue: Integer); virtual; partial; empty;
    method PlatformSetLeft(aValue: Integer); virtual; partial; empty;
    method PlatformSetParent(aValue: TControl); virtual; partial; empty;
    method PlatformSetColor(aValue: TColor); virtual; partial; empty;
    method PlatformSetVisible(aValue: Boolean); virtual; partial; empty;
    method PlatformSetTabOrder(aValue: Integer); virtual; partial; empty;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial; empty;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial; empty;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial; empty;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial; empty;

    method PlatformGetDefaultName: String; virtual; partial; empty;
    method PlatformApplyDefaults; virtual; partial; empty;
    method PlatformInitControl; virtual; partial; empty;
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
    property TabOrder: Integer read fTabOrder write SetTabOrder;
    property Align: TAlign read fAlign write SetAlign;
    property Margins: TMargins read fMargins write SetMargins;
    property AlignWithMargins: Boolean read fAlignWithMargins write SetAlignWithMargins;
    property ExplicitLeft: Integer read fExplicitLeft write fExplicitLeft;
    property ExplicitTop: Integer read fExplicitTop write fExplicitTop;
    property ExplicitWidth: Integer read fExplicitWidth write fExplicitWidth;
    property ExplicitHeight: Integer read fExplicitHeight write fExplicitHeight;
    property OnClick: TNotifyEvent read fOnClick write SetOnClick;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write SetOnKeyPress;
    property OnKeyDown: TKeyEvent read fOnKeyDown write SetOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write setOnKeyUp;
  end;

  TNativeControl = public partial class(TControl)
  private
    fPadding: TPadding;
    method SetPadding(value: TPadding);
    method SetTabStop(value: Boolean);
  protected
    fTabStop: Boolean;
    method DoAlign(aAlign: TAlign; var aRect: TRect);
    method PlatformSetTapStop(value: Boolean); virtual; partial; empty;
  public
    method AlignControl(aControl: TControl); virtual;
    method AlignControls(aControl: TControl; var Rect: TRect); virtual;

    property Padding: TPadding read fPadding write SetPadding;
    property TabStop: Boolean read fTabStop write SetTabStop default false;
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
  PlatformInitControl;
  Name := PlatformGetDefaultName;
  fFont := new TFont();
  fFont.PropertyChanged := @Changed;
  {$IF WEBASSEMBLY OR ECHOESWPF OR MACOS}
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
  result := fCaption;
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
  result := fHeight - fHeightDelta;
end;

method TControl.SetClientHeight(aValue: Integer);
begin
  Height := aValue + fHeightDelta;
end;

method TControl.GetClientWidth: Integer;
begin
  result := fWidth - fWidthDelta;
end;

method TControl.SetClientWidth(aValue: Integer);
begin
  Width := aValue + fWidthDelta;
end;

method TControl.SetColor(aValue: TColor);
begin
  fColor := aValue;
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

method TControl.SetTabOrder(aValue: Integer);
begin
  fTabOrder := aValue;
  // Platforms that support tab order can do itself, just pass the value to the platform
  // Win32 need to handle internally, so save the value here and PlatformSetTabOrder will be empty.
  PlatformSetTabOrder(aValue);
end;

method TControl.InsertControl(aControl: TControl);
begin
  if fControls = nil then
    fControls := new TList<TControl>();

  fControls.Add(aControl);
end;

method TControl.SetAlign(value: TAlign);
begin
  if value <> fAlign then begin
    fAlign := value;
    RequestAlign;
  end;
end;

method TControl.SetMargins(aValue: TMargins);
begin
  fMargins := aValue;
end;

method TControl.SetAlignWithMargins(value: Boolean);
begin
  fAlignWithMargins := value;
  // TODO update align!
end;

method TControl.RequestAlign;
begin
  if (Parent ≠ nil) and (not TComponentState.csLoading in ComponentState) then
    Parent.AlignControl(self);
end;

method TNativeControl.AlignControl(aControl: TControl);
begin
  var lRect: TRect;
  lRect.Top := 0;
  lRect.Left := 0;
  lRect.Right := Width;
  lRect.Bottom := Height;
  AlignControls(aControl, var lRect);
end;

method TNativeControl.AlignControls(aControl: TControl; var Rect: TRect);
begin
  inc(Rect.Left, Padding.Left);
  inc(Rect.Top, Padding.Top);
  dec(Rect.Right, Padding.Right);
  dec(Rect.Bottom, Padding.Bottom);

  DoAlign(TAlign.alTop, var Rect);
  DoAlign(TAlign.alBottom, var Rect);
  DoAlign(TAlign.alLeft, var Rect);
  DoAlign(TAlign.alRight, var Rect);
  DoAlign(TAlign.alClient, var Rect);
end;

method TNativeControl.DoALign(aAlign: TAlign; var aRect: TRect);
begin
  var lControls := new TList<TControl>();

  for i: Integer := 0 to Controls.Count - 1 do
    if Controls[i].Align = aAlign then
      lControls.Add(Controls[i]);

  var lNewLeft, lNewTop, lNewWidth, lNewHeight, lDelta: Int32;

  for each lControl in lControls do begin
    case aAlign of
      TAlign.alTop: begin
        lNewTop := aRect.Top;
        lNewLeft := aRect.Left;
        lNewWidth := aRect.Right - aRect.Left;
        if lControl.AlignWithMargins then begin
          inc(lNewTop, lControl.Margins.Top);
          inc(lNewLeft, lControl.Margins.Left);
          lNewWidth := lNewWidth - (lControl.Margins.Left + lControl.Margins.Right);
          lDelta := lControl.Margins.Bottom;
        end
        else
          lDelta := 0;

        lControl.Width := lNewWidth;
        aRect.Top := lNewTop + lControl.Height + lDelta;
      end;

      TAlign.alBottom: begin
        lNewTop := aRect.Bottom - lControl.Height;
        lNewLeft := aRect.Left;
        lNewWidth := aRect.Right - aRect.Left;
        if lControl.AlignWithMargins then begin
          dec(lNewTop, lControl.Margins.Bottom);
          inc(lNewLeft, lControl.Margins.Left);
          lNewWidth := lNewWidth - (lControl.Margins.Left + lControl.Margins.Right);
          lDelta := lControl.Margins.Top;
        end
        else
          lDelta := 0;

        lControl.Width := lNewWidth;
        aRect.Bottom := lNewTop - lDelta;
      end;

      TAlign.alLeft: begin
        lNewTop := aRect.Top;
        lNewLeft := aRect.Left;
        lNewHeight := aRect.Bottom - aRect.Top;
        if lControl.AlignWithMargins then begin
          inc(lNewTop, lControl.Margins.Top);
          inc(lNewLeft, lControl.Margins.Left);
          lNewHeight := lNewHeight - (lControl.Margins.Top + lControl.Margins.Bottom);
          lDelta := lControl.Margins.Right;
        end
        else
          lDelta := 0;

        lControl.Height := lNewHeight;
        aRect.Left := lNewLeft + lDelta;
      end;

      TAlign.alRight: begin
        lNewTop := aRect.Top;
        lNewLeft := aRect.Right;
        lNewHeight := aRect.Bottom - aRect.Top;
        if lControl.AlignWithMargins then begin
          inc(lNewTop, lControl.Margins.Top);
          dec(lNewLeft, lControl.Margins.Right);
          lNewHeight := lNewHeight - (lControl.Margins.Top + lControl.Margins.Bottom);
          lDelta := lControl.Margins.Left;
        end
        else
          lDelta := 0;

        lControl.Height := lNewHeight;
        aRect.Left := lNewLeft - lDelta;
      end;
    end;
    lControl.Top := lNewTop;
    lControl.Left := lNewLeft;
  end;
end;

method TNativeControl.SetPadding(value: TPadding);
begin
  fPadding := value;
end;

method TNativeControl.SetTabStop(value: Boolean);
begin
  fTabStop := value;
  PlatformSetTapStop(value);
end;

method TMargins.SetLeft(value: TMarginSize);
begin
  fLeft := value;
end;

method TMargins.SetTop(value: TMarginSize);
begin
  fTop := value;
end;

method TMargins.setRight(value: TMarginSize);
begin
  fRight := value;
end;

method TMargins.SetBottom(value: TMarginSize);
begin
  fBottom := value;
end;

class method TMargins.InitDefaults(Margins: TMargins);
begin
  Margins.Left := TMarginSize(3);
  Margins.Top := TMarginSize(3);
  Margins.Right := TMarginSize(3);
  Margins.Bottom := TMarginSize(3);
end;

class method TPadding.InitDefaults(Margins: TMargins);
begin

end;

constructor TPadding;
begin
  fLeft := 0;
  fTop := 0;
  fRight := 0;
  fBottom := 0;
end;

{$ENDIF}

end.