namespace RemObjects.Elements.RTL.Delphi;

{$IF NOT TOFFEE}

interface

type
  TComponent = public class(TObject)
  private
    fName: String;
    fOwner: TComponent;
    method setName(aValue: String);
    method setOwner(aValue: TComponent);
  protected
    method Loaded; virtual;
    constructor(aOwner: TComponent);
  public
    property Name: String read fName write setName;
    property Owner: TComponent read fOwner write setOwner;
  end;

  TShiftStateValues = public enum(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssTouch, ssPen, ssCommand, ssHorizontal) of Integer;
  TShiftState = public set of TShiftStateValues;

  TKeyPressEvent = public block(Sender: TObject; var Key: Char);
  TKeyEvent = public block(Sender: TObject; var Key: Word; Shift: TShiftState);

  TPlatformHandle = {$IF WEBASSEMBLY} dynamic {$ELSEIF ISLAND AND WINDOWS} rtl.HWND {$ELSE} Object {$ENDIF};
  TPropertyChangedEvent = public block(Sender: TObject; PropName: String);

  INotifyPropertyChanged = public interface
    {$IF NOT COOPER}
    event PropertyChanged: Action<TObject, String>;
    {$ENDIF}
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
    method SetClientHeight(value: Integer);
    method GetClientWidth: Integer;
    method SetClientWidth(value: Integer);
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

    method PlatformFontSetColor(value: TColor); partial; empty;
    method PlatformFontSetName(value: String); partial; empty;
    method PlatformFontSetSize(value: Integer); partial; empty;
    method PlatformFontSetStyles(value: TFontStyles); partial; empty;

    method PlatformGetDefaultName: String; virtual; partial; empty;
    method PlatformApplyDefaults; virtual; partial; empty;

  public
    property Handle: TPlatformHandle read fHandle;
    property Font: TFont read fFont write setFont;
    property Parent: TControl read fParent write SetParent;
    property Caption: String read GetCaption write SetCaption;
    property Height: Integer read fHeight write SetHeight;
    property Width: Integer read fWidth write SetWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
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

  TColor = Integer;
  TFontStyle = public enum(Bold, Italic, Underline, StrikeOut) of Integer;
  TFontStyles = public set of TFontStyle;
  TFontCharset = public 0..255;

  TFont = public class(TPersistent)
  private
    fColor: TColor;
    fName: String;
    fSize: Integer;
    fStyles: TFontStyles;
    fCharset: TFontCharset; // TODO
    fHeight: Integer; // TODO
    method setColor(value: TColor);
    method setName(value: String);
    method setSize(value: Integer);
    method setStyles(value: TFontStyles);
    method NotifyChanged(propName: String);
  public
    property PropertyChanged: TPropertyChangedEvent;
    property Color: TColor read fColor write setColor;
    property Name: String read fName write setName;
    property Size: Integer read fSize write setSize;
    property Style: TFontStyles read fStyles write setStyles;
    property Charset: TFontCharset read fCharset write fCharset;
    property Height: Integer read fHeight write fHeight;
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
  if aObject is TFont then begin
    case propName of
      'color': PlatformFontSetColor(fFont.Color);
      'size': PlatformFontSetSize(fFont.Size);
      'name': PlatformFontSetName(fFont.Name);
      'styles': PlatformFontSetStyles(fFont.Style);
    end;
  end;
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
  PlatformFontSetColor(fFont.Color);
  PlatformFontSetSize(fFont.Size);
  PlatformFontSetName(fFont.Name);
  PlatformFontSetStyles(fFont.Style);
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

method TControl.GetClientHeight: Integer;
begin
  // TODO
end;

method TControl.SetClientHeight(value: Integer);
begin
  // TODO
end;

method TControl.GetClientWidth: Integer;
begin
  // TODO
end;

method TControl.SetClientWidth(value: Integer);
begin
  // TODO
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

{$ENDIF}

end.