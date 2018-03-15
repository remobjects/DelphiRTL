namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TPlatformHandle = {$IF WEBASSEMBLY} dynamic {$ENDIF};
  TControl = public partial class(TComponent)
  private
    method setFont(value: TFont);
  protected
    fHandle: dynamic;
    fFont: TFont;
    /*method PlatformSetWidth(aValue: Integer); partial; empty;
    method PlatformSetHeight(aValue: Integer); partial; empty;
    method PlatformSetTop(aValue: Integer); virtual; partial; empty;
    method PlatformSetLeft(aValue: Integer); virtual; partial; empty;
    method PlatformSetParent(aValue: TControl); virtual; partial; empty;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial; empty;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial; empty;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial; empty;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial; empty;

    method PlatformFontSetColor(value: TColor); partial; empty;
    method PlatformFontSetName(value: String); partial; empty;
    method PlatformFontSetSize(value: Integer); partial; empty;
    method PlatformFontSetStyles(value: TFontStyles); partial; empty;
*/
    //method GetDefaultName: String; virtual; partial; empty;
    method CreateHandle; abstract;
    //method ApplyDefaults; virtual; partial; empty;
    method Changed(aObject: TObject; propName: String);

    constructor(aOwner: TComponent);
  public
    property Handle: dynamic read fHandle;
    property Font: TFont read fFont write SetFont;
  end;


implementation

constructor TControl(aOwner: TComponent);
begin
  Name := GetDefaultName;
  fFont := new TFont();
  fFont.PropertyChanged += @Changed;
  CreateHandle;
  ApplyDefaults;
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

method TControl.SetFont(value: TFont);
begin
  fFont := value;
  PlatformFontSetColor(fFont.Color);
  PlatformFontSetSize(fFont.Size);
  PlatformFontSetName(fFont.Name);
  PlatformFontSetStyles(fFont.Style);
end;


end.