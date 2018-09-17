namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS)) OR ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TFontStyle = public enum(Bold, Italic, Underline, StrikeOut) of Integer;
  TFontStyles = public set of TFontStyle;
  //TFontCharset = public 0..255;
  TFontCharset = Integer;
  TFontPitch = public enum (fpDefault, fpVariable, fpFixed) of Integer;
  TFontQuality = public enum(fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqClearType, fqClearTypeNatural) of Integer;

  TFont = public partial class(TPersistent)
  private
    fColor: TColor;
    fName: String := 'Tahoma';
    fSize: Integer;
    fStyles: TFontStyles := [];
    fCharset: TFontCharset; // TODO
    fHeight: Integer := 12;
    fPixelsPerInch: Integer;
    fOrientation: Integer;
    fQuality: TFontQuality := TFontQuality.fqClearType;
    fPitch: TFontPitch;
    method setColor(aValue: TColor);
    method setName(aValue: String);
    method setSize(aValue: Integer);
    method setStyles(aValue: TFontStyles);
    method setHeight(aValue: Integer);
    method GetOrientation: Integer;
    method SetOrientation(aValue: Integer);
    method GetPitch: TFontPitch;
    method SetPitch(aValue: TFontPitch);
    method GetQuality: TFontQuality;
    method SetQuality(aValue: TFontQuality);
    method NotifyChanged(aPropName: String);
  protected
    method PlatformUpdate; virtual; partial; empty;
    method PlatformSetHeight(aValue: Integer); virtual; partial; empty;
  public
    property PropertyChanged: TPropertyChangedEvent;
    property Color: TColor read fColor write setColor;
    property Name: String read fName write setName;
    property Size: Integer read fSize write setSize;
    property Style: TFontStyles read fStyles write setStyles;
    property Charset: TFontCharset read fCharset write fCharset;
    property Height: Integer read fHeight write SetHeight;
    property PixelsPerInch: Integer read fPixelsPerInch write fPixelsPerInch;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;
    property Pitch: TFontPitch read GetPitch write SetPitch default fpDefault;
    property Quality: TFontQuality read GetQuality write SetQuality default TFontQuality.fqDefault;
  end;

implementation

method TFont.NotifyChanged(aPropName: String);
begin
  PlatformUpdate;
  if PropertyChanged <> nil then
    PropertyChanged(self, aPropName);
end;

method TFont.SetColor(aValue: TColor);
begin
  fColor := aValue;
  NotifyChanged('color');
end;

method TFont.SetName(aValue: String);
begin
  fName := aValue;
  NotifyChanged('name');
end;

method TFont.SetSize(aValue: Integer);
begin
  fSize := aValue;
  {$IF WEBASSEMBLY OR ECHOESWPF}
  NotifyChanged('size');
  {$ELSEIF ISLAND AND WINDOWS}
  SetHeight(-(aValue * fPixelsPerInch) / 72); // A point is very close to 1/72 inch...
  {$ENDIF}
end;

method TFont.SetHeight(aValue: Integer);
begin
  fHeight := aValue;
  {$IF ISLAND AND WINDOWS}
  PlatformSetHeight(aValue);
  {$ELSE}
  SetSize(-aValue);
  {$ENDIF}
end;

method TFont.SetStyles(aValue: TFontStyles);
begin
  fStyles := aValue;
  NotifyChanged('styles');
end;

method TFont.GetQuality: TFontQuality;
begin
  result := fQuality;
end;

method TFont.SetQuality(aValue: TFontQuality);
begin
  fQuality := aValue;
  NotifyChanged('quality');
end;

method TFont.GetOrientation: Integer;
begin
  result := fOrientation;
  NotifyChanged('orientation');
end;

method TFont.SetOrientation(aValue: Integer);
begin
  fOrientation := aValue;
  NotifyChanged('orientation');
end;

method TFont.GetPitch: TFontPitch;
begin
  result := fPitch;
end;

method TFont.SetPitch(aValue: TFontPitch);
begin
  fPitch := aValue;
  NotifyChanged('pitch');
end;

{$ENDIF}


end.