namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND (WEBASSEMBLY OR WINDOWS) OR ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TFontStyle = public enum(Bold, Italic, Underline, StrikeOut) of Integer;
  TFontStyles = public set of TFontStyle;
  //TFontCharset = public 0..255;
  TFontCharset = Integer;

  TFont = public partial class(TPersistent)
  private
    fColor: TColor;
    fName: String := 'Tahoma';
    fSize: Integer;
    fStyles: TFontStyles := [];
    fCharset: TFontCharset; // TODO
    fHeight: Integer := 12;
    method setColor(aValue: TColor);
    method setName(aValue: String);
    method setSize(aValue: Integer);
    method setStyles(aValue: TFontStyles);
    method setHeight(aValue: Integer);
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
  {$IF WEBASSEMBLY}
  NotifyChanged('size');
  {$ELSEIF ISLAND AND WINDOWS}
  SetHeight(-(aValue * fPixelsPerInch) / 72); // A point is very close to 1/72 inch...
  {$ENDIF}
end;

method TFont.SetHeight(aValue: Integer);
begin
  fHeight := aValue;
  PlatformSetHeight(aValue);
end;

method TFont.SetStyles(aValue: TFontStyles);
begin
  fStyles := aValue;
  NotifyChanged('styles');
end;

{$ENDIF}


end.