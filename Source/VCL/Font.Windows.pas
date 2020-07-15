namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TFont = public partial class(TPersistent)
  private
    fFontHandle: rtl.HFONT;

    method GetFontHandle: rtl.HFONT;
    method SetFontHandle(aValue: rtl.HFONT);

    class method CreateFontFromData(aFontName: String; aCharset: TFontCharset; aHeight: Integer; aOrientation: Integer; aPitch: TFontPitch; aQuality: TFontQuality; aStyle: TFontStyles): rtl.HFONT;
    class var fDefaultFont: rtl.HFONT := 0;
    class var fDevicePixelsPerInch: Integer := 0;
    class constructor;

  protected
    method PlatformUpdate; virtual; partial;
    method PlatformSetHeight(aValue: Integer); virtual; partial;

  public
    constructor;
    property FontHandle: rtl.HFONT read GetFontHandle write SetFontHandle;
  end;

implementation

method TFont.PlatformSetHeight(aValue: Integer);
begin
  NotifyChanged('height');
end;

method TFont.GetFontHandle: rtl.HFONT;
begin
  result := fFontHandle;
end;

method TFont.SetFontHandle(aValue: rtl.HFONT);
begin
  fFontHandle := aValue;
end;

constructor TFont;
begin
  FontHandle := fDefaultFont;
  fPixelsPerInch := fDevicePixelsPerInch;
end;

class method TFont.CreateFontFromData(aFontName: String; aCharset: TFontCharset; aHeight: Integer; aOrientation: Integer; aPitch: TFontPitch; aQuality: TFontQuality; aStyle: TFontStyles): rtl.HFONT;
begin
  var lFontInfo: rtl.LOGFONT;
  lFontInfo.lfWidth := 0;
  lFontInfo.lfHeight := aHeight;
  lFontInfo.lfEscapement := 0;
  lFontInfo.lfOrientation := aOrientation;
  lFontInfo.lfWeight := if (TFontStyle.Bold in aStyle) then rtl.FW_BOLD else rtl.FW_NORMAL;
  lFontInfo.lfItalic := if (TFontStyle.Italic in aStyle) then 1 else 0;
  lFontInfo.lfUnderline := if (TFontStyle.Underline in aStyle) then 1 else 0;
  lFontInfo.lfCharSet := aCharset;
  lFontInfo.lfOutPrecision := rtl.OUT_DEFAULT_PRECIS;
  lFontInfo.lfClipPrecision := rtl.CLIP_DEFAULT_PRECIS;
  lFontInfo.lfQuality := aQuality;
  lFontInfo.lfPitchAndFamily := aPitch;
  var lName := PlatformString(aFontName).ToCharArray(true);
  memcpy(@lFontInfo.lfFaceName[0], @lName[0], length(lName) * 2);

  result := rtl.CreateFontIndirect(@lFontInfo);
end;

class constructor TFont;
begin
  var lDC := rtl.GetDC(rtl.HWND(0));
  fDevicePixelsPerInch := rtl.GetDeviceCaps(lDC, rtl.LOGPIXELSY);
  rtl.ReleaseDC(rtl.HWND(0), lDC);

  fDefaultFont := CreateFontFromData('Tahoma', 0, 12, 0, TFontPitch.fpDefault, TFontQuality.fqDefault, []);
end;

method TFont.PlatformUpdate;
begin
  var lOldFont := FontHandle;
  FontHandle := CreateFontFromData(fName, fCharset, fHeight, fOrientation, fPitch, fQuality, fStyle);
  //if lOldFont <> fDefaultFont then
    //rtl.DeleteObject(lOldFont);
end;

{$ENDIF}

end.