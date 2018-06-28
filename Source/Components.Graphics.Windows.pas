namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TFontPitch = public enum (fpDefault, fpVariable, fpFixed) of Integer;
  TFontQuality = public enum(fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqClearType, fqClearTypeNatural) of Integer;

  TFont = public partial class(TPersistent)
  private
    fPixelsPerInch: Integer;
    fFontHandle: rtl.HFONT;
    method GetOrientation: Integer;
    method SetOrientation(value: Integer);
    method GetPitch: TFontPitch;
    method SetPitch(value: TFontPitch);
    method GetQuality: TFontQuality;
    method SetQuality(aValue: TFontQuality);
    method GetFontHandle: rtl.HFONT;
    method SetFontHandle(aValue: rtl.HFONT);

    class method CreateFontFromData(aFontName: String; aCharset: TFontCharset; aSize: Integer; aOrientation: Integer; aPitch: TFontPitch; aQuality: TFontQuality; aStyle: TFontStyles): rtl.HFONT;
    class var fDefaultFont: rtl.HFONT := 0;
    class var fPixelsPerInch: Integer := 0;
    class constructor;
  public
    constructor;
    property FontHandle: rtl.HFONT read GetFontHandle write SetFontHandle;
    property PixelsPerInch: Integer read fPixelsPerInch write fPixelsPerInch;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;
    property Pitch: TFontPitch read GetPitch write SetPitch default fpDefault;
    property Quality: TFontQuality read GetQuality write SetQuality default TFontQuality.fqDefault;
  end;

implementation

method TFont.GetQuality: TFontQuality;
begin

end;

method TFont.SetQuality(aValue: TFontQuality);
begin

end;

method TFont.GetOrientation: Integer;
begin

end;

method TFont.SetOrientation(value: Integer);
begin

end;

method TFont.GetPitch: TFontPitch;
begin

end;

method TFont.SetPitch(value: TFontPitch);
begin

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
end;

class method TFont.CreateFontFromData(aFontName: String; aCharset: TFontCharset; aSize: Integer; aOrientation: Integer; aPitch: TFontPitch; aQuality: TFontQuality; aStyle: TFontStyles): rtl.HFONT;
begin
  var lFontInfo: rtl.LOGFONT;
  lFontInfo.lfWidth := 0;
  lFontInfo.lfHeight := -(aSize * fPixelsPerInch) / 72; // A point is very close to 1/72 inch...
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
  var lName := aFontName.ToCharArray(true);
  memcpy(@lFontInfo.lfFaceName[0], @lName[0], length(lName) * 2);

  result := rtl.CreateFontIndirect(@lFontInfo);
end;

class constructor TFont;
begin
  var lDC := rtl.GetDC(rtl.HWND(0));
  fPixelsPerInch := rtl.GetDeviceCaps(lDC, rtl.LOGPIXELSY);
  rtl.ReleaseDC(rtl.HWND(0), lDC);

  fDefaultFont := CreateFontFromData('Tahoma', 0, 12, 0, TFontPitch.fpDefault, TFontQuality.fqDefault, []);
end;

end.