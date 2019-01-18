namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

type
  [CallingConvention(CallingConvention.Stdcall)]
  TSetTheme = function(hwnd: rtl.HWND; pszSubAppName: rtl.LPCWSTR; pszSubIdList: rtl.LPCWSTR): rtl.HRESULT;

  TStyleThemes = public static class
  private
    class var fThemeModule: rtl.HMODULE;
    class var fThemeProc: TSetTheme;
    class constructor;
  public
    class method ThemeControl(aHandle: rtl.HWND; aTheme: String := 'explorer');
  end;

implementation

class constructor TStyleThemes;
begin
  fThemeModule := rtl.LoadLibrary('uxtheme.dll');
  fThemeProc := TSetTheme(rtl.GetProcAddress(fThemeModule, 'SetWindowTheme'));
end;

method TStyleThemes.ThemeControl(aHandle: rtl.HWND; aTheme: String := 'explorer');
begin
  fThemeProc(aHandle, aTheme, nil);
end;

{$ENDIF}

end.