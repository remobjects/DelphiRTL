namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

type
  THighDPISupportLevel = public enum(NoHighDPI, Basic, MultiMonitor, MultiMonitorV2) of Integer;

  [CallingConvention(CallingConvention.Stdcall)]
  TSetProcessDPIAwareFunc = public function: rtl.BOOL;

  [CallingConvention(CallingConvention.Stdcall)]
  TSetProcessDPIAwarenessFunc = public function(aValue: rtl.PROCESS_DPI_AWARENESS): rtl.HRESULT;

  [CallingConvention(CallingConvention.Stdcall)]
  TSetProcessDPIAwarenessContextFunc = public function(aValue: rtl.DPI_AWARENESS_CONTEXT): rtl.BOOL;

  [CallingConvention(CallingConvention.Stdcall)]
  TGetDPIForMonitorFunc = public function(aMonitor: rtl.HMONITOR; dpiType: rtl.MONITOR_DPI_TYPE; dpiX: ^rtl.UINT; dpiY: ^rtl.UINT): rtl.HRESULT;

  THighDPI = public static class
  private
    fMainMonitorPPI: Integer;
    fSupportLevel: THighDPISupportLevel;
    fSetProcessDPIAware: TSetProcessDPIAwareFunc;
    fSetProcessDPIAwareness: TSetProcessDPIAwarenessFunc;
    fSetProcessDPIAwarenessContext: TSetProcessDPIAwarenessContextFunc;
    fGetDPIForMonitor: TGetDPIForMonitorFunc;
    method GetFuncAddress(aName: String; aLib: rtl.HMODULE): ^Void;
    method SetupRequiredFunctions;
  public
    method Initialize;
    method MonitorPPIForHandle(aValue: rtl.HWND): Integer;
    property MainMonitorPPI: Integer read fMainMonitorPPI;
    property SupportLevel: THighDPISupportLevel read fSupportLevel;
    property SetProcessDPIAware: TSetProcessDPIAwareFunc read fSetProcessDPIAware;
    property SetProcessDPIAwareness: TSetProcessDPIAwarenessFunc read fSetProcessDPIAwareness;
    property SetProcessDPIAwarenessContext: TSetProcessDPIAwarenessContextFunc read fSetProcessDPIAwarenessContext;
    property GetDPIForMonitor: TGetDPIForMonitorFunc read fGetDPIForMonitor;
  end;

implementation

method THighDPI.GetFuncAddress(aName: String; aLib: rtl.HMODULE): ^Void;
begin
  var lBytes := aName.ToAnsiChars(true);
  result := ^Void(rtl.GetProcAddress(aLib, @lBytes[0]));
end;

method THighDPI.SetupRequiredFunctions;
begin
  var lUserLibName := 'User32.dll'.ToCharArray(true);
  var lUserLib := rtl.LoadLibrary(@lUserLibName[0]);

  if TOSVersion.Check(6, 0) then
    fSetProcessDPIAware := TSetProcessDPIAwareFunc(GetFuncAddress('SetProcessDpiAware', lUserLib));

  if TOSVersion.Check(6, 3) then begin
    var lShLibName := 'Shcore.dll'.ToCharArray(true);
    var lShLib := rtl.LoadLibrary(@lShLibName[0]);
    fSetProcessDPIAwareness := TSetProcessDPIAwarenessFunc(GetFuncAddress('SetProcessDpiAwareness', lShLib));
    fGetDPIForMonitor := TGetDPIForMonitorFunc(GetFuncAddress('GetDpiForMonitor', lShLib));
    rtl.FreeLibrary(lShLib);
  end;

  if TOSVersion.Check(10, 0) and (TOSVersion.Build >= 15063) then
    fSetProcessDPIAwarenessContext := TSetProcessDPIAwarenessContextFunc(GetFuncAddress('SetProcessDpiAwarenessContext', lUserLib));

  rtl.FreeLibrary(lUserLib);
end;

method THighDPI.Initialize;
begin
  var lDC := rtl.GetDC(rtl.HWND(0));
  fMainMonitorPPI := rtl.GetDeviceCaps(lDC, rtl.LOGPIXELSY);
  rtl.ReleaseDC(rtl.HWND(0), lDC);
  SetupRequiredFunctions;

  if TOSVersion.Check(10, 0) and (TOSVersion.Build >= 15063) then begin
    // Windows 10 Creators Update+, improved DPI support
    fSupportLevel := THighDPISupportLevel.MultiMonitorV2;
    THighDPI.SetProcessDPIAwarenessContext(rtl.DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
  end
  else
    if TOSVersion.Check(6, 3) then begin
      // Windows 8.1+, basic multimonitor support
      fSupportLevel := THighDPISupportLevel.MultiMonitor;
      THighDPI.SetProcessDPIAwareness(rtl.PROCESS_DPI_AWARENESS.PROCESS_PER_MONITOR_DPI_AWARE);
    end
    else
      if TOSVersion.Check(6, 0) then begin
        //Windows Vista, basic HighDPI support, no multimonitor
        fSupportLevel := THighDPISupportLevel.Basic;
        THighDPI.SetProcessDPIAware;
      end
      else
        fSupportLevel := THighDPISupportLevel.NoHighDPI;
end;

method THighDPI.MonitorPPIForHandle(aValue: rtl.HWND): Integer;
begin
  if SupportLevel > THighDPISupportLevel.Basic then begin
    var lMonitor := rtl.MonitorFromWindow(aValue, rtl.MONITOR_DEFAULTTONEAREST);
    var lX, lY: rtl.UINT;
    rtl.GetDpiForMonitor(lMonitor, rtl.MONITOR_DPI_TYPE.MDT_DEFAULT, @lX, @lY);
    result := lY;
  end
  else
    result := THighDPI.MainMonitorPPI;
end;

{$ENDIF}

end.