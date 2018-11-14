namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

type
  TMessage = public record
    Msg: Cardinal;
    wParam: rtl.WPARAM;
    lParam: rtl.LPARAM;
    &Result: rtl.LRESULT;
    hWnd: rtl.HWND;
    constructor(aHwnd: rtl.HWND; aMsg: Cardinal; aWParam: rtl.WPARAM; aLParam: rtl.LPARAM);
  end;

const
  CN_BASE: Cardinal = 48128;
  CN_COMMAND: Cardinal = CN_BASE + rtl.WM_COMMAND;
  CN_KEYDOWN: Cardinal = CN_BASE + rtl.WM_KEYDOWN;
  CN_NOTIFY: Cardinal = CN_BASE + rtl.WM_NOTIFY;

implementation

constructor TMessage(aHwnd: rtl.HWND; aMsg: Cardinal; aWParam: rtl.WPARAM; aLParam: rtl.LPARAM);
begin
  hWnd := aHwnd;
  Msg := aMsg;
  wParam := aWParam;
  lParam := aLParam;
end;

{$ENDIF}

end.