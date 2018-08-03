namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

type
  TMessage = public record
    Msg: Cardinal;
    wParam: rtl.WPARAM;
    lParam: rtl.LPARAM;
    &Result: rtl.LRESULT;
    constructor(aMsg: Cardinal; aWParam: rtl.WPARAM; aLParam: rtl.LPARAM);
  end;

const
  CN_BASE: Cardinal = 48128;
  CN_COMMAND: Cardinal = CN_BASE + rtl.WM_COMMAND;

implementation

constructor TMessage(aMsg: Cardinal; aWParam: rtl.WPARAM; aLParam: rtl.LPARAM);
begin
  Msg := aMsg;
  wParam := aWParam;
  lParam := aLParam;
end;

{$ENDIF}

end.