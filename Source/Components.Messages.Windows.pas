namespace RemObjects.Elements.RTL.Delphi;

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

implementation

constructor TMessage(aMsg: Cardinal; aWParam: rtl.WPARAM; aLParam: rtl.LPARAM);
begin
  Msg := aMsg;
  wParam := aWParam;
  lParam := aLParam;
end;

{$ENDIF}

end.