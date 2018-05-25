namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TButton = public partial class(TWinControl)
  protected
    method CreateWindowHandle(aParams: TCreateParams); override;
  end;

implementation

method TButton.CreateWindowHandle(aParams: TCreateParams);
begin
  var lArray := 'BUTTON'.ToCharArray(true);
  var lCaption := 'Button1'.ToCharArray(true);
  var lParent := if Parent <> nil then Parent.Handle else nil;
  var hInstance := rtl.GetModuleHandle(nil); // TODO

  fHandle := rtl.CreateWindowEx(0, @lArray[0], @lCaption[0], rtl.WS_CHILD or rtl.WS_TABSTOP, Left, Top, 150, 60, lParent, nil, hInstance, nil);
  fOldWndProc := InternalCalls.Cast<TWndProc>(^Void(rtl.GetWindowLongPtr(fHandle, rtl.GWL_WNDPROC)));
  rtl.SetWindowLongPtr(fHandle, rtl.GWL_WNDPROC, NativeUInt(^Void(@GlobalWndProc)));
  rtl.SetWindowLongPtr(fHandle, rtl.GWL_USERDATA, NativeUInt(InternalCalls.Cast(self)));

  // display the window on the screen
  rtl.ShowWindow(fHandle, rtl.SW_SHOW);
end;


end.