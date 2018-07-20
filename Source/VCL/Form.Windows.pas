namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

type
  TCustomForm = public partial class(TScrollingWinControl)
  protected
    class method FormWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  public
    constructor(aOwner: TComponent);
    method CreateWnd; override;
    method WndProc2(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  end;

  TForm = public partial class(TCustomForm)
  public
    //constructor(aOwner: TComponent); empty;
    method Show; override;
  end;


implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  var lStream := new TResourceStream(0, lName, rtl.PCHAR(^void(rtl.RT_RCDATA)));
  lStream.Position := 0;
  var lReader := new TReader(lStream, 100);
  lReader.ReadRootComponent(self);
end;

method TCustomForm.CreateWnd;
begin
  // this struct holds information for the window class
  var wc: rtl.WNDCLASSEX;
  var hInstance := rtl.GetModuleHandle(nil);

  // clear out the window class for use
  memset(@wc, 0, sizeOf(rtl.WNDCLASSEX));

  // fill in the struct with the needed information
  wc.cbSize := sizeOf(rtl.WNDCLASSEX);
  wc.style := rtl.CS_HREDRAW or rtl.CS_VREDRAW;
  wc.lpfnWndProc := @FormWndProc;
  wc.hInstance := hInstance;

  //wc.hCursor = rtl.LoadCursor(nil, rtl.IDC_ARROW.);
  wc.hbrBackground := rtl.HBRUSH(rtl.COLOR_WINDOW);
  var lString: RemObjects.Elements.System.String := 'WindowClass1';
  var lArray := lString.ToCharArray(true);
  wc.lpszClassName := @lArray[0];
  var lTitleArray := 'Test'.ToCharArray(true);

  // register the window class
  rtl.RegisterClassEx(@wc);

  // create the window and use the result as the handle
  fHandle := rtl.CreateWindowEx(0,
    @lArray[0],    // name of the window class
    @lTitleArray[0],   // title of the window
    rtl.WS_OVERLAPPEDWINDOW,    // window style
    300,    // x-position of the window
    300,    // y-position of the window
    500,    // width of the window
    400,    // height of the window
    nil,    // we have no parent window, NULL
    nil,    // we aren't using menus, NULL
    hInstance,    // application handle
  nil);    // used with multiple windows, NULL

  rtl.SetWindowLongPtr(fHandle, rtl.GWLP_USERDATA, NativeUInt(InternalCalls.Cast(self)));
  fOldWndProc := InternalCalls.Cast<TWndProc>(^Void(rtl.GetWindowLongPtr(fHandle, rtl.GWL_WNDPROC)));
  rtl.SetWindowLongPtr(fHandle, rtl.GWL_WNDPROC, NativeUInt(^Void(@GlobalWndProc)));

  // display the window on the screen
  rtl.ShowWindow(fHandle, rtl.SW_SHOW);
end;

method TCustomForm.WndProc2(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
end;

class method TCustomForm.FormWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  var lObject := rtl.GetWindowLongPtr(hWnd, rtl.GWLP_USERDATA);
  if lObject <> 0 then begin
    var lControl := InternalCalls.Cast<TCustomForm>(^Void(lObject));
    result := lControl.WndProc2(hWnd, message, wParam, lParam);
  end
  else
    result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
end;

method TForm.Show;
begin

end;

{$ENDIF}

end.