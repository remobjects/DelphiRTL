namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TApplication = public partial class(TComponent)
  public
    method CreateForm(InstanceClass: TComponentClass; var aFormRef); partial;
    method Initialize; partial;
    method Run; partial;
    method Terminate; partial;
  end;

  TCustomForm = public partial class(TScrollingWinControl)
  protected
    class method FormWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  public
    constructor(aOwner: TComponent);
    method CreateWnd; override;
    method WndProc2(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  end;

implementation

method TApplication.CreateForm(InstanceClass: TComponentClass; var aFormRef);
begin
  var lCtor: MethodInfo;
  var FormRef: TForm := TForm(aFormRef);

  var lCtors := InstanceClass.Methods.Where(a -> ((MethodFlags.Constructor in a.Flags) and (a.Arguments.Count > 0)));
  if lCtors.Count > 1 then begin
    for each lTemp in lCtors do begin
      var lArguments := lTemp.Arguments.ToList;
      if lArguments[0].Type = typeOf(TComponent) then begin
        lCtor := lTemp;
        break;
      end;
    end;
  end
  else
    lCtor := lCtors.FirstOrDefault;

  if lCtor = nil then raise new Exception('No default constructor could be found!');
  var lNew := DefaultGC.New(InstanceClass.RTTI, InstanceClass.SizeOfType);
  FormRef := InternalCalls.Cast<TForm>(lNew);
  lCtor.Invoke(FormRef, [nil]);
  aFormRef := FormRef;
end;

method TApplication.Initialize;
begin

end;

method TApplication.Run;
begin
  var lMsg: rtl.MSG;

  while not Finished do begin
    if rtl.PeekMessageW(@lMsg, nil, 0, 0, rtl.PM_REMOVE) then begin
      rtl.TranslateMessage(@lMsg);
      rtl.DispatchMessage(@lMsg);
    end;

    if lMsg.message = rtl.WM_QUIT then
      Terminate;
  end;
end;

method TApplication.Terminate;
begin
  fFinished := true;
end;

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

{$ENDIF}

end.