namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class(TScrollingWinControl)
  private
    fActiveControl: TWinControl;
    method SetActiveControl(aValue: TWinControl);
    method AddControls(aControl: TControl; aList: TList<TWinControl>);
  protected
    class method FormWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  public
    constructor(aOwner: TComponent);
    method CreateWnd; override;
    method WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
    method SelectNextControl(aControl: TWinControl; reverseMode: Boolean);
    property ActiveControl: TWinControl read fActiveControl write SetActiveControl;
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
  //rtl.ShowWindow(fHandle, rtl.SW_SHOW);
end;

method TCustomForm.WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  case message of
    rtl.WM_CLOSE: begin
      if Application.MainForm = self then
        Application.Terminate
    end;

    rtl.WM_DPICHANGED: begin
      // This message occurs:
      //  - moving the window to another monitor with different dpi
      //  - changing dpi of current monitor
      //
      // HiWord(WParam): Y-axis new DPI
      // LoWord(WParam): X-axis new DPI. For Windows, both values are identical.
      // LParam: Pointer to a TRect with suggested new window size
      var lNewDPI := wParam shr 16;
      var lNewRect: ^rtl.RECT := ^rtl.RECT(^Void(lParam));
      Top := lNewRect^.top;
      Left := lNewRect^.left;
      Width := lNewRect^.right - lNewRect^.left;
      Height := lNewRect^.bottom - lNewRect^.top;
      fCurrentPPI := lNewDPI;
      if Controls ≠ nil then begin
        for i: Integer := 0 to Controls.Count - 1 do
          Controls[i].ScaleForPPI(lNewDPI);
      end;

      result := 0;
    end;

    CN_KEYDOWN: begin
      if wParam = rtl.VK_TAB then begin
        var lShiftState := GetSpecialKeysStatus;
        SelectNextControl(ActiveControl, TShiftStateValues.ssShift in lShiftState);
        result := 0;
      end
      else
        result := 1;
    end;

    else
      result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
  end;
end;

class method TCustomForm.FormWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  var lObject := rtl.GetWindowLongPtr(hWnd, rtl.GWLP_USERDATA);
  if lObject <> 0 then begin
    var lControl := InternalCalls.Cast<TCustomForm>(^Void(lObject));
    result := lControl.WndProc(hWnd, message, wParam, lParam);
  end
  else
    result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
end;

method TCustomForm.SetActiveControl(aValue: TWinControl);
begin
  fActiveControl := aValue;
end;

method TCustomForm.AddControls(aControl: TControl; aList: TList<TWinControl>);
begin
  for each lControl in aControl.Controls do begin
    aList.Add(TWinControl(lControl));
    AddControls(lControl, aList);
  end;
end;

method TCustomForm.SelectNextControl(aControl: TWinControl; reverseMode: Boolean);
begin
  var lControlList := new TList<TWinControl>();
  AddControls(self, lControlList);
  var lIndex := lControlList.IndexOf(aControl);
  if lIndex >= 0 then begin
    if not reverseMode then begin
      inc(lIndex);
      if lIndex ≥ lControlList.Count then
        lIndex := 0;

      while not lControlList[lIndex].TabStop do begin
        inc(lIndex);
        if lIndex ≥ lControlList.Count then
          lIndex := 0;
      end;
    end
    else begin
      dec(lIndex);
      if lIndex ≤ 0 then
        lIndex := lControlList.Count - 1;

      while not lControlList[lIndex].TabStop do begin
        dec(lIndex);
        if lIndex ≤ 0 then
          lIndex := lControlList.Count;
      end;
    end;
    rtl.SetFocus(lControlList[lIndex].Handle);
    rtl.InvalidateRect(lControlList[lIndex].Handle, nil, true);
  end;
end;

method TForm.Show;
begin
  rtl.ShowWindow(fHandle, rtl.SW_SHOW);
  ScaleForPPI(THighDPI.MonitorPPIForHandle(fHandle));
end;

{$ENDIF}

end.