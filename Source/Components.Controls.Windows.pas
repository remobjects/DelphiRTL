namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL.Delphi, rtl;

type
  TCreateParams = public record
    Caption: array of Char;
    Style: rtl.DWORD;
    ExStyle: rtl.DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: rtl.HWND;
    &Param: Pointer;
    WindowClass: rtl.WNDCLASS;
    WinClassName: array of Char;
    WidgetClassName: array of Char;
    DefaultWndProc: Boolean := false;
  end;

  TWndMethod = public method(var aMessage: TMessage);

  TControl = public partial class(TComponent)
  protected
    fWindowProc: TWndMethod;
    //constructor(aOwner: TComponent);
    method GetDefaultName: String;
    method HandleAllocated: Boolean;

    method PlatformGetCaption: String; partial;
    method PlatformSetCaption(aValue: String); partial;
    method PlatformSetWidth(aValue: Integer); partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetColor(aValue: TColor); virtual; partial;
    method PlatformSetVisible(aValue: Boolean); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;

    method DefaultHandler(var aMessage: TMessage); virtual;

  public
    method WndProc(var aMessage: TMessage); virtual;
    method Perform(aMessage: Cardinal; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
    method Perform(var aMessage: TMessage);
    property WindowProc: TWndMethod read fWindowProc write fWindowProc;
  end;

  TGraphicControl = public class(TControl)
  protected
    method Paint; virtual; empty;
  public
    //method WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT; virtual;
  end;

  TNativeControl = public partial class(TControl)
  private
    fTabOrder: Integer; // TODO
    fClass: rtl.WNDCLASS;
  protected
    fOldWndProc: TWndProc;
    method ControlFromHandle(aHandle: rtl.HWND): TNativeControl;

    method CreateHandle; override;
    method CreateClass(var aParams: TCreateParams);

    method CreateParams(var aParams: TCreateParams); virtual;
    method CreateWindowHandle(aParams: TCreateParams); virtual;
    method CreateWnd; virtual;

    method DefaultHandler(var aMessage: TMessage); override;

  public
    method WndProc(var aMessage: TMessage); override;
    constructor(aOwner: TComponent);
    //method WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT; virtual;
    property TabOrder: Integer read fTabOrder write fTabOrder;
  end;

  TWinControl = public TNativeControl;

  TWndProc = public function(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;

  TScrollingWinControl = public partial class(TWinControl)
  end;

  function GlobalWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  function GlobalGraphicControlWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;

  procedure PlatformShowMessage(aMessage: String);

implementation

procedure PlatformShowMessage(aMessage: String);
begin
  var lMessage := aMessage.ToCharArray(true);
  rtl.MessageBoxW(nil, @lMessage[0], nil, 0);
end;

function GlobalWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  var lObject := rtl.GetWindowLongPtr(hWnd, rtl.GWLP_USERDATA);
  if lObject <> 0 then begin
    var lControl := InternalCalls.Cast<TWinControl>(^Void(lObject));
    var lMessage := new TMessage(message, wParam, lParam);
    lControl.WndProc(var lMessage);
    result := lMessage.Result;
  end
  else begin
    result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
  end;
end;

function GlobalGraphicControlWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin

end;

method TNativeControl.CreateHandle;
begin
  CreateWnd;
end;

constructor TNativeControl(aOwner: TComponent);
begin
  fHandle := rtl.HWND(0);
end;

method TNativeControl.WndProc(var aMessage: TMessage);
begin
  case aMessage.Msg of
    rtl.WM_COMMAND: begin
      // HiWord(WParam): Notification code
      // LoWord(WParam): Controld ID
      // LParam: Target Window Handle
      var lNotification := aMessage.wParam shr 16;
      var lControl := ControlFromHandle(rtl.HWND(aMessage.lParam));
      aMessage.Result := lControl.Perform(lNotification, aMessage.wParam, aMessage.lParam);
    end;

    rtl.BN_CLICKED: begin
      // we got this message from our parent via WM_COMMAND
      Click;
      aMessage.Result := 0;
    end;

    else
      inherited(var aMessage);
  end;
end;

method TNativeControl.ControlFromHandle(aHandle: rtl.HWND): TNativeControl;
begin
  var lObject := rtl.GetWindowLongPtr(aHandle, rtl.GWLP_USERDATA);
  if lObject <> 0 then
    result := InternalCalls.Cast<TWinControl>(^Void(lObject))
  else
    result := nil;
end;

method TControl.GetDefaultName: String;
begin

end;

method TControl.PlatformSetWidth(aValue: Integer);
begin
  if HandleAllocated then
    rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, Left, Top, aValue, Width, rtl.SWP_NOMOVE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  if HandleAllocated then
    rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, Left, Top, Width, aValue, rtl.SWP_NOMOVE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  if HandleAllocated then
    rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, Left, aValue, 0, 0, rtl.SWP_NOSIZE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
  if HandleAllocated then
    rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, aValue, Top, 0, 0, rtl.SWP_NOSIZE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetParent(aValue: TControl);
begin
  HandleNeeded; // TODO
end;

method TControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin

end;

method TControl.PlatformSetOnKeyPress(aValue: TKeyPressEvent);
begin

end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin

end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin

end;

method TControl.PlatformGetCaption: String;
begin
  if HandleAllocated then begin
    var lMaxLength := rtl.GetWindowTextLength(fHandle);
    var lBuffer := new Char[lMaxLength + 1];
    rtl.GetWindowText(fHandle, @lBuffer[0], lMaxLength);
    result := String.FromPChar(@lBuffer[0]);
  end
  else
    result := fCaption;
end;

method TControl.PlatformSetCaption(aValue: String);
begin
  if HandleAllocated then begin
    var lText := aValue.ToCharArray(true);
    rtl.SetWindowText(fHandle, @lText[0]);
  end;
end;

method TControl.PlatformSetColor(aValue: TColor);
begin

end;

method TControl.PlatformSetVisible(aValue: Boolean);
begin
  if HandleAllocated then begin
    var lShowValue := if aValue then rtl.SW_SHOW else rtl.SW_HIDE;
    rtl.ShowWindow(fHandle, lShowValue);
  end;
end;

method TControl.Perform(aMessage: Cardinal; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  var lMessage := new TMessage(aMessage, wParam, lParam);
  WndProc(var lMessage);
  result := lMessage.Result;
end;

method TControl.Perform(var aMessage: TMessage);
begin
  WndProc(var aMessage);
end;

method TControl.WndProc(var aMessage: TMessage);
begin
  // Dispatch messages here to message WM_XXXX functions
  DefaultHandler(var aMessage);
end;

method TControl.DefaultHandler(var aMessage: TMessage);
begin

end;

method TNativeControl.CreateParams(var aParams: TCreateParams);
begin
  memset(@aParams, 0, sizeOf(aParams));
  aParams.Style := aParams.Style or rtl.WS_CHILD;
  if Visible then aParams.Style := aParams.Style or rtl.WS_VISIBLE;
  aParams.X := Left;
  aParams.Y := Top;
  aParams.Width := Width;
  aParams.Height := Height;
  var lCaption := 'Caption';
  aParams.Caption := lCaption.ToCharArray(true);
end;

method TNativeControl.CreateWindowHandle(aParams: TCreateParams);
begin
  var lParent := if Parent <> nil then Parent.Handle else nil;
  var hInstance := rtl.GetModuleHandle(nil); // TODO

  fHandle := rtl.CreateWindowEx(aParams.ExStyle, @aParams.WinClassName[0], @aParams.Caption[0], aParams.Style, aParams.X, aParams.Y, aParams.Width, aParams.Height, lParent, nil, hInstance, nil);
  rtl.SetWindowLongPtr(fHandle, rtl.GWL_USERDATA, NativeUInt(InternalCalls.Cast(self)));
  fOldWndProc := InternalCalls.Cast<TWndProc>(^Void(rtl.GetWindowLongPtr(fHandle, rtl.GWL_WNDPROC)));
  if not aParams.DefaultWndProc then
    rtl.SetWindowLongPtr(fHandle, rtl.GWL_WNDPROC, NativeUInt(^Void(@GlobalWndProc)));
end;

method TNativeControl.CreateWnd;
begin
  var lParams: TCreateParams;
  CreateParams(var lParams);
  var lInstance := rtl.GetModuleHandle(nil); // TODO

  var lType := typeOf(self);
  var lClassName := lType.Name.Substring(lType.Name.LastIndexOf('.') + 1);
  lParams.WinClassName := lClassName.ToCharArray(true);
  var lClass: rtl.WNDCLASS;
  if not rtl.GetClassInfo(lInstance, @lParams.WinClassName[0], @lClass) then begin
    //lParams.WindowClass.lpfnWndProc := @GlobalWndProc;
    //lParams.WindowClass.lpfnWndProc := @rtl.DefWindowProc;
    lParams.WindowClass.lpszClassName := @lParams.WinClassName[0];
    rtl.RegisterClass(@lParams.WindowClass);
  end
  else
    lParams.WindowClass := lClass;
  CreateWindowHandle(lParams);
end;

method TNativeControl.CreateClass(var aParams: TCreateParams);
begin
  var lInstance := rtl.GetModuleHandle(nil); // TODO
  rtl.GetClassInfo(lInstance, @aParams.WidgetClassName[0], @aParams.WindowClass);

  aParams.WindowClass.hInstance := lInstance;
end;

method TNativeControl.DefaultHandler(var aMessage: TMessage);
begin
  aMessage.Result := rtl.CallWindowProc(fOldWndProc, fHandle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;

method TControl.HandleAllocated: Boolean;
begin
  result := fHandle <> rtl.HWND(0);
end;

/*method TGraphicControl.WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): LRESULT;
begin
  if message = rtl.WM_PAINT then begin
    Paint;
    result := 0;
  end
  else
    result := rtl.CallWindowProc(fOldWndProc, hWnd, message, wParam, lParam);
end;*/

{$ENDIF}

end.