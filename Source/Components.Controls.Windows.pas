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
  end;

  TControl = public partial class(TComponent)
  protected
    //constructor(aOwner: TComponent);
    method GetDefaultName: String;

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
  public
  end;

  TWinControl = public partial class(TControl)
  private
    fTabOrder: Integer; // TODO
    fClass: rtl.WNDCLASS;
  protected
    fOldWndProc: TWndProc;
    method CreateHandle; override;

    method CreateParams(var aParams: TCreateParams); virtual;
    method CreateWindowHandle(aParams: TCreateParams); virtual;
    method CreateWnd; virtual;
  public
    constructor(aOwner: TComponent);
    method WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT; virtual;
    property TabOrder: Integer read fTabOrder write fTabOrder;
  end;

  TWndProc = public function(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;

  TScrollingWinControl = public partial class(TWinControl)
  end;

  function GlobalWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;

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
    result := lControl.WndProc(hWnd, message, wParam, lParam);
  end
  else
    result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
end;

method TWinControl.CreateHandle;
begin
  CreateWnd;
end;

constructor TWinControl(aOwner: TComponent);
begin

end;

method TWinControl.WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  if message = rtl.WM_LBUTTONDOWN then begin
    if assigned(OnClick) then
      OnClick(self);
    result := 0;
  end
  else
    result := rtl.CallWindowProc(fOldWndProc, hWnd, message, wParam, lParam);
end;

method TControl.GetDefaultName: String;
begin

end;

method TControl.PlatformSetWidth(aValue: Integer);
begin
  rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, Left, Top, aValue, Width, rtl.SWP_NOMOVE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, Left, Top, Width, aValue, rtl.SWP_NOMOVE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  rtl.SetWindowPos(fHandle, rtl.HWND_NOTOPMOST, Left, aValue, 0, 0, rtl.SWP_NOSIZE or rtl.SWP_NOZORDER);
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
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
  var lMaxLength := rtl.GetWindowTextLength(fHandle);
  var lBuffer := new Char[lMaxLength + 1];
  rtl.GetWindowText(fHandle, @lBuffer[0], lMaxLength);
  result := String.FromPChar(@lBuffer[0]);
end;

method TControl.PlatformSetCaption(aValue: String);
begin
  var lText := aValue.ToCharArray(true);
  rtl.SetWindowText(fHandle, @lText[0]);
end;

method TControl.PlatformSetColor(aValue: TColor);
begin

end;

method TControl.PlatformSetVisible(aValue: Boolean);
begin
  var lShowValue := if aValue then rtl.SW_SHOW else rtl.SW_HIDE;
  rtl.ShowWindow(fHandle, lShowValue);
end;

method TWinControl.CreateParams(var aParams: TCreateParams);
begin
  aParams.Style := aParams.Style or rtl.WS_CHILD or rtl.WS_TABSTOP;
  if Visible then aParams.Style := aParams.Style or rtl.WS_VISIBLE;
  aParams.X := Left;
  aParams.Y := Top;
  aParams.Width := Width;
  aParams.Height := Height;
  aParams.Caption := Caption.ToCharArray(true);
end;

method TWinControl.CreateWindowHandle(aParams: TCreateParams);
begin
  var lParent := if Parent <> nil then Parent.Handle else nil;
  var hInstance := rtl.GetModuleHandle(nil); // TODO

  fHandle := rtl.CreateWindowEx(0, @aParams.WinClassName[0], @aParams.Caption[0], aParams.Style, aParams.X, aParams.Y, aParams.Width, aParams.Height, lParent, nil, hInstance, nil);
  fOldWndProc := InternalCalls.Cast<TWndProc>(^Void(rtl.GetWindowLongPtr(fHandle, rtl.GWL_WNDPROC)));
  rtl.SetWindowLongPtr(fHandle, rtl.GWL_WNDPROC, NativeUInt(^Void(@GlobalWndProc)));
  rtl.SetWindowLongPtr(fHandle, rtl.GWL_USERDATA, NativeUInt(InternalCalls.Cast(self)));
end;

method TWinControl.CreateWnd;
begin
  var lParams: TCreateParams;
  CreateParams(var lParams);
  var lInstance := rtl.GetModuleHandle(nil); // TODO
  var lClass: rtl.WNDCLASS;
  if rtl.GetClassInfo(lInstance, @lParams.WinClassName[0], @lClass) then begin
  end;
  CreateWindowHandle(lParams);
end;

{$ENDIF}

end.