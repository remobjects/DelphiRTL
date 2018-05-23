namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TCreateParams = public record
  end;

  TControl = public partial class(TComponent)
  protected
    method GetDefaultName: String;
    //constructor(aOwner: TComponent);

    method PlatformGetCaption: String; partial;
    method PlatformSetCaption(aValue: String); partial;
    method PlatformSetWidth(aValue: Integer); partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetColor(aValue: TColor); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;
  end;

  TWinControl = public partial class(TControl)
  private
    fTabOrder: Integer; // TODO
  protected
    method CreateHandle; override;

    method CreateParams(var aParams: TCreateParams); virtual;
    method CreateWindowHandle(aParams: TCreateParams); virtual;
    method CreateWnd; virtual;
  public
    constructor(aOwner: TComponent);
    property TabOrder: Integer read fTabOrder write fTabOrder;
  end;

  //TScrollingWinControl = public partial class(TWinControl)
  //end;

implementation

method TWinControl.CreateHandle;
begin
  writeLn('TWinControl.CreateHandle!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
  CreateWnd;
end;

constructor TWinControl(aOwner: TComponent);
begin

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

end;

method TControl.PlatformSetCaption(aValue: String);
begin
  //var lText := aValue.ToCharArray(true);
  //rtl.SetWindowText(fHandle, @lText[0]);
end;

method TControl.PlatformSetColor(aValue: TColor);
begin

end;

method TWinControl.CreateParams(var aParams: TCreateParams);
begin

end;

method TWinControl.CreateWindowHandle(aParams: TCreateParams);
begin

end;

method TWinControl.CreateWnd;
begin
  var lParams: TCreateParams;
  CreateWindowHandle(lParams);
end;

end.