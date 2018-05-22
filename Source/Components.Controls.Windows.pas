namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type

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
    method HandleNeeded; override;

    {CreateParams(var Params: TCreateParams); virtual;
    procedure CreateWindowHandle(const Params: TCreateParams); virtual;}
  public
    constructor(aOwner: TComponent);
    property TabOrder: Integer read fTabOrder write fTabOrder;
  end;

  //TScrollingWinControl = public partial class(TWinControl)
  //end;

implementation

method TWinControl.HandleNeeded;
begin
  CreateHandle;
end;

method TWinControl.CreateHandle;
begin
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

end;

method TControl.PlatformSetHeight(aValue: Integer);
begin

end;

method TControl.PlatformSetTop(aValue: Integer);
begin

end;

method TControl.PlatformSetLeft(aValue: Integer);
begin

end;

method TControl.PlatformSetParent(aValue: TControl);
begin

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

end;

method TControl.PlatformSetColor(aValue: TColor);
begin

end;

end.