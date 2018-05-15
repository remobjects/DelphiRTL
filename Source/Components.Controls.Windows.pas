namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type

  TControl = public partial class(TComponent)
  protected
    method HandleNeeded; virtual; partial;
    constructor(aOwner: TComponent);
  end;

  TWinControl = public partial class(TControl)
  protected
    method CreateHandle; override;
    {procedure CreateParams(var Params: TCreateParams); virtual;
    procedure CreateWindowHandle(const Params: TCreateParams); virtual;}
  public
    constructor(aOwner: TComponent);
  end;

  TScrollingWinControl = public partial class(TWinControl)
  end;

implementation

method TControl.HandleNeeded;
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

constructor TControl(aOwner: TComponent);
begin

end;

end.