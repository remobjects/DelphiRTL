﻿namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type

/*TControl = public partial class(TComponent)
protected
  method CreateHandle; virtual; partial; empty;
  method HandleNeeded; virtual; partial; empty;
end;*/

TWinControl = public partial class(TControl)
protected
  method CreateWnd; virtual; empty;
end;

TScrollingWinControl = public partial class(TWinControl)
end;


implementation

end.