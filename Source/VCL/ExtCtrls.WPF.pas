namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
  end;

implementation

method TPanel.CreateHandle;
begin

end;

{$ENDIF}

end.