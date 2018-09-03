namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND (WEBASSEMBLY OR WINDOWS) OR ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TNativeControl)
  protected
  end;

implementation

{$ENDIF}

end.