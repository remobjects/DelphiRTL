namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

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
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "absolute";
end;

{$ENDIF}

end.