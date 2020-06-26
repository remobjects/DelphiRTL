namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TCustomControl)
  protected
    method CreateHandle; override;
  end;

implementation

method TPanel.CreateHandle;
begin
  fHandle := Browser.CreateElement('div');
  fHandle.style.position := "absolute";
end;

{$ENDIF}

end.