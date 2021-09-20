namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS OR (LINUX AND NOT ANDROID))) OR ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TCustomControl)
  protected
  end;

implementation

{$ENDIF}

end.