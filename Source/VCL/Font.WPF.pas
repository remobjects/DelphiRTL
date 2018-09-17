namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi, System.Reflection;

type
  TFont = public partial class(TPersistent)
  private
    class var fDevicePixelsPerInch: Integer := 0;
    class constructor;
  public
    constructor;
  end;

implementation

class constructor TFont;
begin
  var dpiYProperty := typeOf(System.Windows.SystemParameters).GetProperty("Dpi", BindingFlags.NonPublic or BindingFlags.Static);

  fDevicePixelsPerInch := Integer(dpiYProperty.GetValue(nil, nil));
end;

constructor TFont;
begin
  fPixelsPerInch := fDevicePixelsPerInch;
end;


{$ENDIF}

end.