namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TNativeControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

implementation

method TPanel.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'STATIC'.ToCharArray(true);
  aParams.DefaultWndProc := true;
  CreateClass(var aParams);
end;

{$ENDIF}

end.