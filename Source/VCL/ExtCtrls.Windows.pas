namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TCustomControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

implementation

method TPanel.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  //aParams.Style := aParams.Style
  aParams.WidgetClassName := 'WindowClassPanel'.ToCharArray(true);
  aParams.DefaultWndProc := true;
  CreateClass(var aParams);
  aParams.DefaultWndProc := true;
  aParams.WindowClass.lpfnWndProc := @DefaultControlWndProc;
end;

{$ENDIF}

end.