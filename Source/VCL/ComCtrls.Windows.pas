namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TListView = public partial class(TMultiSelectListControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

implementation

method TListView.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);

  {var lInit: rtl.INITCOMMONCONTROLSEX;
  lInit.dwICC := rtl.ICC_LISTVIEW_CLASSES;
  rtl.InitCommonControlsEx(@lInit);}
  rtl.InitCommonControls;

  aParams.WidgetClassName := rtl.WC_LISTVIEW.ToCharArray(true);
  CreateClass(var aParams);
end;

{$ENDIF}

end.