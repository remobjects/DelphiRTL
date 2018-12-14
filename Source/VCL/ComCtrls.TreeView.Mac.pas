namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT (ISLAND AND DARWIN)}

interface

type
  TTreeView = public partial class(TNativeControl)
  private
  assembly
    //fController: TListViewController;
  protected
    method CreateHandle; override;
  public
  end;


implementation

method TTreeView.CreateHandle;
begin
  fHandle := new AppKit.NSOutlineView();
end;

{$ENDIF}

end.