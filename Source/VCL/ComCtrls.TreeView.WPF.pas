namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

type
  TTreeView = public partial class(TNativeControl)
  private assembly
  protected
    method CreateHandle; override;
  public
  end;


implementation

method TTreeView.CreateHandle;
begin
  fHandle := new System.Windows.Controls.TreeView();
end;

{$ENDIF}

end.