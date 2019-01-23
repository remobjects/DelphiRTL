namespace RemObjects.Elements.RTL.Delphi.VCL;

// TODO FIX THAT BEFORE MERGING!!!!!!!!!!!!!!!!

//{$IF MACOS AND NOT (ISLAND AND DARWIN)}
{$IF MACOS}

interface

uses
  RemObjects.Elements.RTL.Delphi,
  AppKit;

type
  TTreeNode = public partial class(TPersistent)
  private
    fChilds: TList<TTreeNode>;
  end;

  TTreeNodes = public partial class(TPersistent)
  private
    fNodes: TList<TTreeNode>;
  end;


  TTreeView = public partial class(TNativeControl)
  private
  assembly
    fController: TTreeViewController;
  protected
    method CreateHandle; override;
  public
  end;

  TTreeViewController = class(NSOutlineViewDelegate)
  public

  end;

  TTreeViewDataSource = class(NSOutlineViewDataSource)
  end;

implementation

method TTreeView.CreateHandle;
begin
  fHandle := new NSOutlineView();
  fController := new TTreeviewController();
  (fHandle as NSOutlineView).delegate := fController;
end;

{$ENDIF}

end.