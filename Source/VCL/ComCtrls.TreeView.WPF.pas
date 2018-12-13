namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi, System.Windows.Controls;

type
  TTreeNodes = public partial class(TPersistent)
  protected
    method InternalInsertNode(asChild: Boolean; asFirst: Boolean; aParentOrSibling: TTreeNode; var aNode: TTreeNode);

    method PlatformAddChild(aParent: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAddChildFirst(aParent: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAdd(aSibling: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAddFirst(aSibling: TTreeNode; var aNode: TTreeNode); partial;
  end;

  TTreeView = public partial class(TNativeControl)
  private assembly
  protected
    method CreateHandle; override;
  public
  end;


implementation

method TTreeView.CreateHandle;
begin
  fHandle := new TreeView();
end;

method TTreeNodes.InternalInsertNode(asChild: Boolean; asFirst: Boolean; aParentOrSibling: TTreeNode; var aNode: TTreeNode);
begin
  var lItem := new System.Windows.Controls.TreeViewItem(Header := aNode.Text);
  if asChild then begin
    if asFirst then begin
      if aParentOrSibling = nil then
        (fOwner.Handle as TreeView).Items.Insert(0, lItem)
      else
        aParentOrSibling.ItemId.Items.Insert(0, lItem);
    end
    else begin
      if aParentOrSibling = nil then
        (fOwner.Handle as TreeView).Items.Add(lItem)
      else
        aParentOrSibling.ItemId.Items.Add(lItem);
    end;
  end
  else begin
    if asFirst then begin
      if aParentOrSibling = nil then
        (fOwner.Handle as TreeView).Items.Insert(0, lItem)
      else
        aParentOrSibling.ItemId.Items.Insert(0, lItem);
    end
    else begin
      if aParentOrSibling = nil then
        (fOwner.Handle as TreeView).Items.Add(lItem)
      else
        aParentOrSibling.ItemId.Items.Add(lItem);
    end;
  end;
  aNode.fItemId := lItem;
end;

method TTreeNodes.PlatformAddChild(aParent: TTreeNode; var aNode: TTreeNode);
begin
  InternalInsertNode(true, false, aParent, var aNode);
end;

method TTreeNodes.PlatformAddChildFirst(aParent: TTreeNode; var aNode: TTreeNode);
begin
  InternalInsertNode(true, true, aParent, var aNode);
end;

method TTreeNodes.PlatformAdd(aSibling: TTreeNode; var aNode: TTreeNode);
begin
  InternalInsertNode(false, false, aSibling, var aNode);
end;

method TTreeNodes.PlatformAddFirst(aSibling: TTreeNode; var aNode: TTreeNode);
begin
  InternalInsertNode(false, true, aSibling, var aNode);
end;

{$ENDIF}

end.