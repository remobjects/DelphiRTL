namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  RemObjects.Elements.RTL.Delphi, System.Windows.Controls;

type
  TTreeNode = public partial class(TPersistent)
  protected
    method PlatformSetText(aValue: VCLString); partial;
    method PlatformGetSelected: Boolean; partial;
    method PlatformGetFocused: Boolean; partial;
    method PlatformGetExpanded: Boolean; partial;
    method PlatformSetSelected(aValue: Boolean); partial;
    method PlatformSetFocused(aValue: Boolean); partial;
    method PlatformSetExpanded(aValue: Boolean); partial;
  end;

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

method TTreeNode.PlatformSetText(aValue: VCLString);
begin
  fItemId.Header := aValue;
end;

method TTreeNode.PlatformGetSelected: Boolean;
begin
  result := fItemId.IsSelected;
end;

method TTreeNode.PlatformSetSelected(aValue: Boolean);
begin
  fItemId.IsSelected := aValue;
end;

method TTreeNode.PlatformGetFocused: Boolean;
begin
  result := fItemId.IsFocused;
end;

method TTreeNode.PlatformSetFocused(aValue: Boolean);
begin
  //fItemId.IsFocused := aValue;
end;

method TTreeNode.PlatformSetExpanded(aValue: Boolean);
begin
  fItemId.IsExpanded := aValue;
end;

method TTreeNode.PlatformGetExpanded: Boolean;
begin
  result := fItemId.IsExpanded;
end;

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