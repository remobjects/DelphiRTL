namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT MACCATALYST AND NOT (ISLAND AND DARWIN)}

interface

uses
  RemObjects.Elements.RTL.Delphi,
  AppKit;

type
  TTreeNode = public partial class(TPersistent)
  private assembly
    fChilds: TList<TTreeNode>;
  protected
    method PlatformCreate; partial;
    method PlatformSetText(aValue: String); partial;
    method PlatformGetSelected: Boolean; partial;
    //method PlatformGetFocused: Boolean; partial;
    method PlatformGetExpanded: Boolean; partial;
    method PlatformSetSelected(aValue: Boolean); partial;
    //method PlatformSetFocused(aValue: Boolean); partial;
    method PlatformSetExpanded(aValue: Boolean); partial;
  public
  end;

  TTreeNodes = public partial class(TPersistent)
  private assembly
    fNodes: TList<TTreeNode>;
  protected
    method PlatformCreate; partial;
    method PlatformAddChild(aParent: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAddChildFirst(aParent: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAdd(aSibling: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAddFirst(aSibling: TTreeNode; var aNode: TTreeNode); partial;

    method PlatformUpdateNodes;
  end;

  TTreeView = public partial class(TNativeControl)
  private
  assembly
    fDataSource: TTreeViewDataSourceController;
    fTree: NSOutlineView;
  protected
    method CreateHandle; override;
  public
    method UpdateDataModel;
  end;

  TTreeViewDataSourceController = class(INSOutlineViewDataSource, INSOutlineViewDelegate)
  private
    fOwner: TTreeView;
  public
    constructor(aOwner: TTreeView);
    method outlineView(outlineView: not nullable NSOutlineView) child(&index: NSInteger) ofItem(item: nullable id): not nullable id;
    method outlineView(outlineView: not nullable NSOutlineView) numberOfChildrenOfItem(item: nullable id): NSInteger;
    method outlineView(outlineView: not nullable NSOutlineView) isItemExpandable(item: not nullable id): BOOL;
    method outlineView(outlineView: not nullable NSOutlineView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) byItem(item: nullable id): nullable id;
  end;

implementation

method TTreeNode.PlatformCreate;
begin
  fChilds := new TList<TTreeNode>();
end;

method TTreeNode.PlatformSetText(aValue: String);
begin
  fOwner.Owner.UpdateDataModel;
end;

method TTreeNode.PlatformGetExpanded: Boolean;
begin
  result := fOwner.Owner.fTree.isItemExpanded(self);
end;

method TTreeNode.PlatformSetExpanded(aValue: Boolean);
begin
  if aValue then
    fOwner.Owner.fTree.expandItem(self)
  else
    fOwner.Owner.fTree.collapseItem(self);
end;

method TTreeNode.PlatformGetSelected: Boolean;
begin
  result := fOwner.Owner.fTree.isRowSelected(fOwner.Owner.fTree.rowForItem(self));
end;

method TTreeNode.PlatformSetSelected(aValue: Boolean);
begin
  var lRow := fOwner.Owner.fTree.rowForItem(self);
  if aValue then
    fOwner.Owner.fTree.selectRowIndexes(new NSIndexSet withIndex(lRow)) byExtendingSelection(YES)
  else
    fOwner.Owner.fTree.deselectRow(lRow);
end;

method TTreeNodes.PlatformCreate;
begin
  fNodes := new TList<TTreeNode>();
end;

method TTreeNodes.PlatformUpdateNodes;
begin
  // TODO check BeginUpdate...
  fOwner.UpdateDataModel;
end;

method TTreeNodes.PlatformAddChild(aParent: TTreeNode; var aNode: TTreeNode);
begin
  if aParent <> nil then
    aParent.fChilds.Add(aNode)
  else
    fNodes.Add(aNode);
  PlatformUpdateNodes;
end;

method TTreeNodes.PlatformAddChildFirst(aParent: TTreeNode; var aNode: TTreeNode);
begin
  if aParent <> nil then
    aParent.fChilds.Insert(0, aNode)
  else
    fNodes.Insert(0, aNode);
  PlatformUpdateNodes;
end;

method TTreeNodes.PlatformAdd(aSibling: TTreeNode; var aNode: TTreeNode);
begin
  if aSibling <> nil then
    aSibling.Parent.fChilds.Add(aNode)
  else
    fNodes.Add(aNode);
  PlatformUpdateNodes;
end;

method TTreeNodes.PlatformAddFirst(aSibling: TTreeNode; var aNode: TTreeNode);
begin
  if aSibling <> nil then
    aSibling.Parent.fChilds.Insert(0, aNode)
  else
    fNodes.Insert(0, aNode);
  PlatformUpdateNodes;
end;

method TTreeView.CreateHandle;
begin
  fTree := new NSOutlineView();
  fHandle := new NSScrollView();
  fDataSource := new TTreeViewDataSourceController(self);
  fTree.delegate := fDataSource;
  fTree.dataSource := fDataSource;
  (fHandle as NSScrollView).documentView := fTree;

  var lColumn := new NSTableColumn withIdentifier(Name + 'C1');
  fTree.addTableColumn(lColumn);
  fTree.outlineTableColumn := lColumn;
  fTree.headerView := nil;
end;

method TTreeView.UpdateDataModel;
begin
  fTree.reloadData;
end;

constructor TTreeViewDataSourceController(aOwner: TTreeView);
begin
  fOwner := aOwner;
end;

method TTreeViewDataSourceController.outlineView(outlineView: not nullable NSOutlineView) child(&index: NSInteger) ofItem(item: nullable id): not nullable id;
begin
  if item = nil then // root node(s)
    result := fOwner.Items.fNodes[&index] as not nullable
  else begin
    var lNode := item as TTreeNode;
    result := lNode.fChilds[&index] as not nullable;
  end;
end;

method TTreeViewDataSourceController.outlineView(outlineView: not nullable NSOutlineView) numberOfChildrenOfItem(item: nullable id): NSInteger;
begin
  if fOwner.Items = nil then
    exit 0;

  if item = nil then
    result := fOwner.Items.fNodes.Count
  else
    result := (item as TTreeNode).fChilds.Count;
end;

method TTreeViewDataSourceController.outlineView(outlineView: not nullable NSOutlineView) isItemExpandable(item: not nullable id): BOOL;
begin
  result := (item as TTreeNode).fChilds.Count > 0;
end;

method TTreeViewDataSourceController.outlineView(outlineView: not nullable NSOutlineView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) byItem(item: nullable id): nullable id;
begin
  if tableColumn = nil then
    exit item;

  if item <> nil then
    result := (item as TTreeNode).Text;
end;
{$ENDIF}

end.