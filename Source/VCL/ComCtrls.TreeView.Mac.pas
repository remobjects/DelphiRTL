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
  private assembly
    fChilds: TList<TTreeNode>;
  protected
    method PlatformCreate; partial;
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
    fController: TTreeViewController;
    fDataSource: TTreeViewDataSource;
    fTree: NSOutlineView;
  protected
    method CreateHandle; override;
  public
    method UpdateDataModel;
  end;

  TTreeViewController = class(NSOutlineViewDelegate)
  private
    fOwner: TTreeView;
  public
    constructor(aOwner: TTreeView);
    //method outlineView(outlineView: not nullable NSOutlineView) viewForTableColumn(tableColumn: nullable NSTableColumn) item(item: not nullable id): nullable NSView;
    method outlineView(outlineView: not nullable NSOutlineView) isGroupItem(item: not nullable id): BOOL;
  end;

  TTreeViewDataSource = class(NSOutlineViewDataSource)
  private
    fOwner: TTreeView;
  public
    constructor(aOwner: TTreeView);
    method outlineView(outlineView: not nullable NSOutlineView) child(&index: NSInteger) ofItem(item: nullable id): not nullable id;
    method outlineView(outlineView: not nullable NSOutlineView) numberOfChildrenOfItem(item: nullable id): NSInteger;
    method outlineView(outlineView: not nullable NSOutlineView) isItemExpandable(item: not nullable id): BOOL;
    method outlineView(outlineView: not nullable NSOutlineView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) byItem(item: nullable id): nullable id;
    method outlineView(outlineView: not nullable NSOutlineView) setObjectValue(object: nullable id) forTableColumn(tableColumn: nullable NSTableColumn) byItem(item: nullable id);
  end;

implementation

method TTreeNode.PlatformCreate;
begin
  fChilds := new TList<TTreeNode>();
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
  fDataSource := new TTreeViewDataSource(self);
  fController := new TTreeViewController(self);
  fTree.delegate := fController;
  fTree.dataSource := fDataSource;
  (fHandle as NSScrollView).documentView := fTree;

  var lColumn := new NSTableColumn withIdentifier(Name + 'C1');
  fTree.addTableColumn(lColumn);
end;

method TTreeView.UpdateDataModel;
begin
  fTree.reloadData;
end;

constructor TTreeViewController(aOwner: TTreeView);
begin
  fOwner := aOwner;
end;

{method TTreeViewController.outlineView(outlineView: not nullable NSOutlineView) viewForTableColumn(tableColumn: nullable NSTableColumn) item(item: not nullable id): nullable NSView;
begin
  writeLn('Cell 1');
  var lCell := fOwner.fTree.makeViewWithIdentifier(fOwner.Name + "_Cell") owner(fOwner.fTree) as NSTableCellView;
  var lTextField := lCell.textField;
  lTextField.stringValue := (item as TTreeNode).Text;
  result := lCell;
  writeLn('Cell 2');
end;}

method TTreeViewController.outlineView(outlineView: not nullable NSOutlineView) isGroupItem(item: not nullable id): BOOL;
begin
  result := false;
end;

constructor TTreeViewDataSource(aOwner: TTreeView);
begin
  fOwner := aOwner;
end;

method TTreeViewDataSource.outlineView(outlineView: not nullable NSOutlineView) child(&index: NSInteger) ofItem(item: nullable id): not nullable id;
begin
  writeLn('Here?');
  if item = nil then // root node(s)
    result := fOwner.Items.fNodes[&index] as not nullable
  else begin
    var lNode := item as TTreeNode;
    result := lNode.fChilds[&index] as not nullable;
  end;
  writeLn('Child Of Item');
  writeLn(result);
end;

method TTreeViewDataSource.outlineView(outlineView: not nullable NSOutlineView) numberOfChildrenOfItem(item: nullable id): NSInteger;
begin
  if fOwner.Items = nil then begin
    writeLn('Not assigned!!');
    exit 0;
  end;

  if item = nil then
    result := fOwner.Items.fNodes.Count
  else begin
    writeLn('Childs 1');
    var lNode := item as TTreeNode;
    result := lNode.fChilds.Count;
    writeLn('Childs 2');
  end;
  writeLn('NumberOfChildrenOfItem');
  writeLn(result);
end;

method TTreeViewDataSource.outlineView(outlineView: not nullable NSOutlineView) isItemExpandable(item: not nullable id): BOOL;
begin
  writeLn('IsExpandable');
  result := (item as TTreeNode).fChilds.Count > 0;
end;

method TTreeViewDataSource.outlineView(outlineView: not nullable NSOutlineView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) byItem(item: nullable id): nullable id;
begin
  //result := item;
  writeLn('objectvalueforTableColumn');
  if item <> nil then
    result := (item as TTreeNode).Text;
end;

method TTreeViewDataSource.outlineView(outlineView: not nullable NSOutlineView) setObjectValue(object: nullable id) forTableColumn(tableColumn: nullable NSTableColumn) byItem(item: nullable id);
begin
  object := item;
end;
{$ENDIF}

end.