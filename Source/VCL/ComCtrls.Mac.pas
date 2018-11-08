namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT (ISLAND AND DARWIN)}

interface

uses
  AppKit, RemObjects.Elements.RTL.Delphi;

type
  TListView = public partial class(TMultiSelectListControl)
  private
    fController: TListViewController;
    fTable: NSTableView;
  protected
    method CreateHandle; override;
  end;

  TListViewController = class(INSTableViewDataSource, INSTableViewDelegate)
  private
    fOwnerList: TListView;
  public
    method numberOfRowsInTableView(tableView: not nullable NSTableView): NSInteger;
  //[Optional] method tableView(tableView: not nullable NSTableView) setObjectValue(object: nullable id) forTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger);
    method tableView(tableView: not nullable NSTableView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger): nullable id;
    constructor(aOwner: TListView);
  end;

implementation

method TListView.CreateHandle;
begin
  fTable := new NSTableView();
  UnderlyingHandle := fTable;
  fHandle := new NSScrollView;
  //var lColumn := new NSTableColumn withIdentifier(Name + 'C1');
  //fTable.addTableColumn(lColumn);
  fController := new TListViewController(self);
  fTable.delegate := fController;
  fTable.dataSource := fController;
  fTable.setHeaderView(nil);

  (fHandle as NSScrollView).documentView := fTable;
end;

method TListViewController.numberOfRowsInTableView(tableView: not nullable NSTableView): NSInteger;
begin
end;

method TListViewController.tableView(tableView: not nullable NSTableView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger): nullable id;
begin
end;

constructor TListViewController(aOwner: TListView);
begin
end;
{$ENDIF}

end.