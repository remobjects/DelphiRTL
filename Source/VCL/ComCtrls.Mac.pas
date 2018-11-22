namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF MACOS AND NOT (ISLAND AND DARWIN)}

interface

uses
  AppKit, RemObjects.Elements.RTL.Delphi;

type
  TListColumn = public partial class(TCollectionItem)
  protected
    method PlatformSetCaption(aValue: String); partial;
  end;

  TListColumns = public partial class(TCollection)
  private
    fPlatformColumn: NSTableColumn;
  protected
    method PlatformAdd: TListColumn; partial;
  end;

  TSubItems = public partial class(TStringList)
  private
    method UpdateSubItem(aIndex: Integer);
  protected
    method PlatformPut(aIndex: Integer; S: DelphiString); partial;
    method PlatformAdd(S: DelphiString); partial;
    method PlatformAddObject(S: DelphiString; aObject: TObject); partial;
    method PlatformClear; partial;
    method PlatformDelete(aIndex: Integer); partial;
    method PlatformInsert(aIndex: Integer; S: DelphiString); partial;
  end;

  TListItem = public partial class(TPersistent)
  protected
    method PlatformSetCaption(aValue: String); partial;
  end;

  TListItems = public partial class(TPersistent)
  protected
    method PlatformAdd; partial;
    method PlatformClear; partial;
    method PlatformDelete(aIndex: Integer); partial;
  end;

  TListView = public partial class(TMultiSelectListControl)
  private
    fController: TListViewController;
    fTable: NSTableView;
    method SetupControlView;
  protected
    method CreateHandle; override;
    method PlatformSetViewStyle(aValue: TViewStyle); partial;
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

method TListColumn.PlatformSetCaption(aValue: String);
begin

end;

method TListColumns.PlatformAdd: TListColumn;
begin

end;

method TSubItems.UpdateSubItem(aIndex: Integer);
begin

end;

method TSubItems.PlatformPut(aIndex: Integer; S: DelphiString);
begin

end;

method TSubItems.PlatformAdd(S: DelphiString);
begin

end;

method TSubItems.PlatformAddObject(S: DelphiString; aObject: TObject);
begin

end;

method TSubItems.PlatformClear;
begin

end;

method TSubItems.PlatformDelete(aIndex: Integer);
begin

end;

method TSubItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin

end;

method TListItem.PlatformSetCaption(aValue: String);
begin

end;

method TListItems.PlatformAdd;
begin

end;

method TListItems.PlatformClear;
begin

end;

method TListItems.PlatformDelete(aIndex: Integer);
begin

end;

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

method TListView.SetupControlView(aValue: TViewStyle);
begin
  case aValue of
    TViewStyle.vsReport: begin
      fTable := new NSTableView();
      UnderlyingHandle := fTable;
      fHandle := new NSScrollView;
      fController := new TListViewController(self);
      fTable.delegate := fController;
      fTable.dataSource := fController;
      for i: Integer := 0 to Columns.Count do begin
        var lColumn := new NSTableColumn withIdentifier(Name + 'C' + i.ToString);
        lColumn.title := Columns[i].Caption;
        Columns[i].fTableColumn := lColumn;
        fTable.addTableColumn(lColumn);
      end;
      //fTable.setHeaderView(nil);

      (fHandle as NSScrollView).documentView := fTable;
    end;
  end;
end;

method TListView.PlatformSetViewStyle(aValue: TViewStyle);
begin
  SetupControlView(aValue);
end;

method TListViewController.numberOfRowsInTableView(tableView: not nullable NSTableView): NSInteger;
begin
  if fOwnerList.Items = nil then
    exit 0;

  result := fOwnerList.Items.Count;
end;

method TListViewController.tableView(tableView: not nullable NSTableView) objectValueForTableColumn(tableColumn: nullable NSTableColumn) row(row: NSInteger): nullable id;
begin

end;

constructor TListViewController(aOwner: TListView);
begin
  fOwnerList := aOwner;
end;
{$ENDIF}

end.