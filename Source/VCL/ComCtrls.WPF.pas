namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

type
  TListColumn = public partial class(TCollectionItem)
  protected
    method PlatformSetCaption(aValue: String); partial;
  public
    PlatformColumn: GridViewColumn;
  end;

  TListColumns = public partial class(TCollection)
  protected
    method PlatformAdd(aListColumn: TListColumn); partial;
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
  private assembly
    fTable: GridView;
  protected
    method CreateHandle; override;

    method PlatformSetViewStyle(aValue: TViewStyle); partial;
  end;

implementation

method TListColumn.PlatformSetCaption(aValue: String);
begin
  PlatformColumn.Header := aValue;
end;

method TListColumns.PlatformAdd(aListColumn: TListColumn);
begin
  var lColumn := new GridViewColumn();
  if aListColumn.Caption ≠ nil then
    lColumn.Header := aListColumn.Caption;
  lColumn.Width := 100;
  aListColumn.PlatformColumn := lColumn;
  fOwner.fTable.Columns.Add(lColumn);
end;

method TListItem.PlatformSetCaption(aValue: String);
begin

end;

method TListItems.PlatformAdd;
begin
  var lItem := Owner.Items.Add;
  lItem.
end;

method TListItems.PlatformClear;
begin

end;

method TListItemsPlatformDelete(aIndex: Integer);
begin

end;

method TListView.CreateHandle;
begin
  fHandle := new ListView();
  fTable := new GridView();
  (fHandle as ListView).View := fTable;
end;

method TListView.PlatformSetViewStyle(aValue: TViewStyle);
begin
end;

{$ENDIF}

end.