namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

type
  TListColumn = public partial class(TCollectionItem)
  protected
    method PlatformSetCaption(aValue: VCLString); partial;
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
    method PlatformSetCaption(aValue: VCLString); partial;
  end;

  TListItems = public partial class(TPersistent)
  protected
    method PlatformAdd(aListItem: TListItem); partial;
    method PlatformClear; partial;
    method PlatformDelete(aIndex: Integer); partial;
  end;

  TListView = public partial class(TMultiSelectListControl)
  private assembly
    fTable: GridView;
  protected
    method CreateHandle; override;

    method PlatformSetViewStyle(aValue: TViewStyle); partial;
    method PlatformNativeCreated; override;
  public
    method RefreshContent;
    method PlatformRefreshContent; partial;
  end;

implementation

method TListColumn.PlatformSetCaption(aValue: VCLString);
begin
  PlatformColumn.Header := aValue;
end;

method TListColumns.PlatformAdd(aListColumn: TListColumn);
begin
  var lColumn := new GridViewColumn();
  if aListColumn.Caption ≠ nil then
    lColumn.Header := aListColumn.Caption;
  lColumn.Width := 100;
  var lBinding: System.Windows.Data.Binding;
  if aListColumn.Index = 0 then
    lBinding := new System.Windows.Data.Binding('Caption')
  else
    lBinding := new System.Windows.Data.Binding('SubItems[' + IntToStr(aListColumn.Index - 1) + ']');
  //lBinding.Mode := System.Windows.Data.BindingMode.TwoWay;
  lColumn.DisplayMemberBinding := lBinding;
  aListColumn.PlatformColumn := lColumn;
  fOwner.fTable.Columns.Add(lColumn);
end;

method TSubItems.UpdateSubItem(aIndex: Integer);
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TSubItems.PlatformPut(aIndex: Integer; S: DelphiString);
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TSubItems.PlatformAdd(S: DelphiString);
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TSubItems.PlatformAddObject(S: DelphiString; aObject: TObject);
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TSubItems.PlatformClear;
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TSubItems.PlatformDelete(aIndex: Integer);
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TSubItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  Owner.Owner.Owner.RefreshContent;
end;

method TListItem.PlatformSetCaption(aValue: VCLString);
begin
  Owner.Owner.RefreshContent;
end;

method TListItems.PlatformAdd(aListItem: TListItem);
begin
  Owner.RefreshContent;
end;

method TListItems.PlatformClear;
begin
  Owner.RefreshContent;
end;

method TListItems.PlatformDelete(aIndex: Integer);
begin
  Owner.RefreshContent;
end;

method TListView.CreateHandle;
begin
  fHandle := new ListView();
  fTable := new GridView();
  (fHandle as ListView).View := fTable;
end;

method TListView.PlatformNativeCreated;
begin
  (fHandle as ListView).ItemsSource := fListItems.fItems;
end;

method TListView.PlatformSetViewStyle(aValue: TViewStyle);
begin
end;

method TListView.RefreshContent;
begin
  if not IsUpdating then
    PlatformRefreshContent;
end;

method TListView.PlatformRefreshContent;
begin
  var view := System.Windows.Data.CollectionViewSource.GetDefaultView((fHandle as ListView).ItemsSource);
  view.Refresh();
end;

{$ENDIF}

end.