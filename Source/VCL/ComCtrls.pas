namespace RemObjects.Elements.RTL.Delphi.VCL;

//{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS)) OR ECHOESWPF OR (MACOS AND NOT DARWIN)}
{$IF (ISLAND AND (WINDOWS) AND NOT DARWIN) OR ECHOESWPF OR (MACOS AND NOT (ISLAND AND DARWIN))}

interface

uses
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi,
  {$IF ECHOESWPF}
  System.Reflection
  {$ELSEIF MACOS}
  RemObjects.Elements.RTL.Reflection
  {$ENDIF}
  ;


type
  TWidth = public Integer;
  TViewStyle = public enum (vsIcon, vsSmallIcon, vsList, vsReport) of Integer;
  TItemChange = public enum (ctText, ctImage, ctState) of Integer;
  TLVChangeEvent = public block(Sender: TObject; Item: TListItem; Change: TItemChange);
  TLVSelectItemEvent = public block(Sender: TObject; Item: TListItem; Selected: Boolean);

  TListColumn = public partial class(TCollectionItem)
  private
    fAlignment: TAlignment;
    fAutoSize: Boolean;
    fCaption: String;
    fMaxWidth: TWidth;
    fMinWidth: TWidth;
    fPrivateWidth: TWidth;
    fTag: Integer;
    fWidth: TWidth;
    fColIndex: Integer;
    method DoChange;
    method GetWidth: TWidth;
    method ReadData(aReader: TReader);
    method SetAlignment(aValue: TAlignment);
    method SetAutoSize(aValue: Boolean);
    method SetCaption(aValue: String);
    method SetMaxWidth(Value: TWidth);
    method SetMinWidth(Value: TWidth);
    method SetWidth(Value: TWidth);
  protected
    method GetDisplayName: String; override;
    //method SetIndex(Value: Integer); override;

    method PlatformSetCaption(aValue: String); partial; virtual; empty;
  public
    constructor(aCollection: TCollection);
    //method Assign(Source: TPersistent); override;
    property Alignment: TAlignment read fAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read fAutoSize write SetAutoSize default False;
    property Caption: String read fCaption write SetCaption;
    //property ImageIndex: TImageIndex read fImageIndex write SetImageIndex default -1;
    property MaxWidth: TWidth read fMaxWidth write SetMaxWidth default 0;
    property MinWidth: TWidth read fMinWidth write SetMinWidth default 0;
    property Tag: Integer read fTag write FTag default 0;
    property Width: TWidth read GetWidth write SetWidth default 50;
    property WidthType: TWidth read fWidth;
    property ColIndex: Integer read fColIndex;
  end;

  TListColumnClass = public &Type;

  TListColumns = public partial class(TCollection)
  private
    fOwner: TListView;
    method GetColumnItem(aIndex: Integer): TListColumn;
    method SetColumnItem(aIndex: Integer; Value: TListColumn);
    method UpdateCols;
  protected
    method GetListColumnClass: TListColumnClass; virtual;
    method GetOwner: TPersistent; override;
    method Update(aItem: TCollectionItem); override;

    method PlatformAdd(aListColumn: TListColumn); partial; virtual; empty;
  public
    constructor(aOwner: TListView);
    method &Add: TListColumn;
    property Items[aIndex: Integer]: TListColumn read GetColumnItem write SetColumnItem; default;
  end;

  TSubItems = public partial class(TStringList)
  private
    fOwner: TListItem;
    //fImageIndices: TList;
    //method GetImageIndex(aIndex: Integer): TImageIndex;
    //method SetImageIndex(aIndex: Integer; const Value: TImageIndex);
  protected
    //method GetHandle: HWND;
    method Put(aIndex: Integer; S: DelphiString); override;
    method SetUpdateState(Updating: Boolean); override;

    method PlatformPut(aIndex: Integer; S: DelphiString); partial; virtual; empty;
    method PlatformAdd(S: DelphiString); partial; virtual; empty;
    method PlatformAddObject(S: DelphiString; aObject: TObject); partial; virtual; empty;
    method PlatformClear; virtual; partial; empty;
    method PlatformDelete(aIndex: Integer); partial; virtual; empty;
    method PlatformInsert(aIndex: Integer; S: DelphiString); partial; virtual; empty;
  public
    constructor Create(aOwner: TListItem);
    function &Add(S: DelphiString): Integer; override;
    function AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; S: DelphiString); override;
    //property Handle: HWND read GetHandle;
    property Owner: TListItem read fOwner;
    //property ImageIndex[Index: Integer]: TImageIndex read GetImageIndex write SetImageIndex;
  end;

  TListItem = public partial class(TPersistent)
  private
    fCaption: String;
    fOwner: TListItems;
    fSubItems: TStrings;
    //fImageIndex: TImageIndex;
    fIndent: Integer;
    fIndex: Integer;
    //fOverlayIndex: TImageIndex;
    //fStateIndex: TImageIndex;
    //fCaption: string;
    //fDeleting: Boolean;
    //fProcessedDeleting: Boolean;
    //fChecked: Boolean;
    //fData: TCustomData;
    //fGroupID: Integer;
    function GetChecked: Boolean;
    //function GetHandle: HWND;
    function GetListView: TListView;
    function GetLeft: Integer;
    function GetState(aIndex: Integer): Boolean;
    function GetTop: Integer;
    function IsEqual(Item: TListItem): Boolean;
    method SetChecked(Value: Boolean);
    method SetCaption(aValue: string);
    //method SetData(Value: TCustomData);
    //method SetImage(Index: Integer; Value: TImageIndex);
    method SetIndent(Value: Integer);
    method SetLeft(Value: Integer);
    method SetState(aIndex: Integer; State: Boolean);
    method SetSubItems(Value: TStrings);
    method SetTop(Value: Integer);
    //function GetSubItemImage(Index: Integer): Integer;
    //method SetSubItemImage(Index: Integer; const Value: Integer);
    //method SetGroupID(Value: Integer);
  protected
    method PlatformSetCaption(aValue: String); partial; virtual; empty;
  public
    constructor(aOwner: TListItems); virtual;
    //method Assign(Source: TPersistent); override;
    method CancelEdit;
    method Delete;
    function EditCaption: Boolean;
    //function GetPosition: TPoint;
    method MakeVisible(PartialOK: Boolean);
    method Update;
    //method SetPosition(const Value: TPoint);
    //function WorkArea: Integer;
    property Caption: String read fCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    //property Cut: Boolean index 0 read GetState write SetState;
    //property Data: TCustomData read FData write SetData;
    //property Deleting: Boolean read FDeleting;
    //property DropTarget: Boolean index 1 read GetState write SetState;
    //property Focused: Boolean read GetState write SetState; // TODO
    //property GroupID: Integer read FGroupID write SetGroupID default -1;
    //property Handle: HWND read GetHandle;
    //property ImageIndex: TImageIndex index 0 read FImageIndex write SetImage;
    //property Indent: Integer read FIndent write SetIndent default 0;
    property &Index: Integer read fIndex write fIndex;
    property Left: Integer read GetLeft write SetLeft;
    property ListView: TListView read GetListView;
    property Owner: TListItems read fOwner;
    //property OverlayIndex: TImageIndex index 1 read FOverlayIndex write SetImage;
    //property Position: TPoint read GetPosition write SetPosition;
    //property Selected: Boolean read GetState write SetState; // TODO index
    //property StateIndex: TImageIndex index 2 read fStateIndex write SetImage;
    property SubItems: TStrings read fSubItems write SetSubItems;
    //property SubItemImages[Index: Integer]: Integer read GetSubItemImage write SetSubItemImage;
    property Top: Integer read GetTop write SetTop;
  end;

  TPlatformListViewItem = public class
  end;

  TListItems = public partial class(TPersistent)
  public
    fItems: TList<TListItem>;
  private assembly
    fOwner: TListView;
    fUpdating: Integer := 0;
    method ReadListItemsData(aReader: TReader);
  protected
    //method DefineProperties(Filer: TFiler); override;
    method CreateItem(aIndex: Integer; aListItem: TListItem): TPlatformListViewItem;
    method GetHandle: TPlatformHandle;
    method GetItem(aIndex: Integer): TListItem;
    method SetCount(aValue: Integer);
    method SetItem(aIndex: Integer; Value: TListItem);
    method SetUpdateState(updating: Boolean);

    method PlatformAdd(aListItem: TListItem); partial; virtual; empty;
    method PlatformClear; partial; virtual; empty;
    method PlatformDelete(aIndex: Integer); partial; virtual; empty;
  public
    constructor(aOwner: TListView);
    method DefineProperties(Filer: TObject {TFiler});
    method &Add: TListItem;
    //method AddItem(aItem: TListItem; aIndex: Integer = -1): TListItem;
    //method Assign(Source: TPersistent); override;
    method BeginUpdate;
    method Clear;
    method Delete(aIndex: Integer);
    method EndUpdate;
    //method GetEnumerator: TListItemsEnumerator;
    method IndexOf(aValue: TListItem): Integer;
    method Insert(aIndex: Integer): TListItem;
    property Count: Integer read fItems.Count write SetCount;
    property Handle: TPlatformHandle read GetHandle;
    property Item[aIndex: Integer]: TListItem read GetItem write SetItem; default;
    property Owner: TListView read fOwner;
  end;

  TListView = public partial class(TMultiSelectListControl)
  private
    //fBorderStyle: TBorderStyle;
    fListColumns: TListColumns;
    fReadOnly: Boolean;
    fHideSelection: Boolean;
    fViewStyle: TViewStyle := TViewStyle.vsIcon;
    fGridLines: Boolean;
    fListItems: TListItems;
    fShowColumnHeaders: Boolean;
    fRowSelect: Boolean;
    fOnChange: TLVChangeEvent;

    fOnSelectItem: TLVSelectItemEvent;

    method SetColumnHeaders(value: Boolean);
    method GetColumnFromIndex(aIndex: Integer): TListColumn;
    method GetFocused: TListItem;
    method SetFocused(aValue: TListItem);
    method GetSelected: TListItem;
    method SetSelected(aValue: TListItem);
    method SetItems(aValue: TListItems);
    method SetListColumns(aValue: TListColumns);
    method SetReadOnly(aValue: Boolean);
    method SetHideSelection(aValue: Boolean);
    method SetRowSelect(aValue: Boolean);
  protected
    method Change(Item: TListItem; aChange: Integer); virtual;
    method ColClick(aColumn: TListColumn); virtual;
    method ColumnsShowing: Boolean;
    method CreateListItem: TListItem; virtual;
    method CreateListItems: TListItems; virtual;
    method Delete(Item: TListItem); virtual;
    method DoSelectItem(aItem: TListItem; aSelected: Boolean); virtual;
    //method Edit(aItem: TLVItem); virtual;
    //method MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    method GetItemIndex(Value: TListItem): Integer; reintroduce; overload;
    method GetItemIndex: Integer; reintroduce; overload; override;
    method GetListColumnsClass: TListColumnClass; virtual;
    method GetSelCount: Integer; override;
    method InsertItem(Item: TListItem); virtual;
    //method Notification(AComponent: TComponent; Operation: TOperation); override;
    method SetItemIndex(aValue: Integer); override;
    method SetMultiSelect(Value: Boolean); override;
    method SetViewStyle(aValue: TViewStyle); virtual;
    method UpdateColumn(AnIndex: Integer);
    method UpdateColumns;
    //method WndProc(var Message: TMessage); override; --> Windows...
    //property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsSingle;
    //property ColumnClick: Boolean read fColumnClick write SetColumnClick default True;
    property &ReadOnly: Boolean read fReadOnly write SetReadOnly;
    property HideSelection: Boolean read fHideSelection write SetHideSelection default true;
    //property IconOptions: TIconOptions read FIconOptions write SetIconOptions;
    //property GroupView: Boolean read FGroupView write SetGroupView default False;
    //property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    //property GroupHeaderImages: TCustomImageList read FGroupHeaderImages write SetGroupHeaderImages;
    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect default false;
    //property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property OnChange: TLVChangeEvent read fOnChange write fOnChange;
    //property OnChanging: TLVChangingEvent read FOnChanging write FOnChanging;
    //property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
    //property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    //property OnCreateItemClass: TLVCreateItemClassEvent read FOnCreateItemClass write FOnCreateItemClass;
    //property OnData: TLVOwnerDataEvent read FOnData write FOnData;
    //property OnDataFind: TLVOwnerDataFindEvent read FOnDataFind write FOnDataFind;
    //property OnDataHint: TLVOwnerDataHintEvent read FOnDataHint write FOnDataHint;
    //property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    //property OnDrawItem: TLVDrawItemEvent read FOnDrawItem write FOnDrawItem;
    //property OnEdited: TLVEditedEvent read FOnEdited write FOnEdited;
    //property OnEditing: TLVEditingEvent read FOnEditing write FOnEditing;
    //property OnInsert: TLVDeletedEvent read FOnInsert write FOnInsert;
    //property OnGetImageIndex: TLVNotifyEvent read FOnGetImageIndex write FOnGetImageIndex;
    //property OnGetSubItemImage: TLVSubItemImageEvent read FOnGetSubItemImage write FOnGetSubItemImage;
    property OnSelectItem: TLVSelectItemEvent read fOnSelectItem write fOnSelectItem;
    //property OnItemChecked: TLVCheckedItemEvent read FOnItemChecked write FOnItemChecked;
    property ShowColumnHeaders: Boolean read fShowColumnHeaders write SetColumnHeaders default true;
    //property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    //property SortType: TSortType read FSortType write SetSortType default stNone;
    //property StateImages: TCustomImageList read FStateImages write SetStateImages;

    method PlatformSetViewStyle(aValue: TViewStyle); partial; virtual; empty;
    method PlatformSetRowSelect(aValue: Boolean); partial; virtual; empty;
  public
    constructor(aOwner: TComponent);
    method PlatformRefreshContent; partial; virtual; empty;
    method AddItem(aItem: DelphiString; aObject: TObject); override;
    //method Arrange(Code: TListArrangement);
    method Clear; override;
    method ClearSelection; override;
    //method CopySelection(Destination: TListControl); override;
    method DeleteSelected; override;
    method FindCaption(StartIndex: Integer; Value: String; &Partial, Inclusive, Wrap: Boolean): TListItem;
    method GetCount: Integer; override;
    method GetItemAt(X, Y: Integer): TListItem;
    method GetSearchString: String;
    method IsEditing: Boolean;
    method Loaded; override;
    method SelectAll; override;
    method Scroll(DX, DY: Integer);
    //property Checkboxes: Boolean read FCheckboxes write SetCheckboxes default False;
    property Column[aIndex: Integer]: TListColumn read GetColumnFromIndex;
    property Columns: TListColumns read fListColumns write SetListColumns;
    //property GridLines: Boolean read fGridLines write SetGridLines default False;
    property IsUpdating: Boolean read Items.fUpdating > 0;
    property ItemFocused: TListItem read GetFocused write SetFocused;
    property Items: TListItems read fListItems write SetItems;
    property RowSelect: Boolean read fRowSelect write SetRowSelect default false;
    property SelCount: Integer read GetSelCount;
    property Selected: TListItem read GetSelected write SetSelected;
    property ViewStyle: TViewStyle read fViewStyle write SetViewStyle default vsIcon;
  end;


implementation

method TListColumn.DoChange;
begin

end;

method TListColumn.GetWidth: TWidth;
begin

end;

method TListColumn.ReadData(aReader: TReader);
begin

end;

method TListColumn.SetAlignment(aValue: TAlignment);
begin

end;

method TListColumn.SetAutoSize(aValue: Boolean);
begin

end;

method TListColumn.SetCaption(aValue: String);
begin
  fCaption := aValue;
  PlatformSetCaption(aValue);
end;

method TListColumn.SetMaxWidth(Value: TWidth);
begin

end;

method TListColumn.SetMinWidth(Value: TWidth);
begin

end;

method TListColumn.SetWidth(Value: TWidth);
begin

end;

method TListColumn.GetDisplayName: String;
begin

end;

//method TListColumn.SetIndex(Value: Integer);
//begin

//end;

constructor TListColumn(aCollection: TCollection);
begin
  Collection := aCollection;
  fColIndex := Collection.Count;
end;

method TListColumns.GetColumnItem(aIndex: Integer): TListColumn;
begin
  result := inherited GetItem(aIndex) as TListColumn;
end;

method TListColumns.SetColumnItem(aIndex: Integer; Value: TListColumn);
begin

end;

method TListColumns.UpdateCols;
begin

end;

method TListColumns.GetListColumnClass: TListColumnClass;
begin

end;

method TListColumns.GetOwner: TPersistent;
begin
  result := fOwner;
end;

method TListColumns.Update(aItem: TCollectionItem);
begin

end;

constructor TListColumns(aOwner: TListView);
begin
  {$IF TOFFEE}
  inherited(new &Type withClass(typeOf(TListColumn)));
  {$ELSE}
  inherited(typeOf(TListColumn));
  {$ENDIF}
  fOwner := aOwner;
end;

method TListColumns.Add: TListColumn;
begin
  result := (inherited &Add) as TListColumn;
  PlatformAdd(result);
end;

method TListItems.ReadListItemsData(aReader: TReader);
begin

end;

method TListItems.CreateItem(aIndex: Integer; aListItem: TListItem): TPlatformListViewitem;
begin

end;

method TListItems.GetHandle: TPlatformHandle;
begin

end;

method TListItems.GetItem(aIndex: Integer): TListItem;
begin
  result := fItems[aIndex];
end;

method TListItems.SetCount(aValue: Integer);
begin

end;

method TListItems.SetItem(aIndex: Integer; Value: TListItem);
begin

end;

method TListItems.SetUpdateState(updating: Boolean);
begin

end;

constructor TListItems(aOwner: TListView);
begin
  fItems := new TList<TListItem>();
  fOwner := aOwner;
end;

method TListItems.DefineProperties(Filer: TObject);
begin
  inherited DefineProperties(Filer);
  var lFiler := Filer as TFiler;
  //TODO lFiler.DefineBinaryProperty('ItemData', ReadListItemsData, nil, true);
end;

method TListItems.Add: TListItem;
begin
  result := new TListItem(self);
  result.Index := fItems.Add(result);
  PlatformAdd(result);
end;

/*method TListItems.AddItem(aItem: TListItem; aIndex: Integer := -1): TListItem;
begin

end;*/

method TListItems.BeginUpdate;
begin
  inc(fUpdating);
end;

method TListItems.Clear;
begin
  fItems.Clear;
  PlatformClear;
end;

method TListItems.Delete(aIndex: Integer);
begin
  fItems.Delete(aIndex);
  PlatformDelete(aIndex);
end;

method TListItems.EndUpdate;
begin
  dec(fUpdating);
  if fUpdating = 0 then
    Owner.PlatformRefreshContent;
end;

method TListItems.IndexOf(aValue: TListItem): Integer;
begin

end;

method TListItems.Insert(aIndex: Integer): TListItem;
begin

end;

method TListView.Change(Item: TListItem; aChange: Integer);
begin

end;

method TListView.ColClick(aColumn: TListColumn);
begin

end;

method TListView.ColumnsShowing: Boolean;
begin

end;

method TListView.CreateListItem: TListItem;
begin

end;

method TListView.CreateListItems: TListItems;
begin

end;

method TListView.Delete(Item: TListItem);
begin

end;

method TListView.DoSelectItem(aItem: TListItem; aSelected: Boolean);
begin

end;

method TListView.GetItemIndex(Value: TListItem): Integer;
begin

end;

method TListView.GetItemIndex: Integer;
begin

end;

method TListView.GetListColumnsClass: TListColumnClass;
begin

end;

method TListView.GetSelCount: Integer;
begin

end;

method TListView.InsertItem(Item: TListItem);
begin

end;

method TListView.Loaded;
begin

end;

method TListView.SetItemIndex(aValue: Integer);
begin

end;

method TListView.SetMultiSelect(Value: Boolean);
begin

end;

method TListView.SetViewStyle(aValue: TViewStyle);
begin
  if (aValue ≠ fViewStyle) then begin
    fViewStyle := aValue;
    PlatformSetViewStyle(aValue);
  end;
end;

method TListView.UpdateColumn(AnIndex: Integer);
begin

end;

method TListView.UpdateColumns;
begin

end;

constructor TListView(aOwner: TComponent);
begin
  fListColumns := new TListColumns(self);
  fListItems := new TListItems(self);
  PlatformNativeCreated;
  //fViewStyle := TViewStyle.vsIcon;
end;

method TListView.AddItem(aItem: DelphiString; aObject: TObject);
begin

end;

method TListView.Clear;
begin

end;

method TListView.ClearSelection;
begin

end;

method TListView.DeleteSelected;
begin

end;

method TListView.FindCaption(StartIndex: Integer; Value: String; &Partial: Boolean; Inclusive: Boolean; Wrap: Boolean): TListItem;
begin

end;

method TListView.GetCount: Integer;
begin

end;

method TListView.GetItemAt(X: Integer; Y: Integer): TListItem;
begin

end;

method TListView.GetSearchString: String;
begin

end;

method TListView.IsEditing: Boolean;
begin

end;

method TListView.SelectAll;
begin

end;

method TListView.Scroll(DX: Integer; DY: Integer);
begin

end;

method TListView.SetColumnHeaders(value: Boolean);
begin

end;

method TListView.GetColumnFromIndex(aIndex: Integer): TListColumn;
begin

end;

method TListView.GetFocused: TListItem;
begin

end;

method TListView.SetFocused(aValue: TListItem);
begin

end;

method TListView.GetSelected: TListItem;
begin

end;

method TListView.SetSelected(aValue: TListItem);
begin

end;

method TListView.SetItems(aValue: TListItems);
begin
end;

method TListView.SetListColumns(aValue: TListColumns);
begin
end;

method TListView.SetReadOnly(aValue: Boolean);
begin
end;

method TListView.SetHideSelection(aValue: Boolean);
begin
end;

method TListView.SetRowSelect(aValue: Boolean);
begin
  fRowSelect := aValue;
  PlatformSetRowSelect(aValue);
end;

constructor TSubItems(aOwner: TListItem);
begin
  fOwner := aOwner;
end;

method TSubItems.Put(aIndex: Integer; S: DelphiString);
begin
  inherited(aIndex, S);
  PlatformPut(aIndex, S);
end;

method TSubItems.SetUpdateState(Updating: Boolean);
begin
end;

function TSubItems.&Add(S: DelphiString): Integer;
begin
  result := inherited(S);
  PlatformAdd(S);
end;

function TSubItems.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  result := inherited(S, aObject);
  PlatformAddObject(S, aObject);
end;

method TSubItems.Clear;
begin
  inherited;
  PlatformClear;
end;

method TSubItems.Delete(aIndex: Integer);
begin
  inherited(aIndex);
  PlatformDelete(aIndex);
end;

method TSubItems.Insert(aIndex: Integer; S: DelphiString);
begin
  inherited(aIndex, S);
  PlatformInsert(aIndex, S);
end;

method TListItem.GetChecked: Boolean;
begin

end;

method TListItem.GetListView: TListView;
begin

end;

method TListItem.GetLeft: Integer;
begin

end;

method TListItem.GetState(aIndex: Integer): Boolean;
begin

end;

method TListItem.GetTop: Integer;
begin

end;

method TListItem.IsEqual(Item: TListItem): Boolean;
begin

end;

method TListItem.SetChecked(Value: Boolean);
begin

end;

method TListItem.SetIndent(Value: Integer);
begin

end;

method TListItem.SetLeft(Value: Integer);
begin

end;

method TListItem.SetState(aIndex: Integer; State: Boolean);
begin

end;

method TListItem.SetSubItems(Value: TStrings);
begin

end;

method TListItem.SetTop(Value: Integer);
begin

end;

constructor TListItem(aOwner: TListItems);
begin
  fOwner := aOwner;
  fSubItems := new TSubItems(self);
end;

method TListItem.CancelEdit;
begin

end;

method TListItem.Delete;
begin

end;

method TListItem.EditCaption: Boolean;
begin

end;

method TListItem.MakeVisible(PartialOK: Boolean);
begin

end;

method TListItem.Update;
begin

end;

method TListItem.SetCaption(aValue: String);
begin
  fCaption := aValue;
  PlatformSetCaption(aValue);
end;
{$ENDIF}

end.