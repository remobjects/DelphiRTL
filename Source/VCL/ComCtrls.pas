namespace RemObjects.Elements.RTL.Delphi.VCL;

interface

type
  TListColumn = class(TCollectionItem)
  private
    fAlignment: TAlignment;
    fAutoSize: Boolean;
    fCaption: string;
    fImageIndex: TImageIndex;
    fMaxWidth: TWidth;
    fMinWidth: TWidth;
    fPrivateWidth: TWidth;
    fTag: Integer;
    fWidth: TWidth;
    method DoChange;
    method GetWidth: TWidth;
    method IsWidthStored: Boolean;
    method ReadData(aReader: TReader);
    method SetAlignment(aValue: TAlignment);
    method SetAutoSize(aValue: Boolean);
    method SetCaption(aValue: string);
    method SetImageIndex(Value: TImageIndex);
    method SetMaxWidth(Value: TWidth);
    method SetMinWidth(Value: TWidth);
    method SetWidth(Value: TWidth);
    method WriteData(Writer: TWriter);
  protected
    method DefineProperties(aFiler: TFiler); override;
    method GetDisplayName: string; override;
    method SetIndex(Value: Integer); override;
  public
    constructor(aCollection: TCollection); override;
    //method Assign(Source: TPersistent); override;
    property Alignment: TAlignment read fAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read fAutoSize write SetAutoSize default False;
    property Caption: string read fCaption write SetCaption;
    property ImageIndex: TImageIndex read fImageIndex write SetImageIndex default -1;
    property MaxWidth: TWidth read fMaxWidth write SetMaxWidth default 0;
    property MinWidth: TWidth read fMinWidth write SetMinWidth default 0;
    property Tag: Integer read fTag write FTag default 0;
    property Width: TWidth read GetWidth write SetWidth stored IsWidthStored default 50;
    property WidthType: TWidth read fWidth;
  end;

  TListColumnsClass = typeOf(TListColumns);

  TListColumns = class(TCollection)
  private
    fOwner: TListView;
    method GetItem(aIndex: Integer): TListColumn;
    method SetItem(aIndex: Integer; Value: TListColumn);
    method UpdateCols;
  protected
    method GetListColumnClass: TListColumnClass; virtual;
    method GetOwner: TPersistent; override;
    method Update(aItem: TCollectionItem); override;
  public
    constructor(aOwner: TListView);
    method &Add: TListColumn;
    method Owner: TListView;
    property Items[aIndex: Integer]: TListColumn read GetItem write SetItem; default;
  end;

  TPlatformListViewitem = class
  end;

  TListItems = class(TPersistent)
  private
    fOwner: TListView;
  protected
    //method DefineProperties(Filer: TFiler); override;
    method CreateItem(aIndex: Integer; aListItem: TListItem): TPlatformListViewItem;
    method GetCount: Integer;
    method GetHandle: TPlatformHandle;
    method GetItem(aIndex: Integer): TListItem;
    method SetCount(aValue: Integer);
    method SetItem(aIndex: Integer; Value: TListItem);
    method SetUpdateState(updating: Boolean);
  public
    constructor(aOwner: TListView);
    method &Add: TListItem;
    method AddItem(aItem: TListItem; aIndex: Integer = -1): TListItem;
    //method Assign(Source: TPersistent); override;
    method BeginUpdate;
    method Clear;
    method Delete(aIndex: Integer);
    method EndUpdate;
    //method GetEnumerator: TListItemsEnumerator;
    method IndexOf(aValue: TListItem): Integer;
    method Insert(aIndex: Integer): TListItem;
    property Count: Integer read GetCount write SetCount;
    property Handle: TPlatformHandle read GetHandle;
    property Item[aIndex: Integer]: TListItem read GetItem write SetItem; default;
    property Owner: TListView read FOwner;
  end;

  TListView = public class(TMultiSelectListControl)
  protected
    method Change(Item: TListItem; Change: Integer); virtual;
    method ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    method ColClick(Column: TListColumn); virtual;
    method ColumnsShowing: Boolean;
    method CreateListItem: TListItem; virtual;
    method CreateListItems: TListItems; virtual;
    method CreateParams(var Params: TCreateParams); override;
    method CreateWnd; override;
    method Delete(Item: TListItem); virtual;
    method DestroyWnd; override;
    method DoSelectItem(Item: TListItem; Selected: Boolean); virtual;
    method Edit(aItem: TLVItem); virtual;
    method MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    method GetItemIndex(Value: TListItem): Integer; reintroduce; overload;
    method GetItemIndex: Integer; reintroduce; overload; override;
    method GetListColumnsClass: TListColumnsClass; virtual;
    method GetSelCount: Integer; override;
    method InsertItem(Item: TListItem); virtual;
    method Loaded; override;
    method Notification(AComponent: TComponent; Operation: TOperation); override;
    method SetItemIndex(aValue: Integer); override;
    method SetMultiSelect(Value: Boolean); override;
    method SetViewStyle(Value: TViewStyle); virtual;
    method UpdateColumn(AnIndex: Integer);
    method UpdateColumns;
    //method WndProc(var Message: TMessage); override; --> Windows...
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Groups: TListGroups read FListGroups write SetListGroups stored StoreGroups;
    property Columns: TListColumns read FListColumns write SetListColumns;
    property ColumnClick: Boolean read FColumnClick write SetColumnClick default True;
    property &ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property IconOptions: TIconOptions read FIconOptions write SetIconOptions;
    property GroupView: Boolean read FGroupView write SetGroupView default False;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property GroupHeaderImages: TCustomImageList read FGroupHeaderImages write SetGroupHeaderImages;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnChanging: TLVChangingEvent read FOnChanging write FOnChanging;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
    property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    property OnCreateItemClass: TLVCreateItemClassEvent read FOnCreateItemClass write FOnCreateItemClass;
    property OnData: TLVOwnerDataEvent read FOnData write FOnData;
    property OnDataFind: TLVOwnerDataFindEvent read FOnDataFind write FOnDataFind;
    property OnDataHint: TLVOwnerDataHintEvent read FOnDataHint write FOnDataHint;
    property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnDrawItem: TLVDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnEdited: TLVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TLVEditingEvent read FOnEditing write FOnEditing;
    property OnInsert: TLVDeletedEvent read FOnInsert write FOnInsert;
    property OnGetImageIndex: TLVNotifyEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSubItemImage: TLVSubItemImageEvent read FOnGetSubItemImage write FOnGetSubItemImage;
    property OnSelectItem: TLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnItemChecked: TLVCheckedItemEvent read FOnItemChecked write FOnItemChecked;
    property Reading: Boolean read FReading;
    property ShowColumnHeaders: Boolean read FShowColumnHeaders write SetColumnHeaders default True;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsIcon;
  public
    constructor(AOwner: TComponent); override;
    method AddItem(Item: String; AObject: TObject); override;
    method Arrange(Code: TListArrangement);
    method Clear; override;
    method ClearSelection; override;
    method CopySelection(Destination: TListControl); override;
    method DeleteSelected; override;
    method FindCaption(StartIndex: Integer; Value: string; &Partial, Inclusive, Wrap: Boolean): TListItem;
    method GetCount: Integer; override;
    method GetItemAt(X, Y: Integer): TListItem;
    method GetSearchString: string;
    method IsEditing: Boolean;
    method SelectAll; override;
    method Scroll(DX, DY: Integer);
    method CustomSort(SortProc: TLVCompare; lParam: LPARAM): Boolean;
    property Checkboxes: Boolean read FCheckboxes write SetCheckboxes default False;
    property Column[Index: Integer]: TListColumn read GetColumnFromIndex;
    property FlatScrollBars: Boolean read FFlatScrollBars write SetFlatScrollBars default False;
    property GridLines: Boolean read FGridLines write SetGridLines default False;
    property ItemFocused: TListItem read GetFocused write SetFocused;
    property Items: TListItems read FListItems write SetItems stored AreItemsStored;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property SelCount: Integer read GetSelCount;
    property Selected: TListItem read GetSelected write SetSelected;
    method StringWidth(S: string): Integer;
    method UpdateItems(FirstIndex, LastIndex: Integer);
  end;

implementation

end.