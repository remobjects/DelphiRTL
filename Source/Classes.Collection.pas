namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TCollectionItem = class(TPersistent)
  private
    fCollection: TCollection;
    fID: Integer;
    method GetIndex: Integer;
  protected
    method Changed(AllItems: Boolean);
    method GetOwner: TPersistent; override;
    method GetDisplayName: String; virtual;
    method SetCollection(aValue: TCollection); virtual;
    method SetIndex(aValue: Integer); virtual;
    method SetDisplayName(aValue: String); virtual;
  public
    constructor(aCollection: TCollection); virtual;
    method Release; virtual;
    method GetNamePath: String; override;
    property Collection: TCollection read fCollection write SetCollection;
    property ID: Integer read fID;
    property &Index: Integer read GetIndex write SetIndex;
    property DisplayName: String read GetDisplayName write SetDisplayName;
  end;

  TCollection = class(TPersistent)
  private
    fItemClass: TCollectionItemClass;
    fItems: TList<TCollectionItem>;
    fUpdateCount: Integer;
    fNextID: Integer;
    fPropName: String;
    method GetCapacity: Integer;
    method GetCount: Integer;
    method GetPropName: String;
    method InsertItem(aItem: TCollectionItem);
    method RemoveItem(aItem: TCollectionItem);
    method SetCapacity(aValue: Integer);
  protected
    property NextID: Integer read fNextID;
    method &Notify(aItem: TCollectionItem; Action: TCollectionNotification); virtual;
    method Changed;
    method GetItem(aIndex: Integer): TCollectionItem;
    method SetItem(aIndex: Integer; Value: TCollectionItem);
    method SetItemName(aItem: TCollectionItem); virtual;
    method Update(aItem: TCollectionItem); virtual;
    property PropName: String read GetPropName write fPropName;
    property UpdateCount: Integer read fUpdateCount;
  public
    constructor(aItemClass: TCollectionItemClass);
    method Owner: TPersistent;
    method &Add: TCollectionItem;
    method Assign(Source: TPersistent); override;
    method BeginUpdate; virtual;
    method Clear;
    method Delete(aIndex: Integer);
    method EndUpdate; virtual;
    method FindItemID(aID: Integer): TCollectionItem;
    method GetEnumerator: TCollectionEnumerator;
    method GetNamePath: String; override;
    method Insert(aIndex: Integer): TCollectionItem;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemClass: TCollectionItemClass read fItemClass;
    property Items[aIndex: Integer]: TCollectionItem read GetItem write SetItem;
  end;

implementation

end.