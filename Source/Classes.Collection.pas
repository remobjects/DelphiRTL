namespace RemObjects.Elements.RTL.Delphi;

interface

{$IF ISLAND}
uses
  RemObjects.Elements.System;
{$ELSEIF TOFFEE OR COOPER}
uses
  RemObjects.Elements.RTL.Reflection;
{$ENDIF}

type
  TObjectCtor = procedure(aInst: Object; aPar: TObject);

  TClassActivator = public static class(TObject)
  public
    class method CreateInstance(aType: &Type; aParameters: array of TObject): TObject;
  end;

  TCollectionItem = public class(TPersistent)
  private
    fCollection: TCollection;
    fID: Integer;
    method GetIndex: Integer;
  protected
    method Changed(AllItems: Boolean);
    //method GetOwner: TPersistent; override;
    method GetDisplayName: String; virtual;
    method SetCollection(aValue: TCollection); virtual;
    method SetIndex(aValue: Integer); virtual;
    method SetDisplayName(aValue: String); virtual;
  public
    constructor(aCollection: TCollection); virtual;
    //method Release; virtual;
    //method GetNamePath: String; override;
    property Collection: TCollection read fCollection write SetCollection;
    property ID: Integer read fID;
    property &Index: Integer read GetIndex write SetIndex;
    property DisplayName: String read GetDisplayName write SetDisplayName;
  end;

  TCollectionItemClass = public &Type;

  TCollection = public class(TPersistent)
  private
    fItemClass: TCollectionItemClass;
    fItems: TList<TCollectionItem>;
    fUpdateCount: Integer;
    fNextID: Integer;
    fPropName: String;
    method GetCapacity: Integer;
    method GetPropName: String;
    method InsertItem(aItem: TCollectionItem);
    method RemoveItem(aItem: TCollectionItem);
    method SetCapacity(aValue: Integer);
  protected
    property NextID: Integer read fNextID;
    method &Notify(aItem: TCollectionItem; Action: TCollectionNotification); virtual;
    method Changed;
    method GetItem(aIndex: Integer): TCollectionItem;
    method SetItem(aIndex: Integer; aValue: TCollectionItem);
    method SetItemName(aItem: TCollectionItem); virtual;
    method Update(aItem: TCollectionItem); virtual;
    property PropName: String read GetPropName write fPropName;
    property UpdateCount: Integer read fUpdateCount;
  public
    constructor(aItemClass: TCollectionItemClass);
    method Owner: TPersistent;
    method &Add: TCollectionItem;
    //method Assign(Source: TPersistent); override;
    method BeginUpdate; virtual;
    method Clear;
    method Delete(aIndex: Integer);
    method EndUpdate; virtual;
    method FindItemID(aID: Integer): TCollectionItem;
    //method GetEnumerator: TCollectionEnumerator;
    //method GetNamePath: String; override;
    method Insert(aIndex: Integer): TCollectionItem;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read fItems.Count;
    property ItemClass: TCollectionItemClass read fItemClass;
    property Items[aIndex: Integer]: TCollectionItem read GetItem write SetItem;
  end;

implementation

method TClassActivator.CreateInstance(aType: &Type; aParameters: array of TObject): TObject;
begin
  {$IF ISLAND}
  var lCtor: MethodInfo;
  var lCtors := aType.Methods.Where(a -> ((MethodFlags.Constructor in a.Flags) and (a.Arguments.Count = 1)));
  if lCtors.Count > 1 then begin
    for each lTemp in lCtors do begin
      var lArguments := lTemp.Arguments.ToList;
      if lArguments[0].Type = typeOf(aParameters[0]) then begin
        lCtor := lTemp;
        break;
      end;
    end;
  end
  else
    lCtor := lCtors.FirstOrDefault;

  if lCtor = nil then raise new Exception('No default constructor can be found!');
  var lNew := DefaultGC.New(aType.RTTI, aType.SizeOfType);
  result := InternalCalls.Cast<TObject>(lNew);
  if aParameters.Length = 1 then begin
    var lCaller := TObjectCtor(lCtor.Pointer);
    lCaller(result, aParameters[0]);
  end
  else
    lCtor.Invoke(result, aParameters);
  {$ELSEIF ECHOES}
  result := TObject(Activator.CreateInstance(aType, aParameters));
  {$ELSEIF MACOS}
  var lInstanceType := aType;
  var lCtor: RemObjects.Elements.RTL.Reflection.Method := nil;
  var lParent: &Class := nil;
  var lCurrentClass := aType.TypeClass;
  while lCtor = nil do begin
    var lMethods := lInstanceType.Methods;
    for each lMethod in lMethods do begin
      if lMethod.Name = 'init:' then begin
        lCtor := lMethod;
        break;
      end;
    end;
    lParent := class_getSuperclass(lCurrentClass);
    if lCurrentClass ≠ lParent then begin
      lCurrentClass := lParent;
      if lParent ≠ nil then
        lInstanceType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lParent)
      else
        break;
    end
    else
      break;
  end;
  if lCtor = nil then raise new Exception('No constructor can be found!');

  var lNew := rtl.objc_msgSend(aType.TypeClass, NSSelectorFromString('alloc'));
  // TODO using aParameters: error, array of dynamic expected.
  result := rtl.objc_msgSend(lNew, lCtor.Selector, [aParameters[0]]);
  {$ENDIF}
end;

method TCollection.GetCapacity: Integer;
begin
  result := fItems.Capacity;
end;

method TCollection.GetPropName: String;
begin

end;

method TCollection.InsertItem(aItem: TCollectionItem);
begin

end;

method TCollection.RemoveItem(aItem: TCollectionItem);
begin

end;

method TCollection.SetCapacity(aValue: Integer);
begin
  fItems.Capacity := aValue;
end;

method TCollection.Notify(aItem: TCollectionItem; Action: TCollectionNotification);
begin

end;

method TCollection.Changed;
begin

end;

method TCollection.GetItem(aIndex: Integer): TCollectionItem;
begin
  result := fItems[aIndex];
end;

method TCollection.SetItem(aIndex: Integer; aValue: TCollectionItem);
begin
  fItems[aIndex] := aValue;
end;

method TCollection.SetItemName(aItem: TCollectionItem);
begin

end;

method TCollection.Update(aItem: TCollectionItem);
begin

end;

constructor TCollection(aItemClass: TCollectionItemClass);
begin
  fItems := new TList<TCollectionItem>();
  fItemClass := aItemClass;
end;

method TCollection.Owner: TPersistent;
begin
  result := GetOwner;
end;

method TCollection.Add: TCollectionItem;
begin
  result := TCollectionItem(TClassActivator.CreateInstance(fItemClass, [self]));
  fItems.Add(result);
end;

method TCollection.BeginUpdate;
begin

end;

method TCollection.Clear;
begin
  fItems.Clear;
end;

method TCollection.Delete(aIndex: Integer);
begin
  fItems.Delete(aIndex);
end;

method TCollection.EndUpdate;
begin

end;

method TCollection.FindItemID(aID: Integer): TCollectionItem;
begin

end;

method TCollection.Insert(aIndex: Integer): TCollectionItem;
begin

end;

method TCollectionItem.GetIndex: Integer;
begin

end;

method TCollectionItem.Changed(AllItems: Boolean);
begin

end;

method TCollectionItem.GetDisplayName: String;
begin

end;

method TCollectionItem.SetCollection(aValue: TCollection);
begin

end;

method TCollectionItem.SetIndex(aValue: Integer);
begin

end;

method TCollectionItem.SetDisplayName(aValue: String);
begin

end;

constructor TCollectionItem(aCollection: TCollection);
begin
  fCollection := aCollection;
end;

end.