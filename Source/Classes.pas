namespace Elements.RTL.Delphi;

interface

type
  TStringsOption = public enum (soStrictDelimiter, soWriteBOM, soTrailingLineBreak, soUseLocale) of Integer;
  TStringsOptions = public set of TStringsOption;
  TDuplicates = public (dupIgnore, dupAccept, dupError);
  TInternalItem = tuple of (DelphiString, TObject);
  TStringListSortCompare = public block(x: TInternalItem; y: TInternalItem): Integer;
  TNotifyEvent = public block(Sender: TObject);

  TPersistent = public class(TObject)
  private
    method AssignError(Source: TPersistent);
  protected
    method AssignTo(Dest: TPersistent); virtual;
    //method DefineProperties(Filer: TFiler); virtual;
    method GetOwner: TPersistent; virtual;
  public
    //destructor Destroy; override;
    method Assign(Source: TPersistent); virtual;
    method GetNamePath: DelphiString; virtual;
  end;

  TStrings = public abstract class(TPersistent)
  private
    fDelimiter: Char;
    fQuoteChar: Char;
    fOptions: TStringsOptions;
    fUpdateCount: Integer;
    function GetUseLocale: Boolean;
    procedure SetUseLocale(Value: Boolean);
  protected
    /*
    method Error(const Msg: DelphiString; Data: Integer); 
    method Error(Msg: PResDelphiStringRec; Data: Integer); 
    method ExtractName(const S: DelphiString): DelphiString;  inline;
    method ExtractName(const S: DelphiString; AllNames: Boolean): DelphiString; */
    method Get(aIndex: Integer): DelphiString; virtual; abstract;
    method GetCapacity: Integer; virtual;
    method GetCount: Integer; virtual; abstract;
    method GetObject(Index: Integer): TObject; virtual;
    /*method GetTextStr: DelphiString; virtual;*/
    method Put(aIndex: Integer; const S: DelphiString); virtual;
    method PutObject(aIndex: Integer; aObject: TObject); virtual;
    method SetCapacity(NewCapacity: Integer); virtual;
    /*method SetEncoding(const Value: TEncoding); virtual;
    method SetTextStr(const Value: DelphiString); virtual;*/
    method SetUpdateState(Updating: Boolean); virtual;
    method CompareStrings(const S1, S2: DelphiString): Integer; virtual;
    property UpdateCount: Integer read fUpdateCount;
  public
/*    constructor;
    //destructor Destroy; override; */
    method &Add(const S: DelphiString): Integer; virtual;
    /*method AddPair(const Name, Value: DelphiString): TDelphiStrings; 
    method AddPair(const Name, Value: DelphiString; AObject: TObject): TDelphiStrings;*/
    method AddObject(const S: DelphiString; aObject: TObject): Integer; virtual;
    /*method Append(const S: DelphiString);
    method AddDelphiStrings(DelphiStrings: TDelphiStrings);  virtual;
    method AddDelphiStrings(const DelphiStrings: TArray<DelphiString>); 
    method AddDelphiStrings(const DelphiStrings: TArray<DelphiString>; const Objects: TArray<TObject>); 
    method Assign(Source: TPersistent); override;
    method SetDelphiStrings(Source: TStrings);*/
    method BeginUpdate;
    method Clear; virtual; abstract;
    method Delete(Index: Integer); virtual; abstract;
    method EndUpdate;
    /*method Equals(DelphiStrings: TStrings): Boolean; reintroduce;*/
    method Exchange(Index1, Index2: Integer); virtual;
    /*method GetEnumerator: TDelphiStringsEnumerator;
    method GetText: PChar; virtual; */
    method IndexOf(const S: DelphiString): Integer; virtual;
    /*method IndexOfName(const Name: DelphiString): Integer; virtual;
    method IndexOfObject(AObject: TObject): Integer; virtual;*/
    method Insert(aIndex: Integer; const S: DelphiString); virtual; abstract;
    method InsertObject(aIndex: Integer; const S: DelphiString; aObject: TObject); virtual;
    /*method LoadFromFile(const FileName: DelphiString);  virtual;
    method LoadFromFile(const FileName: DelphiString; Encoding: TEncoding);  virtual;
    method LoadFromStream(Stream: TStream);  virtual;
    method LoadFromStream(Stream: TStream; Encoding: TEncoding);  virtual;
    method Move(CurIndex, NewIndex: Integer); virtual;
    method SaveToFile(const FileName: DelphiString);  virtual;
    method SaveToFile(const FileName: DelphiString; Encoding: TEncoding);  virtual;
    method SaveToStream(Stream: TStream);  virtual;
    method SaveToStream(Stream: TStream; Encoding: TEncoding);  virtual;
    method SetText(Text: PChar); virtual;
    method ToDelphiStringArray: TArray<DelphiString>;
    method ToObjectArray: TArray<TObject>;
    property Updating: Boolean read GetUpdating;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: DelphiString read GetCommaText write SetCommaText; */
    property Count: Integer read GetCount;
    /*property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding; */
    property Delimiter: Char read fDelimiter write fDelimiter;
    /*property DelimitedText: DelphiString read GetDelimitedText write SetDelimitedText;
    property Encoding: TEncoding read FEncoding;
    property LineBreak: DelphiString read FLineBreak write FLineBreak;
    property Names[Index: Integer]: DelphiString read GetName;
    property KeyNames[Index: Integer]: DelphiString read GetKeyName;*/
    property Objects[aIndex: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char read fQuoteChar write fQuoteChar;
    /*property Values[const Name: DelphiString]: DelphiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: DelphiString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read FNameValueSeparator write FNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;*/
    property Strings[aIndex: Integer]: DelphiString read Get write Put; default;
    /*property Text: DelphiString read GetTextStr write SetTextStr;
    property DelphiStringsAdapter: IDelphiStringsAdapter read FAdapter write SetDelphiStringsAdapter;
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;
    property TrailingLineBreak: Boolean read GetTrailingLineBreak write SetTrailingLineBreak;*/
    property UseLocale: Boolean read GetUseLocale write SetUseLocale;
    property Options: TStringsOptions read fOptions write fOptions;
  end;

  TStringList = public class(TStrings)
  private
    fList: Sugar.Collections.List<TInternalItem>;
    fDuplicates: TDuplicates;
    fSorted: Boolean;
    fCaseSensitive: Boolean;
    fOwnsObject: Boolean;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    procedure Setup;
    method SetSorted(Value: Boolean);
    method SetCaseSensitive(const Value: Boolean);
  protected
    method Changed; virtual;
    method Changing; virtual;
    method Get(aIndex: Integer): DelphiString; override;
    method GetCapacity: Integer; override;
    method GetObject(aIndex: Integer): TObject; override;
    method Put(aIndex: Integer; const S: DelphiString); override;
    method PutObject(aIndex: Integer; aObject: TObject); override;
    method SetCapacity(NewCapacity: Integer); override;
    method SetUpdateState(Updating: Boolean); override;
    method CompareStrings(const S1, S2: DelphiString): Integer; override;
    method InsertItem(aIndex: Integer; const S: DelphiString; aObject: TObject); virtual;
  public
    constructor;
    constructor(aOwnsObject: Boolean);
    constructor(aQuoteChar: Char; aDelimiter: Char);
    constructor(aQuoteChar: Char; aDelimiter: Char; aOptions: TStringsOptions);
    constructor(aDuplicates: TDuplicates; aSorted: Boolean; aCaseSensitive: Boolean);
    //destructor Destroy; override;
    method &Add(const S: DelphiString): Integer; override;
    method AddObject(const S: DelphiString; aObject: TObject): Integer; override;
    method Assign(Source: TPersistent); override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Exchange(Index1, Index2: Integer); override;
    method Find(const S: DelphiString; var aIndex: Integer): Boolean; virtual;
    method IndexOf(const S: DelphiString): Integer; override;
    method Insert(aIndex: Integer; const S: DelphiString); override; inline;
    method InsertObject(aIndex: Integer; const S: DelphiString; aObject: TObject); override;
    method Sort; virtual;
    method CustomSort(Compare: TStringListSortCompare); virtual;
    method GetCount: Integer; override;
    property Duplicates: TDuplicates read fDuplicates write fDuplicates;
    property Sorted: Boolean read fSorted write SetSorted;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OwnsObjects: Boolean read fOwnsObject write fOwnsObject;
  end;

implementation

method TPersistent.AssignError(Source: TPersistent);
begin

end;

method TPersistent.AssignTo(Dest: TPersistent);
begin
  Dest.AssignError(Self);
end;

method TPersistent.GetOwner: TPersistent;
begin
  result := nil;
end;

method TPersistent.Assign(Source: TPersistent);
begin
  if Source <> nil then 
    Source.AssignTo(Self) 
  else 
    AssignError(nil);
end;

method TPersistent.GetNamePath: DelphiString;
begin

end;

method TStringList.Setup;
begin
  fList := new Sugar.Collections.List<TInternalItem>;
end;

method TStringList.SetSorted(Value: Boolean);
begin
  if Value <> fSorted then begin
     if Value then
       Sort;
    fSorted := Value;
  end;
end;

method TStringList.SetCaseSensitive(Value: Boolean);
begin
  if Value <> fCaseSensitive then begin
    fCaseSensitive := Value;
    if Sorted then begin
      Sorted := false;
      Sort;
      Sorted := true;
    end;
  end;
end;

method TStringList.Changed;
begin
  if (UpdateCount = 0) and assigned(fOnChange) then
    fOnChange(self);
end;

method TStringList.Changing;
begin
  if (UpdateCount = 0) and assigned(fOnChanging) then
    fOnChanging(self);
end;

method TStringList.Get(aIndex: Integer): DelphiString;
begin
  if aIndex >= Count then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "Get aIndex");
  result := fList[aIndex][0];
end;

method TStringList.GetCapacity: Integer;
begin
  result := fList.Count;
end;

method TStringList.GetCount: Integer;
begin
  result := fList.Count;
end;

method TStringList.GetObject(aIndex: Integer): TObject;
begin
  if aIndex >= Count then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "GetObject aIndex");
  result := fList[aIndex][1];
end;

method TStringList.Put(aIndex: Integer; S: DelphiString);
begin
  if Sorted then
    raise new Sugar.SugarException("Can not modify a sorted string");
  if aIndex >= Count then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "Put aIndex");

  Changing;
  var lObject := fList[aIndex][1];
  fList[aIndex] := (S, lObject);
  Changed;
end;

method TStringList.PutObject(aIndex: Integer; aObject: TObject);
begin
  if aIndex >= Count then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "Put aIndex");

  Changing;
  var lString := fList[aIndex][0];
  fList[aIndex] := (lString, aObject);
  Changed;
end;

method TStringList.SetCapacity(NewCapacity: Integer);
begin
  //NO OP, for compatibility
end;

method TStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing 
  else
    Changed;
end;

method TStringList.CompareStrings(S1: DelphiString; S2: DelphiString): Integer;
begin
  if UseLocale then begin
    if CaseSensitive then
      result := DelphiString.Compare(S1, S2, False)
    else
      result := DelphiString.Compare(S1, S2, True)
  end
  else
    if CaseSensitive then
      result := DelphiString.CompareOrdinal(S1, S2)
    else
      result := DelphiString.CompareText(S1, S2);
end;

method TStringList.InsertItem(aIndex: Integer; S: DelphiString; aObject: TObject);
begin
  Changing;
  fList.insert(aIndex, (S, aObject));
  Changed;
end;

constructor TStringList;
begin
  Setup;
end;

constructor TStringList(aOwnsObject: Boolean);
begin
  Setup;
  fOwnsObject := aOwnsObject;
end;

constructor TStringList(aQuoteChar: Char; aDelimiter: Char);
begin
  Setup;
  QuoteChar := aQuoteChar;
  Delimiter := aDelimiter;
end;

constructor TStringList(aQuoteChar: Char; aDelimiter: Char; aOptions: TStringsOptions);
begin
  Setup;
  QuoteChar := aQuoteChar;
  Delimiter := aDelimiter;
  Options := aOptions;
end;

constructor TStringList(aDuplicates: TDuplicates; aSorted: Boolean; aCaseSensitive: Boolean);
begin
  Setup;
  Duplicates := aDuplicates;
  Sorted := aSorted;
  CaseSensitive := aCaseSensitive;
end;

method TStringList.&Add(S: DelphiString): Integer;
begin
  result := AddObject(S, nil);
end;

method TStringList.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  if not Sorted then
    result := Count
  else
    if Find(S, var result) then
      case Duplicates of
        TDuplicates.dupIgnore: exit;
        TDuplicates.dupError: raise new Sugar.SugarException("Duplicates not allowed");
      end;

  InsertItem(result, S, aObject);
end;

method TStringList.Assign(Source: TPersistent);
begin
  if Source is TStringList then begin
    fDuplicates := TStringList(Source).fDuplicates;
    fSorted := TStringList(Source).fSorted;
    fCaseSensitive := TStringList(Source).fCaseSensitive;
  end;
  inherited Assign(Source);
end;

method TStringList.Clear;
begin
  fList.Clear;
end;

method TStringList.Delete(aIndex: Integer);
begin
  if (aIndex >= Count) or (aIndex < 0) then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "Exchange Index1 Index2");

  fList.removeAt(aIndex);
end;

method TStringList.Exchange(Index1: Integer; Index2: Integer);
begin
  if (Index1 >= Count) or (Index1 < 0) or (Index2 >= Count) or (Index2 < 0) then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "Exchange Index1 Index2");

  var lItem := fList[Index1];
  fList[Index1] := fList[Index2];
  fList[Index2] := lItem;
end;

method TStringList.Find(S: DelphiString; var aIndex: Integer): Boolean;
begin
  result := False;
  var l := 0;
  var h := Count - 1;
  while l <= h do
  begin
    var i := (l + h) shr 1;
    var c := CompareStrings(fList[i][0], S);
    if c < 0 then l := i + 1 else
    begin
      h := i - 1;
      if c = 0 then
      begin
        result := true;
        if Duplicates <> TDuplicates.dupAccept then
          l := i;
      end;
    end;
  end;
  aIndex := l;
end;

method TStringList.IndexOf(S: DelphiString): Integer;
begin
  if not Sorted then 
    result := inherited IndexOf(S)
  else
    if not Find(S, var result) then 
      result := -1;
end;

method TStringList.Insert(aIndex: Integer; S: DelphiString);
begin
  InsertObject(aIndex, S, nil);
end;

method TStringList.InsertObject(aIndex: Integer; S: DelphiString; aObject: TObject);
begin
  if Sorted then
    raise new Sugar.SugarException("Can not insert in a sorted list");
  if aIndex >= Count then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "InsertObject aIndex");
  
  InsertItem(aIndex, S, aObject);
end;

method TStringList.Sort;
begin
  CustomSort((x, y) -> begin result := CompareStrings(x[0], y[0]) end);
end;

method TStringList.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted then begin
    Changing;
    fList.Sort((x, y) -> Compare(x, y));
    Changed;
  end;
end;

method TStrings.&Add(S: DelphiString): Integer;
begin
  result := GetCount;
  Insert(result, S);
end;

method TStrings.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  result := &Add(S);
  PutObject(result, aObject);
end;

method TStrings.PutObject(aIndex: Integer; aObject: TObject);
begin
  // Empty method
end;

method TStrings.Exchange(Index1: Integer; Index2: Integer);
begin
  var lTempObject: TObject;
  var lTempString: String;
  BeginUpdate;
  try
    lTempString := Strings[Index1];
    lTempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := lTempString;
    Objects[Index2] := lTempObject; 

  finally
    EndUpdate;
  end;
end;

method TStrings.BeginUpdate;
begin
  if fUpdateCount = 0 then
    SetUpdateState(true);
  inc(fUpdateCount);
end;

method TStrings.EndUpdate;
begin
  dec(fUpdateCount);
  if fUpdateCount = 0 then
    SetUpdateState(false);
end;

method TStrings.SetUpdateState(Updating: Boolean);
begin
  // Empty method
end;

method TStrings.GetObject(&Index: Integer): TObject;
begin
  result := nil;
end;

method TStrings.Put(aIndex: Integer; S: DelphiString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(aIndex);
  Delete(aIndex);
  InsertObject(aIndex, S, TempObject);
end;

method TStrings.InsertObject(aIndex: Integer; const S: DelphiString; aObject: TObject);
begin
  Insert(aIndex, S);
  PutObject(aIndex, aObject);
end;

method TStrings.CompareStrings(const S1, S2: DelphiString): Integer;
begin
  if UseLocale then
    result := DelphiString.Compare(S1, S2, True)
  else
    result := DelphiString.CompareText(S1, S2);
end;

method TStrings.GetCapacity: Integer;
begin
  result := Count;
end;

method TStrings.IndexOf(const S: DelphiString): Integer;
begin
  for i: Integer := 0 to GetCount - 1 do
    if CompareStrings(Get(i), S) = 0 then 
      exit(i);
  result := -1;
end;

method TStrings.SetCapacity(NewCapacity: Integer);
begin
  // Empty method
end;

method TStrings.GetUseLocale: Boolean;
begin
  result := TStringsOption.soUseLocale in Options;
end;

method TStrings.SetUseLocale(Value: Boolean);
begin
  if Value then
    fOptions := fOptions + [TStringsOption.soUseLocale]
  else
    fOptions := fOptions - [TStringsOption.soUseLocale];
end;

end.
