namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  Sugar;

type
  TStringsOption = public enum (soStrictDelimiter, soWriteBOM, soTrailingLineBreak, soUseLocale) of Integer;
  TStringsOptions = public set of TStringsOption;
  TDuplicates = public enum (dupIgnore, dupAccept, dupError) of Integer;
  TInternalItem = tuple of (DelphiString, TObject);
  TStringListSortCompare = public block(x: TInternalItem; y: TInternalItem): Integer;
  TNotifyEvent = public block(Sender: TObject);

  TStrings = public abstract class(TPersistent)
  private
    fUpdateCount: Integer;
    fOptions: TStringsOptions;
    function GetUseLocale: Boolean;
    procedure SetUseLocale(aValue: Boolean);
    method GetUpdating: Boolean;
    method GetName(aIndex: Integer): DelphiString;
    method GetKeyName(aIndex: Integer): DelphiString;
    method GetValue(aName: DelphiString): DelphiString;
    method SetValue(aName: DelphiString; aValue: DelphiString);
    method GetValueFromIndex(aIndex: Integer): DelphiString;
    method SetValueFromIndex(aIndex: Integer; Value: DelphiString);    
    method GetCommaText: DelphiString;
    method SetCommaText(aValue: DelphiString);
    method GetDelimitedText: DelphiString;
    method SetDelimitedText(aValue: DelphiString);
    method GetStrictDelimiter: Boolean;
    method SetStrictDelimiter(aValue: Boolean);
    method GetTrailingLineBreak: Boolean; inline;
    method SetTrailingLineBreak(aValue: Boolean);
    method GetDelimitedTextWithChars(aDelimiter: Char; aQuote: Char): DelphiString;
    method SetDelimitedTextWithChars(aValue: DelphiString; aDelimiter: Char; aQuote: Char);
    method IndexOfDelimiter(aString: DelphiString; aDelimiter: Char; aQuote: Char): Integer;
  protected
    method Error(const Msg: DelphiString; Data: Integer);
    method ExtractName(const S: DelphiString): DelphiString;  inline;
    method ExtractName(const S: DelphiString; AllNames: Boolean): DelphiString; 
    method Get(aIndex: Integer): DelphiString; virtual; abstract;
    method GetCapacity: Integer; virtual;
    method GetCount: Integer; virtual; abstract;
    method GetObject(aIndex: Integer): TObject; virtual;
    method GetTextStr: DelphiString; virtual;
    method Put(aIndex: Integer; const S: DelphiString); virtual;
    method PutObject(aIndex: Integer; aObject: TObject); virtual;
    method SetCapacity(aNewCapacity: Integer); virtual;
    /*method SetEncoding(const Value: TEncoding); virtual;*/
    method SetTextStr(const aValue: DelphiString); virtual;
    method SetUpdateState(aUpdating: Boolean); virtual;
    method CompareStrings(const S1, S2: DelphiString): Integer; virtual;
    property UpdateCount: Integer read fUpdateCount;
  public
    /*constructor;
    destructor Destroy; override;*/
    method &Add(const S: DelphiString): Integer; virtual;
    method AddPair(const aName: DelphiString; const aValue: DelphiString): TStrings;
    method AddPair(const aName: DelphiString; const aValue: DelphiString; aObject: TObject): TStrings;
    method AddObject(const S: DelphiString; aObject: TObject): Integer; virtual;
    method Append(const S: DelphiString); inline;
    method AddStrings(aStrings: TStrings);  virtual;
    method AddStrings(const aStrings: array of DelphiString); 
    method AddStrings(const aStrings: array of DelphiString; const aObjects: array of TObject); 
    method Assign(aSource: TPersistent); override;
    method SetStrings(aSource: TStrings);
    method BeginUpdate;
    method Clear; virtual; abstract;
    method Delete(aIndex: Integer); virtual; abstract;
    method EndUpdate;
    method &Equals(aStrings: TStrings): Boolean;
    method Exchange(aIndex1, aIndex2: Integer); virtual;
    /*method GetEnumerator: TStringsEnumerator; */
    method IndexOf(const S: DelphiString): Integer; virtual;
    method IndexOfName(const aName: DelphiString): Integer; virtual;
    method IndexOfObject(aObject: TObject): Integer; virtual;
    method Insert(aIndex: Integer; const S: DelphiString); virtual; abstract;
    method InsertObject(aIndex: Integer; const S: DelphiString; aObject: TObject); virtual;
    method LoadFromFile(const aFileName: DelphiString); virtual;
    method LoadFromFile(const aFileName: DelphiString; aEncoding: TEncoding);  virtual;
    /*method LoadFromStream(Stream: TStream);  virtual;
    method LoadFromStream(Stream: TStream; Encoding: TEncoding);  virtual;*/
    method Move(aCurIndex, aNewIndex: Integer); virtual;
    /*method SaveToFile(const FileName: DelphiString);  virtual;
    method SaveToFile(const FileName: DelphiString; Encoding: TEncoding);  virtual;
    method SaveToStream(Stream: TStream);  virtual;
    method SaveToStream(Stream: TStream; Encoding: TEncoding);  virtual; */
    method ToStringArray: array of DelphiString;
    method ToObjectArray: array of TObject;
    property Updating: Boolean read GetUpdating;
    property Capacity: Integer read GetCapacity;
    property CommaText: DelphiString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    /*property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding; */
    property Delimiter: Char := ',';
    property DelimitedText: DelphiString read GetDelimitedText write SetDelimitedText;
    /*property Encoding: TEncoding read FEncoding;*/
    property LineBreak: DelphiString;
    property Names[Index: Integer]: DelphiString read GetName;
    property KeyNames[Index: Integer]: DelphiString read GetKeyName;
    property Objects[aIndex: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char := '"';
    property Values[const Name: DelphiString]: DelphiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: DelphiString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char :='=';
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[aIndex: Integer]: DelphiString read Get write Put; default;
    property Text: DelphiString read GetTextStr write SetTextStr;
    /*property DelphiStringsAdapter: IDelphiStringsAdapter read FAdapter write SeTStringsAdapter;
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;*/
    property TrailingLineBreak: Boolean read GetTrailingLineBreak write SetTrailingLineBreak;
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
    method GetObject(aIndex: Integer): TObject; override;
    method Put(aIndex: Integer; const S: DelphiString); override;
    method PutObject(aIndex: Integer; aObject: TObject); override;
    method SetCapacity(aNewCapacity: Integer); override;
    method SetUpdateState(aUpdating: Boolean); override;
    method CompareStrings(const S1, S2: DelphiString): Integer; override;
    method InsertItem(aIndex: Integer; const S: DelphiString; aObject: TObject); virtual;
  public
    constructor;
    constructor(aOwnsObject: Boolean);
    constructor(aQuoteChar: Char; aDelimiter: Char);
    constructor(aQuoteChar: Char; aDelimiter: Char; aOptions: TStringsOptions);
    constructor(aDuplicates: TDuplicates; aSorted: Boolean; aCaseSensitive: Boolean);
    //destructor Destroy; override;
    method GetCapacity: Integer; override;
    method &Add(const S: DelphiString): Integer; override;
    method AddObject(const S: DelphiString; aObject: TObject): Integer; override;
    method Assign(aSource: TPersistent); override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Exchange(aIndex1, aIndex2: Integer); override;
    method Find(const S: DelphiString; var aIndex: Integer): Boolean; virtual;
    method IndexOf(const S: DelphiString): Integer; override;
    method Insert(aIndex: Integer; const S: DelphiString); override;
    method InsertObject(aIndex: Integer; const S: DelphiString; aObject: TObject); override;
    method Sort; virtual;
    method CustomSort(aCompare: TStringListSortCompare); virtual;
    method GetCount: Integer; override;
    property Duplicates: TDuplicates read fDuplicates write fDuplicates;
    property Sorted: Boolean read fSorted write SetSorted;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OwnsObjects: Boolean read fOwnsObject write fOwnsObject;
  end;

implementation

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

method TStringList.Put(aIndex: Integer; const S: DelphiString);
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

method TStringList.SetCapacity(aNewCapacity: Integer);
begin
  //NO OP, for compatibility
end;

method TStringList.SetUpdateState(aUpdating: Boolean);
begin
  if aUpdating then
    Changing 
  else
    Changed;
end;

method TStringList.CompareStrings(const S1: DelphiString; const S2: DelphiString): Integer;
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

method TStringList.InsertItem(aIndex: Integer; const S: DelphiString; aObject: TObject);
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

method TStringList.&Add(const S: DelphiString): Integer;
begin
  result := AddObject(S, nil);
end;

method TStringList.AddObject(const S: DelphiString; aObject: TObject): Integer;
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

method TStringList.Assign(aSource: TPersistent);
begin
  if aSource is TStringList then begin
    fDuplicates := TStringList(aSource).fDuplicates;
    fSorted := TStringList(aSource).fSorted;
    fCaseSensitive := TStringList(aSource).fCaseSensitive;
  end;
  inherited Assign(aSource);
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

method TStringList.Exchange(aIndex1: Integer; aIndex2: Integer);
begin
  if (aIndex1 >= Count) or (aIndex1 < 0) or (aIndex2 >= Count) or (aIndex2 < 0) then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "Exchange Index1 Index2");

  var lItem := fList[aIndex1];
  fList[aIndex1] := fList[aIndex2];
  fList[aIndex2] := lItem;
end;

method TStringList.Find(const S: DelphiString; var aIndex: Integer): Boolean;
begin
  result := false;
  var lLow := 0;
  var lHigh := Count - 1;
  while lLow <= lHigh do
  begin
    var i := (lLow + lHigh) shr 1;
    var lRes := CompareStrings(fList[i][0], S);
    if lRes < 0 then
      lLow := i + 1
    else begin
      lHigh := i - 1;
      if lRes = 0 then
      begin
        result := true;
        if Duplicates <> TDuplicates.dupAccept then
          lLow := i;
      end;
    end;
  end;
  aIndex := lLow;
end;

method TStringList.IndexOf(const S: DelphiString): Integer;
begin
  if not Sorted then 
    result := inherited IndexOf(S)
  else
    if not Find(S, var result) then 
      result := -1;
end;

method TStringList.Insert(aIndex: Integer; const S: DelphiString);
begin
  InsertObject(aIndex, S, nil);
end;

method TStringList.InsertObject(aIndex: Integer; const S: DelphiString; aObject: TObject);
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

method TStringList.CustomSort(aCompare: TStringListSortCompare);
begin
  if not Sorted then begin
    Changing;
    fList.Sort((x, y) -> aCompare(x, y));
    Changed;
  end;
end;

method TStrings.&Add(const S: DelphiString): Integer;
begin
  result := GetCount;
  Insert(result, S);
end;

method TStrings.AddObject(const S: DelphiString; aObject: TObject): Integer;
begin
  result := &Add(S);
  PutObject(result, aObject);
end;

method TStrings.PutObject(aIndex: Integer; aObject: TObject);
begin
  // Empty method
end;

method TStrings.Exchange(aIndex1: Integer; aIndex2: Integer);
begin
  var lTempObject: TObject;
  var lTempString: String;
  BeginUpdate;
  try
    lTempString := Strings[aIndex1];
    lTempObject := Objects[aIndex1];
    Strings[aIndex1] := Strings[aIndex2];
    Objects[aIndex1] := Objects[aIndex2];
    Strings[aIndex2] := lTempString;
    Objects[aIndex2] := lTempObject; 

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

method TStrings.SetUpdateState(aUpdating: Boolean);
begin
  // Empty method
end;

method TStrings.GetObject(aIndex: Integer): TObject;
begin
  result := nil;
end;

method TStrings.Put(aIndex: Integer; const S: DelphiString);
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

method TStrings.SetCapacity(aNewCapacity: Integer);
begin
  // Empty method
end;

method TStrings.GetUseLocale: Boolean;
begin
  result := TStringsOption.soUseLocale in Options;
end;

method TStrings.SetUseLocale(aValue: Boolean);
begin
  if aValue then
    fOptions := fOptions + [TStringsOption.soUseLocale]
  else
    fOptions := fOptions - [TStringsOption.soUseLocale];
end;

method TStrings.ExtractName(const S: DelphiString): DelphiString;
begin
  result := ExtractName(S, False);
end;

method TStrings.ExtractName(const S: DelphiString; AllNames: Boolean): DelphiString;
begin
  var lPos := S.IndexOf(NameValueSeparator);
  if lPos >=0 then
    result := S.SubString(0, lPos)
  else
    if AllNames then
      result := S
    else
      result := '';
end;

method TStrings.AddPair(const aName: DelphiString; const aValue: DelphiString): TStrings;
begin
  &Add(aName + NameValueSeparator + aValue);
  result := self;
end;

method TStrings.AddPair(const aName: DelphiString; const aValue: DelphiString; aObject: TObject): TStrings;
begin
  AddObject(aName + NameValueSeparator + aValue, aObject);
  result := self;
end;


method TStrings.Append(const S: DelphiString);
begin
  &Add(S);
end;

method TStrings.AddStrings(aStrings: TStrings);
begin
  BeginUpdate;
  try
    for i: Integer := 0 to aStrings.GetCount - 1 do
      AddObject(aStrings[i], aStrings.Objects[i]);
  finally
    EndUpdate;
  end;
end;

method TStrings.AddStrings(const aStrings: array of DelphiString);
begin
  BeginUpdate;
  try
    for i: Integer := 0 to aStrings.length - 1 do
      &Add(aStrings[i]);
  finally
    EndUpdate;
  end;
end;

method TStrings.AddStrings(const aStrings: array of DelphiString; const aObjects: array of TObject);
begin
  if aStrings.length <> aObjects.length then
    raise new Sugar.SugarArgumentOutOfRangeException(Sugar.ErrorMessage.ARG_OUT_OF_RANGE_ERROR, "AddStrings aStrings aObjects");
  
  BeginUpdate;
  try
    for i: Integer := 0 to aStrings.length - 1 do
      AddObject(aStrings[i], aObjects[i]);
  finally
    EndUpdate;
  end;
end;

method TStrings.Assign(aSource: TPersistent);
begin
  if aSource is TStrings then begin
    BeginUpdate;
    try
      Clear;
      var lSource := TStrings(aSource);
      LineBreak := lSource.LineBreak;
      Delimiter := lSource.Delimiter;
      QuoteChar := lSource.QuoteChar;
      NameValueSeparator := lSource.NameValueSeparator;
      Options := lSource.Options;
      AddStrings(lSource);
    finally
      EndUpdate;
    end;
    exit;
  end;
  inherited Assign(aSource);
end;

method TStrings.SetStrings(aSource: TStrings);
begin
  BeginUpdate;
  try
    Clear;
    AddStrings(aSource);
  finally
    EndUpdate;
  end;
end;

method TStrings.IndexOfName(const aName: DelphiString): Integer;
begin
  for i: Integer := 0 to Count - 1 do begin 
    var lPos := Strings[i].IndexOf(NameValueSeparator);
    if (lPos >= 0) and (DelphiString.Compare(Strings[i].SubString(0, lPos), aName) = 0) then
        exit i;
  end;
  result := -1;
end;

method TStrings.IndexOfObject(aObject: TObject): Integer;
begin
  for i: Integer := 0 to Count - 1 do
    if Objects[i] = aObject then
      exit i;

  result := -1;
end;

method TStrings.ToStringArray: array of DelphiString;
begin
  result := new DelphiString[Count];
  for i: Integer := 0 to Count - 1 do
    result[i] := Strings[i];
end;

method TStrings.ToObjectArray: array of TObject;
begin
  result := new TObject[Count];
  for i: Integer := 0 to Count - 1 do
    result[i] := Objects[i];
end;

method TStrings.GetUpdating: Boolean;
begin
  result := UpdateCount > 0;
end;

method TStrings.GetName(aIndex: Integer): DelphiString;
begin
  result := ExtractName(Strings[aIndex], false);
end;

method TStrings.GetKeyName(aIndex: Integer): DelphiString;
begin
  result := ExtractName(Strings[aIndex], true);
end;

method TStrings.GetValue(aName: DelphiString): DelphiString;
begin
  var lIndex := IndexOfName(aName);
  if lIndex >= 0 then
    result := Strings[lIndex].SubString(aName.Length + 1)
  else
    result := '';
end;

method TStrings.SetValue(aName: DelphiString; aValue: DelphiString);
begin
  var lIndex := IndexOfName(aName);
  if aValue <> '' then begin
    if lIndex < 0 then
      &Add(aName + NameValueSeparator + aValue)
    else
      Put(lIndex, aName + NameValueSeparator + aValue);
  end
  else
    if lIndex >= 0 then
      Delete(lIndex);
end;

method TStrings.GetValueFromIndex(aIndex: Integer): DelphiString;
begin
  var lPos := Strings[aIndex].IndexOf(NameValueSeparator);
  if lPos >= 0 then
    result := Strings[aIndex].SubString(lPos + 1)
  else
    result := '';
end;

method TStrings.SetValueFromIndex(aIndex: Integer; Value: DelphiString);
begin
  if Value = '' then
    Delete(aIndex)
  else begin
    var lPos := Strings[aIndex].IndexOf(NameValueSeparator);
    if lPos >= 0 then
    Put(aIndex, Strings[aIndex].SubString(0, lPos) + NameValueSeparator + Value);    
  end;
end;

method TStrings.Equals(aStrings: TStrings): Boolean;
begin
  if aStrings.Count <> Count then
    exit false;
  for i: Integer := 0 to Count - 1 do
    if aStrings[i] <> Strings[i] then
      exit false;

  result := true;
end;

method TStrings.GetTextStr: DelphiString;
begin
  var lSb := new StringBuilder;
  for i: Integer := 0 to Count - 1 do begin
    lSb.Append(Strings[i]);
    if (i <> Count - 1) or ((i = Count - 1) and TrailingLineBreak) then
      lSb.Append(LineBreak);
  end;
  result := lSb.ToString;
end;

method TStrings.SetTextStr(const aValue: DelphiString);
begin
  BeginUpdate;
  try
    Clear;
    var lArray := aValue.Split([LineBreak]);
    AddStrings(lArray);

  finally
    EndUpdate;
  end;    
end;

method TStrings.GetCommaText: DelphiString;
begin
  result := GetDelimitedTextWithChars(',', '"');
end;

method TStrings.SetCommaText(aValue: DelphiString);
begin
  SetDelimitedTextWithChars(aValue, ',', '"');
end;

method TStrings.GetDelimitedText: DelphiString;
begin
  result := GetDelimitedTextWithChars(Delimiter, QuoteChar);
end;

method TStrings.SetDelimitedText(aValue: DelphiString);
begin
  SetDelimitedTextWithChars(aValue, Delimiter, QuoteChar);
end;

method TStrings.GetStrictDelimiter: Boolean;
begin
  result := TStringsOption.soStrictDelimiter in Options;
end;

method TStrings.SetStrictDelimiter(aValue: Boolean);
begin
  if aValue then
    fOptions := fOptions + [TStringsOption.soStrictDelimiter]
  else
    fOptions := fOptions - [TStringsOption.soStrictDelimiter];
end;

method TStrings.GetTrailingLineBreak: Boolean;
begin
  result := TStringsOption.soTrailingLineBreak in Options;
end;

method TStrings.SetTrailingLineBreak(aValue: Boolean);
begin
  if aValue then
    fOptions := fOptions + [TStringsOption.soTrailingLineBreak]
  else
    fOptions := fOptions - [TStringsOption.soTrailingLineBreak];
end;

method TStrings.IndexOfDelimiter(aString: DelphiString; aDelimiter: Char; aQuote: Char): Integer;
begin
  for j: Integer := 0 to aString.Length - 1 do begin
    var lChar := aString.Chars[j];
    if ((StrictDelimiter) and ((lChar = aDelimiter) or (lChar = aQuote))) or
     ((not StrictDelimiter) and (lChar < ' ') or ((lChar = aDelimiter) or (lChar = aQuote)))  then
      exit j;
  end;
  result := -1;
end;

method TStrings.GetDelimitedTextWithChars(aDelimiter: Char; aQuote: Char): DelphiString;
begin
  var lSb := new StringBuilder;
  for i: Integer := 0 to Count - 1 do begin
    var lTmp := Strings[i];    
    if IndexOfDelimiter(lTmp, aDelimiter, aQuote) >= 0 then
      lTmp := lTmp.QuotedString(aQuote);
    lSb.Append(lTmp);
    if i < Count - 1 then
     lSb.Append(aDelimiter);    
  end;
  result := lSb.ToString;
end;

method TStrings.SetDelimitedTextWithChars(aValue: DelphiString; aDelimiter: Char; aQuote: Char);
begin
  Clear;
  try
    var i := 0;
    var j := 0;
    var lTmp: DelphiString := '';
    while i <= aValue.Length - 1 do begin
      case aValue.Chars[i] of
        aQuote: begin
          var lInQuote := false;
          j := i + 1;
          while j <= aValue.Length - 1 do begin
            if (aValue.Chars[j] = aQuote) then
              lInQuote := not lInQuote
            else begin
              if (aValue.Chars[j] = aDelimiter) and (not lInQuote) then
                break;
              lInQuote := false;
            end;
            inc(j);
          end;
            lTmp := aValue.SubString(i, j - i).DeQuotedString(aQuote); 
        end;

        else begin
          j := i + 1;
          while (j <= aValue.Length) and (IndexOfDelimiter(aValue[j], aDelimiter, aQuote) < 0) do
            inc(j);        
          lTmp := aValue.SubString(i, j - i);
        end;
        &Add(lTmp);
        i := j + 1;
      end;
    end;

  finally
    EndUpdate;
  end;
end;

method TStrings.Move(aCurIndex: Integer; aNewIndex: Integer);
begin
  if aCurIndex <> aNewIndex then
  begin
    BeginUpdate;
    try
      var lString := Strings[aCurIndex];
      var lObject := Objects[aCurIndex];
      Delete(aCurIndex);
      InsertObject(aNewIndex, lString, lObject);
    finally
      EndUpdate;
    end;
  end;
end;

method TStrings.Error(const Msg: DelphiString; Data: Integer);
begin
  raise new Sugar.SugarException(Msg);
end;

method TStrings.LoadFromFile(const aFileName: DelphiString);
begin
  LoadFromFile(aFileName, TEncoding.Default);
end;

method TStrings.LoadFromFile(const aFileName: DelphiString; aEncoding: TEncoding);
begin
  var lHandle := new Sugar.IO.FileHandle(aFileName, Sugar.IO.FileOpenMode.ReadOnly);
  var lBuffer := new Byte[lHandle.Length];
  lHandle.Read(lBuffer, lHandle.Length);
  var lStr := aEncoding.GetString(lBuffer);
  var i := 0;
  var lFrom := 0;
  while i < lStr.Length do begin
    lFrom := i;
    while (lStr[i] in [#13, #10]) and (i < lStr.Length) do
      inc(i);
    &Add(lStr.SubString(lFrom, (i - lFrom)));
    if lStr[i] = #13 then begin
      inc(i);
      if lStr[i] = #10 then
        inc(i)
    end;
  end;  
end;

end.
