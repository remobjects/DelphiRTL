namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS OR LINUX) AND NOT DARWIN) OR ECHOESWPF OR (MACOS AND NOT (ISLAND AND DARWIN))}

interface

uses
  RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TButton = public partial class(TNativeControl)
  published
    constructor(aOwner: TComponent);
    class method Create(AOwner: TComponent): TButton;
  end;

  TEdit = public partial class(TNativeControl)
  protected
    method PlatformGetMaxLength: Integer; virtual; partial; empty;
    method PlatformSetMaxLength(aValue: Integer); virtual; partial; empty;
    method PlatformGetReadOnly: Boolean; virtual; partial; empty;
    method PlatformSetReadOnly(aValue: Boolean); virtual; partial; empty;
    method PlatformGetText: String; virtual; partial; empty;
    method PlatformSetText(aValue: String); virtual; partial; empty;
  published
    constructor(aOwner: TComponent);
    property MaxLength: Integer read PlatformGetMaxLength write PlatformSetMaxLength;
    property &ReadOnly: Boolean read PlatformGetReadOnly write PlatformSetReadOnly;
    property Text: String read PlatformGetText write PlatformSetText;
  end;

  //TLabel = public partial class({$IF ISLAND AND WINDOWS}TGraphicControl{$ELSE}TNativeControl{$ENDIF})
  TLabel = public partial class(TNativeControl)
  published
    class method Create(aOwner: TComponent): TLabel;
  end;

  {$IF NOT LINUX}
  TButtonControl = public partial class(TNativeControl)
  protected
    method PlatformGetChecked: Boolean; virtual; partial; empty;
    method PlatformSetChecked(aValue: Boolean); virtual; partial; empty;
  published
    constructor(aOwner: TComponent);
    property Checked: Boolean read PlatformGetChecked write PlatformSetChecked;
  end;

  TCheckBoxState = public enum (cbUnChecked = 0, cbChecked = 1, cbGrayed = 2) of Integer;

  TCheckBox = public partial class(TButtonControl)
  private
    fState: TCheckBoxState;
    fAllowGrayed: Boolean;
  protected
    method PlatformSetState(aValue: TCheckBoxState); partial; empty;
    method PlatformSetAllowGrayed(aValue: Boolean); partial; empty;
    method SetState(aValue: TCheckBoxState); virtual;
    method SetAllowGrayed(aValue: Boolean); virtual;
  published
    method Toggle; virtual;
    method Click; override;
    property AllowGrayed: Boolean read fAllowGrayed write SetAllowGrayed default false;
    property State: TCheckBoxState read fState write SetState default TCheckBoxState.cbUnchecked;
  end;

  TListControlItems = public partial class(TStringList)
  published
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; S: DelphiString); override;
    property ListControl: TListControl read write;
  end;

  TListControl = public partial abstract class(TNativeControl)
  protected
    method GetItemIndex: Integer; virtual; abstract;
    method SetItemIndex(aValue: Integer); virtual; abstract;
  published
    constructor(aOwner: TComponent);
    method AddItem(Item: DelphiString; aObject: TObject); virtual; abstract;
    method Clear; virtual; abstract;
    method ClearSelection; virtual; abstract;
    method DeleteSelected; virtual; abstract;
    method GetCount: Integer; virtual; abstract;
    method SelectAll; virtual; abstract;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  TMultiSelectListControl = public partial abstract class(TListControl)
  protected
    fMultiSelect: Boolean;
    method GetSelCount: Integer; virtual; abstract;
    method SetMultiSelect(aValue: Boolean); virtual; abstract;
  published
    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect default false;
    property SelCount: Integer read GetSelCount;
  end;

  TListBox = public partial class(TMultiSelectListControl)
  private
    fItems: TStrings;
    fItemHeight: Integer;
    method SetItems(aValue: TStrings);
  protected
    method GetItemIndex: Integer; override;
    method SetItemIndex(aValue: Integer); override;
    method GetSelCount: Integer; override;
    method SetMultiSelect(aValue: Boolean); override;
  published
    constructor(aOwner: TComponent);
    method Loaded; override;
    method AddItem(Item: DelphiString; aObject: TObject); override;
    method Clear; override;
    method ClearSelection; override;
    method DeleteSelected; override;
    method GetCount: Integer; override;
    method SelectAll; override;
    property Items: TStrings read fItems write SetItems;
    property ItemHeight: Integer read fItemHeight write fItemHeight;
    property Selected[aIndex: Integer]: Boolean read PlatformGetSelected write PlatformSetSelected;
  end;

  TComboBoxItems = public partial class(TStringList)
  published
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; S: DelphiString); override;
    property ListControl: TListControl read write;
  end;

  TComboBox = public partial class(TListControl)
  private
    fItems: TStrings;
    fOnSelect: TNotifyEvent;
    fOnChange: TNotifyEvent;
    fItemHeight: Integer;
    method SetOnSelect(aValue: TNotifyEvent);
    method SetOnChange(aValue: TNotifyEvent);
    method SetItems(aValue: TStrings);
  protected
    method GetItemIndex: Integer; override;
    method SetItemIndex(aValue: Integer); override;
  published
    constructor(aOwner: TComponent);
    method Loaded; override;
    method AddItem(Item: DelphiString; aObject: TObject); override;
    method Clear; override;
    method ClearSelection; override;
    method DeleteSelected; override;
    method GetCount: Integer; override;
    method SelectAll; override;
    property Items: TStrings read fItems write SetItems;
    property ItemHeight: Integer read fItemHeight write fItemHeight;
    property Text: String read PlatformGetText write PlatformSetText;
    property OnSelect: TNotifyEvent read fOnSelect write SetOnSelect;
    property OnChange: TNotifyEvent read fOnChange write SetOnChange;
  end;

  TGroupBox = public partial class(TNativeControl)
  protected
  end;

  {$IF WEBASSEMBLY}
  TMemoStrings = partial class(TStringList)
  private
    fMemo: TMemo;
  protected
    method Get(aIndex: Integer): DelphiString; override;
    method GetTextStr: DelphiString; override;
    method SetTextStr(value: DelphiString); override;
  published
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; aString: DelphiString); override;
    property Memo: TMemo read fMemo write fMemo;
  end;

  TMemo = public partial class(TControl)
  private
    fLines: TStrings;
  published
    constructor(aOwner: TComponent);
    property Lines: TStrings read fLines write fLines;
  end;

  TProgressBarStyle = public enum (Normal, Marquee);

  TProgressBar = public partial class(TControl)
  private
    fStyle: TProgressBarStyle;
    fPosition: Integer;
    fMax: Integer;
    method SetMax(value: Integer);
    method SetPosition(value: Integer);
    method SetStyle(value: TProgressBarStyle);
  published
    property Max: Integer read fMax write SetMax;
    property Position: Integer read fPosition write SetPosition;
    property Style: TProgressBarStyle read fStyle write SetStyle;
  end;
  {$ENDIF}
  {$ENDIF}

  procedure ShowMessage(aMessage: String);

implementation

procedure ShowMessage(aMessage: String);
begin
  PlatformShowMessage(aMessage);
end;

constructor TButton(aOwner: TComponent);
begin
  fTabStop := true;
end;

class method TButton.Create(AOwner: TComponent): TButton;
begin
  result := new TButton(AOwner);
end;

constructor TEdit(aOwner: TComponent);
begin
  fTabStop := true;
end;

class method TLabel.Create(aOwner: TComponent): TLabel;
begin
  result := new TLabel(aOwner);
end;

{$IF NOT LINUX}

constructor TButtonControl(aOwner: TComponent);
begin
  fTabStop := true;
end;

method TCheckBox.SetState(aValue: TCheckBoxState);
begin
  fState := aValue;
  PlatformSetState(aValue);
end;

method TCheckBox.SetAllowGrayed(aValue: Boolean);
begin
  fAllowGrayed := aValue;
  PlatformSetAllowGrayed(aValue);
end;

method TCheckBox.Toggle;
begin
  case State of
    TCheckBoxState.cbChecked:
      SetState(TCheckBoxState.cbUnChecked);

    TCheckBoxState.cbUnChecked:
      if fAllowGrayed then
        SetState(TCheckBoxState.cbGrayed)
      else
        SetState(TCheckBoxState.cbChecked);

    TCheckBoxState.cbGrayed:
      SetState(TCheckBoxState.cbChecked);
  end;
end;

method TCheckBox.Click;
begin
  Toggle;
  inherited;
end;

constructor TListControl(aOwner: TComponent);
begin
  fTabStop := true;
end;

method TListControlItems.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  inherited;
  if not (TComponentState.csLoading in self.ListControl.ComponentState) then
    PlatformAddItem(S, aObject);
end;

method TListControlItems.Insert(aIndex: Integer; S: DelphiString);
begin
  inherited;
  PlatformInsert(aIndex, S);
end;

method TListControlItems.Delete(aIndex: Integer);
begin
  inherited;
  PlatformDelete(aIndex);
end;

method TListControlItems.Clear;
begin
  inherited;
  PlatformClear;
end;

constructor TListBox(aOwner: TComponent);
begin
  fItems := new TListControlItems();
  TListControlItems(fItems).ListControl := self;
end;

method TListBox.SetItems(aValue: TStrings);
begin
  fItems.Clear;
  for i: Integer := 0 to aValue.Count - 1 do
    fItems.AddObject(aValue[i], aValue.Objects[i]);
end;

method TListBox.GetItemIndex: Integer;
begin
  result := PlatformGetItemIndex;
end;

method TListBox.SetItemIndex(aValue: Integer);
begin
  PlatformSetItemIndex(aValue);
end;

method TListBox.SetMultiSelect(aValue: Boolean);
begin
  fMultiSelect := aValue;
  PlatformSetMultiSelect(aValue);
end;

method TListBox.GetSelCount: Integer;
begin
  result := PlatformGetSelCount;
end;

method TListBox.Loaded;
begin
  inherited;
  for i: Integer := 0 to Items.Count - 1 do
    AddItem(Items[i], Items.Objects[i]);
end;

method TListBox.AddItem(Item: DelphiString; aObject: TObject);
begin
  (fItems as TListControlItems).AddObject(Item, aObject);
end;

method TListBox.Clear;
begin
  (fItems as TListControlItems).Clear;
end;

method TListBox.ClearSelection;
begin
  PlatformClearSelection;
end;

method TListBox.DeleteSelected;
begin
  PlatformDeleteSelected;
end;

method TListBox.GetCount: Integer;
begin
  result := fItems.Count;
end;

method TListBox.SelectAll;
begin
  PlatformSelectAll;
end;

method TComboBoxItems.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  inherited;
  if not (TComponentState.csLoading in self.ListControl.ComponentState) then
    PlatformAddItem(S, aObject);
end;

method TComboBoxItems.Clear;
begin
  inherited;
  PlatformClear;
end;

method TComboBoxItems.Delete(aIndex: Integer);
begin
  inherited;
  PlatformDelete(aIndex);
end;

method TComboBoxItems.Insert(aIndex: Integer; S: DelphiString);
begin
  inherited;
  PlatformInsert(aIndex, S);
end;

method TComboBox.SetOnSelect(aValue: TNotifyEvent);
begin
  fOnSelect := aValue;
  PlatformSetOnSelect(aValue);
end;

method TComboBox.SetOnChange(aValue: TNotifyEvent);
begin
  fOnChange := aValue;
  PlatformSetOnChange(aValue);
end;

method TComboBox.SetItems(aValue: TStrings);
begin
  fItems.Clear;
  for i: Integer := 0 to aValue.Count - 1 do
    fItems.AddObject(aValue[i], aValue.Objects[i]);
end;

method TComboBox.GetItemIndex: Integer;
begin
  result := PlatformGetItemIndex;
end;

method TComboBox.SetItemIndex(aValue: Integer);
begin
  PlatformSetItemIndex(aValue);
end;

constructor TComboBox(aOwner: TComponent);
begin
  fItems := new TComboBoxItems();
  TComboBoxItems(fItems).ListControl := self;
end;

method TComboBox.Loaded;
begin
  inherited;
  for i: Integer := 0 to Items.Count - 1 do
    AddItem(Items[i], Items.Objects[i]);
end;

method TComboBox.AddItem(Item: DelphiString; aObject: TObject);
begin
  (fItems as TComboBoxItems).AddObject(Item, aObject);
end;

method TComboBox.Clear;
begin
  (fItems as TComboBoxItems).Clear;
end;

method TComboBox.ClearSelection;
begin
  PlatformClearSelection;
end;

method TComboBox.DeleteSelected;
begin
  PlatformDeleteSelected;
end;

method TComboBox.GetCount: Integer;
begin
  result := fItems.Count;
end;

method TComboBox.SelectAll;
begin
  PlatformSelectAll;
end;

{$IF WEBASSEMBLY}
method TMemoStrings.Get(aIndex: Integer): DelphiString;
begin
  var lText := PlatformGetText;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  result := lTmp[aIndex];
end;

method TMemoStrings.GetTextStr: DelphiString;
begin
  result := PlatformGetText;
end;

method TMemoStrings.SetTextStr(value: DelphiString);
begin
  inherited;
  PlatformSetText(value);
end;

method TMemoStrings.Clear;
begin
  inherited;
  PlatformSetText('');
end;

method TMemoStrings.Delete(aIndex: Integer);
begin
  var lText: String := PlatformGetText;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  lTmp.Delete(aIndex);
  PlatformSetText(lTmp.Text);
end;

method TMemoStrings.Insert(aIndex: Integer; aString: DelphiString);
begin
  var lText: String := PlatformGetText;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  lTmp.Insert(aIndex, aString);
  PlatformSetText(lTmp.Text);
end;

method TMemoStrings.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  var lText: String := PlatformGetText;
  var lTmp := TStringList.Create;
  lTmp.Text := lText;
  lTmp.AddObject(S, aObject);
  PlatformSetText(lTmp.Text);
end;

constructor TMemo(aOwner: TComponent);
begin
  fLines := new TMemoStrings();
  TMemoStrings(fLines).Memo := self;
end;

method TProgressBar.SetStyle(value: TProgressBarStyle);
begin
  fStyle := value;
  PlatformSetStyle(value);
end;

method TProgressBar.SetMax(value: Integer);
begin
  fMax := value;
  PlatformSetMax(value);
end;

method TProgressBar.SetPosition(value: Integer);
begin
  fPosition := value;
  PlatformSetPosition(value);
end;
{$ENDIF}

{$ENDIF}

{$ENDIF}


end.