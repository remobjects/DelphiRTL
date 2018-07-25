namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND (WEBASSEMBLY OR WINDOWS)}

interface

uses
  RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TButton = public partial class(TNativeControl)
  public
    class method Create(AOwner: TComponent): TButton;
  end;

  TEdit = public partial class(TNativeControl)
  public
    property MaxLength: Integer read PlatformGetMaxLength write PlatformSetMaxLength;
    property &ReadOnly: Boolean read PlatformGetReadOnly write PlatformSetReadOnly;
    property Text: String read PlatformGetText write PlatformSetText;
  end;

  //TLabel = public partial class({$IF ISLAND AND WINDOWS}TGraphicControl{$ELSE}TNativeControl{$ENDIF})
  TLabel = public partial class(TNativeControl)
  public
    class method Create(aOwner: TComponent): TLabel;
  end;

  TButtonControl = public partial class(TNativeControl)
  protected
    method PlatformGetChecked: Boolean; virtual; partial; empty;
    method PlatformSetChecked(aValue: Boolean); virtual; partial; empty;
  public
    property Checked: Boolean read PlatformGetChecked write PlatformSetChecked;
  end;

  TCheckBoxState = public enum (cbUnChecked = 0, cbChecked = 1, cbGrayed = 2) of Integer;

  TCheckBox = public partial class(TButtonControl)
  private
    fState: TCheckBoxState;
    fAllowGrayed: Boolean;
  protected
    method PlatformSetState(aValue: TCheckBoxState); partial; empty;
    method SetState(aValue: TCheckBoxState); virtual;
  public
    method Toggle; virtual;
    method Click; override;
    property AllowGrayed: Boolean read fAllowGrayed write fAllowGrayed default false;
    property State: TCheckBoxState read fState write SetState default TCheckBoxState.cbUnchecked;
  end;

  TListControlItems = public partial class(TStringList)
  public
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; S: DelphiString); override;
    property ListControl: TListControl read write;
  end;

  TListControl = public partial abstract class(TNativeControl)
  private
    fListBoxMode: Boolean;
    fItemHeight: Integer;
  protected
    fItems: TStrings;
    constructor(aOwner: TComponent);
    method SetItems(aValue: TStrings); virtual;
  public
    method AddItem(Item: DelphiString; aObject: TObject);
    method Clear;
    method ClearSelection;
    method DeleteSelected;
    property Items: TStrings read fItems write SetItems;
    property ItemIndex: Integer read PlatformGetItemIndex write PlatformSetItemIndex;
    property ItemHeight: Integer read fItemHeight write fItemHeight;
  end;

  TListBox = public partial class(TListControl)
  public
    procedure SelectAll;
    property MultiSelect: Boolean read PlatformGetMultiSelect write PlatformSetMultiSelect;
    property Selected[aIndex: Integer]: Boolean read PlatformGetSelected write PlatformSetSelected;
  end;

  TComboBox = public partial class(TListControl)
  private
    fOnSelect: TNotifyEvent;
    method SetOnSelect(aValue: TNotifyEvent);
  public
    property Text: String read PlatformGetText;
    property OnSelect: TNotifyEvent read fOnSelect write SetOnSelect;
  end;

  {$IF WEBASSEMBLY}
  TMemoStrings = partial class(TStringList)
  private
    fMemo: TMemo;
  protected
    method Get(aIndex: Integer): DelphiString; override;
    method GetTextStr: DelphiString; override;
    method SetTextStr(value: DelphiString); override;
  public
    method AddObject(S: DelphiString; aObject: TObject): Integer; override;
    method Clear; override;
    method Delete(aIndex: Integer); override;
    method Insert(aIndex: Integer; aString: DelphiString); override;
    property Memo: TMemo read fMemo write fMemo;
  end;

  TMemo = public partial class(TControl)
  private
    fLines: TStrings;
  public
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
  public
    property Max: Integer read fMax write SetMax;
    property Position: Integer read fPosition write SetPosition;
    property Style: TProgressBarStyle read fStyle write SetStyle;
  end;
  {$ENDIF}

  procedure ShowMessage(aMessage: String);

implementation


procedure ShowMessage(aMessage: String);
begin
  PlatformShowMessage(aMessage);
end;

class method TButton.Create(AOwner: TComponent): TButton;
begin
  result := new TButton(AOwner);
end;

class method TLabel.Create(aOwner: TComponent): TLabel;
begin
  result := new TLabel(aOwner);
end;

method TCheckBox.SetState(aValue: TCheckBoxState);
begin
  fState := aValue;
  PlatformSetState(aValue);
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

method TListControlItems.AddObject(S: DelphiString; aObject: TObject): Integer;
begin
  inherited;
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

constructor TListControl(aOwner: TComponent);
begin
  fItems := new TListControlItems();
  TListControlItems(fItems).ListControl := self;
end;

method TListControl.AddItem(Item: DelphiString; aObject: TObject);
begin
  (fItems as TListControlItems).AddObject(Item, aObject);
end;

method TListControl.Clear;
begin
  (fItems as TListControlItems).Clear;
end;

method TListControl.ClearSelection;
begin
  PlatformClearSelection;
end;

method TListControl.DeleteSelected;
begin
  PlatformDeleteSelected;
end;

method TListControl.SetItems(aValue: TStrings);
begin
  fItems.Clear;
  for i: Integer := 0 to aValue.Count - 1 do
    fItems.AddObject(aValue[i], aValue.Objects[i]);
end;

method TListBox.SelectAll;
begin
  PlatformSelectAll;
end;

method TComboBox.SetOnSelect(aValue: TNotifyEvent);
begin
  fOnSelect := aValue;
  PlatformSetOnSelect(aValue);
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


end.