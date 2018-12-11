namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WINDOWS) AND NOT DARWIN) OR ECHOESWPF OR (MACOS AND NOT (ISLAND AND DARWIN))}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TNodeState = public enum(nsCut, nsDropHilited, nsFocused, nsSelected, nsExpanded) of Integer;
  TNodeAttachMode = public enum(naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert) of Integer;
  TAddMode = public enum(taAddFirst, taAdd, taInsert) of Integer;
  TTreeNodeClass = class of TTreeNode;
  TMultiSelectStyles = public enum(msControlSelect, msShiftSelect, msVisibleOnly, msSiblingOnly) of Integer;
  TMultiSelectStyle = set of TMultiSelectStyles;
  TSortType = public enum(stNone, stData, stText, stBoth) of Integer;

  TTreeNode = public class(TPersistent)
  private
    fOwner: TTreeNodes;
    fText: String;
    //fData: TCustomData;
    //fItemId: HTreeItem;
    fImageIndex: TImageIndex;
    fSelectedIndex: Integer;
    fOverlayIndex: Integer;
    fStateIndex: Integer;
    fDeleting: Boolean;
    fInTree: Boolean;
    fEnabled: Boolean;
    fExpandedImageIndex: TImageIndex;
    method CompareCount(CompareMe: Integer): Boolean;
    method DoCanExpand(Expand: Boolean): Boolean;
    method DoExpand(Expand: Boolean);
    method ExpandItem(Expand: Boolean; Recurse: Boolean);
    method GetAbsoluteIndex: Integer;
    method GetExpanded: Boolean;
    method GetLevel: Integer;
    method GetParent: TTreeNode;
    method GetChildren: Boolean;
    method GetCut: Boolean;
    //method GetDropTarget: Boolean;
    method GetFocused: Boolean;
    method GetIndex: Integer;
    method GetItem(aIndex: Integer): TTreeNode;
    method GetSelected: Boolean;
    method GetCount: Integer;
    method GetTreeView: TTreeView;
    //method InternalMove(ParentNode, Node: TTreeNode; HItem: HTreeItem; AddMode: TAddMode);
    method IsEqual(Node: TTreeNode): Boolean;
    method IsNodeVisible: Boolean;
    //method SetData(aValue: TCustomData);
    method SetChildren(aValue: Boolean);
    method SetCut(aValue: Boolean);
    //method SetDropTarget(aValue: Boolean);
    method SetItem(aIndex: Integer; Value: TTreeNode);
    method SetExpanded(aValue: Boolean);
    method SetFocused(aValue: Boolean);
    method SetImageIndex(aValue: TImageIndex);
    method SetExpandedImageIndex(aValue: TImageIndex);
    method SetOverlayIndex(aValue: Integer);
    method SetSelectedIndex(aValue: Integer);
    method SetSelected(aValue: Boolean);
    method SetStateIndex(aValue: Integer);
    method SetText(aValue: String);
    method SetEnabled(aValue: Boolean);
    //method ReadData(Stream: TStream; Info: PNodeInfo);
    //method ReadNodeData(Stream: TStream; NodeDataType: TNodeDataType);
    //method WriteNodeData(Stream: TStream);
  protected
    method GetState(NodeState: TNodeState): Boolean;
    method SetState(NodeState: TNodeState; Value: Boolean);
    method SetSelectedBit(Value: Boolean);
  public
    constructor(aOwner: TTreeNodes); virtual;
    //method AlphaSort(aRecurse: Boolean := False): Boolean;
    //method Assign(Source: TPersistent); override;
    method Collapse(Recurse: Boolean);
    method Delete;
    method DeleteChildren;
    //method DisplayRect(TextOnly: Boolean): TRect;
    method EditText: Boolean;
    method EndEdit(Cancel: Boolean);
    method Expand(Recurse: Boolean);
    method getFirstChild: TTreeNode;
    //method GetHandle: HWND;
    method GetLastChild: TTreeNode;
    method GetNext: TTreeNode;
    method GetNextChild(Value: TTreeNode): TTreeNode;
    method getNextSibling: TTreeNode;
    method GetNextVisible: TTreeNode;
    method GetPrev: TTreeNode;
    method GetPrevChild(Value: TTreeNode): TTreeNode;
    method getPrevSibling: TTreeNode;
    method GetPrevVisible: TTreeNode;
    method HasAsParent(Value: TTreeNode): Boolean;
    method IndexOf(Value: TTreeNode): Integer;
    method MakeVisible;
    method MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); virtual;
    method IsFirstNode: Boolean;
    //method CustomSort(SortProc: TTVCompare; Data: NativeInt; ARecurse: Boolean = False): Boolean;
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property Count: Integer read GetCount;
    property Cut: Boolean read GetCut write SetCut;
    //property Data: TCustomData read fData write SetData;
    property Deleting: Boolean read fDeleting;
    property Focused: Boolean read GetFocused write SetFocused;
    //property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Selected: Boolean read GetSelected write SetSelected;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property ExpandedImageIndex: TImageIndex read fExpandedImageIndex write SetExpandedImageIndex;
    //property Handle: HWND read GetHandle;
    property HasChildren: Boolean read GetChildren write SetChildren;
    property ImageIndex: TImageIndex read fImageIndex write SetImageIndex;
    property &Index: Integer read GetIndex;
    property IsVisible: Boolean read IsNodeVisible;
    property Item[aIndex: Integer]: TTreeNode read GetItem write SetItem; default;
    //property ItemId: HTreeItem read fItemId;
    property Level: Integer read GetLevel;
    property OverlayIndex: Integer read fOverlayIndex write SetOverlayIndex;
    property Owner: TTreeNodes read fOwner;
    property Parent: TTreeNode read GetParent;
    property SelectedIndex: Integer read fSelectedIndex write SetSelectedIndex;
    property Enabled: Boolean read fEnabled write SetEnabled;
    property StateIndex: Integer read fStateIndex write SetStateIndex;
    property Text: String read fText write SetText;
    property TreeView: TTreeView read GetTreeView;
  end;

  TTreeNodes = public class(TPersistent)
  private
    fOwner: TTreeView;
    fUpdateCount: Integer;
    //fNodeCache: TNodeCache;
    method AddedNode(Value: TTreeNode);
    //method GetHandle: HWND;
    method GetNodeFromIndex(aIndex: Integer): TTreeNode;
    //method ReadData(Stream: TStream);
    //method ReadNodeData(Stream: TStream);
    //method Repaint(Node: TTreeNode);
    //method WriteNodeData(Stream: TStream);
    method ClearCache;
    //method WriteExpandedState(Stream: TStream);
    //method ReadExpandedState(Stream: TStream);
    method GetReading: Boolean;
  protected
    //method AddItem(Parent, Target: HTreeItem; const Item: TTVItem; AddMode: TAddMode): HTreeItem;
    //method CreateItem(Node: TTreeNode): TTVItem;
    method GetCount: Integer;
    method SetItem(aIndex: Integer; aValue: TTreeNode);
    method SetUpdateState(Updating: Boolean);
    property Reading: Boolean read GetReading;
  public
    constructor(aOwner: TTreeView);
    method DefineProperties(aFiler: TObject {TFIler}); override;
    method AddChildFirst(aParent: TTreeNode; S: String): TTreeNode;
    method AddChild(aParent: TTreeNode; S: String): TTreeNode;
    //method AddChildObjectFirst(Parent: TTreeNode; S: string; Ptr: TCustomData): TTreeNode;
    //method AddChildObject(Parent: TTreeNode; S: string; Ptr: TCustomData): TTreeNode;
    //method AddObjectFirst(Sibling: TTreeNode; S: string; Ptr: TCustomData): TTreeNode;
    //method AddObject(Sibling: TTreeNode; S: string; Ptr: TCustomData): TTreeNode;
    //method AddNode(aNode, aRelative: TTreeNode; S: String; aPtr: TCustomData; aMethod: TNodeAttachMode): TTreeNode;
    method AddFirst(aSibling: TTreeNode; S: String): TTreeNode;
    method &Add(aSibling: TTreeNode; S: String): TTreeNode;
    //method AlphaSort(aRecurse: Boolean := False): Boolean;
    //method Assign(aSource: TPersistent); override;
    method BeginUpdate;
    method Clear;
    method Delete(aNode: TTreeNode);
    method EndUpdate;
    method GetFirstNode: TTreeNode;
    //method GetEnumerator: TTreeNodesEnumerator;
    //method GetNode(ItemId: HTreeItem): TTreeNode;
    method Insert(Sibling: TTreeNode; S: String): TTreeNode;
    //method InsertObject(Sibling: TTreeNode; const S: string; Ptr: TCustomData): TTreeNode;
    //method InsertNode(Node, Sibling: TTreeNode; S: String; Ptr: TCustomData): TTreeNode;
    //method CustomSort(SortProc: TTVCompare; Data: NativeInt; ARecurse: Boolean := False): Boolean;
    property Count: Integer read GetCount;
    //property Handle: HWND read GetHandle;
    property Item[aIndex: Integer]: TTreeNode read GetNodeFromIndex; default;
    property Owner: TTreeView read fOwner;
  end;

  TTreeView = public class(TWinControl)
  private
    fAutoExpand: Boolean;
    //fBorderStyle: TBorderStyle;
    //fCanvas: TCanvas;
    //fCanvasChanged: Boolean;
    //fDefEditProc: TWindowProcPtr;
    //fEditInstance: TTVEditInstance;
    //fDragged: Boolean;
    //fDragImage: TDragImageList;
    //fDragNode: TTreeNode;
    //fEditHandle: HWND;
    fHideSelection: Boolean;
    fHotTrack: Boolean;
    //fImageChangeLink: TChangeLink;
    //fImages: TCustomImageList;
    //fInBufferedPrintClient: Boolean;
    //fLastDropTarget: TTreeNode;
    //fMemStream: TMemoryStream;
    //fRClickNode: TTreeNode;
    //fRightClickSelect: Boolean;
    //fManualNotify: Boolean;
    fReadOnly: Boolean;
    fRowSelect: Boolean;
    fSaveIndex: Integer;
    fSaveIndent: Integer;
    fSaveItems: TStringList;
    fSaveTopIndex: Integer;
    fShowButtons: Boolean;
    fShowLines: Boolean;
    fShowRoot: Boolean;
    fSortType: TSortType;
    fStateChanging: Boolean;
    //fStateImages: TCustomImageList;
    //fStateChangeLink: TChangeLink;
    fToolTips: Boolean;
    fTreeNodes: TTreeNodes;
    //fWideText: WideString;
    fMultiSelect: Boolean;
    fMultiSelectStyle: TMultiSelectStyle;
    fSelections: TList<Integer>;
    fSaveIndexes: TList<Integer>;
    fShiftAnchor: TTreeNode;
    fSelecting: Boolean;
    fSelectChanged: Boolean;
    fOurFont: Integer;
    fStockFont: Integer;
    fCreateWndRestores: Boolean;
    fReading: Boolean;
    fEncoding: TEncoding;
    //fOnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent;
    //fOnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent;
    //fOnCancelEdit: TTVChangedEvent;
    //fOnChange: TTVChangedEvent;
    //fOnChanging: TTVChangingEvent;
    //fOnCollapsed: TTVExpandedEvent;
    //fOnCollapsing: TTVCollapsingEvent;
    //fOnCompare: TTVCompareEvent;
    //fOnCustomDraw: TTVCustomDrawEvent;
    //fOnCustomDrawItem: TTVCustomDrawItemEvent;
    //fOnDeletion: TTVExpandedEvent;
    //fOnAddition: TTVExpandedEvent;
    //fOnEditing: TTVEditingEvent;
    //fOnEdited: TTVEditedEvent;
    //fOnExpanded: TTVExpandedEvent;
    //fOnExpanding: TTVExpandingEvent;
    //fOnGetImageIndex: TTVExpandedEvent;
    //fOnGetSelectedIndex: TTVExpandedEvent;
    //fOnHint: TTVHintEvent;
    //fOnCreateNodeClass: TTVCreateNodeClassEvent;
    //method CanvasChanged(Sender: TObject);
    //method CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    //method CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    //method CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    //method CMDrag(var Message: TCMDrag); message CM_DRAG;
    //method CNNotify(var Message: TWMNotifyTV); message CN_NOTIFY;
    //method EditWndProc(var Message: TMessage);
    //method DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    method NodeDeselect(aIndex: Integer);
    method NodeSelect(aNode: TTreeNode; At: Integer := 0);
    method FinishSelection(aNode: TTreeNode; aShiftState: TShiftState);
    method ControlSelectNode(aNode: TTreeNode);
    method ShiftSelectNode(aNode: TTreeNode; aBackward: Boolean; aDeselect: Boolean := True);
    method ControlShiftSelectNode(Node: TTreeNode; Backward: Boolean);
    method SelectNode(Node: TTreeNode);
    method GetChangeDelay: Integer;
    //method GetDropTarget: TTreeNode;
    method GetIndent: Integer;
    //method GetNodeFromItem(Item: TTVItem): TTreeNode;
    method GetSelected: TTreeNode;
    method GetSelectionCount: Cardinal;
    method GetSelection(aIndex: Integer): TTreeNode;
    method GetTopItem: TTreeNode;
    method ImageListChange(Sender: TObject);
    method SetAutoExpand(aValue: Boolean);
    //method SetBorderStyle(aValue: TBorderStyle);
    method SetButtonStyle(aValue: Boolean);
    method SetChangeDelay(aValue: Integer);
    //method SetDropTarget(aValue: TTreeNode);
    method SetHideSelection(aValue: Boolean);
    method SetHotTrack(aValue: Boolean);
    //method SetImageList(aValue: HImageList; aFlags: Integer);
    method SetIndent(aValue: Integer);
    //method SetImages(aValue: TCustomImageList);
    method SetLineStyle(aValue: Boolean);
    method SetMultiSelect(aValue: Boolean);
    method SetMultiSelectStyle(aValue: TMultiSelectStyle);
    method SetReadOnly(aValue: Boolean);
    method SetRootStyle(aValue: Boolean);
    method SetRowSelect(aValue: Boolean);
    method SetSelected(aValue: TTreeNode);
    //method SetSortType(aValue: TSortType);
    //method SetStateImages(aValue: TCustomImageList);
    method SetToolTips(aValue: Boolean);
    method SetTreeNodes(aValue: TTreeNodes);
    method SetTopItem(aValue: TTreeNode);
    //method OnChangeTimer(Sender: TObject);
    //method WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    //method WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    //method WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    //method WMCtlColorEdit(var Message: TMessage); message WM_CTLCOLOREDIT;
    //method CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  protected
    //fChangeTimer: TTimer;
  public
    method CanEdit(aNode: TTreeNode): Boolean; virtual;
    method CanChange(aNode: TTreeNode): Boolean; virtual;
    method CanCollapse(aNode: TTreeNode): Boolean; virtual;
    method CanExpand(aNode: TTreeNode): Boolean; virtual;
    method Change(aNode: TTreeNode); virtual;
    method Collapse(aNode: TTreeNode); virtual;
    method CreateNode: TTreeNode; virtual;
    method CreateNodes: TTreeNodes; virtual;
    method CreateParams(var aParams: TCreateParams); override;
    method CreateWnd; override;
    //method CustomDraw(aRect: TRect; Stage: TCustomDrawStage): Boolean; virtual;
    //method CustomDrawItem(aNode: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; virtual;
    method Delete(aNode: TTreeNode); virtual;
    method Added(aNode: TTreeNode); virtual;
    //method DestroyWnd; override;
    //method DoEndDrag(Target: TObject; X, Y: Integer); override;
    //method DoStartDrag(var DragObject: TDragObject); override;
    //method Edit(aItem: TTVItem); virtual;
    method Expand(aNode: TTreeNode); virtual;
    method GetImageIndex(aNode: TTreeNode); virtual;
    method GetSelectedIndex(aNode: TTreeNode); virtual;
    //method IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; virtual;
    method Loaded; override;
    method Notification(aComponent: TComponent; aOperation: TOperation); override;
    //method SetDragMode(Value: TDragMode); override;
    method WndProc(var Message: TMessage); override;
    //method ValidateSelection;
    //method InvalidateSelectionsRects;
    method MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    method DoEnter; override;
    method DoExit; override;
    //method IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    method SetEncoding(Value: TEncoding);
    property AutoExpand: Boolean read fAutoExpand write SetAutoExpand default False;
    //property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
    //property CreateWndRestores: Boolean read FCreateWndRestores write FCreateWndRestores default True;
    property Encoding: TEncoding read fEncoding;
    property HideSelection: Boolean read fHideSelection write SetHideSelection default True;
    property HotTrack: Boolean read fHotTrack write SetHotTrack default False;
    //property Images: TCustomImageList read fImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent;
    property Items: TTreeNodes read fTreeNodes write SetTreeNodes;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property MultiSelectStyle: TMultiSelectStyle read fMultiSelectStyle write SetMultiSelectStyle default [msControlSelect];
    property Reading: Boolean read fReading;
    property &ReadOnly: Boolean read fReadOnly write SetReadOnly default False;
    //property RightClickSelect: Boolean read fRightClickSelect write fRightClickSelect default False;
    property RowSelect: Boolean read fRowSelect write SetRowSelect default False;
    property ShowButtons: Boolean read fShowButtons write SetButtonStyle default True;
    property ShowLines: Boolean read fShowLines write SetLineStyle default True;
    property ShowRoot: Boolean read fShowRoot write SetRootStyle default True;
    //property SortType: TSortType read fSortType write SetSortType default stNone;
    //property StateImages: TCustomImageList read fStateImages write SetStateImages;
    property ToolTips: Boolean read fToolTips write SetToolTips default True;
    {property OnAddition: TTVExpandedEvent read FOnAddition write FOnAddition;
    property OnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
    property OnCancelEdit: TTVChangedEvent read FOnCancelEdit write FOnCancelEdit;
    property OnChange: TTVChangedEvent read FOnChange write FOnChange;
    property OnChanging: TTVChangingEvent read FOnChanging write FOnChanging;
    property OnCollapsed: TTVExpandedEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
    property OnCompare: TTVCompareEvent read FOnCompare write FOnCompare;
    property OnCustomDraw: TTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property OnHint: TTVHintEvent read FOnHint write FOnHint;
    property OnCreateNodeClass: TTVCreateNodeClassEvent read FOnCreateNodeClass write FOnCreateNodeClass;}
    constructor Create(aOwner: TComponent);
    //method AlphaSort(ARecurse: Boolean = True): Boolean;
    method FullCollapse;
    method FullExpand;
    //method GetHitTestInfoAt(X, Y: Integer): THitTests;
    //method GetNodeAt(X, Y: Integer): TTreeNode;
    //method GetDragImages: TDragImageList; override;
    method IsEditing: Boolean;
    //method LoadFromFile(aFileName: String);
    //method LoadFromFile(aFileName: String; aEncoding: TEncoding);
    //method LoadFromStream(Stream: TStream);
    //method LoadFromStream(Stream: TStream; aEncoding: TEncoding);
    //method SaveToFile(aFileName: String);
    //method SaveToFile(aFileName: String; aEncoding: TEncoding);
    //method SaveToStream(aStream: TStream);
    //method SaveToStream(aStream: TStream; AEncoding: TEncoding);
    method &Select(aNode: TTreeNode; ShiftState: TShiftState = []); virtual;
    method &Select(Nodes: array of TTreeNode); virtual;
    method &Select(Nodes: TList<TTreeNode>); virtual;
    method Deselect(aNode: TTreeNode); virtual;
    method Subselect(aNode: TTreeNode; Validate: Boolean := False); virtual;
    method ClearSelection(KeepPrimary: Boolean := False); virtual;
    method GetSelections(aList: TList<TTreeNode>): TTreeNode;
    method FindNextToSelect: TTreeNode; virtual;
    //method CustomSort(SortProc: TTVCompare; Data: NativeInt; ARecurse: Boolean = True): Boolean;
    //property Canvas: TCanvas read fCanvas;
    //property DropTarget: TTreeNode read GetDropTarget write SetDropTarget;
    property Selected: TTreeNode read GetSelected write SetSelected;
    property TopItem: TTreeNode read GetTopItem write SetTopItem;
    property SelectionCount: Cardinal read GetSelectionCount;
    property Selections[aIndex: Integer]: TTreeNode read GetSelection;
  end;

implementation

method TTreeNode.CompareCount(CompareMe: Integer): Boolean;
begin

end;

method TTreeNode.DoCanExpand(Expand: Boolean): Boolean;
begin

end;

method TTreeNode.DoExpand(Expand: Boolean);
begin

end;

method TTreeNode.ExpandItem(Expand: Boolean; Recurse: Boolean);
begin

end;

method TTreeNode.GetAbsoluteIndex: Integer;
begin

end;

method TTreeNode.GetExpanded: Boolean;
begin

end;

method TTreeNode.GetLevel: Integer;
begin

end;

method TTreeNode.GetParent: TTreeNode;
begin

end;

method TTreeNode.GetChildren: Boolean;
begin

end;

method TTreeNode.GetCut: Boolean;
begin

end;

method TTreeNode.GetFocused: Boolean;
begin

end;

method TTreeNode.GetIndex: Integer;
begin

end;

method TTreeNode.GetItem(aIndex: Integer): TTreeNode;
begin

end;

method TTreeNode.GetSelected: Boolean;
begin

end;

method TTreeNode.GetCount: Integer;
begin

end;

method TTreeNode.GetTreeView: TTreeView;
begin

end;

method TTreeNode.IsEqual(Node: TTreeNode): Boolean;
begin

end;

method TTreeNode.IsNodeVisible: Boolean;
begin

end;

method TTreeNode.SetChildren(aValue: Boolean);
begin

end;

method TTreeNode.SetCut(aValue: Boolean);
begin

end;

method TTreeNode.SetItem(aIndex: Integer; Value: TTreeNode);
begin

end;

method TTreeNode.SetExpanded(aValue: Boolean);
begin

end;

method TTreeNode.SetFocused(aValue: Boolean);
begin

end;

method TTreeNode.SetImageIndex(aValue: TImageIndex);
begin

end;

method TTreeNode.SetExpandedImageIndex(aValue: TImageIndex);
begin

end;

method TTreeNode.SetOverlayIndex(aValue: Integer);
begin

end;

method TTreeNode.SetSelectedIndex(aValue: Integer);
begin

end;

method TTreeNode.SetSelected(aValue: Boolean);
begin

end;

method TTreeNode.SetStateIndex(aValue: Integer);
begin

end;

method TTreeNode.SetText(aValue: String);
begin

end;

method TTreeNode.SetEnabled(aValue: Boolean);
begin

end;

method TTreeNode.GetState(NodeState: TNodeState): Boolean;
begin

end;

method TTreeNode.SetState(NodeState: TNodeState; Value: Boolean);
begin

end;

method TTreeNode.SetSelectedBit(Value: Boolean);
begin

end;

constructor TTreeNode(aOwner: TTreeNodes);
begin

end;

method TTreeNode.Collapse(Recurse: Boolean);
begin

end;

method TTreeNode.Delete;
begin

end;

method TTreeNode.DeleteChildren;
begin

end;

method TTreeNode.EditText: Boolean;
begin

end;

method TTreeNode.EndEdit(Cancel: Boolean);
begin

end;

method TTreeNode.Expand(Recurse: Boolean);
begin

end;

method TTreeNode.getFirstChild: TTreeNode;
begin

end;

method TTreeNode.GetLastChild: TTreeNode;
begin

end;

method TTreeNode.GetNext: TTreeNode;
begin

end;

method TTreeNode.GetNextChild(Value: TTreeNode): TTreeNode;
begin

end;

method TTreeNode.getNextSibling: TTreeNode;
begin

end;

method TTreeNode.GetNextVisible: TTreeNode;
begin

end;

method TTreeNode.GetPrev: TTreeNode;
begin

end;

method TTreeNode.GetPrevChild(Value: TTreeNode): TTreeNode;
begin

end;

method TTreeNode.getPrevSibling: TTreeNode;
begin

end;

method TTreeNode.GetPrevVisible: TTreeNode;
begin

end;

method TTreeNode.HasAsParent(Value: TTreeNode): Boolean;
begin

end;

method TTreeNode.IndexOf(Value: TTreeNode): Integer;
begin

end;

method TTreeNode.MakeVisible;
begin

end;

method TTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
begin

end;

method TTreeNode.IsFirstNode: Boolean;
begin

end;

method TTreeNodes.AddedNode(Value: TTreeNode);
begin

end;

method TTreeNodes.GetNodeFromIndex(aIndex: Integer): TTreeNode;
begin

end;

method TTreeNodes.ClearCache;
begin

end;

method TTreeNodes.GetReading: Boolean;
begin

end;

method TTreeNodes.DefineProperties(aFiler: TObject);
begin

end;

method TTreeNodes.GetCount: Integer;
begin

end;

method TTreeNodes.SetItem(aIndex: Integer; aValue: TTreeNode);
begin

end;

method TTreeNodes.SetUpdateState(Updating: Boolean);
begin

end;

constructor TTreeNodes(aOwner: TTreeView);
begin

end;

method TTreeNodes.AddChildFirst(aParent: TTreeNode; S: String): TTreeNode;
begin

end;

method TTreeNodes.AddChild(aParent: TTreeNode; S: String): TTreeNode;
begin

end;

method TTreeNodes.AddFirst(aSibling: TTreeNode; S: String): TTreeNode;
begin

end;

method TTreeNodes.Add(aSibling: TTreeNode; S: String): TTreeNode;
begin

end;

method TTreeNodes.BeginUpdate;
begin

end;

method TTreeNodes.Clear;
begin

end;

method TTreeNodes.Delete(aNode: TTreeNode);
begin

end;

method TTreeNodes.EndUpdate;
begin

end;

method TTreeNodes.GetFirstNode: TTreeNode;
begin

end;

method TTreeNodes.Insert(Sibling: TTreeNode; S: String): TTreeNode;
begin

end;

method TTreeView.NodeDeselect(aIndex: Integer);
begin

end;

method TTreeView.NodeSelect(aNode: TTreeNode; At: Integer := 0);
begin

end;

method TTreeView.FinishSelection(aNode: TTreeNode; aShiftState: TShiftState);
begin

end;

method TTreeView.ControlSelectNode(aNode: TTreeNode);
begin

end;

method TTreeView.ShiftSelectNode(aNode: TTreeNode; aBackward: Boolean; aDeselect: Boolean := true);
begin

end;

method TTreeView.ControlShiftSelectNode(Node: TTreeNode; Backward: Boolean);
begin

end;

method TTreeView.SelectNode(Node: TTreeNode);
begin

end;

method TTreeView.GetIndent: Integer;
begin

end;

method TTreeView.GetSelected: TTreeNode;
begin

end;

method TTreeView.GetSelectionCount: Cardinal;
begin

end;

method TTreeView.GetSelection(aIndex: Integer): TTreeNode;
begin

end;

method TTreeView.GetTopItem: TTreeNode;
begin

end;

method TTreeView.ImageListChange(Sender: TObject);
begin

end;

method TTreeView.SetAutoExpand(aValue: Boolean);
begin

end;

method TTreeView.SetButtonStyle(aValue: Boolean);
begin

end;

method TTreeView.SetChangeDelay(aValue: Integer);
begin

end;

method TTreeView.SetHideSelection(aValue: Boolean);
begin

end;

method TTreeView.SetIndent(aValue: Integer);
begin

end;

method TTreeView.SetLineStyle(aValue: Boolean);
begin

end;

method TTreeView.SetMultiSelect(aValue: Boolean);
begin

end;

method TTreeView.SetMultiSelectStyle(aValue: TMultiSelectStyle);
begin

end;

method TTreeView.SetReadOnly(aValue: Boolean);
begin

end;

method TTreeView.SetRootStyle(aValue: Boolean);
begin

end;

method TTreeView.SetRowSelect(aValue: Boolean);
begin

end;

method TTreeView.SetSelected(aValue: TTreeNode);
begin

end;

method TTreeView.SetToolTips(aValue: Boolean);
begin

end;

method TTreeView.SetTreeNodes(aValue: TTreeNodes);
begin

end;

method TTreeView.SetTopItem(aValue: TTreeNode);
begin

end;

method TTreeView.CanEdit(aNode: TTreeNode): Boolean;
begin

end;

method TTreeView.CanChange(aNode: TTreeNode): Boolean;
begin

end;

method TTreeView.CanCollapse(aNode: TTreeNode): Boolean;
begin

end;

method TTreeView.CanExpand(aNode: TTreeNode): Boolean;
begin

end;

method TTreeView.Change(aNode: TTreeNode);
begin

end;

method TTreeView.Collapse(aNode: TTreeNode);
begin

end;

method TTreeView.CreateNode: TTreeNode;
begin

end;

method TTreeView.CreateNodes: TTreeNodes;
begin

end;

method TTreeView.CreateParams(var aParams: TCreateParams);
begin

end;

method TTreeView.CreateWnd;
begin

end;

method TTreeView.Delete(aNode: TTreeNode);
begin

end;

method TTreeView.Added(aNode: TTreeNode);
begin

end;

method TTreeView.Expand(aNode: TTreeNode);
begin

end;

method TTreeView.GetImageIndex(aNode: TTreeNode);
begin

end;

method TTreeView.GetSelectedIndex(aNode: TTreeNode);
begin

end;

method TTreeView.Loaded;
begin

end;

method TTreeView.Notification(aComponent: TComponent; aOperation: TOperation);
begin

end;

method TTreeView.WndProc(var Message: TMessage);
begin

end;

method TTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin

end;

method TTreeView.DoEnter;
begin

end;

method TTreeView.DoExit;
begin

end;

method TTreeView.SetEncoding(Value: TEncoding);
begin

end;

constructor TTreeView(aOwner: TComponent);
begin

end;

method TTreeView.FullCollapse;
begin

end;

method TTreeView.FullExpand;
begin

end;

method TTreeView.IsEditing: Boolean;
begin

end;

method TTreeView.Select(aNode: TTreeNode; ShiftState: TShiftState);
begin

end;

method TTreeView.Select(Nodes: array of TTreeNode);
begin

end;

method TTreeView.Select(Nodes: TList<TTreeNode>);
begin

end;

method TTreeView.Deselect(aNode: TTreeNode);
begin

end;

method TTreeView.Subselect(aNode: TTreeNode; Validate: Boolean := false);
begin

end;

method TTreeView.ClearSelection(KeepPrimary: Boolean := false);
begin

end;

method TTreeView.GetSelections(aList: TList<TTreeNode>): TTreeNode;
begin

end;

method TTreeView.FindNextToSelect: TTreeNode;
begin

end;

method TTreeView.GetChangeDelay: Integer;
begin

end;

method TTreeView.SetHotTrack(aValue: Boolean);
begin

end;

{$ENDIF}

end.