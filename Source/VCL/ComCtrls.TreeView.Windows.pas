﻿namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TTreeNode = public partial class(TPersistent)
  private assembly
    method InternalGetNodeState(toCheck: Integer): Boolean;
    method InternalSetState(aState: Integer; aValue: Boolean);
  protected
    method PlatformSetText(aValue: VCLString); partial;
    method PlatformGetSelected: Boolean; partial;
    method PlatformGetFocused: Boolean; partial;
    method PlatformGetExpanded: Boolean; partial;
    method PlatformSetSelected(aValue: Boolean); partial;
    method PlatformSetFocused(aValue: Boolean); partial;
    method PlatformSetExpanded(aValue: Boolean); partial;
  end;

  TTreeNodes = public partial class(TPersistent)
  protected
    method InternalInsertNode(aInsertData: rtl.TVINSERTSTRUCT; var aNode: TTreeNode);

    method PlatformAddChild(aParent: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAddChildFirst(aParent: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAdd(aSibling: TTreeNode; var aNode: TTreeNode); partial;
    method PlatformAddFirst(aSibling: TTreeNode; var aNode: TTreeNode); partial;
  end;

  TTreeView = public partial class(TWinControl)
  private
    class constructor;
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method CreateWnd; override;

    [MessageAttribute(CN_NOTIFY)]
    method CNNotify(var aMessage: TMessage);
  public
    method WndProc(var Message: TMessage); override;
  end;

implementation

method TTreeNode.PlatformSetText(aValue: VCLString);
begin
  var lNode: rtl.TVITEM;
  lNode.mask := rtl.TVIF_TEXT;
  lNode.hItem := fItemId;
  lNode.pszText := PlatformString(aValue).ToLPCWSTR;
  rtl.SendMessage(fOwner.Owner.Handle, rtl.TVM_SETITEM, 0, rtl.LPARAM(@lNode));
end;

method TTreeNode.InternalGetNodeState(toCheck: Integer): Boolean;
begin
  var lNode: rtl.TVITEM;
  lNode.mask := rtl.TVIF_STATE;
  lNode.hItem := fItemId;
  rtl.SendMessage(fOwner.Owner.Handle, rtl.TVM_GETITEM, 0, rtl.LPARAM(@lNode));
  result := (lNode.state and toCheck) <> 0;
end;

method TTreeNode.PlatformGetSelected: Boolean;
begin
  result := InternalGetNodeState(rtl.TVIS_SELECTED);
end;

method TTreeNode.PlatformGetFocused: Boolean;
begin
  const TVIS_FOCUSED = 1;
  result := InternalGetNodeState(TVIS_FOCUSED);
end;

method TTreeNode.PlatformGetExpanded: Boolean;
begin
  result := InternalGetNodeState(rtl.TVIS_EXPANDED);
end;

method TTreeNode.InternalSetState(aState: Integer; aValue: Boolean);
begin
  var lNode: rtl.TVITEM;
  lNode.mask := rtl.TVIF_STATE;
  lNode.hItem := fItemId;
  lNode.stateMask := aState;
  lNode.state := Integer(aValue);
  rtl.SendMessage(fOwner.Owner.Handle, rtl.TVM_SETITEM, 0, rtl.LPARAM(@lNode));
end;

method TTreeNode.PlatformSetSelected(aValue: Boolean);
begin
  InternalSetState(rtl.TVIS_SELECTED, aValue);
end;

method TTreeNode.PlatformSetFocused(aValue: Boolean);
begin
  const TVIS_FOCUSED = 1;
  InternalSetState(TVIS_FOCUSED, aValue);
end;

method TTreeNode.PlatformSetExpanded(aValue: Boolean);
begin
  InternalSetState(rtl.TVIS_EXPANDED, aValue);
end;

method TTreeView.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);

  aParams.WidgetClassName := rtl.WC_TREEVIEW.ToCharArray(true);
  aParams.Style := aParams.Style or rtl.LVS_ICON or rtl.WS_BORDER or rtl.WS_CLIPCHILDREN;
  if fShowLines then aParams.Style := aParams.Style or rtl.TVS_HASLINES;
  if fShowButtons then aParams.Style := aParams.Style or rtl.TVS_HASBUTTONS;
  if fShowRoot then aParams.Style := aParams.Style or rtl.TVS_LINESATROOT;
  if not fHideSelection then aParams.Style := aParams.Style or rtl.TVS_SHOWSELALWAYS;
  if fAutoExpand then aParams.Style := aParams.Style or rtl.TVS_SINGLEEXPAND;
  if not fReadOnly then aParams.Style := aParams.Style or rtl.TVS_EDITLABELS;

  CreateClass(var aParams);
end;

method TTreeView.CreateWnd;
begin
  inherited;

  TStyleThemes.ThemeControl(Handle);
end;

class constructor TTreeView;
begin
  var lIce: rtl.INITCOMMONCONTROLSEX;
  lIce.dwICC := rtl.ICC_TREEVIEW_CLASSES;
  lIce.dwSize := sizeOf(lIce);
  var lModule := rtl.LoadLibrary('Comctl32.dll');
  var lProc := TInitEx(rtl.GetProcAddress(lModule, 'InitCommonControlsEx'));
  lProc(@lIce);
end;

method TTreeView.CNNotify(var aMessage: TMessage);
begin

end;

method TTreeView.WndProc(var Message: TMessage);
begin
  inherited(var Message);
end;

method TTreeNodes.InternalInsertNode(aInsertData: rtl.TVINSERTSTRUCT; var aNode: TTreeNode);
begin
  var lNode: rtl.TVITEM;

  lNode.mask := rtl.TVIF_TEXT;
  lNode.pszText := PlatformString(aNode.Text).ToLPCWSTR;
  aInsertData.u.item := lNode;
  aNode.fItemId := rtl.HTREEITEM(rtl.SendMessage(fOwner.Handle, rtl.TVM_INSERTITEM, 0, rtl.LPARAM(@aInsertData)));
end;

method TTreeNodes.PlatformAddChild(aParent: TTreeNode; var aNode: TTreeNode);
begin
  var lInsertData: rtl.TVINSERTSTRUCT;

  lInsertData.hInsertAfter := rtl.TVI_LAST;
  lInsertData.hParent := if aParent <> nil then aParent.ItemId else rtl.TVI_ROOT;
  InternalInsertNode(lInsertData, var aNode);
end;

method TTreeNodes.PlatformAddChildFirst(aParent: TTreeNode; var aNode: TTreeNode);
begin
  var lInsertData: rtl.TVINSERTSTRUCT;

  lInsertData.hInsertAfter := rtl.TVI_FIRST;
  lInsertData.hParent := if aParent <> nil then aParent.ItemId else rtl.TVI_ROOT;
  InternalInsertNode(lInsertData, var aNode);
end;

method TTreeNodes.PlatformAdd(aSibling: TTreeNode; var aNode: TTreeNode);
begin
  var lInsertData: rtl.TVINSERTSTRUCT;

  lInsertData.hInsertAfter := rtl.TVI_LAST;
  lInsertData.hParent := if aSibling <> nil then aSibling.ItemId else rtl.TVI_ROOT;
  InternalInsertNode(lInsertData, var aNode);
end;

method TTreeNodes.PlatformAddFirst(aSibling: TTreeNode; var aNode: TTreeNode);
begin
  var lInsertData: rtl.TVINSERTSTRUCT;

  lInsertData.hInsertAfter := rtl.TVI_FIRST;
  lInsertData.hParent := if aSibling <> nil then aSibling.ItemId else rtl.TVI_ROOT;
  InternalInsertNode(lInsertData, var aNode);
end;

{$ENDIF}

end.