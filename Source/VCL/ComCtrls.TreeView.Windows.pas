namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TTreeNode = public partial class(TPersistent)
  private assembly
    fNodeHandle: rtl.HTREEITEM;
  end;

  TTreeNodes = public partial class(TPersistent)
  protected
    method PlatformAddChild(aParent: TTreeNode; aNode: TTreeNode); partial;
    method PlatformAddChildFirst(aParent: TTreeNode; aNode: TTreeNode); partial;
    method PlatformAdd(aSibling: TTreeNode; aNode: TTreeNode); partial;
    method PlatformAddFirst(aSibling: TTreeNode; aNode: TTreeNode); partial;
  end;

  TTreeView = public partial class(TWinControl)
  private
    class constructor;
  protected
    method CreateParams(var aParams: TCreateParams); override;
  public
    method WndProc(var Message: TMessage); override;
  end;

implementation

method TTreeView.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);

  aParams.WidgetClassName := rtl.WC_TREEVIEW.ToCharArray(true);
  aParams.Style := aParams.Style or rtl.LVS_ICON or rtl.WS_BORDER or rtl.WS_CLIPCHILDREN;
  CreateClass(var aParams);
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

method TTreeView.WndProc(var Message: TMessage);
begin
  inherited(var Message);
end;

method TTreeNodes.PlatformAddChild(aParent: TTreeNode; aNode: TTreeNode);
begin
  var lInsertData: rtl.TVINSERTSTRUCT;
  var lNode: rtl.TVITEM;

  lNode.mask := rtl.TVIF_TEXT or rtl.TVIF_PARAM;
  lNode.pszText := aNode.Text.ToLPCWSTR;
  lInsertData.u.item := lNode;
  lInsertData.hInsertAfter := rtl.TVI_LAST;

  if aParent <> nil then
    lInsertData.hParent := aParent.fNodeHandle
  else
    lInsertData.hParent := rtl.TVI_ROOT;

  aNode.fNodeHandle := rtl.HTREEITEM(rtl.SendMessage(fOwner.Handle, rtl.TVM_INSERTITEM, 0, rtl.LPARAM(@lInsertData)));
end;

method TTreeNodes.PlatformAddChildFirst(aParent: TTreeNode; aNode: TTreeNode);
begin

end;

method TTreeNodes.PlatformAdd(aSibling: TTreeNode; aNode: TTreeNode);
begin

end;

method TTreeNodes.PlatformAddFirst(aSibling: TTreeNode; aNode: TTreeNode);
begin

end;

{$ENDIF}

end.