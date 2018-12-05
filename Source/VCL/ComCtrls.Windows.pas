namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TListColumn = public partial class(TCollectionItem)
  protected
    method PlatformSetCaption(aValue: String); partial;
  end;

  TListColumns = public partial class(TCollection)
  protected
    method PlatformAdd(aListColumn: TListColumn); partial;
  end;

  TSubItems = public partial class(TStringList)
  private
    method UpdateSubItem(aIndex: Integer);
  protected
    method PlatformPut(aIndex: Integer; S: DelphiString); partial;
    method PlatformAdd(S: DelphiString); partial;
    method PlatformAddObject(S: DelphiString; aObject: TObject); partial;
    method PlatformClear; partial;
    method PlatformDelete(aIndex: Integer); partial;
    method PlatformInsert(aIndex: Integer; S: DelphiString); partial;
  end;

  TListItem = public partial class(TPersistent)
  protected
    method PlatformSetCaption(aValue: String); partial;
    method PlatformGetSelected: Boolean; partial;
    method PlatformSetSelected(aValue: Boolean); partial;
  end;

  TListItems = public partial class(TPersistent)
  protected
    method PlatformAdd(aListItem: TListItem); partial;
    method PlatformClear; partial;
    method PlatformDelete(aIndex: Integer); partial;
  end;

  TListView = public partial class(TMultiSelectListControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method PlatformSetViewStyle(aValue: TViewStyle); partial;
    method PlatformSetRowSelect(aValue: Boolean); partial;

    [MessageAttribute(CN_NOTIFY)]
    method CNNotify(var aMessage: TMessage);
  end;

implementation

method TListColumn.PlatformSetCaption(aValue: String);
begin
  var lColumn: rtl.LVCOLUMN;
  lColumn.mask := rtl.LVCF_TEXT or rtl.LVCF_FMT;
  lColumn.pszText := aValue.ToLPCWSTR;

  rtl.SendMessage(((Collection as TListColumns).Owner as TListView).Handle, rtl.LVM_SETCOLUMN, ColIndex, rtl.LPARAM(@lColumn));
end;

method TListColumns.PlatformAdd(aListColumn: TListColumn);
begin
  var lColumn: rtl.LVCOLUMN;
  lColumn.mask := rtl.LVCF_FMT or rtl.LVCF_WIDTH or rtl.LVCF_TEXT or rtl.LVCF_SUBITEM;
  lColumn.iSubItem := Count - 1;
  lColumn.cx := 65;

  rtl.SendMessage(fOwner.Handle, rtl.LVM_INSERTCOLUMN, Count - 1, rtl.LPARAM(@lColumn));
end;

method TSubItems.UpdateSubItem(aIndex: Integer);
begin
  var lItem: rtl.LV_ITEMW;
  lItem.iSubItem := aIndex;
  lItem.mask := rtl.LVIF_TEXT or rtl.LVIF_IMAGE;
  lItem.pszText := rtl.LPSTR_TEXTCALLBACK;
  //lItem.pszText := String(S).ToLPCWSTR;
  rtl.SendMessage(fOwner.Owner.Owner.Handle, rtl.LVM_SETITEM, fOwner.Index, rtl.LPARAM(@lItem));
end;

method TSubItems.PlatformPut(aIndex: Integer; S: DelphiString);
begin
  for i: Integer := aIndex to Count  - 1 do
    UpdateSubItem(i);
end;

method TSubItems.PlatformAdd(S: DelphiString);
begin
  UpdateSubItem(Count);
end;

method TSubItems.PlatformAddObject(S: DelphiString; aObject: TObject);
begin
  UpdateSubItem(Count);
end;

method TSubItems.PlatformClear;
begin
  for i: Integer := 0 to Count  - 1 do
    UpdateSubItem(i);
end;

method TSubItems.PlatformDelete(aIndex: Integer);
begin
  for i: Integer := aIndex to Count  - 1 do
    UpdateSubItem(i);
end;

method TSubItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  for i: Integer := aIndex to Count  - 1 do
    UpdateSubItem(i);
end;

method TListItem.PlatformSetCaption(aValue: String);
begin
  var lItem: rtl.LV_ITEMW;
  lItem.iSubItem := 0;
  lItem.mask := rtl.LVIF_TEXT or rtl.LVIF_IMAGE;
  // using LPSTR_TEXTCALLBACK as text value means we store the text, so ListView will send LVN_GETDISPINFO messages.
  lItem.pszText := rtl.LPSTR_TEXTCALLBACK;
  //lItem.pszText := aValue.ToLPCWSTR;
  rtl.SendMessage(fOwner.Owner.Handle, rtl.LVM_SETITEMTEXT, &Index, rtl.LPARAM(@lItem));
end;

method TListItem.PlatformGetSelected: Boolean;
begin
  result := (rtl.SendMessage(fOwner.Owner.Handle, rtl.LVM_GETITEMSTATE, &Index, rtl.LPARAM(rtl.LVIS_SELECTED)) and rtl.LVIS_SELECTED) <> 0;
end;

method TListItem.PlatformSetSelected(aValue: Boolean);
begin
  var lItem: rtl.LV_ITEMW;
  lItem.iSubItem := 0;
  lItem.mask := rtl.LVIF_STATE;
  lItem.stateMask := rtl.LVIS_SELECTED;
  lItem.state := Integer(aValue);
  rtl.SendMessage(fOwner.Owner.Handle, rtl.LVM_SETITEMSTATE, &Index, rtl.LPARAM(@lItem));
end;

method TListItems.PlatformAdd(aListItem: TListItem);
begin
  var lHandle: rtl.LV_ITEMW;
  lHandle.iItem := fItems.Count - 1;
  lHandle.iSubItem := 0;
  lHandle.mask := rtl.LVIF_TEXT or rtl.LVIF_IMAGE;
  lHandle.pszText := 'New Item'.ToLPCWSTR;
  rtl.SendMessage(fOwner.Handle, rtl.LVM_INSERTITEM, 0, rtl.LPARAM(@lHandle));
end;

method TListItems.PlatformClear;
begin
  rtl.SendMessage(fOwner.Handle, rtl.LVM_DELETEALLITEMS, 0, 0);
end;

method TListItems.PlatformDelete(aIndex: Integer);
begin
  rtl.SendMessage(fOwner.Handle, rtl.LVM_DELETEITEM, aIndex, 0);
end;

type
  [CallingConvention(CallingConvention.Stdcall)]
  TInitEx = function(picce: ^rtl.INITCOMMONCONTROLSEX): rtl.BOOL;

method TListView.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);

  {var lInit: rtl.INITCOMMONCONTROLSEX;
  lInit.dwICC := rtl.ICC_LISTVIEW_CLASSES;
  rtl.InitCommonControlsEx(@lInit);}

  var lIce: rtl.INITCOMMONCONTROLSEX;
  lIce.dwICC := rtl.ICC_LISTVIEW_CLASSES;
  lIce.dwSize := sizeOf(lIce);
  var lModule := rtl.LoadLibrary('Comctl32.dll');
  var lProc := TInitEx(rtl.GetProcAddress(lModule, 'InitCommonControlsEx'));
  lProc(@lIce);

  aParams.WidgetClassName := rtl.WC_LISTVIEW.ToCharArray(true);
  aParams.Style := aParams.Style or rtl.LVS_ICON or rtl.WS_BORDER or rtl.WS_CLIPCHILDREN;
  CreateClass(var aParams);
end;

method TListView.PlatformSetViewStyle(aValue: TViewStyle);
begin
  var lCurrentStyle := rtl.GetWindowLong(fHandle, rtl.GWL_STYLE);
  var lNewStyle: rtl.DWORD := 0;

  case aValue of
    TViewStyle.vsIcon:
      lNewStyle := rtl.LVS_ICON;

    TViewStyle.vsList:
      lNewStyle := rtl.LVS_LIST;

    TViewStyle.vsSmallIcon:
      lNewStyle := rtl.LVS_SMALLICON;

    TViewStyle.vsReport:
      lNewStyle := rtl.LVS_REPORT;
  end;

  rtl.SetWindowLong(fHandle, rtl.GWL_STYLE, lCurrentStyle or lNewStyle);
end;

method TListView.PlatformSetRowSelect(aValue: Boolean);
begin
  //rtl.SendMessage(fHandle, rtl.LVM_SETEXTENDEDLISTVIEWSTYLE, rtl.LVS_EX_FULLROWSELECT, Convert.ToInt32(aValue));
  rtl.SendMessage(fHandle, rtl.LVM_SETEXTENDEDLISTVIEWSTYLE, 0, rtl.LVS_EX_FULLROWSELECT);
end;

method TListView.CNNotify(var aMessage: TMessage);
begin
  var lInfo: ^rtl.NMHDR := ^rtl.NMHDR(^Void(aMessage.lParam));
  case Integer(lInfo^.code) of
    rtl.LVN_GETDISPINFO: begin
      var lplvdi := ^rtl.NMLVDISPINFO(^Void(aMessage.lParam));
      case lplvdi^.item.iSubItem of
        0: begin
          // 0 as SubItem index is the caption
          lplvdi^.item.pszText := String(Items[lplvdi^.item.iItem].Caption).ToLPCWSTR;
        end;

        else begin
          if (lplvdi^.item.iSubItem - 1) < Items[lplvdi^.item.iItem].SubItems.Count then
            lplvdi^.item.pszText := String(Items[lplvdi^.item.iItem].SubItems[lplvdi^.item.iSubItem - 1]).ToLPCWSTR
          else
            lplvdi^.item.pszText := ''.ToLPCWSTR;
        end;
      end;
    end;

    rtl.LVN_ITEMCHANGED: begin
      var lpNmListView := ^rtl.NMLISTVIEW(^Void(aMessage.lParam));
      var lItem := Items[lpNmListView^.iItem];
      if (lpNmListView^.uOldState and rtl.LVIS_SELECTED <> 0) and (lpNmListView^.uNewState and rtl.LVIS_SELECTED = 0) then
        DoSelectItem(lItem, false)
      else if (lpNmListView^.uOldState and rtl.LVIS_SELECTED = 0) and (lpNmListView^.uNewState and rtl.LVIS_SELECTED <> 0) then
        DoSelectItem(lItem, true);
    end;
  end;
end;

{$ENDIF}

end.