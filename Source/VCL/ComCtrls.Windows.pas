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
    method PlatformAdd: TListColumn; partial;
  end;

  TListItem = public partial class(TPersistent)
  protected
    method PlatformSetCaption(aValue: String); partial;
  end;

  TListItems = public partial class(TPersistent)
  protected
    method PlatformAdd; partial;
    method PlatformClear; partial;
    method PlatformDelete(aIndex: Integer); partial;
  end;

  TListView = public partial class(TMultiSelectListControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method PlatformSetViewStyle(aValue: TViewStyle); partial;
  end;

implementation

method TListColumn.PlatformSetCaption(aValue: String);
begin
  var lColumn: rtl.LVCOLUMN;
  lColumn.mask := rtl.LVCF_TEXT or rtl.LVCF_FMT;
  lColumn.pszText := aValue.ToLPCWSTR;

  rtl.SendMessage(((Collection as TListColumns).Owner as TListView).Handle, rtl.LVM_SETCOLUMN, ColIndex, rtl.LPARAM(@lColumn));
end;

method TListColumns.PlatformAdd: TListColumn;
begin
  var lColumn: rtl.LVCOLUMN;
  lColumn.mask := rtl.LVCF_FMT or rtl.LVCF_WIDTH or rtl.LVCF_TEXT or rtl.LVCF_SUBITEM;
  lColumn.iSubItem := Count - 1;
  lColumn.cx := 65;

  rtl.SendMessage(fOwner.Handle, rtl.LVM_INSERTCOLUMN, Count - 1, rtl.LPARAM(@lColumn));
end;

method TListItem.PlatformSetCaption(aValue: String);
begin
  var lItem: rtl.LV_ITEMW;
  lItem.iSubItem := 0;
  lItem.mask := rtl.LVIF_TEXT or rtl.LVIF_IMAGE;
  //lItem.pszText := rtl.LPSTR_TEXTCALLBACK;
  lItem.pszText := aValue.ToLPCWSTR;
  rtl.SendMessage(fOwner.Owner.Handle, rtl.LVM_SETITEMTEXT, &Index, rtl.LPARAM(@lItem));
end;

method TListItems.PlatformAdd;
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

{$ENDIF}

end.