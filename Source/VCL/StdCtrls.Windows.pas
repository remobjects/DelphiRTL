namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

{$GLOBALS ON}

interface

uses
  RemObjects.Elements.RTL.Delphi, rtl;

type
  TButton = public partial class(TWinControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    [MessageAttribute(CN_COMMAND)]
    method NCCommand(var aMessage: TMessage);
  end;

  TLabel = public partial class(TNativeControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

  TEdit = public partial class(TWinControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method PlatformGetMaxLength: Integer;
    method PlatformSetMaxLength(aValue: Integer);
    method PlatformGetReadOnly: Boolean;
    method PlatformSetReadOnly(aValue: Boolean);
    method PlatformGetText: String;
    method PlatformSetText(aValue: String);
  end;

  TCheckBox = public partial class(TButtonControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;

    method PlatformGetChecked: Boolean; override; partial;
    method PlatformSetChecked(aValue: Boolean); override; partial;
    method PlatformSetState(aValue: TCheckBoxState); partial;
  end;

  TRadioButton = public partial class(TButtonControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method CreateWnd; override;

    method Click; override;

    method PlatformGetChecked: Boolean; override; partial;
    method PlatformSetChecked(aValue: Boolean); override; partial;
  end;

  TListControlItems = public partial class(TStringList)
  private
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TListBox = public partial class(TMultiSelectListControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method PlatformSelectAll;
    method PlatformGetSelected(aIndex: Integer): Boolean;
    method PlatformSetSelected(aIndex: Integer; value: Boolean);
    method PlatformGetSelCount: Integer;
    method PlatformGetMultiSelect: Boolean;
    method PlatformSetMultiSelect(value: Boolean);
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(value: Integer);
    method PlatformGetItemIndex: Integer;
  end;

  TComboBoxItems = public partial class(TStringList)
  private
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TComboBox = public partial class(TListControl)
  protected
    fOldItemIndex: Integer := -1;
    fEditHandle: rtl.HWND;
    fOldEditWndProc: TWndProc;
    method CreateParams(var aParams: TCreateParams); override;
    method CreateHandle; override;
    method PlatformGetText: String;
    method PlatformSetText(aValue: String);
    method PlatformSetOnSelect(aValue: TNotifyEvent);
    method PlatformSetOnChange(aValue: TNotifyEvent);
    method PlatformSelectAll;
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(aValue: Integer);
    method PlatformGetItemIndex: Integer;

    [MessageAttribute(CN_COMMAND)]
    method CNCOMMAND(var aMessage: TMessage);

    class constructor;
  public
    method EditWndProc(var aMessage: TMessage): rtl.LRESULT;
    method WndProc(var aMessage: TMessage); override;
  end;

  TGroupBox = public partial class(TWinControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

  function ComboWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;

implementation

method TEdit.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.ExStyle := rtl.WS_EX_CLIENTEDGE;
  aParams.WidgetClassName := 'EDIT'.ToCharArray(true);
  CreateClass(var aParams);
end;

method TEdit.PlatformGetMaxLength: Integer;
begin
  result := rtl.SendMessage(fHandle, rtl.EM_GETLIMITTEXT, 0, 0);
end;

method TEdit.PlatformSetMaxLength(aValue: Integer);
begin
  rtl.SendMessage(fHandle, rtl.EM_SETLIMITTEXT, aValue, 0);
end;

method TEdit.PlatformGetReadOnly: Boolean;
begin
  result := (rtl.GetWindowLongPtr(fHandle, rtl.GWL_STYLE) and rtl.ES_READONLY) > 0;
end;

method TEdit.PlatformSetReadOnly(aValue: Boolean);
begin
  rtl.SendMessage(fHandle, rtl.EM_SETREADONLY, Integer(aValue), 0);
end;

method TEdit.PlatformGetText: String;
begin
  var lMaxLength := rtl.GetWindowTextLength(fHandle);
  var lBuffer := new Char[lMaxLength + 1];
  rtl.GetWindowText(fHandle, @lBuffer[0], lMaxLength + 1);
  result := String.FromPChar(@lBuffer[0]);
end;

method TEdit.PlatformSetText(aValue: String);
begin
  var lBuffer := aValue.ToCharArray(true);
  rtl.SetWindowText(fHandle, @lBuffer[0]);
end;

method TCheckBox.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  CreateClass(var aParams);
  aParams.Style := aParams.Style or rtl.BS_3STATE;
end;

method TCheckBox.PlatformGetChecked: Boolean;
begin
  result := (rtl.SendMessage(fHandle, rtl.BM_GETCHECK, 0, 0) = rtl.BST_CHECKED);
end;

method TCheckBox.PlatformSetChecked(aValue: Boolean);
begin
  rtl.SendMessage(fHandle, rtl.BM_SETCHECK, Convert.ToInt32(aValue), 0);
end;

method TCheckBox.PlatformSetState(aValue: TCheckBoxState);
begin
  rtl.SendMessage(fHandle, rtl.BM_SETCHECK, Convert.ToInt32(aValue), 0);
end;

method TRadioButton.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  CreateClass(var aParams);
  aParams.Style := aParams.Style or rtl.BS_RADIOBUTTON;
end;

method TRadioButton.PlatformGetChecked: Boolean;
begin
  result := (rtl.SendMessage(fHandle, rtl.BM_GETCHECK, 0, 0) = rtl.BST_CHECKED);
end;

method TRadioButton.PlatformSetChecked(aValue: Boolean);
begin
  var lValue := if aValue then 1 else 0;
  rtl.SendMessage(fHandle, rtl.BM_SETCHECK, Convert.ToInt32(lValue), 0);
end;

method TRadioButton.Click;
begin
  Checked := not Checked;
  inherited;
end;

method TButton.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  CreateClass(var aParams);
end;

method TButton.NCCommand(var aMessage: TMessage);
begin
  // wParam: WM_COMMAND notification, the important matter here!
  case aMessage.wParam of
    rtl.BN_CLICKED: begin
      if assigned(OnClick) then
        OnClick(self);
    end;
  end;
end;

method TLabel.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'STATIC'.ToCharArray(true);
  aParams.DefaultWndProc := true;
  CreateClass(var aParams);
end;

method TRadioButton.CreateWnd;
begin
  inherited;
  rtl.SendMessage(fHandle, rtl.BM_SETCHECK, rtl.WPARAM(0), 0);
end;

method TListControlItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lString := String(S);
  var lArray := lString.ToCharArray(true);
  rtl.SendMessage(ListControl.Handle, rtl.LB_ADDSTRING, 0, rtl.LPARAM(@lArray[0]));
end;

method TListControlItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  var lString := String(S);
  var lArray := lString.ToCharArray(true);
  rtl.SendMessage(ListControl.Handle, rtl.LB_INSERTSTRING, aIndex, rtl.LPARAM(@lArray[0]));
end;

method TListControlItems.PlatformClear;
begin
  rtl.SendMessage(ListControl.Handle, rtl.LB_RESETCONTENT, 0, 0);
end;

method TListControlItems.PlatformDelete(aIndex: Integer);
begin
  rtl.SendMessage(ListControl.Handle, rtl.LB_DELETESTRING, aIndex, 0);
end;

method TListBox.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'LISTBOX'.ToCharArray(true);
  aParams.Style := aParams.Style or rtl.ES_AUTOVSCROLL;
  if MultiSelect then aParams.Style := aParams.Style or rtl.LBS_EXTENDEDSEL;
  aParams.ExStyle := rtl.WS_EX_CLIENTEDGE;
  CreateClass(var aParams);
end;

method TListBox.PlatformSelectAll;
begin
  rtl.SendMessage(fHandle, rtl.LB_SETSEL, 1, -1); // using -1 as item index does the trick
end;

method TListBox.PlatformGetSelected(aIndex: Integer): Boolean;
begin
  result := rtl.SendMessage(fHandle, rtl.LB_GETSEL, aIndex, 0) > 0;
end;

method TListBox.PlatformSetSelected(aIndex: Integer; value: Boolean);
begin
  rtl.SendMessage(fHandle, rtl.LB_SETSEL, Convert.ToInt32(value), aIndex);
end;

method TListBox.PlatformGetSelCount: Integer;
begin
  result := 0; // TODO
end;

method TListBox.PlatformGetMultiSelect: Boolean;
begin
  // TODO recreateWnd, can not be changed in runtime...
end;

method TListBox.PlatformSetMultiSelect(value: Boolean);
begin
  // TODO recreateWnd, can not be changed in runtime...
end;

method TListBox.PlatformClearSelection;
begin
  rtl.SendMessage(fHandle, rtl.LB_SETSEL, 0, -1); // using -1 as item index does the trick
end;

method TListBox.PlatformDeleteSelected;
begin
  var lIndex := ItemIndex;
  if lIndex ≥ 0 then
    Items.Delete(lIndex);
end;

method TListBox.PlatformSetItemIndex(value: Integer);
begin
  rtl.SendMessage(fHandle, rtl.LB_SETCURSEL, value, 0);
end;

method TListBox.PlatformGetItemIndex: Integer;
begin
  result := rtl.SendMessage(fHandle, rtl.LB_GETCURSEL, 0, 0);
end;

method TComboBoxItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin
  var lString := String(S);
  var lArray := lString.ToCharArray(true);
  rtl.SendMessage(ListControl.Handle, rtl.CB_ADDSTRING, 0, rtl.LPARAM(@lArray[0]));
end;

method TComboBoxItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin
  var lString := String(S);
  var lArray := lString.ToCharArray(true);
  rtl.SendMessage(ListControl.Handle, rtl.CB_INSERTSTRING, aIndex, rtl.LPARAM(@lArray[0]));
end;

method TComboBoxItems.PlatformClear;
begin
  rtl.SendMessage(ListControl.Handle, rtl.CB_RESETCONTENT, 0, 0);
end;

method TComboBoxItems.PlatformDelete(aIndex: Integer);
begin
  rtl.SendMessage(ListControl.Handle, rtl.CB_DELETESTRING, aIndex, 0);
end;

method TComboBox.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'COMBOBOX'.ToCharArray(true);
  aParams.Style := aParams.Style or rtl.CBS_DROPDOWN or rtl.CBS_HASSTRINGS or rtl.CBS_AUTOHSCROLL;
  aParams.ExStyle := rtl.WS_EX_CLIENTEDGE;
  CreateClass(var aParams);
end;

method TComboBox.CreateHandle;
begin
  inherited;
  // Here we get the handle of the edit component
  var lInfo: rtl.COMBOBOXINFO;
  lInfo.cbSize := sizeOf(lInfo);
  rtl.GetComboBoxInfo(fHandle, @lInfo);
  fEditHandle := lInfo.hwndItem;
  fOldEditWndProc := InternalCalls.Cast<TWndProc>(^Void(rtl.GetWindowLongPtr(fEditHandle, rtl.GWL_WNDPROC)));
  rtl.SetWindowLongPtr(fEditHandle, rtl.GWL_USERDATA, NativeUInt(InternalCalls.Cast(self)));
  fOldEditWndProc := TWndProc(^Void(rtl.SetWindowLongPtr(fEditHandle, rtl.GWL_WNDPROC, NativeUInt(^Void(@ComboWndProc)))));
end;

method TComboBox.EditWndProc(var aMessage: TMessage): rtl.LRESULT;
begin
  result := rtl.CallWindowProc(fOldEditWndProc, fEditHandle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;

method TComboBox.WndProc(var aMessage: TMessage);
begin
  if aMessage.hWnd = fEditHandle then begin
    case aMessage.Msg of
      rtl.WM_KEYDOWN: begin
        if aMessage.wParam = rtl.VK_TAB then begin
          var lShiftState := GetSpecialKeysStatus;
          var lParentForm := GetParentForm;
          if lParentForm ≠ nil then
            lParentForm.SelectNextControl(self, TShiftStateValues.ssShift in lShiftState);
        end;
      end;

      rtl.WM_SETFOCUS: begin
        var lParentForm := GetParentForm;
        if lParentForm ≠ nil then
          lParentForm.ActiveControl := self;
      end;
    end;

    aMessage.Result := rtl.CallWindowProc(fOldEditWndProc, fEditHandle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
  end
  else
    //aMessage.Result := rtl.CallWindowProc(fOldWndProc, fHandle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
    inherited(var aMessage);
end;

function ComboWndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  var lObject := rtl.GetWindowLongPtr(hWnd, rtl.GWLP_USERDATA);
  if lObject <> 0 then begin
    var lControl := InternalCalls.Cast<TComboBox>(^Void(lObject));
    var lMessage := new TMessage(hWnd, message, wParam, lParam);
    lControl.WndProc(var lMessage);
    result := lMessage.Result;
  end
  else begin
    result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
  end;
end;


method TComboBox.PlatformSetOnSelect(aValue: TNotifyEvent);
begin

end;

method TComboBox.PlatformSetOnChange(aValue: TNotifyEvent);
begin

end;

method TComboBox.PlatformGetText: String;
begin
  var lMaxLength := rtl.GetWindowTextLength(fHandle);
  var lBuffer := new Char[lMaxLength + 1];
  rtl.GetWindowText(fHandle, @lBuffer[0], lMaxLength + 1);
  result := String.FromPChar(@lBuffer[0]);
end;

method TComboBox.PlatformSetText(aValue: String);
begin
  var lBuffer := aValue.ToCharArray(true);
  rtl.SetWindowText(fHandle, @lBuffer[0]);
end;

method TComboBox.PlatformSelectAll;
begin
  rtl.SendMessage(fHandle, rtl.CB_SETCURSEL, 1, -1);
end;

method TComboBox.PlatformClearSelection;
begin
  rtl.SendMessage(fHandle, rtl.CB_SETCURSEL, -1, -1);
end;

method TComboBox.PlatformDeleteSelected;
begin
  var lIndex := ItemIndex;
  if lIndex ≥ 0 then
    Items.Delete(lIndex);
end;

method TComboBox.PlatformSetItemIndex(aValue: Integer);
begin
  rtl.SendMessage(fHandle, rtl.CB_SETCURSEL, aValue, 0);
end;

method TComboBox.PlatformGetItemIndex: Integer;
begin
  result := rtl.SendMessage(fHandle, rtl.CB_GETCURSEL, 0, 0);
end;

method TComboBox.CNCommand(var aMessage: TMessage);
begin
  case aMessage.wParam of
    rtl.CBN_DROPDOWN: begin
    end;

    rtl.CBN_CLOSEUP: begin
    end;

    rtl.CBN_SELENDOK: begin
      if assigned(fOnSelect) then fOnSelect(self);
    end;

    rtl.CBN_SELCHANGE: begin
    end;
  end;
end;

class constructor TComboBox;
begin
  TMessageTableCache.MessageTableFor(typeOf(self));
end;

method TGroupBox.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  aParams.Style := aParams.Style or rtl.BS_GROUPBOX or rtl.WS_GROUP or rtl.WS_CHILD;
  CreateClass(var aParams);
end;

{$ENDIF}

end.