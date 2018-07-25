namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi, rtl;

type
  TButton = public partial class(TWinControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
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

  TListControl = public partial abstract class(TNativeControl)
  protected
    method PlatformGetMultiSelect: Boolean;
    method PlatformSetMultiSelect(value: Boolean);
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(value: Integer);
    method PlatformGetItemIndex: Integer;
  end;

  TListBox = public partial class(TListControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method PlatformSelectAll;
    method PlatformGetSelected(aIndex: Integer): Boolean;
    method PlatformSetSelected(aIndex: Integer; value: Boolean);
  end;

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

method TListControl.PlatformGetMultiSelect: Boolean;
begin
  // TODO recreateWnd, can not be changed in runtime...
end;

method TListControl.PlatformSetMultiSelect(value: Boolean);
begin
  // TODO recreateWnd, can not be changed in runtime...
end;

method TListControl.PlatformClearSelection;
begin
  rtl.SendMessage(fHandle, rtl.LB_SETSEL, 0, -1); // using -1 as item index does the trick
end;

method TListControl.PlatformDeleteSelected;
begin
  var lIndex := ItemIndex;
  if lIndex ≥ 0 then
    Items.Delete(lIndex);
end;

method TListControl.PlatformSetItemIndex(value: Integer);
begin
  rtl.SendMessage(fHandle, rtl.LB_SETCURSEL, value, 0);
end;

method TListControl.PlatformGetItemIndex: Integer;
begin
  result := rtl.SendMessage(fHandle, rtl.LB_GETCURSEL, 0, 0);
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
{$ENDIF}

end.