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

implementation

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
  rtl.GetWindowText(fHandle, @lBuffer[0], lMaxLength);
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

method TEdit.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'EDIT'.ToCharArray(true);
  CreateClass(var aParams);
end;

method TRadioButton.CreateWnd;
begin
  inherited;
  rtl.SendMessage(fHandle, rtl.BM_SETCHECK, rtl.WPARAM(0), 0);
end;

{$ENDIF}

end.