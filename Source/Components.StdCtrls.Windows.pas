namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TButton = public partial class(TWinControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

  TLabel = public partial class(TWinControl)
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

  TButtonControl = public partial class(TWinControl)
  protected
    method PlatformGetChecked: Boolean; virtual; partial;
    method PlatformSetChecked(aValue: Boolean); virtual; partial;
  end;

  TCheckBox = public partial class(TButtonControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
  end;

  TRadioButton = public partial class(TButtonControl)
  protected
    method CreateParams(var aParams: TCreateParams); override;
    method CreateWnd; override;
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

method TButtonControl.PlatformGetChecked: Boolean;
begin
  result := false;
end;

method TButtonControl.PlatformSetChecked(aValue: Boolean);
begin

end;

method TCheckBox.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  CreateClass(var aParams, 'BUTTON');
  aParams.Style := aParams.Style or rtl.BS_3STATE;
end;

method TRadioButton.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  CreateClass(var aParams, 'BUTTON');
  aParams.Style := aParams.Style or rtl.BS_RADIOBUTTON;
end;

method TButton.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'BUTTON'.ToCharArray(true);
  CreateClass(var aParams, 'BUTTON');
end;

method TLabel.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'STATIC'.ToCharArray(true);
  CreateClass(var aParams, 'STATIC');
end;

method TEdit.CreateParams(var aParams: TCreateParams);
begin
  inherited(var aParams);
  aParams.WidgetClassName := 'EDIT'.ToCharArray(true);
  CreateClass(var aParams, 'EDIT');
end;

method TRadioButton.CreateWnd;
begin
  inherited;
  rtl.SendMessage(fHandle, rtl.BM_SETCHECK, rtl.WPARAM(1), 0);
end;

{$ENDIF}

end.