namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TButton = public partial class(TNativeControl)
  protected
    method CreateHandle; partial; override;
    method PlatformSetCaption(aValue: String); partial; override;
  end;

  TLabel = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TGroupBox = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

  TEdit = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
    method PlatformSetText(aValue: String);
    method PlatformGetText: String;
    method PlatformGetMaxLength: Integer;
    method PlatformSetMaxLength(aValue: Integer);
    method PlatformGetReadOnly: Boolean;
    method PlatformSetReadOnly(aValue: Boolean);
  end;

  TButtonControl = public partial class(TNativeControl)
  protected
    method PlatformGetChecked: Boolean; virtual; partial;
    method PlatformSetChecked(aValue: Boolean); virtual; partial;
  end;

  TCheckBox = public partial class(TButtonControl)
  protected
    method CreateHandle; override;

    method PlatformSetState(aValue: TCheckBoxState); partial;
  end;

  TRadioButton = public class(TButtonControl)
  protected
    method CreateHandle; override;

    method Click; override;
  end;

  TListControlItems = public partial class(TStringList)
  protected
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TListBox = public partial class(TMultiSelectListControl)
  protected
    method CreateHandle; override;
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
    method PlatformAddItem(S: DelphiString; aObject: TObject);
    method PlatformInsert(aIndex: Integer; S: DelphiString);
    method PlatformClear;
    method PlatformDelete(aIndex: Integer);
  end;

  TComboBox = public partial class(TListControl)
  protected
    method CreateHandle; override;
    method PlatformGetText: String;
    method PlatformSetText(aValue: String);
    method PlatformSetOnSelect(aValue: TNotifyEvent);
    method PlatformSetOnChange(aValue: TNotifyEvent);
    method PlatformSelectAll;
    method PlatformClearSelection;
    method PlatformDeleteSelected;
    method PlatformSetItemIndex(value: Integer);
    method PlatformGetItemIndex: Integer;
  end;

  procedure PlatformShowMessage(aMessage: String);

implementation

procedure PlatformShowMessage(aMessage: String);
begin
  System.Windows.MessageBox.Show(aMessage);
end;

method TButton.CreateHandle;
begin
  fHandle := new Button();
end;

method TButton.PlatformSetCaption(aValue: String);
begin
  (fHandle as Button).Content := aValue;
end;

method TLabel.CreateHandle;
begin
  fHandle := new Label();
end;

method TLabel.PlatformSetCaption(aValue: String);
begin
  Width := 31;
  Height := 13;
  (fHandle as Label).Content := aValue;
end;

method TGroupBox.CreateHandle;
begin
  fHandle := new GroupBox();
end;

method TGroupBox.PlatformSetCaption(aValue: String);
begin
  (fHandle as GroupBox).Content := aValue;
end;

method TEdit.CreateHandle;
begin
  fHandle := new TextBox();
end;

method TEdit.PlatformSetText(aValue: String);
begin
  (fHandle as TextBox).Text := aValue;
end;

method TEdit.PlatformGetText: String;
begin
  result := (fHandle as TextBox).Text;
end;

method TEdit.PlatformGetMaxLength: Integer;
begin
  result := (fHandle as TextBox).MaxLength;
end;

method TEdit.PlatformSetMaxLength(aValue: Integer);
begin
  (fHandle as TextBox).MaxLength := aValue;
end;

method TEdit.PlatformGetReadOnly: Boolean;
begin
  result := (fHandle as TextBox).IsReadOnly;
end;

method TEdit.PlatformSetReadOnly(aValue: Boolean);
begin
  (fHandle as TextBox).IsReadOnly := aValue;
end;

method TButtonControl.PlatformGetChecked: Boolean;
begin
  result := (fHandle as System.Windows.Controls.Primitives.ToggleButton).IsChecked
end;

method TButtonControl.PlatformSetChecked(aValue: Boolean);
begin
  (fHandle as System.Windows.Controls.Primitives.ToggleButton).IsChecked := aValue;
end;

method TCheckBox.CreateHandle;
begin
  fHandle := new CheckBox();
end;

method TCheckBox.PlatformSetState(aValue: TCheckBoxState);
begin

end;

method TRadioButton.CreateHandle;
begin
  fHandle := new RadioButton();
end;

method TRadioButton.Click;
begin

end;

method TListControlItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin

end;

method TListControlItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin

end;

method TListControlItems.PlatformClear;
begin

end;

method TListControlItems.PlatformDelete(aIndex: Integer);
begin

end;

method TListBox.CreateHandle;
begin
  fHandle := new ListBox();
end;

method TListBox.PlatformSelectAll;
begin

end;

method TListBox.PlatformGetSelected(aIndex: Integer): Boolean;
begin

end;

method TListBox.PlatformSetSelected(aIndex: Integer; value: Boolean);
begin

end;

method TListBox.PlatformGetSelCount: Integer;
begin

end;

method TListBox.PlatformGetMultiSelect: Boolean;
begin

end;

method TListBox.PlatformSetMultiSelect(value: Boolean);
begin

end;

method TListBox.PlatformClearSelection;
begin

end;

method TListBox.PlatformDeleteSelected;
begin

end;

method TListBox.PlatformSetItemIndex(value: Integer);
begin

end;

method TListBox.PlatformGetItemIndex: Integer;
begin

end;

method TComboBoxItems.PlatformAddItem(S: DelphiString; aObject: TObject);
begin

end;

method TComboBoxItems.PlatformInsert(aIndex: Integer; S: DelphiString);
begin

end;

method TComboBoxItems.PlatformClear;
begin

end;

method TComboBoxItems.PlatformDelete(aIndex: Integer);
begin

end;

method TComboBox.CreateHandle;
begin
  fHandle := new ComboBox();
end;

method TComboBox.PlatformGetText: String;
begin

end;

method TComboBox.PlatformSetText(aValue: String);
begin

end;

method TComboBox.PlatformSetOnSelect(aValue: TNotifyEvent);
begin

end;

method TComboBox.PlatformSetOnChange(aValue: TNotifyEvent);
begin

end;

method TComboBox.PlatformSelectAll;
begin

end;

method TComboBox.PlatformClearSelection;
begin

end;

method TComboBox.PlatformDeleteSelected;
begin

end;

method TComboBox.PlatformSetItemIndex(value: Integer);
begin

end;

method TComboBox.PlatformGetItemIndex: Integer;
begin

end;

{$ENDIF}

end.