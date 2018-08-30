﻿namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TControl = public partial class(TComponent)
  protected
    fPanel: Canvas;
    method HandleAllocated: Boolean; virtual; partial;
    method PlatformSetWidth(aValue: Integer); virtual; partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;
    method PlatformSetCaption(aValue: String); virtual; partial;

    method PlatformFontChanged; virtual; partial;

    method PlatformGetDefaultName: String; virtual; partial;
    method PlatformApplyDefaults; virtual; partial;
  public

  end;

implementation

method TControl.HandleAllocated: Boolean;
begin
  result := fHandle ≠ nil;
end;

method TControl.PlatformSetWidth(aValue: Integer);
begin
  fHandle.Width := aValue;
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  fHandle.Height := aValue;
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  fPanel.SetTop(fHandle, aValue);
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
  fPanel.SetLeft(fHandle, aValue);
end;

method TControl.PlatformSetParent(aValue: TControl);
begin
  aValue.fPanel.Children.Add(fHandle);
end;

method TControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  // Ok, there is no Click event on Control, so need to check that current one is subclass of ButtonBase
  if fHandle.GetType.IsSubclassOf(TypeOf(System.Windows.Controls.Primitives.ButtonBase)) then
    (fHandle as System.Windows.Controls.Primitives.ButtonBase).Click += new System.Windows.RoutedEventHandler((s, e)-> begin if assigned(fOnClick) then fOnClick(self); end);
end;

method TControl.PlatformSetOnKeyPress(aValue: TKeyPressEvent);
begin

end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin

end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin

end;

method TControl.PlatformSetCaption(aValue: String);
begin
  (fHandle as ContentControl).Content := aValue;
end;

method TControl.PlatformFontChanged;
begin

end;

method TControl.PlatformGetDefaultName: String;
begin

end;

method TControl.PlatformApplyDefaults;
begin

end;

{$ENDIF}

end.