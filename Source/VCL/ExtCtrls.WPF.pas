﻿namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TCustomControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: VCLString); override;
  end;

implementation

method TPanel.CreateHandle;
begin
  // We use a ContentControl and add a Canvas as Content.
  fHandle := new ContentControl();
  fPanel := new Canvas();
  (fHandle as System.Windows.Controls.ContentControl).Content := fPanel;
  System.Windows.Input.KeyboardNavigation.SetTabNavigation(fHandle, System.Windows.Input.KeyboardNavigationMode.Local);
  (fHandle as System.Windows.Controls.ContentControl).Focusable := false;
end;

method TPanel.PlatformSetCaption(aValue: VCLString);
begin
  // do nothing
end;

{$ENDIF}

end.