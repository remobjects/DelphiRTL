namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND LINUX}

interface

type
  TPanel = public partial class(TCustomControl)
  protected
    method CreateHandle; override;
    method PlatformSetCaption(aValue: String); override;
  end;

implementation

method TPanel.CreateHandle;
begin
  fHandle := gtk.gtk_fixed_new();
end;

method TPanel.PlatformSetCaption(aValue: String);
begin
  // do nothing
end;

{$ENDIF}

end.