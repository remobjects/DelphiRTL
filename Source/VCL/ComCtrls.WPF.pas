namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

type
  TListView = public partial class(TMultiSelectListControl)
  protected
    method CreateHandle; override;

    method PlatformSetViewStyle(aValue: TViewStyle); partial;
  end;

implementation

method TListView.CreateHandle;
begin
  fHandle := new ListView();
end;

method TListView.PlatformSetViewStyle(aValue: TViewStyle);
begin
end;

{$ENDIF}

end.