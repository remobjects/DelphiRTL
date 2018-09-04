namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

type
  TPanel = public partial class(TCustomControl)
  protected
    fPanel: Canvas;
    method CreateHandle; override;
    method HandleAllocated: Boolean; override;
    method PlatformSetParent(aValue: TControl); override;
  end;

implementation

method TPanel.CreateHandle;
begin
  // WPF, Panel does not descend from Control...
  fPanel := new Canvas();
end;

method TPanel.HandleAllocated: Boolean;
begin
  result := fPanel ≠ nil;
end;

method TPanel.PlatformSetParent(aValue: TControl);
begin
  fPanel.Children.Add(aValue.Handle);
end;

{$ENDIF}

end.