namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TButton = public partial class(TControl)
  protected
    method PlatformSetCaption(aValue: String); partial;
  end;


implementation

method TButton.PlatformSetCaption(aValue: String);
begin

end;

end.