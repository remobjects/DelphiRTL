namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

type
  TCustomForm = public partial class(TNativeControl)
  public
    constructor(aOwner: TComponent);
  end;

implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  var lStream := new TResourceStream(0, 'Unit6.dfm'{lName});
  lStream.Position := 0;
  var lReader := new TReader(lStream, 100);
  lReader.ReadRootComponent(self);
end;
{$ENDIF}

end.