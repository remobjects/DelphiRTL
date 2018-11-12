namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

type
  TCustomForm = public partial class(TNativeControl)
  protected
    method PlatformSetCaption(aValue: String); override;
  public
    constructor(aOwner: TComponent);
  end;

  TForm = public partial class(TCustomForm)
  protected
    method CreateHandle; override;
  public
    method Show; override;
    method Show(aRootView: dynamic);
  end;

implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  if lName.ToUpper <> 'TFORM' then begin
    var lStream := new TResourceStream(0, lName + '.dfm');
    lStream.Position := 0;
    var lReader := new TReader(lStream, 100);
    lReader.ReadRootComponent(self);
  end;
end;

method TCustomForm.PlatformSetCaption(aValue: String);
begin
  // do nothing
end;

method TForm.CreateHandle;
begin
  fHandle := WebAssembly.CreateElement('div');
  fHandle.style.position := "relative";
  fHandle.style.margin := "0 auto";
end;

method TForm.Show(aRootView: dynamic);
begin
  var lRootView := aRootView;
  if lRootView = nil then begin
    // No parent html element provided to 'host' the main div
    var lWindow := WebAssembly.GetWindowObject;
    lRootView := WebAssembly.CreateElement('div');
    lRootView.style.margin := "0 auto";
    lWindow.document.body.appendChild(lRootView);
  end;

  lRootView.appendChild(fHandle);
end;

method TForm.Show;
begin
  Show(nil);
end;

{$ENDIF}

end.