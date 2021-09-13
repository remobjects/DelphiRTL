namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

//{$DEFINE ASYNC_FORM}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class(TNativeControl)
  protected
    method PlatformSetCaption(aValue: VCLString); override;
  public
    constructor(aOwner: TComponent);
  end;

  TForm = public partial class(TCustomForm)
  protected
    method CreateHandle; override;
  public
    method Show; override;
    method Show(aRootView: Object);
  end;

implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  if lName.ToUpper <> 'TFORM' then begin
    {$IF ASYNC_FORM}
    var lHttp := Browser.NewXMLHttpRequest();
    lHttp.open('GET', 'wasm/resources/' + lName + '.dfm');
    lHttp.overrideMimeType('text/plain; charset=x-user-defined');
    lHttp.onload := new WebAssemblyDelegate(() -> begin
      var lStream := new TResourceStream(0, lName + '.dfm', lHttp.responseText);
      lStream.Position := 0;
      var lReader := new TReader(lStream, 100);
      lReader.ReadRootComponent(self);
    end);
    lHttp.send(nil);
    {$ELSE}
    var lStream := new TResourceStream(0, lName + '.dfm');
    lStream.Position := 0;
    var lReader := new TReader(lStream, 100);
    lReader.ReadRootComponent(self);
    {$ENDIF}
  end;
end;

method TCustomForm.PlatformSetCaption(aValue: VCLString);
begin
  // do nothing
end;

method TForm.CreateHandle;
begin
  fHandle := Browser.CreateElement('div');
  fHandle.style.position := "relative";
  fHandle.style.margin := "0 auto";
end;

method TForm.Show(aRootView: Object);
begin
  var lRootView: dynamic := aRootView;
  if lRootView = nil then begin
    // No parent html element provided to 'host' the main div
    var lWindow := Browser.GetWindowObject;
    lRootView := Browser.CreateElement('div');
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