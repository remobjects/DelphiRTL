namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

interface

uses
  System.Windows.Controls, RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class(TNativeControl)
  protected
    method PlatformSetTop(aValue: Integer); override;
    method PlatformSetLeft(aValue: Integer); override;
    method PlatformSetCaption(aValue: String); override;
    method PlatformInitControl; override;

  public
    constructor(aOwner: TComponent);
  end;

  TForm = public partial class(TCustomForm)
  protected
    method CreateHandle; override;
  public
    method Show; override;
  end;

implementation

constructor TCustomForm(aOwner: TComponent);
begin
  HandleNeeded;
  var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  if lName.ToUpper <> 'TFORM' then begin
    // the resource is on the .exe, not here!
    var lAssembly := System.Reflection.Assembly.GetEntryAssembly();
    var lResStream := lAssembly.GetManifestResourceStream(lName);
    var lBuffer := new Byte[lResStream.Length];
    lResStream.Read(lBuffer, 0, lResStream.Length);
    var lStream := new TMemoryStream();
    lStream.Write(lBuffer, lResStream.Length);
    lStream.Position := 74; // Discard header
    var lReader := new TReader(lStream, 100);
    lReader.ReadRootComponent(self);
  end;
end;

method TCustomForm.PlatformSetTop(aValue: Integer);
begin
  (fHandle as System.Windows.Window).Top := aValue;
end;

method TCustomForm.PlatformSetLeft(aValue: Integer);
begin
  (fHandle as System.Windows.Window).Left := aValue;
end;

method TCustomForm.PlatformSetCaption(aValue: String);
begin
  (fHandle as System.Windows.Window).Title := aValue;
end;

method TCustomForm.PlatformInitControl;
begin
  fHeightDelta := Integer(System.Windows.SystemParameters.WindowCaptionHeight +
    System.Windows.SystemParameters.ResizeFrameHorizontalBorderHeight);
  fWidthDelta := Integer(System.Windows.SystemParameters.ResizeFrameVerticalBorderWidth);
end;

method TForm.CreateHandle;
begin
  fHandle := new System.Windows.Window();
  fPanel := new Canvas();
  (fHandle as System.Windows.Window).Content := fPanel;
end;

method TForm.Show;
begin
  (fHandle as System.Windows.Window).Show;
end;


{$ENDIF}

end.