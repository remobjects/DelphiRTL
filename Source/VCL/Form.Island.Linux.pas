namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND LINUX}

interface

uses
  RemObjects.Elements.System, RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class(TNativeControl)
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
  lName := lName.Substring(lName.LastIndexOf('.') + 1);
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  if lName.ToUpper <> 'TFORM' then begin
    var lRes := Resources.FindResource(lName);
    if lRes <> nil then begin
      var lStream := new TMemoryStream();
      lStream.Write(lRes.Data.Pointer, lRes.Data.Length);
      lStream.Position := 0;
      var lReader := new TReader(lStream, 100);
      lReader.ReadRootComponent(self);
    end
    else
      raise new Exception("Resource: " + lName + " not found!");
  end;
end;

method TForm.Show;
begin
  gtk.gtk_widget_show(fHandle);
end;

method TForm.CreateHandle;
begin
  fHandle := gtk.gtk_window_new(gtk.GtkWindowType.GTK_WINDOW_TOPLEVEL);
  gobject.g_signal_connect_data(fHandle, @PlatformString("destroy").ToAnsiChars(true)[0], glib.GVoidFunc(()->gtk.gtk_main_quit), nil, nil, 0);
  fBox := gtk.gtk_fixed_new();
  gtk.gtk_container_add(^gtk.GtkContainer(fHandle), fBox);
  gtk.gtk_widget_show(fBox);
end;

{$ENDIF}

end.