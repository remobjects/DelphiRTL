namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ISLAND AND LINUX}

interface

uses
  RemObjects.Elements.RTL.Delphi;

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

  var lHandle := rtl.dlopen(nil, rtl.RTLD_LAZY);
  var lSection := 'ISLNDRES'.ToAnsiChars(true);
  var lData := rtl.dlsym(lHandle, @lSection[0]);
  if lData <> nil then
    writeLn("yes")
  else
    writeLn("no");
  //var lSize: {$IF __LP64__}UInt64{$ELSE}UInt32{$ENDIF};
  //var lSecName := NSString('__island_res').UTF8String;
  //var lData := NSString('__DATA').UTF8String;
  //var lStart := rtl.getsectiondata(@rtl._mh_execute_header, lData, lSecName, @lSize);




  {var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;
  if lName.ToUpper <> 'TFORM' then begin
    var lStream := new TResourceStream(0, lName, nil);
    lStream.Position := 0;
    var lReader := new TReader(lStream, 100);
    lReader.ReadRootComponent(self);
  end;}
end;

method TForm.Show;
begin
  gtk.gtk_widget_show(fHandle);
end;

method TForm.CreateHandle;
begin
  fHandle := gtk.gtk_window_new(gtk.GtkWindowType.GTK_WINDOW_TOPLEVEL);
  gobject.g_signal_connect_data(fHandle, "destroy", glib.GVoidFunc(()->gtk.gtk_main_quit), nil, nil, 0);
end;

{$ENDIF}

end.