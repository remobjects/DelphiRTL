namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF ECHOESWPF}

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
  var lName := typeOf(self).Name;
  lName := lName.Substring(lName.LastIndexOf('.') + 1).ToUpper;

  var lStream := new TFileStream('c:\users\public\ok.res', fmOpenRead);
  writeLn('File read!');
  lStream.Position := 74;
  var lReader := new TReader(lStream, 100);
  writeLn('ReadRootComponent');
  lReader.ReadRootComponent(self);


  // TODO ! ! ! load resource in .net
  {var lStream := new TResourceStream(0, lName, rtl.PCHAR(^void(rtl.RT_RCDATA)));
  lStream.Position := 0;
  var lReader := new TReader(lStream, 100);
  lReader.ReadRootComponent(self);}
end;

method TForm.CreateHandle;
begin
  fHandle := new System.Windows.Window();
end;

method TForm.Show;
begin
  (fHandle as System.Windows.Window).Show;
end;




{$ENDIF}

end.