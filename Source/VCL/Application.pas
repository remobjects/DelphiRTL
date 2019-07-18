namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS) AND NOT DARWIN) OR ECHOESWPF OR (MACOS AND NOT (ISLAND AND DARWIN))}

interface

type
 TApplication = public partial class(TComponent)
private
  fFinished: Boolean;
  fMainForm: TForm := nil;
  class constructor;
public
  constructor(aOwner: TComponent);
  class method Create(aOwner: TComponent): TApplication;
  method CreateForm(InstanceClass: TComponentClass; var aFormRef); partial; empty;
  method Initialize; partial; empty;
  method Run; partial; empty;
  method Terminate; partial; empty;
  property Finished: Boolean read fFinished;
  property MainForm: TForm read fMainForm write fMainForm;
end;

TScreen = public partial class(TComponent)
private
  fPixelsPerInch: Integer;
published
  property PixelsPerInch: Integer read fPixelsPerInch write fPixelsPerInch;
end;

var
  Application: TApplication := nil;
  Screen: TScreen := nil;

implementation

class constructor TApplication;
begin
  //Application := new TApplication(nil);
end;

constructor TApplication(aOwner: TComponent);
begin

end;

class method TApplication.Create(aOwner: TComponent): TApplication;
begin
  result := new TApplication(aOwner);
end;

{$ENDIF}

end.