namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
TComponentClass = public &Type;

TCustomForm = public partial class({$IFDEF WINDOWS}TScrollingWinControl{$ELSE}TControl{$ENDIF})
public
  //constructor(aOwner: TComponent); empty;
end;

TForm = public partial class(TCustomForm)
public
  //constructor(aOwner: TComponent); empty;
end;

TApplication = public partial class(TComponent)
private
  fFinished: Boolean;
  fMainForm: TForm;
  class constructor;
public
  constructor(aOwner: TComponent);
  class method Create(aOwner: TComponent): TApplication;
  method CreateForm(InstanceClass: TComponentClass; var FormRef: TComponent); partial; empty;
  method Initialize; partial; empty;
  method Run; partial; empty;
  method Terminate; partial; empty;
  property Finished: Boolean read fFinished;
  property MainForm: TForm read fMainForm;
end;

var
  Application: TApplication;

implementation

class constructor TApplication;
begin
  Application := new TApplication(nil);
end;

constructor TApplication(aOwner: TComponent);
begin

end;

class method TApplication.Create(aOwner: TComponent): TApplication;
begin
  result := new TApplication(aOwner);
end;

end.