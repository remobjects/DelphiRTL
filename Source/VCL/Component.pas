namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF NOT TOFFEE}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TComponentStateEnum = public enum(csLoading, csReading, csWriting, csDestroying, csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification) of integer;
  TComponentState = set of TComponentStateEnum;

  {$IF COOPER}
  TComponentClass = public &Class;
  {$ELSE}
  TComponentClass = public &Type;
  {$ENDIF}

  TComponent = public class(TObject)
  private
    fName: String;
    fOwner: TComponent;
    fComponentState: TComponentState;
    method setName(aValue: String);
    method setOwner(aValue: TComponent);
  protected
    method Loaded; virtual;
    constructor(aOwner: TComponent);
  public
    method SetComponentState(aState: TComponentStateEnum);
    method RemoveComponentState(aState: TComponentStateEnum);
    property ComponentState: TComponentState read fComponentState;
    property Name: String read fName write setName;
    property Owner: TComponent read fOwner write setOwner;
  end;

  TShiftStateValues = public enum(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssTouch, ssPen, ssCommand, ssHorizontal) of Integer;
  TShiftState = public set of TShiftStateValues;

  TKeyPressEvent = public block(Sender: TObject; var Key: Char);
  TKeyEvent = public block(Sender: TObject; var Key: Word; Shift: TShiftState);

  TPlatformHandle = {$IF WEBASSEMBLY} dynamic {$ELSEIF ISLAND AND WINDOWS} rtl.HWND {$ELSE} Object {$ENDIF};
  TPropertyChangedEvent = public block(Sender: TObject; PropName: String);

  INotifyPropertyChanged = public interface
    {$IF NOT COOPER}
    event PropertyChanged: Action<TObject, String>;
    {$ENDIF}
  end;

  TColor = Integer;

implementation

method TComponent.setName(aValue: String);
begin
  fName := aValue;
end;

constructor TComponent(aOwner: TComponent);
begin
  fOwner := aOwner;
end;

method TComponent.setOwner(aValue: TComponent);
begin
  fOwner := aValue;
end;

method TComponent.Loaded;
begin
  // Nothing
end;

method TComponent.SetComponentState(aState: TComponentStateEnum);
begin
  fComponentState := fComponentState + [aState];
end;

method TComponent.RemoveComponentState(aState: TComponentStateEnum);
begin
  fComponentState := fComponentState - [aState];
end;

{$ENDIF}

end.