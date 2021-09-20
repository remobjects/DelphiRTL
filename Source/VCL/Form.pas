namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS OR (LINUX AND NOT ANDROID)) AND NOT DARWIN) OR ECHOESWPF OR (MACOS AND NOT (ISLAND AND DARWIN))}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class({$IFDEF WINDOWS}TScrollingWinControl{$ELSE}TNativeControl{$ENDIF})
  private
    fPixelsPerInch: Integer;
    fOldCreateOrder: Boolean;
    fTextHeight: Integer;
    method Dummy; empty;
  published
  //public
    //constructor(aOwner: TComponent); empty;
    property OldCreateOrder: Boolean read fOldCreateOrder write fOldCreateOrder;
    property PixelsPerInch: Integer read fPixelsPerInch write fPixelsPerInch;
    property TextHeight: Integer read fTextHeight write fTextHeight;
  end;

  TForm = public partial class(TCustomForm)
  published
    constructor(aOwner: TComponent);
  end;

  TScreen = public partial class(TComponent)
  protected
    method PlatformGetScreenHeight: Integer; virtual; partial; empty;
    method PlatformGetScreenWidth: Integer; virtual; partial; empty;
  published
    property Height: Integer read PlatformGetScreenHeight;
    property Width: Integer read PlatformGetScreenWidth;
  end;

implementation

constructor TForm(aOwner: TComponent);
begin
  if Application.MainForm = nil then
    Application.MainForm := self;
end;

{$ENDIF}

end.