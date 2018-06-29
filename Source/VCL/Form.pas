namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF NOT TOFFEE}

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TCustomForm = public partial class({$IFDEF WINDOWS}TScrollingWinControl{$ELSE}TNativeControl{$ENDIF})
  private
    fPixelsPerInch: Integer;
    fOldCreateOrder: Boolean;
    fTextHeight: Integer;
  public
    //constructor(aOwner: TComponent); empty;
    property OldCreateOrder: Boolean read fOldCreateOrder write fOldCreateOrder;
    property PixelsPerInch: Integer read fPixelsPerInch write fPixelsPerInch;
    property TextHeight: Integer read fTextHeight write fTextHeight;
  end;

  TForm = public partial class(TCustomForm)
  public
    //constructor(aOwner: TComponent); empty;
  end;

implementation

{$ENDIF}

end.