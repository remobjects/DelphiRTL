namespace DelphiRTL.Tests.Shared.Test_Cases;

{$IF ISLAND AND WINDOWS}

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  SystemRegistryUsage = public class(Test)
  private
    fReg: TRegistry;
  public
    method Setup; override;
    begin
      fReg := new TRegistry();
    end;

    method OpenKeyTests;
    begin
      if fReg.OpenKey('\Software\Microsoft\Windows\CurrentVersion', false) then begin

      end;
    end;
  end;

{$ENDIF}

end.