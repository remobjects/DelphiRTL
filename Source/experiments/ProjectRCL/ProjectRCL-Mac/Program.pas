namespace ProjectRCLMac;

interface

uses
  Foundation, RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type
  Program = public static class
  public
    method Main(aArguments: array of String): Int32;
  end;

implementation

method Program.Main(aArguments: array of String): Int32;
begin
  var lCasiType := typeOf(TControl);
  var lType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lCasiType);
  var lProps := lType.Properties;
  for each lProp in lProps do begin
    writeLn(lProp.Name);
    var lStringType := property_getAttributes(lProp.PropertyClass);
    writeLn(lStringType);
    writeLn('------------');

  end;
  Application := new TApplication(nil);
  Application.Initialize;
  Application.CreateForm(typeOf(TForm6), var Form6);
  Application.Run;
  writeLn('The magic happens here.');
end;

end.