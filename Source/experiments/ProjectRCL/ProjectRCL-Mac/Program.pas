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
  {var lButton := new TControl;
  //lButton.OnClick := begin

  var lCasiType := typeOf(TControl);
  var lType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lCasiType);
  var lMethods := lType.Methods;
  for each lProp in lProps do begin
    writeLn(lProp.Name);
    var lStringType := property_getAttributes(lProp.PropertyClass);
    writeLn(lStringType);
    writeLn('------------');

  end;}

  //var lCount := objc_getClassList(nil, 0);
  //var lList := new List<&RemObjects.Elements.RTL.Reflection.Type> withCapacity(lCount);

  //var lClasses := new unretained &Class[lCount];
  //lCount := objc_getClassList(lClasses, lCount);
{  var lCount: UInt32;
  var lClasses := objc_copyClassList(@lCount);


  writeLn(lCount);
  writeLn('Come on');
  for i: Integer := 0 to lCount-1 do begin
    writeLn('Come on 1');
    var lClass: &Class := lClasses[i];
    writeLn('Come on 2');
    //var lType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lClass);
    writeLn('Come on 3');
    //lList.Add(lType);
    writeLn('Come on 4');
  end;}

  //writeLn(lList.Count);

  Application := new TApplication(nil);
  Application.Initialize;
  Application.CreateForm(typeOf(TForm6), var Form6);
  Application.Run;
  writeLn('The magic happens here.');
end;

end.