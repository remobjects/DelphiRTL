namespace Elements.RTL.Delphi;

interface

type
TTests = public class
public
  method Test;
end;

implementation

method TTests.Test;
begin
  var vamos := new DelphiString;
  vamos := 'asdad';
  vamos := 'ased2';
  //vamos[0] := 's';
  vamos[1] := 's';
end;

end.
