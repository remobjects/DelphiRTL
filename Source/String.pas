namespace RTL2.Delphi;

type
  PlatformString = {$IF ECHOES}System.String{$ELSEIF TOFFEE}Foundation.NSString{$ELSEIF COOPER}java.lang.String{$ELSEIF ISLAND}RemObjects.Elements.System.String{$ENDIF};

  String = public class mapped to PlatformString
  public
  
  end;

end.
