namespace DelphiRTL.Tests;

interface

uses
  java.util,
  android.app,
  android.content,
  android.os,
  android.util,
  android.view,
  android.widget,
  remobjects.elements.eunit;

type
  MainActivity = public class(Activity)
  public
    method onCreate(savedInstanceState: Bundle); override;
  end;

implementation

method MainActivity.onCreate(savedInstanceState: Bundle);
begin
  inherited;
  ContentView := R.layout.main;

  var lTests := Discovery.DiscoverTests(self);
  Runner.RunTests(lTests) withListener(Runner.DefaultListener);
end;

end.
