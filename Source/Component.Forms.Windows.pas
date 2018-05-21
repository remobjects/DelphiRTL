namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TApplication = public partial class(TComponent)
  public
    method CreateForm(InstanceClass: TComponentClass; var FormRef: TComponent); partial;
    method Initialize; partial;
    method Run; partial;
    method Terminate; partial;
  end;

  TCustomForm = public partial class(TScrollingWinControl)
  protected
    class method WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
  public
    constructor(aOwner: TComponent);
    method CreateWnd; override;
  end;

  ComponentCtorHelper = public procedure(aInst: Object; aOwner: TComponent);

implementation

method TApplication.CreateForm(InstanceClass: TComponentClass; var FormRef: TComponent);
begin
  var lCtor: MethodInfo;
  //var FormRef: TForm := TForm(aForm);
  writeLn(InstanceClass.name);

  var lCtors := InstanceClass.Methods.Where(a -> ((MethodFlags.Constructor in a.Flags) and (a.Arguments.Count > 0)));
  if lCtors.Count > 1 then begin
    for each lTemp in lCtors do begin
      var lArguments := lTemp.Arguments.ToList;
      writeLn('types:');
      writeLn(lArguments[0].&Type.Name);
      if lArguments[0].Type = typeOf(TComponent) then begin
        lCtor := lTemp;
        writeLn('Found!');
        break;
      end;
    end;
  end
  else begin
    lCtor := lCtors.FirstOrDefault;
    WriteLn('Not found..');
    var lX := lCtor.Arguments.ToList;
    WriteLn(lX[0].Type.Name);
  end;

  if lCtor = nil then raise new Exception('No default constructor could be found!');
  var lRealCtor := ComponentCtorHelper(lCtor.Pointer);
  if lRealCtor = nil then raise new Exception('No default constructor could be found!');
  var lNew := DefaultGC.New(InstanceClass.RTTI, InstanceClass.SizeOfType);
  FormRef := InternalCalls.Cast<TComponent>(lNew);
  WriteLn(FormRef.Name);
  //lRealCtor(FormRef, nil);

  lCtor.Invoke(FormRef, [nil]);
  WriteLn('Vamos 4');
end;

method TApplication.Initialize;
begin

end;

method TApplication.Run;
begin
  var lMsg: rtl.MSG;

  while not Finished do begin
    if rtl.PeekMessageW(@lMsg, nil, 0, 0, rtl.PM_REMOVE) then begin
      rtl.TranslateMessage(@lMsg);
      rtl.DispatchMessage(@lMsg);
    end;

    if lMsg.message = rtl.WM_QUIT then
      Terminate;
  end;
end;

method TApplication.Terminate;
begin
  fFinished := true;
end;

constructor TCustomForm(aOwner: TComponent);
begin
  var lStream := new TFileStream('c:\dev\ro\dam.res', fmOpenRead);
  lStream.Position := 76;
  var lReader := new TReader(lStream, 100);
  lReader.ReadRootComponent(self);
end;

method TCustomForm.CreateWnd;
begin
  var hWnd: rtl.HWND;
  // this struct holds information for the window class
  var wc: rtl.WNDCLASSEX;
  var hInstance := rtl.GetModuleHandle(nil);

  // clear out the window class for use
  memset(@wc, 0, sizeOf(rtl.WNDCLASSEX));

  // fill in the struct with the needed information
  wc.cbSize := sizeOf(rtl.WNDCLASSEX);
  wc.style := rtl.CS_HREDRAW or rtl.CS_VREDRAW;
  wc.lpfnWndProc := @WndProc;
  wc.hInstance := hInstance;

  //wc.hCursor = rtl.LoadCursor(nil, rtl.IDC_ARROW.);
  wc.hbrBackground := rtl.HBRUSH(rtl.COLOR_WINDOW);
  var lString: RemObjects.Elements.System.String := 'WindowClass1';
  var lArray := lString.ToCharArray(true);
  wc.lpszClassName := @lArray[0];

  var lTitle: RemObjects.Elements.System.String := 'Good Stuff!!!';
  var lTitleArray := lTitle.ToCharArray(true);

  // register the window class
  rtl.RegisterClassEx(@wc);

  // create the window and use the result as the handle
  hWnd := rtl.CreateWindowEx(0,
  @lArray[0],    // name of the window class
  @lTitleArray[0],   // title of the window
  rtl.WS_OVERLAPPEDWINDOW,    // window style
  300,    // x-position of the window
  300,    // y-position of the window
  500,    // width of the window
  400,    // height of the window
  nil,    // we have no parent window, NULL
  nil,    // we aren't using menus, NULL
  hInstance,    // application handle
  nil);    // used with multiple windows, NULL

  // display the window on the screen
  rtl.ShowWindow(hWnd, rtl.SW_SHOW);
end;

class method TCustomForm.WndProc(hWnd: rtl.HWND; message: rtl.UINT; wParam: rtl.WPARAM; lParam: rtl.LPARAM): rtl.LRESULT;
begin
  result := rtl.DefWindowProc(hWnd, message, wParam, lParam);
end;


end.