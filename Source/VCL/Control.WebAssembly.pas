namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF WEBASSEMBLY}

interface

uses
  RemObjects.Elements.RTL.Delphi;

{$GLOBALS ON}

type
  TControl = public partial class(TComponent)
  private
    method ProcessKeyboardStatus(aStatus: EcmaScriptObject; var aKey: Word): TShiftState;
    method InternalSetKeyboardEvent(aEvent: String; aValue: TKeyEvent);
  protected
    method HandleAllocated: Boolean; virtual; partial;
    method PlatformSetWidth(aValue: Integer); virtual; partial;
    method PlatformSetHeight(aValue: Integer); partial;
    method PlatformSetTop(aValue: Integer); virtual; partial;
    method PlatformSetLeft(aValue: Integer); virtual; partial;
    method PlatformSetParent(aValue: TControl); virtual; partial;
    method PlatformSetOnClick(aValue: TNotifyEvent); partial;
    method PlatformSetOnKeyPress(aValue: TKeyPressEvent); partial;
    method PlatformSetOnKeyDown(aValue: TKeyEvent); partial;
    method PlatformSetOnKeyUp(aValue: TKeyEvent); partial;
    method PlatformSetCaption(aValue: String); virtual; partial;
    method PlatformSetTabOrder(aValue: Integer); virtual; partial;
    method PlatformSetColor(aValue: TColor); virtual; partial;


    method PlatformFontChanged; virtual; partial;

    method PlatformGetDefaultName: String; virtual; partial;
    method PlatformApplyDefaults; virtual; partial;
  end;

  TCustomControl = public partial class(TNativeControl)
  protected
    method CreateHandle; override;
  end;

  TGraphicControl = public partial class(TControl)
  protected
    method CreateHandle; override;
  end;

implementation

method TControl.HandleAllocated: Boolean;
begin
  result := fHandle ≠ nil;
end;

method TControl.PlatformSetWidth(aValue: Integer);
begin
  fHandle.style.width := aValue.ToString + 'px';
end;

method TControl.PlatformSetHeight(aValue: Integer);
begin
  fHandle.style.height := aValue.ToString + 'px';
end;

method TControl.PlatformSetTop(aValue: Integer);
begin
  fHandle.style.top := aValue.ToString + 'px';
end;

method TControl.PlatformSetLeft(aValue: Integer);
begin
  fHandle.style.left := aValue.ToString + 'px';
end;

method TControl.PlatformSetParent(aValue: TControl);
begin
  aValue.Handle.appendChild(fHandle);
end;

method TControl.PlatformSetOnClick(aValue: TNotifyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> aValue(self));
  fHandle.addEventListener("click", lDelegate);
end;

method TControl.PlatformGetDefaultName: String;
begin
  var i := 1;
  //var lObject: dynamic;
  repeat
    result := InstanceClassName;
    result := result.Substring(result.LastIndexOf('.') + 2) + i.ToString; // + 2 to remove initial 'T'...
    inc(i);
    //lObject := Browser.GetElementById(result);
  //until lObject = nil;
  until Browser.GetElementById(result) = nil;
end;

method TControl.PlatformApplyDefaults;
begin
  HandleNeeded;
  fHandle.setAttribute('id', Name);
end;

method TControl.PlatformFontChanged;
begin
  fHandle.style.fontFamily := fFont.Name;
  fHandle.style.fontSize := fFont.Size + 'px';
  if (TFontStyle.Italic in fFont.Style) then fHandle.style.fontStyle := 'italic' else fHandle.style.fontStyle := 'normal';
  if (TFontStyle.Bold in fFont.Style) then fHandle.style.fontWeight := 'bold' else fHandle.style.fontWeight := 'normal';
  if (TFontStyle.StrikeOut in fFont.Style) or (TFontStyle.Underline in fFont.Style) then begin
    if (TFontStyle.StrikeOut in fFont.Style) then fHandle.style.textDecoration := 'line-throught';
    if (TFontStyle.Underline in fFont.Style) then fHandle.style.textDecoration := 'underline';
  end
  else
    fHandle.style.textDecoration := 'none';
end;

/*
method TControl.PlatformFontSetColor(value: TColor);
begin
  fHandle.style.color := value.ToString;
  fHandle.style.textDecorationColor := value.ToString;
end;*/

method TControl.PlatformSetOnKeyPress(aValue: TKeyPressEvent);
begin
  //'which' in case of using Mozilla FireFox browser
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lObject := new EcmaScriptObject(WebAssemblyCalls.GetArray(a.Handle, 0)); var lKey := chr(Max(Double(lObject['keyCode']), Double(lObject['which']))); aValue(self, var lKey); end);
  fHandle.addEventListener("keypress", lDelegate);
end;

method TControl.InternalSetKeyboardEvent(aEvent: String; aValue: TKeyEvent);
begin
  var lDelegate := new WebAssemblyDelegate((a) -> begin var lObject := new EcmaScriptObject(WebAssemblyCalls.GetArray(a.Handle, 0)); var lKey: Word; var lShiftState := ProcessKeyboardStatus(lObject, var lKey); aValue(self, var lKey, lShiftState); end);
  fHandle.addEventListener(aEvent, lDelegate);
end;

method TControl.PlatformSetOnKeyDown(aValue: TKeyEvent);
begin
  InternalSetKeyboardEvent("keydown", aValue);
end;

method TControl.PlatformSetOnKeyUp(aValue: TKeyEvent);
begin
  InternalSetKeyboardEvent("keyup", aValue);
end;

method TControl.PlatformSetCaption(aValue: String);
begin
  fHandle.innerText := aValue;
end;

method TControl.PlatformSetTabOrder(aValue: Integer);
begin
  fHandle.tabindex := aValue;
end;

method TControl.PlatformSetColor(aValue: TColor);
begin
  fhandle.style['background-color'] := '#' + RemObjects.Elements.RTL.Convert.ToHexString(aValue);
end;

method TControl.ProcessKeyboardStatus(aStatus: EcmaScriptObject; var aKey: Word): TShiftState;
begin
  result := [];
  if Boolean(aStatus['altKey']) then result := result + [TShiftState.ssAlt];
  if Boolean(aStatus['ctrlKey']) then result := result + [TShiftState.ssCtrl];
  if Boolean(aStatus['shiftKey']) then result := result + [TShiftState.ssShift];
  if Boolean(aStatus['metaKey']) then result := result + [TShiftState.ssCommand];
  aKey := Integer((Max(Double(aStatus['keyCode']), Double(aStatus['which']))));
end;

method TCustomControl.CreateHandle;
begin
  fHandle := Browser.CreateElement('div');
  fHandle.style.position := "absolute";
end;

method TGraphicControl.CreateHandle;
begin
  fHandle := Browser.CreateElement('div');
  fHandle.style.position := "absolute";
end;

{$ENDIF}
end.