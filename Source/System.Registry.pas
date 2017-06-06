namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.System;

type
  TRegistry = public class(TObject)
  private
    fRootKey: rtl.HKEY;
    fAccess: LongWord;
    fCurrentKey: rtl.HKEY;
    fLastError: Int32;

    method IsRelative(Key: DelphiString): Boolean;  
    method GetLastErrorMsg: DelphiString;
    method SetRootKey(Value: rtl.HKEY);
  protected
    //method ChangeKey(Value: HKey; const Path: string);
    method CheckResult(RetVal: Longint): Boolean;
    method GetBaseKey(Relative: Boolean): rtl.HKey;
    /*method GetData(const Name: string; Buffer: Pointer;
      BufSize: Integer; var RegData: TRegDataType): Integer;
    method GetKey(const Key: string): HKEY;*/
    method GetRootKeyName: DelphiString;
    /*method PutData(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType);
    method SetCurrentKey(Value: HKEY);*/
  public
    constructor;
    constructor(aAccess: LongWord);
    //destructor Destroy; override;
    method CloseKey;
    method CreateKey(Key: DelphiString): Boolean;
    /*method DeleteKey(const Key: string): Boolean;
    method DeleteValue(const Name: string): Boolean;
    method GetDataAsString(const ValueName: string; PrefixType: Boolean = false): string;
    method GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    method GetDataSize(const ValueName: string): Integer;
    method GetDataType(const ValueName: string): TRegDataType;
    method GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    method GetKeyNames(Strings: TStrings);
    method GetValueNames(Strings: TStrings);
    method HasSubKeys: Boolean;
    method KeyExists(const Key: string): Boolean;
    method LoadKey(const Key, FileName: string): Boolean;
    method MoveKey(const OldName, NewName: string; Delete: Boolean);
    */
    method OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    /*method OpenKeyReadOnly(const Key: String): Boolean;
    method ReadCurrency(const Name: string): Currency;
    method ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    method ReadBool(const Name: string): Boolean;
    method ReadDate(const Name: string): TDateTime;
    method ReadDateTime(const Name: string): TDateTime;
    method ReadFloat(const Name: string): Double;
    method ReadInteger(const Name: string): Integer;
    method ReadString(const Name: string): string;
    method ReadTime(const Name: string): TDateTime;
    method RegistryConnect(const UNCName: string): Boolean;
    method RenameValue(const OldName, NewName: string);
    method ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
    method RestoreKey(const Key, FileName: string): Boolean;
    method SaveKey(const Key, FileName: string): Boolean;
    method UnLoadKey(const Key: string): Boolean;
    method ValueExists(const Name: string): Boolean;
    method WriteCurrency(const Name: string; Value: Currency);
    method WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    method WriteBool(const Name: string; Value: Boolean);
    method WriteDate(const Name: string; Value: TDateTime);
    method WriteDateTime(const Name: string; Value: TDateTime);
    method WriteFloat(const Name: string; Value: Double);
    method WriteInteger(const Name: string; Value: Integer);
    method WriteString(const Name, Value: string);
    method WriteExpandString(const Name, Value: string);
    method WriteTime(const Name: string; Value: TDateTime);*/
    property CurrentKey: rtl.HKEY read fCurrentKey;
    /*property CurrentPath: string read FCurrentPath;
    property LazyWrite: Boolean read FLazyWrite write FLazyWrite;
*/
    property LastError: Longint read fLastError;
    property LastErrorMsg: DelphiString read GetLastErrorMsg;
    property RootKey: rtl.HKEY read FRootKey write SetRootKey;
    property RootKeyName: DelphiString read GetRootKeyName;
    property Access: LongWord read fAccess write fAccess;
  end;

implementation

constructor TRegistry;
begin
  fRootKey := rtl.HKEY_CURRENT_USER;
  fAccess := rtl.KEY_ALL_ACCESS;
end;

constructor TRegistry(aAccess: LongWord);
begin
  fRootKey := rtl.HKEY_CURRENT_USER;
  fAccess := rtl.KEY_ALL_ACCESS;
end;

method TRegistry.CheckResult(RetVal: Longint): Boolean;
begin
  fLastError := RetVal;
  result := RetVal = rtl.ERROR_SUCCESS;
end;

method TRegistry.GetBaseKey(Relative: Boolean): rtl.HKey;
begin
  result := if Relative or (fCurrentKey^.unused <> 0) then fCurrentKey else fRootKey;
end;

method TRegistry.IsRelative(Key: DelphiString): Boolean;  
begin
  result := (Key.Length > 0) and (Key.Chars[0] <> '\');
end;

method TRegistry.GetRootKeyName: DelphiString;
begin

end;

method TRegistry.GetLastErrorMsg: DelphiString;
begin

end;

method TRegistry.SetRootKey(Value: rtl.HKEY);
begin

end;

method TRegistry.CloseKey;
begin
  if fCurrentKey^.unused <> 0 then begin
    rtl.RegCloseKey(fCurrentKey);
    fCurrentKey^.unused := 0;
  end;    
end;

method TRegistry.CreateKey(Key: DelphiString): Boolean;
begin
  var lNewKey: rtl.HKEY;
  var lDisposition: rtl.DWORD;
  var lRelative := IsRelative(Key);
  var lKey := Key;
  if lRelative then
    lKey := Key.SubString(1);
    
  result := CheckResult(rtl.RegCreateKeyEx(GetBaseKey(lRelative), String(lKey).FirstChar, 0, nil, rtl.REG_OPTION_NON_VOLATILE, 
    rtl.KEY_ALL_ACCESS or rtl.KEY_WOW64_RES, nil, @lNewKey, @lDisposition));
  
  if not result then 
    raise new Exception('Error while creating new registry key: ' + Key)
  else
    if lDisposition = rtl.REG_OPENED_EXISTING_KEY then
      rtl.RegCloseKey(lNewKey);
end;

method TRegistry.OpenKey(const Key: string; CanCreate: Boolean): Boolean;
begin
  CloseKey;
  var lNewKey: rtl.HKEY;
  var lRelative := IsRelative(Key);
  var lKey := Key;
  var lDisposition: rtl.DWORD;
  if lRelative then
    lKey := Key.SubString(1);

  if CanCreate then
    result := CheckResult(rtl.RegCreateKeyEx(GetBaseKey(lRelative), String(lKey).FirstChar, 0, nil, rtl.REG_OPTION_NON_VOLATILE, 
      rtl.KEY_ALL_ACCESS or rtl.KEY_WOW64_RES, nil, @lNewKey, @lDisposition))
  else
    result := CheckResult(rtl.RegOpenKeyEx(GetBaseKey(IsRelative(Key)), lKey.FirstChar, 0, fAccess or rtl.KEY_WOW64_RES, @lNewKey));
end;

{$ENDIF}

end.
