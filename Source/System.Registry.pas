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
    fCurrentPath: DelphiString;

    method IsRelative(Key: DelphiString): Boolean;
    method GetLastErrorMsg: DelphiString;
    method SetRootKey(Value: rtl.HKEY);
  protected
    method ChangeKey(Value: rtl.HKey; Path: DelphiString);
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
    class method Create: TRegistry; static;
    class method Create(aAccess: LongWord): TRegistry; static;
    //destructor Destroy; override;
    method CloseKey;
    method CreateKey(Key: DelphiString): Boolean;
    method DeleteKey(Key: DelphiString): Boolean;
    method DeleteValue(Name: DelphiString): Boolean;
    /*method GetDataAsString(const ValueName: string; PrefixType: Boolean = false): string;
    method GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    method GetDataSize(const ValueName: string): Integer;
    method GetDataType(const ValueName: string): TRegDataType;
    method GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    method GetKeyNames(Strings: TStrings);
    method GetValueNames(Strings: TStrings);*/
    method HasSubKeys: Boolean;
    method KeyExists(Key: DelphiString): Boolean;
    /*method LoadKey(const Key, FileName: string): Boolean;
    method MoveKey(const OldName, NewName: string; Delete: Boolean);
    */
    method OpenKey(const Key: DelphiString; CanCreate: Boolean): Boolean;
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
    method UnLoadKey(const Key: string): Boolean;*/
    method ValueExists(Name: DelphiString): Boolean;
    /*method WriteCurrency(const Name: string; Value: Currency);
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
    property CurrentPath: DelphiString read fCurrentPath;
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

class method TRegistry.Create: TRegistry;
begin
  result := new TRegistry();
end;

class method TRegistry.Create(aAccess: LongWord): TRegistry;
begin
  result := new TRegistry(aAccess);
end;

method TRegistry.ChangeKey(Value: rtl.HKey; Path: DelphiString);
begin
  fCurrentPath := Path;
  fCurrentKey := Value;
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
  result := case fRootKey of
    rtl.HKEY_CLASSES_ROOT: 'HKEY_CLASSES_ROOT';
    rtl.HKEY_CURRENT_USER: 'HKEY_CURRENT_USER';
    rtl.HKEY_LOCAL_MACHINE: 'HKEY_LOCAL_MACHINE';
    rtl.HKEY_USERS: 'HKEY_USERS';
    rtl.HKEY_CURRENT_CONFIG: 'HKEY_CURRENT_CONFIG';
    rtl.HKEY_PERFORMANCE_DATA: 'HKEY_PERFORMANCE_DATA';
    rtl.HKEY_DYN_DATA: 'HKEY_PERFORMANCE_DATA';
    else '';
  end;
end;

method TRegistry.GetLastErrorMsg: DelphiString;
begin
end;

method TRegistry.SetRootKey(Value: rtl.HKEY);
begin
  fRootKey := Value;
  CloseKey;
end;

method TRegistry.CloseKey;
begin
  if fCurrentKey^.unused <> 0 then begin
    rtl.RegFlushKey(fCurrentKey);
    rtl.RegCloseKey(fCurrentKey);
    fCurrentKey^.unused := 0;
    fCurrentPath := '';
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

method TRegistry.DeleteKey(Key: DelphiString): Boolean;
begin
  var lRelative := IsRelative(Key);
  var lKey := Key;
  if lRelative then
    lKey := Key.SubString(1);

  result := CheckResult(rtl.RegDeleteKeyEx(GetBaseKey(lRelative), String(lKey).FirstChar, rtl.KEY_ALL_ACCESS or rtl.KEY_WOW64_RES, 0));
end;

method TRegistry.DeleteValue(Name: DelphiString): Boolean;
begin
  result := CheckResult(rtl.RegDeleteValue(CurrentKey, String(Name).FirstChar));
end;

method TRegistry.HasSubKeys: Boolean;
begin
  var lSubkeys: rtl.DWORD;
  if not CheckResult(rtl.RegQueryInfoKey(CurrentKey, nil, nil, nil, @lSubKeys, nil, nil, nil, nil, nil, nil, nil)) then
    raise new Exception('Error getting info of registry key: ' + CurrentPath);
  result := lSubkeys > 0;
end;

method TRegistry.KeyExists(Key: DelphiString): Boolean;
begin
  var lNewKey: rtl.HKEY;
  var lRelative := IsRelative(Key);
  var lKey := Key;
  if lRelative then
    lKey := Key.SubString(1);

    result := CheckResult(rtl.RegOpenKeyEx(GetBaseKey(lRelative), String(lKey).FirstChar, 0, fAccess or rtl.KEY_WOW64_RES, @lNewKey));
end;

method TRegistry.OpenKey(const Key: DelphiString; CanCreate: Boolean): Boolean;
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
    result := CheckResult(rtl.RegOpenKeyEx(GetBaseKey(lRelative), String(lKey).FirstChar, 0, fAccess or rtl.KEY_WOW64_RES, @lNewKey));

  if result then begin
    var lCurrentPath: DelphiString;
    if lRelative then
      lCurrentPath := fCurrentPath + '\' + Key
    else
      lCurrentPath := RootKeyName + Key;
    ChangeKey(lNewKey, lCurrentPath)
  end;
end;

method TRegistry.ValueExists(Name: DelphiString): Boolean;
begin
  var lType: rtl.DWORD;
  result := CheckResult(rtl.RegQueryValueEx(CurrentKey, String(Name).FirstChar, nil, @lType, nil, nil));
end;

{$ENDIF}

end.