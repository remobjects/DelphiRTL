namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

uses
  RemObjects.Elements.System;

type
  TRegKeyInfo = public record
    NumSubKeys: rtl.DWORD;
    MaxSubKeyLen: rtl.DWORD;
    NumValues: rtl.DWORD;
    MaxValueLen: rtl.DWORD;
    MaxDataLen: rtl.DWORD;
    FileTime: rtl.FILETIME;
  end;

  TRegDataType = public (Unknown, String, ExpandString, Integer, Binary) of rtl.DWORD;

  TRegDataInfo = public record
    RegData: TRegDataType;
    DataSize: rtl.DWORD;
  end;

  TRegistry = public class(TObject)
  private
    fRootKey: rtl.HKEY;
    fAccess: LongWord;
    fCurrentKey: rtl.HKEY;
    fLastError: Int32;
    fCurrentPath: DelphiString;

    method IsRelative(Key: DelphiString): Boolean;
    method SetRootKey(Value: rtl.HKEY);
    method ConvertDataType(Value: rtl.DWORD): TRegDataType;
    method ConvertToDataType(Value: TRegDataType): rtl.DWORD;
    method GetProperKey(Key: DelphiString; var Normalized: DelphiString): rtl.HKEY;
  protected
    method ChangeKey(Value: rtl.HKey; Path: DelphiString);
    method CheckResult(RetVal: Longint): Boolean;
    method GetBaseKey(Relative: Boolean): rtl.HKey;
    method GetData(Name: DelphiString; Buffer: Pointer; BufSize: rtl.DWORD; var RegData: TRegDataType): Integer;
    method GetKey(Key: DelphiString): rtl.HKEY;
    method GetRootKeyName: DelphiString;
    method PutData(Name: DelphiString; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType);
  public
    constructor;
    constructor(aAccess: LongWord);
    class method Create: TRegistry; static;
    class method Create(aAccess: LongWord): TRegistry; static;
    method CloseKey;
    method CreateKey(Key: DelphiString): Boolean;
    method DeleteKey(Key: DelphiString): Boolean;
    method DeleteValue(Name: DelphiString): Boolean;
    method GetDataInfo(ValueName: DelphiString; var Value: TRegDataInfo): Boolean;
    method GetDataSize(ValueName: DelphiString): Integer;
    method GetDataType(ValueName: DelphiString): TRegDataType;
    method GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    method GetKeyNames(Strings: TStrings);
    method GetValueNames(Strings: TStrings);
    method HasSubKeys: Boolean;
    method KeyExists(Key: DelphiString): Boolean;
    method LoadKey(Key, FileName: DelphiString): Boolean;
    method OpenKey(Key: DelphiString; CanCreate: Boolean): Boolean;
    method OpenKeyReadOnly(Key: DelphiString): Boolean;

    method ReadBinaryData(Name: DelphiString; var Buffer: Pointer; BufSize: Integer): Integer;
    method ReadBool(Name: DelphiString): Boolean;
    method ReadDate(Name: DelphiString): TDateTime;
    method ReadDateTime(Name: DelphiString): TDateTime;
    method ReadFloat(Name: DelphiString): Double;
    method ReadInteger(Name: DelphiString): Integer;
    method ReadString(Name: DelphiString): DelphiString;
    method ReadTime(Name: DelphiString): TDateTime;
    method RegistryConnect(UNCName: DelphiString): Boolean;
    method ReplaceKey(Key, FileName, BackUpFileName: DelphiString): Boolean;
    method RestoreKey(Key, FileName: DelphiString): Boolean;
    method SaveKey(Key, FileName: DelphiString): Boolean;
    method UnLoadKey(Key: DelphiString): Boolean;
    method ValueExists(Name: DelphiString): Boolean;
    
    method WriteBinaryData(Name: DelphiString; Buffer: Pointer; BufSize: Integer);
    method WriteBool(Name: DelphiString; Value: Boolean);
    method WriteDate(Name: DelphiString; Value: TDateTime);
    method WriteDateTime(Name: DelphiString; Value: TDateTime);
    method WriteFloat(Name: DelphiString; Value: Double);
    method WriteInteger(Name: DelphiString; Value: Integer);
    method WriteString(Name, Value: DelphiString);
    method WriteExpandString(Name, Value: DelphiString);
    method WriteTime(Name: DelphiString; Value: TDateTime);
    property CurrentKey: rtl.HKEY read fCurrentKey;
    property CurrentPath: DelphiString read fCurrentPath;
    property LastError: Longint read fLastError;
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
  result := if (not Relative) or (fCurrentKey = nil) then fRootKey else fCurrentKey;
end;

method TRegistry.IsRelative(Key: DelphiString): Boolean;
begin
  result := (Key.Length > 0) and (Key.Chars[0] <> '\');
end;

method TRegistry.GetData(Name: DelphiString; Buffer: Pointer; BufSize: rtl.DWORD; var RegData: TRegDataType): Integer;
begin
  var lType: rtl.DWORD;
  var lKeyChars := Name.ToString.ToCharArray(true);
  if not CheckResult(rtl.RegGetValue(CurrentKey, nil, @lKeyChars[0], rtl.RRF_RT_ANY, @lType, Buffer, @BufSize)) then
    raise new Exception('Can not retrieve value for ' + Name);
  RegData := ConvertDataType(lType);
  result := BufSize;
end;

method TRegistry.GetKey(Key: DelphiString): rtl.HKEY;
begin
  var lNewKey: rtl.HKEY;
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);
  var lKeyChars := lKey.ToString.ToCharArray(true);

  if CheckResult(rtl.RegOpenKeyEx(lCurrent, @lKeyChars[0], 0, fAccess or rtl.KEY_WOW64_RES, @lNewKey)) then
    result := lNewKey
  else
    raise new Exception('Error opening ' + Key);
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

method TRegistry.PutData(Name: DelphiString; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType);
begin
  var lNameChars := Name.ToString.ToCharArray(true);
  if not CheckResult(rtl.RegSetKeyValue(CurrentKey, nil, @lNameChars[0], ConvertToDataType(RegData), Buffer, BufSize)) then
    raise new Exception('Can not write ' + Name + ' value to registry');
end;

method TRegistry.SetRootKey(Value: rtl.HKEY);
begin
  fRootKey := Value;
  CloseKey;
end;

method TRegistry.ConvertDataType(Value: rtl.DWORD): TRegDataType;
begin
  result := case Value of
    rtl.REG_BINARY: TRegDataType.Binary;
    rtl.REG_DWORD: TRegDataType.Integer;
    rtl.REG_SZ: TRegDataType.String;
    rtl.REG_EXPAND_SZ: TRegDataType.ExpandString;
    else TRegDataType.Unknown;
  end;
end;

method TRegistry.ConvertToDataType(Value: TRegDataType): rtl.DWORD;
begin
  result := case Value of
    TRegDataType.Binary: rtl.REG_BINARY;
    TRegDataType.Integer: rtl.REG_DWORD;
    TRegDataType.String: rtl.REG_SZ;
    TRegDataType.ExpandString: rtl.REG_EXPAND_SZ;
    else rtl.REG_NONE;
  end;
end;

method TRegistry.GetProperKey(Key: DelphiString; var Normalized: DelphiString): rtl.HKEY;
begin
  var lRelative := IsRelative(Key);
  if not lRelative then
    Normalized := Key.SubString(1)
  else
    Normalized := Key;
  result := GetBaseKey(lRelative);
end;

method TRegistry.CloseKey;
begin
  if (fCurrentKey <> nil) then begin
    rtl.RegFlushKey(fCurrentKey);
    rtl.RegCloseKey(fCurrentKey);
    fCurrentKey := nil;
    fCurrentPath := '';
  end;
end;

method TRegistry.CreateKey(Key: DelphiString): Boolean;
begin
  var lNewKey: rtl.HKEY;
  var lDisposition: rtl.DWORD;
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);
  var lKeyChars := lKey.ToString.ToCharArray(true);

  result := CheckResult(rtl.RegCreateKeyEx(lCurrent, @lKeyChars[0], 0, nil, rtl.REG_OPTION_NON_VOLATILE,
    rtl.KEY_ALL_ACCESS or (fAccess and rtl.KEY_WOW64_RES), nil, @lNewKey, @lDisposition));

  if not result then
    raise new Exception('Error while creating new registry key: ' + Key)
  else
    if lDisposition = rtl.REG_OPENED_EXISTING_KEY then
      rtl.RegCloseKey(lNewKey);
end;

method TRegistry.DeleteKey(Key: DelphiString): Boolean;
begin
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);
  var lKeyChars := lKey.ToString.ToCharArray(true);

  result := CheckResult(rtl.RegDeleteKeyEx(lCurrent, @lKeyChars[0], rtl.KEY_ALL_ACCESS {and rtl.KEY_WOW64_RES}, 0));
end;

method TRegistry.DeleteValue(Name: DelphiString): Boolean;
begin
  var lKeyChars := Name.ToString.ToCharArray(true);
  result := CheckResult(rtl.RegDeleteValue(CurrentKey, @lKeyChars[0]));
end;

method TRegistry.GetDataInfo(ValueName: DelphiString; var Value: TRegDataInfo): Boolean;
begin
  var lType: rtl.DWORD;
  var lValueChars := ValueName.ToString.ToCharArray(true);
  result := CheckResult(rtl.RegGetValue(CurrentKey, nil, @lValueChars[0], rtl.RRF_RT_ANY, @lType, nil, @Value.DataSize));
  Value.RegData := ConvertDataType(lType);
end;

method TRegistry.GetDataSize(ValueName: DelphiString): Integer;
begin
  var lData: TRegDataInfo;
  if GetDataInfo(ValueName, var lData) then
    result := lData.DataSize
  else
    result := -1;
end;

method TRegistry.GetDataType(ValueName: DelphiString): TRegDataType;
begin
  var lData: TRegDataInfo;
  if GetDataInfo(ValueName, var lData) then
    result := lData.RegData
  else
    result := TRegDataType.Unknown;
end;

method TRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;
begin
  result := CheckResult(rtl.RegQueryInfoKey(CurrentKey, nil, nil, nil, @Value.NumSubKeys, @Value.MaxSubKeyLen, nil, 
    @Value.NumValues, @Value.MaxValueLen, @Value.MaxDataLen, nil, @Value.FileTime));
end;

method TRegistry.GetKeyNames(Strings: TStrings);
begin
  var lKeyInfo: TRegKeyInfo;
  var lWritten: rtl.DWORD;
  if GetKeyInfo(var lKeyInfo) then begin
    var lBuffer := new Char[lKeyInfo.MaxValueLen + 1];
    Strings.BeginUpdate;
    try
      for i: Integer := 0 to lKeyInfo.NumSubKeys - 1 do begin
        lWritten := lBuffer.Length;
        if not CheckResult(rtl.RegEnumKeyEx(CurrentKey, i, @lBuffer[0], @lWritten, nil, nil, nil, nil)) then
          raise new Exception("Can not get registry subkeys");
        Strings.Add(DelphiString.Create(lBuffer, 0, lWritten));
      end;

    finally
      Strings.EndUpdate;
    end;
  end;
end;

method TRegistry.GetValueNames(Strings: TStrings);
begin
  var lKeyInfo: TRegKeyInfo;
  var lWritten: rtl.DWORD;
  if GetKeyInfo(var lKeyInfo) then begin
    var lBuffer := new Char[lKeyInfo.MaxValueLen + 1];
    Strings.BeginUpdate;
    try
      for i: Integer := 0 to lKeyInfo.NumValues - 1 do begin
        lWritten := lBuffer.Length;
        if not CheckResult(rtl.RegEnumValue(CurrentKey, i, @lBuffer[0], @lWritten, nil, nil, nil, nil)) then
          raise new Exception("Can not get registry value names");
        Strings.Add(DelphiString.Create(lBuffer, 0, lWritten));
      end;

    finally
      Strings.EndUpdate;
    end;
  end;
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
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);
  var lKeyChars := lKey.ToString.ToCharArray(true);

  rtl.RegOpenKeyEx(lCurrent, @lKeyChars[0], 0, (fAccess and rtl.KEY_WOW64_RES) or rtl.STANDARD_RIGHTS_READ or rtl.KEY_QUERY_VALUE or rtl.KEY_ENUMERATE_SUB_KEYS, @lNewKey);
  if lNewKey <> nil then begin
    rtl.RegCloseKey(lNewKey);
    result := true;
  end
  else 
    result := false;
end;

method TRegistry.LoadKey(Key, FileName: DelphiString): Boolean;
begin
  var lKeyChars := Key.ToString.ToCharArray(true);
  var lFileChars := FileName.ToString.ToCharArray(true);
  result := CheckResult(rtl.RegLoadKey(fRootKey, @lKeyChars[0], @lFileChars[0]));
end;

method TRegistry.OpenKey(const Key: DelphiString; CanCreate: Boolean): Boolean;
begin
  CloseKey;
  var lNewKey: rtl.HKEY;
  var lDisposition: rtl.DWORD;
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);
  var lKeyChars := lKey.ToString.ToCharArray(true);

  if CanCreate then
    result := CheckResult(rtl.RegCreateKeyEx(lCurrent, @lKeyChars[0], 0, nil, rtl.REG_OPTION_NON_VOLATILE,
      rtl.KEY_ALL_ACCESS or (fAccess and rtl.KEY_WOW64_RES), nil, @lNewKey, @lDisposition))
  else
    result := CheckResult(rtl.RegOpenKeyEx(lCurrent, @lKeyChars[0], 0, fAccess, @lNewKey));

  if result then begin
    var lCurrentPath: DelphiString;
    if IsRelative(Key) then
      lCurrentPath := fCurrentPath + '\' + Key
    else
      lCurrentPath := RootKeyName + Key;
    ChangeKey(lNewKey, lCurrentPath)
  end;
end;

method TRegistry.OpenKeyReadOnly(Key: DelphiString): Boolean;
begin
  CloseKey;
  var lNewKey: rtl.HKEY;
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);

  result := CheckResult(rtl.RegOpenKeyEx(lCurrent, lKey.ToString.FirstChar, 0, fAccess or rtl.KEY_READ, @lNewKey));
  if result then begin
    var lCurrentPath: DelphiString;
    if IsRelative(Key) then
      lCurrentPath := fCurrentPath + '\' + Key
    else
      lCurrentPath := RootKeyName + Key;
    ChangeKey(lNewKey, lCurrentPath)
  end;
end;

method TRegistry.ReadBinaryData(Name: DelphiString; var Buffer: Pointer; BufSize: Integer): Integer;
begin
  var lRegData: TRegDataType;
  result := GetData(Name, Buffer, BufSize, var lRegData);
  if lRegData <> TRegDataType.Binary then
    raise new Exception('Can not read ' + Name + ' as binary data');
end;

method TRegistry.ReadBool(Name: DelphiString): Boolean;
begin
  result := ReadInteger(Name) > 0;
end;

method TRegistry.ReadDate(Name: DelphiString): TDateTime;
begin
  result := ReadDateTime(Name);
end;

method TRegistry.ReadDateTime(Name: DelphiString): TDateTime;
begin
  var lRegData: TRegDataType;
  GetData(Name, @result, sizeOf(result), var lRegData);
  if lRegData <> TRegDataType.Binary then
    raise new Exception('Can not read ' + Name + ' as datetime value');
end;

method TRegistry.ReadFloat(Name: DelphiString): Double;
begin
  var lRegData: TRegDataType;
  GetData(Name, @result, sizeOf(result), var lRegData);
  if lRegData <> TRegDataType.Binary then
    raise new Exception('Can not read ' + Name + ' as float value');
end;

method TRegistry.ReadInteger(Name: DelphiString): Integer;
begin
  var lRegData: TRegDataType;
  GetData(Name, @result, sizeOf(result), var lRegData);
  if lRegData <> TRegDataType.Integer then
    raise new Exception('Can not read ' + Name + ' as integer value');
end;

method TRegistry.ReadString(Name: DelphiString): DelphiString;
begin
  var lSize := GetDataSize(Name);
  var lChars := new Char[lSize div sizeOf(Char)];
  var lRegData: TRegDataType;
  var lTotal := GetData(Name, @lChars[0], lSize, var lRegData);
  if (lRegData = TRegDataType.String) or (lRegData = TRegDataType.ExpandString) then
    result := DelphiString.Create(lChars, 0, (lTotal div sizeOf(Char)) - 1) // remove #0
  else
    raise new Exception('Can no read ' + Name + ' as string value');
end;

method TRegistry.ReadTime(Name: DelphiString): TDateTime;
begin
  result := ReadDateTime(Name);
end;

method TRegistry.RegistryConnect(UNCName: DelphiString): Boolean;
begin
  var lNewKey: rtl.HKEY;
  result := CheckResult(rtl.RegConnectRegistry(UNCName.ToString.FirstChar, fRootKey, @lNewKey));
  if result then
    fRootKey := lNewKey;
end;

method TRegistry.ReplaceKey(Key, FileName, BackUpFileName: DelphiString): Boolean;
begin
  var lKey: DelphiString;
  var lCurrent := GetProperKey(Key, var lKey);

  result := CheckResult(rtl.RegReplaceKey(lCurrent, lKey.ToString.FirstChar, FileName.ToString.FirstChar, FileName.ToString.FirstChar));
end;

method TRegistry.RestoreKey(Key, FileName: DelphiString): Boolean;
begin
  var lKey := GetKey(Key);
  var lFile := FileName.ToCharArray;
  result := CheckResult(rtl.RegRestoreKey(lKey, @lFile[0], 0));
end;

method TRegistry.SaveKey(Key, FileName: DelphiString): Boolean;
begin
  var lKey := GetKey(Key);
  var lFile := FileName.ToCharArray;
  result := CheckResult(rtl.RegSaveKeyEx(lKey, @lFile[0], nil, rtl.REG_LATEST_FORMAT));
end;

method TRegistry.UnLoadKey(Key: DelphiString): Boolean;
begin
  result := CheckResult(rtl.RegUnLoadKey(fRootKey, Key.ToString.FirstChar));
end;

method TRegistry.ValueExists(Name: DelphiString): Boolean;
begin
  var lType: rtl.DWORD;
  result := CheckResult(rtl.RegQueryValueEx(CurrentKey, Name.ToString.FirstChar, nil, @lType, nil, nil));
end;

method TRegistry.WriteBinaryData(Name: DelphiString; Buffer: Pointer; BufSize: Integer);
begin
  PutData(Name, Buffer, BufSize, TRegDataType.Binary);
end;

method TRegistry.WriteBool(Name: DelphiString; Value: Boolean);
begin
  WriteInteger(Name, Integer(Value));
end;

method TRegistry.WriteDate(Name: DelphiString; Value: TDateTime);
begin
  WriteDateTime(Name, Value);
end;

method TRegistry.WriteDateTime(Name: DelphiString; Value: TDateTime);
begin
  PutData(Name, @Value, sizeOf(Double), TRegDataType.Binary);
end;

method TRegistry.WriteFloat(Name: DelphiString; Value: Double);
begin
  PutData(Name, @Value, sizeOf(Double), TRegDataType.Binary);
end;

method TRegistry.WriteInteger(Name: DelphiString; Value: Integer);
begin
  PutData(Name, @Value, sizeOf(Integer), TRegDataType.Integer);
end;

method TRegistry.WriteString(Name, Value: DelphiString);
begin
  var lChars := Value.ToString.ToCharArray(true);
  PutData(Name, @lChars[0], lChars.Length * sizeOf(Char), TRegDataType.String);
end;

method TRegistry.WriteExpandString(Name, Value: DelphiString);
begin
  WriteString(Name, Value);
end;

method TRegistry.WriteTime(Name: DelphiString; Value: TDateTime);
begin
  WriteDateTime(Name, Value);
end;

{$ENDIF}

end.