namespace RemObjects.Elements.RTL.Delphi;

{$IF ISLAND AND WINDOWS}

interface

type
  TRegistry = public class(TObject)
  private
    fRootKey: rtl.HKEY;
    fAccess: LongWord;
  
  protected
    /*procedure ChangeKey(Value: HKey; const Path: string);
    function CheckResult(RetVal: Longint): Boolean;
    function GetBaseKey(Relative: Boolean): HKey;
    function GetData(const Name: string; Buffer: Pointer;
      BufSize: Integer; var RegData: TRegDataType): Integer;
    function GetKey(const Key: string): HKEY;
    function GetRootKeyName: string;
    procedure PutData(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType);
    procedure SetCurrentKey(Value: HKEY);*/
  public
    constructor;
    constructor(aAccess: LongWord);
    //destructor Destroy; override;
    procedure CloseKey;
    /*function CreateKey(const Key: string): Boolean;
    function DeleteKey(const Key: string): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function GetDataAsString(const ValueName: string; PrefixType: Boolean = false): string;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    function GetDataSize(const ValueName: string): Integer;
    function GetDataType(const ValueName: string): TRegDataType;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    function HasSubKeys: Boolean;
    function KeyExists(const Key: string): Boolean;
    function LoadKey(const Key, FileName: string): Boolean;
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    function OpenKeyReadOnly(const Key: String): Boolean;
    function ReadCurrency(const Name: string): Currency;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadBool(const Name: string): Boolean;
    function ReadDate(const Name: string): TDateTime;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadTime(const Name: string): TDateTime;
    function RegistryConnect(const UNCName: string): Boolean;
    procedure RenameValue(const OldName, NewName: string);
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
    function RestoreKey(const Key, FileName: string): Boolean;
    function SaveKey(const Key, FileName: string): Boolean;
    function UnLoadKey(const Key: string): Boolean;
    function ValueExists(const Name: string): Boolean;
    procedure WriteCurrency(const Name: string; Value: Currency);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDate(const Name: string; Value: TDateTime);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteExpandString(const Name, Value: string);
    procedure WriteTime(const Name: string; Value: TDateTime);
    property CurrentKey: HKEY read FCurrentKey;
    property CurrentPath: string read FCurrentPath;
    property LazyWrite: Boolean read FLazyWrite write FLazyWrite;
    property LastError: Longint read FLastError;
    property LastErrorMsg: string read GetLastErrorMsg;
    property RootKey: HKEY read FRootKey write SetRootKey;
    property RootKeyName: string read GetRootKeyName;
    property Access: LongWord read FAccess write FAccess;*/
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

procedure TRegistry.CloseKey;
begin

end;

{$ENDIF}

end.
