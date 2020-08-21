namespace RemObjects.Elements.RTL.Delphi;

{$IFNDEF WEBASSEMBLY}

interface

uses
  RemObjects.Elements.RTL;

type
  TCustomIniFile = public abstract class(TObject)
  private
    fFileName: DelphiString;
  protected
    //const SectionNameSeparator: DelphiString = '\';
    method InternalReadSections(aSection: DelphiString; aStrings: TStrings; aSubSectionNamesOnly, aRecurse: Boolean); virtual;
  public
    constructor(aFileName: DelphiString);
    method SectionExists(aSection: DelphiString): Boolean;
    method ReadString(aSection, aIdent, aDefault: DelphiString): DelphiString; virtual; abstract;
    method WriteString(aSection, aIdent, aValue: DelphiString); virtual; abstract;
    method ReadInteger(aSection, aIdent: DelphiString; aDefault: Integer): Integer; virtual;
    method WriteInteger(aSection, aIdent: DelphiString; aValue: Integer); virtual;
    method ReadBool(aSection, aIdent: DelphiString; aDefault: Boolean): Boolean; virtual;
    method WriteBool(aSection, aIdent: DelphiString; aValue: Boolean); virtual;
    //method ReadBinaryStream(const Section, Name: DelphiString; Value: TStream): Integer; virtual;
    method ReadDate(aSection, aName: DelphiString; aDefault: TDateTime): TDateTime; virtual;
    method ReadDateTime(aSection, aName: DelphiString; aDefault: TDateTime): TDateTime; virtual;
    method ReadFloat(aSection, aName: DelphiString; aDefault: Double): Double; virtual;
    method ReadTime(aSection, aName: DelphiString; aDefault: TDateTime): TDateTime; virtual;
    //method WriteBinaryStream(const Section, Name: DelphiString; Value: TStream); virtual;
    method WriteDate(Section, Name: DelphiString; Value: TDateTime); virtual;
    method WriteDateTime(Section, Name: DelphiString; Value: TDateTime); virtual;
    method WriteFloat(aSection, aName: DelphiString; aValue: Double); virtual;
    method WriteTime(Section, Name: DelphiString; Value: TDateTime); virtual;
    method ReadSection(Section: DelphiString; DelphiStrings: TStrings); virtual; abstract;
    method ReadSections(DelphiStrings: TStrings); virtual; abstract;
    method ReadSections(aSection: DelphiString; aStrings: TStrings); virtual;
    method ReadSubSections(aSection: DelphiString; aStrings: TStrings; aRecurse: Boolean := False); virtual;
    method ReadSectionValues(aSection: DelphiString; aStrings: TStrings); virtual; abstract;
    method EraseSection(Section: DelphiString); virtual; abstract;
    method DeleteKey(Section, Ident: DelphiString); virtual; abstract;
    method UpdateFile; virtual; abstract;
    method ValueExists(aSection, aIdent: DelphiString): Boolean; virtual;
    property FileName: DelphiString read fFileName;
  end;

  TIniSectionsPair = TPair<String, TStringList>;
  TMemIniFile = public class(TCustomIniFile)
  private
    fEncoding: TEncoding;
    fModified: Boolean;
    fAutoSave: Boolean;
    fData: TList<TIniSectionsPair>;
    fCaseSensitive: Boolean;
    method SetCaseSensitive(Value: Boolean);
    method LoadIni;
    method IndexOfSection(aSection: DelphiString): Integer;
  public
    constructor(aFileName: DelphiString);
    constructor(aFileName: DelphiString; aEncoding: TEncoding);
    constructor(aFileName: DelphiString; aEncoding: TEncoding; aCaseSensitive: Boolean); virtual;
    class method Create(aFileName: DelphiString): TMemIniFile; static;
    method Clear;
    method DeleteKey(aSection, aIdent: DelphiString); override;
    method EraseSection(Section: DelphiString); override;
    method GetStrings(aList: TStrings);
    method ReadSection(aSection: DelphiString; aStrings: TStrings); override;
    method ReadSections(Strings: TStrings); override;
    method ReadSectionValues(aSection: DelphiString; aStrings: TStrings); override;
    method ReadString(aSection, aIdent, aDefault: DelphiString): DelphiString; override;
    method Rename(FileName: DelphiString; Reload: Boolean);
    method SetStrings(aList: TStrings);
    method UpdateFile; override;
    method WriteString(aSection, aIdent, aValue: DelphiString); override;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Encoding: TEncoding read fEncoding write fEncoding;
    property Modified: Boolean read fModified write fModified;
    property AutoSave: Boolean read fAutoSave write fAutoSave;
  end;

  TIniFile = public class(TMemIniFile)
  public
    constructor(aFileName: DelphiString);
    constructor(aFileName: DelphiString; aEncoding: TEncoding);
    constructor(aFileName: DelphiString; aEncoding: TEncoding; aCaseSensitive: Boolean); override;
  end;

implementation

method TCustomIniFile.InternalReadSections(aSection: DelphiString; aStrings: TStrings; aSubSectionNamesOnly: Boolean; aRecurse: Boolean);
begin
  var lValues := TStringList.Create;
  ReadSections(lValues);
  aStrings.BeginUpdate;
  try
    for i: Integer := 0 to lValues.Count - 1 do begin
      if (aSection = '') or (CompareText(aSection, lValues[i]) = 0) then
        aStrings.Add(lValues[i]);
    end;

  finally
    aStrings.EndUpdate;
  end;
end;

constructor TCustomIniFile(aFileName: DelphiString);
begin
  fFileName := aFileName;
end;

method TCustomIniFile.SectionExists(aSection: DelphiString): Boolean;
begin
  var lStr := TStringList.Create;
  ReadSectionValues(aSection, lStr);
  result := lStr.Count > 0;
end;

method TCustomIniFile.ReadInteger(aSection: DelphiString; aIdent: DelphiString; aDefault: Integer): Integer;
begin
  var lStr := ReadString(aSection, aIdent, '');
  if lStr <> '' then begin
    var lInt := Convert.TryToInt32(lStr); // TODO
    if lInt <> nil then
      result := lInt
    else
      result := aDefault;
  end
  else
    result := aDefault;
end;

method TCustomIniFile.WriteInteger(aSection: DelphiString; aIdent: DelphiString; aValue: Integer);
begin
  var lStr := Convert.ToString(aValue);
  WriteString(aSection, aIdent, lStr);
end;

method TCustomIniFile.ReadBool(aSection: DelphiString; aIdent: DelphiString; aDefault: Boolean): Boolean;
begin
  var lStr := ReadString(aSection, aIdent, '');
  if lStr <> '' then begin
    result := StrToInt(lStr) <> 0;
  end
  else
    result := aDefault;
end;

method TCustomIniFile.WriteBool(aSection: DelphiString; aIdent: DelphiString; aValue: Boolean);
begin
  var lStr: DelphiString;
  if aValue then
    lStr := '1'
  else
    lStr := '0';
  WriteString(aSection, aIdent, lStr);
end;

method TCustomIniFile.ReadDate(aSection: DelphiString; aName: DelphiString; aDefault: TDateTime): TDateTime;
begin
  var lStr := ReadString(aSection, aName, '');
  if lStr <> '' then begin
    if not TryStrToDate(lStr, out result) then
      result := aDefault;
  end
  else
     result := aDefault;
end;

method TCustomIniFile.ReadDateTime(aSection: DelphiString; aName: DelphiString; aDefault: TDateTime): TDateTime;
begin
  var lStr := ReadString(aSection, aName, '');
  if lStr <> '' then begin
    if not TryStrToDateTime(lStr, out result) then
      result := aDefault;
  end
  else
     result := aDefault;
end;

method TCustomIniFile.ReadFloat(aSection: DelphiString; aName: DelphiString; aDefault: Double): Double;
begin
  var lStr := ReadString(aSection, aName, '');
  if lStr <> '' then begin
    var lDouble := Convert.TryToDoubleInvariant(lStr); // TODO
    if lDouble <> nil then
      result := lDouble
    else
      result := aDefault;
  end
  else
    result := aDefault;
end;

method TCustomIniFile.ReadTime(aSection: DelphiString; aName: DelphiString; aDefault: TDateTime): TDateTime;
begin
  var lStr := ReadString(aSection, aName, '');
  if lStr <> '' then begin
    if not TryStrToTime(lStr, out result) then
      result := aDefault;
  end
  else
     result := aDefault;
end;

method TCustomIniFile.WriteDate(Section: DelphiString; Name: DelphiString; Value: TDateTime);
begin

end;

method TCustomIniFile.WriteDateTime(Section: DelphiString; Name: DelphiString; Value: TDateTime);
begin

end;

method TCustomIniFile.WriteFloat(aSection: DelphiString; aName: DelphiString; aValue: Double);
begin
  var lStr := Convert.ToString(aValue);
  WriteString(aSection, aName, lStr);
end;

method TCustomIniFile.WriteTime(Section: DelphiString; Name: DelphiString; Value: TDateTime);
begin

end;

method TCustomIniFile.ReadSections(aSection: DelphiString; aStrings: TStrings);
begin
  InternalReadSections(aSection, aStrings, false, true);
end;

method TCustomIniFile.ReadSubSections(aSection: DelphiString; aStrings: TStrings; aRecurse: Boolean := false);
begin
  InternalReadSections(aSection, aStrings, true, aRecurse);
end;

method TCustomIniFile.ValueExists(aSection: DelphiString; aIdent: DelphiString): Boolean;
begin
  var lValues := TStringList.Create;
  ReadSection(aSection, lValues);
  result := lValues.IndexOf(aIdent) >= 0;
end;

method TMemIniFile.SetCaseSensitive(Value: Boolean);
begin
  fCaseSensitive := Value;
end;

constructor TMemIniFile(aFileName: DelphiString);
begin
  constructor(aFileName, TEncoding.Default, false);
end;

constructor TMemIniFile(aFileName: DelphiString; aEncoding: TEncoding);
begin
  constructor(aFileName, aEncoding, false);
end;

constructor TMemIniFile(aFileName: DelphiString; aEncoding: TEncoding; aCaseSensitive: Boolean);
begin
  inherited constructor(aFileName);
  fEncoding := aEncoding;
  fData := new TList<TIniSectionsPair>();
  CaseSensitive := aCaseSensitive;
  LoadIni;
end;

class method TMemIniFile.Create(aFileName: DelphiString): TMemIniFile;
begin
  result := new TMemIniFile(aFileName);
end;

method TMemIniFile.Clear;
begin
  fData.Clear;
end;

method TMemIniFile.DeleteKey(aSection: DelphiString; aIdent: DelphiString);
begin
  var lSectionIndex := IndexOfSection(aSection);
  if lSectionIndex >= 0 then begin
    var lValues := TStringList.Create;
    lValues.CaseSensitive := CaseSensitive;
    ReadSection(aSection, lValues);
    var lIndex := lValues.IndexOf(aIdent);
    if lIndex >= 0 then
      fData[lSectionIndex].Value.Delete(lIndex);
  end;
end;

method TMemIniFile.EraseSection(Section: DelphiString);
begin
  var lIndex := IndexOfSection(Section);
  if lIndex >= 0 then
    fData.Delete(lIndex);
end;

method TMemIniFile.GetStrings(aList: TStrings);
begin
  aList.BeginUpdate;
  try
    aList.Clear;
    for each lItem in fData do begin
      aList.Add('[' + (lItem.Key as not nullable as PlatformString) + ']');
      aList.AddStrings(lItem.Value);
    end;
  finally
    aList.EndUpdate;
  end;
end;

method TMemIniFile.ReadSection(aSection: DelphiString; aStrings: TStrings);
begin
  aStrings.BeginUpdate;
  try
    aStrings.Clear;
    var lIndex := IndexOfSection(aSection);
    if lIndex >= 0 then begin
      var lStrings := fData[lIndex].Value;
      for i: Integer := 0 to lStrings.Count - 1 do
        aStrings.Add(lStrings.Names[i]);
    end;
  finally
    aStrings.EndUpdate;
  end;
end;

method TMemIniFile.ReadSections(Strings: TStrings);
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for each lItem in fData do
      Strings.Add(lItem.Key);
  finally
    Strings.EndUpdate;
  end;
end;

method TMemIniFile.ReadSectionValues(aSection: DelphiString; aStrings: TStrings);
begin
  aStrings.BeginUpdate;
  try
    aStrings.Clear;
    var lIndex := IndexOfSection(aSection);
    if lIndex >= 0 then
      aStrings.SetStrings(fData[lIndex].Value);
  finally
    aStrings.EndUpdate;
  end;
end;

method TMemIniFile.ReadString(aSection: DelphiString; aIdent: DelphiString; aDefault: DelphiString): DelphiString;
begin
  result := aDefault;
  var lIndex := IndexOfSection(aSection);
  if lIndex >= 0 then begin
    var lValue := fData[lIndex].Value.Values[aIdent];
    if lValue <> '' then
      result := lValue;
  end;
end;

method TMemIniFile.Rename(FileName: DelphiString; Reload: Boolean);
begin

end;

method TMemIniFile.SetStrings(aList: TStrings);
begin
  var lStr: DelphiString;
  var lCurrentSection: TStringList := nil;
  for i: Integer := 0 to aList.Count - 1 do begin
    lStr := aList[i].Trim;
    if (lStr.Length > 0) and (lStr.Chars[0] <> ';') then begin  // avoid comments and empty lines
      if lStr.Chars[0] = '[' then begin
        var lTitle := lStr.SubString(1, lStr.Length - 2);
        lCurrentSection := TStringList.Create;
        lCurrentSection.CaseSensitive := CaseSensitive;
        fData.Add(new TIniSectionsPair(lTitle, lCurrentSection));
      end
      else begin
        var lPos: Integer;
        if lCurrentSection <> nil then begin
          lPos := lStr.IndexOf('=');
          if lPos > 0 then begin
            var lValue: DelphiString := '';
            if lPos + 1 < lStr.Length then
              lValue := lStr.SubString(lPos + 1).Trim;
            lCurrentSection.Add(lStr.SubString(0, lPos).Trim + '=' + lValue);
          end
          else
            lCurrentSection.Add(lStr);
        end;
      end;
    end;
  end;
end;

method TMemIniFile.UpdateFile;
begin

end;

method TMemIniFile.WriteString(aSection: DelphiString; aIdent: DelphiString; aValue: DelphiString);
begin
  var lIndex := IndexOfSection(aSection);
  if lIndex >= 0 then begin
    fData[lIndex].Value.Values[aIdent] := aValue;
  end;
end;

method TMemIniFile.LoadIni;
begin
  var lValues := new TStringList;
  lValues.LoadFromFile(FileName);
  SetStrings(lValues);
end;

method TMemIniFile.IndexOfSection(aSection: DelphiString): Integer;
begin
  for i: Integer := 0 to fData.Count - 1 do
    if CaseSensitive then begin
      if aSection = fData[i].Key then
        exit(i);
    end
    else
      if DelphiString.Compare(aSection, fData[i].Key, true) = 0 then
        exit(i);

  result := -1;
end;

constructor TIniFile(aFileName: DelphiString);
begin
  inherited constructor(aFileName);
  AutoSave := true;
end;

constructor TIniFile(aFileName: DelphiString; aEncoding: TEncoding);
begin
  inherited constructor(aFileName, aEncoding);
  AutoSave := true;
end;

constructor TIniFile(aFileName: DelphiString; aEncoding: TEncoding; aCaseSensitive: Boolean);
begin
  inherited constructor(aFileName, aEncoding, aCaseSensitive);
  AutoSave := true;
end;

{$ENDIF}

end.