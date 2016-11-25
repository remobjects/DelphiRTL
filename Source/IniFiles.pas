namespace RemObjects.Elements.RTL.Delphi;

interface

type

  TCustomIniFile = public abstract class(TObject)
  private
    fFileName: DelphiString;
  protected
    //const SectionNameSeparator: DelphiString = '\';
    method InternalReadSections(const Section: DelphiString; DelphiStrings: TStrings; SubSectionNamesOnly, Recurse: Boolean); virtual;
  public
    constructor(const aFileName: DelphiString);
    method SectionExists(const Section: DelphiString): Boolean;
    method ReadDelphiString(const Section, Ident, Default: DelphiString): DelphiString; virtual; abstract;
    method WriteString(const Section, Ident, Value: DelphiString); virtual; abstract;
    method ReadInteger(const Section, Ident: DelphiString; Default: Integer): Integer; virtual;
    method WriteInteger(const Section, Ident: DelphiString; Value: Integer); virtual;
    method ReadBool(const Section, Ident: DelphiString; Default: Boolean): Boolean; virtual;
    method WriteBool(const Section, Ident: DelphiString; Value: Boolean); virtual;
    //method ReadBinaryStream(const Section, Name: DelphiString; Value: TStream): Integer; virtual;
    //method ReadDate(const Section, Name: DelphiString; Default: TDateTime): TDateTime; virtual;
    //method ReadDateTime(const Section, Name: DelphiString; Default: TDateTime): TDateTime; virtual;
    method ReadFloat(const Section, Name: DelphiString; Default: Double): Double; virtual;
    //method ReadTime(const Section, Name: DelphiString; Default: TDateTime): TDateTime; virtual;
    //method WriteBinaryStream(const Section, Name: DelphiString; Value: TStream); virtual;
    //method WriteDate(const Section, Name: DelphiString; Value: TDateTime); virtual;
    //method WriteDateTime(const Section, Name: DelphiString; Value: TDateTime); virtual;
    method WriteFloat(const Section, Name: DelphiString; Value: Double); virtual;
    //method WriteTime(const Section, Name: DelphiString; Value: TDateTime); virtual;
    method ReadSection(const Section: DelphiString; DelphiStrings: TStrings); virtual; abstract;
    method ReadSections(DelphiStrings: TStrings); virtual; abstract;
    method ReadSections(const Section: DelphiString; DelphiStrings: TStrings); virtual;
    method ReadSubSections(const Section: DelphiString; DelphiStrings: TStrings; Recurse: Boolean := False); virtual;
    method ReadSectionValues(const Section: DelphiString; DelphiStrings: TStrings); virtual; abstract;
    method EraseSection(const Section: DelphiString); virtual; abstract;
    method DeleteKey(const Section, Ident: DelphiString); virtual; abstract;
    method UpdateFile; virtual; abstract;
    method ValueExists(const Section, Ident: DelphiString): Boolean; virtual;
    property FileName: DelphiString read FFileName;
  end;

  TMemIniFile = class(TCustomIniFile)
  private
    fEncoding: TEncoding;
    fModified: Boolean;
    fAutoSave: Boolean;
    method GetCaseSensitive: Boolean;
    method SetCaseSensitive(Value: Boolean);
    method LoadIni;
  public
    constructor(const aFileName: DelphiString);
    constructor(const aFileName: DelphiString; const aEncoding: TEncoding);
    constructor(const aFileName: DelphiString; const aEncoding: TEncoding; aCaseSensitive: Boolean); virtual;
    method Clear;
    method DeleteKey(const Section, Ident: DelphiString); override;
    method EraseSection(const Section: DelphiString); override;
    method GeTStrings(const List: TStrings);
    method ReadSection(const Section: DelphiString; DelphiStrings: TStrings); override;
    method ReadSections(DelphiStrings: TStrings); override;
    method ReadSectionValues(const Section: DelphiString; DelphiStrings: TStrings); override;
    method ReadDelphiString(const Section, Ident, Default: DelphiString): DelphiString; override;
    method Rename(const FileName: DelphiString; Reload: Boolean);
    method SetStrings(const List: TStrings);
    method UpdateFile; override;
    method WriteString(const Section, Ident, Value: DelphiString); override;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property Encoding: TEncoding read FEncoding write FEncoding;
    property Modified: Boolean read FModified write FModified;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
  end;

  TIniFile = class(TMemIniFile)
  public
    constructor(const FileName: DelphiString; const Encoding: TEncoding; CaseSensitive: Boolean); override;
  end;

implementation

method TCustomIniFile.InternalReadSections(Section: DelphiString; DelphiStrings: TStrings; SubSectionNamesOnly: Boolean; Recurse: Boolean);
begin

end;

constructor TCustomIniFile(const aFileName: DelphiString);
begin
  FFileName := FileName;
end;

method TCustomIniFile.SectionExists(Section: DelphiString): Boolean;
begin

end;

method TCustomIniFile.ReadInteger(Section: DelphiString; Ident: DelphiString; &Default: Integer): Integer;
begin

end;

method TCustomIniFile.WriteInteger(Section: DelphiString; Ident: DelphiString; Value: Integer);
begin

end;

method TCustomIniFile.ReadBool(Section: DelphiString; Ident: DelphiString; &Default: Boolean): Boolean;
begin

end;

method TCustomIniFile.WriteBool(Section: DelphiString; Ident: DelphiString; Value: Boolean);
begin

end;

/*method TCustomIniFile.ReadDate(Section: DelphiString; Name: DelphiString; &Default: TDateTime): TDateTime;
begin

end;

method TCustomIniFile.ReadDateTime(Section: DelphiString; Name: DelphiString; &Default: TDateTime): TDateTime;
begin

end;
*/

method TCustomIniFile.ReadFloat(Section: DelphiString; Name: DelphiString; &Default: Double): Double;
begin

end;

/*method TCustomIniFile.ReadTime(Section: DelphiString; Name: DelphiString; &Default: TDateTime): TDateTime;
begin

end;

method TCustomIniFile.WriteDate(Section: DelphiString; Name: DelphiString; Value: TDateTime);
begin

end;

method TCustomIniFile.WriteDateTime(Section: DelphiString; Name: DelphiString; Value: TDateTime);
begin

end;*/

method TCustomIniFile.WriteFloat(Section: DelphiString; Name: DelphiString; Value: Double);
begin

end;

/*method TCustomIniFile.WriteTime(Section: DelphiString; Name: DelphiString; Value: TDateTime);
begin

end;*/

method TCustomIniFile.ReadSections(Section: DelphiString; DelphiStrings: TStrings);
begin

end;

method TCustomIniFile.ReadSubSections(Section: DelphiString; DelphiStrings: TStrings; Recurse: Boolean := false);
begin

end;

method TCustomIniFile.ValueExists(Section: DelphiString; Ident: DelphiString): Boolean;
begin

end;

method TMemIniFile.GetCaseSensitive: Boolean;
begin

end;

method TMemIniFile.SetCaseSensitive(Value: Boolean);
begin

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
  SetCaseSensitive(aCaseSensitive); // TODO
  LoadIni;
end;

method TMemIniFile.Clear;
begin

end;

method TMemIniFile.DeleteKey(Section: DelphiString; Ident: DelphiString);
begin

end;

method TMemIniFile.EraseSection(Section: DelphiString);
begin

end;

method TMemIniFile.GeTStrings(List: TStrings);
begin

end;

method TMemIniFile.ReadSection(Section: DelphiString; DelphiStrings: TStrings);
begin

end;

method TMemIniFile.ReadSections(DelphiStrings: TStrings);
begin

end;

method TMemIniFile.ReadSectionValues(Section: DelphiString; DelphiStrings: TStrings);
begin

end;

method TMemIniFile.ReadDelphiString(Section: DelphiString; Ident: DelphiString; &Default: DelphiString): DelphiString;
begin

end;

method TMemIniFile.Rename(FileName: DelphiString; Reload: Boolean);
begin

end;

method TMemIniFile.SetStrings(List: TStrings);
begin

end;

method TMemIniFile.UpdateFile;
begin

end;

method TMemIniFile.WriteString(Section: DelphiString; Ident: DelphiString; Value: DelphiString);
begin

end;

method TMemIniFile.LoadIni;
begin
  var lValues := new TStringList;
  lValues.LoadFromFile(FileName);
  var lStr: DelphiString;
  for i: Integer := 0 to lValues.Count - 1 do begin
    lStr := lValues[i].Trim;
    if (lStr.Length > 0) and (lStr.Chars[0] <> ';') then begin  // avoid comments and empty lines


    end;
  end;
end;

constructor TIniFile(FileName: DelphiString; Encoding: TEncoding; CaseSensitive: Boolean);
begin
  AutoSave := true;
end;

end.
