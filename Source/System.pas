namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

type
  THandle = Int64; // Since native types are not supported on Java, THandle stores a 64 bit integer always
  TArray<T> = array of T;
  TDateTime = public Double;
  TDate = public TDateTime;
  TTime = public TDateTime;


function Pos(SubStr: DelphiString; S: DelphiString; aOffset: Integer := 1): Integer;
procedure Insert(aSource: DelphiString; var aTarget: DelphiString; aOffset: Integer);
procedure Delete(var S: DelphiString; aIndex: Integer; aCount: Integer);
function &Copy(S: DelphiString; aIndex: Integer; aCount: Integer): DelphiString;

implementation

function Pos(SubStr: DelphiString; S: DelphiString; aOffset: Integer := 1): Integer;
begin
  result := S.IndexOf(SubStr, aOffset) + 1;
end;

procedure Insert(aSource: DelphiString; var aTarget: DelphiString; aOffset: Integer);
begin
  aTarget.Insert(aOffset - 1, aSource);
end;

procedure Delete(var S: DelphiString; aIndex: Integer; aCount: Integer);
begin
  S.Remove(aIndex - 1, aCount);
end;

function &Copy(S: DelphiString; aIndex: Integer; aCount: Integer): DelphiString;
begin
  result := S.SubString(aIndex - 1, aCount);
end;

end.