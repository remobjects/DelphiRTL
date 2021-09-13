﻿namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TPersistent = public class(TObject)
  private
    method AssignError(Source: TPersistent);
  protected
    method AssignTo(Dest: TPersistent); virtual;
    method GetOwner: TPersistent; virtual;
  public
    method DefineProperties(Filer: TObject {TFiler}); virtual;
    method Assign(Source: TPersistent); virtual;
    method GetNamePath: DelphiString; virtual;
  end;

implementation

method TPersistent.AssignError(Source: TPersistent);
begin
  raise new Exception('Can not assign');
end;

method TPersistent.AssignTo(Dest: TPersistent);
begin
  Dest.AssignError(Self);
end;

method TPersistent.DefineProperties(Filer: TObject {TFiler});
begin
  // do nothing here
end;

method TPersistent.GetOwner: TPersistent;
begin
  result := nil;
end;

method TPersistent.Assign(Source: TPersistent);
begin
  if Source <> nil then
    Source.AssignTo(Self)
  else
    AssignError(nil);
end;

method TPersistent.GetNamePath: DelphiString;
begin

end;

end.