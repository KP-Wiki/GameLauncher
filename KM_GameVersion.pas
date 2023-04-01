unit KM_GameVersion;
interface
uses
  Classes, SysUtils, RegularExpressions;


type
  TKMGameBranch = (gbUnknown, gbStable, gbBeta);

  TKMGameVersion = record
    VersionFrom: Integer; // 0 if it is a full package or an installed game
    VersionTo: Integer;
    Branch: TKMGameBranch;
    class function NewFromName(const aName: string): TKMGameVersion; static;
    class function NewFromGameFolder(const aPath: string): TKMGameVersion; static;
  end;


implementation
uses
  StrUtils, KM_Settings;


{ TKMGameVersion }
class function TKMGameVersion.NewFromName(const aName: string): TKMGameVersion;
var
  revs: TMatchCollection;
begin
  Result := default(TKMGameVersion);

  // RegEx to find "r123456" and trim "r"
  revs := TRegEx.Matches(aName, 'r\K\d+');

  if revs.Count = 1 then
    Result.VersionTo := StrToIntDef(revs[0].Value, 0)
  else if revs.Count = 2 then
  begin
    Result.VersionFrom := StrToIntDef(revs[0].Value, 0);
    Result.VersionTo := StrToIntDef(revs[1].Value, 0);
  end;

  // Beta marker is often a suffix, (e.g. "Alpha 12" vs "Alpha 12 wip"), check fo it first
  if ContainsText(aName, TKMSettings.VERSION_BETA) then
    Result.Branch := gbBeta
  else
  if ContainsText(aName, TKMSettings.VERSION_STABLE) then
    Result.Branch := gbStable
  else
    Result.Branch := gbUnknown;
end;


class function TKMGameVersion.NewFromGameFolder(const aPath: string): TKMGameVersion;
begin
  Result := default(TKMGameVersion);

  //todo: TKMGameVersion.NewFromGameFolder
end;


end.
