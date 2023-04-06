unit KM_GameVersion;
interface
uses
  Classes, SysUtils, RegularExpressions;


type
  TKMGameBranch = (gbUnknown, gbStable, gbUnstable);

  TKMGameVersion = record
  public
    VersionFrom: Integer; // 0 if it is a full package or an installed game
    VersionTo: Integer;
    Branch: TKMGameBranch;
    class function NewFromName(const aName: string): TKMGameVersion; static;
    class function NewFromGameFolder(const aPath: string): TKMGameVersion; static;
    function GetVersionString: string;
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

  // There can be two revisions if this is a patch
  revs := TRegEx.Matches(aName, TKMSettings.VERSION_REVISION_REGEX);

  case revs.Count of
    1:  Result.VersionTo := StrToIntDef(revs[0].Value, 0);
    2:  begin
          Result.VersionFrom := StrToIntDef(revs[0].Value, 0);
          Result.VersionTo := StrToIntDef(revs[1].Value, 0);
        end;
  else
    // This is not a bundle, both VersionFrom/VersionTo will remain at 0
    Exit;
  end;


  // Beta marker is often a suffix, (e.g. "Alpha 12" vs "Alpha 12 wip"), check for it first
  if ContainsText(aName, TKMSettings.VERSION_BRANCH_SUFFIX_UNSTABLE) then
    Result.Branch := gbUnstable
  else
  if ContainsText(aName, TKMSettings.VERSION_BRANCH_SUFFIX_STABLE) then
    Result.Branch := gbStable
  else
    Result.Branch := gbUnknown;
end;


class function TKMGameVersion.NewFromGameFolder(const aPath: string): TKMGameVersion;
var
  sl: TStringList;
begin
  if not FileExists(aPath + 'version') then Exit(TKMGameVersion.NewFromName(''));

  sl := TStringList.Create;
  sl.LoadFromFile(aPath + 'version');

  Result := NewFromName(Trim(sl.Text));

  sl.Free;
end;


function TKMGameVersion.GetVersionString: string;
begin
  if (VersionTo = 0) or (Branch = gbUnknown) then
    Result := 'Unknown'
  else
  case Branch of
    gbUnknown:  Result := Format(TKMSettings.VERSION_NAME_UNKNOWN, [VersionTo]);
    gbStable:   Result := Format(TKMSettings.VERSION_NAME_STABLE, [VersionTo]);
    gbUnstable: Result := Format(TKMSettings.VERSION_NAME_UNSTABLE, [VersionTo]);
  end;
end;


end.
