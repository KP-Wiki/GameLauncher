unit KM_GameVersion;
interface
uses
  Classes, SysUtils, RegularExpressions;


type
  TKMGameBranch = (gbUnknown, gbStable, gbUnstable);

  TKMGameVersion = record
  public const
    FILENAME = 'version';
  public
    VersionFrom: Integer; // 0 if it is a full package or an installed game
    VersionTo: Integer;
    Branch: TKMGameBranch;
    class function NewFromString(const aString: string): TKMGameVersion; static;
    class function NewFromPath(const aPath: string): TKMGameVersion; static;
    function GetVersionString: string;
    procedure SaveToFile(const aPath: string);
  end;


implementation
uses
  StrUtils, KM_Settings;


{ TKMGameVersion }
// Used on 2 occasions:
//  - bundle file name (returned by the server/repository)
//  - "version" file contents inside the build
// Hence, it needs to be both human readable? Not really
// Humans dont need to interact with patches, since they can not apply them without Launcher anyway
class function TKMGameVersion.NewFromString(const aString: string): TKMGameVersion;
var
  revs: TMatchCollection;
begin
  Result := default(TKMGameVersion);

  // There can be two revisions if this is a patch
  revs := TRegEx.Matches(aString, TKMSettings.VERSION_REVISION_REGEX);

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
  if ContainsText(aString, TKMSettings.VERSION_BRANCH_SUFFIX_UNSTABLE) then
    Result.Branch := gbUnstable
  else
  if ContainsText(aString, TKMSettings.VERSION_BRANCH_SUFFIX_STABLE) then
    Result.Branch := gbStable
  else
    Result.Branch := gbUnknown;
end;


class function TKMGameVersion.NewFromPath(const aPath: string): TKMGameVersion;
var
  sl: TStringList;
begin
  if not FileExists(aPath + TKMGameVersion.FILENAME) then Exit(default(TKMGameVersion));

  sl := TStringList.Create;
  sl.LoadFromFile(aPath + TKMGameVersion.FILENAME);

  Result := NewFromString(Trim(sl.Text));

  sl.Free;
end;


function TKMGameVersion.GetVersionString: string;
begin
  if (VersionTo = 0) or (Branch = gbUnknown) then
    Result := 'Unknown'
  else
  if (VersionFrom = 0) then
    // Build
    case Branch of
      gbUnknown:  Result := Format(TKMSettings.VERSION_NAME_UNKNOWN, [VersionTo]);
      gbStable:   Result := Format(TKMSettings.VERSION_NAME_STABLE, [VersionTo]);
      gbUnstable: Result := Format(TKMSettings.VERSION_NAME_UNSTABLE, [VersionTo]);
    end
  else
    // Patch
    case Branch of
      gbUnknown:  Result := Format(TKMSettings.PATCH_NAME_UNKNOWN, [VersionFrom, VersionTo]);
      gbStable:   Result := Format(TKMSettings.PATCH_NAME_STABLE, [VersionFrom, VersionTo]);
      gbUnstable: Result := Format(TKMSettings.PATCH_NAME_UNSTABLE, [VersionFrom, VersionTo]);
    end;
end;


procedure TKMGameVersion.SaveToFile(const aPath: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := GetVersionString;
  sl.SaveToFile(aPath + TKMGameVersion.FILENAME);
  sl.Free;
end;


end.
