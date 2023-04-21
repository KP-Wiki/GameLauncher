unit KM_GameVersion;
interface
uses
  Classes, SysUtils, RegularExpressions;


type
  TKMGameBranch = (gbUnknown, gbStable, gbUnstable);

  TKMGameVersion = record
  public const
    VERSION_FILENAME = 'version';
  private
    function GetVersionRevisionString: string;
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
  if ContainsText(aString, TKMSettings.VERSION_BRANCH_UNSTABLE) then
    Result.Branch := gbUnstable
  else
  if (TKMSettings.VERSION_BRANCH_STABLE = '') or ContainsText(aString, TKMSettings.VERSION_BRANCH_STABLE) then
    Result.Branch := gbStable
  else
    Result.Branch := gbUnknown;
end;


class function TKMGameVersion.NewFromPath(const aPath: string): TKMGameVersion;
var
  sl: TStringList;
begin
  if not FileExists(aPath + TKMGameVersion.VERSION_FILENAME) then Exit(default(TKMGameVersion));

  sl := TStringList.Create;
  sl.LoadFromFile(aPath + TKMGameVersion.VERSION_FILENAME);

  // Trim any trailing EOLs that TStringList might have added
  Result := NewFromString(Trim(sl.Text));

  sl.Free;
end;


function TKMGameVersion.GetVersionRevisionString: string;
begin
  if (VersionFrom = 0) then
    // Build
    Result := Format(TKMSettings.VERSION_REVISION_BUILD_NAME, [VersionTo])
  else
    // Patch
    Result := Format(TKMSettings.VERSION_REVISION_PATCH_NAME, [VersionFrom, VersionTo]);
end;


function TKMGameVersion.GetVersionString: string;
begin
  if (VersionTo = 0) or (Branch = gbUnknown) then
    Result := 'Unknown'
  else
    case Branch of
      gbUnknown:  Result := TKMSettings.VERSION_BRANCH_UNKNOWN + ' ' + GetVersionRevisionString;
      gbStable:   Result := TKMSettings.VERSION_BRANCH_STABLE + ' ' + GetVersionRevisionString;
      gbUnstable: Result := TKMSettings.VERSION_BRANCH_UNSTABLE + ' ' + GetVersionRevisionString;
    end;

  // Trim the result in case branch suffix is ampty string
  Result := Trim(Result);
end;


procedure TKMGameVersion.SaveToFile(const aPath: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := GetVersionString;
  sl.SaveToFile(aPath + TKMGameVersion.VERSION_FILENAME);
  sl.Free;
end;


end.
