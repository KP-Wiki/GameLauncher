unit KM_GameVersion;
interface
uses
  System.Classes, System.SysUtils, System.RegularExpressions;


type
  TKMGameVersion = record
  public const
    VERSION_FILENAME = 'version';
  private
    function GetVersionRevisionString: string;
  public
    Epoch: Integer;
    Major: Integer;
    Minor: Integer;

    RevisionFrom: Integer; // 0 if it is a full package or an installed game
    RevisionTo: Integer;

    IsValid: Boolean; // Wherever this is a valid game version string

    class function NewFromString(const aString: string): TKMGameVersion; static;
    class function NewFromPath(const aPath: string): TKMGameVersion; static;
    function GetVersionString: string;
    procedure SaveToFile(const aPath: string);
  end;


implementation
uses
  System.StrUtils, KM_Settings;


{ TKMGameVersion }
// Used on 2 occasions:
//  - bundle file name (returned by the server/repository)
//  - "version" file contents inside the build
// Hence, it needs to be both human readable? Not really
// Humans dont need to interact with patches, since they can not apply them without Launcher anyway
class function TKMGameVersion.NewFromString(const aString: string): TKMGameVersion;
begin
  Result := default(TKMGameVersion);

  // Examples:
  //  - version file: Alpha 13.2 r17800
  //  - old build:    kp2026-02-07 (Alpha 13.2 r17915).7z
  //  - new build:    Knights Province Alpha 13.2.17915.7z
  //  - patch:        Knights Province Alpha wip r17534-r17541.zip
  // Should be skipped:
  //  - dedicated server:    KnightsProvince DedicatedServer r16347.7z

  // This check is not ideal
  if ContainsText(aString, 'Minimap') then Exit;
  if ContainsText(aString, 'Server') then Exit;

  // Patches dont have this part, it is safe to skip
  begin
    var regexMatchEpoch := TRegEx.Matches(aString, TKMSettings.VERSION_EPOCH_REGEX);
    if regexMatchEpoch.Count = 1 then
      Result.Epoch := StrToIntDef(regexMatchEpoch[0].Value, 0);

    var regexMatchMajor := TRegEx.Matches(aString, TKMSettings.VERSION_MAJOR_REGEX);
    if regexMatchMajor.Count = 1 then
      Result.Major := StrToIntDef(regexMatchMajor[0].Value, 0);

    var regexMatchMinor := TRegEx.Matches(aString, TKMSettings.VERSION_MINOR_REGEX);
    if regexMatchMinor.Count = 1 then
      Result.Minor := StrToIntDef(regexMatchMinor[0].Value, 0);
  end;

  var regexMatchRevision := TRegEx.Matches(aString, TKMSettings.VERSION_REVISION_REGEX);
  case regexMatchRevision.Count of
    1:  Result.RevisionTo := StrToIntDef(regexMatchRevision[0].Value, 0);
    2:  begin
          // There can be two revisions if this is a patch
          Result.RevisionFrom := StrToIntDef(regexMatchRevision[0].Value, 0);
          Result.RevisionTo := StrToIntDef(regexMatchRevision[1].Value, 0);
        end;
  else
    // This is not a bundle, both RevisionFrom/RevisionTo will remain at 0
    Exit;
  end;

  Result.IsValid := True;
end;


class function TKMGameVersion.NewFromPath(const aPath: string): TKMGameVersion;
begin
  if not FileExists(aPath + TKMGameVersion.VERSION_FILENAME) then Exit(default(TKMGameVersion));

  var sl := TStringList.Create;
  sl.LoadFromFile(aPath + TKMGameVersion.VERSION_FILENAME);

  // Trim any trailing EOLs that TStringList might have added
  Result := NewFromString(Trim(sl.Text));

  sl.Free;
end;


function TKMGameVersion.GetVersionRevisionString: string;
begin
  if (RevisionFrom = 0) then
    // Build
    Result := Format(TKMSettings.VERSION_REVISION_BUILD_PATTERN, [RevisionTo])
  else
    // Patch
    Result := Format(TKMSettings.VERSION_REVISION_PATCH_PATTERN, [RevisionFrom, RevisionTo]);
end;


function TKMGameVersion.GetVersionString: string;
begin
  if not IsValid then
    Result := 'Unknown'
  else
    Result := GetVersionRevisionString;

  Result := Trim(Result);
end;


procedure TKMGameVersion.SaveToFile(const aPath: string);
begin
  var sl := TStringList.Create;
  sl.Text := GetVersionString;
  sl.SaveToFile(aPath + TKMGameVersion.VERSION_FILENAME);
  sl.Free;
end;


end.
