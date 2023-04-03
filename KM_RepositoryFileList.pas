unit KM_RepositoryFileList;
interface
uses
  Classes, SysUtils, Generics.Collections,
  REST.Client, IPPeerClient, REST.Types, REST.Utils,
  JsonDataObjects,
  KM_GameVersion;


type
  TKMRepositoryFile = class
  {
    "name": "kp2023-03-28 (Alpha 12 wip r12832).7z",
    "timestamp": 1679997564,
    "datetime": "2023-03-28 09:59:24 +0000",
    "size": 267145217,
    "is_dev": 1,
    "url": "https:\/\/release.knightsprovince.com\/index.php\/apps\/kpautomation\/api\/1.0\/download?client=kp&file=kp2023-03-28%20%28Alpha%2012%20wip%20r12832%29.7z&is_dev=1"
  }
  {
    "name": "KaM Remake Beta r14765.exe",
    "timestamp": 1653329166,
    "datetime": "2022-05-23 18:06:06 +0000",
    "size": 595277701,
    "is_dev": 1,
    "url": "https:\/\/release.knightsprovince.com\/index.php\/apps\/kpautomation\/api\/1.0\/download?file=KaM%20Remake%20Beta%20r14765.exe&client=kmr&is_dev=1"
  }
  public
    Name: string;
    //DateTime: TDateTime;
    Size: Integer;
    Url: string;
    Version: TKMGameVersion;
    constructor CreateFromJson(aJson: TJsonObject);
  end;

  TKMRepositoryFileList = class
  private
    fList: TObjectList<TKMRepositoryFile>;
    function GetCount: Integer;
    function GetItem(aIndex: Integer): TKMRepositoryFile;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJsonString(const aJson: string);
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMRepositoryFile read GetItem; default;
    function FindLatestVersion(aBranch: TKMGameBranch): TKMRepositoryFile;
  end;

  TKMPatchChainType = (
    pcNoUpdateNeeded,
    pcCanPatch,
    pcNeedFullVersion,
    pcUnknown
  );

  // Chain of patches to get to the target game version
  TKMPatchChain = class(TList<TKMRepositoryFile>)
  private
    fVersionFrom: Integer;
    fChainType: TKMPatchChainType;
  public
    procedure TryToAssemble(aBranch: TKMGameBranch; aVersionFrom: Integer; aFileList: TKMRepositoryFileList);
    property ChainType: TKMPatchChainType read fChainType;
  end;


implementation
uses
  StrUtils;


{ TKMRepositoryFile }
constructor TKMRepositoryFile.CreateFromJson(aJson: TJsonObject);
begin
  inherited Create;

  Name := aJson.S['name'];
  //DateTime := aJson.S['datetime'];
  Size := aJson.I['size'];
  Url := aJson.S['url'];

  Version := TKMGameVersion.NewFromName(ChangeFileExt(Name, ''));
end;


{ TKMRepositoryFileList }
constructor TKMRepositoryFileList.Create;
begin
  inherited;

  fList := TObjectList<TKMRepositoryFile>.Create;
end;


destructor TKMRepositoryFileList.Destroy;
begin
  FreeAndNil(fList);

  inherited;
end;


function TKMRepositoryFileList.GetCount: Integer;
begin
  Result := fList.Count;
end;


function TKMRepositoryFileList.GetItem(aIndex: Integer): TKMRepositoryFile;
begin
  Result := fList[aIndex];
end;


procedure TKMRepositoryFileList.LoadFromJsonString(const aJson: string);
var
  ja: TJsonArray;
  I: Integer;
  rf: TKMRepositoryFile;
begin
  fList.Clear;

  ja := TJsonArray.Create;
  ja.FromJSON(aJson);

  for I := 0 to ja.Count - 1 do
  begin
    rf := TKMRepositoryFile.CreateFromJson(ja[I]);
    fList.Add(rf);
  end;

  {  rf := TKMRepositoryFile.Create;
    rf.Version.VersionFrom := 12858;
    rf.Version.VersionTo := 12866;
    rf.Version.Branch := gbBeta;
    fList.Add(rf);
  }

  ja.Free;
end;


function TKMRepositoryFileList.FindLatestVersion(aBranch: TKMGameBranch): TKMRepositoryFile;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to fList.Count - 1 do
  if fList[I].Version.Branch = aBranch then
    if (Result = nil) or (fList[I].Version.VersionTo > Result.Version.VersionTo) then
      Result := fList[I];
end;


{ TKMPatchChain }
procedure TKMPatchChain.TryToAssemble(aBranch: TKMGameBranch; aVersionFrom: Integer; aFileList: TKMRepositoryFileList);
  procedure FindNextLink(aFrom: Integer);
  var
    I: Integer;
  begin
    // KISS, assume there's a single chain, not a graph of variants
    for I := 0 to aFileList.Count - 1 do
    if aFileList[I].Version.VersionFrom = aFrom then
    begin
      Add(aFileList[I]);
      FindNextLink(aFileList[I].Version.VersionTo);
      Break;
    end;
  end;
var
  versionTo: TKMRepositoryFile;
begin
  Clear;

  fChainType := pcUnknown;
  fVersionFrom := aVersionFrom;
  versionTo := aFileList.FindLatestVersion(aBranch);

  if versionTo.Version.VersionTo = fVersionFrom then
  begin
    // We have the latest version
    fChainType := pcNoUpdateNeeded;
    Exit;
  end;
  if versionTo.Version.VersionTo < fVersionFrom then
  begin
    // Our version is newer than in the repo
    fChainType := pcNoUpdateNeeded;
    Exit;
  end;
  if versionTo = nil then
  begin
    // There's no link to any version on this Branch
    fChainType := pcUnknown;
    Exit;
  end;

  // Try to build a chain (Building bottom-up should be faster in case there's no chain)
  FindNextLink(fVersionFrom);

  if Last.Version.VersionTo <> versionTo.Version.VersionTo then
  begin
    // There is a full newer version
    fChainType := pcNeedFullVersion;
    Exit;
  end else
  begin
    // There is a chain of patches we can apply
    fChainType := pcCanPatch;
    Exit;
  end;
end;


end.
