unit KM_RepositoryFileList;
interface
uses
  Classes, SysUtils, Generics.Collections,
  REST.Client, IPPeerClient, REST.Types, REST.Utils,
  JsonDataObjects,
  KM_GameVersion;


type
  TKMRepositoryFile = record
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
    class function NewFromJson(aJson: TJsonObject): TKMRepositoryFile; static;
  end;


  // Chain of patches to get to the target game version
  TKMPatchChainType = (
    pcNoUpdateNeeded,
    pcCanPatch,
    pcNeedFullVersion,
    pcUnknown
  );

  TKMRepositoryFileList = class
  private
    fList: TList<TKMRepositoryFile>;
    function GetCount: Integer;
    function GetItem(aIndex: Integer): TKMRepositoryFile;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJsonString(const aJson: string);
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMRepositoryFile read GetItem; default;
  end;


  TKMPatchChain = class(TList<TKMRepositoryFile>)
  public
    VersionFrom: Integer;
    procedure TryToAssemble(aBranch: TKMGameBranch; aVersionFrom: Integer; aFileList: TKMRepositoryFileList);
    function GetChainType: TKMPatchChainType;
  end;


implementation
uses
  StrUtils;


{ TKMRepositoryFile }
class function TKMRepositoryFile.NewFromJson(aJson: TJsonObject): TKMRepositoryFile;
begin
  Result := default(TKMRepositoryFile);

  Result.Name := aJson.S['name'];
  //Result.DateTime := aJson.S['datetime'];
  Result.Size := aJson.I['size'];
  Result.Url := aJson.S['url'];

  Result.Version := TKMGameVersion.NewFromName(Result.Name);
end;


{ TKMRepositoryFileList }
constructor TKMRepositoryFileList.Create;
begin
  inherited;

  fList := TList<TKMRepositoryFile>.Create;
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
    rf := TKMRepositoryFile.NewFromJson(ja[I]);
    fList.Add(rf);
  end;

  ja.Free;
end;


{ TKMPatchChain }
procedure TKMPatchChain.TryToAssemble(aBranch: TKMGameBranch; aVersionFrom: Integer; aFileList: TKMRepositoryFileList);
begin
  Clear;

  VersionFrom := aVersionFrom;

  //todo: TKMPatchChain.TryToAssemble

end;


function TKMPatchChain.GetChainType: TKMPatchChainType;
begin
  if Count = 0 then Exit(pcUnknown);

  if Last.Version.VersionTo = VersionFrom then
    // We have the latest version
    Result := pcNoUpdateNeeded
  else
  if Last.Version.VersionTo > VersionFrom then
    if (First.Version.VersionFrom = VersionFrom) then
      // There is a chain of patches we can apply
      Result := pcCanPatch
    else
      // There is a full newer version
      Result := pcNeedFullVersion
  else
    // Unknown version
    Result := pcUnknown;
end;


end.
