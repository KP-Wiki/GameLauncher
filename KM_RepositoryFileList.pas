unit KM_RepositoryFileList;
interface
uses
  Classes, SysUtils, Generics.Collections,
  REST.Client, IPPeerClient, REST.Types, REST.Utils, REST.Exception,
  JsonDataObjects;


type
  TKMRepositoryFile = record
  {
    "name": "KnightsProvince_r12853fix.7z",
    "timestamp": 1680239114,
    "datetime": "2023-03-31 05:05:14 +0000",
    "size": 9658914,
    "is_dev": 1,
    "url": "https://release.knightsprovince.com/index.php/apps/kpautomation/api/1.0/download?file=KnightsProvince_r12853fix.7z&client=kp&is_dev=1"
  }
  public
    Name: string;
    //DateTime: TDateTime;
    Size: Integer;
    Url: string;
    class function NewFromJson(aJson: TJsonObject): TKMRepositoryFile; static;
  end;

  TKMRepositoryFileList = class
  private
    fList: TList<TKMRepositoryFile>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromJsonString(const aJson: string);
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


end.
