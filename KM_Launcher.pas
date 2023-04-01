unit KM_Launcher;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_Repository, KM_RepositoryFileList;


type
  TKMLauncher = class
  private
    fRepository: TKMRepository;
    fPatchChain: TKMPatchChain;

  public
    constructor Create;
    destructor Destroy; override;

    function IsGameExists: Boolean;
    function IsGameRunning: Boolean;
    class function IsLauncherRunning: Boolean;
    procedure GameRun;
    function GameVersionGet: TKMGameVersion;
    procedure VersionCheck(aOnProgress: TProc<string>; aOnDone: TProc);
    property Repository: TKMRepository read fRepository;
    property PatchChain: TKMPatchChain read fPatchChain;
  end;


implementation
uses
  KM_Settings;


{ TKMLauncher }
constructor TKMLauncher.Create;
begin
  inherited;

  fRepository := TKMRepository.Create(TKMRepository.DEFAULT_SERVER_ADDRESS, 'Launcher');
  fPatchChain := TKMPatchChain.Create;
end;


destructor TKMLauncher.Destroy;
begin
  FreeAndNil(fPatchChain);
  FreeAndNil(fRepository);

  inherited;
end;


procedure TKMLauncher.GameRun;
begin
  //todo: GameRun
end;


function TKMLauncher.GameVersionGet: TKMGameVersion;
begin
  Result := TKMGameVersion.NewFromGameFolder('.\');
end;


function TKMLauncher.IsGameExists: Boolean;
begin
  Result := FileExists(TKMSettings.GAME_EXE_NAME);
end;


function TKMLauncher.IsGameRunning: Boolean;
begin
  //todo: IsGameRunning
  Result := False;
end;


class function TKMLauncher.IsLauncherRunning: Boolean;
begin
  //todo: IsLauncherRunning
  Result := False;
end;


procedure TKMLauncher.VersionCheck(aOnProgress: TProc<string>; aOnDone: TProc);
begin
  aOnProgress('Checking for latest version ..');

  fRepository.FileListGet(
    procedure
    begin
      fPatchChain.TryToAssemble(GameVersionGet.Branch, GameVersionGet.VersionTo, fRepository.FileList);

      aOnDone;
    end,
    procedure (aError: string)
    begin
      aOnDone;
    end)
    ;
end;


end.
