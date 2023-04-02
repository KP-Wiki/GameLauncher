unit KM_Launcher;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_Repository, KM_RepositoryFileList, KM_Patcher;


type
  TKMLauncher = class
  private class var
    fMutexApp: NativeUInt;
  private
    fRepository: TKMRepository;
    fPatchChain: TKMPatchChain;
    fPatcher: TKMPatcher;

    fGamePath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function IsGameExists: Boolean;
    class function TryLauncherInstanceLock: Boolean;
    class procedure LauncherInstanceUnlock;
    procedure GameRun;
    function GameVersionGet: TKMGameVersion;
    procedure VersionCheck(aOnProgress: TProc<string>; aOnDone: TProc);
    property Repository: TKMRepository read fRepository;
    property PatchChain: TKMPatchChain read fPatchChain;
    procedure UpdateGame(aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc);
  end;


implementation
uses
  Windows, ShellAPI,
  KM_Mutex, KM_Settings;


{ TKMLauncher }
constructor TKMLauncher.Create;
begin
  inherited;

  fRepository := TKMRepository.Create(TKMRepository.DEFAULT_SERVER_ADDRESS, 'Launcher');
  fPatchChain := TKMPatchChain.Create;

  fGamePath := ExpandFileName('.\');
end;


destructor TKMLauncher.Destroy;
begin
  FreeAndNil(fPatchChain);
  FreeAndNil(fRepository);

  inherited;
end;


procedure TKMLauncher.GameRun;
var
  shi: TShellExecuteInfo;
begin
  shi := Default(TShellExecuteInfo);
  shi.cbSize := SizeOf(TShellExecuteInfo);
  shi.lpFile := PChar(fGamePath + TKMSettings.GAME_EXE_NAME);
  shi.nShow := SW_SHOWNORMAL;

  ShellExecuteEx(@shi);
end;


function TKMLauncher.GameVersionGet: TKMGameVersion;
begin
  Result := TKMGameVersion.NewFromGameFolder('.\');
end;


function TKMLauncher.IsGameExists: Boolean;
begin
  Result := FileExists(fGamePath + TKMSettings.GAME_EXE_NAME);
end;


class function TKMLauncher.TryLauncherInstanceLock: Boolean;
begin
  // Pass application path, cos we could allow 2 updaters in 2 different folders - thats no big deal
  Result := SingleInstanceLock(ParamStr(0), fMutexApp);
end;


class procedure TKMLauncher.LauncherInstanceUnlock;
begin
  SingleInstanceUnlock(fMutexApp);
end;


procedure TKMLauncher.VersionCheck(aOnProgress: TProc<string>; aOnDone: TProc);
begin
  aOnProgress('Checking for available versions ..');

  fRepository.FileListGet(
    procedure
    begin
      aOnProgress('Successfully acquired list of versions from the server');
      fPatchChain.TryToAssemble(GameVersionGet.Branch, GameVersionGet.VersionTo, fRepository.FileList);

      aOnDone;
    end,
    procedure (aError: string)
    begin
      aOnProgress('Error occured: ' + aError);
      aOnDone;
    end);
end;


procedure TKMLauncher.UpdateGame(aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc);
begin
  Assert(fPatchChain.ChainType = pcCanPatch);

  fPatcher := TKMPatcher.Create(fGamePath, fRepository, fPatchChain, aOnProgress, aOnDone, aOnFail);
end;


end.
