unit KM_Launcher;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_Repository, KM_RepositoryFileList;


type
  TKMLauncher = class
  private class var
    fMutexApp: NativeUInt;
  private
    fRepository: TKMRepository;
    fPatchChain: TKMPatchChain;

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

  fGamePath := ExpandFileName(TKMSettings.GAME_EXE_NAME);
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
  shi.lpFile := PChar(fGamePath);
  shi.nShow := SW_SHOWNORMAL;

  ShellExecuteEx(@shi);
end;


function TKMLauncher.GameVersionGet: TKMGameVersion;
begin
  Result := TKMGameVersion.NewFromGameFolder('.\');
end;


function TKMLauncher.IsGameExists: Boolean;
begin
  Result := FileExists(fGamePath);
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
