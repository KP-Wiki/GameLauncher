unit KM_Launcher;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_ServerAPI, KM_Bundles, KM_Patcher;


type
  TKMLauncher = class
  private class var
    fMutexApp: NativeUInt;
  private
    fServerAPI: TKMServerAPI;
    fPatchChain: TKMPatchChain;
    fPatcher: TKMPatcher;
    fBundles: TKMBundles;

    fRootPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function IsGameExists: Boolean;
    class function TryLauncherInstanceLock: Boolean;
    class procedure LauncherInstanceUnlock;
    procedure GameRun;
    function GameVersionGet: TKMGameVersion;
    procedure VersionCheck(aOnProgress: TProc<string>; aOnDone: TProc);
    property ServerAPI: TKMServerAPI read fServerAPI;
    property PatchChain: TKMPatchChain read fPatchChain;
    procedure UpdateGame(aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc<string>);
  end;


implementation
uses
  Windows, ShellAPI,
  KM_Mutex, KM_Settings;


{ TKMLauncher }
constructor TKMLauncher.Create;
begin
  inherited;

  fServerAPI := TKMServerAPI.Create(TKMSettings.SERVER_ADDRESS, 'Launcher');
  fBundles := TKMBundles.Create;
  fPatchChain := TKMPatchChain.Create;

  fRootPath := ExpandFileName('.\');
end;


destructor TKMLauncher.Destroy;
begin
  FreeAndNil(fPatchChain);
  FreeAndNil(fBundles);
  FreeAndNil(fServerAPI);

  inherited;
end;


procedure TKMLauncher.GameRun;
var
  shi: TShellExecuteInfo;
begin
  shi := Default(TShellExecuteInfo);
  shi.cbSize := SizeOf(TShellExecuteInfo);
  shi.lpFile := PChar(fRootPath + TKMSettings.GAME_EXE_NAME);
  shi.nShow := SW_SHOWNORMAL;

  ShellExecuteEx(@shi);
end;


function TKMLauncher.GameVersionGet: TKMGameVersion;
begin
  //todo: Look into getting (and setting) game version from main exe (https://ru.stackoverflow.com/questions/880727)
  Result := TKMGameVersion.NewFromPath(fRootPath);
end;


function TKMLauncher.IsGameExists: Boolean;
begin
  Result := FileExists(fRootPath + TKMSettings.GAME_EXE_NAME);
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
  if GameVersionGet.Branch = gbUnknown then
  begin
    fPatchChain.TryToAssemble(GameVersionGet.Branch, GameVersionGet.VersionTo, nil);
    aOnDone;
  end else
  begin
    aOnProgress('Checking for available versions ..');

    fServerAPI.FileListGet(
      procedure (aData: string)
      begin
        fBundles.LoadFromJsonString(aData);

        fBundles.AppendFromLocal(fRootPath);

        aOnProgress('Successfully acquired list of versions from the server');
        fPatchChain.TryToAssemble(GameVersionGet.Branch, GameVersionGet.VersionTo, fBundles);
        aOnDone;
      end,
      procedure (aError: string)
      begin
        aOnProgress('Update server error: ' + aError);
        fPatchChain.TryToAssemble(GameVersionGet.Branch, GameVersionGet.VersionTo, nil);
        aOnDone;
      end);
  end;
end;


procedure TKMLauncher.UpdateGame(aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc<string>);
begin
  Assert(fPatchChain.ChainType = pcCanPatch);

  fPatcher := TKMPatcher.Create(fRootPath, fServerAPI, fPatchChain, aOnProgress, aOnDone, aOnFail);
end;


end.
