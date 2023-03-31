unit KM_Launcher;
interface
uses
  Classes, SysUtils,
  KM_Repository;


type
  TKMVersionState = (
    vsUnknown,    // Error-state
    vsActual,     // We are up to date
    vsLocalOlder  // We have old version
  );

  TKMLauncher = class
  private
    fRepository: TKMRepository;

    fVersionState: TKMVersionState;
  public
    constructor Create;
    destructor Destroy; override;

    function IsGameExists: Boolean;
    function IsGameRunning: Boolean;
    class function IsLauncherRunning: Boolean;
    procedure GameRun;
    procedure VersionCheck(aOnProgress: TProc<string>; aOnDone: TProc);
    property VersionState: TKMVersionState read fVersionState;
  end;

implementation


{ TKMLauncher }
constructor TKMLauncher.Create;
begin
  inherited;

  fRepository := TKMRepository.Create(TKMRepository.DEFAULT_SERVER_ADDRESS, 'Launcher');
end;


destructor TKMLauncher.Destroy;
begin
  FreeAndNil(fRepository);

  inherited;
end;


procedure TKMLauncher.GameRun;
begin
  //todo: GameRun
end;


function TKMLauncher.IsGameExists: Boolean;
begin
  //todo: IsGameExists
  Result := False;
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
  fVersionState := vsUnknown;

  aOnProgress('Checking for latest version ..');

  fRepository.FileListGet(
    procedure
    begin
      aOnDone;
    end,
    procedure (aError: string)
    begin
      aOnDone;
    end)
    ;
end;


end.
