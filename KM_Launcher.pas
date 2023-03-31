unit KM_Launcher;
interface


type
  TKMLauncher = class
  private

  protected

  public
    constructor Create;
    destructor Destroy; override;

    function IsGameRunning: Boolean;
    class function IsLauncherRunning: Boolean;
    procedure GameRun;
  end;

implementation


{ TKMLauncher }
constructor TKMLauncher.Create;
begin
  inherited;

end;


destructor TKMLauncher.Destroy;
begin

  inherited;
end;


procedure TKMLauncher.GameRun;
begin
  //todo: GameRun
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


end.
