unit Form_Main;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_Launcher;

type
  TForm1 = class(TForm)
    btnLaunch: TButton;
    lbVersionStatus: TLabel;
    btnVersionCheck: TButton;
    procedure btnLaunchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVersionCheckClick(Sender: TObject);
  private
    fLauncher: TKMLauncher;

    procedure VersionCheck;
    procedure VersionCheckDone;
  end;


implementation

{$R *.dfm}

procedure TForm1.btnLaunchClick(Sender: TObject);
begin
  if fLauncher.IsGameExists then
  begin
    MessageBox(Handle, 'Game exe not found', 'Error', MB_ICONERROR + MB_OK);
    Exit;
  end;

  if fLauncher.IsGameRunning then
  begin
    MessageBox(Handle, 'Game is already running', 'Error', MB_ICONERROR + MB_OK);
    Exit;
  end;

  // Launch the game
  fLauncher.GameRun;

  // Close self
  Close;
end;


procedure TForm1.btnVersionCheckClick(Sender: TObject);
begin
  VersionCheck;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fLauncher := TKMLauncher.Create;

  VersionCheck;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fLauncher);
end;


procedure TForm1.VersionCheckDone;
begin
  case fLauncher.VersionState of
    vsUnknown:    lbVersionStatus.Caption := 'Unknown';
    vsActual:     lbVersionStatus.Caption := 'You have the latest game version';
    vsLocalOlder: lbVersionStatus.Caption := 'There is a newer version out!';
  end;

  btnVersionCheck.Enabled := True;
end;


procedure TForm1.VersionCheck;
begin
  btnVersionCheck.Enabled := False;
  fLauncher.VersionCheck(
    procedure (aText: string)
    begin
      lbVersionStatus.Caption := aText;

    end,
    VersionCheckDone
  );
end;


end.
