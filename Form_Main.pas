unit Form_Main;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_Launcher, KM_RepositoryFileList;

type
  TForm1 = class(TForm)
    btnLaunch: TButton;
    lbVersionStatus: TLabel;
    btnVersionCheck: TButton;
    meLog: TMemo;
    lbVersionCurrent: TLabel;
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
  if not fLauncher.IsGameExists then
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
var
  I: Integer;
  rf: TKMRepositoryFile;
begin
  case fLauncher.PatchChain.GetChainType of
    pcNoUpdateNeeded:   lbVersionStatus.Caption := 'You have the latest game version';
    pcCanPatch:         lbVersionStatus.Caption := 'There is a newer version out! Patch available';
    pcNeedFullVersion:  lbVersionStatus.Caption := 'There is a newer version out! Need full version';
    pcUnknown:          lbVersionStatus.Caption := 'Unknown';
  end;

  btnVersionCheck.Enabled := True;

  meLog.Clear;
  for I := 0 to fLauncher.Repository.FileList.Count - 1 do
  begin
    rf := fLauncher.Repository.FileList[I];
    meLog.Lines.Append(Format('%d-%d (%s)', [rf.Version.VersionFrom, rf.Version.VersionTo, rf.Name]));
  end;
end;


procedure TForm1.VersionCheck;
begin
  lbVersionCurrent.Caption := Format('Current game version is %d', [fLauncher.GameVersionGet.VersionTo]);

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
