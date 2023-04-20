unit Form_Main;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.ComCtrls,
  KM_Patchmaker, KM_Launcher;

type
  TForm1 = class(TForm)
    btnLaunch: TButton;
    btnVersionCheck: TButton;
    meLog: TMemo;
    Image1: TImage;
    btnUpdate: TButton;
    pbProgress: TProgressBar;
    procedure btnLaunchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVersionCheckClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  private
    fPatchmaker: TKMPatchmaker;
    fLauncher: TKMLauncher;

    procedure HandleLog(aText: string);
    procedure InitPatchmaker(const aLatestBuild: string);
    procedure InitLauncher;
    procedure VersionCheck;
    procedure VersionCheckDone;
  end;


implementation
uses
  Math,
  KM_Bundles,
  KM_Settings;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamStr(1) = '' then
    InitLauncher
  else
    InitPatchmaker(ParamStr(1));
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fLauncher);
end;


procedure TForm1.HandleLog(aText: string);
var
  sl: TStringList;
begin
  meLog.Lines.Append(aText);

  sl := TStringList.Create;
  sl.LoadFromFile(ExtractFilePath(Application.ExeName) + 'launcher.log');
  sl.Text := sl.Text + aText + sLineBreak;
  sl.SaveToFile(ExtractFilePath(Application.ExeName) + 'launcher.log');
  sl.Free;
end;


procedure TForm1.InitPatchmaker(const aLatestBuild: string);
begin
  Caption := TKMSettings.GAME_NAME + ' patchmaker';

  // We dont need any of those to create a patch
  Image1.Free;
  btnLaunch.Free;
  btnVersionCheck.Free;
  btnUpdate.Free;
  pbProgress.Free;

  meLog.Top := ScaleValue(16);
  meLog.Height := ClientHeight - ScaleValue(32);

  meLog.Clear;

  try
    fPatchmaker := TKMPatchmaker.Create(HandleLog, aLatestBuild);
  except
    // App will remain opened with the error in the log
    on E: Exception do
      HandleLog(E.Message);
  end;
end;


procedure TForm1.InitLauncher;
begin
  Caption := TKMSettings.GAME_NAME + ' launcher';
  fLauncher := TKMLauncher.Create;
  VersionCheck;
end;


procedure TForm1.btnLaunchClick(Sender: TObject);
begin
  if not fLauncher.IsGameExists then
  begin
    MessageBox(Handle, 'Game exe not found', 'Error', MB_ICONERROR + MB_OK);
    Exit;
  end;

  // Sometimes it is useful to run 2 games at once (e.g. MapEd)
  // Other times it should be blocked (for MP), but the game will take care of that itself

  // Launch the game
  fLauncher.GameRun;

  // Close self
  Close;
end;


procedure TForm1.btnUpdateClick(Sender: TObject);
begin
  btnVersionCheck.Enabled := False;
  btnUpdate.Enabled := False;
  btnLaunch.Enabled := False;

  pbProgress.BarColor := clGreen;
  pbProgress.Position := 0;
  fLauncher.UpdateGame(
    procedure (aCaption: string; aProgress: Single)
    begin
      HandleLog(aCaption);
      pbProgress.Position := Round(aProgress * pbProgress.Max);
    end,
    procedure
    begin
      btnVersionCheck.Enabled := True;
      btnUpdate.Enabled := True;
      btnLaunch.Enabled := True;
    end,
    procedure (aError: string)
    begin
      HandleLog(aError);
      pbProgress.BarColor := clRed;
      btnVersionCheck.Enabled := True;
      btnUpdate.Enabled := True;
      btnLaunch.Enabled := True;
    end
  );
end;


procedure TForm1.btnVersionCheckClick(Sender: TObject);
begin
  VersionCheck;
end;


procedure TForm1.VersionCheckDone;
var
  I: Integer;
  bundle: TKMBundle;
begin
  case fLauncher.PatchChain.ChainType of
    pcNoUpdateNeeded:   HandleLog('You have the latest game version');
    pcCanPatch:         begin
                          HandleLog('There is a newer version out! Patch available:');

                          for I := 0 to fLauncher.PatchChain.Count - 1 do
                          begin
                            bundle := fLauncher.PatchChain[I];
                            HandleLog(Format('%d -> %d (%dmb)', [bundle.Version.VersionFrom, bundle.Version.VersionTo, Ceil(bundle.Size / 1024 / 1024)]));
                          end;
                          btnUpdate.Enabled := True;
                        end;
    pcNeedFullVersion:  HandleLog(Format('There is a newer version out (%s)! Need full version download', [fLauncher.PatchChain.Last.Name]));
    pcUnknownVersion:   HandleLog('There is no information on the server about your game version.'+sLineBreak+'You may need to download full version from site or Discord');
    pcUnknown:          HandleLog('Status unknown');
  end;

  btnVersionCheck.Enabled := True;
end;


procedure TForm1.VersionCheck;
begin
  btnUpdate.Enabled := False;

  meLog.Clear;

  btnVersionCheck.Enabled := False;
  fLauncher.VersionCheck(
    HandleLog,
    VersionCheckDone
  );
end;


end.
