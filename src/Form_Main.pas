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
    lbVersion: TLabel;
    procedure btnLaunchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVersionCheckClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  private
    fPatchmaker: TKMPatchmaker;
    fLauncher: TKMLauncher;

    procedure HandleLog(aText: string);
    procedure SaveLog(aText: string);
    procedure InitPatchmaker(const aLatestBuild: string);
    procedure InitLauncher;
    procedure VersionCheck;
    procedure ClearLog;
  end;


implementation
uses
  Math, StrUtils,
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
  FreeAndNil(fPatchmaker);
end;


procedure TForm1.HandleLog(aText: string);
begin
  meLog.Lines.Append(aText);

  if fPatchmaker <> nil then
    SaveLog(aText);
end;


procedure TForm1.SaveLog(aText: string);
var
  sl: TStringList;
  fname: string;
begin
  fname := ExtractFilePath(Application.ExeName) + 'Launcher.log';

  sl := TStringList.Create;
  try
    if FileExists(fname) then
      sl.LoadFromFile(fname);
    sl.Text := sl.Text + aText + sLineBreak;
    sl.SaveToFile(fname);
  finally
    sl.Free;
  end;
end;


procedure TForm1.ClearLog;
var
  fname: string;
begin
  fname := ExtractFilePath(Application.ExeName) + 'Launcher.log';

  DeleteFile(fname);
end;


procedure TForm1.InitPatchmaker(const aLatestBuild: string);
begin
  Caption := TKMSettings.GAME_NAME + ' patchmaker';

  // We dont need any of those to create a patch
  Image1.Free;
  lbVersion.Free;
  btnLaunch.Free;
  btnVersionCheck.Free;
  btnUpdate.Free;
  pbProgress.Free;

  meLog.Top := ScaleValue(16);
  meLog.Height := ClientHeight - ScaleValue(32);

  ClearLog;
  meLog.Clear;

  try
    fPatchmaker := TKMPatchmaker.Create(aLatestBuild, HandleLog,
      procedure
      begin
        Close;
      end);
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

  lbVersion.Caption := Format('Current game version: "%s"', [fLauncher.GameVersionGet.GetVersionString]);

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


procedure TForm1.VersionCheck;
const
  TXT_CAN_PATCH = 'There is a newer version out! Patch available:';
  TXT_NEED_FULL = 'There is a newer version out (%s)! You need a full version download';
  TXT_UNKNOWN_VER = 'There is no information on the server about your game version.'+sLineBreak+'You may need to download full game version from the website or Discord';
begin
  meLog.Clear;
  btnUpdate.Enabled := False;
  btnVersionCheck.Enabled := False;

  fLauncher.VersionCheck(
    HandleLog,
    procedure
    begin
      case fLauncher.PatchChain.ChainType of
        pcNoUpdateNeeded:   HandleLog('You have the latest game version');
        pcCanPatch:         begin
                              HandleLog(TXT_CAN_PATCH + sLineBreak + fLauncher.PatchChain.GetChainAsString);
                              btnUpdate.Enabled := True;
                            end;
        pcNeedFullVersion:  HandleLog(Format(TXT_NEED_FULL, [fLauncher.PatchChain.Last.Name]));
        pcUnknownVersion:   HandleLog(TXT_UNKNOWN_VER);
        pcUnknown:          HandleLog('Status unknown');
      end;

      btnVersionCheck.Enabled := True;
    end
  );
end;


end.
