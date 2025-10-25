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
    fLogName: string;
    fPatchmaker: TKMPatchmaker;
    fLauncher: TKMLauncher;

    procedure HandleLog(aText: string);
    procedure SaveLog(const aText: string);
    procedure InitPatchmaker(const aLatestBuild: string);
    procedure InitPatchmakerDirect(const aOperation, aFrom, aTo, aPatch: string);
    procedure InitLauncher;
    procedure VersionCheck;
  end;


implementation
uses
  System.Math, System.StrUtils,
  KM_HDiffPatch,
  KM_Bundles,
  KM_Settings;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  fLogName := ExtractFilePath(Application.ExeName) + 'logs' + PathDelim + 'Launcher' + '_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.log';

  if ParamStr(1) = '' then
    InitLauncher
  else
  if (ParamStr(1) = 'diff') or (ParamStr(1) = 'patch') then
  begin
    // Original for comparison:
    //   hdiffz.exe -s-64 -SD-256k -p-4 -f data13116.pack data13150.pack "data.pack (by hdiffz).patch"

    // Test direct:
    //   Launcher.exe diff data13116.pack data13150.pack "data.pack (by Launcher2).patch"
    //   Launcher.exe patch data13116.pack data13150alt.pack "data.pack (by Launcher2).patch"

    Assert(ParamCount = 4);
    InitPatchmakerDirect(ParamStr(1), ParamStr(2), ParamStr(3), ParamStr(4));
  end else
    // Usage (aka drag-n-drop on to exe):
    //   Launcher.exe "kp2023-06-14 (Alpha 12 wip r13150).7z"
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

  SaveLog(aText);
end;


procedure TForm1.SaveLog(const aText: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if FileExists(fLogName) then
      sl.LoadFromFile(fLogName);

    sl.Text := sl.Text + aText + sLineBreak;

    ForceDirectories(ExtractFilePath(fLogName));

    sl.SaveToFile(fLogName);
  finally
    sl.Free;
  end;
end;


procedure TForm1.InitPatchmaker(const aLatestBuild: string);
begin
  Caption := TKMSettings.GAME_NAME + ' patchmaker';
  BorderStyle := bsSizeable;

  // We dont need any of those to create a patch
  Image1.Free;
  lbVersion.Free;
  btnLaunch.Free;
  btnVersionCheck.Free;
  btnUpdate.Free;
  pbProgress.Free;

  meLog.Top := ScaleValue(16);
  meLog.Height := ClientHeight - ScaleValue(32);

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


procedure TForm1.InitPatchmakerDirect(const aOperation, aFrom, aTo, aPatch: string);
begin
  Caption := TKMSettings.GAME_NAME + ' direct';

  // We dont need any of those to create a patch
  Image1.Free;
  lbVersion.Free;
  btnLaunch.Free;
  btnVersionCheck.Free;
  btnUpdate.Free;
  pbProgress.Free;

  meLog.Top := ScaleValue(16);
  meLog.Height := ClientHeight - ScaleValue(32);

  meLog.Clear;
  HandleLog(aOperation + ':' + sLineBreak + aFrom + sLineBreak + aTo + sLineBreak + aPatch);

  try
    TThread.CreateAnonymousThread(
      procedure
      var
        msOld, msNew, msDiff: TMemoryStream;
        hddp: TKMHDiffPatch;
      begin
        msOld := TMemoryStream.Create;
        msNew := TMemoryStream.Create;
        msDiff := TMemoryStream.Create;
        try
          if aOperation = 'diff' then
          begin
            msOld.LoadFromFile(aFrom);
            msNew.LoadFromFile(aTo);

            hddp := TKMHDiffPatch.Create(HandleLog);
            hddp.PatchCreate(msOld, msNew, msDiff);

            if TKMSettings.TEST_CREATED_PATCH then
            begin
              msDiff.Position := 0;
              hddp.PatchTest(msOld, msDiff, msNew);
            end;
            hddp.Free;

            msDiff.SaveToFile(aPatch);
            HandleLog('Done');
          end else
          if aOperation = 'patch' then
          begin
            msOld.LoadFromFile(aFrom);
            msDiff.LoadFromFile(aPatch);

            hddp := TKMHDiffPatch.Create(HandleLog);
            hddp.PatchApply(msOld, msDiff, msNew);
            hddp.Free;

            msNew.SaveToFile(aTo);
            HandleLog('Done');
          end;
        finally
          msOld.Free;
          msNew.Free;
          msDiff.Free;
        end;

        TThread.Synchronize(nil, procedure begin Close; end);
      end).Start;
  except
    // App will remain opened with the error in the log
    on E: Exception do
      HandleLog(E.Message);
  end;
end;


procedure TForm1.InitLauncher;
begin
  Caption := TKMSettings.GAME_NAME + ' launcher';

  // Delete obsolete Launcher if it exists
  DeleteFile(TKMSettings.LAUNCHER_EXE_NAME_OLD);

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
    // OnSuccess
    procedure
    begin
      btnVersionCheck.Enabled := True;
      btnUpdate.Enabled := True;
      btnLaunch.Enabled := True;

      VersionCheck;
    end,
    // OnFail
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
  TXT_SERVER_ERROR = 'You may need to retry or check and download full game version from the website or Discord';
begin
  lbVersion.Caption := Format('Current game version: "%s"', [fLauncher.GameVersionGet.GetVersionString]);

  meLog.Clear;
  btnUpdate.Enabled := False;
  btnVersionCheck.Enabled := False;
  pbProgress.Position := 0;

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
        pcServerError:      HandleLog(TXT_SERVER_ERROR);
        pcUnknown:          HandleLog('Status unknown');
      end;

      btnVersionCheck.Enabled := True;
    end
  );
end;


end.
