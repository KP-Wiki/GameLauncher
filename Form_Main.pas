unit Form_Main;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_Launcher, KM_RepositoryFileList, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnLaunch: TButton;
    btnVersionCheck: TButton;
    meLog: TMemo;
    Image1: TImage;
    btnUpdate: TButton;
    procedure btnLaunchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVersionCheckClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  private
    fLauncher: TKMLauncher;

    procedure VersionCheck;
    procedure VersionCheckDone;
  end;


implementation
uses
  Math,
  KM_Settings;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := TKMSettings.GAME_NAME + ' launcher';

  fLauncher := TKMLauncher.Create;

  VersionCheck;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fLauncher);
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

  fLauncher.UpdateGame(
    procedure (aCaption: string; aProgress: Single)
    begin
      meLog.Lines.Append(aCaption);
    end,
    procedure
    begin
      meLog.Lines.Append('Patching succeeded');

  btnVersionCheck.Enabled := True;
  btnUpdate.Enabled := True;
  btnLaunch.Enabled := True;
    end,
    procedure
    begin
      meLog.Lines.Append('Patching failed');

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
  rf: TKMRepositoryFile;
begin
  case fLauncher.PatchChain.ChainType of
    pcNoUpdateNeeded:   meLog.Lines.Append('You have the latest game version');
    pcCanPatch:         begin
                          meLog.Lines.Append('There is a newer version out! Patch available:');

                          for I := 0 to fLauncher.PatchChain.Count - 1 do
                          begin
                            rf := fLauncher.PatchChain[I];
                            meLog.Lines.Append(Format('%d -> %d (%dkb)', [rf.Version.VersionFrom, rf.Version.VersionTo, Ceil(rf.Size / 1024)]));
                          end;
                          btnUpdate.Enabled := True;
                        end;
    pcNeedFullVersion:  meLog.Lines.Append('There is a newer version out! Need full version download');
    pcUnknown:          meLog.Lines.Append('Unknown');
  end;

  btnVersionCheck.Enabled := True;
end;


procedure TForm1.VersionCheck;
begin
  btnUpdate.Enabled := False;

  meLog.Clear;
  meLog.Lines.Append(Format('Current game version is "%s"', [fLauncher.GameVersionGet.GetVersionString]));

  btnVersionCheck.Enabled := False;
  fLauncher.VersionCheck(
    procedure (aText: string)
    begin
      meLog.Lines.Append(aText);
    end,
    VersionCheckDone
  );
end;


end.
