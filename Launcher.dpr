program Launcher;
uses
  Vcl.Forms,
  Winapi.Windows,
  JsonDataObjects in 'src\JsonDataObjects.pas',
  Form_Main in 'src\Form_Main.pas' {Form1},
  KM_Bundles in 'src\KM_Bundles.pas',
  KM_HDiffPatch in 'src\KM_HDiffPatch.pas',
  KM_HDiffPatchTypes in 'src\KM_HDiffPatchTypes.pas',
  KM_GameVersion in 'src\KM_GameVersion.pas',
  KM_Launcher in 'src\KM_Launcher.pas',
  KM_Mutex in 'src\KM_Mutex.pas',
  KM_Patcher in 'src\KM_Patcher.pas',
  KM_Patchmaker in 'src\KM_Patchmaker.pas',
  KM_ServerAPI in 'src\KM_ServerAPI.pas',
  KM_Settings in 'src\KM_Settings.pas',
  KM_Utils in 'src\KM_Utils.pas';

{$R *.res}

var
  Form1: TForm1;

begin
  // Block duplicate launch
  if not TKMLauncher.TryLauncherInstanceLock then
  begin
    MessageBox(0, 'Launcher is already running', 'Error', MB_ICONERROR + MB_OK);
    Exit;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

  // Not really needed, since OS will do it for us on Clsoe anyway, but let's be nice
  TKMLauncher.LauncherInstanceUnlock;
end.
