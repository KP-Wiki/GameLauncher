program Launcher;
uses
  Vcl.Forms,
  Winapi.Windows,
  Form_Main in 'Form_Main.pas' {Form1},
  KM_GameVersion in 'KM_GameVersion.pas',
  KM_Launcher in 'KM_Launcher.pas',
  KM_Repository in 'KM_Repository.pas',
  KM_RepositoryFileList in 'KM_RepositoryFileList.pas',
  KM_Settings in 'KM_Settings.pas';

{$R *.res}

var
  Form1: TForm1;

begin
  // Block duplicate launch
  if TKMLauncher.IsLauncherRunning then
  begin
    MessageBox(0, 'Launcher is already running', 'Error', MB_ICONERROR + MB_OK);
    Exit;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
