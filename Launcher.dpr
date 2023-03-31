program Launcher;
uses
  Vcl.Forms,
  Winapi.Windows,
  Form_Main in 'Form_Main.pas' {Form1},
  KM_Launcher in 'KM_Launcher.pas';

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
