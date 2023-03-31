unit Form_Main;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_Launcher;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    fLauncher: TKMLauncher;
  end;


implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if fLauncher.IsGameRunning then
  begin
    MessageBox(Handle, 'Game is already running', 'Error', MB_ICONWARNING + MB_OK);
    Exit;
  end;

  // Launch the game
  fLauncher.GameRun;

  // Close self
  Close;
end;


end.
