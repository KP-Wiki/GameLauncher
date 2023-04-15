unit KM_Utils;
interface


  function CreateProcessSimple(aFilename: string; aShowWindow, aWait, aLowPriority: Boolean): NativeUInt;
  function CheckFilesTheSame(const aFilenameA, aFilenameB: string): Boolean;
  procedure KMDeleteFolder(const aFolderPath: string);
  function Lerp(A, B: Single; aMixValue: Single): Single; inline;


implementation
uses
  Classes, IOUtils, Math, ShellAPI, SysUtils, Windows;


function CreateProcessSimple(aFilename: string; aShowWindow, aWait, aLowPriority: Boolean): NativeUInt;
var
  appName: array [0..512] of Char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  res: Cardinal;
begin
  StrPCopy(appName, aFilename);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := IfThen(aShowWindow, SW_SHOWDEFAULT, SW_HIDE);

  CreateProcess(
    nil,
    appName,
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS or (BELOW_NORMAL_PRIORITY_CLASS * Ord(aLowPriority)),
    nil,
    nil,
    StartupInfo,
    ProcessInfo);

  Result := ProcessInfo.hProcess;

  if aWait then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, res);
    Result := 0;
  end;
end;


function CheckFilesTheSame(const aFilenameA, aFilenameB: string): Boolean;
const
  // Reading and comparing in chunks is much faster. 16kb seems to be okay
  CHUNK = 16384;
var
  size1, size2: Int64;
  fs1, fs2: TFileStream;
  I, K: Integer;
  buf1, buf2: array [0..CHUNK-1] of Byte;
  sz: Integer;
begin
  size1 := TFile.GetSize(aFilenameA);
  size2 := TFile.GetSize(aFilenameB);

  if size1 <> size2 then Exit(False);

  // This branch will be called rarely
  // It is very rare case that two files will be identical in size and have different contents
  fs1 := TFileStream.Create(aFilenameA, fmOpenRead);
  fs2 := TFileStream.Create(aFilenameB, fmOpenRead);
  try
    for I := 0 to fs1.Size div CHUNK do
    begin
      sz := Min(fs1.Size - I * CHUNK, CHUNK);

      fs1.Read(buf1, sz);
      fs2.Read(buf2, sz);

      for K := 0 to sz - 1 do
      if buf1[K] <> buf2[K] then
        Exit(False);
    end;
  finally
    fs1.Free;
    fs2.Free;
  end;

  Result := True;
end;


procedure KMDeleteFolder(const aFolderPath: string);
{$IFDEF MSWINDOWS}
var
  ShOp: TSHFileOpStruct;
{$IFDEF FPC}
const
  FOF_NO_UI = FOF_SILENT or FOF_NOCONFIRMATION or FOF_NOERRORUI or FOF_NOCONFIRMMKDIR; // don't display any UI at all
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF FPC}
  ShOp.hWnd := 0;
  {$ELSE}
  ShOp.Wnd := 0;
  {$ENDIF}
  ShOp.wFunc := FO_DELETE;
  ShOp.pFrom := PChar(aFolderPath + #0);
  ShOp.pTo := nil;
  ShOp.fFlags := FOF_NO_UI{ or FOF_ALLOWUNDO};
  SHFileOperation(ShOp);
  {$ENDIF}
end;


function Lerp(A, B: Single; aMixValue: Single): Single;
begin
  Result := A + (B - A) * aMixValue;
end;


end.
