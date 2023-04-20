unit KM_Utils;
interface
uses
  Classes;

  function CreateProcessSimple(aFilename: string; aShowWindow, aWait, aLowPriority: Boolean): NativeUInt;
  function CheckFilesTheSame(const aFilenameA, aFilenameB: string): Boolean;
  function CheckFileStreamTheSame(const aFilename: string; aStream: TStream): Boolean;
  procedure KMDeleteFolder(const aFolderPath: string);
  function GetFileHash(const aFilename: string): string;
  function Lerp(A, B: Single; aMixValue: Single): Single; inline;


implementation
uses
  Windows, IOUtils, Math, ShellAPI, SysUtils, System.Hash;


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


function GetFileSize(const aFilename: string): LongInt;
var
  SearchRec: TSearchRec;
begin
  if not FileExists(aFilename) then
    raise Exception.Create(ExtractFileName(aFilename) + ' could not be found. Check that data.pack exists');

  Result := -1;
  if FindFirst(ExpandFileName(aFilename), faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;


function CheckStreamsTheSame(aStreamA, aStreamB: TStream): Boolean;
const
  // Reading and comparing in chunks is much faster. 16kb seems to be okay
  CHUNK = 16384;
var
  I, K: Integer;
  buf1, buf2: array [0..CHUNK-1] of Byte;
  sz: Integer;
begin
  if aStreamA.Size <> aStreamA.Size then Exit(False);

  // This branch will be called rarely
  // It is very rare case that two files will be identical in size and have different contents
  for I := 0 to aStreamA.Size div CHUNK do
  begin
    sz := Min(aStreamA.Size - I * CHUNK, CHUNK);

    aStreamA.Read(buf1, sz);
    aStreamB.Read(buf2, sz);

    for K := 0 to sz - 1 do
    if buf1[K] <> buf2[K] then
      Exit(False);
  end;

  Result := True;
end;


function CheckFilesTheSame(const aFilenameA, aFilenameB: string): Boolean;
var
  fs1, fs2: TFileStream;
begin
  fs1 := TFileStream.Create(aFilenameA, fmOpenRead);
  fs2 := TFileStream.Create(aFilenameB, fmOpenRead);
  try
    Result := CheckStreamsTheSame(fs1, fs2);
  finally
    fs1.Free;
    fs2.Free;
  end;
end;


function CheckFileStreamTheSame(const aFilename: string; aStream: TStream): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFilename, fmOpenRead);
  try
    Result := CheckStreamsTheSame(fs, aStream);
  finally
    fs.Free;
  end;
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


function GetFileHash(const aFilename: string): string;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(aFilename);
    Result := System.Hash.THashMD5.GetHashString(ms);
  finally
    ms.Free;
  end;
end;


function Lerp(A, B: Single; aMixValue: Single): Single;
begin
  Result := A + (B - A) * aMixValue;
end;


end.
