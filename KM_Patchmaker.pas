unit KM_Patchmaker;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_Bundles, KM_Patcher, KM_HDiffPatch;


type
  TKMPatchmaker = class(TThread)
  private
    fRootPath: string;
    fOnLog: TProc<string>;
    fHDiffPatch: TKMHDiffPatch;
    fNewBuild: TKMBundle;
    fNewPath: string;
    fOldBuild: TKMBundle;
    fOldPath: string;

    fPatchVersion: TKMGameVersion;
    fPatchPath: string;

    fScript: TKMPatchScript;

    procedure DoLog(aText: string);
    procedure FindBuildsInFolder;
    procedure Unpack(const aZipFilename, aFolder: string);
    procedure Package(const aFolder, aZipFilename: string);
    function FixNestedFolders(aPath: string): string;
    procedure CompareBuilds(const aOldPath, aNewPath: string);
    function CreatePatch(const aFileOld, aFileNew: string): Boolean;
  public
    constructor Create(aOnLog: TProc<string>; const aLatestBuild: string);

    procedure Execute; override;
  end;


implementation
uses
  IOUtils, Windows, ShellAPI, Math,
  KM_Settings;


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


{ TKMPatchmaker }
constructor TKMPatchmaker.Create(aOnLog: TProc<string>; const aLatestBuild: string);
begin
  inherited Create(False);

  fOnLog := aOnLog;
  fRootPath := ExpandFileName('.\');
  fNewBuild := TKMBundle.Create;
  fNewBuild.Name := aLatestBuild;
  fNewBuild.Version := TKMGameVersion.NewFromName(aLatestBuild);

  // Pass DoLog since we are going to call fHDiffPatch from a thread
  fHDiffPatch := TKMHDiffPatch.Create(DoLog);
  fScript := TKMPatchScript.Create;
end;


procedure TKMPatchmaker.DoLog(aText: string);
begin
  TThread.Queue(nil, procedure begin fOnLog(aText); end);
end;


procedure TKMPatchmaker.FindBuildsInFolder;
var
  files: TArray<string>;
  I: Integer;
  ver: TKMGameVersion;
begin
  fOldBuild := TKMBundle.Create;

  files := TDirectory.GetFiles(fRootPath);

  DoLog('Searching for previous builds:');
  for I := 0 to High(files) do
  begin
    ver := TKMGameVersion.NewFromName(ChangeFileExt(files[I], ''));

    // Accept only matching branch, full builds, skip self or anything newer
    if (ver.Branch <> gbUnknown) and (ver.VersionFrom = 0) and (ver.VersionTo < fNewBuild.Version.VersionTo) then
    begin
      // This could be a previous build
      DoLog(Format('File "%s" - version "%s"', [ExtractFileName(files[I]), ver.GetVersionString]));

      if (ver.VersionTo > fOldBuild.Version.VersionTo) then
      begin
        fOldBuild.Name := files[I];
        fOldBuild.Version := ver;
      end;
    end;
  end;

  DoLog(Format('Previous version - "%s"', [fOldBuild.Version.GetVersionString]));
end;


procedure TKMPatchmaker.Unpack(const aZipFilename, aFolder: string);
var
  commandUnZipFile: string;
begin
  RemoveDirectory(PChar(aFolder));
  commandUnZipFile := Format(TKMSettings.PATH_TO_7ZIP + ' x "%s" -o"%s" -y', [aZipFilename, aFolder]);
  CreateProcessSimple(commandUnZipFile, True, True, False);
end;


procedure TKMPatchmaker.Package(const aFolder, aZipFilename: string);
var
  commandZipFile: string;
begin
  DeleteFile(PWideChar(aZipFilename));

  commandZipFile := Format(TKMSettings.PATH_TO_7ZIP + ' a -tzip "%s" "%s*"', [aZipFilename, aFolder]);
  CreateProcessSimple(commandZipFile, True, True, False);
end;


//
function TKMPatchmaker.FixNestedFolders(aPath: string): string;
var
  fse: TArray<string>;
begin
  Result := aPath;

  fse := TDirectory.GetFiles(aPath);
  if Length(fse) <> 0 then Exit;

  fse := TDirectory.GetDirectories(aPath);
  if Length(fse) = 1 then
    Result := fse[0] + '\';
end;


procedure TKMPatchmaker.CompareBuilds(const aOldPath, aNewPath: string);
  procedure FindDifference(aAct: TKMPatchAction; const aLeft, aRight, aSubFolder: string);
  var
    fse: TArray<string>;
    I: Integer;
    res: Boolean;
    copyFrom, copyTo: string;
  begin
    // Check for sub-folders
    fse := TDirectory.GetDirectories(aLeft + aSubFolder);
    for I := 0 to High(fse) do
      FindDifference(aAct, aLeft, aRight, ExtractRelativePath(aLeft, fse[I]) + '\');

    // Check files
    fse := TDirectory.GetFiles(aLeft + aSubFolder);

    // Trim prefix path
    for I := 0 to High(fse) do
      fse[I] := ExtractRelativePath(aLeft, fse[I]);

    // Store the difference
    for I := 0 to High(fse) do
      if not FileExists(aRight + fse[I]) then
      begin
        if aAct = paAdd then
        begin
          copyFrom := fRootPath + aLeft + fse[I];
          copyTo := fRootPath + fPatchPath + fse[I];

          Assert(FileExists(copyFrom));
          Assert(not FileExists(copyTo));
          ForceDirectories(ExtractFilePath(copyTo));

          res := CopyFile(PWideChar(copyFrom), PWideChar(copyTo), False);
          if not res then
            raise Exception.Create(Format('Failed to copy "%s" to "%s"', [copyFrom, copyTo]));
        end;

        fScript.Add(TKMPatchOperation.NewDifference(aAct, fse[I]));
      end;
  end;
  procedure FindChanged(const aSubFolder: string);
  var
    fse: TArray<string>;
    I: Integer;
  begin
    // Check for sub-folders
    fse := TDirectory.GetDirectories(aOldPath + aSubFolder);
    for I := 0 to High(fse) do
      FindChanged(ExtractRelativePath(aOldPath, fse[I]) + '\');

    // Check files
    fse := TDirectory.GetFiles(aOldPath + aSubFolder);

    // Trim prefix path
    for I := 0 to High(fse) do
      fse[I] := ExtractRelativePath(aOldPath, fse[I]);

    // Check for changes
    for I := 0 to High(fse) do
      if FileExists(aOldPath + fse[I])
      and FileExists(aNewPath + fse[I]) then
        if not CheckFilesTheSame(aOldPath + fse[I], aNewPath + fse[I]) then
          CreatePatch(aOldPath + fse[I], aNewPath + fse[I]);
  end;
var
  I: Integer;
begin
  // Find difference between folders. One way is Delete, other way is Add
  FindDifference(paDelete, aOldPath, aNewPath, '');
  FindDifference(paAdd, aNewPath, aOldPath, '');

  // Find changed files
  FindChanged('');

  for I := 0 to fScript.Count - 1 do
    DoLog(fScript[I].ToLine);
end;


function TKMPatchmaker.CreatePatch(const aFileOld, aFileNew: string): Boolean;
var
  msOld, msNew, msDiff: TMemoryStream;
  patchFileName: string;
  fname: string;
begin
  msOld := TMemoryStream.Create;
  msNew := TMemoryStream.Create;
  msDiff := TMemoryStream.Create;
  try
    msOld.LoadFromFile(aFileOld);
    msNew.LoadFromFile(aFileNew);

    fHDiffPatch.CreateDiff(msOld, msNew, msDiff);

    if TKMSettings.TEST_CREATED_PATCH then
    begin
      msDiff.Position := 0;
      fHDiffPatch.TestPatch(msOld, msDiff, msNew);
    end;

    // Write down the patch
    fname := ExtractRelativePath(fOldPath, aFileOld);
    patchFileName := fname + '.patch';
    Assert(not FileExists(fRootPath + fPatchPath + patchFileName));
    ForceDirectories(ExtractFilePath(fRootPath + fPatchPath + patchFileName));
    msDiff.SaveToFile(fRootPath + fPatchPath + patchFileName);
    fScript.Add(TKMPatchOperation.NewPatch(fname, patchFileName));
  finally
    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;

//todo: Decide on naming - Diff or Patch
end;


procedure TKMPatchmaker.Execute;
var
  zipName: string;
begin
  try
    DoLog(Format('Source argument - "%s"', [fNewBuild.Name]));

    DoLog(Format('Latest version - "%s"', [fNewBuild.Version.GetVersionString]));

    // Find previous build (of the same branch)
    FindBuildsInFolder;

    if fOldBuild.Version.VersionTo = 0 then
      raise Exception.Create('Could not find previous version to make a diff from');

    if fOldBuild.Version.Branch <> fNewBuild.Version.Branch then
      raise Exception.Create('Wrong branch');

    // We can reasonably assume, that the latest build folder is not contaminated (yet)
    // But since we also allow for manual execution, we can not rely on both folders existing or being pristine
    // Unpack latest and previous
    fNewPath := '_tmp' + IntToStr(fNewBuild.Version.VersionTo) + '\';
    DoLog(Format('Unpacking latest to "%s"', [fNewPath]));
//todo:    Unpack(fNewBuild.Name, fRootPath + fNewPath);

    fOldPath := '_tmp' + IntToStr(fOldBuild.Version.VersionTo) + '\';
    DoLog(Format('Unpacking previous to "%s"', [fOldPath]));
//todo:    Unpack(fOldBuild.Name, fRootPath + fOldPath);

    // Due to how we create archives, they contain a games folder
    // Thus, resulting path to access the build is "_tmp12853\kp2023-03-31 (Alpha 12 wip r12853)\"
    fNewPath := FixNestedFolders(fNewPath);
    fOldPath := FixNestedFolders(fOldPath);
    DoLog(Format('Fixed latest to "%s"', [fNewPath]));
    DoLog(Format('Fixed previous to "%s"', [fOldPath]));

    // Describe the patch
    fPatchVersion.Branch := fNewBuild.Version.Branch;
    fPatchVersion.VersionFrom := fOldBuild.Version.VersionTo;
    fPatchVersion.VersionTo := fNewBuild.Version.VersionTo;

    fPatchPath := Format(TKMSettings.GAME_NAME + ' %s\', [fPatchVersion.GetVersionString]);
    Assert(TKMSettings.FORCE_REWRITE_PATCH_FOLDER or not DirectoryExists(fRootPath + fPatchPath));
    if TKMSettings.FORCE_REWRITE_PATCH_FOLDER then
      KMDeleteFolder(fPatchPath);

    DoLog(Format('Creating patch folder - "%s"', [fPatchPath]));
    ForceDirectories(fRootPath + fPatchPath);

    // Compare files and assemble patch script
    CompareBuilds(fOldPath, fNewPath);

    // Package script along with files/patches into new folder
    fScript.SaveToFile(fRootPath + fPatchPath + TKMSettings.PATCH_SCRIPT_FILENAME);

    //todo: Add if Terminated then Exit;

    // Zip script folder
    zipName := TKMSettings.GAME_NAME + ' ' + fPatchVersion.GetVersionString + '.zip';
    Package(fRootPath + fPatchPath, fRootPath + zipname);

    DoLog(Format('Created patch archive - "%s"', [zipname]));

    DoLog('Done!');
  except
    on E: Exception do
      DoLog(E.Message);
  end;

  //todo: FreeAndNil(fHDDiffPatch);
  //todo: FreeAndNil(fScript);
end;


end.
