unit KM_DiffMaker;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_Bundles, KM_Patcher, KM_HDiffPatch;


type
  TKMDiffMaker = class(TThread)
  private
    fRootPath: string;
    fOnLog: TProc<string>;
    fHDiffPatch: TKMHDiffPatch;
    fLatestBuild: TKMBundle;
    fPreviousBuild: TKMBundle;

    fScript: TKMPatchScript;

    procedure DoLog(aText: string);
    procedure FindBuildsInFolder;
    procedure Unpack(const aZipFilename, aFolder: string);
    function FixNestedFolders(aPath: string): string;
    procedure CompareBuilds(const aPreviousPath, aLatestPath: string);
    function CreatePatch(const aFilePrevious, aFileLatest: string): Boolean;
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


{ TKMDiffMaker }
constructor TKMDiffMaker.Create(aOnLog: TProc<string>; const aLatestBuild: string);
begin
  inherited Create(False);

  fRootPath := ExpandFileName('.\');
  fLatestBuild := TKMBundle.Create;
  fLatestBuild.Name := aLatestBuild;
  fOnLog := aOnLog;

  // Pass DoLog since we are going to call fHDiffPatch from a thread
  fHDiffPatch := TKMHDiffPatch.Create(DoLog);
  fScript := TKMPatchScript.Create;
end;


procedure TKMDiffMaker.DoLog(aText: string);
begin
  TThread.Queue(nil, procedure begin fOnLog(aText); end);
end;


procedure TKMDiffMaker.FindBuildsInFolder;
var
  files: TArray<string>;
  I: Integer;
  ver: TKMGameVersion;
begin
  fPreviousBuild := TKMBundle.Create;

  files := TDirectory.GetFiles(fRootPath);

  DoLog('Searching for previous builds:');
  for I := 0 to High(files) do
  begin
    ver := TKMGameVersion.NewFromName(ChangeFileExt(files[I], ''));

    // Accept only matching branch, full builds, skip self or anything newer
    if (ver.Branch <> gbUnknown) and (ver.VersionFrom = 0) and (ver.VersionTo < fLatestBuild.Version.VersionTo) then
    begin
      // This could be a previous build
      DoLog(Format('File "%s" - version "%s"', [ExtractFileName(files[I]), ver.GetVersionString]));

      if (ver.VersionTo > fPreviousBuild.Version.VersionTo) then
      begin
        fPreviousBuild.Name := files[I];
        fPreviousBuild.Version := ver;
      end;
    end;
  end;

  DoLog(Format('Previous version - "%s"', [fPreviousBuild.Version.GetVersionString]));
end;


procedure TKMDiffMaker.Unpack(const aZipFilename, aFolder: string);
var
  commandUnZipFile: string;
begin
  RemoveDirectory(PChar(aFolder));
  commandUnZipFile := Format(TKMSettings.PATH_TO_7ZIP + ' x "%s" -o"%s" -y', [aZipFilename, aFolder]);
  CreateProcessSimple(commandUnZipFile, True, True, False);
end;


//
function TKMDiffMaker.FixNestedFolders(aPath: string): string;
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


procedure TKMDiffMaker.CompareBuilds(const aPreviousPath, aLatestPath: string);
  procedure FindDifference(aAct: TKMPatchAction; const aLeft, aRight, aSubFolder: string);
  var
    fse: TArray<string>;
    I: Integer;
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
        fScript.Add(TKMPatchOperation.NewDifference(aAct, fse[I]));
  end;
  procedure FindChanged(const aSubFolder: string);
  var
    fse: TArray<string>;
    I: Integer;
  begin
    // Check for sub-folders
    fse := TDirectory.GetDirectories(aPreviousPath + aSubFolder);
    for I := 0 to High(fse) do
      FindChanged(ExtractRelativePath(aPreviousPath, fse[I]) + '\');

    // Check files
    fse := TDirectory.GetFiles(aPreviousPath + aSubFolder);

    // Trim prefix path
    for I := 0 to High(fse) do
      fse[I] := ExtractRelativePath(aPreviousPath, fse[I]);

    // Check for changes
    for I := 0 to High(fse) do
      if FileExists(aPreviousPath + fse[I])
      and FileExists(aLatestPath + fse[I]) then
        if not CheckFilesTheSame(aPreviousPath + fse[I], aLatestPath + fse[I]) then
        begin
          CreatePatch(aPreviousPath + fse[I], aLatestPath + fse[I]);

          //todo: fScript.Add(TKMPatchOperation.NewPatch(fse[I], diffFile));
        end;
  end;
var
  I: Integer;
begin
  // Find difference between folders. One way is Delete, other way is Add
  FindDifference(paDelete, aPreviousPath, aLatestPath, '');
  FindDifference(paAdd, aLatestPath, aPreviousPath, '');

  // Find changed files
  FindChanged('');

  for I := 0 to fScript.Count - 1 do
    DoLog(fScript[I].ToLine);
end;


function TKMDiffMaker.CreatePatch(const aFilePrevious, aFileLatest: string): Boolean;
var
  msOld, msNew, msDiff: TMemoryStream;
begin
  msOld := TMemoryStream.Create;
  msNew := TMemoryStream.Create;
  msDiff := TMemoryStream.Create;
  msOld.LoadFromFile(aFilePrevious);
  msNew.LoadFromFile(aFileLatest);

  fHDiffPatch.CreateDiff(msOld, msNew, msDiff);

  if TKMSettings.TEST_CREATED_PATCH then
  begin
    msDiff.Position := 0;
    fHDiffPatch.TestPatch(msOld, msDiff, msNew);
  end;

//todo: Write down the patch
//todo: Decide on naming - Diff or Patch
end;


procedure TKMDiffMaker.Execute;
var
  folderLatest: string;
  folderPrevious: string;
begin
  try
    DoLog(Format('Source argument - "%s"', [fLatestBuild.Name]));

    fLatestBuild.Version := TKMGameVersion.NewFromName(fLatestBuild.Name);

    DoLog(Format('Latest version - "%s"', [fLatestBuild.Version.GetVersionString]));

    // Find previous build (of the same branch)
    FindBuildsInFolder;

    if fPreviousBuild.Version.VersionTo = 0 then
      raise Exception.Create('Could not find previous version to make a diff from');

    // We can reasonably assume, that the latest build folder is not contaminated (yet)
    // But since we also allow for manual execution, we can not rely on both folders existing or being pristine
    // Unpack latest and previous
    folderLatest := '_tmp' + IntToStr(fLatestBuild.Version.VersionTo) + '\';
    DoLog(Format('Unpacking latest to "%s"', [folderLatest]));
//todo:    Unpack(fLatestBuild.Name, fRootPath + folderLatest);

    folderPrevious := '_tmp' + IntToStr(fPreviousBuild.Version.VersionTo) + '\';
    DoLog(Format('Unpacking previous to "%s"', [folderPrevious]));
//todo:    Unpack(fPreviousBuild.Name, fRootPath + folderPrevious);

    // Fix temp paths
    folderLatest := FixNestedFolders(folderLatest);
    folderPrevious := FixNestedFolders(folderPrevious);
    DoLog(Format('Fixed latest to "%s"', [folderLatest]));
    DoLog(Format('Fixed previous to "%s"', [folderPrevious]));

    // Compare files and assemble diff script
    CompareBuilds(folderPrevious, folderLatest);

    //todo: Add if Terminated then Exit;

    //todo: Package script along with files/patches into new folder

    //todo: 7Zip script folder

    DoLog('Done!');
  except
    on E: Exception do
      DoLog(E.Message);
  end;

  //todo: FreeAndNil(fHDDiffPatch);
  //todo: FreeAndNil(fScript);
end;


end.
