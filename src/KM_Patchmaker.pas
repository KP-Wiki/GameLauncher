unit KM_Patchmaker;
interface
uses
  System.Classes, System.SysUtils, System.Types,
  KM_GameVersion, KM_Bundles, KM_Patcher, KM_HDiffPatch;


type
  TKMPatchmaker = class(TThread)
  strict private const
    HDIFF_THREAD_COUNT = 1;
  private
    fRootPath: string;
    fOnLog: TProc<string>;
    fOnSuccess: TProc;
    fHDiffPatch: TKMHDiffPatch;
    fNewBuild: TKMBundle;
    fNewFolder: string;
    fOldBuild: TKMBundle;
    fOldFolder: string;

    fPatchVersion: TKMGameVersion;
    fPatchFolder: string;

    fScript: TKMPatchScript;

    procedure DoLog(aText: string);
    procedure FindBuildsInFolder;
    procedure Unpack(const aZipFilename, aFolder: string);
    procedure Package(const aFolder, aZipFilename: string);
    function FixNestedFolders(aPath: string): string;
    procedure CreatePatch(const aFileOld, aFileNew: string);
    procedure DiffBuilds(const aOldPath, aNewPath: string);
    procedure DiffSimple(aAct: TKMPatchAction; const aLeft, aRight, aSubFolder: string);
    procedure DiffChanged(const aLeft, aRight, aSubFolder: string);
  public
    constructor Create(const aLatestBuild: string; aOnLog: TProc<string>; aOnSuccess: TProc);

    procedure Execute; override;
  end;


implementation
uses
  System.IOUtils, Winapi.Windows,
  KM_Settings, KM_Utils;


{ TKMPatchmaker }
constructor TKMPatchmaker.Create(const aLatestBuild: string; aOnLog: TProc<string>; aOnSuccess: TProc);
begin
  inherited Create(False);

  fOnLog := aOnLog;
  fOnSuccess := aOnSuccess;

  fNewBuild := TKMBundle.Create;
  fNewBuild.Filename := aLatestBuild;
  fNewBuild.Version := TKMGameVersion.NewFromString(aLatestBuild);
end;


procedure TKMPatchmaker.DoLog(aText: string);
begin
  TThread.Synchronize(nil, procedure begin fOnLog(aText); end);
end;


procedure TKMPatchmaker.FindBuildsInFolder;
var
  files: TStringDynArray;
  I: Integer;
  ver: TKMGameVersion;
begin
  fOldBuild := TKMBundle.Create;

  files := TDirectory.GetFiles(fRootPath, '*.7z');

  DoLog('Searching for older builds:');
  for I := 0 to High(files) do
  begin
    ver := TKMGameVersion.NewFromString(ChangeFileExt(files[I], ''));

    // Accept only matching branch, full builds, skip self or anything newer
    if (ver.Branch <> gbUnknown) and (ver.VersionFrom = 0) and (ver.VersionTo < fNewBuild.Version.VersionTo) then
    begin
      DoLog(Format('Build "%s" - version "%s"', [ExtractFileName(files[I]), ver.GetVersionString]));

      if (ver.VersionTo > fOldBuild.Version.VersionTo) then
      begin
        fOldBuild.Filename := files[I];
        fOldBuild.Version := ver;
      end;
    end;
  end;

  DoLog(Format('Older build found - "%s"', [fOldBuild.Version.GetVersionString]));
end;


procedure TKMPatchmaker.Unpack(const aZipFilename, aFolder: string);
var
  commandUnZipFile: string;
begin
  KMDeleteFolder(aFolder);
  commandUnZipFile := Format(TKMSettings.PATH_TO_7ZIP + ' x "%s" -o"%s" -y', [aZipFilename, aFolder]);
  CreateProcessSimple(commandUnZipFile, True, True, False);
end;


procedure TKMPatchmaker.Package(const aFolder, aZipFilename: string);
var
  commandZipFile: string;
begin
  commandZipFile := Format(TKMSettings.PATH_TO_7ZIP + ' a -tzip "%s" "%s*"', [aZipFilename, aFolder]);
  CreateProcessSimple(commandZipFile, True, True, False);
end;


function TKMPatchmaker.FixNestedFolders(aPath: string): string;
var
  fse: TStringDynArray;
begin
  Result := aPath;

  fse := TDirectory.GetFiles(fRootPath + aPath);
  if Length(fse) <> 0 then Exit;

  fse := TDirectory.GetDirectories(fRootPath + aPath);
  if Length(fse) = 1 then
    Result := ExtractRelativePath(fRootPath, fse[0] + '\');
end;


procedure TKMPatchmaker.DiffSimple(aAct: TKMPatchAction; const aLeft, aRight, aSubFolder: string);
var
  I: Integer;
  res: Boolean;
  copyFrom, copyTo: string;
begin
  // Check for sub-folders
  var listFolders := TDirectory.GetDirectories(aLeft + aSubFolder);
  for I := 0 to High(listFolders) do
    DiffSimple(aAct, aLeft, aRight, ExtractRelativePath(aLeft, listFolders[I]) + '\');

  // Add folders (files will be handled after that)
  if aAct = paAdd then
  if not DirectoryExists(aRight + aSubFolder) then
  begin
    // Copy the folder into the patch
    copyFrom := aLeft + aSubFolder;
    copyTo := fRootPath + fPatchFolder + aSubFolder;

    Assert(DirectoryExists(copyFrom), Format('"%s" does not exist', [copyFrom]));

    if DirectoryExists(copyTo) then
      // Parsing goes deep first, hence we can get paths like:
      // - data/aipresets/text/
      // - data/aipresets/
      // - ...
      // If the folder already exists - just skip it
    else
    begin
      ForceDirectories(ExtractFilePath(copyTo));
      fScript.Add(TKMPatchOperation.NewAdd(aSubFolder));
    end;
  end;

  // Check files
  var listFiles := TDirectory.GetFiles(aLeft + aSubFolder);

  // Trim prefix path
  for I := 0 to High(listFiles) do
    listFiles[I] := ExtractRelativePath(aLeft, listFiles[I]);

  // Check for the differences
  for I := 0 to High(listFiles) do
    if not FileExists(aRight + listFiles[I]) then
    begin
      if aAct = paAdd then
      begin
        // Copy the file into the patch
        copyFrom := aLeft + listFiles[I];
        copyTo := fRootPath + fPatchFolder + listFiles[I];

        Assert(FileExists(copyFrom), Format('"%s" does not exist', [copyFrom]));
        Assert(not FileExists(copyTo), Format('"%s" should not exist', [copyTo]));
        ForceDirectories(ExtractFilePath(copyTo));

        res := CopyFile(PWideChar(copyFrom), PWideChar(copyTo), False);
        if not res then
          raise Exception.Create(Format('Failed to copy "%s" to "%s"', [copyFrom, copyTo]));

        fScript.Add(TKMPatchOperation.NewAdd(listFiles[I]));
      end else
      if aAct = paDelete then
        fScript.Add(TKMPatchOperation.NewDelete(listFiles[I], GetFileHash(aLeft + listFiles[I])));
    end;

  // Delete folders (after the files are handled)
  if aAct = paDelete then
  if not DirectoryExists(aRight + aSubFolder) then
    fScript.Add(TKMPatchOperation.NewDelete(aSubFolder, ''));
end;


procedure TKMPatchmaker.DiffChanged(const aLeft, aRight, aSubFolder: string);
var
  fse: TStringDynArray;
  I: Integer;
  fileFrom, fileTo: string;
begin
  // Check for sub-folders
  fse := TDirectory.GetDirectories(aLeft + aSubFolder);
  for I := 0 to High(fse) do
    DiffChanged(aLeft, aRight, ExtractRelativePath(aLeft, fse[I]) + '\');

  // Check files
  fse := TDirectory.GetFiles(aLeft + aSubFolder);

  // Trim prefix path
  for I := 0 to High(fse) do
    fse[I] := ExtractRelativePath(aLeft, fse[I]);

  // Check for changes between existing files
  for I := 0 to High(fse) do
  begin
    fileFrom := aLeft + fse[I];
    fileTo := aRight + fse[I];

    if FileExists(fileFrom) and FileExists(fileTo) then
      if not CheckFilesTheSame(fileFrom, fileTo) then
        CreatePatch(fileFrom, fileTo);
  end;
end;


procedure TKMPatchmaker.DiffBuilds(const aOldPath, aNewPath: string);
var
  I: Integer;
begin
  // Find difference between files in folders. One way is Delete, other way is Add
  DiffSimple(paDelete, fRootPath + aOldPath, fRootPath + aNewPath, '');
  DiffSimple(paAdd, fRootPath + aNewPath, fRootPath + aOldPath, '');

  // Find changed files
  DiffChanged(fRootPath + aOldPath, fRootPath + aNewPath, '');

  for I := 0 to fScript.Count - 1 do
    DoLog(fScript[I].ToLine);
end;


procedure TKMPatchmaker.CreatePatch(const aFileOld, aFileNew: string);
var
  msOld, msNew, msDiff: TMemoryStream;
  patchFileName: string;
  fname: string;
begin
  // It's not ideal to load big files wholy into RAM, but we will need to read them anyway and they do still fit into 2GB
  msOld := TMemoryStream.Create;
  msNew := TMemoryStream.Create;
  msDiff := TMemoryStream.Create;
  try
    msOld.LoadFromFile(aFileOld);
    msNew.LoadFromFile(aFileNew);
    msOld.Position := 0;
    msNew.Position := 0;
    fHDiffPatch.PatchCreate(msOld, msNew, msDiff);

    Assert(HDIFF_THREAD_COUNT = 1, 'Only Single-thread diff is reliable. See explanation below');
    if TKMSettings.TEST_CREATED_PATCH then
    begin
      // More than once created patches were wrong
      // This test went fine, but the actual patching produced broken patched files (it was main game executable both times)
      // Oddly, on second patchmaking attempt diff was different and correct

      // Happened once between r17173 -> 17175. No investigation was made.
      // Happened again between r17399 -> 17404

      // I made a test build with 50 identical exes in it.
      // Generated patch will have 0..7 malformed diffs depending on thread count in TKMHDiffPatch:
      //
      // Threads   1T 2T 3T 4T 5T 6T
      // Errors     0  0  0  4  3  0
      //            0  0  0  5  1  0
      //            0  0  -  1  1  0
      //            0  0  -  4  1  -
      //            -  -  -  7  1  -
      //            -  -  -  2  1  -

      // The test does not catch the erorrs above (even in new TKMHDiffPatch instance)
      msOld.Position := 0;
      msNew.Position := 0;
      msDiff.Position := 0;
      fHDiffPatch.PatchTest(msOld, msDiff, msNew);
    end;

    // Write down the patch
    fname := ExtractRelativePath(fRootPath + fOldFolder, aFileOld);
    patchFileName := fname + '.patch';
    Assert(not FileExists(fRootPath + fPatchFolder + patchFileName));
    ForceDirectories(ExtractFilePath(fRootPath + fPatchFolder + patchFileName));
    msDiff.SaveToFile(fRootPath + fPatchFolder + patchFileName);
    fScript.Add(TKMPatchOperation.NewPatch(fname, GetFileHash(aFileOld), patchFileName, GetFileHash(aFileNew)));
  finally
    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;
end;


procedure TKMPatchmaker.Execute;
var
  newFolder, oldFolder: string;
  zipName: string;
begin
  try
    // Pass DoLog since we are going to call fHDiffPatch from a thread
    fHDiffPatch := TKMHDiffPatch.Create(HDIFF_THREAD_COUNT, DoLog);
    fScript := TKMPatchScript.Create;
    try
      DoLog('----------------------------------------');
      DoLog(Format('Source argument - "%s"', [fNewBuild.Filename]));

      if not FileExists(fNewBuild.Filename) then
        raise Exception.Create('Source file not found');

      fRootPath := ExtractFilePath(fNewBuild.Filename);

      if fRootPath = '' then
        raise Exception.Create('Could not extract folder from source file path');

      DoLog(Format('New version - "%s"', [fNewBuild.Version.GetVersionString]));

      // Find older build (of the same branch)
      FindBuildsInFolder;

      if fOldBuild.Version.VersionTo = 0 then
        raise Exception.Create('Could not find old version to make a diff from');

      if fOldBuild.Version.Branch <> fNewBuild.Version.Branch then
        raise Exception.Create('Wrong branch');

      // Describe the patch
      fPatchVersion.Branch := fNewBuild.Version.Branch;
      fPatchVersion.VersionFrom := fOldBuild.Version.VersionTo;
      fPatchVersion.VersionTo := fNewBuild.Version.VersionTo;

      // Delete remnants of older patchmaker ASAP (so that if patchmaking fails we dont confuse old patch for a new one)
      fPatchFolder := Format(TKMSettings.GAME_NAME + ' %s\', [fPatchVersion.GetVersionString]);
      KMDeleteFolder(fRootPath + fPatchFolder);
      zipName := TKMSettings.GAME_NAME + ' ' + fPatchVersion.GetVersionString + '.zip';
      DeleteFile(PWideChar(fRootPath + zipname));

      // We can reasonably assume, that the latest build folder is not contaminated (yet)
      // But since we also allow for manual execution, we can not rely on both folders existing or being pristine
      // Unpack new and old
      newFolder := '_tmp' + IntToStr(fNewBuild.Version.VersionTo) + '\';
      DoLog(Format('Unpacking new build to "%s"', [newFolder]));
      Unpack(fNewBuild.Filename, fRootPath + newFolder);

      oldFolder := '_tmp' + IntToStr(fOldBuild.Version.VersionTo) + '\';
      DoLog(Format('Unpacking old build to "%s"', [oldFolder]));
      Unpack(fOldBuild.Filename, fRootPath + oldFolder);

      // Due to how we create archives, they contain a nested games folder
      // Thus, resulting path to access the build is "_tmp12853\kp2023-03-31 (Alpha 12 wip r12853)\"
      fNewFolder := FixNestedFolders(newFolder);
      fOldFolder := FixNestedFolders(oldFolder);
      DoLog(Format('Fixed new path to "%s"', [fNewFolder]));
      DoLog(Format('Fixed old path to "%s"', [fOldFolder]));

      DoLog(Format('Creating patch folder - "%s"', [fPatchFolder]));
      ForceDirectories(fRootPath + fPatchFolder);

      if Terminated then Exit;

      // Compare files/folders and assemble patch script
      DiffBuilds(fOldFolder, fNewFolder);

      if Terminated then Exit;

      // Delete unpacked builds as we no longer need them
      DoLog('Deleting version folders ... please wait ...');
      KMDeleteFolder(fRootPath + oldFolder);
      KMDeleteFolder(fRootPath + newFolder);

      if Terminated then Exit;

      // Place patch script along with files/patches into new folder
      fScript.SaveToFile(fRootPath + fPatchFolder + TKMPatchScript.PATCH_SCRIPT_FILENAME);

      // Add version file
      fPatchVersion.SaveToFile(fRootPath + fPatchFolder);

      // Zip patch folder
      DoLog(Format('Packaging "%s"', [fRootPath + zipname]));
      Package(fRootPath + fPatchFolder, fRootPath + zipname);

      if Terminated then Exit;

      // Delete patch folder
      DoLog(Format('Deleting patch folder - "%s"', [fRootPath + fPatchFolder]));
      KMDeleteFolder(fRootPath + fPatchFolder);

      DoLog(Format('Created patch archive - "%s"', [zipname]));

      // Success!
      DoLog(sLineBreak + 'Done!');
      Sleep(3000);
      TThread.Synchronize(nil, procedure begin fOnSuccess; end);
    finally
      FreeAndNil(fHDiffPatch);
      FreeAndNil(fScript);
    end;
  except
    on E: Exception do
      DoLog(E.Message);
  end;

end;


end.
