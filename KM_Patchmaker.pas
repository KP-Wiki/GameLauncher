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
    fNewFolder: string;
    fOldBuild: TKMBundle;
    fOldFolder: string;

    fPatchVersion: TKMGameVersion;
    fPatchPath: string;

    fScript: TKMPatchScript;

    procedure DoLog(aText: string);
    procedure FindBuildsInFolder;
    procedure Unpack(const aZipFilename, aFolder: string);
    procedure Package(const aFolder, aZipFilename: string);
    function FixNestedFolders(aPath: string): string;
    procedure CompareBuilds(const aOldPath, aNewPath: string);
    procedure CreatePatch(const aFileOld, aFileNew: string);
  public
    constructor Create(aOnLog: TProc<string>; const aLatestBuild: string);

    procedure Execute; override;
  end;


implementation
uses
  IOUtils, Windows,
  KM_Settings, KM_Utils;


{ TKMPatchmaker }
constructor TKMPatchmaker.Create(aOnLog: TProc<string>; const aLatestBuild: string);
begin
  inherited Create(False);

  fOnLog := aOnLog;
  fRootPath := ExpandFileName('.\');
  fNewBuild := TKMBundle.Create;
  fNewBuild.Name := aLatestBuild;
  fNewBuild.Version := TKMGameVersion.NewFromString(aLatestBuild);
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
        fOldBuild.Name := files[I];
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

  fse := TDirectory.GetFiles(fRootPath + aPath);
  if Length(fse) <> 0 then Exit;

  fse := TDirectory.GetDirectories(fRootPath + aPath);
  if Length(fse) = 1 then
    Result := ExtractRelativePath(fRootPath, fse[0] + '\');
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
    fse := TDirectory.GetDirectories(fRootPath + aOldPath + aSubFolder);
    for I := 0 to High(fse) do
      FindChanged(ExtractRelativePath(fRootPath + aOldPath, fse[I]) + '\');

    // Check files
    fse := TDirectory.GetFiles(fRootPath + aOldPath + aSubFolder);

    // Trim prefix path
    for I := 0 to High(fse) do
      fse[I] := ExtractRelativePath(fRootPath + aOldPath, fse[I]);

    // Check for changes
    for I := 0 to High(fse) do
      if FileExists(fRootPath + aOldPath + fse[I])
      and FileExists(fRootPath + aNewPath + fse[I]) then
        if not CheckFilesTheSame(fRootPath + aOldPath + fse[I], fRootPath + aNewPath + fse[I]) then
          CreatePatch(fRootPath + aOldPath + fse[I], fRootPath + aNewPath + fse[I]);
  end;
var
  I: Integer;
begin
  // Find difference between folders. One way is Delete, other way is Add
  FindDifference(paDelete, fRootPath + aOldPath, fRootPath + aNewPath, '');
  FindDifference(paAdd, fRootPath + aNewPath, fRootPath + aOldPath, '');

  // Find changed files
  FindChanged('');

  for I := 0 to fScript.Count - 1 do
    DoLog(fScript[I].ToLine);
end;


procedure TKMPatchmaker.CreatePatch(const aFileOld, aFileNew: string);
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
    fname := ExtractRelativePath(fRootPath + fOldFolder, aFileOld);
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
end;


procedure TKMPatchmaker.Execute;
var
  newFolder, oldFolder: string;
  zipName: string;
begin
  try
    // Pass DoLog since we are going to call fHDiffPatch from a thread
    fHDiffPatch := TKMHDiffPatch.Create(DoLog);
    fScript := TKMPatchScript.Create;
    try
      DoLog('----------------------------------------');
      DoLog(Format('Source argument - "%s"', [fNewBuild.Name]));

      DoLog(Format('New version - "%s"', [fNewBuild.Version.GetVersionString]));

      // Find older build (of the same branch)
      FindBuildsInFolder;

      if fOldBuild.Version.VersionTo = 0 then
        raise Exception.Create('Could not find old version to make a diff from');

      if fOldBuild.Version.Branch <> fNewBuild.Version.Branch then
        raise Exception.Create('Wrong branch');

      // We can reasonably assume, that the latest build folder is not contaminated (yet)
      // But since we also allow for manual execution, we can not rely on both folders existing or being pristine
      // Unpack new and old
      newFolder := '_tmp' + IntToStr(fNewBuild.Version.VersionTo) + '\';
      DoLog(Format('Unpacking new build to "%s"', [newFolder]));
      Unpack(fNewBuild.Name, fRootPath + newFolder);

      oldFolder := '_tmp' + IntToStr(fOldBuild.Version.VersionTo) + '\';
      DoLog(Format('Unpacking old build to "%s"', [oldFolder]));
      Unpack(fOldBuild.Name, fRootPath + oldFolder);

      // Due to how we create archives, they contain a nested games folder
      // Thus, resulting path to access the build is "_tmp12853\kp2023-03-31 (Alpha 12 wip r12853)\"
      fNewFolder := FixNestedFolders(newFolder);
      fOldFolder := FixNestedFolders(oldFolder);
      DoLog(Format('Fixed new path to "%s"', [fNewFolder]));
      DoLog(Format('Fixed old path to "%s"', [fOldFolder]));

      // Describe the patch
      fPatchVersion.Branch := fNewBuild.Version.Branch;
      fPatchVersion.VersionFrom := fOldBuild.Version.VersionTo;
      fPatchVersion.VersionTo := fNewBuild.Version.VersionTo;

      fPatchPath := Format(TKMSettings.GAME_NAME + ' %s\', [fPatchVersion.GetVersionString]);
      Assert(TKMSettings.FORCE_REWRITE_PATCH_FOLDER or not DirectoryExists(fRootPath + fPatchPath));
      if TKMSettings.FORCE_REWRITE_PATCH_FOLDER then
        KMDeleteFolder(fRootPath + fPatchPath);

      DoLog(Format('Creating patch folder - "%s"', [fPatchPath]));
      ForceDirectories(fRootPath + fPatchPath);

      if Terminated then Exit;

      // Compare files and assemble patch script
      CompareBuilds(fOldFolder, fNewFolder);

      // Delete unpacked builds
      DoLog('Deleting version folders');
      KMDeleteFolder(fRootPath + oldFolder);
      KMDeleteFolder(fRootPath + newFolder);

      // Package script along with files/patches into new folder
      fScript.SaveToFile(fRootPath + fPatchPath + TKMSettings.PATCH_SCRIPT_FILENAME);

      if Terminated then Exit;

      //todo: Add version file
      // - "version" of the new build
      // - "version" that describes patch itself
      fPatchVersion.SaveToFile(fPatchPath);

      // Zip script folder
      zipName := TKMSettings.GAME_NAME + ' ' + fPatchVersion.GetVersionString + '.zip';
      DoLog(Format('Packaging "%s"', [fRootPath + zipname]));
      Package(fRootPath + fPatchPath, fRootPath + zipname);

      // Delete patch folder
      DoLog(Format('Deleting patch folder - "%s"', [fRootPath + fPatchPath]));
      KMDeleteFolder(fRootPath + fPatchPath);

      DoLog(Format('Created patch archive - "%s"', [zipname]));

      DoLog('Done!');
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
