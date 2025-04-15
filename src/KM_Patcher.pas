unit KM_Patcher;
interface
uses
  System.Classes, System.Generics.Collections, System.SysUtils,
  System.Hash, System.Zip,
  KM_GameVersion, KM_ServerAPI, KM_Bundles, KM_HDiffPatch;


type
  TKMPatchAction = (
    paAdd,    // Add (or replace) file from the patch to the game   FileFrom - pathname in archive, FileTo - pathname in game
    paDelete, // Delete file or folder from the game                FileFrom - pathname in game, FileTo - none
    paPatch   // patch range of bytes in game file                  FileFrom - pathname in archive, FileTo - pathname in game, Range to replace (-1 if insert)
    //paMove,   // YAGNI. (moves file from one place to another in the game   FileFrom - pathname in game, FileTo - pathname in game)
  );

const
  PatchActionName: array [TKMPatchAction] of string = ('add', 'del', 'patch');

type
  TKMPatchOperation = record
  public
    Act: TKMPatchAction;
    FilenameFrom: string;
    FilenameFromHash: string;
    FilenameTo: string;
    class function NewAdd(const aFilename: string): TKMPatchOperation; static;
    class function NewDelete(const aFilename, aFilenameHash: string): TKMPatchOperation; static;
    class function NewPatch(const aFilenameFrom, aFilenameFromHash, aFilenameDiff: string): TKMPatchOperation; static;
    class function NewFromLine(const aLine: string): TKMPatchOperation; static;
    function ToLine: string;
  end;

  TKMPatchScript = class(TList<TKMPatchOperation>)
  public const
    PATCH_SCRIPT_FILENAME = 'script';
  public
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFilename: string);
    function ContainsFileDelete(const aFilename: string): Boolean;
  end;

  TKMPatchStage = (psDownloading, psLoading, psVerifying, psPatching, psDone, psDone1);

  TKMPatcher = class(TThread)
  private
    fRootPath: string;
    fServerAPI: TKMServerAPI;
    fPatchChain: TKMPatchChain;
    fHDiffPatch: TKMHDiffPatch;

    fPatchCount: Integer;
    fPatchNum: Integer;
    fPatchStage: TKMPatchStage;

    fOnProgress: TProc<string, Single>;
    fOnDone: TProc;
    fOnFail: TProc<string>;

    procedure SyncProgress(aText: string; aProgressSub: Single);
    procedure SyncDone;
    procedure SyncFail(aError: string);
    procedure DownloadPatch(aBundle: TKMBundle; aToStream: TMemoryStream; aProgressBase: Integer);
    procedure VerifyPatchVersion(aBundle: TKMBundle; aZipFile: TZipFile);
    procedure ScriptLoad(aZipFile: TZipFile; aPatchScript: TKMPatchScript);
    procedure ScriptApply(aZipFile: TZipFile; aPatchScript: TKMPatchScript; aProgressBase: Integer);
    procedure ScriptVerify(aZipFile: TZipFile; aPatchScript: TKMPatchScript; aProgressBase: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(const aRootPath: string; aServerAPI: TKMServerAPI; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>;
      aOnDone: TProc; aOnFail: TProc<string>);
  end;


implementation
uses
  Winapi.Windows, Winapi.ShellAPI, System.StrUtils, System.IOUtils, System.Types, System.Math,
  KM_Mutex, KM_Settings, KM_Utils;


function NameToPatchAction(const aName: string): TKMPatchAction;
var
  I: TKMPatchAction;
begin
  for I := Low(TKMPatchAction) to High(TKMPatchAction) do
  if SameText(PatchActionName[I], aName) then
    Exit(I);

  raise Exception.Create('Unexpected TKMPatchAction = "' + aName + '"');
end;


{ TKMPatchOperation }
class function TKMPatchOperation.NewAdd(const aFilename: string): TKMPatchOperation;
begin
  Result := default(TKMPatchOperation);

  Result.Act := paAdd;
  Result.FilenameTo := aFilename;
end;


class function TKMPatchOperation.NewDelete(const aFilename, aFilenameHash: string): TKMPatchOperation;
begin
  Result := default(TKMPatchOperation);

  Result.Act := paDelete;
  Result.FilenameFrom := aFilename;
  Result.FilenameFromHash := aFilenameHash;
end;


class function TKMPatchOperation.NewPatch(const aFilenameFrom, aFilenameFromHash, aFilenameDiff: string): TKMPatchOperation;
begin
  Result := default(TKMPatchOperation);

  Result.Act := paPatch;
  Result.FilenameFrom := aFilenameFrom;
  Result.FilenameFromHash := aFilenameFromHash;
  Result.FilenameTo := aFilenameDiff;
end;


class function TKMPatchOperation.NewFromLine(const aLine: string): TKMPatchOperation;
var
  p1, p2, p3: Integer;
begin
  // We use delimiter that can not be part of the relative path and pad with spaces for readability
  p1 := Pos(':', aLine);
  p2 := Pos(':', aLine, p1+1);
  p3 := Pos(':', aLine, p2+1);

  Result.Act := NameToPatchAction(Trim(Copy(aLine, 1, p1-1)));
  Result.FilenameFrom := Trim(Copy(aLine, p1+1, p2-p1-1));
  Result.FilenameFromHash := Trim(Copy(aLine, p2+1, p3-p2-1));
  Result.FilenameTo := Trim(Copy(aLine, p3+1, Length(aLine)));
end;


function TKMPatchOperation.ToLine: string;
begin
  // We use delimiter that can not be part of the relative path and pad with spaces for readability
  Result := Format('%-6s  :  %s  :  %s  :  %s', [PatchActionName[Act], FilenameFrom, FilenameFromHash, FilenameTo]);
end;


{ TKMPatchScript }
procedure TKMPatchScript.LoadFromStream(aStream: TStream);
var
  sl: TStringList;
  I: Integer;
begin
  sl := TStringList.Create;

  sl.LoadFromStream(aStream);

  for I := 0 to sl.Count - 1 do
    Add(TKMPatchOperation.NewFromLine(sl[I]));

  sl.Free;
end;


procedure TKMPatchScript.SaveToFile(const aFilename: string);
var
  sl: TStringList;
  I: Integer;
begin
  sl := TStringList.Create;

  for I := 0 to Count - 1 do
    sl.Append(Items[I].ToLine);

  sl.SaveToFile(aFilename);

  sl.Free;
end;


function TKMPatchScript.ContainsFileDelete(const aFilename: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if (Items[I].Act = paDelete) and (Items[I].FilenameFrom = aFilename) then
      Exit(True);
end;


{ TKMPatcher }
constructor TKMPatcher.Create(const aRootPath: string; aServerAPI: TKMServerAPI; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc<string>);
begin
  inherited Create(False);

  fRootPath := aRootPath;

  fServerAPI := aServerAPI;
  fPatchChain := aPatchChain;

  fOnProgress := aOnProgress;
  fOnDone := aOnDone;
  fOnFail := aOnFail;
end;


procedure TKMPatcher.SyncProgress(aText: string; aProgressSub: Single);
const
  STAGE_STAKE: array [TKMPatchStage] of Single = (
    0.0, // psDownloading
    0.5, // psLoading
    0.5, // psVerifying
    0.6, // psPatching
    1.0, // psDone
    1.0  // psDone1
  );
begin
  TThread.Synchronize(nil,
    procedure
    var
      n: TKMPatchStage;
      progressSub: Single;
    begin
      n := Succ(fPatchStage);
      progressSub := Lerp(STAGE_STAKE[fPatchStage], STAGE_STAKE[n], aProgressSub);
      fOnProgress(aText, (fPatchNum + progressSub) / Max(fPatchCount, 1));
    end);
end;


procedure TKMPatcher.SyncDone;
begin
  TThread.Synchronize(nil, procedure begin fOnDone; end);
end;


procedure TKMPatcher.SyncFail(aError: string);
begin
  TThread.Synchronize(nil, procedure begin fOnFail(aError); end);
end;


procedure TKMPatcher.DownloadPatch(aBundle: TKMBundle; aToStream: TMemoryStream; aProgressBase: Integer);
begin
  fPatchStage := psDownloading;

  // Clear the stream. Even though we write from Position 0, we still don't want any remainders (zip cant handle that)
  aToStream.Clear;

  SyncProgress(Format('Downloading "%s" ..', [aBundle.Name]), 0.0);
  try
    case aBundle.Location of
      blServer: fServerAPI.FileGet(
                  aBundle.Url2, aToStream,
                  procedure
                  begin
                    SyncProgress(Format('Downloading %d/%d bytes', [aToStream.Size, aBundle.Size]), aToStream.Size / aBundle.Size);
                  end
                );
      blLocal:  begin
                  aToStream.LoadFromFile(aBundle.Url2);
                  SyncProgress(Format('Loaded %d/%d bytes', [aToStream.Size, aBundle.Size]), aToStream.Size / aBundle.Size);
                end;
    end;

    // Dont forget to rewind back to the start for anyone who needs to use the stream data
    aToStream.Position := 0;
  except
    on E: Exception do
      raise Exception.Create(Format('Failed to download "%s" - %s', [aBundle.Name, E.Message]));
  end;

  SyncProgress(Format('Downloaded %d/%d bytes', [aToStream.Size, aBundle.Size]), 1.0);
end;


procedure TKMPatcher.VerifyPatchVersion(aBundle: TKMBundle; aZipFile: TZipFile);
var
  fs: TStream;
  zh: TZipHeader;
  sl: TStringList;
  gv: TKMGameVersion;
begin
  aZipFile.Read(TKMGameVersion.VERSION_FILENAME, fs, zh);

  sl := TStringList.Create;
  try
    sl.LoadFromStream(fs);
    fs.Free;

    gv := TKMGameVersion.NewFromString(Trim(sl.Text));
    if gv.VersionTo <> aBundle.Version.VersionTo then
      raise Exception.Create(Format('Version in patch (%d) mismatches version in description (%d)', [gv.VersionTo, aBundle.Version.VersionTo]));
  finally
    sl.Free;
  end;
end;


procedure TKMPatcher.ScriptLoad(aZipFile: TZipFile; aPatchScript: TKMPatchScript);
var
  fs: TStream;
  zh: TZipHeader;
begin
  aZipFile.Read(TKMPatchScript.PATCH_SCRIPT_FILENAME, fs, zh);
  aPatchScript.LoadFromStream(fs);
  fs.Free;
end;


procedure TKMPatcher.ScriptVerify(aZipFile: TZipFile; aPatchScript: TKMPatchScript; aProgressBase: Integer);
  function FileIsTemp(const aFilename: string): Boolean;
  begin
    Result := EndsText(TKMSettings.TEMP_FILE_ENDING1, LowerCase(aFilename)) or EndsText(TKMSettings.TEMP_FILE_ENDING2, LowerCase(aFilename));
  end;
var
  I, K: Integer;
  ps: TKMPatchOperation;
  fs: TStream;
  zh: TZipHeader;
  filesInFolder: TStringDynArray;
  goodToDelete: Boolean;
begin
  fPatchStage := psVerifying;

  // We prefer to be preservationists - if there's anything change - cancel the patching, so player does not loose any data
  // Downloading and installing a full new build is always an easy option
  // Rolling back changes to restore lost data is much more complicated
  for I := 0 to aPatchScript.Count - 1 do
  begin
    ps := aPatchScript[I];

    case ps.Act of
      paAdd:    // Added files should not overwrite anything (except own replicas)
                if FileExists(fRootPath + ps.FilenameTo) then
                begin
                  aZipFile.Read(ChangeDelimForZip(ps.FilenameTo), fs, zh);
                  try
                    if not CheckFileStreamTheSame(fRootPath + ps.FilenameTo, fs) then
                      raise Exception.Create(Format('Different file (%s) already exists', [ps.FilenameTo]));
                  finally
                    fs.Free;
                  end;
                end;
      paDelete: if EndsText(PathDelim, ps.FilenameFrom) then
                begin
                  if DirectoryExists(fRootPath + ps.FilenameFrom) then
                  begin
                    // Removed folders should become empty (or contiain only temp files)
                    filesInFolder := TDirectory.GetFiles(fRootPath + ps.FilenameFrom);

                    goodToDelete := True;
                    for K := 0 to High(filesInFolder) do
                      goodToDelete := goodToDelete and (FileIsTemp(filesInFolder[K]) or aPatchScript.ContainsFileDelete(ExtractRelativePath(fRootPath, filesInFolder[K])));

                    if not goodToDelete then
                      raise Exception.Create(Format('Folder that needs to be deleted (%s) is not going to be empty', [ps.FilenameFrom]));
                  end;
                end else
                  // Removed files should be "original" (check hash) or already gone
                  if FileExists(fRootPath + ps.FilenameFrom)
                  and (GetFileHash(fRootPath + ps.FilenameFrom) <> ps.FilenameFromHash) then
                    raise Exception.Create(Format('File that needs to be deleted (%s) is different', [ps.FilenameFrom]));
      paPatch:  begin
                  if ps.FilenameFrom = TKMSettings.DIFF_PATCH_DLL_NAME then
                    raise Exception.Create('Launcher DLL needs to be updated');

                  // Patched files should be "original" (check hash)
                  if GetFileHash(fRootPath + ps.FilenameFrom) <> ps.FilenameFromHash then
                    raise Exception.Create(Format('File that needs to be patched (%s) is different', [ps.FilenameFrom]));
                end;
    end;
  end;
end;


procedure TKMPatcher.ScriptApply(aZipFile: TZipFile; aPatchScript: TKMPatchScript; aProgressBase: Integer);
var
  I: Integer;
  ps: TKMPatchOperation;
  fs: TStream;
  zh: TZipHeader;
  fsAdd: TFileStream;
  fsOld, fsDiff, fsNew: TMemoryStream;
begin
  fPatchStage := psPatching;

  // Apply patch following its script
  for I := 0 to aPatchScript.Count - 1 do
  begin
    ps := aPatchScript[I];

    ForceDirectories(fRootPath + ExtractFilePath(ps.FilenameTo));

    case ps.Act of
      paAdd:    begin
                  SyncProgress(Format('Adding "%s"', [ps.FilenameFrom]), I / aPatchScript.Count);

                  // We can not read empty folders from zip
                  if EndsText(PathDelim, ps.FilenameTo) then
                    // Already created above
                  else
                  begin
                    // Read into stream and save ourselves, to avoid the hassle with paths
                    aZipFile.Read(ChangeDelimForZip(ps.FilenameTo), fs, zh);

                    fsAdd := TFileStream.Create(fRootPath + ps.FilenameTo, fmCreate);
                    try
                      fs.Position := 0;
                      fsAdd.CopyFrom(fs, fs.Size);
                    finally
                      fsAdd.Free;
                    end;

                    fs.Free;
                  end;
                end;
      paDelete: if EndsText(PathDelim, ps.FilenameFrom) then
                begin
                  if DirectoryExists(fRootPath + ps.FilenameFrom) then
                    RemoveDirectory(PWideChar(fRootPath + ps.FilenameFrom));
                end else
                  if FileExists(fRootPath + ps.FilenameFrom) then
                    DeleteFile(PWideChar(fRootPath + ps.FilenameFrom));
      //paMove:   // Move file in the game
      //          MoveFile(PChar(fRootPath + ps.FilenameFrom), PChar(fRootPath + ps.FilenameTo));
      paPatch:  begin
                  SyncProgress(Format('Patching "%s"', [ps.FilenameFrom]), I / aPatchScript.Count);

                  if not FileExists(fRootPath + ps.FilenameFrom) then
                    raise Exception.Create('File does not exist');

                  // Read diff into stream
                  aZipFile.Read(ChangeDelimForZip(ps.FilenameTo), fs, zh);
                  fsDiff := TMemoryStream.Create;
                  fsDiff.LoadFromStream(fs);
                  fsDiff.Position := 0;

                  // Read old file into stream
                  fsOld := TMemoryStream.Create;
                  fsOld.LoadFromFile(fRootPath + ps.FilenameFrom);

                  fsNew := TMemoryStream.Create;

                  // fHDiffPatch has some sort of CRC check for the resulting file (Adler32 by default?)
                  fHDiffPatch.ApplyPatch(fsOld, fsDiff, fsNew);

                  fsOld.Free;
                  fsDiff.Free;

                  // If we need to patch ourselves, do some special steps
                  if ps.FilenameFrom = TKMSettings.LAUNCHER_EXE_NAME then
                    RenameFile(TKMSettings.LAUNCHER_EXE_NAME, TKMSettings.LAUNCHER_EXE_NAME_OLD);

                  // If we need to patch DLL, do some special steps
                  // For now YAGNI. Until then - fail in ScriptVerify and ask for a full build
                  //if ps.FilenameFrom = TKMSettings.DIFF_PATCH_DLL_NAME then

                  fsNew.SaveToFile(fRootPath + ps.FilenameFrom);
                  fsNew.Free;
                end;
    end;
  end;
end;


procedure TKMPatcher.Execute;
var
  I: Integer;
  zipStream: TMemoryStream;
  zipFile: TZipFile;
  patchScript: TKMPatchScript;
begin
  inherited;

  try
    fHDiffPatch := TKMHDiffPatch.Create(procedure (aText: string) begin SyncProgress(aText, 0.0); end);
    try
      fPatchCount := fPatchChain.Count;

      zipStream := TMemoryStream.Create;

      for I := 0 to fPatchChain.Count - 1 do
      begin
        fPatchNum := I;
        Assert(fPatchChain[I].Size > 0);

        DownloadPatch(fPatchChain[I], zipStream, I);

        // Open patch
        zipFile := TZipFile.Create;
        zipFile.Open(zipStream, zmRead);

        fPatchStage := psLoading;
        SyncProgress(Format('Patch containing %d entries', [zipFile.FileCount]), 0.0);
        VerifyPatchVersion(fPatchChain[I], zipFile);
        patchScript := TKMPatchScript.Create;
        ScriptLoad(zipFile, patchScript);
        SyncProgress(Format('Patch containing %d operations', [patchScript.Count]), 0.0);

        // Verify that the patch can be applied without loosing any data
        // Rolling back unsuccessful patches is YAGNI at this stage
        ScriptVerify(zipFile, patchScript, I);

        // Apply the patch (it includes version file patch)
        ScriptApply(zipFile, patchScript, I);

        patchScript.Free;
        zipFile.Free;
      end;

      zipStream.Free;

      fPatchStage := psDone;
      SyncProgress('Done', 0.0);
      SyncDone;
    finally
      FreeAndNil(fHDiffPatch);
    end;
  except
    on E: Exception do
      SyncFail(E.Message + sLineBreak + 'Patch could not be applied automatically' + sLineBreak + 'Please download full game version');
  end;
end;


end.
