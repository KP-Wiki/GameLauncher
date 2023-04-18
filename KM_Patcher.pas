unit KM_Patcher;
interface
uses
  Classes, Generics.Collections, SysUtils, Zip,
  KM_GameVersion, KM_ServerAPI, KM_Bundles, KM_HDiffPatch;


type
  TKMPatchAction = (
    paAdd,    // Add (or replace) file from the patch to the game   FileFrom - pathname in archive, FileTo - pathname in game
    paDelete, // Delete file or folder from the game                FileFrom - pathname in game, FileTo - none
    paPatch   // patch range of bytes in game file               FileFrom - pathname in archive, FileTo - pathname in game, Range to replace (-1 if insert)
    //todo: paMove,   // Moves file from one place to another in the game   FileFrom - pathname in game, FileTo - pathname in game
    //todo: paLauncher update the Launcher.exe itself
  );

const
  PatchActionName: array [TKMPatchAction] of string = ('add', 'del', 'patch');

type
  TKMPatchOperation = record
  public
    Act: TKMPatchAction;
    FilenameFrom: string;
    FilenameTo: string;
    class function NewDifference(aAct: TKMPatchAction; aFilename: string): TKMPatchOperation; static;
    class function NewPatch(const aFilenameFrom, aFilenameDiff: string): TKMPatchOperation; static;
    class function NewFromLine(const aLine: string): TKMPatchOperation; static;
    function ToLine: string;
  end;

  TKMPatchScript = class(TList<TKMPatchOperation>)
  public
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFilename: string);
  end;

  TKMPatchStage = (psDownloading, psVerifying, psPreparing, psPatching, psDone, psDone1);

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
    procedure LoadScript(aZipFile: TZipFile; aPatchScript: TKMPatchScript);
    procedure ApplyScript(aZipFile: TZipFile; aPatchScript: TKMPatchScript; aProgressBase: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(const aRootPath: string; aServerAPI: TKMServerAPI; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>;
      aOnDone: TProc; aOnFail: TProc<string>);
  end;


implementation
uses
  Windows, ShellAPI, StrUtils,
  KM_Mutex, KM_Settings, KM_Utils;


function NameToPatchAction(const aName: string): TKMPatchAction;
var
  I: TKMPatchAction;
begin
  Result := paAdd;
  for I := Low(TKMPatchAction) to High(TKMPatchAction) do
  if SameText(PatchActionName[I], aName) then
    Exit(I);

  raise Exception.Create('Unexpected TKMPatchAction = "' + aName + '"');
end;


{ TKMPatchOperation }
class function TKMPatchOperation.NewDifference(aAct: TKMPatchAction; aFilename: string): TKMPatchOperation;
begin
  Result := default(TKMPatchOperation);

  Result.Act := aAct;

  case Result.Act of
    paAdd:    Result.FilenameTo := aFilename;
    paDelete: Result.FilenameFrom := aFilename;
  else
    raise Exception.Create('Error Message');
  end;
end;


class function TKMPatchOperation.NewPatch(const aFilenameFrom, aFilenameDiff: string): TKMPatchOperation;
begin
  Result := default(TKMPatchOperation);

  Result.Act := paPatch;
  Result.FilenameFrom := aFilenameFrom;
  Result.FilenameTo := aFilenameDiff;
end;


class function TKMPatchOperation.NewFromLine(const aLine: string): TKMPatchOperation;
var
  p1, p2: Integer;
begin
  // Characters that are NOT supported include, but are not limited to: @ $ % & \ / : * ? " ' < > | ~ ` # ^ + = { } [ ] ; !
  p1 := Pos(':', aLine);
  p2 := Pos(':', aLine, p1+1);

  Result.Act := NameToPatchAction(Trim(Copy(aLine, 1, p1-1)));
  Result.FilenameFrom := Trim(Copy(aLine, p1+1, p2-p1-1));
  Result.FilenameTo := Trim(Copy(aLine, p2+1, Length(aLine)));
end;


function TKMPatchOperation.ToLine: string;
begin
  Result := Format('%-6s : %s : %s', [PatchActionName[Act], FilenameFrom, FilenameTo]);
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
    0.5, // psVerifying
    0.5, // psPreparing
    0.5, // psPatching
    1.0, // psDone
    1.0  // psDone1
  );
var
  n: TKMPatchStage;
  progressSub: Single;
begin
  n := Succ(fPatchStage);

  progressSub := Lerp(STAGE_STAKE[fPatchStage], STAGE_STAKE[n], aProgressSub);

  TThread.Queue(nil, procedure begin fOnProgress(aText, (fPatchNum + progressSub) / fPatchCount); end);
end;


procedure TKMPatcher.SyncDone;
begin
  TThread.Queue(nil, procedure begin fOnDone; end);
end;


procedure TKMPatcher.SyncFail(aError: string);
begin
  TThread.Queue(nil, procedure begin fOnFail(aError); end);
end;


procedure TKMPatcher.DownloadPatch(aBundle: TKMBundle; aToStream: TMemoryStream; aProgressBase: Integer);
begin
  fPatchStage := psDownloading;

  SyncProgress(Format('Downloading "%s" ..', [aBundle.Name]), 0.0);
  try
    fServerAPI.FileGet(
      aBundle.Url, aToStream,
      procedure
      begin
        SyncProgress(Format('Downloaded %d/%d bytes', [aToStream.Size, aBundle.Size]), aToStream.Size / aBundle.Size);
      end
    );
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
  aZipFile.Read('version', fs, zh);

  sl := TStringList.Create;
  try
    sl.LoadFromStream(fs);
    fs.Free;

    gv := TKMGameVersion.NewFromName(Trim(sl.Text));
    if gv.VersionTo <> aBundle.Version.VersionTo then
      raise Exception.Create(Format('Version in patch (%d) mismatches version in description (%d)', [gv.VersionTo, aBundle.Version.VersionTo]));
  finally
    sl.Free;
  end;
end;


procedure TKMPatcher.LoadScript(aZipFile: TZipFile; aPatchScript: TKMPatchScript);
var
  fs: TStream;
  zh: TZipHeader;
begin
  aZipFile.Read(TKMSettings.PATCH_SCRIPT_FILENAME, fs, zh);
  aPatchScript.LoadFromStream(fs);
  fs.Free;
end;


procedure TKMPatcher.ApplyScript(aZipFile: TZipFile; aPatchScript: TKMPatchScript; aProgressBase: Integer);
var
  I: Integer;
  fs: TStream;
  zh: TZipHeader;
  fsAdd: TFileStream;
  fsOld, fsDiff, fsNew: TMemoryStream;
  ps: TKMPatchOperation;
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

                  // Read into stream and save ourselves, to avoid the hassle with paths
                  aZipFile.Read(ps.FilenameFrom, fs, zh);

                  if FileExists(fRootPath + ps.FilenameTo) then
                    raise Exception.Create('File already exists');

                  fsAdd := TFileStream.Create(fRootPath + ps.FilenameTo, fmCreate);
                  try
                    fsAdd.CopyFrom(fs);
                  finally
                    fsAdd.Free;
                  end;

                  fs.Free;
                end;
      paDelete: begin
                  if EndsText(PathDelim, ps.FilenameFrom) then
                  begin
                    if not DirectoryExists(PChar(fRootPath + ps.FilenameFrom)) then
                      raise Exception.Create('Folder does not exist');

                    RemoveDirectory(PChar(fRootPath + ps.FilenameFrom));
                  end else
                  begin
                    if not FileExists(fRootPath + ps.FilenameFrom) then
                      raise Exception.Create('File does not exist');

                    DeleteFile(PChar(fRootPath + ps.FilenameFrom));
                  end;
                end;
      //paMove:   // Move file in the game
      //          MoveFile(PChar(fRootPath + ps.FilenameFrom), PChar(fRootPath + ps.FilenameTo));
      paPatch:  begin
                  SyncProgress(Format('Patching "%s"', [ps.FilenameFrom]), I / aPatchScript.Count);

                  if not FileExists(fRootPath + ps.FilenameFrom) then
                    raise Exception.Create('File does not exist');

                  // Read diff into stream
                  aZipFile.Read(ps.FilenameTo, fs, zh);
                  fsDiff := TMemoryStream.Create;
                  fsDiff.LoadFromStream(fs);
                  fsDiff.Position := 0;

                  // Read old into stream
                  fsOld := TMemoryStream.Create;
                  fsOld.LoadFromFile(fRootPath + ps.FilenameFrom);

                  fsNew := TMemoryStream.Create;

                  // fHDiffPatch has some sort of CRC check for the resulting file (Adler32 by default?)
                  fHDiffPatch.ApplyPatch(fsOld, fsDiff, fsNew);

                  fsOld.Free;
                  fsDiff.Free;

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

  fHDiffPatch := TKMHDiffPatch.Create(procedure (aText: string) begin SyncProgress(aText, 0.0); end);
  try
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

        fPatchStage := psVerifying;
        SyncProgress(Format('Patch containing %d entries', [zipFile.FileCount]), 0.0);

        VerifyPatchVersion(fPatchChain[I], zipFile);

        patchScript := TKMPatchScript.Create;
        LoadScript(zipFile, patchScript);

        fPatchStage := psPreparing;
        SyncProgress(Format('Operations in patch script - "%d"', [patchScript.Count]), 0.0);

        // Rolling back unsuccessful patches is YAGNI at this stage
        ApplyScript(zipFile, patchScript, I);

        patchScript.Free;
      end;

      zipStream.Free;

      fPatchStage := psDone;
      SyncProgress('Done', 0.0);
      SyncDone;
    except
      on E: Exception do
        SyncFail(E.Message);
    end;
  finally
    FreeAndNil(fHDiffPatch);
  end;
end;


end.
