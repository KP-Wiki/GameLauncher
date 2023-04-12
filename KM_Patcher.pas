unit KM_Patcher;
interface
uses
  Classes, Generics.Collections, SysUtils, Zip,
  KM_GameVersion, KM_ServerAPI, KM_Bundles;


type
  TKMPatchAction = (
    paNone,
    paAdd,    // Add (or replace) file from the patch to the game   FileFrom - pathname in archive, FileTo - pathname in game
    paDelete, // Delete file or folder from the game                FileFrom - pathname in game, FileTo - none
    //todo: paMove,   // Moves file from one place to another in the game   FileFrom - pathname in game, FileTo - pathname in game
    paPatch   // patch range of bytes in game file               FileFrom - pathname in archive, FileTo - pathname in game, Range to replace (-1 if insert)
    //todo: paLauncher update the Launcher.exe itself
  );

const
  PatchActionName: array [TKMPatchAction] of string = ('', 'add', 'del', 'patch');

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
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(const aFilename: string);
  end;

  TKMPatcher = class(TThread)
  private
    fRootPath: string;
    fServerAPI: TKMServerAPI;
    fPatchChain: TKMPatchChain;

    fOnProgress: TProc<string, Single>;
    fOnDone: TProc;
    fOnFail: TProc<string>;

    procedure SyncProgress(aText: string; aProgressBase, aProgressSub, aProgressCount: Single);
    procedure SyncDone;
    procedure SyncFail(aError: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const aRootPath: string; aServerAPI: TKMServerAPI; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>;
      aOnDone: TProc; aOnFail: TProc<string>);
  end;


implementation
uses
  Windows, ShellAPI, StrUtils,
  KM_Mutex, KM_Settings;


function NameToPatchAction(const aName: string): TKMPatchAction;
var
  I: TKMPatchAction;
begin
  Result := paNone;
  for I := Low(TKMPatchAction) to High(TKMPatchAction) do
  if SameText(PatchActionName[I], aName) then
    Exit(I);
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


procedure TKMPatcher.SyncProgress(aText: string; aProgressBase, aProgressSub, aProgressCount: Single);
begin
  TThread.Queue(nil, procedure begin fOnProgress(aText, (aProgressBase + aProgressSub) / aProgressCount); end);
end;


procedure TKMPatcher.SyncDone;
begin
  TThread.Queue(nil, procedure begin fOnDone; end);
end;


procedure TKMPatcher.SyncFail(aError: string);
begin
  TThread.Queue(nil, procedure begin fOnFail(aError); end);
end;


procedure TKMPatcher.Execute;
var
  I, K: Integer;
  ms: TMemoryStream;
  fs: TStream;
  zf: TZipFile;
  zh: TZipHeader;
  sl: TStringList;
  ps: TKMPatchScript;
  fs2: TFileStream;
  gv: TKMGameVersion;
begin
  inherited;

  try
    ms := TMemoryStream.Create;

    for I := 0 to fPatchChain.Count - 1 do
    begin
      Assert(fPatchChain[I].Size > 0);

      // Download patch (async)
      SyncProgress(Format('Downloading "%s" ..', [fPatchChain[I].Name]), I, 0.1, fPatchChain.Count);
      try
        fServerAPI.FileGet(fPatchChain[I].Url, ms,
          procedure
          var
            dlProgress: Single;
          begin
            dlProgress := ms.Size / fPatchChain[I].Size;
            SyncProgress(Format('Downloaded %d/%d bytes', [ms.Size, fPatchChain[I].Size]), I, 0.1 + dlProgress * 0.3, fPatchChain.Count);
          end);
      except
        on E: Exception do
          raise Exception.Create(Format('Failed to download "%s" - %s', [fPatchChain[I].Name, E.Message]));
      end;

      SyncProgress(Format('Downloaded %d/%d bytes', [ms.Size, fPatchChain[I].Size]), I, 0.4, fPatchChain.Count);

      // Unpack patch
      zf := TZipFile.Create;
      zf.Open(ms, zmRead);

      SyncProgress(Format('Patch containing %d entries', [zf.FileCount]), I, 0.5, fPatchChain.Count);

      // Verify patch
      zf.Read('version', fs, zh);
      sl := TStringList.Create;
      sl.LoadFromStream(fs);
      gv := TKMGameVersion.NewFromName(Trim(sl.Text));
      if gv.VersionTo <> fPatchChain[I].Version.VersionTo then
        raise Exception.Create(Format('Version in patch (%d) mismatches version in description (%d)', [gv.VersionTo, fPatchChain[I].Version.VersionTo]));
      sl.Free;
      fs.Free;

      // Load patch script
      zf.Read(TKMSettings.PATCH_SCRIPT_FILENAME, fs, zh);
      ps := TKMPatchScript.Create;
      ps.LoadFromStream(fs);
      SyncProgress(Format('Operations in patch script - "%d"', [ps.Count]), I, 0.5, fPatchChain.Count);
      fs.Free;

      // Delete game version so that if anything goes wrong we dont attempt to patch it twice
      DeleteFile(PChar(fRootPath + 'version'));

      // Apply patch following its script
      for K := 0 to ps.Count - 1 do
      begin
        ForceDirectories(fRootPath + ExtractFilePath(ps[K].FilenameTo));

        case ps[K].Act of
          paAdd:    begin
                      // Read into stream and save ourselves, to avoid the hassle with paths
                      zf.Read(ps[K].FilenameFrom, fs, zh);

                      SyncProgress(Format('Extracting "%s"', [ps[K].FilenameFrom]), I, 0.5 + K / ps.Count / 2, fPatchChain.Count);
                      fs2 := TFileStream.Create(fRootPath + ps[K].FilenameTo, fmCreate);
                      try
                        fs2.CopyFrom(fs);
                      finally
                        fs2.Free;
                      end;

                      fs.Free;
                    end;
          paDelete: begin
                      if EndsText(PathDelim, ps[K].FilenameFrom) then
                        RemoveDirectory(PChar(fRootPath + ps[K].FilenameFrom))
                      else
                        DeleteFile(PChar(fRootPath + ps[K].FilenameFrom));
                    end;
          //paMove:   // Move file in the game
          //          MoveFile(PChar(fRootPath + ps[K].FilenameFrom), PChar(fRootPath + ps[K].FilenameTo));
          //todo: paPatch:;
        end;
      end;
      ps.Free;
    end;

    ms.Free;

    SyncProgress('Done', I, 1.0, fPatchChain.Count);
    SyncDone;
  except
    on E: Exception do
      SyncFail(E.Message);
  end;
end;


end.
