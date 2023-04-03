unit KM_Patcher;
interface
uses
  Classes, Generics.Collections, SysUtils, Zip,
  KM_GameVersion, KM_ServerAPI, KM_RepositoryFileList;


type
  TKMPatchAction = (paNone, paAdd, paDelete, paMove);

const
  PatchActionName: array [TKMPatchAction] of string = ('', 'add', 'del', 'mov');

type
  TKMPatchOperation = record
  public
    Act: TKMPatchAction;
    FilenameFrom: string;
    FilenameTo: string;
    class function NewFromLine(const aLine: string): TKMPatchOperation; static;
  end;

  TKMPatchScript = class(TList<TKMPatchOperation>)
    procedure LoadFromStream(aStream: TStream);
  end;

  TKMPatcher = class(TThread)
  private
    fRootPath: string;
    fServerAPI: TKMServerAPI;
    fPatchChain: TKMPatchChain;

    fOnProgress: TProc<string, Single>;
    fOnDone: TProc;
    fOnFail: TProc;

    procedure SyncProgress(aText: string; aValue: Single);
    procedure SyncDone;
    procedure SyncFail;
  protected
    procedure Execute; override;
  public
    constructor Create(const aRootPath: string; aServerAPI: TKMServerAPI; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc);
  end;


implementation
uses
  Windows, ShellAPI,
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


{ TKMPatcher }
constructor TKMPatcher.Create(const aRootPath: string; aServerAPI: TKMServerAPI; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc);
begin
  inherited Create(False);

  fRootPath := aRootPath;

  fServerAPI := aServerAPI;
  fPatchChain := aPatchChain;

  fOnProgress := aOnProgress;
  fOnDone := aOnDone;
  fOnFail := aOnFail;
end;


procedure TKMPatcher.SyncProgress(aText: string; aValue: Single);
begin
  TThread.Queue(nil, procedure begin fOnProgress(aText, aValue); end);
end;


procedure TKMPatcher.SyncDone;
begin
  TThread.Queue(nil, procedure begin fOnDone; end);
end;


procedure TKMPatcher.SyncFail;
begin
  TThread.Queue(nil, procedure begin fOnFail; end);
end;


procedure TKMPatcher.Execute;
var
  progress: Single;
  I, K: Integer;
  ms: TMemoryStream;
  fs: TStream;
  zf: TZipFile;
  zh: TZipHeader;
  sl: TStringList;
  ps: TKMPatchScript;
  fs2: TFileStream;
begin
  inherited;

  progress := 0;
  try
    ms := TMemoryStream.Create;

    for I := 0 to fPatchChain.Count - 1 do
    begin
      // Download patch (async)
      progress := I / fPatchChain.Count;
      SyncProgress(Format('Downloading "%s" ..', [fPatchChain[I].Name]), progress);
      try
        fServerAPI.FileGet(fPatchChain[I].Url, ms);
      except
        on E: Exception do
          raise Exception.Create(Format('Failed to download "%s" - %s', [fPatchChain[I].Name, E.Message]));
      end;

      progress := (I + 0.3) / fPatchChain.Count;
      SyncProgress(Format('Downloaded %d/%d bytes', [ms.Size, fPatchChain[I].Size]), progress);

      //ForceDirectories(fRootPath + 'updater_temp\');
      //ms.SaveToFile(fRootPath + 'updater_temp\' + fPatchChain[I].Name);

      // Unpack patch
      ms.Position := 0;
      zf := TZipFile.Create;
      zf.Open(ms, zmRead);

      progress := (I + 0.4) / fPatchChain.Count;
      SyncProgress(Format('Patch containing %d entries', [zf.FileCount]), progress);

      // Verify patch
      zf.Read('version', fs, zh);
      sl := TStringList.Create;
      sl.LoadFromStream(fs);
      SyncProgress(Format('Version in patch - "%s"', [sl[0]]), progress);
      sl.Free;
      fs.Free;

      // Load patch script
      zf.Read('script', fs, zh);
      ps := TKMPatchScript.Create;
      ps.LoadFromStream(fs);
      SyncProgress(Format('Operations in patch script - "%d"', [ps.Count]), progress);
      fs.Free;

      // Apply patch following its script
      for K := 0 to ps.Count - 1 do
      case ps[K].Act of
        paAdd:    begin
                    // Read into stream and save ourselves, to avoid the hassle with paths
                    zf.Read(ps[K].FilenameFrom, fs, zh);

                    SyncProgress(Format('Extracting "%s"', [ps[K].FilenameFrom]), progress);
                    fs2 := TFileStream.Create(fRootPath + ps[K].FilenameTo + '2', fmCreate);
                    try
                      fs2.CopyFrom(fs);
                    finally
                      fs2.Free;
                    end;

                    fs.Free;
                  end;
        paDelete: ;//todo: Apply patch following its script
        paMove:   ;//todo: Apply patch following its script
      end;
      ps.Free;

    end;

    ms.Free;

    SyncDone;
  except
    on E: Exception do
    begin
      SyncProgress(E.Message, progress);
      SyncFail;
    end;
  end;
end;


end.
