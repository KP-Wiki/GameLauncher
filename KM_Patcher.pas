unit KM_Patcher;
interface
uses
  Classes, SysUtils,
  KM_GameVersion, KM_Repository, KM_RepositoryFileList;


type
  TKMPatcher = class(TThread)
  private
    fRootPath: string;
    fRepository: TKMRepository;
    fPatchChain: TKMPatchChain;

    fOnProgress: TProc<string, Single>;
    fOnDone: TProc;
    fOnFail: TProc;

    procedure SyncProgress(aText: string; aValue: Single);
    procedure SyncFail;
  protected
    procedure Execute; override;
  public
    constructor Create(const aRootPath: string; aRepository: TKMRepository; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc);
  end;


implementation
uses
  Windows, ShellAPI,
  KM_Mutex, KM_Settings;


{ TKMPatcher }
constructor TKMPatcher.Create(const aRootPath: string; aRepository: TKMRepository; aPatchChain: TKMPatchChain; aOnProgress: TProc<string, Single>; aOnDone: TProc; aOnFail: TProc);
begin
  inherited Create(False);

  fRootPath := aRootPath;

  fRepository := aRepository;
  fPatchChain := aPatchChain;

  fOnProgress := aOnProgress;
  fOnDone := aOnDone;
  fOnFail := aOnFail;
end;


procedure TKMPatcher.SyncProgress(aText: string; aValue: Single);
begin
  TThread.Queue(nil, procedure begin fOnProgress(aText, aValue); end);
end;


procedure TKMPatcher.SyncFail;
begin
  TThread.Queue(nil, procedure begin fOnFail end);
end;


procedure TKMPatcher.Execute;
var
  progress: Single;
  I: Integer;
  ms: TMemoryStream;
begin
  inherited;

  ms := TMemoryStream.Create;

  for I := 0 to fPatchChain.Count - 1 do
  begin
    progress := I / fPatchChain.Count;

    // Download patch (async)
    SyncProgress(Format('Downloading "%s" ..', [fPatchChain[I].Name]), progress);
    try
      fRepository.FileGet(fPatchChain[I].Url, ms);
    except
      on E: Exception do
      begin
        SyncProgress(Format('Failed to download "%s"', [fPatchChain[I].Name]), progress);
        fOnFail;
      end;
    end;

    progress := (I + 0.3) / fPatchChain.Count;

    SyncProgress(Format('Downloaded %d/%d bytes', [ms.Size, fPatchChain[I].Size]), progress);

    ForceDirectories(fRootPath + 'updater_temp\');
    ms.SaveToFile(fRootPath + 'updater_temp\' + fPatchChain[I].Name);

    //todo: Unpack patch



        //todo: Verify patch
        //todo: Apply patch following its script

  end;

  ms.Free;
end;


end.
