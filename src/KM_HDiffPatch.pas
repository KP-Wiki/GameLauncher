unit KM_HDiffPatch;
interface
uses
  System.Classes, System.Math, System.SysUtils, Winapi.Windows,
  KM_HDiffPatchTypes;


type
  TKMHDiffPatch = class
  private const
    //MATCH_SCORE = 4; // DEFAULT -m-6, recommended bin: 0--4 text: 4--9 etc...
    MATCH_BLOCK_SIZE_SMALL = 8; // DEFAULT 1<<6 recommended (1<<4)--(1<<14)
    MATCH_BLOCK_SIZE_BIG = 64; // DEFAULT 1<<6 recommended (1<<4)--(1<<14)
    PATCH_STEP_SIZE = 1024 * 256; // DEFAULT -SD-256k, recommended 64k,2m etc...
  private
    fThreadCount: Integer;
    fOnLog: TProc<string>;
    fLibHandle: NativeUInt;
    fDLLCreateDiff: TDLLCreateDiff;
    fDLLCreateDiffStream: TDLLCreateDiffStream;
    fDLLInfoDiff: TDLLInfoDiff;
    fDLLPatchDiff: TDLLPatchDiff;
    procedure DoLog(const aText: string);
    procedure LoadDLL(const aDLLPath: string);
    //procedure TestDLL1;
    //procedure TestDLL2;
    procedure TestDLL_Stream;
  public
    constructor Create(aThreadCount: Integer; aOnLog: TProc<string>);
    destructor Destroy; override;

    //procedure CreateDiff(aStreamOld, aStreamNew, aStreamDiff: TMemoryStream);
    procedure PatchCreate(aStreamOld, aStreamNew: TStream; aStreamDiff: TMemoryStream);
    procedure PatchApply(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
    procedure PatchTest(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
  end;


implementation
uses
  KM_Settings, KM_Utils;


function funcRW(const aStream: PStreamOutput; aReadFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
begin
  Assert(False, 'No one has used it yet');

  Result := MaxInt;
end;


function funcW(const aStream: PStreamOutput; aWriteToPos: UInt64; aData, aDataEnd: Pointer): Integer; cdecl;
var
  len: NativeUInt;
  requiredSize: UInt64;
begin
  len := NativeUInt(aDataEnd) - NativeUInt(aData);

  requiredSize := Max(UInt64(aStream.ms.Size), aWriteToPos + len);

  if requiredSize > aStream.ms.Size then
    aStream.ms.Size := requiredSize;

  // Read through generic means (futureproofing)
  aStream.ms.Position := aWriteToPos;
  aStream.ms.Write(aData^, len);

  aStream.StreamSize := aStream.ms.Size;

  Result := len;
end;


function funcR(const aStream: PStreamInput; aReadFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
var
  len: Integer;
begin
  len := NativeUInt(aOutDataEnd) - NativeUInt(aOutData);

  // Read through generic means
  aStream.ss.Position := aReadFromPos;
  aStream.ss.Read(aOutData^, len);

  Result := len;
end;


{ TKMHDiffPatch }
constructor TKMHDiffPatch.Create(aThreadCount: Integer; aOnLog: TProc<string>);
begin
  inherited Create;

  fThreadCount := aThreadCount;
  fOnLog := aOnLog;

  // Load DLL dynamically, so we could move it into the utility folder
  LoadDLL(TKMSettings.DIFF_PATCH_DLL_NAME);

  //TestDLL1;
  //TestDLL2;
  TestDLL_Stream;
end;


destructor TKMHDiffPatch.Destroy;
begin
  // Unload DLL
  if fLibHandle <> 0 then
    FreeLibrary(fLibHandle);

  inherited;
end;


procedure TKMHDiffPatch.DoLog(const aText: string);
begin
  if Assigned(fOnLog) then
    fOnLog(aText);
end;


procedure TKMHDiffPatch.LoadDLL(const aDLLPath: string);
var
  err: Cardinal;
begin
  if not FileExists(aDLLPath) then
    raise Exception.Create(Format('Error - %s not found', [aDLLPath]));

  DoLog(Format('Loading "%s"', [aDLLPath]));

  // Load without displaying any popup error messages
  fLibHandle := SafeLoadLibrary(aDLLPath, $FFFF);
  if fLibHandle = 0 then
    raise Exception.Create(Format('DLL was NOT loaded - %d', [GetLastError]));

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('Error in the DLL loading - %d', [err]));

  fDLLCreateDiff := GetProcAddress(fLibHandle, 'create_single_compressed_diff');
  fDLLCreateDiffStream := GetProcAddress(fLibHandle, 'create_single_compressed_diff_stream');
  fDLLInfoDiff := GetProcAddress(fLibHandle, 'getSingleCompressedDiffInfo');
  fDLLPatchDiff := GetProcAddress(fLibHandle, 'patch_single_compressed_diff');

  if not Assigned(fDLLCreateDiff) or not Assigned(fDLLCreateDiffStream) or not Assigned(fDLLInfoDiff) or not Assigned(fDLLPatchDiff) then
    raise Exception.Create('Could not get process address in the DLL');

  DoLog(Format('Linked create_single_compressed_diff at "$%.8x"', [PCardinal(Addr(fDLLCreateDiff))^]));
  DoLog(Format('Linked create_single_compressed_diff_stream at "$%.8x"', [PCardinal(Addr(fDLLCreateDiffStream))^]));
  DoLog(Format('Linked getSingleCompressedDiffInfo at "$%.8x"', [PCardinal(Addr(fDLLInfoDiff))^]));
  DoLog(Format('Linked patch_single_compressed_diff at "$%.8x"', [PCardinal(Addr(fDLLPatchDiff))^]));
end;


{procedure TKMHDiffPatch.TestDLL1;
const
  OLDTEXT = '01234567890123456789ABCDefgh';
  NEWTEXT = '01234012340123401234abcdEFGH';
var
  msOld, msNew, msDiff: TMemoryStream;
  oldString, newString: AnsiString;
begin
  // Create test diff
  begin
    oldString := OLDTEXT;
    msOld := TMemoryStream.Create;
    msOld.Write(oldString[1], Length(oldString));
    msOld.Position := 0;

    newString := NEWTEXT;
    msNew := TMemoryStream.Create;
    msNew.Write(newString[1], Length(newString));
    msNew.Position := 0;

    msDiff := TMemoryStream.Create;

    CreateDiff(msOld, msNew, msDiff);

    msOld.Free;
    msNew.Free;
  end;

  // Apply test patch
  begin
    oldString := OLDTEXT;
    msOld := TMemoryStream.Create;
    msOld.Write(oldString[1], Length(oldString));
    msOld.Position := 0;

    // Rewind to start
    msDiff.Position := 0;

    msNew := TMemoryStream.Create;

    ApplyPatch(msOld, msDiff, msNew);

    msNew.Position := 0;
    SetLength(newString, msNew.Size);
    msNew.Read(newString[1], msNew.Size);

    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;

  // Report result
  if newString = NEWTEXT then
    DoLog('DLL self-test - Ok')
  else
    raise Exception.Create('DLL self-test - Fail');
end;


procedure TKMHDiffPatch.TestDLL2;
var
  msOld, msNew, msDiff: TMemoryStream;
begin
  // Create test diff
  begin
    msOld := TMemoryStream.Create;
    msNew := TMemoryStream.Create;
    msDiff := TMemoryStream.Create;

    CreateDiff(msOld, msNew, msDiff);

    msOld.Free;
    msNew.Free;
  end;

  // Apply test patch
  begin
    msOld := TMemoryStream.Create;

    // Rewind to start
    msDiff.Position := 0;

    msNew := TMemoryStream.Create;

    ApplyPatch(msOld, msDiff, msNew);

    if msNew.Size = 0 then
      DoLog('DLL self-test - Ok')
    else
      raise Exception.Create('DLL self-test - Fail');

    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;
end;}


procedure TKMHDiffPatch.TestDLL_Stream;
const
  OLDTEXT = '01234567890123456789ABCDefgh';
  NEWTEXT = '01234012340123401234abcdEFGH';
var
  msOld, msNew, msDiff: TMemoryStream;
  oldString, newString: AnsiString;
begin
  // Create test diff
  begin
    oldString := OLDTEXT;
    msOld := TMemoryStream.Create;
    msOld.Write(oldString[1], Length(oldString));
    msOld.Position := 0;

    newString := NEWTEXT;
    msNew := TMemoryStream.Create;
    msNew.Write(newString[1], Length(newString));
    msNew.Position := 0;

    msDiff := TMemoryStream.Create;

    PatchCreate(msOld, msNew, msDiff);

    msOld.Free;
    msNew.Free;
  end;

  // Apply test patch
  begin
    oldString := OLDTEXT;
    msOld := TMemoryStream.Create;
    msOld.Write(oldString[1], Length(oldString));
    msOld.Position := 0;

    // Rewind to start
    msDiff.Position := 0;

    msNew := TMemoryStream.Create;

    PatchApply(msOld, msDiff, msNew);

    msNew.Position := 0;
    SetLength(newString, msNew.Size);
    msNew.Read(newString[1], msNew.Size);

    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;

  // Report result
  if newString = NEWTEXT then
    DoLog('DLL self-test - Ok')
  else
    raise Exception.Create('DLL self-test - Fail');
end;


{procedure TKMHDiffPatch.CreateDiff(aStreamOld, aStreamNew, aStreamDiff: TMemoryStream);
var
  bufDiff: TStreamOutput;
  t: Cardinal;
begin
  t := GetTickCount;

  Assert(aStreamDiff.Size = 0, 'Diff stream must be empty');

  bufDiff.streamImport := nil;
  bufDiff.StreamSize := 0;
  bufDiff.RW := funcRW;
  bufDiff.W := funcW;
  bufDiff.ms := aStreamDiff;

  DoLog(Format('Creating diff for %s <-> %s', [BytesToStr(aStreamOld.Size), BytesToStr(aStreamNew.Size)]));

  fDLLCreateDiff(
    aStreamNew.Memory, Pointer(NativeUInt(aStreamNew.Memory) + aStreamNew.Size),
    aStreamOld.Memory, Pointer(NativeUInt(aStreamOld.Memory) + aStreamOld.Size),
    @bufDiff, nil, MATCH_SCORE, PATCH_STEP_SIZE, 0, nil, DLL_THREAD_COUNT);

  DoLog(Format('  .. created %s in %dms', [BytesToStr(aStreamDiff.Size), GetTickCount - t]));
end;}


procedure TKMHDiffPatch.PatchCreate(aStreamOld, aStreamNew: TStream; aStreamDiff: TMemoryStream);
var
  bufOld, bufNew: TStreamInput;
  bufDiff: TStreamOutput;
  t: Cardinal;
  mt: TMTSets;
  matchBlockSize: Integer;
begin
  t := GetTickCount;

  Assert(aStreamDiff.Size = 0, 'Diff stream must be empty');

  bufOld.streamImport := nil;
  bufOld.StreamSize := aStreamOld.Size;
  bufOld.R := funcR;
  bufOld.ss := aStreamOld;

  bufNew.streamImport := nil;
  bufNew.StreamSize := aStreamNew.Size;
  bufNew.R := funcR;
  bufNew.ss := aStreamNew;

  bufDiff.streamImport := nil;
  bufDiff.StreamSize := 0;
  bufDiff.RW := funcRW;
  bufDiff.W := funcW;
  bufDiff.ms := aStreamDiff;

  DoLog(Format('Creating diff_stream for %s <-> %s', [BytesToStr(aStreamOld.Size), BytesToStr(aStreamNew.Size)]));

  mt.threadNum := fThreadCount;
  mt.threadNumForSearch := fThreadCount;
  mt.newDataIsMTSafe := 1;
  mt.oldDataIsMTSafe := 1;
  mt.newAndOldDataIsMTSameRes := 1;

  // Small BlockSize hugely increases diff time for large files (522mb: 6 = 240sec, 64 = 5sec)
  // Big BlockSize greatly affects diff size for average files (14mb: 6 = 2mb, 64 = 9mb)

  //   blockSize         6                64
  //   14mb          2mb / 4sec       9mb / 2.5sec
  //   50mb         17mb /           31mb /
  //   522mb       0.5mb / 240sec!  0.5mb / 5sec

  // Division is empirical at 60mb
  matchBlockSize := IfThen(aStreamOld.Size < 60_000_000, MATCH_BLOCK_SIZE_SMALL, MATCH_BLOCK_SIZE_BIG);

  fDLLCreateDiffStream(@bufNew, @bufOld, @bufDiff, nil, matchBlockSize, PATCH_STEP_SIZE, @mt);

  DoLog(Format('  .. created %s in %dms', [BytesToStr(aStreamDiff.Size), GetTickCount - t]));
end;


procedure TKMHDiffPatch.PatchApply(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
var
  bufOld, bufDiff: TStreamInput;
  bufNew: TStreamOutput;
  res: Integer;
  tc: array of Byte;
  diffInfo: TSingleCompressedDiffInfo;
  t: Cardinal;
begin
  t := GetTickCount;
  DoLog(Format('Applying diff for %s + %s', [BytesToStr(aStreamOld.Size), BytesToStr(aStreamDiff.Size)]));

  bufOld.streamImport := nil;
  bufOld.StreamSize := aStreamOld.Size;
  bufOld.R := funcR;
  bufOld.ss := aStreamOld;

  bufDiff.streamImport := nil;
  bufDiff.StreamSize := aStreamDiff.Size;
  bufDiff.R := funcR;
  bufDiff.ss := aStreamDiff;

  Assert(aStreamNew.Size = 0, 'New stream must be empty');
  bufNew.streamImport := nil;
  bufNew.StreamSize := 0;
  bufNew.RW := funcRW;
  bufNew.W := funcW;
  bufNew.ms := aStreamNew;

  res := fDLLInfoDiff(@diffInfo, @bufDiff, 0);
  if res <> 1 then
    raise Exception.Create('fDLLInfoDiff error - ' + IntToStr(res));

  SetLength(tc, PATCH_STEP_SIZE * 2); // needs to be more than PATCH_STEP_SIZE

  res := fDLLPatchDiff(
    @bufNew, @bufOld, @bufDiff,
    diffInfo.diffDataPos,
    diffInfo.uncompressedSize, diffInfo.compressedSize, nil,
    diffInfo.coverCount,
    diffInfo.stepMemSize,
    @tc[0], @tc[High(tc)],
    nil
  );
  if res <> 1 then
    raise Exception.Create('fDLLPatchDiff error - ' + IntToStr(res));

  DoLog(Format('  .. applied %s in %dms', [BytesToStr(aStreamDiff.Size), GetTickCount - t]));
end;


procedure TKMHDiffPatch.PatchTest(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
var
  msTest: TMemoryStream;
  I: Int64;
begin
  msTest := TMemoryStream.Create;
  try
    PatchApply(aStreamOld, aStreamDiff, msTest);

    if msTest.Size <> aStreamNew.Size then
      raise Exception.Create('Size mismatch');

    DoLog(Format('Verifying diff for %s + %s == %s', [BytesToStr(aStreamOld.Size), BytesToStr(aStreamDiff.Size), BytesToStr(aStreamNew.Size)]));

    // Compare byte-by-byte to avoid fiddling with files whose sizes are not divisible by 2/4/8
    for I := 0 to msTest.Size - 1 do
    if PByte(NativeUInt(msTest.Memory) + I)^ <> PByte(NativeUInt(aStreamNew.Memory) + I)^ then
      raise Exception.Create('Contents mismatch');
  except
    msTest.Free;
  end;
end;


end.
