unit KM_HDiffPatch;
interface
uses
  Classes, Math, SysUtils, Windows;


type
{
  struct hpatch_TStreamInput{
    void*               streamImport;
    hpatch_StreamPos_t  streamSize; //stream size,max readable range;
    hpatch_BOOL         (*read)(const struct hpatch_TStreamInput* stream,hpatch_StreamPos_t readFromPos, unsigned char* out_data,unsigned char* out_data_end);
    void*               _private_reserved;

  struct hpatch_TStreamOutput {
    void*               streamImport;
    hpatch_StreamPos_t  streamSize; //stream size,max writable range; not is write pos!
    hpatch_BOOL         (*read_writed)(const struct hpatch_TStreamOutput* stream,hpatch_StreamPos_t readFromPos, unsigned char* out_data,unsigned char* out_data_end);
    hpatch_BOOL         (*write)(const struct hpatch_TStreamOutput* stream,hpatch_StreamPos_t writeToPos, const unsigned char* data,const unsigned char* data_end);
}

  PStreamInput = ^TStreamInput;
  PStreamOutput = ^TStreamOutput;
  TReadFunc = function (const aStream: PStreamInput; readFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
  TReadWriteFunc = function (const aStream: PStreamOutput; aReadFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
  TWriteFunc = function (const aStream: PStreamOutput; aWriteToPos: UInt64; aData, aDataEnd: Pointer): Integer; cdecl;
  TSI = procedure; cdecl;

  TStreamInput = record
    streamImport: TSI;
    StreamSize: UInt64;
    R: TReadFunc;
    ss: TStream; // Reference
  end;

  TStreamOutput = record
    streamImport: TSI;
    StreamSize: UInt64;
    RW: TReadWriteFunc;
    W: TWriteFunc;
    ms: TMemoryStream; // Reference
  end;

{
  create_single_compressed_diff(
    const unsigned char* newData,const unsigned char* newData_end,
    const unsigned char* oldData,const unsigned char* oldData_end,
    const hpatch_TStreamOutput* out_diff,
    const hdiff_TCompress* compressPlugin=0,
    int kMinSingleMatchScore=kMinSingleMatchScore_default,
    size_t patchStepMemSize=kDefaultPatchStepMemSize,
    bool isUseBigCacheMatch=false,
    ICoverLinesListener* listener=0,
    size_t threadNum=1)
}

  TDLLCreateDiff = procedure(const aNewData, aNewDataEnd, aOldData, aOldDataEnd: Pointer; const aOutDiff: PStreamOutput;
    const hdiff_TCompress: Pointer; kMinSingleMatchScore: Integer; patchStepMemSize: Cardinal; isUseBigCacheMatch: Byte;
    ICoverLinesListener: Pointer; threadNum: Cardinal); cdecl;

{
  create_single_compressed_diff_stream(const hpatch_TStreamInput*  newData,
                                          const hpatch_TStreamInput*  oldData,
                                          const hpatch_TStreamOutput* out_diff,
                                          const hdiff_TCompress* compressPlugin=0,
                                          size_t kMatchBlockSize=kMatchBlockSize_default,
                                          size_t patchStepMemSize=kDefaultPatchStepMemSize,
                                          const hdiff_TMTSets_s* mtsets=0);
}

{
  struct hdiff_TMTSets_s{ // used by $hdiff -s
        size_t threadNum;
        size_t threadNumForSearch; // NOTE: muti-thread search need frequent random disk read
        bool   newDataIsMTSafe;
        bool   oldDataIsMTSafe;
        bool   newAndOldDataIsMTSameRes; //for dir diff
}

  PMTSets = ^TMTSets;
  TMTSets = record
    threadNum: Cardinal;
    threadNumForSearch: Cardinal;
    newDataIsMTSafe: Integer;
    oldDataIsMTSafe: Integer;
    newAndOldDataIsMTSameRes: Integer;
  end;

  TDLLCreateDiffStream = procedure(const aNewData, aOldData: PStreamInput; const aOutDiff: PStreamOutput;
    const hdiff_TCompress: Pointer; kMatchBlockSize: Integer; patchStepMemSize: Cardinal; mtsets: PMTSets); cdecl;

{
  hpatch_BOOL patch_single_compressed_diff(
    const hpatch_TStreamOutput* out_newData,          //sequential write
    const hpatch_TStreamInput*  oldData,              //random read
    const hpatch_TStreamInput*  singleCompressedDiff, //sequential read
    hpatch_StreamPos_t          diffData_pos, //diffData begin pos in singleCompressedDiff
    hpatch_StreamPos_t          uncompressedSize,
    hpatch_StreamPos_t          compressedSize,
    hpatch_TDecompress*         decompressPlugin,
    hpatch_StreamPos_t coverCount,
    hpatch_size_t stepMemSize,
    unsigned char* temp_cache, unsigned char* temp_cache_end,
    sspatch_coversListener_t* coversListener //default NULL, call by on got covers
   )
}

  TDLLPatchDiff = function(const aNewData: PStreamOutput; const aOldData: PStreamInput; const aDiff: PStreamInput;
    diffData_pos: UInt64; uncompressedSize: UInt64; compressedSize: UInt64; decompressPlugin: Pointer; coverCount: UInt64;
    stepMemSize: Cardinal; temp_cache: Pointer; temp_cache_end: Pointer; coversListener: Pointer): Integer; cdecl;

{
  hpatch_singleCompressedDiffInfo
    hpatch_StreamPos_t  newDataSize;
    hpatch_StreamPos_t  oldDataSize;
    hpatch_StreamPos_t  uncompressedSize;
    hpatch_StreamPos_t  compressedSize;
    hpatch_StreamPos_t  diffDataPos;
    hpatch_StreamPos_t  coverCount;
    hpatch_StreamPos_t  stepMemSize;
    char                compressType[hpatch_kMaxPluginTypeLength+1]; //ascii cstring

  hpatch_BOOL getSingleCompressedDiffInfo(
    hpatch_singleCompressedDiffInfo* out_diffInfo,
    const hpatch_TStreamInput*  singleCompressedDiff,   //sequential read
    hpatch_StreamPos_t diffInfo_pos//default 0, begin pos in singleCompressedDiff
  );
}

  PSingleCompressedDiffInfo = ^TSingleCompressedDiffInfo;
  TSingleCompressedDiffInfo = record
    newDataSize: UInt64;
    oldDataSize: UInt64;
    uncompressedSize: UInt64;
    compressedSize: UInt64;
    diffDataPos: UInt64;
    coverCount: UInt64;
    stepMemSize: UInt64;
    compressType: array [0..260] of AnsiChar;
  end;

  TDLLInfoDiff = function(aDiffInfo: PSingleCompressedDiffInfo; const aDiff: PStreamInput; aDiffInfoPos: UInt64): Integer; cdecl;


  TKMHDiffPatch = class
  private const
    MATCH_SCORE = 4; // DEFAULT -m-6, recommended bin: 0--4 text: 4--9 etc...
    MATCH_BLOCK_SIZE_SMALL = 8; // DEFAULT 1<<6 recommended (1<<4)--(1<<14)
    MATCH_BLOCK_SIZE_BIG = 64; // DEFAULT 1<<6 recommended (1<<4)--(1<<14)
    PATCH_STEP_SIZE = 1024 * 256; // DEFAULT -SD-256k, recommended 64k,2m etc...
    DLL_THREAD_COUNT = 4; // DEFAULT -p-4; requires more memory!
  private
    fOnLog: TProc<string>;
    fLibHandle: NativeUInt;
    fDLLCreateDiff: TDLLCreateDiff;
    fDLLCreateDiffStream: TDLLCreateDiffStream;
    fDLLInfoDiff: TDLLInfoDiff;
    fDLLPatchDiff: TDLLPatchDiff;
    procedure DoLog(const aText: string);
    procedure LoadDLL(const aDLLPath: string);
    procedure TestDLL1;
    procedure TestDLL2;
    procedure TestDLL_Stream;
  public
    constructor Create(aOnLog: TProc<string>);
    destructor Destroy; override;

    procedure CreateDiff(aStreamOld, aStreamNew, aStreamDiff: TMemoryStream);
    procedure CreateDiffStream(aStreamOld, aStreamNew: TStream; aStreamDiff: TMemoryStream);
    procedure ApplyPatch(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
    procedure TestPatch(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
  end;


implementation


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


function BytesToStr(aCount: Integer): string;
begin
  if aCount < 1000 then
    Result := IntToStr(aCount) + 'b'
  else
  if aCount < 1000000 then
    Result := IntToStr(Round(aCount / 1000)) + 'kb'
  else
  if aCount < 1000000000 then
    Result := IntToStr(Round(aCount / 1000000)) + 'mb';
end;


{ TKMHDiffPatch }
constructor TKMHDiffPatch.Create(aOnLog: TProc<string>);
begin
  inherited Create;

  fOnLog := aOnLog;

  // Load DLL dynamically, so we could move it into the utility folder
  LoadDLL('hdiffz.dll');

  TestDLL1;
  TestDLL2;
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
  DoLog(Format('Linked create_single_compressed_diff at "$%.8x"', [PCardinal(Addr(fDLLCreateDiff))^]));
  DoLog(Format('Linked create_single_compressed_diff_stream at "$%.8x"', [PCardinal(Addr(fDLLCreateDiffStream))^]));
  DoLog(Format('Linked getSingleCompressedDiffInfo at "$%.8x"', [PCardinal(Addr(fDLLInfoDiff))^]));
  DoLog(Format('Linked patch_single_compressed_diff at "$%.8x"', [PCardinal(Addr(fDLLPatchDiff))^]));
end;


procedure TKMHDiffPatch.TestDLL1;
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
end;


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

    CreateDiffStream(msOld, msNew, msDiff);

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


procedure TKMHDiffPatch.CreateDiff(aStreamOld, aStreamNew, aStreamDiff: TMemoryStream);
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
end;


procedure TKMHDiffPatch.CreateDiffStream(aStreamOld, aStreamNew: TStream; aStreamDiff: TMemoryStream);
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

  mt.threadNum := DLL_THREAD_COUNT;
  mt.threadNumForSearch := DLL_THREAD_COUNT;
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


procedure TKMHDiffPatch.ApplyPatch(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
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


procedure TKMHDiffPatch.TestPatch(aStreamOld, aStreamDiff, aStreamNew: TMemoryStream);
var
  msTest: TMemoryStream;
  I: Int64;
begin
  msTest := TMemoryStream.Create;
  try
    ApplyPatch(aStreamOld, aStreamDiff, msTest);

    if msTest.Size <> aStreamNew.Size then
      raise Exception.Create('Error Message');

    DoLog(Format('Verifying diff for %s + %s == %s', [BytesToStr(aStreamOld.Size), BytesToStr(aStreamDiff.Size), BytesToStr(aStreamNew.Size)]));

    for I := 0 to msTest.Size - 1 do
    if PByte(NativeUInt(msTest.Memory) + I)^ <> PByte(NativeUInt(aStreamNew.Memory) + I)^ then
      raise Exception.Create('Error Message');
  except
    msTest.Free;
  end;
end;


end.
