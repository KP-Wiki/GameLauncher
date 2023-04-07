unit KM_HDiffPatch;
interface
uses
  Classes, Math, SysUtils, Windows;


type
{
  struct hpatch_TStreamOutput {
    void*            streamImport;
    hpatch_StreamPos_t streamSize; //stream size,max writable range; not is write pos!
    //read_writed for ReadWriteIO, can null!
    hpatch_BOOL     (*read_writed)(const struct hpatch_TStreamOutput* stream,hpatch_StreamPos_t readFromPos, unsigned char* out_data,unsigned char* out_data_end);
    //write() must wrote (out_data_end-out_data), otherwise error return hpatch_FALSE
    hpatch_BOOL           (*write)(const struct hpatch_TStreamOutput* stream,hpatch_StreamPos_t writeToPos, const unsigned char* data,const unsigned char* data_end);
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

  Phpatch_TStreamOutput = ^hpatch_TStreamOutput;
  TSI = procedure; cdecl;
  TRW = function (const aStream: Phpatch_TStreamOutput; readFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
  TW = function (const aStream: Phpatch_TStreamOutput; writeToPos: UInt64; aData, aDataEnd: Pointer): Integer; cdecl;
  hpatch_TStreamOutput = record
    streamImport: TSI;
    StreamSize: UInt64;
    RW: TRW;
    W: TW;
    s: AnsiString; // Our field
  end;

  TDLLCreateDiff = procedure(const aNewData, aNewDataEnd, aOldData, aOldDataEnd: Pointer;
    const aOutDiff: Phpatch_TStreamOutput;
    const hdiff_TCompress: Pointer;
    kMinSingleMatchScore: Integer;
    patchStepMemSize: Cardinal;
    isUseBigCacheMatch: Byte;
    ICoverLinesListener: Pointer;
    threadNum: Cardinal); cdecl;

{
  struct hpatch_TStreamInput{
    void*            streamImport;
    hpatch_StreamPos_t streamSize; //stream size,max readable range;
    //read() must read (out_data_end-out_data), otherwise error return hpatch_FALSE
    hpatch_BOOL            (*read)(const struct hpatch_TStreamInput* stream,hpatch_StreamPos_t readFromPos, unsigned char* out_data,unsigned char* out_data_end);
    void*        _private_reserved;

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

  Phpatch_TStreamInput = ^hpatch_TStreamInput;
  TR = function (const aStream: Phpatch_TStreamInput; readFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
  hpatch_TStreamInput = record
    streamImport: TSI;
    StreamSize: UInt64;
    R: TR;
    s: AnsiString; // Our field
  end;

  TDLLPatchDiff = function(
    const aNewData: Phpatch_TStreamOutput;
    const aOldData: Phpatch_TStreamInput;
    const aDiff: Phpatch_TStreamInput;
    diffData_pos: UInt64;
    uncompressedSize: UInt64;
    compressedSize: UInt64;
    decompressPlugin: Pointer;
    coverCount: UInt64; stepMemSize: Cardinal;
    temp_cache: Pointer; temp_cache_end: Pointer;
    coversListener: Pointer
  ): Integer; cdecl;

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

  Phpatch_singleCompressedDiffInfo = ^Thpatch_singleCompressedDiffInfo;
  Thpatch_singleCompressedDiffInfo = record
    newDataSize: UInt64;
    oldDataSize: UInt64;
    uncompressedSize: UInt64;
    compressedSize: UInt64;
    diffDataPos: UInt64;
    coverCount: UInt64;
    stepMemSize: UInt64;
    compressType: array [0..260] of AnsiChar; //ascii cstring
  end;

  TDLLInfoDiff = function(
    out_diffInfo: Phpatch_singleCompressedDiffInfo;
    const aDiff: Phpatch_TStreamInput;
    diffInfo_pos: UInt64
  ): Integer; cdecl;


  TKMHDiffPatch = class
  private
    fOnLog: TProc<string>;
    fLibHandle: NativeUInt;
    fDLLCreateDiff: TDLLCreateDiff;
    fDLLInfoDiff: TDLLInfoDiff;
    fDLLPatchDiff: TDLLPatchDiff;
    procedure DoLog(const aText: string);
    procedure LoadDLL(aDLLPath: string);
    procedure TestDLL;
  public
    constructor Create(aOnLog: TProc<string>);

    procedure TestDLLDiff(aStreamOld, aStreamNew, aStreamDiff: TMemoryStream);
    procedure TestDLLPatch(aStreamOld, aStreamDiff, aStreamNew: TStream);
  end;


implementation


function funcRW(const aStream: Phpatch_TStreamOutput; readFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
begin
  Assert(False);

  Result := MaxInt;
end;


function funcW(const aStream: Phpatch_TStreamOutput; writeToPos: UInt64; aData, aDataEnd: Pointer): Integer; cdecl;
var
  len: UInt64;
  s: AnsiString;
begin
  len := Cardinal(aDataEnd) - Cardinal(aData);

  SetLength(s, len);
  Move(aData^, s[1], len);

  SetLength(aStream.s, Max(UInt64(Length(aStream.s)), writeToPos + len));

  Move(aData^, aStream.s[writeToPos + 1], len);

  aStream.StreamSize := Length(aStream.s);

  Result := len;
end;


function funcR(const aStream: Phpatch_TStreamInput; readFromPos: UInt64; aOutData, aOutDataEnd: Pointer): Integer; cdecl;
var
  len: UInt64;
  s: AnsiString;
begin
  len := Cardinal(aOutDataEnd) - Cardinal(aOutData);

  Move(aStream.s[readFromPos + 1], aOutData^, len);

  SetLength(s, len);
  Move(aOutData^, s[1], len);

  Result := len;
end;


{ TKMHDiffPatch }
constructor TKMHDiffPatch.Create(aOnLog: TProc<string>);
begin
  inherited Create;

  fOnLog := aOnLog;

  // Load DLL dynamically, so we could move it into the utility folder
  LoadDLL('hdiffz.dll');

  TestDLL;
end;


procedure TKMHDiffPatch.DoLog(const aText: string);
begin
  fOnLog(aText);
end;


procedure TKMHDiffPatch.LoadDLL(aDLLPath: string);
var
  err: Cardinal;
begin
  DoLog(Format('Loading "%s"', [aDLLPath]));

  // Load without displaying any popup error messages
  fLibHandle := SafeLoadLibrary(aDLLPath, $FFFF);
  if fLibHandle = 0 then
    raise Exception.Create(Format('DLL was NOT loaded - %d', [GetLastError]));

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('Error in the DLL loading - %d', [err]));

  fDLLCreateDiff := GetProcAddress(fLibHandle, 'create_single_compressed_diff');
  fDLLInfoDiff := GetProcAddress(fLibHandle, 'getSingleCompressedDiffInfo');
  fDLLPatchDiff := GetProcAddress(fLibHandle, 'patch_single_compressed_diff');
  DoLog(Format('Loaded create_single_compressed_diff at "$%.8x"', [PCardinal(Addr(fDLLCreateDiff))^]));
  DoLog(Format('Loaded getSingleCompressedDiffInfo at "$%.8x"', [PCardinal(Addr(fDLLInfoDiff))^]));
  DoLog(Format('Loaded patch_single_compressed_diff at "$%.8x"', [PCardinal(Addr(fDLLPatchDiff))^]));
end;


procedure TKMHDiffPatch.TestDLL;
const
  OLDTEXT = '0123456789';
  NEWTEXT = '0123401234';
var
  msOld, msNew, msDiff: TMemoryStream;
  oldString, newString: AnsiString;
begin
  // Create test diff
  begin
    oldString := OLDTEXT;
    msOld := TMemoryStream.Create;
    msOld.Write(oldString[1], 10);

    newString := NEWTEXT;
    msNew := TMemoryStream.Create;
    msNew.Write(newString[1], 10);

    msDiff := TMemoryStream.Create;

    TestDLLDiff(msOld, msNew, msDiff);

    msDiff.SaveToFile('hdiffz_out_dll.txt');
    DoLog(Format('DLL test diff size - %d', [msDiff.Size]));

    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;

  // Apply test patch
  begin
    oldString := OLDTEXT;
    msOld := TMemoryStream.Create;
    msOld.Write(oldString[1], 10);

    msDiff := TMemoryStream.Create;
    msDiff.LoadFromFile('hdiffz_out_dll.txt');

    msNew := TMemoryStream.Create;

    TestDLLPatch(msOld, msDiff, msNew);

    msNew.Position := 0;
    SetLength(newString, msNew.Size);
    msNew.Read(newString[1], msNew.Size);
    DoLog(Format('DLL test patched data - "%s"', [newString]));

    msOld.Free;
    msNew.Free;
    msDiff.Free;
  end;
end;


procedure TKMHDiffPatch.TestDLLDiff(aStreamOld, aStreamNew, aStreamDiff: TMemoryStream);
var
  bufDiff: hpatch_TStreamOutput;
begin
  bufDiff.streamImport := nil;
  bufDiff.StreamSize := 0;
  bufDiff.RW := funcRW;
  bufDiff.W := funcW;

  fDLLCreateDiff(
    aStreamNew.Memory, Pointer(Cardinal(aStreamNew.Memory) + aStreamNew.Size),
    aStreamOld.Memory, Pointer(Cardinal(aStreamOld.Memory) + aStreamOld.Size),
    @bufDiff, nil, 6, 1024*256, 0, nil, 1);

  aStreamDiff.Write(bufDiff.s[1], Length(bufDiff.s));
end;


procedure TKMHDiffPatch.TestDLLPatch(aStreamOld, aStreamDiff, aStreamNew: TStream);
var
  bufOld, bufDiff: hpatch_TStreamInput;
  bufNew: hpatch_TStreamOutput;
  res: Integer;
  tc: array [0..1024*1024] of Byte;
  diffInfo: Thpatch_singleCompressedDiffInfo;
begin
  bufOld.streamImport := nil;
  bufOld.StreamSize := aStreamOld.Size;
  bufOld.R := funcR;
  SetLength(bufOld.s, aStreamOld.Size);
  aStreamOld.Read(bufOld.s[1], aStreamOld.Size);

  SetLength(bufDiff.s, aStreamDiff.Size);
  aStreamDiff.Read(bufDiff.s[1], aStreamDiff.Size);
  bufDiff.streamImport := nil;
  bufDiff.StreamSize := aStreamDiff.Size;
  bufDiff.R := funcR;

  bufNew.streamImport := nil;
  bufNew.StreamSize := 0;
  bufNew.RW := funcRW;
  bufNew.W := funcW;

  res := fDLLInfoDiff(@diffInfo, @bufDiff, 0);
  if res <> 1 then
    raise Exception.Create('fDLLInfoDiff error - ' + IntToStr(res));

  res := fDLLPatchDiff(
    @bufNew, @bufOld, @bufDiff,
    diffInfo.diffDataPos,
    diffInfo.uncompressedSize, diffInfo.compressedSize, nil,
    diffInfo.coverCount,
    diffInfo.stepMemSize,
    @tc[0], @tc[1024*1024], {needs to be more than 1024*256}
    nil
  );
  if res <> 1 then
    raise Exception.Create('fDLLPatchDiff error - ' + IntToStr(res));

  aStreamNew.Write(bufNew.s[1], Length(bufNew.s));
end;


end.
