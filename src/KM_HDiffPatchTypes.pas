unit KM_HDiffPatchTypes;
interface
uses
  Classes;


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


implementation


end.
