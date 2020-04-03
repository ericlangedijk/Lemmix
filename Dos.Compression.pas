{-------------------------------------------------------------------------------
  This unit contains:
  • the dos decompression algorithm, based on the c-code of ccexplore.
  • a dos compression algorithm, based on free basic code of Mindless
  • an easy to use sectionlist
-------------------------------------------------------------------------------}

unit Dos.Compression;

{$include lem_directives.inc}

interface

uses
  System.Classes, System.SysUtils, System.Math, System.Contnrs,
  Base.Utils;

type
  TRawBytes = array[0..MaxInt div 16 - 1] of Byte;
  PRawBytes = ^TRawBytes;

  {-------------------------------------------------------------------------------
    This header is at each section of compressed data.
    • Watch out: the words are Big Endian so swap when reading or writing!!!
    • Compressed size includes this 10 bytes header
    • Never change this declaration
  -------------------------------------------------------------------------------}
  TCompressionHeaderRec = packed record
    BitCnt            : Byte;
    Checksum          : Byte;
    Unused1           : Word;
    DecompressedSize  : Word;
    Unused2           : Word;
    CompressedSize    : Word;
  end;

  // TDecompressorStateRec is internally used when decompressing
  TDecompressorStateRec = record
    BitCnt     : Byte;
    CurBits    : Byte;
    Cdata      : PRawBytes;
    Cptr       : Integer;
    Ddata      : PRawBytes;
    Dptr       : Integer;
    Checksum   : Integer;
    CSize      : Integer; // added for read validation
    DSize      : Integer; // added for write validation
  end;

  // two classes to split up dos dat file in sections and keep in mem
  TDosDatSection = class
  private
    fCompressedData    : TMemoryStream;
    fDecompressedData  : TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;
    property CompressedData    : TMemoryStream read fCompressedData;
    property DecompressedData  : TMemoryStream read fDecompressedData;
  end;

  TDosDatSectionList = class(TFastObjectList<TDosDatSection>);

type
  TDosDatDecompressor = class
  private
    fSkipErrors: Boolean;
    function ValidSrc(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
    function ValidDst(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
    function CheckSrc(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
    function CheckDst(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
  protected
    function GetNextBits(n: integer; var State: TDecompressorStateRec): Integer;
    procedure CopyPrevData(aBlocklen, aOffsetSize: Integer; var State: TDecompressorStateRec);
    procedure DumpData(Numbytes: Integer; var State: TDecompressorStateRec);
    function Decompress(aCompData, aDecompData: PRawBytes; aBitCnt: Byte; aCompSize, aDecompSize: Integer): Integer;
  public
  // core routine
    function DecompressSection(SrcStream, DstStream: TStream): Integer;
  // higher
    function LoadSection(SrcStream: TStream; Sec: TDosDatSection; DecompressOnTheFly: Boolean = True): Boolean;
    procedure LoadSectionList(SrcStream: TStream; aList: TDosDatSectionList; DecompressOnTheFly: Boolean = True);
    procedure LoadSectionListFromFile(const aFileName: string; aList: TDosDatSectionList; DecompressOnTheFly: Boolean = True);
    function GetNumberOfSectionsOnly(SrcStream: TStream; out LVLCheck: Boolean): Integer; overload;
    function GetNumberOfSectionsOnly(const aFilename: string; out LVLCheck: Boolean): Integer; overload;
    property SkipErrors: Boolean read fSkipErrors write fSkipErrors;
  end;


type
  TProgressEvent = procedure(aPosition, aMaximum: Integer) of object;

  TDosDatCompressor = class
  private
    DSize       : Word;
    CSize       : Word;
    di          : Word; // index of source data byte
    ci          : Word; // index of dest data byte
    BitCnt      : Byte;
    CData       : PRawBytes;
    DData       : PRawBytes;
    fOnProgress : TProgressEvent;
  protected
    procedure InternalCompress;
    function  FindLongRef(var Len: Word): Word;
    function  FindRef(Len: Word): Word;
    procedure EncodeRawChunk(var Len: Word);
    procedure PushNextBits(n, bits: word);
  public
    procedure CompressFile(const aFileName, aDstFile: string); // obsolete
  // core method
    function Compress(Src, Dst: TStream): Integer;
  // higher methods
    procedure StoreSection(aList: TDosDatSection; DstStream: TStream; CompressOnTheFly: Boolean = True);
    procedure StoreSectionList(aList: TDosDatSectionList; DstStream: TStream; CompressOnTheFly: Boolean = True);
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
  end;

const
  COMPRESSIONHEADER_SIZE = SizeOf(TCompressionHeaderRec);

type
  EDecompressError = class(Exception);

implementation

resourcestring
  SDecompressorSrcError_ddddddd = 'Decompress error. ' +
    'Attempt to read or write source data at index %d. ' +
    'BitCnt=%d, ' +
    'CurBits=%d, ' +
    'Cptr=%d, ' +
    'Dptr=%d, ' +
    'CSize=%d, '  +
    'DSize=%d';

  SDecompressorDstError_ddddddd = 'Decompress error. ' +
    'Attempt to read or write destination data at index %d. ' +
    'BitCnt=%d, ' +
    'CurBits=%d, ' +
    'Cptr=%d, ' +
    'Dptr=%d, ' +
    'CSize=%d, '  +
    'DSize=%d';

{ TDosDatDecompressor }

function TDosDatDecompressor.ValidSrc(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
begin
  Result := (aIndex < State.CSize) and (aIndex >= 0);
end;

function TDosDatDecompressor.ValidDst(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
begin
  Result := (aIndex < State.DSize) and (aIndex >= 0);
end;

function TDosDatDecompressor.CheckSrc(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
begin
  Result := ValidSrc(State, aIndex);
  if not Result and not SkipErrors then
    raise EDecompressError.CreateFmt(SDecompressorSrcError_ddddddd,
      [aIndex, State.BitCnt, State.CurBits, State.Cptr, State.Dptr, State.CSize, State.DSize]);
end;

function TDosDatDecompressor.CheckDst(const State: TDecompressorStateRec; aIndex: Integer): Boolean;
begin
  Result := ValidDst(State, aIndex);
  if not Result and not SkipErrors then
    raise EDecompressError.CreateFmt(SDecompressorDstError_ddddddd,
      [aIndex, State.BitCnt, State.CurBits, State.Cptr, State.Dptr, State.CSize, State.DSize]);
end;

function TDosDatDecompressor.GetNextBits(N: integer; var State: TDecompressorStateRec): Integer;
begin
  Result := 0;
  while n > 0 do begin
    Dec(State.BitCnt);
    if State.BitCnt = 0 then begin
      Dec(State.Cptr);
      CheckSrc(State, State.CPtr);
      State.CurBits := State.Cdata^[State.Cptr];
      State.Checksum := State.CheckSum xor State.Curbits;
      State.BitCnt := 8;
    end;
    Result := Result shl 1;
    Result := Result or (State.CurBits and 1);
    State.CurBits := State.CurBits shr 1;
    Dec(n);
  end;
end;

procedure TDosDatDecompressor.CopyPrevData(aBlocklen, aOffsetSize: Integer; var State: TDecompressorStateRec);
var
  i, Offset: Integer;
begin
  Offset := GetNextBits(aOffsetSize, State);
  for i := 0 to aBlockLen - 1 do begin
    Dec(State.Dptr);
    CheckDst(State, State.DPtr);
    CheckDst(State, State.DPtr + Offset + 1);
    State.Ddata^[State.Dptr] := State.Ddata^[State.Dptr + Offset + 1];
  end;
end;

procedure TDosDatDecompressor.DumpData(Numbytes: Integer; var State: TDecompressorStateRec);
var
  B: Byte;
begin
  while NumBytes > 0 do begin
    Dec(State.Dptr);
    B := Byte(GetNextBits(8, State));
    CheckDst(State, State.DPtr);
    State.Ddata^[State.Dptr] := B;
    Dec(NumBytes);
  end;
end;

function TDosDatDecompressor.Decompress(aCompData, aDecompData: PRawBytes; aBitCnt: Byte; aCompSize, aDecompSize: Integer): Integer;
var
  State: TDecompressorStateRec;
begin
  FillChar(State, SizeOf(State), 0);

  State.BitCnt := aBitCnt + 1;
  State.Cptr := aCompSize - 1;
  State.Dptr := aDecompSize;
  State.Cdata := aCompdata;
  State.Ddata := aDecompData;
  State.CurBits := aCompdata^[State.Cptr];
  State.Checksum := State.CurBits;
  State.CSize := aCompSize;
  State.DSize := aDecompSize;

  while State.Dptr > 0 do
  begin
    if (GetNextBits(1, State) = 1) then begin
      case GetNextBits(2, State) of
        0: CopyPrevData(3, 9, State);
        1: CopyPrevData(4, 10, State);
        2: CopyPrevData(GetNextBits(8, State) + 1, 12, State);
        3: DumpData(GetNextBits(8, State) + 9, State);
      end;
    end
    else begin
      case GetNextBits(1, State) of
        0: DumpData(GetNextBits(3, State) + 1, State);
        1: CopyPrevData(2, 8, State);
      end;
    end;
  end;
  Result := State.Checksum;
end;

function TDosDatDecompressor.DecompressSection(SrcStream, DstStream: TStream): Integer;
{-------------------------------------------------------------------------------
  Code for decompressing one section from source stream to destination stream.
  There can be more sections in a file of course
  • None of the positions of the streams is changed before decompression!
    So be sure you're at the right position.
  • Return value is the number of decompressed bytes.
  • The compression-header must be included in the SrcStream.
  • The data is read en written at the current positions of the streams.
-------------------------------------------------------------------------------}
var
  Rd: Integer;
  Header: TCompressionHeaderRec;
  SrcData, DstData: PRawBytes;
  ComputedChecksum: Integer;
begin
  Assert(COMPRESSIONHEADER_SIZE = 10, 'Program error DecompressSection');
  Result := 0;
  FillChar(Header, SizeOf(Header), 0);
  Rd := SrcStream.Read(Header, COMPRESSIONHEADER_SIZE);
  if Rd <> COMPRESSIONHEADER_SIZE then
    Exit;
  Header.CompressedSize := System.Swap(Header.CompressedSize); // convert from bigendian
  Dec(Header.CompressedSize, COMPRESSIONHEADER_SIZE); // exclude the headersize which is included in this size
  Header.DecompressedSize := System.Swap(Header.DecompressedSize); // convert from bigendian

  GetMem(SrcData, Header.CompressedSize);
  GetMem(DstData, Header.DecompressedSize);
  try
    FillChar(SrcData^, Header.CompressedSize, 0);
    FillChar(DstData^, Header.DecompressedSize, 0);
    SrcStream.ReadBuffer(SrcData^, Header.CompressedSize);
    ComputedChecksum := Decompress(SrcData, DstData, Header.BitCnt, Header.CompressedSize, Header.DecompressedSize);
    if ComputedCheckSum <> Header.Checksum then
      raise EDecompressError.Create('Checksum error occurred during decompression');
    DstStream.WriteBuffer(DstData^, Header.DecompressedSize);
    Result := Header.DecompressedSize;
  finally
    FreeMem(SrcData);
    FreeMem(DstData);
  end;
end;

function TDosDatDecompressor.LoadSection(SrcStream: TStream; Sec: TDosDatSection; DecompressOnTheFly: Boolean): Boolean;
var
  Header: TCompressionHeaderRec;
  Rd: Integer;
  CopySize: Integer;
begin
  Rd := SrcStream.Read(Header, SizeOf(Header));
  Result := Rd <> 0;
  if not Result then
    Exit;
  if Rd <> SizeOf(Header) then
    raise EDecompressError.Create('Decompressor Header Error');
  Header.CompressedSize := System.Swap(Header.CompressedSize);
  CopySize := Header.CompressedSize;
  Dec(Header.CompressedSize, 10); // exclude the headersize which is included in this size
  Header.DecompressedSize := System.Swap(Header.DecompressedSize);
  SrcStream.Seek(-SizeOf(Header), soFromCurrent); // go back for copying in section object data
  Sec.CompressedData.CopyFrom(SrcStream, CopySize);
  if DecompressOnTheFly then begin
    Sec.CompressedData.Seek(0, soFromBeginning);
    DecompressSection(Sec.CompressedData, Sec.DecompressedData);
    Sec.CompressedData.Seek(0, soFromBeginning);
    Sec.DecompressedData.Seek(0, soFromBeginning);
  end;
end;

procedure TDosDatDecompressor.LoadSectionList(SrcStream: TStream; aList: TDosDatSectionList; DecompressOnTheFly: Boolean = True);
{-------------------------------------------------------------------------------
  This is a kind of specialized method to auto split up the file or stream in
  it's sections. Mainly to keep the extraction knowledge in this class.
  Other classes only need to know that there ARE sections and can use this
  method to easily get all data.
  Since in these days this dos file sizes are peanuts we can just cache the
  whole thing without any problem.
  Because it's fast too, I don't think progress feedback is needed for one
  extraction of all sections.
  On error the list is cleared.
-------------------------------------------------------------------------------}
var
  Header: TCompressionHeaderRec;
  Rd: Integer;
  Sec: TDosDatSection;
  CopySize: Integer;
begin
  aList.Clear;
  SrcStream.Seek(0, soFromBeginning);
  repeat
    Rd := SrcStream.Read(Header, SizeOf(Header));
    if Rd = 0 then
      Break;
    if Rd <> SizeOf(Header) then
      raise EDecompressError.Create('Decompressor Header Error');
    Header.CompressedSize := System.Swap(Header.CompressedSize);
    CopySize := Header.CompressedSize;
    Dec(Header.CompressedSize, 10); // exclude the headersize which is included in this size
    Header.DecompressedSize := System.Swap(Header.DecompressedSize);
    SrcStream.Seek(-SizeOf(Header), soFromCurrent); // go back for copying in section object data
    Sec := TDosDatSection.Create;
    try
      Sec.CompressedData.CopyFrom(SrcStream, CopySize);
      if DecompressOnTheFly then begin
        Sec.CompressedData.Seek(0, soFromBeginning);
        DecompressSection(Sec.CompressedData, Sec.DecompressedData);
        Sec.CompressedData.Seek(0, soFromBeginning);
        Sec.DecompressedData.Seek(0, soFromBeginning);
      end
      else begin
        Sec.CompressedData.Seek(0, soFromBeginning);
        Sec.DecompressedData.Seek(0, soFromBeginning);
      end;
    except
      Sec.Free;
      aList.Clear;
      raise;
    end;
    aList.Add(Sec);
  until False;
end;

function TDosDatDecompressor.GetNumberOfSectionsOnly(SrcStream: TStream; out LVLCheck: Boolean): Integer;
// fast determination of number of section and determine if all sections are levels
var
  Header: TCompressionHeaderRec;
  Rd, CopySize: Integer;
begin
  LVLCheck := True;
  Result := 0;
  SrcStream.Seek(0, soFromBeginning);
  repeat
    Rd := SrcStream.Read(Header, SizeOf(Header));
    if Rd = 0 then
      Break;
    if Rd <> SizeOf(Header) then
      raise EDecompressError.Create('GetNumberOfSectionsOnly Header Error');
    Inc(Result);
    Header.CompressedSize := System.Swap(Header.CompressedSize);
    if Header.CompressedSize = 0 then
      Break;
    CopySize := Header.CompressedSize;
    Dec(Header.CompressedSize, 10); // exclude the headersize which is included in this size
    Header.DecompressedSize := System.Swap(Header.DecompressedSize);
    LVLCheck := LVLCheck and (Header.DecompressedSize = 2048);
    SrcStream.Seek(-SizeOf(Header), soFromCurrent); // go back included header size
    SrcStream.Seek(CopySize, soFromCurrent); // go to next header
  until False;
  LVLCheck := LVLCheck and (Result > 0)
end;

function TDosDatDecompressor.GetNumberOfSectionsOnly(const aFilename: string; out LVLCheck: Boolean): Integer;
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    Result := GetNumberOfSectionsOnly(F, LVLCheck);
  finally
    F.Free;
  end;
end;

procedure TDosDatDecompressor.LoadSectionListFromFile(const aFileName: string; aList: TDosDatSectionList; DecompressOnTheFly: Boolean);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    LoadSectionList(F, aList, DecompressOnTheFly);
  finally
    F.Free;
  end;
end;

{ TDosDatSection }

constructor TDosDatSection.Create;
begin
  inherited;
  fCompressedData := TMemoryStream.Create;
  fDecompressedData := TMemoryStream.Create;
end;

destructor TDosDatSection.Destroy;
begin
  fCompressedData.Free;
  fDecompressedData.Free;
  inherited;
end;

{ TDosDatCompressor }

procedure TDosDatCompressor.InternalCompress;
// not used
//  Original code of Mindless
var
  Ref, Len, Raw: Word;
begin
  di := 0;
  ci := 0;
  BitCnt := 8;
  Raw := 0;

  repeat
    //Ref := 0;
    Len := 0;
    Ref := FindLongRef(Len);

    if Ref <> 0 then begin
      EncodeRawChunk(Raw);
      PushNextBits(12, Ref - 1);
      PushNextBits(8, Len);
      PushNextBits(3, 6); // 110b
      Inc(di, Len + 1);
    end
    else begin
      Ref := FindRef(4);
      if Ref <> 0 then begin
        EncodeRawChunk(Raw);
        PushNextBits(10, Ref - 1);
        PushNextBits(3, 5); // 101b
        Inc(di, 4);
      end
      else begin
        Ref := FindRef(3);
        if Ref <> 0 then begin
          EncodeRawChunk(Raw);
          PushNextBits(9, Ref - 1);
          PushNextBits(3, 4); // 100b
          Inc(di, 3);
        end
        else begin
          Ref := FindRef(2);
          if Ref <> 0 then begin
            EncodeRawChunk(Raw);
            PushNextBits(8, Ref - 1);
            PushNextBits(2, 1); // 01b
            Inc(di, 2);
          end;
        end;
      end;
    end;

    if Ref = 0 then
    begin
      PushNextBits(8, DData^[di]);
      Inc(Raw);
      if Raw = 264 then
        EncodeRawChunk(Raw);
      Inc(di);
      //deb([di, raw]);
    end;

    if Assigned(fOnProgress) then
      if di mod 8 = 0 then   //totally random which says: do not call too often
        fOnProgress(di, DSize);

  until di > DSize;

  EncodeRawChunk(Raw);
end;

function TDosDatCompressor.FindLongRef(var Len: Word): Word;
// not used
//  Find a chunk of duplicate sourcedata
{ WOW for the VAR Len}
var
  MaxDst, MaxLen, i, L, Ref: word;
begin
  MaxLen := 1 shl 8; // 256
  MaxDst := 1 shl 12; // 4096
  i := di + 1;
  Ref := 0;

  while i < di + MaxDst do
  begin
    for L := 0 to MaxLen - 1 do begin
      if (i + L > DSize) then
        Break;
      if (DData^[di + L] <> DData^[i + L]) then
        Break;
      if L > Len then begin
        Ref := i - di;
        Len := L;
      end;
    end;
    Inc(i);
  end;

  if Len < 5 then
    Ref := 0;
  Result := Ref;
end;

function TDosDatCompressor.FindRef(Len: Word): Word;
// not used
var
  MaxDst, i, L, Ref: Word;
begin
  MaxDst := 0; // stupid compiler warning

  case Len of
    2: MaxDst := 1 shl 8;  // 256
    3: MaxDst := 1 shl 9;  // 512
    4: MaxDst := 1 shl 10; // 1024
  end;

  i := di + 1;
  Ref := 0;

  while (i < di + MaxDst) and (Ref = 0) and (i + Len < DSize) do begin
    for L := 0 to Len - 1 do begin
      if (DData^[di + L] <> DData^[i + L]) then
        Break
      else if L = len - 1 then
        Ref := i - di;
    end;
    Inc(i);
  end;

  Result := Ref;
end;

procedure TDosDatCompressor.EncodeRawChunk(var Len: Word);
// WOW for the VAR Len
begin
  if Len = 0 then
    Exit;
  if Len >= 9 then begin
    PushNextBits(8, Len - 9);
    PushNextBits(3, 7); // 111b
  end
  else begin
    PushNextBits(3, Len - 1);
    PushNextBits(2, 0); // 00b
  end;
  Len := 0;
end;

procedure TDosDatCompressor.PushNextBits(N, Bits: Word);
begin
  repeat
    if BitCnt = 0 then begin
      Inc(ci);
      if ci > CSize then begin
        CSize := ci;
        ReallocMem(CData, CSize + 1);
      end;
      BitCnt := 8;
    end;

    CData^[ci] := CData^[ci] shl 1;
    CData^[ci] := CData^[ci] or (Bits and 1); // if ci < 5 then deb(['ci,cdata[ci],bitcnt',ci, cdata^[ci],bit8str(cdata^[ci]), bitcnt]);
    Bits := Bits shr 1;
    Dec(BitCnt);
    Dec(n);
  until n = 0;
end;


procedure TDosDatCompressor.CompressFile(const aFileName, aDstFile: string);
var
  F: TBufferedFileStream;
  Header: TCompressionHeaderRec;

  procedure savetotemp;
  var
    t: tBufferedfilestream;
  begin
    t := tBufferedfilestream.create(adstFile, fmcreate);
    try
      t.write(header, sizeof(header));
      t.write(cdata^, csize + 1);
    finally
      t.free;
    end;
  end;

var
  i: Integer;

begin
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    FillChar(Header, SizeOf(Header), 0);
    DSize := F.Size - 1;
    CSize := DSize;
    GetMem(DData, DSize + 1);
    GetMem(CData, CSize + 1);
    FillChar(DData^, DSize + 1, 0);
    FillChar(CData^, CSize + 1, 0);

    try
      F.Read(DData^, DSize + 1);
      InternalCompress;
      CSize := ci;
      ReallocMem(cdata, CSize + 1);
      Header.BitCnt := 8 - BitCnt;
      Header.CheckSum := 0;
      for i := 0 to CSize do
        Header.CheckSum := Header.Checksum xor CData^[i];
      Header.DecompressedSize := DSize + 1;
      Header.CompressedSize := ci + 10 + 1;
      // convert little-endian to big-endian
      Header.DecompressedSize := System.Swap(header.DecompressedSize);
      Header.CompressedSize := System.Swap(header.CompressedSize);
      Savetotemp;
    finally
      FreeMem(DData);
      FreeMem(CData);
      CData := nil;
      DData := nil;
    end;

  finally
    F.Free;
  end;

end;

function TDosDatCompressor.Compress(Src, Dst: TStream): Integer;
var
  Header: TCompressionHeaderRec;
  NumBytes: Integer;
  i: Integer;
begin
  //Result := 0;
  NumBytes := Src.Size - Src.Position;
  FillChar(Header, SizeOf(Header), 0);
  DSize := Numbytes - 1;
  CSize := DSize;
  GetMem(DData, DSize + 1);
  GetMem(CData, CSize + 1);
  FillChar(DData^, DSize + 1, 0);
  FillChar(CData^, CSize + 1, 0);

  try
    Src.ReadBuffer(DData^, DSize + 1);
    InternalCompress;
    CSize := ci;
    ReallocMem(CData, CSize + 1);
    Header.BitCnt := 8 - BitCnt;
    Header.CheckSum := 0;
    for i := 0 to CSize do
      Header.CheckSum := Header.Checksum xor CData^[i];
    Header.DecompressedSize := DSize + 1;
    Header.CompressedSize := ci + 10 + 1;
    {
      if I remember correctly from the past, the WinLemmings had a checksum error somewhere (!)
      windlg(['checksum = ' + hexstr(header.checksum), 'decompressed size = ' + i2s(dsize+1), 'compressed size = ' + i2s(ci+1)]);
    }

    // convert little-endian to big-endian
    Header.DecompressedSize := System.Swap(Header.DecompressedSize);
    Header.CompressedSize := System.Swap(Header.CompressedSize);
    Dst.WriteBuffer(Header, COMPRESSIONHEADER_SIZE);
    Dst.WriteBuffer(CData^, CSize + 1);
    Result := CSize + 1 + COMPRESSIONHEADER_SIZE;
  finally
    FreeMem(DData);
    FreeMem(CData);
    CData := nil;
    DData := nil;
  end;
end;

procedure TDosDatCompressor.StoreSection(aList: TDosDatSection; DstStream: TStream; CompressOnTheFly: Boolean);
begin
  Throw('StoreSection not implemented yet');
end;

procedure TDosDatCompressor.StoreSectionList(aList: TDosDatSectionList; DstStream: TStream; CompressOnTheFly: Boolean = True);
//  Write compresseddata of each section item to stream.
var
  Sec: TDosDatSection;
begin
  for sec in aList do begin
    if CompressOnTheFly then begin
      Sec.DecompressedData.Seek(0, soFromBeginning);
      Sec.CompressedData.Clear;
      Compress(Sec.DecompressedData, Sec.CompressedData);
    end;
    Sec.CompressedData.Seek(0, soFromBeginning);
    DstStream.CopyFrom(Sec.CompressedData, Sec.CompressedData.Size);
  end;
end;

end.

