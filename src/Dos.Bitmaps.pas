unit Dos.Bitmaps;


{$include lem_directives.inc}

interface

uses
  System.Classes, System.Types, System.SysUtils, System.Contnrs, System.Math,
  GR32, GR32_OrdinalMaps,
  Dos.Compression, Dos.Structures,
  Base.Utils, Base.Bitmaps, Base.Strings,
  Prog.Data;

type
  TLemmixPalette = record
  end;

  {-------------------------------------------------------------------------------
    planar bitmaps in the dosfiles are stored per plane, not interlaced
  -------------------------------------------------------------------------------}
  TDosPlanarBitmap = record
  public
    class procedure GetByteMap(S: TStream; aByteMap: TByteMap; aPos, aWidth, aHeight: Integer; BPP: Byte); static;
    class procedure LoadFromFile(const aFilename: string; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer; BPP: Byte; const aPalette: TArrayOfColor32); overload; static;
    class procedure LoadFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer; BPP: Byte; const aPalette: TArrayOfColor32); overload; static;
    class procedure LoadAnimationFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight, aFrameCount: Integer; BPP: Byte; const aPalette: TArrayOfColor32); static;
  end;

  {-------------------------------------------------------------------------------
    class to extract the "special" bitmaps from dos-files (for the "special" levels).
    the class can also create vgaspec-files
  -------------------------------------------------------------------------------}
  TVgaSpecBitmap = class
  private
    fOnCompressProgress: TProgressEvent;
  { reading methods}
    procedure GetSectionsAndPalette(Src, Dst: TStream; var Pal: TDosVGAPalette8);
  { writing methods }
  {$HINTS OFF} // stupid hints I want these in here
    procedure DoCompressProgress(aPosition, aMaximum: Integer);
    procedure CheckBitmap(aBitmap: TBitmap32);
    procedure CreatePal(aBitmap: TBitmap32; var Pal: TDosVgaSpecPaletteHeader);
  {$HINTS ON}
  public
  { reading methods }
    function DecodeSection(var Src, Dst: PRawBytes; SrcSize: Integer): Integer;
    procedure LoadFromFile(const aFileName: string; aBitmap: TBitmap32);
    procedure LoadFromStream(S: TStream; aBitmap: TBitmap32);
    procedure LoadPaletteFromFile(const aFileName: string; out Pal: TDosVGAPalette8);
    procedure LoadPaletteFromStream(S: TStream; out Pal: TDosVGAPalette8);
  { writing methods }
    function EncodeSection(var Src, Dst: PRawBytes; SrcSize: Integer): Integer;
    procedure EncodeSectionStream(Src, Dst: TStream);
    procedure SaveToFile(aBitmap: TBitmap32; const aFileName: string);
    procedure SaveToStream(aBitmap: TBitmap32; Dst: TStream);
  { progress when compressing, this takes a few seconds! }
    property OnCompressProgress: TProgressEvent read fOnCompressProgress write fOnCompressProgress;
  end;


function DosPaletteEntryToColor32(Red, Green, Blue: Byte): TColor32; overload; inline;
function DosPaletteEntryToColor32(const Entry: TDOSVGAColorRec): TColor32; overload; inline;
function DosPaletteToArrayOfColor32(const Pal: TDosVGAPalette8): TArrayOfColor32; inline;
function Color32ToDosPaletteEntry(C: TColor32): TDOSVGAColorRec; inline;

const
  VGASPEC_SECTIONSIZE = 14400;

implementation


function DosPaletteEntryToColor32(Red, Green, Blue: Byte): TColor32;
// using colorhelper
// decoding with (Integer(R) * 255) div 63 (as i encountered somewhere) has very slightly different results for the colors.
// probably this way of decoding is better, because 63 maps to 255
begin
  Result.R := Red shl 2;
  Result.G := Green shl 2;
  Result.B := Blue shl 2;
  // handle transparancy
  if (Result.R = 0) and (Result.G = 0) and (Result.B = 0)
  then Result.A := 0
  else Result.A := $FF;
end;

function DosPaletteEntryToColor32(const Entry: TDOSVGAColorRec): TColor32;
begin
  Result := DosPaletteEntryToColor32(Entry.R, Entry.G, Entry.B);
end;

function Color32ToDosPaletteEntry(C: TColor32): TDOSVGAColorRec;
// using colorhelper
begin
  Result.R := C.R shr 2;
  Result.G := C.G shr 2;
  Result.B := C.B shr 2;
end;

function DosPaletteToArrayOfColor32(const Pal: TDosVGAPalette8): TArrayOfColor32;
var
  i: Integer;
begin
  SetLength(Result, Length(Pal));
  for i := 0 to Length(Pal) - 1 do
    Result[i] := DosPaletteEntryToColor32(Pal[i]);
end;

{ TDosPlanarBitmap }

class procedure TDosPlanarBitmap.LoadFromFile(const aFilename: string; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer; BPP: Byte; const aPalette: TArrayOfColor32);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F, aBitmap, aPos, aWidth, aHeight, BPP, aPalette);
  finally
    F.Free;
  end;
end;

const
  ALPHA_TRANSPARENTBLACK = $80000000; // todo:check if used

class procedure TDosPlanarBitmap.GetByteMap(S: TStream; aByteMap: TByteMap; aPos, aWidth, aHeight: Integer; BPP: Byte);
{-------------------------------------------------------------------------------
  This should be the key routine: it converts a planar stored bitmap to
  a simple two-dimensional palette entry array: each (x,y) is a palette number as stored in the DOS file.
  aWidth, aHeight, BPP must be correct for this procedure to work.
  We use the Graphics32 TByteMap for that.
-------------------------------------------------------------------------------}
var
  NumBytes : Integer;
  Buf: PRawBytes;
  PlaneSize: Integer;
  LineSize: Integer;

    // local function
    function GetPix(X, Y: Integer): Byte;
    var
      BytePtr: PByte;
      i, P: Integer;
      BitNumber, Mask: Byte;
    begin
      Result := 0;
      // adres of pixel in the first plane
      P := Y * LineSize + X div 8;
      // get the right bit (WOW! totally excellent: it's backwards)
      BitNumber := 7 - X mod 8; // downwards bits!
      Mask := 1 shl BitNumber;
      // get the seperated bits from the planes
      BytePtr := @Buf^[P];
      for i := 0 to BPP - 1 do begin
        if BytePtr^ and Mask <> 0 then
          Result := Result or (1 shl i);
        Inc(BytePtr, PlaneSize); // typed ptr increment
      end;
    end;

var
  x, y: Integer;
  Entry: Byte;
begin
    {$ifdef paranoid} Assert(BPP in [1..8], 'bpp error'); {$endif}

  LineSize := aWidth div 8; // Every bit is a pixel (in each plane)
  PlaneSize := LineSize * aHeight; // Now we know the planesize
  NumBytes := PlaneSize * BPP; // and the total bytes

  GetMem(Buf, NumBytes);
  try
    // continue reading if param < 0 or goto position in stream
    if aPos >= 0 then
      S.Seek(aPos, soFromBeginning);
    S.ReadBuffer(Buf^, NumBytes);
    aByteMap.SetSize(aWidth, aHeight);
    aByteMap.Clear(0);

    for y := 0 to aHeight-1 do begin
      for x := 0 to aWidth - 1 do begin
        Entry := GetPix(x, y);
        aByteMap[x, y] := Entry;
      end;
    end;
  finally
    FreeMem(Buf);
  end;
end;

class procedure TDosPlanarBitmap.LoadFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer; BPP: Byte; const aPalette: TArrayOfColor32);
{-------------------------------------------------------------------------------
  load bytemap and convert it to bitmap, using the palette parameter
-------------------------------------------------------------------------------}
var
  ByteMap: TByteMap;
  x, y: Integer;
  C: PColor32;
  B: System.Types.PByte;
  PalLen: Integer;

    procedure PreparePal;
    var
      i: Integer;
    begin
      for i := 1 to PalLen - 1 do
        if aPalette[i] = 0 then
          aPalette[i] := aPalette[i] or ALPHA_TRANSPARENTBLACK;
    end;

begin
  PalLen := Length(aPalette);
  PreparePal;

  ByteMap := TByteMap.Create;
  try
    GetByteMap(S, ByteMap, aPos, aWidth, aHeight, BPP);
    aBitmap.SetSize(aWidth, aHeight);
    aBitmap.Clear(0);
    C := aBitMap.PixelPtr[0, 0];
    B := ByteMap.ValPtr[0, 0];
    for y := 0 to aHeight - 1 do
      for x := 0 to aWidth - 1 do
      begin
          {$ifdef paranoid} Assert(B^ < PalLen, 'error color ' + IntToStr(B^) + '; length pal = ' + IntToStr(PalLen)); {$endif}
        C^ := aPalette[B^];
        Inc(C); // typed pointer increment
        Inc(B); // typed pointer increment
      end;

  finally
    ByteMap.Free;
  end;
end;

class procedure TDosPlanarBitmap.LoadAnimationFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight, aFrameCount: Integer; BPP: Byte; const aPalette: TArrayOfColor32);
{-------------------------------------------------------------------------------
  We assume that
  • frames are in the stream in a row after eachother
  • the are same size
  • the have the same palette
  • the have the same BPP
-------------------------------------------------------------------------------}
var
  FrameBitmap: TBitmap32;
  i: Integer;
begin
  // set initial position, the rest is read automatically
  if aPos >= 0 then
    S.Seek(aPos, soFromBeginning);

  FrameBitmap := TBitmap32.Create;
  try
    aBitmap.SetSize(aWidth, aHeight * aFrameCount);
    aBitmap.Clear(0);
    for i := 0 to aFrameCount - 1 do begin
      LoadFromStream(S, FrameBitmap, -1, aWidth, aHeight, BPP, aPalette);
      FrameBitmap.DrawTo(aBitmap, 0, aHeight * i);
    end;
  finally
    FrameBitmap.Free;
  end;
end;

{ TVgaSpecBitmap }

procedure TVgaSpecBitmap.GetSectionsAndPalette(Src, Dst: TStream; var Pal: TDosVGAPalette8);
var
  CurByte: Byte;
  Cnt, Rd, CurSection: Integer;
  Value: Byte;
  PalInfo: TDosVgaSpecPaletteHeader;
  Buf: PRawBytes;
begin
  Buf := nil;
  Src.Seek(0, soFromBeginning);
  Dst.Seek(0, soFromBeginning);
  Src.Read(PalInfo, Sizeof(PalInfo));
  Pal := PalInfo.VgaPal;
  CurSection := 0;

  try
    repeat
      Rd := Src.Read(CurByte, 1);
      if Rd <> 1 then
        Break;
      case CurByte of
        // end section
        128:
          begin
            if Dst.Position mod VGASPEC_SECTIONSIZE <> 0 then
              raise Exception.Create('vga spec section size error');
            //deb(['currsection', cursection, src.position]);
            Inc(CurSection);
            if CurSection > 3 then
              Break;
          end;
        // raw bytes
        0..127:
          begin
            Cnt := CurByte + 1;
            Dst.CopyFrom(Src, Cnt);
          end;
        // repeated bytes
        129..255:
          begin
            Cnt := 257 - CurByte;
            ReallocMem(Buf, Cnt); // we could use just a 256 byte buffer or so, no realloc needed
            Src.Read(Value, 1);
            FillChar(Buf^, Cnt, Value);
            Dst.Write(Buf^, Cnt);
          end;
      end; //case
    until False;

  finally
    FreeMem(Buf);
  end;
end;

function TVgaSpecBitmap.DecodeSection(var Src, Dst: PRawBytes; SrcSize: Integer): Integer;
var
  CodeByte, Value: Byte;
  i, si, di, Cnt, Allocated: Integer;

    procedure EnsureMem(aSize: Integer; Exact: Boolean = False);
    begin
      case Exact of
        False:
          if aSize > Allocated then begin
            ReallocMem(Dst, aSize);
            Allocated := aSize;
          end;
        True:
          if aSize <> Allocated then begin
            ReallocMem(Dst, aSize);
            Allocated := aSize;
          end;
      end;
    end;

begin
  Result := 0;
  si := 0;
  di := 0;
  Allocated := 0;
  EnsureMem(SrcSize, True);
  FillChar(Dst^, Allocated, 0);
  while si <= SrcSize - 1 do
  begin
    CodeByte := Src^[si];
    case CodeByte of
      // end section
      128:
        begin
          Exit;
        end;
      // raw bytes
      0..127:
        begin
          Cnt := CodeByte + 1;
          Inc(si);
          EnsureMem(di + Cnt);
          Move(Src^[si], Dst^[di], Cnt);
          Inc(si, Cnt);
          Inc(di, Cnt);
        end;
      // repeated bytes
      129..255:
        begin
          Cnt := 257 - CodeByte;
          Inc(si);
          EnsureMem(di + Cnt);
          Value := Src^[si];
          for i := 0 to Cnt - 1 do
            Dst^[di + i] := Value;
          Inc(di, Cnt);
          Inc(si);
        end;
    end; //case

  end;

  EnsureMem(di, True);
  Result := di;
end;


procedure TVgaSpecBitmap.LoadFromFile(const aFileName: string; aBitmap: TBitmap32);
{-------------------------------------------------------------------------------
  method to load a bitmap from the vgaspec?.dat files
-------------------------------------------------------------------------------}
var
  F: TBufferedFileStream;
begin
  if aBitmap = nil then
    Exit;
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F, aBitmap);
  finally
    F.Free;
  end;
end;

procedure TVgaSpecBitmap.LoadFromStream(S: TStream; aBitmap: TBitmap32);
{-------------------------------------------------------------------------------
  So here we are at the decoding of vgaspec?.dat:
  • Step 1: Decompress with the "default" dos lemming decompression code
  • Step 2: Get the vga-palette from the first few bytes
  • Step 3: Decode 4 sections with the "bitmap" decompression code
  • Step 4: Now in each of the 4 sections, which should be 14400 bytes, extract
            a planar bitmap (3 BPP, 960x40)
  • Step 5: Create one big bitmap of this 4 planar bitmaps
-------------------------------------------------------------------------------}
var
  Decompressor: TDosDatDecompressor;
  Mem, PMem: TMemoryStream;
  TempBitmap: TBitmap32;
  Sec: Integer;
  DosPal: TDosVGAPalette8;
  Pal: TArrayOfColor32;
begin
    {$ifdef paranoid} Assert(aBitmap <> nil); {$endif}

  Decompressor := TDosDatDecompressor.Create;
  Mem := TMemoryStream.Create;
  try

    { step 1: decompress }
    try
      Decompressor.DecompressSection(S, Mem);
    finally
      Decompressor.Free;
    end;

    PMem := TMemoryStream.Create;
    TempBitmap := TBitmap32.Create;
    try
      { step 2 + 3 : getpalette, extract 4 sections }
      GetSectionsAndPalette(Mem, PMem, DosPal);
      Pal := DosPaletteToArrayOfColor32(DosPal);

      aBitmap.SetSize(960, 160);
      aBitmap.Clear(0); // clear with #transparent black
      for Sec := 0 to 3 do
      begin
        { step 4: read planar bitmap part from section }
        TempBitmap.Clear(0);
        TDosPlanarBitmap.LoadFromStream(PMem, TempBitmap, Sec * VGASPEC_SECTIONSIZE, 960, 40, 3, Pal);
        { step 5: draw to bitmap }
        aBitmap.Draw(0, Sec * 40, TempBitmap);
      end;
    finally
      PMem.Free;
      TempBitmap.Free;
    end;

   finally
     Mem.Free;
   end;
end;

procedure TVgaSpecBitmap.LoadPaletteFromFile(const aFileName: string; out Pal: TDosVGAPalette8);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    LoadPaletteFromStream(F, Pal);
  finally
    F.Free;
  end;
end;

procedure TVgaSpecBitmap.LoadPaletteFromStream(S: TStream; out Pal: TDosVGAPalette8);
var
  Decompressor: TDosDatDecompressor;
  Mem: TMemoryStream;
begin
  Decompressor := TDosDatDecompressor.Create;
  Mem := TMemoryStream.Create;
  try
    try
      Decompressor.DecompressSection(S, Mem);
    finally
      Decompressor.Free;
    end;
    Mem.Seek(0, soFromBeginning);
    Mem.Read(Pal, Sizeof(Pal)); // first section, first bytes = vgapalette
   finally
     Mem.Free;
   end;
end;

procedure TVgaSpecBitmap.CheckBitmap(aBitmap: TBitmap32);
const
  SVgaSpecDimensionError_dd = 'Special Dos level graphics must be %d x %d pixels';
begin
  if (aBitmap.Width <> 960) or (aBitmap.Height <> 160) then
    raise Exception.CreateFmt(SVgaSpecDimensionError_dd, [aBitmap.Width, aBitmap.Height]);
end;

procedure TVgaSpecBitmap.CreatePal(aBitmap: TBitmap32; var Pal: TDosVgaSpecPaletteHeader);
var
  C: PColor32;
  y, x: Integer;
  LastEntry: Integer;
  ColorArray: array[0..7] of TColor32;

    procedure AddColor;
    var
      i, Entry: Integer;
    begin
      if C^ = clBlack32 then
        Exit;
      for i := 0 to LastEntry do
        if ColorArray[i] = C^ then
          Exit;
      Entry := LastEntry + 1;
      if Entry > 7 then
        Throw('TVgaSpecBitmap.CreatePal vgaspec error bitmap has too many colors');
      LastEntry := Entry;
      ColorArray[Entry] := C^;
      Pal.VgaPal[Entry].R := C^.R div 4;
      Pal.VgaPal[Entry].G := C^.G div 4;
      Pal.VgaPal[Entry].B := C^.B div 4;
    end;

begin
  FillChar(ColorArray, SizeOf(ColorArray), 0);
  LastEntry := 0;
  FillChar(Pal, SizeOf(0), 0);
  C := aBitmap.PixelPtr[0, 0];
  for y := 0 to aBitmap.Height - 1 do
    for x := 0 to aBitmap.Width - 1 do begin
      AddColor;
      Inc(C);
    end;
end;

function TVgaSpecBitmap.EncodeSection(var Src, Dst: PRawBytes; SrcSize: Integer): Integer;
{-------------------------------------------------------------------------------
  Encoding of bytes as done in VGASPEC?.DAT
  Returns number of allocated bytes of Dst after encoding.
  Dst should be nil at entrance
-------------------------------------------------------------------------------}
var
  DstPtr: Integer;
  Allocated: Integer;

    function NextSection(ix: Integer; var aRepeated: Boolean): Integer;
    {-------------------------------------------------------------------------------
       Local proc: Get the next section from index ix
       • Repeated will be true if there are repeated characters
       • Returnvalue = length of the section
    -------------------------------------------------------------------------------}
    var
      i: Integer;
    begin
      aRepeated := False;
      Result := 0;

      if ix > SrcSize - 1 then // beyond end
        Exit
      else if ix = SrcSize - 1 then // last byte
      begin
        Result := 1;
        aRepeated := True;
        Exit;
      end;

      aRepeated := Src^[ix] = Src^[ix + 1];
      i := ix;
      case aRepeated of
        True:
          while Src^[i] = Src^[i + 1] do
          begin
            Inc(i);
            if i - ix >= 127 then
              Break; // maximum count = 128
            if i >= SrcSize - 1 then
              Break; // check end
          end;
        False :
          while (Src^[i] <> Src^[i + 1]) do
          begin
            Inc(i);
            if i - ix >= 128 then
              Break; // maximum count = 128
            if i >= SrcSize - 1 then
              Break; // check end
          end;
      end;
      Result := i - ix;

      if aRepeated or (i = SrcSize - 1) then
        Inc(Result)
      else if Result = 1 then
        aRepeated := True; // one character is stored as a single repeat
    end;

    procedure Store(ix, Cnt: Integer; aRepeated: Boolean);
    {-------------------------------------------------------------------------------
      Local proc: Store bytes in dst
    -------------------------------------------------------------------------------}
    begin
      //Deb([ix, cnt, arepeated, dstptr]);
      if DstPtr + Cnt + 2 > Allocated then
      begin
        ReallocMem(Dst, DstPtr + Cnt + 2);
        Allocated := DstPtr + Cnt + 2;
      end;

      case aRepeated of
        False:
          begin
            Dst^[DstPtr] := Cnt - 1;
            Inc(DstPtr);
            Move(Src^[ix], Dst^[DstPtr], Cnt);
            Inc(DstPtr, Cnt);
          end;
        True:
          begin
            Dst^[DstPtr] := 257 - Cnt;
            Inc(DstPtr);
            Dst^[DstPtr] := Src^[ix];
            Inc(DstPtr);
          end;
      end;

    end;

var
  ix: Integer;
  Cnt: Byte;
  Rep: Boolean;
begin
  Allocated := SrcSize;
  ReallocMem(Dst, Allocated);
  FillChar(Dst^, Allocated, 0);
  ix := 0;
  DstPtr := 0;

  repeat
    Cnt := NextSection(ix, Rep);
    if Cnt = 0 then
      Break;
    Store(ix, Cnt, Rep);
    Inc(Ix, Cnt);
  until False;

  if DstPtr <> Allocated then
    ReallocMem(Dst, DstPtr);

  Result := DstPtr;
end;

procedure TVgaSpecBitmap.EncodeSectionStream(Src, Dst: TStream);
var
  InSize, OutSize: Integer;
  A, B: PRawBytes;
begin
  Src.Seek(0, soFromBeginning);
  InSize := Src.Size;
  GetMem(A, InSize);
  Src.Read(A^, InSize);
  B := nil;
  OutSize := EncodeSection(A, B, InSize);
  Dst.Write(B^, OutSize);
  FreeMem(A);
  Freemem(B);
end;

procedure TVgaSpecBitmap.SaveToFile(aBitmap: TBitmap32; const aFileName: string);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(aBitmap, F);
  finally
    F.Free;
  end;
end;

procedure TVgaSpecBitmap.SaveToStream(aBitmap: TBitmap32; Dst: TStream);
{-------------------------------------------------------------------------------
  Here the code to create a vgaspec.dat file from a bitmap.
  • Check bitmap 960x160
  • Create palette entries (max 7 colors)
  • Write Palette to the stream
  • Encode bitmap to 4 planarbitmap sections
-------------------------------------------------------------------------------}
(*
var
  TempBitmap: TBitmap32;
  H, Sec: Integer;
  Pal: TDosVgaSpecPaletteHeader;
  DosCompressor: TDosDatCompressor;
  DstRect, SrcRect: TRect;
  PlanarBitmap: TDosPlanarBitmap;
  PlanarMem: TMemoryStream;
  Mem1: TMemoryStream;
  *)
begin

  Throw('TVgaSpecBitmap.SaveToStream');

  (*

  CheckBitmap(aBitmap);
  CreatePal(aBitmap, Pal);

  DstRect := Rect(0, 0, 960, 40);
  SrcRect := DstRect;
  TempBitmap := TBitmap32.Create;
  PlanarBitmap := TDosPlanarBitmap.Create;
  Mem1 := TMemoryStream.Create;
  PlanarMem := TMemoryStream.Create;
  DosCompressor := TDosDatCompressor.Create;
  DosCompressor.OnProgress := DoCompressProgress;
  try
    TempBitmap.SetSize(960, 40);
    Mem1.Write(Pal, SizeOf(Pal));
    for Sec := 0 to 3 do
    begin
      TempBitmap.Draw(DstRect, SrcRect, aBitmap);
      RectMove(SrcRect, 0, 40);
      PlanarMem.Clear;
      PlanarBitmap.SaveToStream(TempBitmap, PlanarMem, 3, Pal.VgaPal, Pal.VgaPal);
      PlanarMem.Seek(0, soFromBeginning);
      EncodeSectionStream(PlanarMem, Mem1);
    end;
    Mem1.Seek(0, soFromBeginning);

    H := DosCompressor.Compress(Mem1, Dst);
    //windlg([mem1.size, dst.size]);
  finally
    TempBitmap.Free;
    PlanarBitmap.Free;
    Mem1.Free;
    DosCompressor.Free;
    PlanarMem.Free;
  end;

  *)
end;


procedure TVgaSpecBitmap.DoCompressProgress(aPosition, aMaximum: Integer);
begin
  if Assigned(fOnCompressProgress) then
    fOnCompressProgress(aPosition, aMaximum);
end;

end.
