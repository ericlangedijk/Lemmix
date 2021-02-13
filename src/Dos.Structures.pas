unit Dos.Structures;

{$include lem_directives.inc}

interface

uses
  Classes, SysUtils, Types,
  GR32,
  Dos.Consts,
  Base.Utils, Base.Bitmaps,
  Prog.Data;

// LVL raw level file structure consts as used by dos en winlemmings and probably many other platforms.
const
  LVL_MAXOBJECTCOUNT  = 32;
  LVL_MAXTERRAINCOUNT = 400;
  LVL_MAXSTEELCOUNT   = 32;

type
  TLVLObject = packed record
  case Byte of
    0: ( AsInt64: Int64 );
    1: ( D0, D1: DWord);
    2: ( W0, W1, W2, W3: Word);
    3: ( B0, B1, B2, B3, B4, B5, B6, B7: Byte);
    4: (
          XPos              : Word; // swap lo and hi
          YPos              : Word; // swap lo and hi
          ObjectID          : Word;
          Modifier          : Byte;
          DisplayMode       : Byte; // $8F = invert; $0F = normal
        );

  end;

  {
    bits 0..3    = modifier
    bits 4..7    shl 8 for xpos
    bits 8..15   add to xpos
    bits 16..24  9 bits number YPos
  }

  TLVLTerrain = packed record
  case Byte of
    0: ( D0: DWord );
    1: ( W0, W1: Word );
    2: ( B0, B1, B2, B3: Byte );
    3: (
          XPos              : Word;
          YPos              : Byte; // 9 bits
          TerrainID         : Byte;
       );
  end;

  TLVLSteel = packed record
  case Byte of
    0: ( D0: DWord );
    1: ( W0, W1: Word);
    2: ( B0, B1, B2, B3: Byte);
    3: (
         XPos              : Byte; // 9 bits
         YPos              : Byte; // 7 bits bits 1..7
         Area              : Byte; // bits 0..3 is height in 4 pixel units (then add 4)
                                   // bit 4..7 is width in 4 pixel units (then add 4)
         b                 : Byte; // always zero
       );
  end;

  TLVLTitle = array[0..31] of AnsiChar;

  TLVLObjects = array[0..LVL_MAXOBJECTCOUNT - 1] of TLVLObject;
  TLVLTerrains = array[0..LVL_MAXTERRAINCOUNT - 1] of TLVLTerrain;
  TLVLSteels = array[0..LVL_MAXSTEELCOUNT - 1] of TLVLSteel;


  // the dos level record 2048 bytes
  PLVLRec = ^TLVLRec;
  TLVLRec = packed record
  public
    {0000}  ReleaseRate                : Word; // big endian, swap!
    {    }  LemmingsCount              : Word; // big endian, swap!
    {    }  RescueCount                : Word; // big endian, swap!
    {    }  TimeLimit                  : Word; // big endian, swap!
    {0008}  ClimberCount               : Word; // big endian, swap!
    {    }  FloaterCount               : Word; // big endian, swap!
    {    }  BomberCount                : Word; // big endian, swap!
    {    }  BlockerCount               : Word; // big endian, swap!
    {    }  BuilderCount               : Word; // big endian, swap!
    {    }  BasherCount                : Word; // big endian, swap!
    {    }  MinerCount                 : Word; // big endian, swap!
    {    }  DiggerCount                : Word; // big endian, swap!
    {0018}  ScreenPosition             : Word; // big endian, swap!
    {001A}  GraphicSet                 : Word; // big endian, swap!
    {    }  GraphicSetEx               : Word; // big endian, swap!
    {001E}  Reserved                   : Word; // big endian, swap! $FFFF if SuperLemming else $0000
    {0020}  Objects                    : TLVLObjects;
    {0120}  Terrain                    : TLVLTerrains;
    {0760}  Steel                      : TLVLSteels;
    {07E0}  LevelName                  : TLVLTitle; // 32 bytes
  public
    function SwapProp(aProp: Word): Word; inline;
    function GetObjectCount(out entranceCount, exitCount: Integer): Integer;
    function GetTerrainCount: Integer;
    function GetSteelCount: Integer;
    function IsSuperLemming: Boolean; inline;
  end;

const
  LVL_SIZE = SizeOf(TLVLRec);
  LVL_TITLE_OFFSET = $750;

{-------------------------------------------------------------------------------
  GROUNDXX.DAT files (1056 bytes) (16 objects, 64 terrain, color palette)
  • Somewhere there is documentation for details.
  • Little Endian words, so we can just load them from disk halleluyah!
-------------------------------------------------------------------------------}

type
  PDosMetaObject = ^TDosMetaObject;
  TDosMetaObject = packed record
    oAnimation_flags               : Word; // 2
    oStart_animation_frame_index   : Byte; // 3
    oAnimation_frame_count         : Byte; // 4
    oWidth                         : Byte; // 5
    oHeight                        : Byte; // 6
    oAnimation_frame_data_size     : Word; // 8   size in bytes of each animation frame
    oMask_offset_from_image        : Word; // 10
    oUnknown1                      : Word; // 12
    oUnknown2                      : Word; // 14
    oTrigger_left                  : Word; // 16
    oTrigger_top                   : Word; // 18
    oTrigger_width                 : Byte; // 19
    oTrigger_height                : Byte; // 20
    oTrigger_effect_id             : Byte; // 21
    oAnimation_frames_base_loc     : Word; // 23
    oPreview_image_location        : Word; // 25
    oUnknown3                      : Word; // 27
    oSound_effect_id               : Byte; // 28
  end;
  TDosMetaObjectArray = packed array[0..15] of TDOSMetaObject;

  PDosMetaTerrain = ^TDosMetaTerrain;
  TDosMetaTerrain = packed record
    tWidth        : Byte;
    tHeight       : Byte;
    tImage_loc    : Word;
    tMask_loc     : Word;
    tUnknown1     : Word
  end;

  TDosMetaTerrainArray = packed array[0..63] of TDosMetaTerrain;
  TDosEGAPalette8 = packed array[0..7] of Byte;

  PDosVgaColorRec = ^TDosVgaColorRec;
  TDosVgaColorRec = packed record
    R, G, B: Byte;
  end;

  TDosVGAPalette8 = packed array[0..7] of TDosVGAColorRec;

  // this is the total structure of a dos ground?.dat
  TDosGroundRec = packed record
    ObjectInfoArray     : TDosMetaObjectArray;
    TerrainInfoArray    : TDosMetaTerrainArray;
    EGA_PaletteCustom   : TDosEGAPalette8;
    EGA_PaletteStandard : TDOSEGAPalette8;
    EGA_PalettePreview  : TDOSEGAPalette8;
    VGA_PaletteCustom   : TDOSVGAPalette8;
    VGA_PaletteStandard : TDOSVGAPalette8;
    VGA_PalettePreview  : TDOSVGAPalette8;
  end;

  TDosVGAPalette16 = packed array[0..15] of TDosVGAColorRec;

  TDosVgaSpecPaletteHeader = packed record
    VgaPal: TDosVGAPalette8; // 24
    EgaPal: TDosEGAPalette8; // 8
    UnknownPal: array[0..7] of Byte; // maybe even less colors
  end;

  // big endian words, swap!
  TDosOddTableRec = packed record
    ReleaseRate                : Word;
    LemmingsCount              : Word;
    RescueCount                : Word;
    TimeLimit                  : Word;
    ClimberCount               : Word;
    FloaterCount               : Word;
    BomberCount                : Word;
    BlockerCount               : Word;
    BuilderCount               : Word;
    BasherCount                : Word;
    MinerCount                 : Word;
    DiggerCount                : Word;
    LevelName                  : array[0..31] of AnsiChar;
  end;


type
  // So this is a very non-oop record to easily load the oddtable in records
  TDosOddTable = record
  public
    Recs: array of TDosOddTableRec;
    procedure Clear;
    procedure LoadFromFile(const aStylename, aFilename: string);
    procedure LoadFromStream(S: TStream);
  end;

const
  // These values do *need* to be converted (about shl 2)
  DosInLevelPalettes: array[Boolean] of TDosVGAPalette8 = (

    // the default palette (christmas = false)
    (
      (R: 000; G: 000; B: 000), // black
      (R: 016; G: 016; B: 056), // blue
      (R: 000; G: 044; B: 000), // green
      (R: 060; G: 052; B: 052), // white
      (R: 044; G: 044; B: 000), // yellow
      (R: 060; G: 008; B: 008), // red
      (R: 032; G: 032; B: 032), // gray
      (R: 000; G: 000; B: 000)  // not used: probably this color is replaced with the standard palette entry in ground??.dat
    ),

    // the christmas palette (christmas = true)
    (
      (R: 000; G: 000; B: 000), // black
      (R: 060; G: 008; B: 008), // red
      (R: 000; G: 044; B: 000), // green
      (R: 060; G: 052; B: 052), // white
      (R: 044; G: 044; B: 000), // yellow
      (R: 016; G: 016; B: 056), // blue
      (R: 032; G: 032; B: 032), // gray
      (R: 000; G: 000; B: 000)  // not used: probably this color is replaced with the standard palette entry in ground??.dat
    )

  );

const
  { These values do not need to be converted }
  DosMainMenuPalette: TDosVGAPalette16 = (
    (R: 000; G: 000; B: 000), // black
    (R: 128; G: 064; B: 032), // browns
    (R: 096; G: 048; B: 032),
    (R: 048; G: 000; B: 016),
    (R: 032; G: 008; B: 124), // purples
    (R: 064; G: 044; B: 144),
    (R: 104; G: 088; B: 164),
    (R: 152; G: 140; B: 188),
    (R: 000; G: 080; B: 000), // greens
    (R: 000; G: 096; B: 016),
    (R: 000; G: 112; B: 032),
    (R: 000; G: 128; B: 064),
    (R: 208; G: 208; B: 208), // white
    (R: 176; G: 176; B: 000), // yellow
    (R: 064; G: 080; B: 176), // blue
    (R: 224; G: 128; B: 144)  // pink
  );

function DosVgaColorToColor32(const ColorRec: TDosVgaColorRec): TColor32;
function GetDosMainMenuPaletteColors32: TArrayOfColor32;
procedure DosVgaPalette8ToLemmixPalette(const DosPal: TDosVGAPalette8; var LemmixPal: TArrayOfColor32);

function LVLTitleAsString(const title: TLVLTitle; trimmed: Boolean): string;
function EmptyLVLTitle: TLVLTitle;


implementation

function LVLTitleAsString(const title: TLVLTitle; trimmed: Boolean): string;
var
  C: AnsiChar;
begin
  SetLength(Result, 32);
  for var i := 0 to 31 do begin
    C := title[i];
    Result[i + 1] := Char(C);
  end;
  if trimmed then
    Result := Result.Trim;
end;

function EmptyLVLTitle: TLVLTitle;
begin
  FillChar(Result, SizeOf(Result), 32); // space
end;

function GetDosMainMenuPaletteColors32: TArrayOfColor32;
var
  i: Integer;
  P: PColor32;
begin
  SetLength(Result, 16);
  P := @Result[0];
  for i := 0 to 15 do begin
    P^.A := 0;
    P^.R := DosMainMenuPalette[i].R;
    P^.G := DosMainMenuPalette[i].G;
    P^.B := DosMainMenuPalette[i].B;
    Inc(P); // typed ptr increment
  end;
end;

function DosVgaColorToColor32(const ColorRec: TDosVgaColorRec): TColor32;
begin
  Result.R := ColorRec.R * 4;
  Result.G := ColorRec.G * 4;
  Result.B := ColorRec.B * 4;
end;

{ TLVLRec }

function TLVLRec.SwapProp(aProp: Word): Word;
begin
  Result := System.Swap(aProp);
end;

function TLVLRec.GetObjectCount(out entranceCount, exitCount: Integer): Integer;
var
  O: TLVLObject;
begin
  Result := 0;
  entranceCount := 0;
  exitCount := 0;
  for var i := 0 to LVL_MAXOBJECTCOUNT - 1 do begin
    O := Objects[i];
    if O.AsInt64 <> 0 then begin
      Inc(Result);
      case Integer(O.B5 and 15) of
        DOS_OBJECT_ID_EXIT: Inc(exitCount);
        DOS_OBJECT_ID_ENTRANCE: Inc(entranceCount);
      end;
    end;
  end;
end;

function TLVLRec.GetTerrainCount: Integer;
var
  T: TLVLTerrain;
begin
  Result := 0;
  for var i := 0 to LVL_MAXTERRAINCOUNT - 1 do begin
    T := Terrain[i];
    if T.D0 <> $FFFFFFFF then
      Inc(Result);
  end;
end;

function TLVLRec.GetSteelCount: Integer;
var
  S: TLVLSteel;
begin
  Result := 0;
  for var i := 0 to LVL_MAXSTEELCOUNT - 1 do begin
    S := Steel[i];
    if S.D0 <> 0 then
      Inc(Result);
  end;
end;

function TLVLRec.IsSuperLemming: Boolean;
begin
  Result := Reserved = $FFFF;
end;

{TDosOddTable}

procedure TDosOddTable.Clear;
begin
  SetLength(Recs, 0);
end;

procedure TDosOddTable.LoadFromFile(const aStylename, aFilename: string);
var
  F: TStream;
begin
  F := TData.CreateDataStream(aStylename, aFileName, TDataType.LemmingData);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TDosOddTable.LoadFromStream(S: TStream);
var
  i: Integer;
begin
  SetLength(Recs, S.Size div SizeOf(TDosOddTableRec));
  for i := 0 to Length(Recs) - 1 do
    S.ReadBuffer(Recs[i], Sizeof(TDosOddTableRec));
end;

procedure DosVgaPalette8ToLemmixPalette(const DosPal: TDosVGAPalette8; var LemmixPal: TArrayOfColor32);
// 6 --> 8 bit color conversions
var
  i: Integer;
  E: PColor32Entry;
  D: PDosVgaColorRec;
begin
  SetLength(LemmixPal, 8);
  for i := 0 to 7 do begin
    E := @LemmixPal[i];
    D := @DosPal[i];
    E^.A := 0;
    E^.R := (Integer(D^.R) * 255) div 63;
    E^.G := (Integer(D^.G) * 255) div 63;
    E^.B := (Integer(D^.B) * 255) div 63;
  end;
end;

end.

