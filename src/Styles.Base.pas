unit Styles.Base;

{$include lem_directives.inc}

// The abstract style system. descendants in Styles.Dos, Styles.User

interface

uses
  System.Classes, System.SysUtils, System.Contnrs, System.Generics.Collections, System.Character,
  GR32,
  Base.Utils, Base.Types, Base.Bitmaps,
  Dos.Consts, Dos.Compression, Dos.Bitmaps, Dos.Structures,
  Meta.Structures,
  Level.Base, Level.Hash, Level.Loader,
  Prog.Base, Prog.Data;

type
  TStyle = class;
  TLemmingAnimationSet = class;
  TGraphicSet = class;
  TLevelSystem = class;
  TSection = class;
  TLevelLoadingInformation = class;

  TLevelSystemClass = class of TLevelSystem;
  TSectionList = class(TFastObjectList<TSection>);
  TLevelLoadingInformationList = class(TFastObjectList<TLevelLoadingInformation>);

  TStyle = class abstract
  private
    fIsByFactory: Boolean;
    function GetRootPath: string; inline;
  strict protected
    fMainDatFileName     : string; // main.dat
    fLemmingAnimationSet : TLemmingAnimationSet;
    fStyleInformation    : Consts.TStyleInformation;
    fLevelSystem         : TLevelSystem;
    fMechanics           : TMechanics;
    fName                : string;
    fDef                 : TStyleDef;
    fIsPooledByFactory   : Boolean;
  protected
    function GetLevelSystemClass: TLevelSystemClass; virtual; abstract;
    function GetMechanics: TMechanics; virtual; abstract;
    function GetMainDatFileName: string; virtual; // default 'main.dat'
  public
    constructor Create(const aName: string); virtual;
    procedure BeforeDestruction; override; final;
    destructor Destroy; override;
    procedure AfterConstruction; override; final;
    property StyleInformation: Consts.TStyleInformation read fStyleInformation;
    property MainDatFileName: string read fMainDatFileName; // default 'main.dat'
    property LemmingAnimationSet: TLemmingAnimationSet read fLemmingAnimationSet;
    property LevelSystem: TLevelSystem read fLevelSystem;
    property Mechanics: TMechanics read fMechanics;
    property Name: string read fName;
    property RootPath: string read GetRootPath;
    property Def: TStyleDef read fDef;
    property IsPooledByFactory: Boolean read fIsByFactory write fIsPooledByFactory;
  end;

  TLemmingAnimationSet = class sealed
  public
    const
      LTR = False;
      RTL = True;

    const
    {-------------------------------------------------------------------------------
      dos animations ordered by their appearance in main.dat
      the constants below show the exact order
    -------------------------------------------------------------------------------}
      WALKING             = 0;
      JUMPING             = 1;
      WALKING_RTL         = 2;
      JUMPING_RTL         = 3;
      DIGGING             = 4;
      CLIMBING            = 5;
      CLIMBING_RTL        = 6;
      DROWNING            = 7;
      HOISTING            = 8;
      HOISTING_RTL        = 9;
      BRICKLAYING         = 10;
      BRICKLAYING_RTL     = 11;
      BASHING             = 12;
      BASHING_RTL         = 13;
      MINING              = 14;
      MINING_RTL          = 15;
      FALLING             = 16;
      FALLING_RTL         = 17;
      UMBRELLA            = 18;
      UMBRELLA_RTL        = 19;
      SPLATTING           = 20;
      EXITING             = 21;
      FRIED               = 22;
      BLOCKING            = 23;
      SHRUGGING           = 24;
      SHRUGGING_RTL       = 25;
      OHNOING             = 26;
      EXPLOSION           = 27;

      AnimationIndices : array[TLemmingAction, LTR..RTL] of Integer = (
        (0, 0),
        (WALKING, WALKING_RTL),                   // baWalk,
        (JUMPING, JUMPING_RTL),                   // baJumping,
        (DIGGING, DIGGING),                       // baDigging,
        (CLIMBING, CLIMBING_RTL),                 // baClimbing,
        (DROWNING, DROWNING),                     // baDrowning,
        (HOISTING, HOISTING_RTL),                 // baHoisting,
        (BRICKLAYING, BRICKLAYING_RTL),           // baBricklaying,
        (BASHING, BASHING_RTL),                   // baBashing,
        (MINING, MINING_RTL),                     // baMining,
        (FALLING, FALLING_RTL),                   // baFalling,
        (UMBRELLA, UMBRELLA_RTL),                 // baUmbrella,
        (SPLATTING, SPLATTING),                   // baSplatting,
        (EXITING, EXITING),                       // baExiting,
        (FRIED, FRIED),                           // baFried,
        (BLOCKING, BLOCKING),                     // baBlocking,
        (SHRUGGING, SHRUGGING_RTL),               // baShrugging,
        (OHNOING, OHNOING),                       // baOhnoing,
        (EXPLOSION, EXPLOSION)                    // baExploding
      );

  strict private
    fStyle                    : TStyle; // owner style
  // metadata
    fMetaLemmingAnimationList : TMetaLemmingAnimationList;
    fMetaExtraAnimationList   : TMetaExtraAnimationList;
  // data
    fLemmingBitmaps           : TBitmaps;
    fExtraBitmaps             : TBitmaps;
    fAnimationPalette         : TArrayOfColor32;
    fExplosionMaskBitmap      : TBitmap32; // ref
    fBashMasksBitmap          : TBitmap32; // ref
    fBashMasksRTLBitmap       : TBitmap32; // ref
    fMineMasksBitmap          : TBitmap32; // ref
    fMineMasksRTLBitmap       : TBitmap32; // ref
    fCountDownDigitsBitmap    : TBitmap32; // ref
    procedure InitMetadata;
  public
    constructor Create(aStyle: TStyle);
    destructor Destroy; override;
    procedure Load;
    property Style: TStyle read fStyle;
    property MetaLemmingAnimationList: TMetaLemmingAnimationList read fMetaLemmingAnimationList;
    property MetaExtraAnimationList: TMetaExtraAnimationList read fMetaExtraAnimationList;
    property LemmingBitmaps: TBitmaps read fLemmingBitmaps;
  // easy references, these point to the MaskExtraAnimationList
    property ExplosionMaskBitmap   : TBitmap32 read fExplosionMaskBitmap;
    property BashMasksBitmap       : TBitmap32 read fBashMasksBitmap;
    property BashMasksRTLBitmap    : TBitmap32 read fBashMasksRTLBitmap;
    property MineMasksBitmap       : TBitmap32 read fMineMasksBitmap;
    property MineMasksRTLBitmap    : TBitmap32 read fMineMasksRTLBitmap;
    property CountDownDigitsBitmap : TBitmap32 read fCountDownDigitsBitmap;
    property AnimationPalette      : TArrayOfColor32 read fAnimationPalette write fAnimationPalette;
  end;

  TGraphicSet = class sealed
  strict private
    fStyle           : TStyle; // ref to style
  // metadata
    fGraphicSetId    : Integer; // number identifier
    fGraphicSetIdExt : Integer; // extended graphics identifier (vgaspec)
    fMetaDataFile    : string; // ground?.dat
    fGraphicFile     : string; // vgagr?.dat
    fGraphicExtFile  : string; // vgaspec?.dat
    fMetaObjectList  : TMetaObjectList;
    fMetaTerrainList : TMetaTerrainList;
  // data
    fTerrainBitmaps  : TBitmaps;
    fObjectBitmaps   : TBitmaps;
    fSpecialBitmap   : TBitmap32;
    fPaletteCustom   : TArrayOfColor32;  // 8
    fPaletteStandard : TArrayOfColor32;  // 8
    fPalettePreview  : TArrayOfColor32;  // 8
    fPalette         : TArrayOfColor32;  // 16
    fBrickColor      : TColor32;
    procedure Clear;
    procedure LoadMetaData;
    procedure LoadData;
  public
    constructor Create(aStyle: TStyle);
    destructor Destroy; override;
    procedure Load(aId, aIdExt: Integer);
  // properties
    property Style: TStyle read fStyle;
    property GraphicSetId: Integer read fGraphicSetId;
    property GraphicSetIdExt: Integer read fGraphicSetIdExt;
    property MetaTerrainList: TMetaTerrainList read fMetaTerrainList;
    property MetaObjectList: TMetaObjectList read fMetaObjectList;
    property TerrainBitmaps: TBitmaps read fTerrainBitmaps;
    property SpecialBitmap: TBitmap32 read fSpecialBitmap;
    property ObjectBitmaps: TBitmaps read fObjectBitmaps;
    property PaletteCustom: TArrayOfColor32 read fPaletteCustom;
    property PaletteStandard: TArrayOfColor32 read fPaletteStandard;
    property PalettePreview: TArrayOfColor32 read fPalettePreview;
    property Palette: TArrayOfColor32 read fPalette;
    property BrickColor: TColor32 read fBrickColor;
  end;

  TLevelSystem = class
  strict private
    fStyle: TStyle; // owner
    fSectionList: TSectionList;
  private
    procedure DoAddSection(aSection: TSection); // internal
  strict protected
    fOddTableFileName : string;
  protected
    procedure GetFileNamesForGraphicSet(aGraphicSetId, aGraphicSetIdExt: Integer; out aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName: string); virtual;
    procedure DoInitializeLevelSystem; virtual;
  public
    constructor Create(aStyle: TStyle); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override; final;
  // default graphics filenames
    class procedure GetDefaultNamesForGraphics(aGraphicSetId, aGraphicSetIdExt: Integer; out aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName: string); static;
  // level finding
    function FirstLevelOrDefault: TLevelLoadingInformation; inline;
    function FirstLevel: TLevelLoadingInformation; inline;
    function LastLevel: TLevelLoadingInformation; inline;
    function FindLevelByIndex(const aSectionIndex, aLevelIndex: Integer): TLevelLoadingInformation;
    function FindLevelBySectionNameAndNumber(const aCode: string): TLevelLoadingInformation;
    function FindLevelByHash(const aHash: UInt64): TLevelLoadingInformation;
    function SelectRandomLevel: TLevelLoadingInformation;
  // properties
    property OddTableFileName: string read fOddTableFileName;
    property Style: TStyle read fStyle;
    property SectionList: TSectionList read fSectionList;
  end;

  TSection = class sealed
  private
    fLevelSystem: TLevelSystem; // owner
    fSectionIndex: Integer;
    fSectionName: string;
    fPrev: TSection;
    fNext: TSection;
    fLevelLoadingInformationList: TLevelLoadingInformationList;
    function GetStyle: TStyle; inline;
    procedure DoAddLevelInfo(aLevelInfo: TLevelLoadingInformation);
  public
    constructor Create(aLevelSystem: TLevelSystem);
    destructor Destroy; override;
    property Style: TStyle read GetStyle;
    property SectionIndex: Integer read fSectionIndex;
    property SectionName: string read fSectionName write fSectionName;
    property LevelSystem: TLevelSystem read fLevelSystem;
    property LevelLoadingInformationList: TLevelLoadingInformationList read fLevelLoadingInformationList;
    property Prev: TSection read fPrev;
    property Next: TSection read fNext;
  end;

  TLevelLoadingInformation = class sealed
  private
    fSection                  : TSection;
    fLevelIndex               : Integer;
    fSourceFileName           : string; // loading info
    fSectionIndexInSourceFile : Integer; // loading info
    fUseOddTable              : Boolean; // loading info
    fOddTableIndex            : Integer; // loading info
    fIsRawLVLFile             : Boolean; // loading info
    fIsLemminiFile            : Boolean; // loading info
    fPrev                     : TLevelLoadingInformation;
    fNext                     : TLevelLoadingInformation;
    fCachedLVL                : PLVLRec;
    fCachedHash               : UInt64;
    fCachedLevelCode          : string;
    fMusicFileName            : string;
    fMusicStreamType          : TMusicStreamType;
    function GetStyle: TStyle; inline;
    function GetLevelSystem: TLevelSystem; inline;
    function GetSectionIndex: Integer; inline;
    function GetIsCached: Boolean; inline;
    procedure CacheLVL; inline;
    procedure SetMusicFilename(const s: string);
  public
    constructor Create(aSection: TSection);
    destructor Destroy; override;
    procedure LoadLevel(aLevel: TLevel);
    function GetLevelHash: UInt64;
    function GetLevelCode: string;
    function GetRawLVL: TLVLRec;
    function GetRawLVLTitle: TLVLTitle;
    function GetLevelTitle(trimmed: Boolean = True): string;
//    function GenFileNameWithoutExtension: string;
  // properties
    property Style: TStyle read GetStyle;
    property LevelSystem: TLevelSystem read GetLevelSystem;
    property CachedLVL: PLVLRec read fCachedLVL;
    property Section: TSection read fSection;
    property SectionIndex: Integer read GetSectionIndex;
    property LevelIndex: Integer read fLevelIndex;
    property SourceFileName: string read fSourceFileName write fSourceFileName; // loading info
    property SectionIndexInSourceFile: Integer read fSectionIndexInSourceFile write fSectionIndexInSourceFile; // loading info
    property UseOddTable: Boolean read fUseOddTable write fUseOddTable; // loading info
    property OddTableIndex: Integer read fOddTableIndex write fOddTableIndex; // loading info
    property IsRawLVLFile: Boolean read fIsRawLVLFile write fIsRawLVLFile; // loading info
    property IsLemminiFile: Boolean read fIsLemminiFile write fIsLemminiFile; // loading info
    property MusicFileName: string read fMusicFileName write SetMusicFileName;  // loading info
    property MusicStreamType: TMusicStreamType read fMusicStreamType;  // loading info
    property Prev: TLevelLoadingInformation read fPrev;
    property Next: TLevelLoadingInformation read fNext;
    property IsCached: Boolean read GetIsCached;
  end;

type
  TLevelFactory = class sealed
  private
    class procedure LoadLVL(aInfo: TLevelLoadingInformation; var LVL: TLVLRec); static;
  end;

function GenFilenameWithoutExtension(const aStylename: string; aSectionIndex, aLevelIndex: Integer; const title: TLVLTitle): string; overload;
function GenFilenameWithoutExtension(info: TLevelLoadingInformation): string; overload; inline;

implementation

function GenFilenameWithoutExtension(const aStylename: string; aSectionIndex, aLevelIndex: Integer; const title: TLVLTitle): string;
begin
  Result := aStylename + '.' + Succ(aSectionIndex).ToString + '.' + Succ(aLevelIndex).ToString.PadLeft(3, '0') + '.' + LVLTitleAsString(title, True);
  Result := StripInvalidFileChars(Result, False, True, True);
end;

function GenFilenameWithoutExtension(info: TLevelLoadingInformation): string; overload; inline;
begin
  Result := GenFilenameWithoutExtension(info.Style.Name, info.SectionIndex, info.LevelIndex, info.GetRawLVLTitle);
end;

{ TStyle }

procedure TStyle.BeforeDestruction;
begin
  if fIsPooledByFactory then
    Throw('Pooled style destruction error', 'BeforeDestruction')
end;

constructor TStyle.Create(const aName: string);
begin
  if GetLevelSystemClass = nil then
    Throw('TStyle.Create (' + ClassName + '): undefined levelsystemclass');

  // name and def first
  fName := aName;
  // set the styledef
  for var info: Consts.TStyleInformation in Consts.StyleInformationlist do
    if info.Name = fName then begin
      fStyleInformation := info;
      fDef := info.StyleDef;
      Break;
    end;
end;

procedure TStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  fMainDatFileName := GetMainDatFileName;
  fMechanics := GetMechanics;
  fLemmingAnimationSet := TLemmingAnimationSet.Create(Self);
  fLevelSystem := GetLevelSystemClass.Create(Self);
end;

destructor TStyle.Destroy;
begin
  fLevelSystem.Free;
  fLemmingAnimationSet.Free;
  inherited;
end;

function TStyle.GetMainDatFileName: string;
begin
  Result := 'main.dat';
end;

function TStyle.GetRootPath: string;
begin
  Result := Consts.PathToStyle[fName];
end;

{ TLemmingAnimationSet }

constructor TLemmingAnimationSet.Create(aStyle: TStyle);
begin
  fStyle := aStyle;
  fMetaLemmingAnimationList := TMetaLemmingAnimationList.Create;
  fMetaExtraAnimationList := TMetaExtraAnimationList.Create;
  fLemmingBitmaps := TBitmaps.Create;
  fExtraBitmaps := TBitmaps.Create;
  InitMetaData;
end;

destructor TLemmingAnimationSet.Destroy;
begin
  fMetaLemmingAnimationList.Free;
  fMetaExtraAnimationList.Free;
  fLemmingBitmaps.Free;
  fExtraBitmaps.Free;
  inherited;
end;

procedure TLemmingAnimationSet.InitMetadata;
{-------------------------------------------------------------------------------
  We dont have to read. It's fixed in this order in the main.dat.
  foot positions from ccexpore's emails, see lemming_mechanics pseudo code

  o make lemming animations
  o make mask animations metadata
-------------------------------------------------------------------------------}

    procedure Lem(aImageLocation: Integer; const aDescription: string; aFrameCount, aWidth, aHeight, aBPP, aFootX, aFootY: Integer; aAnimType: TLemmingAnimationType);
    begin
      var A: TMetaLemmingAnimation := TMetaLemmingAnimation.Create(aDescription, aFrameCount, aWidth, aHeight, aBPP, aImageLocation, aAnimType, aFootX, aFootY);
      fMetaLemmingAnimationList.Add(A);
    end;

    procedure Msk(aImageLocation: Integer; const aDescription: string; aFrameCount, aWidth, aHeight, aBPP: Integer);
    begin
      var M: TMetaExtraAnimation := TMetaExtraAnimation.Create(aDescription, aFrameCount, aWidth, aHeight, aBPP, aImageLocation);
      fMetaExtraAnimationList.Add(M);
    end;

begin

  //  place   description            F   W   H  BPP   FX   FY   animationtype
  Lem($0000, 'Walking'           ,   8, 16, 10,   2,   8,  10,   TLemmingAnimationType.Loop); // 0
  Lem($0140, 'Jumping'           ,   1, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once);
  Lem($0168, 'Walking (rtl)'     ,   8, 16, 10,   2,   8,  10,   TLemmingAnimationType.Loop);
  Lem($02A8, 'Jumping (rtl)'     ,   1, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once);
  Lem($02D0, 'Digging'           ,  16, 16, 14,   3,   8,  12,   TLemmingAnimationType.Loop);
  Lem($0810, 'Climbing'          ,   8, 16, 12,   2,   8,  12,   TLemmingAnimationType.Loop);
  Lem($0990, 'Climbing (rtl)'    ,   8, 16, 12,   2,   8,  12,   TLemmingAnimationType.Loop);
  Lem($0B10, 'Drowning'          ,  16, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once);
  Lem($0D90, 'Hoisting'          ,   8, 16, 12,   2,   8,  12,   TLemmingAnimationType.Once);
  Lem($0F10, 'Hoisting (rtl)'    ,   8, 16, 12,   2,   8,  12,   TLemmingAnimationType.Once);
  Lem($1090, 'Building'          ,  16, 16, 13,   3,   8,  13,   TLemmingAnimationType.Loop); // 10
  Lem($1570, 'Building (rtl)'    ,  16, 16, 13,   3,   8,  13,   TLemmingAnimationType.Loop);
  Lem($1A50, 'Bashing'           ,  32, 16, 10,   3,   8,  10,   TLemmingAnimationType.Loop);
  Lem($21D0, 'Bashing (rtl)'     ,  32, 16, 10,   3,   8,  10,   TLemmingAnimationType.Loop);
  Lem($2950, 'Mining'            ,  24, 16, 13,   3,   8,  13,   TLemmingAnimationType.Loop);
  Lem($30A0, 'Mining (rtl)'      ,  24, 16, 13,   3,   8,  13,   TLemmingAnimationType.Loop);
  Lem($37F0, 'Falling'           ,   4, 16, 10,   2,   8,  10,   TLemmingAnimationType.Loop);
  Lem($3890, 'Falling (rtl)'     ,   4, 16, 10,   2,   8,  10,   TLemmingAnimationType.Loop);
  Lem($3930, 'Umbrella'          ,   8, 16, 16,   3,   8,  16,   TLemmingAnimationType.Loop);
  Lem($3C30, 'Umbrella (rtl)'    ,   8, 16, 16,   3,   8,  16,   TLemmingAnimationType.Loop);
  Lem($3F30, 'Splatting'         ,  16, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once); // 20
  Lem($41B0, 'Exiting'           ,   8, 16, 13,   2,   8,  13,   TLemmingAnimationType.Once);
  Lem($4350, 'Vaporizing'        ,  14, 16, 14,   4,   8,  14,   TLemmingAnimationType.Once);
  Lem($4970, 'Blocking'          ,  16, 16, 10,   2,   8,  10,   TLemmingAnimationType.Loop);
  Lem($4BF0, 'Shrugging'         ,   8, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once);
  Lem($4D30, 'Shrugging (rtl)'   ,   8, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once);
  Lem($4E70, 'Oh-No-ing'         ,  16, 16, 10,   2,   8,  10,   TLemmingAnimationType.Once);
  Lem($50F0, 'Exploding'         ,   1, 32, 32,   3,  16,  25,   TLemmingAnimationType.Once); // 27


  //  place   description            F   W   H  BPP
  Msk($0000, 'Bashmasks'         ,   4, 16, 10,   1);
  Msk($0050, 'Bashmasks (rtl)'   ,   4, 16, 10,   1);
  Msk($00A0, 'Minemasks'         ,   2, 16, 13,   1);
  Msk($00D4, 'Minemasks (rtl)'   ,   2, 16, 13,   1);
  Msk($0108, 'Explosionmask'     ,   1, 16, 22,   1);
  Msk($0154, 'Countdown digits'  ,   5,  8,  8,   1); // we only take the digits 5 downto zero
end;

procedure TLemmingAnimationSet.Load;
var
  Fn: string;
  Mem: TMemoryStream;
  Bmp: TBitmap32;
  TempBitmap: TBitmap32;
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  iFrame: Integer;
  MLA: TMetaLemmingAnimation;
  MA: TMetaExtraAnimation;
  Y: Integer;
  Pal: TArrayOfColor32;
  LocalStream: TStream;

begin
  fLemmingBitmaps.Clear;
  fExtraBitmaps.Clear;
  // fried and or vaporizing have high color indices
  {$ifdef paranoid} Assert(Length(AnimationPalette) >= 16); {$endif}
  Pal := Copy(fAnimationPalette);

  Fn := Style.MainDatFileName;
  TempBitmap := TBitmap32.Create;
  Sections := TDosDatSectionList.Create;
  Decompressor := TDosDatDecompressor.Create;

  try

    LocalStream := TData.CreateDataStream(Style.Name, Fn, TDataType.LemmingData);
    try
      Decompressor.LoadSectionList(LocalStream, Sections, False);
    finally
      LocalStream.Free;
    end;
    Decompressor.DecompressSection(Sections[0].CompressedData, Sections[0].DecompressedData);
    Mem := Sections[0].DecompressedData;

    for MLA in fMetaLemmingAnimationList do begin
      Bmp := TBitmap32.Create;
      fLemmingBitmaps.Add(Bmp);
      TDosPlanarBitmap.LoadAnimationFromStream(Mem, Bmp, MLA.ImageLocation, MLA.Width, MLA.Height, MLA.FrameCount, MLA.BitsPerPixel, Pal);
      //bmp.ReplaceColor(Color32(64,64,224), cllime32);
    end;


    Pal[0] := 0;
    Pal[1] := clMask32; // the ugly mask pink.
    Decompressor.DecompressSection(Sections[1].CompressedData, Sections[1].DecompressedData);
    Mem := Sections[1].DecompressedData;
    for MA in fMetaExtraAnimationList do begin
      Y := 0;
      Bmp := TBitmap32.Create;
      fExtraBitmaps.Add(Bmp);
      // also: here the direct LoadAnimationFromStream does not work correctly. there are masks inbetween i think.
      Bmp.Width := MA.Width;
      Bmp.Height := MA.FrameCount * MA.Height;
      Bmp.Clear(0);
      Mem.Seek(MA.ImageLocation, soFromBeginning);
      for iFrame := 0 to MA.FrameCount - 1 do
      begin
        TDosPlanarBitmap.LoadFromStream(Mem, TempBitmap, -1, MA.Width, MA.Height, MA.BitsPerPixel, Pal);
        TempBitmap.DrawTo(Bmp, 0, Y);
        Inc(Y, MA.Height);
      end;
    end;

    // refer the "easy access" bitmaps
    fBashMasksBitmap := fExtraBitmaps[0];
    fBashMasksRTLBitmap := fExtraBitmaps[1];
    fMineMasksBitmap := fExtraBitmaps[2];
    fMineMasksRTLBitmap := fExtraBitmaps[3];
    fExplosionMaskBitmap := fExtraBitmaps[4];
    fCountDownDigitsBitmap := fExtraBitmaps[5];

    fCountDownDigitsBitmap.ReplaceColor(clMask32, Pal[3]);

  finally
    Sections.Free;
    Decompressor.Free;
    TempBitmap.Free;
  end;
end;

{ TGraphicSet }

constructor TGraphicSet.Create(aStyle: TStyle);
begin
  fStyle := aStyle;
  fGraphicSetId := -1;
  fGraphicSetIdExt := -1;
  fMetaObjectList := TMetaObjectList.Create;
  fMetaTerrainList := TMetaTerrainList.Create;
  fTerrainBitmaps := TBitmaps.Create;
  fObjectBitmaps := TBitmaps.Create;
  fSpecialBitmap := TBitmap32.Create;
end;

destructor TGraphicSet.Destroy;
begin
  fMetaObjectList.Free;
  fMetaTerrainList.Free;
  fTerrainBitmaps.Free;
  fObjectBitmaps.Free;
  fSpecialBitmap.Free;
  inherited;
end;

procedure TGraphicSet.Clear;
begin
  fMetaObjectList.Clear;
  fMetaTerrainList.Clear;
  fTerrainBitmaps.Clear;
  fObjectBitmaps.Clear;
  fSpecialBitmap.SetSize(0, 0);
  SetLength(fPaletteCustom, 0);
  SetLength(fPaletteStandard, 0);
  SetLength(fPalettePreview, 0);
  SetLength(fPalette, 0);
  fBrickColor := 0;
  fMetaDataFile := string.Empty;
  fGraphicFile := string.Empty;
  fGraphicExtFile := string.Empty;
end;

procedure TGraphicSet.LoadMetaData;
// Read dos metadata and translate to lemmix classes

    procedure LoadDosMetaData(var aData: TDosGroundRec);
    var
      D: TStream;
    begin
      D := TData.CreateDataStream(Style.Name, fMetaDataFile, TDataType.LevelGraphics);
      try
        D.ReadBuffer(aData, SizeOf(aData));
      finally
        D.Free;
      end;
    end;

    function LoadSpecPalette: TDosVGAPalette8;
    var
      SpecBmp: TVgaSpecBitmap;
      DataStream: TStream;
    begin
      SpecBmp := TVgaSpecBitmap.Create;
      DataStream := TData.CreateDataStream(Style.Name, fGraphicExtFile, TDataType.LevelSpecialGraphics);
      try
        SpecBmp.LoadPaletteFromStream(DataStream, Result);
      finally
        SpecBmp.Free;
        DataStream.Free;
      end;
    end;

    procedure AssemblePalette;
    // Concatenate the fixed palette and the loaded custompalette.
    // Then copy the first customcolor to the last fixed color (bridges, minimap).
    // For special graphics we use a hardcoded color for now.
    var
      i: Integer;
    begin
      SetLength(fPalette, 16);
      for i := 0 to 7 do
        fPalette[i] := DosPaletteEntryToColor32(DosInlevelPalettes[Consts.ChristmasPalette][i]);
      for i := 8 to 15 do
        fPalette[i] := fPaletteCustom[i - 8];
      if fGraphicSetIdExt > 0 then
        fPalette[8] := Color32(124, 124, 0, 0);
      fPalette[7] := fPalette[8];
      fBrickColor := fPalette[7];
    end;

var
  DosData: TDosGroundRec;

begin
  LoadDosMetaData(DosData);

  // convert palettes
  DosVgaPalette8ToLemmixPalette(DosData.VGA_PaletteCustom, fPaletteCustom);
  DosVgaPalette8ToLemmixPalette(DosData.VGA_PaletteStandard, fPaletteStandard);
  DosVgaPalette8ToLemmixPalette(DosData.VGA_PalettePreview, fPalettePreview);

  // if special graphic then overwrite the custom palette
  if fGraphicSetIdExt > 0 then
    DosVgaPalette8ToLemmixPalette(LoadSpecPalette, fPaletteCustom);

  // make 16 color palette
  AssemblePalette;

  // meta objects
  for var O: TDosMetaObject in DosData.ObjectInfoArray do begin
    if O.oWidth = 0 then
      Break; // the rest is empty
    var MO: TMetaObject := TMetaObject.Create;
    fMetaObjectList.Add(MO);
    MO.AssignFromDos(O);
  end;

  // if extended graphic then no terrain
  if fGraphicSetIdExt > 0 then
    Exit;

  // meta terrains
  for var T: TDosMetaTerrain in DosData.TerrainInfoArray do begin
    if T.tWidth = 0 then
      Break; // the rest is empty
    var MT: TMetaTerrain := TMetaTerrain.Create;
    fMetaTerrainList.Add(MT);
    MT.AssignFromDos(T);
  end;
end;

procedure TGraphicSet.LoadData;
// read all terrain- and object bitmaps from dos file and translate to lemmix classes
var
  DosSections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  MemStream: TMemoryStream;
  DataStream: TStream;
begin

  DosSections := TDosDatSectionList.Create;
  try
    // first decompress all data into the sectionlist
    DataStream := nil;
    Decompressor := TDosDatDecompressor.Create;
    try
      DataStream := TData.CreateDataStream(Style.Name, fGraphicFile, TDataType.LevelGraphics);
      Decompressor.LoadSectionList(DataStream, DosSections, True);
    finally
      Decompressor.Free;
      DataStream.Free;
    end;

    // get terrains from the first section if this is a normal graphicset (#0)
    if fGraphicSetIdExt <= 0 then begin
      MemStream := DosSections[0].DecompressedData;
      for var MT: TMetaTerrain in fMetaTerrainList do begin
        var Bmp: TBitmap32 := TBitmap32.Create;
        fTerrainBitmaps.Add(Bmp);
        TDosPlanarBitmap.LoadFromStream(MemStream, Bmp, MT.ImageLocation, MT.Width, MT.Height, 4, fPalette);
      end;
    end
    // or get the one terrainbitmap from the vgaspec
    else begin
      var SpecBmp: TVgaSpecBitmap := TVgaSpecBitmap.Create;
      try
        DataStream := TData.CreateDataStream(Style.Name, fGraphicExtFile, TDataType.LevelSpecialGraphics);
        try
          SpecBmp.LoadFromStream(DataStream, fSpecialBitmap);
        finally
          DataStream.Free;
        end;
      finally
        SpecBmp.Free;
      end;
    end;

    // get objects from the second section (#1)
    // loadanimationfromstream is not really impossible. maybe there are gaps, maybe some offset error
    // get objects from the second section
    MemStream := DosSections[1].DecompressedData;
    for var MO: TMetaObject in fMetaObjectList do begin
      var Bmp: TBitmap32 := TBitmap32.Create;
      Bmp.SetSize(MO.Width, MO.Height * MO.AnimationFrameCount);
      fObjectBitmaps.Add(Bmp);
      var y: Integer := 0;
      var Loc: Integer := MO.AnimationFramesBaseLoc;
      var framebitmap: TBitmap32 := TBitmap32.Create;
      // load all animation frames and glue together
      for var f: Integer := 0 to MO.AnimationFrameCount - 1 do begin
        TDosPlanarBitmap.LoadFromStream(MemStream, FrameBitmap, Loc, MO.Width, MO.Height, 4, fPalette);
        FrameBitmap.DrawTo(Bmp, 0, Y);
        Inc(Y, MO.Height);
        Inc(Loc, MO.AnimationFrameDataSize);
      end;
      framebitmap.Free;
    end;

  finally
    DosSections.Free;
  end;

end;

procedure TGraphicSet.Load(aId, aIdExt: Integer);
begin
  Clear;
  fGraphicSetId := aId;
  fGraphicSetIdExt := aIdExt;
  // get the filenames from which we load the metadata and data
  Style.LevelSystem.GetFileNamesForGraphicSet(fGraphicSetId, fGraphicSetIdExt, {out} fMetaDataFile, {out} fGraphicFile, {out} fGraphicExtFile);
  LoadMetaData;
  LoadData;
end;

{ TLevelSystem }

constructor TLevelSystem.Create(aStyle: TStyle);
begin
  fStyle := aStyle;
  fSectionList := TSectionList.Create;
end;

procedure TLevelSystem.AfterConstruction;
begin
  inherited AfterConstruction;
  DoInitializeLevelSystem; // let descendant implement sections and levels

  // do a little validation on the descendants, who are responsibly for filling sections and levelinformation
  if SectionList.IsEmpty then
    Throw('No game sections implemented', 'AfterConstruction');

  for var section: TSection in Sectionlist do
    if section.LevelLoadingInformationList.IsEmpty then
      Throw('No levelinformation found in one of the sections', 'AfterConstruction');
end;

destructor TLevelSystem.Destroy;
begin
  fSectionList.Free;
  inherited;
end;

class procedure TLevelSystem.GetDefaultNamesForGraphics(aGraphicSetId, aGraphicSetIdExt: Integer; out aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName: string);
// all our 6 built in styles use the same naming convention
begin
  aMetaDataFileName := 'ground' + aGraphicSetId.ToString + 'o.dat';
  aGraphicsFileName := 'vgagr' + aGraphicSetId.ToString + '.dat';
  aSpecialGraphicsFileName := string.Empty;
  if aGraphicSetIdExt > 0 then
    aSpecialGraphicsFileName := 'vgaspec' + pred(aGraphicSetIdExt).ToString + '.dat' // 1 maps to 0 for filename
end;

procedure TLevelSystem.GetFileNamesForGraphicSet(aGraphicSetId, aGraphicSetIdExt: Integer; out aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName: string);
// this is default and only overwritten for userstyle if needed
begin
  GetDefaultNamesForGraphics(aGraphicSetId, aGraphicSetIdExt, aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName);
end;

procedure TLevelSystem.DoAddSection(aSection: TSection);
var
  ix: Integer;
  thePrev: TSection;
begin
  aSection.fSectionIndex := fSectionList.Count;
  fSectionList.Add(aSection);
  ix := aSection.fSectionIndex;
  if ix > 0 then begin
    thePrev := fSectionList[ix - 1];
    thePrev.fNext := aSection;
    aSection.fPrev := thePrev;
  end
  else begin
    thePrev := aSection.fPrev;
    if Assigned(thePrev) and thePrev.fLevelLoadingInformationList.HasItems then begin
      thePrev.fNext := aSection;
      aSection.fPrev := thePrev;
    end;
  end;
end;

procedure TLevelSystem.DoInitializeLevelSystem;
begin
  // must override
end;

function TLevelSystem.FirstLevelOrDefault: TLevelLoadingInformation;
begin
  if SectionList.IsEmpty or SectionList.First.LevelLoadingInformationList.IsEmpty then
    Exit(nil);
  Result := SectionList.First.LevelLoadingInformationList.First;
end;

function TLevelSystem.FirstLevel: TLevelLoadingInformation;
begin
  {$ifdef paranoid}
  if SectionList.IsEmpty or SectionList.First.LevelLoadingInformationList.IsEmpty then
    Throw('No levels in system');
  {$endif}
  Result := SectionList.First.LevelLoadingInformationList.First;
end;

function TLevelSystem.LastLevel: TLevelLoadingInformation;
begin
  {$ifdef paranoid}
  if SectionList.IsEmpty or SectionList.Last.LevelLoadingInformationList.IsEmpty then
    Throw('No levels in system');
  {$endif}
  Result := SectionList.Last.LevelLoadingInformationList.Last;
end;

function TLevelSystem.FindLevelByIndex(const aSectionIndex, aLevelIndex: Integer): TLevelLoadingInformation;
begin
  if (aSectionIndex < fSectionList.Count) and (aLevelIndex < fSectionList[aSectionIndex].LevelLoadingInformationList.Count) then
    Result := fSectionList[aSectionIndex].LevelLoadingInformationList[aLevelIndex]
  else
    Result := nil;
end;

function TLevelSystem.FindLevelBySectionNameAndNumber(const aCode: string): TLevelLoadingInformation;
var
  sectionstring: string;
  levelstring: string;
  foundSection: TSection;
  charsFound, digitsFound: Boolean;
  ix: Integer;
begin
  Result := nil;
  foundSection := nil;
  charsFound := False;
  digitsFound := False;

  if Length(aCode) < 2 then
    Exit;

  sectionstring := string.Empty;
  levelstring := string.Empty;
  for var C: Char in aCode do
    if C.IsLetter then begin
      if digitsFound then
        Exit;
      charsFound := True;
      sectionstring := sectionstring + C;
    end
    else if C.IsDigit then begin
      if not CharsFound then
        Exit;
      digitsFound := True;
      levelstring := levelstring + C;
    end;

  for var section: TSection in SectionList do
    if SameText(section.SectionName, sectionstring) then begin
      foundSection := section;
      Break;
    end;

  if foundSection = nil then
    Exit;

  if not TryStrToInt(levelstring, ix) then
    Exit;

  if (ix < 1) or (ix > foundSection.LevelLoadingInformationList.Count) then
    Exit;

  Dec(ix);

  Result := foundSection.LevelLoadingInformationList[ix];
end;

function TLevelSystem.FindLevelByHash(const aHash: UInt64): TLevelLoadingInformation;
begin
  for var section: TSection in fSectionList do
    for var info: TLevelLoadingInformation in section.fLevelLoadingInformationList do begin
      if info.GetLevelHash = aHash then
        Exit(info);
    end;
  Result := nil;
end;

function TLevelSystem.SelectRandomLevel: TLevelLoadingInformation;
begin
  var s: Integer := Random(fSectionList.Count);
  var l: Integer := Random(fSectionList[s].fLevelLoadingInformationList.Count);
  Result := FindLevelByIndex(s, l);
end;

{ TSection }

constructor TSection.Create(aLevelSystem: TLevelSystem);
begin
  fLevelSystem := aLevelSystem;
  fLevelLoadingInformationList := TLevelLoadingInformationList.Create;
  fLevelSystem.DoAddSection(Self);
end;

destructor TSection.Destroy;
begin
  fLevelLoadingInformationList.Free;
  inherited;
end;

procedure TSection.DoAddLevelInfo(aLevelInfo: TLevelLoadingInformation);
var
  ix: Integer;
  thePrev: TLevelLoadingInformation;
begin
  aLevelInfo.fLevelIndex := fLevelLoadingInformationList.Count;
  fLevelLoadingInformationList.Add(aLevelInfo);

  ix := aLevelInfo.fLevelIndex;
  if ix > 0 then begin
    thePrev := fLevelLoadingInformationList[ix - 1];
    thePrev.fNext := aLevelInfo;
    aLevelInfo.fPrev := thePrev;
  end
  else begin
    if Assigned(fPrev) and fPrev.LevelLoadingInformationList.HasItems then begin
      thePrev := fPrev.LevelLoadingInformationList.Last;
      thePrev.fNext := aLevelInfo;
      aLevelInfo.fPrev := thePrev;
    end;
  end;

end;

function TSection.GetStyle: TStyle;
begin
  Result := fLevelSystem.Style;
end;

{ TLevelLoadingInformation }

constructor TLevelLoadingInformation.Create(aSection: TSection);
begin
  fSection := aSection;
  fSection.DoAddLevelInfo(Self);
end;

destructor TLevelLoadingInformation.Destroy;
begin
  if fCachedLVL <> nil then
    Dispose(fCachedLVL);
  inherited;
end;

procedure TLevelLoadingInformation.SetMusicFilename(const s: string);
var
  ext: string;
begin
  fMusicFileName := s;
  ext := ExtractFileExt(s).ToUpper;
  if ext = '.MOD' then
    fMusicStreamType := TMusicStreamType.&MOD
  else if ext = '.MP3' then
    fMusicStreamType := TMusicStreamType.MP3
  else
    fMusicStreamType := TMusicStreamType.None;
end;

//function TLevelLoadingInformation.GenFileNameWithoutExtension(includeStyle: Boolean = True; includeSection: Boolean = True; includeLevelIndex: Boolean = True): string;
//
//  procedure AddDot;
//  begin
//    if Result <> string.Empty then
//      Result := Result + '.';
//  end;
//
//var
//  title: string;
//
//begin
//  Result := string.Empty;
//  if includeStyle then
//    Result := Result + Style.Name;
//
//  if includeSection then begin
//    AddDot;
//    Result := Result + Section.SectionName;
//  end;
//
//  if includeLevelIndex then begin
//    AddDot;
//    Result := Result + Succ(LevelIndex).ToString.PadLeft(3, '0');
//  end;
//
//  AddDot;
//  title := GetLevelTitle(true);
//  if title.IsEmpty then
//    title := 'noname';
//  result := Result + title;
//
//  Result := StripInvalidFileChars(Result, False, False, True);
//end;

function TLevelLoadingInformation.GetIsCached: Boolean;
begin
  Result := Assigned(fCachedLVL);
end;

function TLevelLoadingInformation.GetLevelSystem: TLevelSystem;
begin
  Result := fSection.LevelSystem;
end;

function TLevelLoadingInformation.GetSectionIndex: Integer;
begin
  Result := fSection.fSectionIndex;
end;

function TLevelLoadingInformation.GetStyle: TStyle;
begin
  Result := fSection.Style;
end;

procedure TLevelLoadingInformation.CacheLVL;
// only accessible in this unit
begin
  if not IsCached then begin
    New(fCachedLVL);
    TLevelFactory.LoadLVL(Self, fCachedLVL^);
    fCachedHash := TLevelHasher.ShortHash(fCachedLVL^);
    fCachedLevelCode := TLevelHasher.GetLevelCode(fCachedHash);
  end;
end;

procedure TLevelLoadingInformation.LoadLevel(aLevel: TLevel);
begin
  if IsCached then
    TLevelLoader.TranslateLevel(fCachedLVL^, aLevel)
  else begin
    CacheLVL;
    TLevelLoader.TranslateLevel(fCachedLVL^, aLevel)
  end;
end;

function TLevelLoadingInformation.GetLevelHash: UInt64;
begin
  CacheLVL;
  Result := fCachedHash;
end;

function TLevelLoadingInformation.GetLevelCode: string;
begin
  CacheLVL;
  Result := fCachedLevelCode;
end;

function TLevelLoadingInformation.GetRawLVLTitle: TLVLTitle;
begin
  CacheLVL;
  Result := fCachedLVL^.LevelName;
end;

function TLevelLoadingInformation.GetLevelTitle(trimmed: Boolean): string;
begin
  CacheLVL;
  Result := string(fCachedLVL^.LevelName);
  if trimmed then
    Result := Result.Trim;
end;

function TLevelLoadingInformation.GetRawLVL: TLVLRec;
begin
  CacheLVL;
  Result := fCachedLVL^;
end;

{ TLevelFactory }

class procedure TLevelFactory.LoadLVL(aInfo: TLevelLoadingInformation; var LVL: TLVLRec);
// NB: a little moving/messing around here with mem, only do this when using ansichars
var
  OddTable: TDosOddTable;
  Ox: Integer;
  DataStream: TStream;
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  TheSection: TDosDatSection;
begin
  // LVL
  if aInfo.IsRawLVLFile then begin
    TLevelLoader.LoadLVLFromFile(Consts.PathToLemmings[aInfo.Style.Name] + aInfo.fSourceFileName, LVL);
    Exit;
  end
  // Lemmini
  else if aInfo.IsLemminiFile then begin
    TLemminiLoader.LoadLVLFromFile(aInfo.Style.RootPath + aInfo.fSourceFileName, LVL);
    Exit;
  end;

  // otherwise DAT
  OddTable.Clear;

  Sections := TDosDatSectionList.Create;
  Decompressor := TDosDatDecompressor.Create;
  try
    DataStream := TData.CreateDataStream(aInfo.Style.Name, aInfo.SourceFileName, TDataType.Level);
    try
      Decompressor.LoadSectionList(DataStream, Sections, False);
    finally
      DataStream.Free;
    end;

    TheSection := Sections[aInfo.SectionIndexInSourceFile];
    Decompressor.DecompressSection(TheSection.CompressedData, TheSection.DecompressedData);
    TheSection.DecompressedData.Seek(0, soFromBeginning);
    TheSection.DecompressedData.ReadBuffer(LVL, SizeOf(LVL));
    TheSection.DecompressedData.Seek(0, soFromBeginning);

  // odd level?
  if aInfo.UseOddTable then begin
    Ox := aInfo.OddTableIndex;
    OddTable.LoadFromFile(aInfo.Style.Name, aInfo.LevelSystem.OddTableFileName); // this one also uses the resource if needed
    Move(OddTable.Recs[Ox], LVL, SizeOf(TDosOddTableRec) - 32);
    Move(OddTable.Recs[Ox].LevelName, LVL.LevelName, 32);
    TheSection.DecompressedData.WriteBuffer(LVL, SizeOf(LVL));
  end;

  TheSection.DecompressedData.Seek(0, soFromBeginning);
  TLevelLoader.LoadLVLFromStream(TheSection.DecompressedData, LVL);

  finally
    Decompressor.Free;
    Sections.Free;
  end;

end;

end.

