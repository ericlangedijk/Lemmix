
unit Game;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Types, System.Classes, System.Contnrs, System.SysUtils, System.Math, System.Generics.Collections,
  Vcl.Forms, Vcl.Dialogs, Vcl.Controls, Vcl.Graphics, Vcl.ClipBrd, Vcl.Imaging.PngImage,
  GR32, GR32_OrdinalMaps, GR32_Layers, GR32_Image, GR32_Blend,
  Base.Utils, Base.Types, Base.Bitmaps, Base.Strings,
  Dos.Structures, Dos.Consts, Dos.Bitmaps,
  Meta.Structures,
  Level.Base,
  Styles.Base, Styles.Dos,
  Prog.Base, Prog.Data, Prog.Cache, Prog.Voice,
  Game.Rendering, Game.SkillPanel, Game.Sound;

const
  COMBINE_FLAG_CLIMBER = 1 shl 0;
  COMBINE_FLAG_FLOATER = 1 shl 1;
  COMBINE_FLAG_BUILDER = 1 shl 2;

// we pass everything to the game preparation with this record
type
  TGameInfoRec = record
    Style                                : TStyle;
    Renderer                             : TRenderer;
    SoundMgr                             : TSoundMgr;
    ReplayCache                          : TReplayCache;
    Img                                  : TImage32;
    TargetBitmap                         : TBitmap32;
    DisplayScale                         : Integer;
    Level                                : TLevel;
    LevelLoadingInfo                     : TLevelLoadingInformation;
    GraphicSet                           : TGraphicSet;
    SoundOptions                         : TSoundOptions;
    GameOptions                          : TGameOptions;
    MiscOptions                          : TMiscOptions;
    OptionalMechanics                    : TOptionalMechanics;
    DebugLayerEnabled                    : Boolean;
  end;

type
  TParticleRec = packed record
    DX, DY: ShortInt
  end;
  TParticleArray = packed array[0..79] of TParticleRec;
  TParticleTable = packed array[0..50] of TParticleArray;

const
  ParticleColorIndices: array[0..15] of Byte = (
    4, 15, 14, 13, 12, 11, 10, 9, 8, 11, 10, 9, 8, 7, 6, 2
  );

type
  TLemmingGame = class;
  THighResolutionLayer = class;

  TLemming = class
  private
    function GetLocationBounds: TRect;                       // paint rect in fWorld
    function GetFrameBounds: TRect;                          // frame rect from animation bitmap
    function GetCountDownDigitBounds: TRect;                 // painting rect countdown digits
    function GetRTL: Boolean; inline;
  public
    PixelCombine               : TPixelCombineEvent;         // effects
    SavedMap                   : array[0..8] of Byte;        // saves part of fObjectMap of game when blocking
    RectToErase                : TRect;                      // the rectangle of the last drawaction (can include space for countdown digits)
    ListIndex                  : Integer;                    // index in the fLemmingList
    XPos                       : Integer;                    // the "main" foot x position
    YPos                       : Integer;                    // the "main" foot y position
    XDelta                     : Integer;                    // x speed (1 if left to right, -1 if right to left)
    Fallen                     : Integer;                    // number of pixels a faller has fallen
    ExplosionTimer             : Integer;                    // 79 downto 0
    LMA                        : TMetaLemmingAnimation;      // ref to Lemming Meta Animation
    LAB                        : TBitmap32;                  // ref to Lemming Animation Bitmap
    Frame                      : Integer;                    // current animationframe
    MaxFrame                   : Integer;                    // copy from LMA
    AnimationType              : TLemmingAnimationType;      // copy from LMA
    ParticleTimer              : Integer;                    // @particles, 52 downto 0, after explosion
    ParticleFrame              : Integer;                    // the "frame" of the particle drawing algorithm
    FrameTopDy                 : Integer;                    // = -LMA.FootY (ccexplore code compatible)
    FrameLeftDx                : Integer;                    // = -LMA.FootX (ccexplore code compatible)
    FloatParametersTableIndex  : Integer;                    // index for floaters
    NumberOfBricksLeft         : Integer;                    // for builder
    Born                       : Integer;                    // game iteration the lemming was created
    Action                     : TLemmingAction;             // current action of the lemming
    ActionBits                 : Integer;                    // in sync current action of the lemming. faster bit-checking
    ObjectBelow                : Byte;
    ObjectInFront              : Byte;
    EndOfAnimation             : Boolean;
    IsRemoved                  : Boolean;                    // the lemming is not in the level anymore
    IsClimber                  : Boolean;
    IsFloater                  : Boolean;
    IsBlocking                 : Boolean;                    // not always exactly in sync with the action
    IsNewDigger                : Boolean;
    IsExploded                 : Boolean;                    // @particles, set after a Lemming actually exploded, used to control particles-drawing
    PhotoFlashForReplay        : Boolean;
    CombineFlags               : Byte;
  // easy function
    function ActionIn(aFlag: Integer): Boolean; inline;      // fast bitwise actionbits checking
  // easy property
    property RTL: Boolean read GetRTL;
  end;

  // object used by game
  TInteractiveObjectInfo = class
  private
    function GetBounds: TRect;
    function OnlyOnTerrain: Boolean; inline;
  public
    MetaObj        : TMetaObject;
    Obj            : TInteractiveObject;
    CurrentFrame   : Integer;
    Triggered      : Boolean;
    property Bounds: TRect read GetBounds;
  end;

  TLowResolutionMessage = class
  private
    fGame         : TLemmingGame;
    fBuffer       : TBitmap32;
    fDuration     : Integer;
    fCurrentFrame : Integer;
    fLocation     : TPoint;
    fDeltaY       : integer;
    fDeltaX       : Integer;
    fText         : string;
    fEnded        : Boolean;
    procedure NextFrame;
  public
    constructor Create(aGame: TLemmingGame);
    destructor Destroy; override;
    property Duration: Integer read fDuration write fDuration default 32;
    property CurrentFrame: Integer read fCurrentFrame write fCurrentFrame;
    property DeltaY: Integer read fDeltaY write fDeltaY default -2;
    property DeltaX: Integer read fDeltaX write fDeltaX;
    procedure SetText(const Value: string; aColor: TColor32);
    property Buffer: TBitmap32 read fBuffer;
  end;

  THighResolutionMessage = class
  private
    fLayer: THighResolutionLayer;
    fBuffer: TBitmap32;
    fXPos: Integer; // coord = screen coord
    fYPos: Integer;
    fDeltaX: Integer;
    fDeltaY: Integer;
    fDuration: Integer;
    fCurrentFrame: Integer;
    fEnded: Boolean;
    fAlphaDelta: Integer;
    fAlpha: Integer;
  public
    constructor Create(aLayer: THighResolutionLayer; x, y, deltaX, deltaY, duration: Integer; const aText: string; aColor: TColor32);
    destructor Destroy; override;
    procedure Paint(buffer: TBitmap32); inline;
    function GetRect: TRect; inline;
    procedure NextFrame;
  end;

  THighResolutionLayer = class(TPositionedLayer)
  strict private
    fMessageList: TFastObjectList<THighResolutionMessage>;
    fFont: TFont;
  private
    procedure UpdateMessages;
    procedure AddMessage(x, y, deltaX, deltaY, duration: Integer; const aText: string; aColor: TColor32);
    property MessageList: TFastObjectList<THighResolutionMessage> read fMessageList;
    property Font: TFont read fFont;
  protected
  public
    constructor Create(aLayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  end;

  TDebugLayer = class(TPositionedLayer)
  private
    CachedMaxTextWidth: Integer;
  public
    constructor Create(aLayerCollection: TLayerCollection); override;
  end;

  TReplayCursor = class
  strict private
    fGame: TLemmingGame;
    fBitmap: TBitmap32;
    fPosition: TPoint;
    fCursorPos: TPoint;
    fDrawRect: TRect;
    fPreviousDrawRect: TRect;
    fVisible: Boolean;
    fErasable: Boolean;
    fFrames: Integer;
    fAlpha: Byte;
  public
    constructor Create(aGame: TLemmingGame);
    destructor Destroy; override;
    procedure Reset;
    procedure Activate(const aCursorPos: TPoint);
    procedure Decrement; inline;
    property Bitmap: TBitmap32 read fBitmap;
    property Position: TPoint read fPosition;
    property DrawRect: TRect read fDrawRect;
    property PreviousDrawRect: TRect read fPreviousDrawRect write fPreviousDrawRect;
    property Erasable: Boolean read fErasable;
    property Visible: Boolean read fVisible;
  end;

  TLemmingList = class(TFastObjectList<TLemming>);
  TInteractiveObjectInfoList = class(TFastObjectList<TInteractiveObjectInfo>);
  TGameMessageList = class(TFastObjectList<TLowResolutionMessage>);

  // Replay stuff
  TReplayFileHeaderRec = packed record
    Signature                     : array[0..2] of AnsiChar;  //  3 bytes -  3
    Version                       : Byte;                     //  1 byte  -  4
    FileSize                      : Integer;                  //  4 bytes -  8
    HeaderSize                    : Word;                     //  2 bytes - 10
    Mechanics                     : TMechanics;               //  2 bytes - 12
    FirstRecordPos                : Integer;                  //  4 bytes - 16
    Reserved1                     : Byte;                     //  1 byte  - 17  #EL 2021 version 3 replaced with reserved unused bytes (formerly unused and later a wrong ReplayRecordSize)
    Reserved2                     : Byte;                     //  1 byte  - 18
    ReplayRecordCount             : Word;                     //  2 bytes - 20
    Hash                          : UInt64;                   //  8 bytes - 28  #EL 2020 added levelhash for accurate finding of level (replayversion 2)
    ReplayGlitchPauseIterations   : Integer;                  //  4 bytes - 32  #EL 2020: use last reserved int for glitch (replayversion 2)
    LevelTitle                    : TLVLTitle;                // 32 bytes - 64
    procedure Clear; inline;
  end;

  TReplayRec = packed record
    Check              : AnsiChar;     // 1 byte  -  1
    Iteration          : Integer;      // 4 bytes -  5
    ActionFlags        : Word;         // 2 bytes -  7
    AssignedSkill      : Byte;         // 1 byte  -  8
    SelectedButton     : Byte;         // 1 byte  -  9
    ReleaseRate        : Integer;      // 1 byte  - 13
    LemmingIndex       : Integer;      // 4 bytes - 17
    LemmingX           : Integer;      // 4 bytes - 21
    LemmingY           : Integer;      // 4 bytes - 25
    CursorX            : SmallInt;     // 2 bytes - 27
    CursorY            : SmallInt;     // 2 bytes - 29
    Flags              : Byte;         // 1 byte  - 30  #EL 2020: added for accurate replaying of RightClickGlitch (replayversion 3).
    Reserved2          : Byte;
    Reserved3          : Byte;         // 32 bytes
    procedure Clear; inline;
  end;

  TReplayItem = class
  strict private
    fIteration          : Integer;
    fActionFlags        : Word; // raf_xxx
    fAssignedSkill      : Byte;
    fSelectedButton     : Byte;
    fReleaseRate        : Byte;
    fLemmingIndex       : Integer;
    fLemmingX           : Integer;
    fLemmingY           : Integer;
    fCursorY            : Integer;
    fCursorX            : Integer;
    fFlags              : Byte; // rf_xxx
  strict private
    fHasValidCursorData: Boolean;
  public
    property Iteration: Integer read fIteration write fIteration;
    property ActionFlags: Word read fActionFlags write fActionFlags;
    property AssignedSkill: Byte read fAssignedSkill write fAssignedSkill;
    property SelectedButton: Byte read fSelectedButton write fSelectedButton;
    property ReleaseRate: Byte read fReleaseRate write fReleaseRate;
    property LemmingIndex: Integer read fLemmingIndex write fLemmingIndex;
    property LemmingX: Integer read fLemmingX write fLemmingX;
    property LemmingY: Integer read fLemmingY write fLemmingY;
    property CursorX: Integer read fCursorX write fCursorX;
    property CursorY: Integer read fCursorY write fCursorY;
    property Flags: Byte read fFlags write fFlags;
    property HasValidCursorData: Boolean read fHasValidCursorData write fHasValidCursorData;
  end;

  TRecorder = class
  strict private
    fWasSaved: Boolean;
    fWasLoaded: Boolean;
  private
    fGame: TLemmingGame;
    List: TFastObjectList<TReplayItem>;
    fCurrentMechanics: TMechanics;
    fRecordedGlitchPauseIterations: Integer;
    fCurrentHeader: TReplayFileHeaderRec;
    function GetIsEmpty: Boolean; inline;
  public
    constructor Create(aGame: TLemmingGame);
    destructor Destroy; override;
    function Add: TReplayItem; inline;
    procedure Clear; inline;
    procedure Truncate(aCount: Integer); inline;
    procedure SaveToFile(const aFileName: string; updateCache: Boolean);
    procedure SaveToStream(S: TStream);
    procedure SaveToTxt(const aFileName: string; includeGameResult: Boolean);
    function LoadFromFile(const aFileName: string; out error: string): Boolean;
    function LoadFromStream(stream: TStream; out error: string): Boolean;
    class function LoadTitleHashVersionFromHeader(const aFileName: string; out aHash: UInt64; out aVersion: Byte; out aTitle: TLVLTitle): Boolean;
    class function LoadHeader(const aFileName: string; out aHeader: TReplayFileHeaderRec): Boolean;
    property WasLoaded: Boolean read fWasLoaded;
    property WasSaved: Boolean read fWasSaved;
    property CurrentHeader: TReplayFileHeaderRec read fCurrentHeader;
    property IsEmpty: Boolean read GetIsEmpty;
    property RecordedGlitchPauseIterations: Integer read fRecordedGlitchPauseIterations;
  end;

  TReleaseRateStatus = (
    None,
    SlowingDown,
    SpeedingUp
  ); // todo: use this

  TPauseCommandMode = (
    None,
    F11,
    PauseKey
  );

  TLemmingMethod = function (L: TLemming): Boolean of object;
  TLemmingMethodArray = array[TLemmingAction] of TLemmingMethod;
  TSkillMethod = function (Lemming1, Lemming2: TLemming): TLemming of object;
  TSkillMethodArray = array[TLemmingAction] of TSkillMethod;

  TLemmingGame = class
  private
    const
      ClimberColor = clGreen32;
      FloaterColor = clCornFlowerBlue32;
      AthleteColor = clOrangeRed32;
    const
      MESSAGE_COLOR_COUNT = 8;
      fMessageColors: array[0..MESSAGE_COLOR_COUNT - 1] of TColor32 = (
        clBlue32, clRed32, clYellow32, clGreen32, clFuchsia32, clLime32, clLightGray32, clOrange32
      );
  private
  // these vars are "global" because we made OnPixelCombine static
    class var BrickPixelColors      : array[0..11] of TColor32;          // 12 gradient steps (or equal depending on config)
    class var CurrentlyDrawnLemming : TLemming;
    class var BrickPixelColor       : TColor32;
  private
  // vars are ordered by size
  // misc sized stuff
    fParticles                           : TParticleTable;               // all particle offsets
    LemmingMethods                       : TLemmingMethodArray;          // a method for each basic lemming state
    SkillMethods                         : TSkillMethodArray;            // a method for assigning jobs (including dummies)
    fParticleColors                      : array[0..15] of TColor32;
  public
    GameResultRec                        : TGameResultsRec;
  private
    DosEntranceOrderTable                : array[0..3] of Integer;       // for entrance release order
    BashMasks                            : array[Boolean] of TBitmap32;  // ref to style.animationset.bashmasks (not RTL, RTL)
    MineMasks                            : array[Boolean] of TBitmap32;  // ref to style.animationset.minemasks (not RTL, RTL)
    fOnFinish                            : TNotifyEvent;                 // event for gamescreen
  // owned objects
    fLemmingList                         : TLemmingList;                 // the list of lemmings
    fWorld                               : TBitmap32;                    // actual bitmap that is changed by the lemmings
    fMiniMap                             : TBitmap32;                    // fMiniMap of fWorld
    fMinimapBuffer                       : TBitmap32;                    // drawing buffer fMiniMap
    fObjectMap                           : TByteMap;                     // dos compatible 4 pixel resolution map
    fRecorder                            : TRecorder;
    MessageList                          : TGameMessageList;
    ReplayCursor                         : TReplayCursor;
    HighResolutionLayer                  : THighResolutionLayer;
    DebugLayer                           : TDebugLayer;
    fSoundsToPlay                        : TList<Integer>;
    ObjectInfos                          : TInteractiveObjectInfoList;   // list of objects excluding entrances
    Entrances                            : TInteractiveObjectInfoList;   // list of entrances
    // fCurrentlyDrawnLemming               : TLemming;                     // needed for pixelcombining bridges in combinebuilderpixels
    fLastNonPrioritizedLemming           : TLemming;                     // RightClickGlitch emulation (user optional mechanic)
  // reference objects, easy access
    fSoundMgr                            : TSoundMgr;                    // ref
    fReplayCache                         : TReplayCache;                 // ref
    fImg                                 : TImage32;                     // ref to gamescreen img
    fTargetBitmap                        : TBitmap32;                    // ref to the drawing bitmap on the gamewindow
    CntDownBmp                           : TBitmap32;                    // ref to style.animationset.countdowndigits
    ExplodeMaskBmp                       : TBitmap32;                    // ref to style.animationset.explosionmask
    fRenderer                            : TRenderer;                    // ref to gameparams.renderer
    fToolbar                             : TSkillPanelToolbar;           // ref to skillpanel for drawing
    fLevel                               : TLevel;                       // ref to gameparams.level
    fLevelLoadingInfo                    : TLevelLoadingInformation;     // ref to the loading information
    fStyle                               : TStyle;                       // ref to gameparams.style
    fGraph                               : TGraphicSet;                  // ref to gameparams.graph
  // 32 bits sized stuff
    fDisplayScale                        : Integer;
    fCurrentIteration                    : Integer;
    fClockFrame                          : Integer;                      // 17 frames is one game-second
    fGlitchPauseIterations               : Integer;                      // pause glitch
    LemmingsReleased                     : Integer;                      // number of lemmings that were created
    LemmingsOut                          : Integer;                      // number of lemmings currently walking around
    LemmingsSaved                        : integer;                      // number of lemmings that made it to heaven
    LemmingsRemoved                      : Integer;                      // number of lemmings removed
    fCursorPoint                         : TPoint;
    Minutes                              : Integer;                      // minutes left
    Seconds                              : Integer;                      // seconds left
    MaxNumLemmings                       : Integer;
    CurrReleaseRate                      : Integer;
    CurrClimberCount                     : Integer;
    CurrFloaterCount                     : Integer;
    CurrBomberCount                      : Integer;
    CurrBlockerCount                     : Integer;
    CurrBuilderCount                     : Integer;
    CurrBasherCount                      : Integer;
    CurrMinerCount                       : Integer;
    CurrDiggerCount                      : Integer;
    fIsNukedByUser                       : Boolean;
    fIndexOfLemmingToBeNuked             : Integer;
    fCurrentCursor                       : Integer;                      // normal or highlight lemming
    fParticleFinishTimer                 : Integer;                      // extra frames to enable viewing of explosions
    MUSIC_INDEX                          : Integer;                      // music index of soundmgr. there is one music possible
    NextLemmingCountDown                 : Integer;
    fReplayIndex                         : Integer;
    fLastCueSoundIteration               : Integer;
    fMessagesPlayedCount                 : Integer;
    fTargetIteration                     : Integer;                      // this is used in hyperspeed
    fUpdateCallsSession                  : Integer;
    fUpdateCalls                         : Integer;
    fHandleLemmingCalls                  : Integer;
    fReplayMessageCounter                : Integer;
  // 16 bits sized stuff
    fGameOptions                         : TGameOptions;
    fMechanics                           : TMechanics;                   // mechanic options
    fMiscOptions                         : TMiscOptions;
  // 8 bits sized stuff
    fSoundOpts                           : TSoundOptions;
    fSelectedSkill                       : TSkillPanelButton;            // currently selected skill restricted by F3-F9
    fReleaseRateStatus                   : TReleaseRateStatus;
  // states
    fRightMouseButtonHeldDown            : Boolean;
    fPlaying                             : Boolean;                      // game in active playing mode?
    EntrancesOpened                      : Boolean;
    fIsPaused                            : Boolean;
    fIsPausedExt                         : Boolean;                      // paused with pause key todo: rename
    fIsFinished                          : Boolean;
    fIsCheated                           : Boolean;
    fIsExploderAssignInProgress          : Boolean;
    fIsLastRecordedRecordReached         : Boolean;
    fHyperSpeed                          : Boolean;                      // we are at hyperspeed no targetbitmap output
    fTargetBitmapsUpdatingSet            : Boolean;                      // extra optimization for hyperspeed (no flickering when going frames back)
    fEntranceAnimationCompleted          : Boolean;
    fStartupMusicAfterEntrance           : Boolean;
    fFastForward                         : Boolean;
    fReplaying                           : Boolean;
    fExplodingPixelsUpdateNeeded         : Boolean;
    fAssignmentIsRightClickGlitch        : Boolean;                      // storage of the bug. global because we do not want extra parameters in AssignSkill
    fDebugLayerEnabled                   : Boolean;
  public
    SaveToEngineFileAtStartup            : Boolean;
  private
  // pixel combine eventhandlers
    class procedure CombineDefault(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineLemming(F: TColor32; var B: TColor32; M: TColor32); static;
  // colored lemmings
    class procedure CombineClimber(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineFloater(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineAthlete(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineBuilder(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineBuilderClimber(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineBuilderFloater(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineBuilderAthlete(F: TColor32; var B: TColor32; M: TColor32); static;
  // photo
    class procedure CombineLemmingPhotoFlash(F: TColor32; var B: TColor32; M: TColor32); static;
  // masks
    class procedure CombineMask(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineMinimapWorld(F: TColor32; var B: TColor32; M: TColor32); static;
  // private methods
    procedure AddReplayMessage(L: TLemming; aAssignedAction: TLemmingAction; const Suffix: string = string.Empty);
    procedure AddFeedbackMessage(const s: string);
    procedure AdjustReleaseRate(Delta: Integer);
    procedure ApplyBashingMask(L: TLemming; MaskFrame: Integer);
    procedure ApplyExplosionMask(L: TLemming);
    procedure ApplyMinerMask(L: TLemming; MaskFrame, X, Y: Integer);
    procedure BtnInternalSelectOnly(btn: TSkillPanelButton); inline;
    procedure CheckAdjustReleaseRate;
    procedure CheckForGameFinished;
    procedure CheckForInteractiveObjects(L: TLemming);
    function CheckForLevelTopBoundary(L: TLemming; LocalFrameTopDy: Integer = 0): Boolean;
    function CheckForOverlappingField(L: TLemming): Boolean;
    procedure CheckForPlaySoundEffect;
    procedure CheckForReplayAction;
    procedure CheckLemmings;
    procedure CheckSpawnLemming;
    procedure CheckUpdateNuking;
    procedure CreateInfoLayer;
    procedure CreateDebugLayer;
    procedure CueSoundEffect(aSoundId: Integer);
    function DigOneRow(L: TLemming; Y: Integer): Boolean;
    procedure DrawAnimatedObjects;
    procedure DrawLemmings;
    procedure DrawParticles(L: TLemming);
    procedure DrawReplayCursorCheck;
    procedure DrawReplayCursor(const P: TPoint);
//    procedure DrawInitialStatics;
    procedure DrawMessages;
    procedure DrawToolbar;
    procedure DrawMinimap;
    procedure DrawDebug; inline;
    procedure EraseLemmings;
    procedure EraseMessages;
    procedure EraseParticles(L: TLemming);
    procedure EraseReplayCursor;
    function GetTrapSoundIndex(aDosSoundEffect: Integer): Integer;
    function GetIsActuallyReplaying: Boolean; inline;
    function GetIsSpeedingUp: Boolean; inline;
    function GetIsSlowingDown: Boolean; inline;
    function HasPixelAt(X, Y: Integer): Boolean;
    function HasPixelAt_ClipY(X, Y, minY: Integer): Boolean;
    procedure HyperSpeedBegin;
    procedure HyperSpeedEnd;
    procedure IncrementIteration;
    procedure InitializeBrickColors(aBrickPixelColor: TColor32);
    procedure InitializeMiniMap;
    procedure InitializeObjectMap;
    procedure InternalUpdate(force, leaveHyperSpeed: Boolean);
    procedure InternalRefresh;
    procedure LayBrick(L: TLemming);
    function PrioritizedHitTest(out Lemming1, Lemming2: TLemming; const CP: TPoint; CheckRightMouseButton: Boolean): Integer;
    function ReadObjectMap(X, Y: Integer): Byte;
    procedure RecordStartPause;
    procedure RecordEndPause;
    procedure RecordNuke;
    procedure RecordReleaseRate(aActionFlag: Byte);
    procedure RecordSkillAssignment(L: TLemming; aSkill: TLemmingAction; usedLemming2, rightMouseGlitched: Boolean);
    procedure RecordSkillSelection(aSkill: TSkillPanelButton);
//    procedure RefreshTo
    procedure RemoveLemming(L: TLemming);
    procedure RemovePixelAt(X, Y: Integer);
    procedure RepaintSkillPanelButtons;
    function ReplaySkillAssignment(item: TReplayItem): Boolean;
    procedure ReplaySkillSelection(aReplayItem: TReplayItem);
    procedure RestoreMap(L: TLemming);
    procedure SaveMap(L: TLemming);
    procedure UpdatePixelCombine(L: TLemming);
    function SelectReplayMessageTextColor: TColor32; inline;
    function SelectFeedbackMessageTextColor: TColor32; inline;
    procedure SetBlockerField(L: TLemming);
    procedure Transition(L: TLemming; aAction: TLemmingAction; DoTurn: Boolean = False);
    procedure TurnAround(L: TLemming);
    function UpdateExplosionTimer(L: TLemming): Boolean;
    procedure UpdateInteractiveObjects;
    procedure UpdateMessages;
    procedure WriteObjectMap(X, Y: Integer; aValue: Byte);
  // internal events
    procedure DebugLayer_Paint(Sender: TObject; buffer: TBitmap32);
    procedure InfoLayer_Paint(Sender: TObject; buffer: TBitmap32);
  // lemming actions
    function HandleLemming(L: TLemming): Boolean;
    function HandleWalking(L: TLemming): Boolean;
    function HandleJumping(L: TLemming): Boolean;
    function HandleDigging(L: TLemming): Boolean;
    function HandleClimbing(L: TLemming): Boolean;
    function HandleDrowning(L: TLemming): Boolean;
    function HandleHoisting(L: TLemming): Boolean;
    function HandleBuilding(L: TLemming): Boolean;
    function HandleBashing(L: TLemming): Boolean;
    function HandleMining(L: TLemming): Boolean;
    function HandleFalling(L: TLemming): Boolean;
    function HandleFloating(L: TLemming): Boolean;
    function HandleSplatting(L: TLemming): Boolean;
    function HandleExiting(L: TLemming): Boolean;
    function HandleVaporizing(L: TLemming): Boolean;
    function HandleBlocking(L: TLemming): Boolean;
    function HandleShrugging(L: TLemming): Boolean;
    function HandleOhNoing(L: TLemming): Boolean;
    function HandleExploding(L: TLemming): Boolean;
  // skill interaction
    function AssignSkill(Lemming1, Lemming2: TLemming; aSkill: TLemmingAction): TLemming;
    function AssignClimber(Lemming1, Lemming2: TLemming): TLemming;
    function AssignFloater(Lemming1, Lemming2: TLemming): TLemming;
    function AssignBomber(Lemming1, Lemming2: TLemming): TLemming;
    function AssignBlocker(Lemming1, Lemming2: TLemming): TLemming;
    function AssignBuilder(Lemming1, Lemming2: TLemming): TLemming;
    function AssignBasher(Lemming1, Lemming2: TLemming): TLemming;
    function AssignMiner(Lemming1, Lemming2: TLemming): TLemming;
    function AssignDigger(Lemming1, Lemming2: TLemming): TLemming;
    procedure SetSoundOpts(const Value: TSoundOptions);
    procedure InternalSave(auto, includeGameResult, displayGameMessage: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  // callable
    procedure ChangeMusicVolume(up: Boolean);
    procedure Cheat;
    procedure DeveloperCreateLemmingAtCursorPoint;
    procedure Developer99Skills;
    procedure Finish;
    procedure Terminate;
    procedure HitTest;
    function ProcessSkillAssignment(checkRegainControl: Boolean): TLemming;
    procedure Prepare(const aInfo: TGameInfoRec);
    procedure RegainControl;
    procedure AutoSave; inline;
    procedure Save(includeGameResult: Boolean); inline;
    procedure SaveCurrentFrameToPng;
    procedure SaveToEngineFile(const aFileName: string);
    procedure SetGameResult;
  // buttons
    procedure BtnSlower(minimize: Boolean);
    //procedure BtnSlowerStop;
    procedure BtnFaster(maximize: Boolean);
    //procedure BtnFasterStop;
    procedure BtnStopChangingReleaseRate;
    procedure BtnClimber;
    procedure BtnUmbrella;
    procedure BtnExplode;
    procedure BtnBlocker;
    procedure BtnBuilder;
    procedure BtnBasher;
    procedure BtnMiner;
    procedure BtnDigger;
    procedure BtnPause(mode: TPauseCommandMode = TPauseCommandMode.None);
    procedure BtnPauseStop;
    procedure BtnTogglePause(mode: TPauseCommandMode = TPauseCommandMode.None);
    procedure BtnNuke;
 // mechanics
    procedure Start(aReplay: Boolean = False; startWithHyperspeed: Boolean = False);
    procedure GotoIteration(aTargetIteration: Integer);
    procedure Update; inline;
    procedure SetDebugLayerEnabled(value: Boolean);
  // properties
    property ClockFrame: Integer read fClockFrame;
    property CurrentCursor: Integer read fCurrentCursor;
    property CurrentIteration: Integer read fCurrentIteration;
    property CursorPoint: TPoint read fCursorPoint write fCursorPoint;
    property FastForward: Boolean read fFastForward write fFastForward;
    property GameOptions: TGameOptions read fGameOptions;
    property IsFinished: Boolean read fIsFinished;
    property HyperSpeed: Boolean read fHyperSpeed;
    property Toolbar: TSkillPanelToolbar read fToolbar write fToolbar;
    property IsActuallyReplaying: Boolean read GetIsActuallyReplaying;
    property IsPaused: Boolean read fIsPaused write fIsPaused;
    property IsLastRecordedRecordReached: Boolean read fIsLastRecordedRecordReached;
    property IsSpeedingUp: Boolean read GetIsSpeedingUp;
    property IsSlowingDown: Boolean read GetIsSlowingDown;
    property IsNukedByUser: Boolean read fIsNukedByUser;
    property Level: TLevel read fLevel;
    property LevelLoadingInfo: TLevelLoadingInformation read fLevelLoadingInfo;
    property Mechanics: TMechanics read fMechanics;
    property MiscOptions: TMiscOptions read fMiscOptions;
    property MiniMapBuffer: TBitmap32 read fMiniMapBuffer;
    property GlitchPauseIterations: Integer read fGlitchPauseIterations;
    property Playing: Boolean read fPlaying;
    property Recorder: TRecorder read fRecorder;
    property Renderer: TRenderer read fRenderer;
    property Replaying: Boolean read fReplaying;
    property RightMouseButtonHeldDown: Boolean read fRightMouseButtonHeldDown write fRightMouseButtonHeldDown;
    property ReleaseRateStatus: TReleaseRateStatus read fReleaseRateStatus;
    property SoundOpts: TSoundOptions read fSoundOpts write SetSoundOpts;
    property TargetIteration: Integer read fTargetIteration write fTargetIteration;
    property DebugLayerEnabled: Boolean read fDebugLayerEnabled write SetDebugLayerEnabled;
  // events
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
  end;


const
  //Recorded Action Flags
	raf_StartPause          = Bit0;
	raf_EndPause            = Bit1;
	raf_Pausing             = Bit2;
	raf_StartIncreaseRR     = Bit3;  // only allowed when not pausing
	raf_StartDecreaseRR     = Bit4;  // only allowed when not pausing
	raf_StopChangingRR      = Bit5;  // only allowed when not pausing
	raf_SkillSelection      = Bit6;
	raf_SkillAssignment     = Bit7;
	raf_Nuke                = Bit8;  // only allowed when not pausing, as in the game

  // Replay Flag
  rf_UseLemming2        = Bit0;
  rf_RightMouseGlitch   = Bit1;

  // Recorded Lemming Action
  rla_None       = 0;
  rla_Walking    = 1;  // recording not allowed
  rla_Jumping    = 2;  // recording not allowed
  rla_Digging    = 3;
  rla_Climbing   = 4;
  rla_Drowning   = 5;  // recording not allowed
  rla_Hoisting   = 6;  // recording not allowed
  rla_Building   = 7;
  rla_Bashing    = 8;
  rla_Mining     = 9;
  rla_Falling    = 10; // recording not allowed
  rla_Floating   = 11;
  rla_Splatting  = 12; // recording not allowed
  rla_Exiting    = 13; // recording not allowed
  rla_Vaporizing = 14; // recording not allowed
  rla_Blocking   = 15;
  rla_Shrugging  = 16; // recording not allowed
  rla_Ohnoing    = 17; // recording not allowed
  rla_Exploding  = 18;

  // Recorded Selected Button
  rsb_None       = 0;
  rsb_Slower     = 1;  // select not allowed
  rsb_Faster     = 2;  // select not allowed
  rsb_Climber    = 3;
  rsb_Umbrella   = 4;
  rsb_Explode    = 5;
  rsb_Stopper    = 6;
  rsb_Builder    = 7;
  rsb_Basher     = 8;
  rsb_Miner      = 9;
  rsb_Digger     = 10;
  rsb_Pause      = 11; // select not allowed
  rsb_Nuke       = 12; // select not allowed

type
  // special frame handling for floaters
  TFloatParameterRec = record
    Dy: Integer;
    AnimationFrameIndex: Integer;
  end;

const
  {-------------------------------------------------------------------------------
    So what is this: A table which describes what to do when floating.
    The floaters animation has 8 frames: 0..3 is opening the umbrella
    and 4..7 is the actual floating.
    This table "fakes" 16 frames of floating and what should happen with
    the Y-position of the lemming each frame. Frame zero is missing, because that is
    automatically frame zero.
    Additionally: after 15 go back to 8
  -------------------------------------------------------------------------------}
  FloatParametersTable: array[0..15] of TFloatParameterRec = (
    (Dy:  3; AnimationFrameIndex: 1),
    (Dy:  3; AnimationFrameIndex: 2),
    (Dy:  3; AnimationFrameIndex: 3),
    (Dy:  3; AnimationFrameIndex: 5),
    (Dy: -1; AnimationFrameIndex: 5),
    (Dy:  0; AnimationFrameIndex: 5),
    (Dy:  1; AnimationFrameIndex: 5),
    (Dy:  1; AnimationFrameIndex: 5),
    (Dy:  2; AnimationFrameIndex: 5),
    (Dy:  2; AnimationFrameIndex: 6),
    (Dy:  2; AnimationFrameIndex: 7),
    (Dy:  2; AnimationFrameIndex: 7),
    (Dy:  2; AnimationFrameIndex: 6),
    (Dy:  2; AnimationFrameIndex: 5),
    (Dy:  2; AnimationFrameIndex: 4),
    (Dy:  2; AnimationFrameIndex: 4)
  );

implementation

{$ifdef logging}
uses
  Game.Logger;
{$endif}

const
  OBJMAPOFFSET = 16;
  OBJMAPADD = OBJMAPOFFSET div 4;

const
  LEMMIX_REPLAY_VERSION    = 3;
  MAX_REPLAY_RECORDS       = 32768;
  MAX_FALLDISTANCE         = 60;

const
  // values for the (4 pixel resolution) Dos Object Map (for triggereffects)
  DOM_NONE             = 128 + 0;
  DOM_EXIT             = 128 + 1;
  DOM_FORCELEFT        = 128 + 2; // left arm of blocker
  DOM_FORCERIGHT       = 128 + 3; // right arm of blocker
  // DOM_TRAP          = 128 + 4; // not used
  DOM_WATER            = 128 + 5; // causes drowning
  DOM_FIRE             = 128 + 6; // causes vaporizing
  DOM_ONEWAYLEFT       = 128 + 7;
  DOM_ONEWAYRIGHT      = 128 + 8;
  DOM_STEEL            = 128 + 9;
  DOM_BLOCKER          = 128 + 10; // the middle part of blocker

  HEAD_MIN_Y           = -5;
  LEMMING_MIN_X        = 0;
  LEMMING_MAX_X        = 1647;
  LEMMING_MAX_Y        = 163;

  PARTICLE_FRAMECOUNT  = 52;

{$ifdef debug}
function CheckRectCopy(const A, B: TRect): Boolean;
begin
  Result := (A.Width = B.Width) and (A.Height = B.Height);
end;
{$endif}

{ TReplayFileHeaderRec }

procedure TReplayFileHeaderRec.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{ TReplayRec }

procedure TReplayRec.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{ TLemming }

function TLemming.GetRTL: Boolean; // inline
begin
  Result := xDelta < 0;
end;

function TLemming.ActionIn(aFlag: Integer): Boolean; // inline
begin
  Result := ActionBits and aFlag <> 0;
end;

function TLemming.GetCountDownDigitBounds: TRect;
begin
  Result.Left := xPos - 1;
  Result.Top := yPos + FrameTopDy - 12;
  Result.Right := Result.Left + 8;
  Result.Bottom := Result.Top + 8;
end;

function TLemming.GetFrameBounds: TRect;
begin
  Result.Left := 0;
  Result.Top := Frame * LMA.Height;
  Result.Right := LMA.Width;
  Result.Bottom := Result.Top + LMA.Height;
end;

function TLemming.GetLocationBounds: TRect;
begin
  Result.Left := xPos - LMA.FootX;
  Result.Top := yPos - LMA.FootY;
  Result.Right := Result.Left + LMA.Width;
  Result.Bottom := Result.Top + LMA.Height;
end;

{ TInteractiveObjectInfo }

function TInteractiveObjectInfo.GetBounds: TRect;
begin
  Result.Left := Obj.Left;
  Result.Top := Obj.Top;
  Result.Right := Result.Left + MetaObj.Height;
  Result.Bottom := Result.Top + MetaObj.Width;
end;

function TInteractiveObjectInfo.OnlyOnTerrain: Boolean;
begin
  Result := odf_OnlyOnTerrain and Obj.DrawingFlags <> 0;
end;

{ TLowResolutionMessage }

constructor TLowResolutionMessage.Create(aGame: TLemmingGame);
begin
  inherited Create;
  fGame := aGame;
  fDuration := 32;
  fDeltaY := -2;
  fBuffer := TBitmap32.Create;
  fBuffer.SetSize(1,1);
  fBuffer.Font.Height := 12;
  fBuffer.Font.Name := 'Arial';
  fBuffer.Font.Quality := TFontQuality.fqNonAntialiased;
  fBuffer.DrawMode := dmTransparent;
end;

destructor TLowResolutionMessage.Destroy;
// note we do not have to destroy the layers, because the image component of the gamescreen takes care of that
begin
  fBuffer.Free;
  inherited;
end;

procedure TLowResolutionMessage.NextFrame;
begin
  Inc(fCurrentFrame);
  Inc(fLocation.Y, fDeltaY);
  Inc(fLocation.X, fDeltaX);
  if (fLocation.Y + fBuffer.Height <= 0)
  or (fLocation.X >= GAME_BMPWIDTH)
  or (fLocation.X + fBuffer.Width <= 0)
  or (fCurrentFrame > fDuration) then
    fEnded := True;
end;

procedure TLowResolutionMessage.SetText(const Value: string; aColor: TColor32);
var
  size: TSize;
begin
  fText := Value;
  fBuffer.Font.Color := WinColor(aColor);
  size := fBuffer.TextExtent(Ftext);
  fbuffer.SetSize(size.cx, size.cy);
  fbuffer.Clear(0);
  fbuffer.Textout(0, 0, fText);
  fbuffer.DrawMode := dmTransparent;
//  fbuffer.MasterAlpha := 128;
end;

{ THighResolutionMessage }

constructor THighResolutionMessage.Create(aLayer: THighResolutionLayer; x, y, deltaX, deltaY, duration: Integer; const aText: string; aColor: TColor32);
var
  size: TSize;
begin
  fLayer := aLayer;
  fXPos := x;
  fYPos := y;
  fDeltaX := deltaX;
  fDeltaY := deltaY;
  fDuration := duration;
  fBuffer := TBitmap32.Create;
  fBuffer.SetSize(1, 1);
  fBuffer.Font := fLayer.Font;
//  fBuffer.Font.Name := 'Segoe UI';
  fBuffer.Font.Color := WinColor(aColor);
  fBuffer.Font.Height := Scale(31);
  size := fBuffer.TextExtent(aText);
  fBuffer.SetSize(size.cx, size.cy);
  fBuffer.Clear(0);
  fBuffer.Textout(0, 0, aText);
  fBuffer.ReplaceAllNonZeroColors(aColor);
  fBuffer.DrawMode := dmBlend;
  fAlpha := 255;
  fAlphaDelta := Round(255/duration);
end;

destructor THighResolutionMessage.Destroy;
begin
  fBuffer.Free;
  inherited;
end;

procedure THighResolutionMessage.NextFrame;
begin
  if fEnded then
    Exit;
  Inc(fCurrentFrame);
  Inc(fXPos, fDeltaX);
  Inc(fYPos, fDeltaY);
  if fAlpha > 0 then begin
    Dec(fAlpha, fAlphaDelta);
    if fAlpha < 0 then
      fAlpha := 0;
    fBuffer.MasterAlpha := fAlpha;
  end;

  if fCurrentFrame > fDuration then
    fEnded := True;
end;

procedure THighResolutionMessage.Paint(buffer: TBitmap32);
begin
  if fEnded then
    Exit;
  fBuffer.DrawTo(buffer, fXpos, fYpos);
end;

function THighResolutionMessage.GetRect: TRect;
begin
  Result := Rect(fXpos, fYpos, fXpos + fBuffer.Width, fYpos + fBuffer.Height);
end;


{ THighResolutionLayer }

constructor THighResolutionLayer.Create(aLayerCollection: TLayerCollection);
begin
  inherited Create(aLayerCollection);
  fMessageList := TFastObjectList<THighResolutionMessage>.Create;
  fFont := TFont.Create;
  fFont.Name := 'Segoe UI';
end;

destructor THighResolutionLayer.Destroy;
begin
  fMessageList.Free;
  fFont.Free;
  inherited Destroy;
end;

procedure THighResolutionLayer.AddMessage(x, y, deltaX, deltaY, duration: Integer; const aText: string; aColor: TColor32);
begin
  var msg: THighResolutionMessage := THighResolutionMessage.Create(Self, x, y, deltaX, deltaY, duration, aText, aColor);
  fMessageList.Add(msg);
end;

procedure THighResolutionLayer.UpdateMessages;
var
  msg: THighResolutionMessage;
begin
  if fMessageList.IsEmpty then
    Exit;

  // free the messages that have ended
  for var i := fMessageList.Count - 1 downto 0 do begin
    msg := MessageList[i];
    if Msg.fEnded then
      fMessageList.Delete(i);
  end;

  for msg in fMessageList do
    msg.NextFrame;

  if Visible and fMessageList.IsEmpty then
    Visible := False
  else if not Visible and not fMessageList.IsEmpty then
    Visible := True;
end;

{ TDebugLayer }

constructor TDebugLayer.Create(aLayerCollection: TLayerCollection);
begin
  inherited Create(aLayerCollection);
  Scaled := False;
end;

{ TReplayCursor }

constructor TReplayCursor.Create(aGame: TLemmingGame);
begin
  inherited Create;
  fGame := aGame;
  fBitmap := TBitmap32.Create;
  var bmp: TBitmap := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHighlight);
  fBitmap.Assign(bmp);
  bmp.Free;
  fBitmap.ReplaceColor(SetAlpha(clBlack32, 255), 0);
  fBitmap.DrawMode := dmBlend;
end;

destructor TReplayCursor.Destroy;
begin
  fBitmap.Free;
  inherited;
end;

procedure TReplayCursor.Reset;
begin
  fPosition := Point(0, 0);
  fCursorPos := Point(0, 0);
  fDrawRect := TRect.Empty;
  fPreviousDrawRect := fDrawRect;
  fVisible := False;
  fErasable := False;
  fFrames := 0;
  fAlpha := 0;
end;

procedure TReplayCursor.Activate(const aCursorPos: TPoint);
begin
  // if reset we still need to erase the previous location
  if fErasable then
    fPreviousDrawRect := fDrawRect
  else
    fPreviousDrawRect := TRect.Empty;

  // convert the normal hotspot to the hotspot the game uses (4,9 instead of 7,7), see gamescreen
  fCursorPos := aCursorPos;
  fPosition := Point(fCursorPos.X - 7 + 3, fCursorPos.Y - 7 - 2);
  fDrawRect.Left := fPosition.X;
  fDrawRect.Top := fPosition.Y;
  fDrawRect.Right := fDrawRect.Left + fBitmap.Width;
  fDrawRect.Bottom := fDrawRect.Top + fBitmap.Height;
  fAlpha := 132;
  fBitmap.MasterAlpha := fAlpha;//ReplaceAlphaForAllNonZeroColors(fAlpha);
  fFrames := 10;
  fVisible := True;
  fErasable := True;
end;

procedure TReplayCursor.Decrement;
begin
  // only call if fVisible
  if fFrames >= 0 then begin
    Dec(fFrames);
    fVisible := fFrames >= 0;
    fErasable := fFrames >= -1;
    Dec(fAlpha, 12);
    fBitmap.MasterAlpha := fAlpha;
  end;
end;

{ TLemmingGame }

constructor TLemmingGame.Create;
begin
  inherited Create;

  {$ifdef paranoid}
  Assert(SizeOf(TReplayFileHeaderRec) = 64);
  Assert(SizeOf(TMechanics) = 2); // mechanics set has to fit in correctly inside a replay record
  {$endif}

  fLemmingList   := TLemmingList.Create;
  fWorld         := TBitmap32.Create;
  ObjectInfos    := TInteractiveObjectInfoList.Create;
  Entrances      := TInteractiveObjectInfoList.Create;
  fObjectMap     := TByteMap.Create;
  fMiniMap       := TBitmap32.Create;
  fMinimapBuffer := TBitmap32.Create;
  fRecorder      := TRecorder.Create(Self);
  MessageList    := TGameMessageList.Create;
  ReplayCursor   := TReplayCursor.Create(Self);
  fSoundsToPlay  := TList<Integer>.Create;

  LemmingMethods[TLemmingAction.None]       := nil;
  LemmingMethods[TLemmingAction.Walking]    := HandleWalking;
  LemmingMethods[TLemmingAction.Jumping]    := HandleJumping;
  LemmingMethods[TLemmingAction.Digging]    := HandleDigging;
  LemmingMethods[TLemmingAction.Climbing]   := HandleClimbing;
  LemmingMethods[TLemmingAction.Drowning]   := HandleDrowning;
  LemmingMethods[TLemmingAction.Hoisting]   := HandleHoisting;
  LemmingMethods[TLemmingAction.Building]   := HandleBuilding;
  LemmingMethods[TLemmingAction.Bashing]    := HandleBashing;
  LemmingMethods[TLemmingAction.Mining]     := HandleMining;
  LemmingMethods[TLemmingAction.Falling]    := HandleFalling;
  LemmingMethods[TLemmingAction.Floating]   := HandleFloating;
  LemmingMethods[TLemmingAction.Splatting]  := HandleSplatting;
  LemmingMethods[TLemmingAction.Exiting]    := HandleExiting;
  LemmingMethods[TLemmingAction.Vaporizing] := HandleVaporizing;
  LemmingMethods[TLemmingAction.Blocking]   := HandleBlocking;
  LemmingMethods[TLemmingAction.Shrugging]  := HandleShrugging;
  LemmingMethods[TLemmingAction.Ohnoing]    := HandleOhNoing;
  LemmingMethods[TLemmingAction.Exploding]  := HandleExploding;

  SkillMethods[TLemmingAction.None]         := nil;
  SkillMethods[TLemmingAction.Walking]      := nil;
  SkillMethods[TLemmingAction.Jumping]      := nil;
  SkillMethods[TLemmingAction.Digging]      := AssignDigger;
  SkillMethods[TLemmingAction.Climbing]     := AssignClimber;
  SkillMethods[TLemmingAction.Drowning]     := nil;
  SkillMethods[TLemmingAction.Hoisting]     := nil;
  SkillMethods[TLemmingAction.Building]     := AssignBuilder;
  SkillMethods[TLemmingAction.Bashing]      := AssignBasher;
  SkillMethods[TLemmingAction.Mining]       := AssignMiner;
  SkillMethods[TLemmingAction.Falling]      := nil;
  SkillMethods[TLemmingAction.Floating]     := AssignFloater;
  SkillMethods[TLemmingAction.Splatting]    := nil;
  SkillMethods[TLemmingAction.Exiting]      := nil;
  SkillMethods[TLemmingAction.Vaporizing]   := nil;
  SkillMethods[TLemmingAction.Blocking]     := AssignBlocker;
  SkillMethods[TLemmingAction.Shrugging]    := nil;
  SkillMethods[TLemmingAction.Ohnoing]      := nil;
  SkillMethods[TLemmingAction.Exploding]    := AssignBomber;

end;

destructor TLemmingGame.Destroy;
begin
  fLemmingList.Free;
  ObjectInfos.Free;
  fWorld.Free;
  Entrances.Free;
  fObjectMap.Free;
  fMiniMap.Free;
  fMinimapBuffer.Free;
  fRecorder.Free;
  MessageList.Free;
  ReplayCursor.Free;
  fSoundsToPlay.Free;
  inherited Destroy;
end;

function TLemmingGame.SelectReplayMessageTextColor: TColor32; // inline
begin
  Result := fMessageColors[fReplayMessageCounter]; // inline
end;

function TLemmingGame.SelectFeedbackMessageTextColor: TColor32; // inline
begin
  Result := fMessageColors[fCurrentIteration mod 8];
end;

function TLemmingGame.GetIsActuallyReplaying: Boolean; // inline
begin
  Result := fReplaying and not fIsLastRecordedRecordReached and not fRecorder.IsEmpty;
end;

function TLemmingGame.GetIsSpeedingUp: Boolean; // inline
begin
  Result := fReleaseRateStatus = TReleaseRateStatus.SpeedingUp;
end;

function TLemmingGame.GetIsSlowingDown: Boolean; // inline
begin
  Result := fReleaseRateStatus = TReleaseRateStatus.SlowingDown;
end;

procedure TLemmingGame.Update; // inline
begin
  InternalUpdate(false, false);
end;

procedure TLemmingGame.DrawDebug; // inline
begin
  if fDebugLayerEnabled and not HyperSpeed then
    DebugLayer.Update;
end;

procedure TLemmingGame.UpdatePixelCombine(L: TLemming);
begin
  if not (TGameOption.ColorizeLemmings in GameOptions) then begin
    L.PixelCombine := CombineLemming;
    Exit;
  end;

  case L.CombineFlags of
    0:
      L.PixelCombine := CombineLemming;
    COMBINE_FLAG_CLIMBER:
      L.PixelCombine := CombineClimber;
    COMBINE_FLAG_CLIMBER or COMBINE_FLAG_FLOATER:
      L.PixelCombine := CombineAthlete;
    COMBINE_FLAG_CLIMBER or COMBINE_FLAG_BUILDER:
      L.PixelCombine := CombineBuilderClimber;
    COMBINE_FLAG_CLIMBER or COMBINE_FLAG_FLOATER or COMBINE_FLAG_BUILDER:
      L.PixelCombine := CombineBuilderAthlete;
    COMBINE_FLAG_FLOATER:
      L.PixelCombine := CombineFloater;
    COMBINE_FLAG_FLOATER or COMBINE_FLAG_BUILDER:
      L.PixelCombine := CombineBuilderFloater;
    COMBINE_FLAG_BUILDER:
      L.PixelCombine := CombineBuilder;
  end;
end;

procedure TLemmingGame.BtnInternalSelectOnly(btn: TSkillPanelButton);
begin
  case btn of
    TSkillPanelButton.Climber  : BtnClimber;
    TSkillPanelButton.Umbrella : BtnUmbrella;
    TSkillPanelButton.Explode  : BtnExplode;
    TSkillPanelButton.Blocker  : BtnBlocker;
    TSkillPanelButton.Builder  : BtnBuilder;
    TSkillPanelButton.Basher   : BtnBasher;
    TSkillPanelButton.Miner    : BtnMiner;
    TSkillPanelButton.Digger   : BtnDigger;
  end;
end;

procedure TLemmingGame.CreateInfoLayer;
begin
  HighResolutionLayer := THighResolutionLayer.Create(fImg.Layers);
  HighResolutionLayer.MouseEvents := False;
  HighResolutionLayer.Visible := True;
  HighResolutionLayer.Scaled := False;
  HighResolutionLayer.OnPaint := InfoLayer_Paint;
end;

procedure TLemmingGame.InfoLayer_Paint(Sender: TObject; buffer: TBitmap32);
begin
  if HyperSpeed or not Playing or IsFinished or HighResolutionLayer.MessageList.IsEmpty then
    Exit;

  if buffer.MeasuringMode then begin
    for var msg: THighResolutionMessage in HighResolutionLayer.MessageList do
      buffer.Changed(msg.GetRect);
    Exit;
  end;

  for var msg: THighResolutionMessage in HighResolutionLayer.MessageList do
    msg.Paint(buffer);
end;

procedure TLemmingGame.CreateDebugLayer;
begin
  DebugLayer := TDebugLayer.Create(fImg.Layers);
  DebugLayer.MouseEvents := False;
  DebugLayer.Visible := False;
  DebugLayer.Scaled := False;
  DebugLayer.OnPaint := DebugLayer_Paint;
end;

procedure TLemmingGame.DebugLayer_Paint(Sender: TObject; buffer: TBitmap32);
var
  r: TRect;
  y, dist: Integer;

    procedure Txt(var y: Integer; const s: string);
    begin
      buffer.Textout(0, y, s);
      Inc(y, dist);
    end;

const
  line_count = 13;
begin
  if not fDebugLayerEnabled or HyperSpeed or not Playing or IsFinished then
    Exit;

  dist := Scale(19);
  buffer.Font.Name := 'courier new';
  buffer.Font.Height := Scale(19);

  if DebugLayer.CachedMaxTextWidth = 0 then
    DebugLayer.CachedMaxTextWidth := buffer.TextWidth('rec glitchcount: 99999999');

  if buffer.MeasuringMode then begin
    buffer.Changed(Rect(0, 0,  DebugLayer.CachedMaxTextWidth, dist * line_count));
    Exit;
  end;

  r := Rect(0, 0, DebugLayer.CachedMaxTextWidth, dist * line_count);
  buffer.Font.Color := clWhite;
  buffer.FillRectTS(r, SetAlpha(clBlue32, 160{200}{80}));

  y := 0;
  Txt(y, '   session: ' + fUpdateCallsSession.ToThousandString);
  Txt(y, '    update: ' + fUpdateCalls.ToThousandString);
  Txt(y, '     frame: ' + CurrentIteration.ToString);
  Txt(y, '     pause: ' + YesNo(fIsPaused));
  Txt(y, '     clock: ' + ClockFrame.ToString);
  Txt(y, '        RR: ' + CurrReleaseRate.ToString);
  Txt(y, '    cursor: ' + CursorPoint.X.ToString + ':' + CursorPoint.Y.ToString);
  Txt(y, '    replay: ' + YesNo(fReplaying));
  Txt(y, 'act replay: ' + YesNo(IsActuallyReplaying));
  Txt(y, ' ix replay: ' + fReplayIndex.ToString);
  Txt(y, 'max replay: ' + Pred(fRecorder.List.Count).ToString);
  Txt(y, '    glitch: ' + fGlitchPauseIterations.ToString);
  Txt(y, 'rec glitch: ' + fRecorder.fRecordedGlitchPauseIterations.ToString);

  {--------------------------------------------------------------------------------
  // test frames per second which is with normal play about 16.9 fps,
  // which matches the 17 frames = one game-second

  if fCurrentIteration > 0 then begin
    var seconds: Double := MSBetweenD(fGameStartTime, QueryTimer) / 1000;
    var fps: Double;
    if seconds > 0 then
      fps := fCurrentIteration / seconds
    else
      fps := 0;

    Txt(y, '   avg fps: ' + SimpleRoundTo(fps, -2).ToString);
  end;
  --------------------------------------------------------------------------------}
end;

procedure TLemmingGame.SetDebugLayerEnabled(value: Boolean);
begin
  if (value = fDebugLayerEnabled) and (DebugLayer.Visible = value) then
    Exit;
  fDebugLayerEnabled := value;
  DebugLayer.Visible := value;
end;

procedure TLemmingGame.Prepare(const aInfo: TGameInfoRec);
// #EL 2009-04-02 added options (needed for replay).
// #EL 2020-02-23 decoupled game from global app: all info is passed to this method.
var
  Ani: TLemmingAnimationSet;
  Bmp: TBitmap32;
begin
  fUpdateCallsSession := 0;

  // copy info param
  fMechanics := aInfo.Style.Mechanics;
  fSoundOpts := aInfo.SoundOptions;
  fGameOptions := aInfo.GameOptions;
  fMiscOptions := aInfo.MiscOptions;
  fRenderer := aInfo.Renderer;
  fImg := aInfo.Img;
  fTargetBitmap := aInfo.TargetBitmap;
  fDisplayScale := aInfo.DisplayScale;
  fSoundMgr := aInfo.SoundMgr;
  fLevel := aInfo.Level;
  fStyle := aInfo.Style;
  fGraph := aInfo.GraphicSet;
  fLevelLoadingInfo := aInfo.LevelLoadingInfo;

//  dlg(fimg.Layers.Count.ToString);

  fLastNonPrioritizedLemming := nil;

  CreateInfoLayer;
  CreateDebugLayer;
  DebugLayerEnabled := aInfo.DebugLayerEnabled;

  if TOptionalMechanic.NukeGlitch in aInfo.OptionalMechanics then
    Include(fMechanics, TMechanic.NukeGlitch);

  if TOptionalMechanic.PauseGlitch in aInfo.OptionalMechanics then
    Include(fMechanics, TMechanic.PauseGlitch);

  if TOptionalMechanic.RighClickGlitch in aInfo.OptionalMechanics then
    Include(fMechanics, TMechanic.RightClickGlitch);

  fRecorder.fCurrentMechanics := fMechanics;

  fMessagesPlayedCount := 0;
  fStartupMusicAfterEntrance := True;

  {--------------------------------------------------------------------------------------
    Initialize the palette of AnimationSet.
    Low part is the fixed palette
    Hi part comes from the graphicset.
    After that let the AnimationSet read the animations.
    #EL 2020-02-23: reloading the lemminganimations is needed because of the brickcolor.
                    The graphicset assembles the palette and is already loaded.
  ---------------------------------------------------------------------------------------}
  Ani := fStyle.LemmingAnimationSet;
  Ani.AnimationPalette := Copy(fGraph.Palette);
  Ani.Load;

  // initialize explosion particle colors
  for var i := 0 to 15 do
    fParticleColors[i] := fGraph.Palette[ParticleColorIndices[i]];

  // prepare masks for drawing
  CntDownBmp := Ani.CountDownDigitsBitmap;
  CntDownBmp.DrawMode := dmCustom;
  CntDownBmp.OnPixelCombine := CombineDefault;

  ExplodeMaskBmp := Ani.ExplosionMaskBitmap;
  ExplodeMaskBmp.DrawMode := dmCustom;
  ExplodeMaskBmp.OnPixelCombine := CombineMask;

  BashMasks[False] := Ani.BashMasksBitmap;
  BashMasks[False].DrawMode := dmCustom;
  BashMasks[False].OnPixelCombine := CombineMask;

  BashMasks[True] := Ani.BashMasksRTLBitmap;
  BashMasks[True].DrawMode := dmCustom;
  BashMasks[True].OnPixelCombine := CombineMask;

  MineMasks[False] := Ani.MineMasksBitmap;
  MineMasks[False].DrawMode := dmCustom;
  MineMasks[False].OnPixelCombine := CombineMask;

  MineMasks[True] := Ani.MineMasksRTLBitmap;
  MineMasks[True].DrawMode := dmCustom;
  MineMasks[True].OnPixelCombine := CombineMask;

  // prepare animationbitmaps for drawing (set pixelcombine eventhandlers)
  for Bmp in Ani.LemmingBitmaps do begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineLemming;
  end;

  fWorld.SetSize(GAME_BMPWIDTH, GAME_BMPHEIGHT);

  // load particle array
  var stream: TStream := TData.CreateDataStream(fStyle.Name, Consts.FilenameParticles, TDataType.Particles);
  try
    stream.Read(fParticles, stream.Size);
  finally
    stream.Free;
  end;

  if not fLevelLoadingInfo.MusicFileName.IsEmpty then begin
    if not MiscOptions.ShuffledMusic then
      MUSIC_INDEX := fSoundMgr.AddMusicFromFileName(fLevelLoadingInfo.MusicFileName, fLevelLoadingInfo.MusicStreamType)
    else begin
      var info := fStyle.LevelSystem.SelectRandomLevel;
      if Assigned(info) and not info.MusicFileName.IsEmpty then
        MUSIC_INDEX := fSoundMgr.AddMusicFromFileName(info.MusicFileName, info.MusicStreamType)
    end;
  end
  else
    MUSIC_INDEX := -1;
end;

procedure TLemmingGame.Start(aReplay: Boolean = False; startWithHyperspeed: Boolean = False);
var
  O: TInteractiveObject;
  MO: TMetaObject;
  Inf: TInteractiveObjectInfo;

    procedure SetOrder(i0, i1, i2, i3: Byte);
    begin
      DosEntranceOrderTable[0] := i0;
      DosEntranceOrderTable[1] := i1;
      DosEntranceOrderTable[2] := i2;
      DosEntranceOrderTable[3] := i3;
    end;

begin
  {$ifdef logging} TGameLogger.Log('start'); {$endif} // do not localize
  {$ifdef paranoid} Assert(fToolbar <> nil); {$endif}

  fPlaying := False;

  HighResolutionLayer.MessageList.Clear;
  MessageList.Clear;
  ReplayCursor.Reset;
  // fSoundMgr.StopSounds;

  // flicker free hyperspeed rewind
  if startwithHyperspeed then begin
    fTargetBitmapsUpdatingSet := True;
    fTargetBitmap.BeginUpdate;
    fToolbar.BeginUpdateImg;
  end
  else
    fTargetBitmapsUpdatingSet := False;

  fRenderer.RenderWorld(fWorld, False);

  fTargetBitmap.Assign(fWorld);

  // hyperspeed things
  fTargetIteration := 0;
  fHyperSpeed := startwithHyperspeed;
  fEntranceAnimationCompleted := False;

  fUpdateCalls := 0;
  fHandleLemmingCalls := 0;

  fFastForward := False;
  fIsLastRecordedRecordReached := False;

  fIsFinished := False;
  fIsCheated := False;
  LemmingsReleased := 0;
  fWorld.Assign(fTargetBitmap);
  fWorld.OuterColor := 0;
  Minutes := Level.Info.TimeLimit;
  Seconds := 0;

  FillChar(GameResultRec, SizeOf(GameResultRec), 0);
  GameResultRec.LemmingCount  := Level.Info.LemmingsCount;
  GameResultRec.ToRescue := Level.Info.RescueCount;

  fReplayIndex := 0;
  LemmingsReleased := 0;
  LemmingsOut := 0;
  LemmingsSaved := 0;
  LemmingsRemoved := 0;
  fRightMouseButtonHeldDown := False;
  fCurrentIteration := 0;

  fLastCueSoundIteration := 0;
  fClockFrame := 0;
  EntrancesOpened := False;
  ObjectInfos.Clear;
  Entrances.Clear;
  SetOrder(0, 0, 0, 0);
  fReleaseRateStatus := TReleaseRateStatus.None;
  fIsPaused := False; //startPaused; todo: bug in progress
  fIsPausedExt := False;
  fIsNukedByUser := False;
  fIsExploderAssignInProgress := False;
  fIndexOfLemmingToBeNuked := 0;
  fCurrentCursor := 0;
  fParticleFinishTimer := 0;
  fLemmingList.Clear;
  fSoundsToPlay.Clear;
  if not aReplay then
    fRecorder.Clear;
  fReplaying := aReplay;
  fExplodingPixelsUpdateNeeded := False;
  fLastNonPrioritizedLemming := nil;
  fReplayMessageCounter := 0;

  // replay pause glitch aware stuff
  fGlitchPauseIterations := 0;

  // if replaying then overwrite mechanics from the recorder
  if aReplay and (fRecorder.WasLoaded or fRecorder.WasSaved or not fRecorder.List.IsEmpty) then
    fMechanics := fRecorder.fCurrentMechanics;

  // if replaying then reset glitchpauseiterations
  if aReplay and (TMechanic.PauseGlitch in fMechanics) then
    fGlitchPauseIterations := fRecorder.fRecordedGlitchPauseIterations;

  MaxNumLemmings := Level.Info.LemmingsCount;

  currReleaseRate    := Level.Info.ReleaseRate;
  currClimberCount   := Level.Info.ClimberCount;
  currFloaterCount   := Level.Info.FloaterCount;
  currBomberCount    := Level.Info.BomberCount;
  currBlockerCount   := Level.Info.BlockerCount;
  currBuilderCount   := Level.Info.BuilderCount;
  currBasherCount    := Level.Info.BasherCount;
  currMinerCount     := Level.Info.MinerCount;
  currDiggerCount    := Level.Info.DiggerCount;

  NextLemmingCountDown := 20;

  for O in Level.InteractiveObjects do begin
    if not fGraph.MetaObjectList.ValidIndex(O.Identifier) then
      Throw('Close encounter with an invalid object identifier (' + O.Identifier.ToString  + ')', 'Start');
    MO := fGraph.MetaObjectList[O.Identifier];
    Inf := TInteractiveObjectInfo.Create;
    Inf.Obj := O;
    Inf.MetaObj := MO;
    Inf.CurrentFrame := MO.StartAnimationFrameIndex;
    // add to the right list (Entrances or other objects)
    if O.Identifier = DOS_OBJECT_ID_ENTRANCE then
      Entrances.Add(Inf)
    else
      ObjectInfos.Add(Inf);
  end;

  // release order index table
  if TMechanic.OldEntranceABBAOrder in Mechanics then begin
    case Entrances.Count of
      2: SetOrder(0, 1, 1, 0); // ABBA
      3: SetOrder(0, 1, 2, 1); // ABCB
      4: SetOrder(0, 1, 2, 3); // ABCD
    else
      SetOrder(0, 0, 0, 0); // AAAA
    end; // case
  end
  else begin
    case Entrances.Count of
      2: SetOrder(0, 1, 0, 1); // ABAB
      3: SetOrder(0, 1, 2, 1); // ABCB  #EL 2009-07-02 BUG solved: changed last index from 0 to 1. #EL 2020: that is why maybe some replay files fail
      4: SetOrder(0, 1, 2, 3); // ABCD
    else
      SetOrder(0, 0, 0, 0); // AAAA
    end
  end;

  InitializeBrickColors(fGraph.BrickColor);
  InitializeObjectMap;
  InitializeMiniMap;

  if not startWithHyperspeed then
    DrawAnimatedObjects; // first draw needed

  fToolbar.SetInfoMinutes(Minutes);
  fToolbar.SetInfoSeconds(Seconds);
  fToolbar.SetInfoLemmingsOut(LemmingsOut);
  fToolbar.SetInfoLemmingsSaved(0, 1, not MiscOptions.LemmingsPercentages);
  if not fHyperSpeed then
    fToolBar.SetPauseHighlight(False);

  fSelectedSkill := TSkillPanelButton.None; // to force update
  BtnClimber; // this is not recorded because not playing yet

  if not fHyperSpeed then
    RepaintSkillPanelButtons;

  if not aReplay and SaveToEngineFileAtStartup then begin
    var f: string := Consts.PathToBin + GenFileNameWithoutExtension(fLevelLoadingInfo) + '.bin';
    if ForceDir(Consts.PathToBin) then
      SaveToEngineFile(f);
  end;

  if Assigned(DebugLayer) then
    DebugLayerEnabled := fDebugLayerEnabled;

  // fGameStartTime := QueryTimer;
  fPlaying := True;
end;

procedure TLemmingGame.InternalRefresh;
// only called from gotoiteration (when rewinding to frame zero)
begin
  EraseLemmings;
  EraseReplayCursor;
  EraseMessages;

  fTargetBitmap.Assign(fWorld);
  if fTargetBitmapsUpdatingSet then begin
    Assert((fTargetBitmap.GetUpdateCount > 0) and (fToolbar.GetUpdateCount = fTargetBitmap.GetUpdateCount));
    fTargetBitmapsUpdatingSet := False;
    fTargetBitmap.EndUpdate;
    fTargetBitmap.Changed;
    fToolbar.EndUpdateImg; // calls changed internally
  end;

  DrawAnimatedObjects;
  DrawLemmings;
  DrawReplayCursorCheck;
  DrawMessages;
  DrawDebug;

  // force update if raw explosion pixels drawn
  if fExplodingPixelsUpdateNeeded then begin
    fTargetBitmap.Changed;
    fExplodingPixelsUpdateNeeded := False;
  end;
end;

procedure TLemmingGame.GotoIteration(aTargetIteration: Integer);
// todo: when rewinding to frame #0 repaint problem
var
  oldPause: Boolean;
begin
  {$ifdef paranoid} Assert(not fHyperSpeed); {$endif}
  if not Playing then
    Exit;
  if aTargetIteration <= 0 then
    aTargetIteration := 0;
  if aTargetIteration = CurrentIteration then
    Exit;

  oldPause := fIsPaused;

  // rewind
  if aTargetIteration < CurrentIteration then begin
    if aTargetIteration = 0 then begin
      Start(True, False);
      InternalRefresh;
    end
    else begin
      Start(True, True);
      while (CurrentIteration < aTargetIteration) and not fIsFinished do
        InternalUpdate(True, CurrentIteration = aTargetIteration - 1);
      HyperSpeedEnd;
    end;
  end
  // skip forward
  else begin
    HyperSpeedBegin;
    while (CurrentIteration < aTargetIteration) and not fIsFinished do
      InternalUpdate(True, CurrentIteration = aTargetIteration - 1);
    HyperSpeedEnd;
  end;

  if fIsPaused <> oldPause then begin
    fIsPaused := oldPause;
    Toolbar.SetPauseHighlight(fIsPaused);
  end;
end;

class procedure TLemmingGame.CombineDefault(F: TColor32; var B: TColor32; M: TColor32);
// normal transparent
begin
  if F <> 0 then B := F;
end;

class procedure TLemmingGame.CombineLemming(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then B := F;
end;

class procedure TLemmingGame.CombineClimber(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F.B = 224 then
    B := ClimberColor
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineFloater(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F.B = 224 then
    B := FloaterColor
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineAthlete(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F.B = 224 then
    B := AthleteColor
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineBuilder(F: TColor32; var B: TColor32; M: TColor32);
// This trusts the CurrentlyDrawnLemming class var
begin
  if F = BrickPixelColor then
    B := BrickPixelColors[12 - CurrentlyDrawnLemming.NumberOfBricksLeft]
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineBuilderClimber(F: TColor32; var B: TColor32; M: TColor32);
// This trusts the CurrentlyDrawnLemming class var
begin
  if F = BrickPixelColor then
    B := BrickPixelColors[12 - CurrentlyDrawnLemming.NumberOfBricksLeft]
  else if F.B = 224 then
    B := ClimberColor
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineBuilderFloater(F: TColor32; var B: TColor32; M: TColor32);
// This trusts the CurrentlyDrawnLemming class var
begin
  if F = BrickPixelColor then
    B := BrickPixelColors[12 - CurrentlyDrawnLemming.NumberOfBricksLeft]
  else if F.B = 224 then
    B := FloaterColor
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineBuilderAthlete(F: TColor32; var B: TColor32; M: TColor32);
// This trusts the CurrentlyDrawnLemming class var
begin
  if F = BrickPixelColor then
    B := BrickPixelColors[12 - CurrentlyDrawnLemming.NumberOfBricksLeft]
  else if F.B = 224 then
    B := AthleteColor
  else if F <> 0 then
    B := F;
end;

class procedure TLemmingGame.CombineLemmingPhotoFlash(F: TColor32; var B: TColor32; M: TColor32);
// photoflash
begin
  if F <> 0 then B := clBlack32 else BlendMem(clTrWhite32, B);
end;

class procedure TLemmingGame.CombineMask(F: TColor32; var B: TColor32; M: TColor32);
// copy masks to fWorld
begin
  if F <> 0 then B := 0;
end;

class procedure TLemmingGame.CombineMinimapWorld(F: TColor32; var B: TColor32; M: TColor32);
// copy fWorld to fMiniMap
begin
  if F <> 0 then B := BrickPixelColor;
end;

function TLemmingGame.HasPixelAt(X, Y: Integer): Boolean;
//  Read value from fWorld. The function returns True when the value at (x, y) is terrain
begin
  Result := (X >= 0) and (Y >= 0) and (X < fWorld.Width) and (Y < fWorld.Height) and (fWorld.Pixel[X, Y] and ALPHA_TERRAIN <> 0);
end;

function TLemmingGame.HasPixelAt_ClipY(X, Y, minY: Integer): Boolean;
begin
  if Y >= minY then
    Result := HasPixelAt(X, Y)
  else
    Result := HasPixelAt(X, minY);
end;

procedure TLemmingGame.RemovePixelAt(X, Y: Integer);
begin
  fWorld.PixelS[x, y] := 0;
end;

function TLemmingGame.ReadObjectMap(X, Y: Integer): Byte;
// original dos fObjectMap has a resolution of 4
begin
  // the "and not 3" ensures rounding down when operand is negative (eg. -0.25 -> -1)
  X := (X and not 3) div 4;
  Y := (Y and not 3) div 4;

  Inc(X, OBJMAPADD);
  Inc(Y, OBJMAPADD);

  if (X >= 0) and (X < fObjectMap.Width) and (Y >= 0) and (Y < fObjectMap.Height) then
    Result := fObjectMap.Bits^[X + Y * fObjectMap.Width]
  else
    Result := DOM_NONE; // whoops, important
end;

procedure TLemmingGame.WriteObjectMap(X, Y: Integer; aValue: Byte);
// original dos fObjectMap has a resolution of 4
begin
  // the "and not 3" ensures rounding down when operand is negative (eg. -0.25 -> -1)
  X := (X and not 3) div 4;
  Y := (Y and not 3) div 4;

  Inc(X, OBJMAPADD);
  Inc(Y, OBJMAPADD);

  if (X >= 0) and (X < fObjectMap.Width) and (Y >= 0) and (Y < fObjectMap.Height) then
    fObjectMap.Bits^[X + Y * fObjectMap.Width] := aValue;
end;

procedure TLemmingGame.SaveMap(L: TLemming);
begin
  L.SavedMap[0] := ReadObjectMap(L.XPos - 4, L.YPos - 6);
  L.SavedMap[1] := ReadObjectMap(L.XPos,     L.YPos - 6);
  L.SavedMap[2] := ReadObjectMap(L.XPos + 4, L.YPos - 6);
  L.SavedMap[3] := ReadObjectMap(L.XPos - 4, L.YPos - 2);
  L.SavedMap[4] := ReadObjectMap(L.XPos,     L.YPos - 2);
  L.SavedMap[5] := ReadObjectMap(L.XPos + 4, L.YPos - 2);
  L.SavedMap[6] := ReadObjectMap(L.XPos - 4, L.YPos + 2);
  L.SavedMap[7] := ReadObjectMap(L.XPos,     L.YPos + 2);
  L.SavedMap[8] := ReadObjectMap(L.XPos + 4, L.YPos + 2);
end;

procedure TLemmingGame.RestoreMap(L: TLemming);
begin
  WriteObjectMap(L.XPos - 4, L.YPos - 6, L.SavedMap[0]);
  WriteObjectMap(L.XPos,     L.YPos - 6, L.SavedMap[1]);
  WriteObjectMap(L.XPos + 4, L.YPos - 6, L.SavedMap[2]);
  WriteObjectMap(L.XPos - 4, L.YPos - 2, L.SavedMap[3]);
  WriteObjectMap(L.XPos,     L.YPos - 2, L.SavedMap[4]);
  WriteObjectMap(L.XPos + 4, L.YPos - 2, L.SavedMap[5]);
  WriteObjectMap(L.XPos - 4, L.YPos + 2, L.SavedMap[6]);
  WriteObjectMap(L.XPos,     L.YPos + 2, L.SavedMap[7]);
  WriteObjectMap(L.XPos + 4, L.YPos + 2, L.SavedMap[8]);
end;

procedure TLemmingGame.SetBlockerField(L: TLemming);
begin
  WriteObjectMap(L.XPos - 4, L.YPos - 6, DOM_FORCELEFT);
  WriteObjectMap(L.XPos,     L.YPos - 6, DOM_BLOCKER);
  WriteObjectMap(L.XPos + 4, L.YPos - 6, DOM_FORCERIGHT);
  WriteObjectMap(L.XPos - 4, L.YPos - 2, DOM_FORCELEFT);
  WriteObjectMap(L.XPos,     L.YPos - 2, DOM_BLOCKER);
  WriteObjectMap(L.XPos + 4, L.YPos - 2, DOM_FORCERIGHT);
  WriteObjectMap(L.XPos - 4, L.YPos + 2, DOM_FORCELEFT);
  WriteObjectMap(L.XPos,     L.YPos + 2, DOM_BLOCKER);
  WriteObjectMap(L.XPos + 4, L.YPos + 2, DOM_FORCERIGHT);
end;

function TLemmingGame.CheckForOverlappingField(L: TLemming): Boolean;
const
  BytesToCheck = [DOM_FORCELEFT, DOM_BLOCKER, DOM_FORCERIGHT];
begin
  Result := (ReadObjectMap(L.XPos - 4, L.YPos - 6) in BytesToCheck) or
            (ReadObjectMap(L.XPos,     L.YPos - 6) in BytesToCheck) or
            (ReadObjectMap(L.XPos + 4, L.YPos - 6) in BytesToCheck) or
            (ReadObjectMap(L.XPos - 4, L.YPos - 2) in BytesToCheck) or
            (ReadObjectMap(L.XPos,     L.YPos - 2) in BytesToCheck) or
            (ReadObjectMap(L.XPos + 4, L.YPos - 2) in BytesToCheck) or
            (ReadObjectMap(L.XPos - 4, L.YPos + 2) in BytesToCheck) or
            (ReadObjectMap(L.XPos,     L.YPos + 2) in BytesToCheck) or
            (ReadObjectMap(L.XPos + 4, L.YPos + 2) in BytesToCheck);
end;

procedure TLemmingGame.Transition(L: TLemming; aAction: TLemmingAction; DoTurn: Boolean = False);
//  Handling of a transition and/or turnaround
var
  ix: Integer;
  oldFlags: Byte;
begin
  {$ifdef log_transitions} TGameLogger.LogTransition(Self, L, L.Action, aAction, DoTurn); {$endif}

  // check if any change
  if (L.Action = aAction) and not DoTurn then
    Exit;

  if DoTurn then
    L.xDelta := -L.xDelta;

  // *always* new animation
  ix := TLemmingAnimationSet.AnimationIndices[aAction, L.RTL]; // watch out: here we use the aAction parameter!
  L.LMA := fStyle.LemmingAnimationSet.MetaLemmingAnimationList[ix];
  L.LAB := fStyle.LemmingAnimationSet.LemmingBitmaps[ix];
  L.MaxFrame := L.LMA.FrameCount - 1;
  L.AnimationType := L.LMA.AnimationType;
  L.FrameTopDy  := -L.LMA.FootY; // ccexplore code compatible
  L.FrameLeftDx := -L.LMA.FootX; // ccexplore code compatible

  // transition
  if L.Action = aAction then
    Exit;

  oldFlags := L.CombineFlags;

  L.Action := aAction;
  L.ActionBits := 1 shl Ord(aAction);
  L.Frame := 0;
  L.EndOfAnimation := False;
  L.Fallen := 0;
  L.NumberOfBricksLeft := 0;
  L.CombineFlags := L.CombineFlags and not COMBINE_FLAG_BUILDER;

  // some things to do when entering state
  case L.Action of
    TLemmingAction.Splatting:
      begin
        L.ExplosionTimer := 0;
        L.xDelta := 0;
        CueSoundEffect(SoundData.SFX_SPLAT)
      end;
    TLemmingAction.Blocking:
      begin
        L.IsBlocking := True;
        SaveMap(L);
        SetBlockerField(L);
      end;
    TLemmingAction.Exiting:
      CueSoundEffect(SoundData.SFX_YIPPEE);
    TLemmingAction.Digging:
      L.IsNewDigger := True;
    TLemmingAction.Falling:
      if TMechanic.FallerStartsWith3 in Mechanics then
        L.Fallen := 3;
    TLemmingAction.Building:
      begin
        L.NumberOfBricksLeft := 12;
        L.CombineFlags := L.CombineFlags and not COMBINE_FLAG_BUILDER;
      end;
    TLemmingAction.Ohnoing:
      if not fIsNukedByUser then
        CueSoundEffect(SoundData.SFX_OHNO);
    TLemmingAction.Exploding:
      CueSoundEffect(SoundData.SFX_EXPLOSION);
    TLemmingAction.Floating:
      L.FloatParametersTableIndex := 0;
    TLemmingAction.Mining:
      Inc(L.YPos);
  end;

  if oldFlags <> L.CombineFlags then
    UpdatePixelCombine(L);
end;

procedure TLemmingGame.TurnAround(L: TLemming);
// we assume that the mirrored animations at least have the same framecount
var
  i: Integer;
begin
  L.xDelta := -L.xDelta;
  i := TLemmingAnimationSet.AnimationIndices[L.Action, L.RTL];
  L.LMA := fStyle.LemmingAnimationSet.MetaLemmingAnimationList[i];
  L.LAB := fStyle.LemmingAnimationSet.LemmingBitmaps[i];
  L.MaxFrame := L.LMA.FrameCount - 1;
  L.AnimationType := L.LMA.AnimationType;
  L.FrameTopDy  := -L.LMA.FootY; // ccexplore code compatible
  L.FrameLeftDx := -L.LMA.FootX; // ccexplore code compatible
end;

function TLemmingGame.AssignSkill(Lemming1, Lemming2: TLemming; aSkill: TLemmingAction): TLemming;
// key method
var
  Method: TSkillMethod;
begin
  {$ifdef log_assignments} TGameLogger.LogAssignment(Self, Lemming1, Lemming2, aSkill, fCursorPoint); {$endif}
  Result := nil;
  Method := Skillmethods[aSkill];
  if Assigned(Method) then begin
    Result := Method(Lemming1, Lemming2);
    if Assigned(Result) then begin
      UpdatePixelCombine(Result);
      CueSoundEffect(SoundData.SFX_ASSIGN_SKILL);
    end;
  end;
  fAssignmentIsRightClickGlitch := False;
end;

function TLemmingGame.AssignClimber(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (CurrClimberCount > 0)
  and not Lemming1.IsClimber
  and not (Lemming1.ActionIn(ACTION_BIT_BLOCKING or ACTION_BIT_SPLATTING or ACTION_BIT_EXPLODING)) then begin
    Lemming1.IsClimber := True;
    Dec(CurrClimberCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Climber, currClimberCount);
    if TMechanic.AssignClimberShruggerActionBug in Mechanics then
      if (Lemming1.Action = TLemmingAction.Shrugging) then begin
        Lemming1.Action := TLemmingAction.Walking; // take this bug literally
        Lemming1.ActionBits := ACTION_BIT_WALKING;
      end;
    Result := Lemming1;
    Result.CombineFlags := Result.CombineFlags or COMBINE_FLAG_CLIMBER;
    RecordSkillAssignment(Lemming1, TLemmingAction.Climbing, False, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignFloater(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (CurrFloaterCount > 0)
  and not Lemming1.IsFloater
  and not (Lemming1.ActionIn(ACTION_BIT_BLOCKING or ACTION_BIT_SPLATTING or ACTION_BIT_EXPLODING)) then
  begin
    Lemming1.IsFloater := True;
    Dec(currFloaterCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Umbrella, currFloaterCount);
    Result := Lemming1;
    Result.CombineFlags := Result.CombineFlags or COMBINE_FLAG_FLOATER;
    RecordSkillAssignment(Lemming1, TLemmingAction.Floating, False, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignBomber(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (currBomberCount > 0)
  and (Lemming1.ExplosionTimer = 0)
  and not (lemming1.ActionIn(ACTION_BIT_OHNOING or ACTION_BIT_EXPLODING or ACTION_BIT_VAPORIZING or ACTION_BIT_SPLATTING)) then begin
    Lemming1.ExplosionTimer := 79;
    Dec(currBomberCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Explode, currBomberCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Exploding, False, fAssignmentIsRightClickGlitch);
  end
end;

function TLemmingGame.AssignBlocker(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (currBlockerCount > 0)
  and (lemming1.ActionIn(ACTION_BIT_WALKING or ACTION_BIT_SHRUGGING or ACTION_BIT_BUILDING or ACTION_BIT_BASHING or ACTION_BIT_MINING or ACTION_BIT_DIGGING))
  and (CheckForOverlappingField(Lemming1) = False) then begin
    Dec(CurrBlockerCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Blocker, currBlockerCount);
    Transition(Lemming1, TLemmingAction.Blocking);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Blocking, False, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignBuilder(Lemming1, Lemming2: TLemming): TLemming;
const
  actionSet = ACTION_BIT_WALKING or ACTION_BIT_SHRUGGING or ACTION_BIT_BASHING or ACTION_BIT_MINING or ACTION_BIT_DIGGING;
begin
  Result := nil;

  if (currBuilderCount = 0) or (Lemming1.YPos + Lemming1.FrameTopdy < HEAD_MIN_Y) then
    Exit;

  if (Lemming1.ActionIn(ActionSet)) then begin
    Transition(Lemming1, TLemmingAction.Building);
    Dec(CurrBuilderCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Builder, currBuilderCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Building, False, fAssignmentIsRightClickGlitch);
  end
  else if (Lemming2 <> nil) and (Lemming2.ActionIn(actionSet)) then begin
    Transition(Lemming2, TLemmingAction.Building);
    Dec(CurrBuilderCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Builder, currBuilderCount);
    Result := Lemming2;
    RecordSkillAssignment(Lemming2, TLemmingAction.Building, True, fAssignmentIsRightClickGlitch);
  end;

end;

function TLemmingGame.AssignBasher(Lemming1, Lemming2: TLemming): TLemming;
var
  SelectedLemming: TLemming;
const
  actionSet = ACTION_BIT_WALKING or ACTION_BIT_SHRUGGING or ACTION_BIT_BUILDING or ACTION_BIT_MINING or ACTION_BIT_DIGGING;
begin
  Result := nil;
  if (currBasherCount = 0) then
    Exit
  else if Lemming1.ActionIn(actionSet) then
    SelectedLemming := Lemming1
  else if (Lemming2 <> nil) and (Lemming2.ActionIn(actionSet)) then
    SelectedLemming := Lemming2
  else
    Exit;

  if (SelectedLemming.ObjectInFront = DOM_STEEL) then
  begin
    CueSoundEffect(SoundData.SFX_HITS_STEEL);
    Exit;
  end
  else if ((SelectedLemming.ObjectInFront = DOM_ONEWAYLEFT) and (SelectedLemming.xDelta <> -1)) or
          ((SelectedLemming.ObjectInFront = DOM_ONEWAYRIGHT) and (SelectedLemming.xDelta <> 1)) then
    Exit
  else begin
    Transition(SelectedLemming, TLemmingAction.Bashing);
    Dec(CurrBasherCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Basher, CurrBasherCount);
    Result := SelectedLemming;
    RecordSkillAssignment(SelectedLemming, TLemmingAction.Bashing, SelectedLemming = Lemming2, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignMiner(Lemming1, Lemming2: TLemming): TLemming;
var
  SelectedLemming: TLemming;
const
  ActionSet = ACTION_BIT_WALKING or ACTION_BIT_SHRUGGING or ACTION_BIT_BUILDING or ACTION_BIT_BASHING or ACTION_BIT_DIGGING;
begin
  Result := nil;
  if (CurrMinerCount = 0) then
    Exit
  else if lemming1.ActionIn(actionSet) then
    SelectedLemming := lemming1
  else if Assigned(Lemming2) and (lemming2.ActionIn(actionSet)) then
    SelectedLemming := Lemming2
  else
    Exit;

  if (SelectedLemming.ObjectInFront = DOM_STEEL) then begin
    CueSoundEffect(SoundData.SFX_HITS_STEEL);
    Exit;
  end
  else if (SelectedLemming.ObjectBelow = DOM_STEEL)
  or ((SelectedLemming.ObjectInFront = DOM_ONEWAYLEFT) and (SelectedLemming.xDelta <> -1))
  or ((SelectedLemming.ObjectInFront = DOM_ONEWAYRIGHT) and (SelectedLemming.xDelta <> 1)) then
    Exit
  else begin
    Transition(SelectedLemming, TLemmingAction.Mining);
    Dec(currMinerCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Miner, currMinerCount);
    Result := SelectedLemming;
    RecordSkillAssignment(SelectedLemming, TLemmingAction.Mining, SelectedLemming = Lemming2, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignDigger(Lemming1, Lemming2: TLemming): TLemming;
const
  actionSet = ACTION_BIT_WALKING or ACTION_BIT_SHRUGGING or ACTION_BIT_BUILDING or ACTION_BIT_BASHING or ACTION_BIT_MINING;
begin
  Result := nil;
  if (CurrDiggerCount = 0) or (lemming1.ObjectBelow = DOM_STEEL) then
    Exit
  else if (lemming1.ActionIn(actionSet)) then begin
    Transition(lemming1, TLemmingAction.Digging);
    Dec(currDiggerCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Digger, currDiggerCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Digging, False, fAssignmentIsRightClickGlitch);
  end
  else if Assigned(lemming2) and (lemming2.ActionIn(actionSet)) then begin
    Transition(lemming2, TLemmingAction.Digging);
    Dec(currDiggerCount);
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Digger, currDiggerCount);
    Result := Lemming2;
    RecordSkillAssignment(Lemming2, TLemmingAction.Digging, True, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.UpdateExplosionTimer(L: TLemming): Boolean;
begin
  Result := False;
  Dec(L.ExplosionTimer);
  if L.ExplosionTimer > 0 then
    Exit
  else begin
    if L.ActionIn(ACTION_BIT_VAPORIZING or ACTION_BIT_DROWNING or ACTION_BIT_FLOATING or ACTION_BIT_FALLING)
    then Transition(L, TLemmingAction.Exploding)
    else Transition(L, TLemmingAction.Ohnoing);
    Result := True;
  end;
end;

procedure TLemmingGame.CheckForGameFinished;
begin
  if fIsFinished then
    Exit;
  if fParticleFinishTimer > 0 then
    Exit;

  if (Minutes <= 0) and (Seconds <= 0) then begin
    GameResultRec.TimeIsUp := True;
    Finish;
    Exit;
  end;

  if (LemmingsSaved >= MaxNumLemmings)
  or (LemmingsRemoved >= MaxNumLemmings)
  or (fIsNukedByUser and (LemmingsOut = 0)) then
    Finish;
end;

procedure TLemmingGame.InitializeObjectMap;
//  In one of the previous e-mails I said the DOS Lemmings object map has an
//  x range from -16 to 1647 and a y range from 0 to 159.
//  I think to provide better safety margins, let us extend the y range a bit,
//  say from -16 to 175 (I added 16 in both directions).
//  This is probably slightly on the excessive side but memory is cheap these days,
//  and you can always reduce the x range since DOS Lemmings
//  does not let you scroll to anywhere near x=1647
//  (I think the max visible x range is like 1580 or something).
var
  x, y: Integer;
  Inf : TInteractiveObjectInfo;
  Steel: TSteel;
  Effect, V: Byte;
  MaxO: Integer;
begin

  fObjectMap.SetSize((1647 + OBJMAPOFFSET) div 4, (175 + OBJMAPOFFSET) div 4);
  fObjectMap.Clear(DOM_NONE);

  // map steel
  for Steel in Level.Steels do begin
    for y := Steel.Top to Steel.Top + Steel.Height - 1 do
      for x := Steel.Left to Steel.Left + Steel.Width - 1 do
        WriteObjectMap(x, y, DOM_STEEL);
  end;

  if TMechanic.DisableObjectsAfter15 in Mechanics then
    MaxO := Min(ObjectInfos.Count - 1, 15)
  else
    MaxO := ObjectInfos.Count - 1;

  for var i := 0 to MaxO do begin
    Inf := ObjectInfos[i];
    // 0..127   = triggered trap index
    // 128..255 = triggereffect (128 is DOM_NONE)
    Effect := Inf.MetaObj.TriggerEffect;

    if Effect = ote_TriggeredTrap then
      V := i
    else
      V := Effect + 128;

    var offsetY: Integer := Inf.Obj.Top and not 3 + Inf.MetaObj.TriggerTop;
    var offsetX: Integer := Inf.Obj.Left and not 3 + Inf.MetaObj.TriggerLeft;

    for y := offsetY to offsetY + Inf.MetaObj.TriggerHeight - 1 do
      for x := offsetX to offsetX + Inf.MetaObj.TriggerWidth - 1 do
        WriteObjectMap(x, y, V) // traps --> object_id
  end; // for i

end;


procedure TLemmingGame.InitializeMiniMap;
//  Put the terrainpixels in the fMiniMap. Copy them (scaled) from the worldbitmap.
//  During the game the fMiniMap will be updated like the fWorld-bitmap gets updated.
//  The lemming-pixels are not drawn in the fMiniMap: these are drawn directly in the
//  MiniMapBuffer.
var
  OldCombine: TPixelCombineEvent;
  OldMode: TDrawMode;
  SrcRect, DstRect: TRect;
begin
  fMiniMap.SetSize(DOS_MINIMAP_WIDTH, DOS_MINIMAP_HEIGHT);
  fMiniMap.Clear(0);
  OldCombine := fWorld.OnPixelCombine;
  OldMode := fWorld.DrawMode;
  fWorld.DrawMode := dmCustom;
  fWorld.OnPixelCombine := CombineMinimapWorld;
  SrcRect := fWorld.BoundsRect;
  DstRect := Rect(0, 0, fWorld.Width div 16, fWorld.Height div 8);
  fWorld.DrawTo(fMiniMap, DstRect, SrcRect);
  fWorld.OnPixelCombine := OldCombine;
  fWorld.DrawMode := OldMode;
end;

function TLemmingGame.GetTrapSoundIndex(aDosSoundEffect: Integer): Integer;
begin
  case aDosSoundEffect of
    ose_RopeTrap             : Result := SoundData.SFX_ROPETRAP;
    ose_SquishingTrap        : Result := SoundData.SFX_SQUISHINGTRAP;
    ose_TenTonTrap           : Result := SoundData.SFX_TENTON;
    ose_BearTrap             : Result := SoundData.SFX_BEARTRAP;
    ose_ElectroTrap          : Result := SoundData.SFX_ELECTROTRAP;
    ose_SpinningTrap         : Result := SoundData.SFX_SPINNINGTRAP;
  else
    Result := -1;
  end;
end;

procedure TLemmingGame.CheckForInteractiveObjects(L: TLemming);
var
  Inf: TInteractiveObjectInfo;
begin
  L.ObjectBelow := ReadObjectMap(L.XPos, L.YPos);
  L.ObjectInFront := ReadObjectMap(L.XPos + 8 * L.xDelta, L.YPos - 8);

  case L.ObjectBelow of
    // DOM_NONE = 128 = nothing
    DOM_NONE:
      Exit;
    // 0..127 triggered objects
    0..127:
      begin
        Inf := ObjectInfos[L.ObjectBelow];
        if not Inf.Triggered then begin
          {$ifdef log_traptriggering} TGameLogger.LogTrapTriggering(Self, inf); {$endif}
          Inf.Triggered := True;
          Inf.CurrentFrame := 0;
          if not (TMechanic.TriggeredTrapLemmixBugSolved in fMechanics) then
            Inc(Inf.CurrentFrame);
          if Inf.CurrentFrame >= Inf.MetaObj.AnimationFrameCount then
            Inf.CurrentFrame := 0;
          RemoveLemming(L);
          CueSoundEffect(GetTrapSoundIndex(Inf.MetaObj.SoundEffect));
        end;
      end;

    // 128 + n (continuous objects, staticobjects, steel, oneway wall)
    DOM_EXIT:
      begin
        if L.Action <> TLemmingAction.Falling then begin
          Transition(L, TLemmingAction.Exiting);
          CueSoundEffect(SoundData.SFX_YIPPEE);
        end;
      end;
    DOM_FORCELEFT:
      if L.xDelta > 0 then
        TurnAround(L);
    DOM_FORCERIGHT:
      if L.xDelta < 0 then
        TurnAround(L);
    DOM_WATER:
      begin
        Transition(L, TLemmingAction.Drowning);
        CueSoundEffect(SoundData.SFX_DROWNING);
      end;
    DOM_FIRE:
      begin
        Transition(L, TLemmingAction.Vaporizing);
        CueSoundEffect(SoundData.SFX_VAPORIZING);
      end;
  end;
end;

procedure TLemmingGame.ApplyExplosionMask(L: TLemming);
// dos explosion mask 16 x 22
var
  X, Y: Integer;
begin
  ExplodeMaskBmp.DrawTo(fWorld, L.XPos - 8, L.YPos -14);
  if not HyperSpeed then
    ExplodeMaskBmp.DrawTo(fTargetBitmap, L.XPos - 8, L.YPos -14);

  // fake draw mask in fMiniMap. this clears 4 pixels as windows programmers should know
  X := L.XPos div 16;
  Y := L.YPos div 8;
  fMiniMap.FillRectS(X - 1, Y - 1, X + 1, Y + 1, 0);
end;

procedure TLemmingGame.ApplyBashingMask(L: TLemming; MaskFrame: Integer);
// dos bashing mask = 16 x 10
var
  Bmp: TBitmap32;
  S, D: TRect;
  X, Y: Integer;
begin
  Bmp := BashMasks[L.RTL];

  S := Bmp.CalcFrameRect(4, MaskFrame);
  D.Left := L.XPos + L.FrameLeftDx;
  D.Top := L.YPos + L.FrameTopDy;
  D.Right := D.Left + 16;
  D.Bottom := D.Top + 10;

  {$ifdef paranoid} Assert(CheckRectCopy(D, S), 'bash rect error'); {$endif}

  Bmp.DrawTo(fWorld, D, S);
  if not HyperSpeed then
    Bmp.DrawTo(fTargetBitmap, D, S);

  // fake draw mask in fMiniMap
  X := L.XPos div 16;
  Y := L.YPos div 8;
  fMiniMap.PixelS[X, Y] := 0;
end;

procedure TLemmingGame.ApplyMinerMask(L: TLemming; MaskFrame, X, Y: Integer);
// x,y is topleft
var
  Bmp: TBitmap32;
  S, D: TRect;
  aX, aY: Integer;
begin
  {$ifdef paranoid} Assert((MaskFrame >= 0) and (MaskFrame <= 1), 'miner mask error'); {$endif}

  Bmp := MineMasks[L.RTL];

  S := Bmp.CalcFrameRect(2, MaskFrame);
  D.Create(X, Y, X + S.Width, Y + S.Height);

  {$ifdef paranoid} Assert(CheckRectCopy(D, S), 'miner rect error ' + rectstr(d) + ' ' + rectstr(s)); {$endif}

  Bmp.DrawTo(fWorld, D, S);
  if not HyperSpeed then
    Bmp.DrawTo(fTargetBitmap, D, S);

  // fake draw mask in fMiniMap
  aX := L.XPos div 16;
  aY := L.YPos div 8;
  fMiniMap.PixelS[aX, aY] := 0;
end;

procedure TLemmingGame.EraseParticles(L: TLemming);
//  Erase the previously drawn particles of an exploded lemming
var
  i, X, Y: Integer;
  Drawn: Boolean;
begin
  if not GameOptions.ShowParticles then
    Exit;

  Drawn := False;

  if L.ParticleFrame <= 50 then begin
    for i := 0 to 79 do begin
      X := fParticles[L.ParticleFrame][i].DX;
      Y := fParticles[L.ParticleFrame][i].DY;
      if (X <> -128) and (Y <> -128) then begin
        X := L.XPos + X;
        Y := L.YPos + Y;
        fTargetBitmap.PixelS[X, Y] := fWorld.PixelS[X, Y];
        Drawn := True;
      end;
    end;
  end;

  fExplodingPixelsUpdateNeeded := Drawn;
end;

procedure TLemmingGame.DrawParticles(L: TLemming);
var
  i: Integer;
  X, Y: Integer;
  Drawn: Boolean;
begin
  if not GameOptions.ShowParticles then
    Exit;

  Drawn := False;

  if L.ParticleFrame <= 50 then begin
    for i := 0 to 79 do begin
      X := fParticles[L.ParticleFrame][i].DX;
      Y := fParticles[L.ParticleFrame][i].DY;
      if (X <> -128) and (Y <> -128) then begin
        X := L.XPos + X;
        Y := L.YPos + Y;
        fTargetBitmap.PixelS[X, Y] := fParticleColors[i mod 16];
        Drawn := True;
      end;
    end;
  end;

  fExplodingPixelsUpdateNeeded := Drawn;
end;


procedure TLemmingGame.DrawAnimatedObjects;
// the order is important
var
  Inf : TInteractiveObjectInfo;
begin
  if HyperSpeed then
    Exit;

  // erase entrances
  for Inf in Entrances do
    Renderer.EraseObject(fTargetBitmap, Inf.Obj, fWorld);

  // erase other objects
  for Inf in ObjectInfos do
    Renderer.EraseObject(fTargetBitmap, Inf.Obj, fWorld);

  // draw entrances only on terrain
  for Inf in Entrances do
    if Inf.OnlyOnTerrain then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil);

  // draw other objects only on terrain
  for Inf in ObjectInfos do
    if Inf.OnlyOnTerrain then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil);

  // draw entrances rest
  for Inf in Entrances do
    if not Inf.OnlyOnTerrain then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil);

  // draw other objects
  for Inf in ObjectInfos do
    if not Inf.OnlyOnTerrain then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil);

end;

procedure TLemmingGame.EraseLemmings;
// Erase the lemming from the targetbitmap by copying its rect from the fWorld bitmap.
var
  CurrentLemming: TLemming;
  DstRect: TRect;
begin
  if HyperSpeed or fLemmingList.IsEmpty then
    Exit;

  for CurrentLemming in fLemmingList do begin
    if not CurrentLemming.IsRemoved then begin
      DstRect := CurrentLemming.RectToErase;
      DstRect.Inflate(2, 2);
      // important to intersect the rects
      if GR32.IntersectRect(DstRect, DstRect, fWorld.BoundsRect) then
        fWorld.DrawTo(fTargetBitmap, DstRect, DstRect);
    end
    else if CurrentLemming.ParticleTimer > 0 then begin
      EraseParticles(CurrentLemming);
    end;
  end;

end;

procedure TLemmingGame.DrawLemmings;
var
  L: TLemming;
  SrcRect, DstRect, DigRect: TRect;
  Digit: Integer;
  oldCombine: TPixelCombineEvent;
begin
  if HyperSpeed then
    Exit;

  fMinimapBuffer.Assign(fMiniMap);

  for L in fLemmingList do begin
    if not L.IsRemoved then begin
      CurrentlyDrawnLemming := L;
      SrcRect := L.GetFrameBounds;
      DstRect := L.GetLocationBounds;
      L.RectToErase := DstRect;

      fMinimapBuffer.PixelS[L.XPos div 16, L.YPos div 8] := Color32(0, 255, 000);

      if not L.PhotoFlashForReplay then begin
        //oldCombine := L.LAB.OnPixelCombine;
        L.LAB.OnPixelCombine := L.PixelCombine;
        L.LAB.DrawTo(fTargetBitmap, DstRect, SrcRect);
        //L.LAB.OnPixelCombine := oldCombine;
      end
      else begin
        oldCombine := L.LAB.OnPixelCombine;
        L.LAB.OnPixelCombine := TLemmingGame.CombineLemmingPhotoFlash;
        L.LAB.DrawTo(fTargetBitmap, DstRect, SrcRect);
        L.LAB.OnPixelCombine := oldCombine;
        L.PhotoFlashForReplay := False;
      end;

      if L.ExplosionTimer > 0 then
      begin
        SrcRect := Rect(0, 0, 8, 8);
        DigRect := L.GetCountDownDigitBounds;
        L.RectToErase.Top := DigRect.Top;
        {$ifdef paranoid} Assert(CheckRectCopy(SrcRect, DigRect), 'digit rect copy'); {$endif}

        case L.ExplosionTimer of
          65..79  : Digit := 5;
          49..64  : Digit := 4;
          33..48  : Digit := 3;
          17..32  : Digit := 2;
          00..16  : Digit := 1;
        else Digit := 1;
        end;

        SrcRect.Offset(0, (5 - Digit) * 8); // get "frame"
        CntDownBmp.DrawTo(fTargetBitmap, DigRect, SrcRect);
      end;
    end // not IsRemoved

    // check explosiondrawing if the lemming is already dead
    else if L.ParticleTimer > 0 then begin
      DrawParticles(L);
    end;

  end; // for lemming...

  // todo: i do not like the next lines being here
  HitTest;
  fToolbar.SetInfoLemmingsOut(LemmingsOut);
end;

procedure TLemmingGame.EraseReplayCursor;
begin
  if fHyperSpeed or not fReplaying or not ReplayCursor.Erasable then
    Exit;
  if not ReplayCursor.PreviousDrawRect.IsEmpty then begin
    fWorld.DrawTo(fTargetBitmap, ReplayCursor.PreviousDrawRect, ReplayCursor.PreviousDrawRect);
    ReplayCursor.PreviousDrawRect := TRect.Empty;
  end;
  fWorld.DrawTo(fTargetBitmap, ReplayCursor.DrawRect, ReplayCursor.DrawRect);
end;

procedure TLemmingGame.DrawReplayCursorCheck;
// this draw is called directly so no checks needed
begin
  if HyperSpeed or not ReplayCursor.Visible then
    Exit;
  ReplayCursor.Bitmap.DrawTo(fTargetBitmap, ReplayCursor.Position.X, ReplayCursor.Position.Y);
  ReplayCursor.Decrement;
end;

procedure TLemmingGame.DrawReplayCursor(const P: TPoint);
// this draw is called directly so no checks needed
begin
  ReplayCursor.Activate(P);
  ReplayCursor.Bitmap.DrawTo(fTargetBitmap, ReplayCursor.Position.X, ReplayCursor.Position.Y);
end;

procedure TLemmingGame.EraseMessages;
// N.B: high res messages do not need to erase
var
  DstRect: TRect;
  Msg: TLowResolutionMessage;
  i: Integer;
begin
  if HyperSpeed or MessageList.IsEmpty or GameOptions.HighResolutionGameMessages then
    Exit;

  for Msg in MessageList do begin
    DstRect := Rect(Msg.fLocation.X, Msg.fLocation.Y, Msg.fLocation.X + Msg.fBuffer.Width, Msg.fLocation.Y + Msg.fBuffer.Height);
    if fWorld.BoundsRect.IntersectsWith(DstRect) then
      fWorld.DrawTo(fTargetBitmap, DstRect, DstRect);
  end;

  // free the messages that have ended
  for i := MessageList.Count - 1 downto 0 do begin
    Msg := MessageList[i];
    if Msg.fEnded then
      MessageList.Delete(i);
  end;
end;

procedure TLemmingGame.DrawMessages;
var
  DstRect: TRect;
  Msg: TLowResolutionMessage;
begin
  if HyperSpeed then
    Exit;
  if GameOptions.HighResolutionGameMessages then begin
    if HighResolutionLayer.Visible and not HighResolutionLayer.MessageList.IsEmpty then
      HighResolutionLayer.Update;
  end
  else begin
    for Msg in MessageList do begin
      if Msg.fEnded then
        Continue;
      DstRect := Rect(Msg.fLocation.X, Msg.fLocation.Y, Msg.fLocation.X + Msg.fBuffer.Width, Msg.fLocation.Y + Msg.fBuffer.Height);
      Msg.fBuffer.DrawTo(fTargetBitmap, Msg.fLocation.X, Msg.fLocation.Y);
    end;
  end;
end;

procedure TLemmingGame.DrawToolbar;
begin
  if HyperSpeed then
    Exit;
  Toolbar.RefreshInfo;
  //Toolbar.RefreshButtons;
end;

procedure TLemmingGame.DrawMinimap;
begin
  if HyperSpeed then
    Exit;
  Toolbar.DrawMiniMap(MiniMapBuffer);
end;

procedure TLemmingGame.LayBrick(L: TLemming);
{-------------------------------------------------------------------------------
  bricks are in the lemming area so will automatically be copied to the screen
  during drawlemmings
-------------------------------------------------------------------------------}
var
  i, x: Integer;
  NumPixelsFilled: Integer;
  C: TColor32;
begin
  NumPixelsFilled := 0;

  if (L.xDelta = 1)
  then x := L.XPos
  else x := L.XPos - 4;

  i := 12 - L.NumberOfBricksLeft;
  if i < 0 then i := 0;
  if i > 11 then i := 11;
  C := BrickPixelColors[i];
  C := C or ALPHA_TERRAIN;

  repeat
    if fWorld.PixelS[x, L.YPos - 1] = 0 then
      fWorld.PixelS[x, L.YPos - 1] := C;
    Inc(NumPixelsFilled);
    Inc(X);
  until NumPixelsFilled = 6;
end;

function TLemmingGame.DigOneRow(L: TLemming; Y: Integer): Boolean;
var
  yy, N, X: Integer;
begin
  Result := False;

  n := 1;
  x := L.XPos - 4;
  yy := Y;
  if (yy < 0) then yy := 0;

  while (n <= 9) do begin
    if HasPixelAt(x, yy) then begin
      RemovePixelAt(x, yy);
      Result := True;
    end;
    inc(n);
    inc(x);
  end;// while

  // fake draw mask in fMiniMap
  fMiniMap.PixelS[L.XPos div 16, L.YPos div 8] := 0;

  if Result then
    CueSoundEffect(SoundData.SFX_DIGGER);

end;

function TLemmingGame.HandleLemming(L: TLemming): Boolean;
{-------------------------------------------------------------------------------
  This is the main lemming method, called by CheckLemmings().
  The return value should return true if the lemming has to be checked by
  interactive objects.
  • Increment lemming animationframe
  • Call specialized action-method
  • Do *not* call this method for a removed lemming
-------------------------------------------------------------------------------}
var
  Method: TLemmingMethod;
begin
  Inc(fHandleLemmingCalls);

  // next frame (except floating and digging which are handled differently)
  if not (L.ActionIn(ACTION_BIT_FLOATING or ACTION_BIT_DIGGING)) then begin
    if L.Frame < L.MaxFrame then begin
      L.EndOfAnimation := False;
      Inc(L.Frame);
    end
    else begin
      L.EndOfAnimation := True;
      if L.AnimationType = TLemmingAnimationType.Loop then
        L.Frame := 0;
    end;
  end;

  Method := LemmingMethods[L.Action];
  Result := Method(L);
end;

function TLemmingGame.HandleWalking(L: TLemming): Boolean;
var
  dy, NewY: Integer;
begin
  Inc(L.XPos, L.xDelta);

  if (L.XPos >= LEMMING_MIN_X) and (L.XPos <= LEMMING_MAX_X) then begin
    if HasPixelAt_ClipY(L.XPos, L.YPos, 0) then begin
      // walk, jump, climb, or turn around
      dy := 0;
      NewY := L.YPos;
      while (dy <= 6) and HasPixelAt_ClipY(L.XPos, NewY - 1, -dy - 1) do begin
        Inc(dy);
        Dec(NewY);
      end;

      if dy > 6 then begin
        if (L.IsClimber)
        then Transition(L, TLemmingAction.Climbing)
        else TurnAround(L);
        Exit(True);
      end
      else begin
        if dy >= 3 then begin
          Transition(L, TLemmingAction.Jumping);
          NewY := L.YPos - 2;
        end;
        L.YPos := NewY;
        CheckForLevelTopBoundary(L);
        Exit(True);
      end
    end
    // no pixel at feet
    else begin
      // walk or fall downwards
      dy := 1;
      while dy <= 3 do begin
        Inc(L.YPos);
        if HasPixelAt_ClipY(L.XPos, L.YPos, dy) then
          Break;
        Inc(Dy);
      end;

      if dy > 3 then begin
        // in this case, lemming becomes a faller
        Inc(L.YPos);
        Transition(L, TLemmingAction.Falling);
      end;

      if L.YPos > LEMMING_MAX_Y then begin
        RemoveLemming(L);
        CueSoundEffect(SoundData.SFX_SILENTDEATH);
        Exit(False);
      end
      else
        Exit(True);
    end;
  end
  // almost out of level bounds
  else begin
    TurnAround(L);
    Exit(True);
  end;
end;

function TLemmingGame.HandleJumping(L: TLemming): Boolean;
var
  dy: Integer;
begin
  dy := 0;
  while (dy < 2) and HasPixelAt_ClipY(L.XPos, L.YPos - 1, -dy - 1) do begin
    Inc(Dy);
    Dec(L.YPos);
  end;

  if dy < 2 then
    Transition(L, TLemmingAction.Walking);
  CheckForLevelTopBoundary(L);
  Result := True;
end;

function TLemmingGame.HandleDigging(L: TLemming): Boolean;
// returns False if there are no terrain pixels to remove (??? did I write this?)
var
  Y: Integer;
begin
  if L.IsNewDigger then begin
    DigOneRow(L, L.YPos - 2);
    DigOneRow(L, L.YPos - 1);
    L.IsNewDigger := False;
  end
  else begin
    Inc(L.Frame);
    if (L.Frame >= 16) then
      L.Frame := L.Frame - 16
  end;

  if L.Frame in [0, 8] then begin
    y := L.YPos;
    Inc(L.YPos);

    if (L.YPos > LEMMING_MAX_Y) then begin
      // #EL 2020-02-23: we changed L.IsRemoved with RemoveLemming(). This was a small bug. But we almost never get here so in 15 years nobody noticed.
      RemoveLemming(L);
      CueSoundEffect(SoundData.SFX_SILENTDEATH);
      Exit(False);
    end;

    if not DigOneRow(L, y) then
      Transition(L, TLemmingAction.Falling)
    else if (ReadObjectMap(L.XPos, L.YPos) = DOM_STEEL) then begin
      CueSoundEffect(SoundData.SFX_HITS_STEEL);
      Transition(L, TLemmingAction.Walking);
    end;

    Exit(True);
  end
  else
    Exit(False);
end;

function TLemmingGame.HandleClimbing(L: TLemming): Boolean;
begin
  if (L.Frame <= 3) then begin
    // check if we approached the top
    if not HasPixelAt_ClipY(L.XPos, L.YPos - 7 - L.Frame, 0) then begin
      L.YPos := L.YPos - L.Frame + 2;
      Transition(L, TLemmingAction.Hoisting);
      CheckForLevelTopBoundary(L);
    end;
    Exit(True);
  end
  else begin
    Dec(L.YPos);
    // check for overhang or level top boundary
    if (L.YPos + L.FrameTopDy < HEAD_MIN_Y)
    or HasPixelAt_ClipY(L.XPos - L.xDelta, L.YPos - 8, -8) then begin
      Transition(L, TLemmingAction.Falling, True);
      Inc(L.XPos, L.xDelta * 2);
    end;
    Exit(True);
  end;
end;

function TLemmingGame.HandleDrowning(L: TLemming): Boolean;
// here the use of HasPixelAt rather than HasPixelAt_ClipY is correct
begin
  Result := False;
  if L.EndOfAnimation then
    RemoveLemming(L)
  else if not HasPixelAt(L.XPos + 8 * L.xDelta, L.YPos) then
    Inc(L.XPos, L.xDelta);
end;

function TLemmingGame.HandleHoisting(L: TLemming): Boolean;
begin
  Result := False;
  if L.Frame <= 4 then begin
    Dec(L.YPos, 2);
    CheckForLevelTopBoundary(L);
    Exit(True);
  end
  // Frame = 7
  else if (L.EndOfAnimation) then begin
    Transition(L, TLemmingAction.Walking);
    CheckForLevelTopBoundary(L);
    Exit(True);
  end
end;

function TLemmingGame.HandleBuilding(L: TLemming): Boolean;
begin
  // sound
  if (L.Frame = 10) and (L.NumberOfBricksLeft <= 3) then
    CueSoundEffect(SoundData.SFX_BUILDER_WARNING);

  // lay brick
  if (L.Frame = 9) or ( (L.Frame = 10) and (L.NumberOfBricksLeft = 9) ) then begin
    LayBrick(L);
    Exit(False);
  end
  else if (L.Frame = 0) then begin
    Inc(L.XPos, L.xDelta);
    Dec(L.YPos);
    if (L.XPos <= LEMMING_MIN_X) or (L.XPos > LEMMING_MAX_X)
    or HasPixelAt_ClipY(L.XPos, L.YPos - 1, -1) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      CheckForLevelTopBoundary(L);
      Exit(True);
    end;

    Inc(L.XPos, L.xDelta);
    if HasPixelAt_ClipY(L.XPos, L.YPos - 1, -1) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      CheckForLevelTopBoundary(L);
      Exit(True);
    end;

    Dec(L.NumberOfBricksLeft);
    if (L.NumberOfBricksLeft = 0) then begin
      Transition(L, TLemmingAction.Shrugging);
      CheckForLevelTopBoundary(L);
      Exit(True);
    end;

    if HasPixelAt_ClipY(L.XPos + L.xDelta * 2, L.YPos - 9, -9)
    or (L.XPos <= LEMMING_MIN_X)
    or (L.XPos > LEMMING_MAX_X) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      CheckForLevelTopBoundary(L);
      Exit(True);
    end;

    {-------------------------------------------------------------------------------
      if builder too high he becomes a walker. will *not* turn around
      although it seems he should, but the CheckForLevelTop fails because
      of a changed FrameTopDy
    -------------------------------------------------------------------------------}
    if (L.YPos + L.FrameTopDy < HEAD_MIN_Y) then
    begin
      Transition(L, TLemmingAction.Walking);
      CheckForLevelTopBoundary(L);
    end;

    Exit(True);
  end
  else
    Exit(True);
end;

function TLemmingGame.HandleBashing(L: TLemming): Boolean;
var
  n, x, y, dy, Index: Integer;
  FrontObj: Byte;
begin
  index := L.Frame;
  if index >= 16 then
    Dec(index, 16);

  if (11 <= index) and (index <= 15) then begin
    Inc(L.XPos, L.xDelta);

    if (L.XPos < LEMMING_MIN_X) or (L.XPos > LEMMING_MAX_X) then begin
      // outside leftside or outside rightside?
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
    end
    else begin
      // check 3 pixels below the new position
      dy := 0;
      while (dy < 3) and not HasPixelAt_ClipY(L.XPos, L.YPos, dy) do begin
        Inc(dy);
        Inc(L.YPos);
      end;

      if dy = 3 then
        Transition(L, TLemmingAction.Falling)
      else begin
        // check steel or one way digging
        FrontObj := ReadObjectMap(L.XPos + L.xDelta * 8, L.YPos - 8);

        if (FrontObj = DOM_STEEL) then
          CueSoundEffect(SoundData.SFX_HITS_STEEL);

        if (FrontObj = DOM_STEEL)
        or ( (FrontObj = DOM_ONEWAYLEFT) and (L.xDelta <> -1) )
        or ( (FrontObj = DOM_ONEWAYRIGHT) and (L.xDelta <> 1) ) then
        begin
          //TurnAround(L);
          Transition(L, TLemmingAction.Walking, True);  // turn around as well
        end;
      end;

    end;
    Exit(True);
  end
  else begin
    if (2 <= index) and (index <= 5) then begin
      // frame 2..5 and 18..21 or used for masking
      ApplyBashingMask(L, index - 2);
      if index = 5 then
        CueSoundEffect(SoundData.SFX_BASHER);
      // special treatment frame 5 (see txt)
      if L.Frame = 5 then begin
        n := 0;
        x := L.XPos + L.xDelta * 8;
        y := L.YPos - 6;
        // here the use of HasPixelAt rather than HasPixelAt_ClipY is correct
        while (n < 4) and not HasPixelAt(x, y) do begin
          Inc(n);
          Inc(x, L.xDelta);
        end;
        if n = 4 then
          Transition(L, TLemmingAction.Walking);
      end;
    end;
    Exit(False);
  end;
end;

function TLemmingGame.HandleMining(L: TLemming): Boolean;
var
  BelowObj: Byte;
  Bug: Boolean;
begin
  if L.Frame = 1 then begin
    ApplyMinerMask(L, 0, L.XPos + L.frameLeftdx, L.YPos + L.frameTopdy);
    Exit(False);
  end
  else if L.Frame = 2 then begin
    ApplyMinerMask(L, 1, L.XPos + L.xDelta + L.frameLeftdx, L.YPos + 1 + L.frameTopdy);
    CueSoundEffect(SoundData.SFX_MINER);
    Exit(False);
  end
  else if L.Frame in [3, 15] then begin
    Inc(L.XPos, L.xDelta);
    if (L.XPos < LEMMING_MIN_X) or (L.XPos > LEMMING_MAX_X) then begin
      Transition(L, TLemmingAction.Walking, True); // turn around as well
      Exit(True);
    end;

    Inc(L.XPos, L.xDelta);
    if (L.XPos < LEMMING_MIN_X) or (L.XPos > LEMMING_MAX_X) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      Exit(True);
    end;

    if (L.Frame = 3) then begin
      Inc(L.YPos);
      if (L.YPos > LEMMING_MAX_Y) then begin
        RemoveLemming(L);
        CueSoundEffect(SoundData.SFX_SILENTDEATH);
        Exit(False);
      end;
    end;

    if not HasPixelAt_ClipY(L.XPos, L.YPos, 0) then begin
      Transition(L, TLemmingAction.Falling);
      Exit(True);
    end;

    belowObj := ReadObjectMap(L.XPos, L.YPos);
    if (belowObj = DOM_STEEL) then
      CueSoundEffect(SoundData.SFX_HITS_STEEL);

    {-------------------------------------------------------------------------------
      Emulating dos-original bug onewayright:
      The check for steel and one-way walls, you see that the (belowObj == ONE_WAY_RIGHT)
      seems to be missing a check for the lemming's facing direction.
      That's correct as is, and it is simply emulating the same bug in DOS Lemmings,
      which results in a one-way wall pointing right being unminable in either direction.
    -------------------------------------------------------------------------------}
    // Optional Game Mechanic
    Bug := TMechanic.MinerOneWayRightBug in Mechanics;
    if Bug then begin
      if (belowObj = DOM_STEEL)
      or ( (belowObj = DOM_ONEWAYLEFT) and (L.xDelta <> -1) )
      or (belowObj = DOM_ONEWAYRIGHT) then begin
        Transition(L, TLemmingAction.Walking, True);  // turn around as well
     end;
    end
    else begin
      if (belowObj = DOM_STEEL)
      or ( (belowObj = DOM_ONEWAYLEFT) and (L.xDelta <> -1) )
      or ( (belowObj = DOM_ONEWAYRIGHT) and (L.xDelta <> 1) ) then // complete check
        Transition(L, TLemmingAction.Walking, True);  // turn around as well
    end;
    // until here the bug-issue

    Exit(True);
  end
  else if (L.Frame = 0) then begin
    Inc(L.YPos);
    if (L.YPos > LEMMING_MAX_Y) then begin
      RemoveLemming(L);
      CueSoundEffect(SoundData.SFX_SILENTDEATH);
      Exit(False);
    end
    else
      Exit(True);
  end
  else
    Exit(False);
end;

function TLemmingGame.HandleFalling(L: TLemming): Boolean;
var
  dy: Integer;
begin
  if (L.Fallen > 16) and L.IsFloater then begin
    Transition(L, TLemmingAction.Floating);
    Exit(True);
  end
  else begin
    dy := 0;
    while (dy < 3) and not HasPixelAt_ClipY(L.XPos, L.YPos, dy) do begin
      Inc(Dy);
      Inc(L.YPos);
      if (L.YPos > LEMMING_MAX_Y) then begin
        RemoveLemming(L);
        CueSoundEffect(SoundData.SFX_SILENTDEATH);
        Exit(False);
      end;
    end; // while

    if dy = 3 then begin
      Inc(L.Fallen, 3);
      Exit(True);
    end
    else begin
      if L.Fallen > MAX_FALLDISTANCE then begin
        Transition(L, TLemmingAction.Splatting);
        { ccexplore:
          However, the "return true" after call lemming.SetToSplattering()
          is actually correct.  It is in fact the bug in DOS Lemmings that
          I believe enables the "direct drop to exit":
          by returning True, it gives a chance for lemming.CheckForInteractiveObjects()
          to be called immediately afterwards, which ultimately results in the
          lemming's action turning from SPLATTERING to EXITING.
        }
        Exit(True);
      end
      else begin
        Transition(L, TLemmingAction.Walking);
        Exit(True);
      end;
    end;
  end;
end;

function TLemmingGame.HandleFloating(L: TLemming): Boolean;
var
  dy, minY: Integer;
begin
  L.Frame := FloatParametersTable[L.FloatParametersTableIndex].AnimationFrameIndex;
  dy := FloatParametersTable[L.FloatParametersTableIndex].dy;

  Inc(L.FloatParametersTableIndex);
  if L.FloatParametersTableIndex >= 16 then
    L.FloatParametersTableIndex := 8;

  if (dy <= 0) then
    Inc(L.YPos, dy)
  else begin
    minY := 0;
    while (dy > 0) do begin
      if HasPixelAt_ClipY(L.XPos, L.YPos, minY) then begin
        Transition(L, TLemmingAction.Walking);
        Exit(True)
      end
      else begin
        Inc(L.YPos);
        Dec(dy);
        Inc(minY);
      end;
    end; // while
    if L.FloatParametersTableIndex = 1 then
      CueSoundEffect(SoundData.SFX_OPENUMBRELLA);
  end;

  if (L.YPos > LEMMING_MAX_Y) then begin
    RemoveLemming(L);
    CueSoundEffect(SoundData.SFX_SILENTDEATH);
    Exit(False);
  end
  else
    Exit(True);
end;

function TLemmingGame.HandleSplatting(L: TLemming): Boolean;
begin
  Result := False;
  if L.EndOfAnimation then
    RemoveLemming(L);
end;

function TLemmingGame.HandleExiting(L: TLemming): Boolean;
begin
  Result := False;
  if L.EndOfAnimation then begin
    RemoveLemming(L);
    Inc(LemmingsSaved);
    fToolbar.SetInfoLemmingsOut(LemmingsOut);
    fToolbar.SetInfoLemmingsSaved(LemmingsSaved, MaxNumLemmings, not MiscOptions.LemmingsPercentages);
  end;
end;

function TLemmingGame.HandleVaporizing(L: TLemming): Boolean;
begin
  Result := False;
  if L.EndOfAnimation then
    RemoveLemming(L);
end;

function TLemmingGame.HandleBlocking(L: TLemming): Boolean;
begin
  Result := False;
  if not HasPixelAt_ClipY(L.XPos, L.YPos, 0) then begin
    Transition(L, TLemmingAction.Walking);
    L.IsBlocking := False;
    RestoreMap(L);
  end;
end;

function TLemmingGame.HandleShrugging(L: TLemming): Boolean;
begin
  Result := False;
  if L.EndOfAnimation then begin
    Transition(L, TLemmingAction.Walking);
    Result := True;
  end
end;

function TLemmingGame.HandleOhNoing(L: TLemming): Boolean;
var
  dy: Integer;
begin
  if L.EndOfAnimation then begin
    Transition(L, TLemmingAction.Exploding);
    Exit(False);
  end
  else begin
    dy := 0;
    while (dy < 3) and not HasPixelAt_ClipY(L.XPos, L.YPos, dy) do begin
      Inc(dy);
      Inc(L.YPos);
    end;

    if (L.YPos > LEMMING_MAX_Y) then begin
      RemoveLemming(L);
      CueSoundEffect(SoundData.SFX_SILENTDEATH);
      Exit(False);
    end
    else
      Exit(True);
  end;
end;

function TLemmingGame.HandleExploding(L: TLemming): Boolean;
begin
  Result := False;
  if L.EndOfAnimation then begin
    if L.IsBlocking then begin
      L.IsBlocking := False;
      RestoreMap(L);
    end;
    if not (ReadObjectMap(L.XPos, L.YPos) in [DOM_STEEL, DOM_WATER]) then
      ApplyExplosionMask(L);
    RemoveLemming(L);
    L.IsExploded := True;
    L.ParticleTimer := PARTICLE_FRAMECOUNT;
    if GameOptions.ShowParticles then
      fParticleFinishTimer := PARTICLE_FRAMECOUNT;
  end;
end;

function TLemmingGame.CheckForLevelTopBoundary(L: TLemming; LocalFrameTopDy: Integer = 0): Boolean;
var
  dy: Integer;
begin
  Result := False;
  if LocalFrameTopDy = 0 then
    dy := L.FrameTopDy
  else
    dy := LocalFrameTopDy;
  if (L.YPos + dy < HEAD_MIN_Y) then begin
    Result := True;
    L.YPos := HEAD_MIN_Y - 2 - dy;
    TurnAround(L);
    if L.Action = TLemmingAction.Jumping then
      Transition(L, TLemmingAction.Walking);
  end;
end;


procedure TLemmingGame.RemoveLemming(L: TLemming);
begin
  {$ifdef log_remove} TGameLogger.LogRemove(Self, L); {$endif}
    L.IsRemoved := True;
  Dec(LemmingsOut);
  Inc(LemmingsRemoved);
end;

procedure TLemmingGame.InternalUpdate(force, leaveHyperspeed: Boolean);
{-------------------------------------------------------------------------------
  The main method: handling a single frame of the game.
-------------------------------------------------------------------------------}
begin
  Inc(fUpdateCalls);
  Inc(fUpdateCallsSession);
  if fIsFinished then
    Exit;
  CheckForGameFinished;

  // issue #14: replay iteration zero
  if CurrentIteration = 0 then
    CheckForReplayAction;

  if (force and HyperSpeed) or not (fIsPaused and fReplaying) then
    CheckAdjustReleaseRate;

  if IsPaused and not force then begin
    DrawToolBar;
    DrawMiniMap;
    DrawDebug;
    Exit;
  end;

  IncrementIteration;

  if not HyperSpeed then begin
    EraseLemmings;
    EraseReplayCursor;
    EraseMessages;
  end;

  CheckSpawnLemming;
  CheckLemmings;
  CheckUpdateNuking;
  UpdateInteractiveObjects;
  UpdateMessages;

  // when hyperspeed is terminated then copy complete fWorld back into targetbitmap
  if leaveHyperSpeed then begin
    fHyperSpeed := False;
    fTargetBitmap.Assign(fWorld);
    RepaintSkillPanelButtons;
    if fTargetBitmapsUpdatingSet then begin
      Assert((fTargetBitmap.GetUpdateCount > 0) and (fToolbar.GetUpdateCount = fTargetBitmap.GetUpdateCount));
      fTargetBitmapsUpdatingSet := False;
      fTargetBitmap.EndUpdate;
      fTargetBitmap.Changed;
      fToolbar.EndUpdateImg; // calls changed internally
    end;
  end;

  if not HyperSpeed then begin
    DrawAnimatedObjects;
    DrawLemmings;
    DrawReplayCursorCheck;
    DrawMessages;
    DrawToolBar;
    DrawMiniMap;
    DrawDebug;
  end;

  CheckForReplayAction;
  CheckForPlaySoundEffect;

  // force update if raw explosion pixels drawn
  if fExplodingPixelsUpdateNeeded then begin
    fTargetBitmap.Changed;
    fExplodingPixelsUpdateNeeded := False;
  end;

end;

procedure TLemmingGame.UpdateMessages;
begin
  if HyperSpeed then
    Exit;
  if GameOptions.HighResolutionGameMessages then begin
    HighResolutionLayer.UpdateMessages;
  end
  else begin
    if MessageList.IsEmpty then
      Exit;
    for var Msg: TLowResolutionMessage in MessageList do begin
      Msg.NextFrame;
    end;
  end;
end;

procedure TLemmingGame.IncrementIteration;
begin
  Inc(fCurrentIteration);

  // pause glitch
  if (fGlitchPauseIterations <= 0) or not (TMechanic.PauseGlitch in fMechanics) then
    Inc(fClockFrame)
  else if fGlitchPauseIterations > 0 then
    Dec(fGlitchPauseIterations);

  if fParticleFinishTimer > 0 then
    Dec(fParticleFinishTimer);

  if fClockFrame >= 17 then begin
    fClockFrame := 0;
    Dec(Seconds);
    if Seconds < 0 then begin
      if Minutes > 0 then begin
        Dec(Minutes);
        Seconds := 59;
      end
      else begin
        Minutes := 0;
        Seconds := 0;
      end;
    end;
  end
  else if fClockFrame = 1 then begin
    if fToolbar <> nil then begin
      fToolbar.SetInfoMinutes(Minutes);
      fToolbar.SetInfoSeconds(Seconds);
    end;
 end;

  // hard coded dos frame numbers
  case CurrentIteration of
    15: if not IsPaused then CueSoundEffect(SoundData.SFX_LETSGO); // todo: rewind pause thingy
    34: if not IsPaused then CueSoundEffect(SoundData.SFX_ENTRANCE); // todo: rewind pause thingy
    35:
      begin
        EntrancesOpened := True;
        for var entrance: TInteractiveObjectInfo in Entrances do begin
          entrance.Triggered := True;
          entrance.CurrentFrame := 1; // #EL 2020 corrected from 2 to 1
        end;
      end;
    55:
      begin
        // prevent restart music after rewind
        if fStartupMusicAfterEntrance then begin
          fStartupMusicAfterEntrance := False;
          // prevent restart music if the music already started by user
          if (TSoundOption.Music in fSoundOpts) and not fSoundMgr.MusicIsPlaying[MUSIC_INDEX] then
            fSoundMgr.PlayMusic(MUSIC_INDEX);
        end;
      end;
  end;

end;

//procedure TLemmingGame.DrawInitialStatics;
//// draw the level info at startup
//begin
//  {$ifdef paranoid} Assert(Assigned(fToolbar)); {$endif}
//  fToolbar.DrawSkillCount(TSkillPanelButton.Slower, Level.Info.ReleaseRate);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Faster, Level.Info.ReleaseRate);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Climber, Level.Info.ClimberCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Umbrella, Level.Info.FloaterCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Explode, Level.Info.BomberCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Blocker, Level.Info.BlockerCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Builder, Level.Info.BuilderCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Basher, Level.Info.BasherCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Miner, Level.Info.MinerCount);
//  fToolbar.DrawSkillCount(TSkillPanelButton.Digger, Level.Info.DiggerCount);
//end;

procedure TLemmingGame.RepaintSkillPanelButtons;
// draw after hyperspeed
begin
  if fHyperSpeed then
    Exit;

  fToolbar.DrawSkillCount(TSkillPanelButton.Slower, CurrReleaseRate);
  fToolbar.DrawSkillCount(TSkillPanelButton.Faster, CurrReleaseRate);
  fToolbar.DrawSkillCount(TSkillPanelButton.Climber, CurrClimberCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Umbrella, CurrFloaterCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Explode, CurrBomberCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Blocker, CurrBlockerCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Builder, CurrBuilderCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Basher, CurrBasherCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Miner, CurrMinerCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Digger, CurrDiggerCount);

  for var skill : TSkillPanelButton := TSkillPanelButton.Climber to TSkillPanelButton.Digger do
    fToolbar.DrawButtonSelector(skill, skill = fSelectedSkill);
end;

function TLemmingGame.PrioritizedHitTest(out Lemming1, Lemming2: TLemming; const CP: TPoint; CheckRightMouseButton: Boolean): Integer;
{-------------------------------------------------------------------------------
  meant for both prioritized processskillassignment and hittest.
  returns number of hits.
-------------------------------------------------------------------------------}
var
  L, PrioritizedLemming, NonPrioritizedLemming: TLemming;
  x, y: Integer;
const
  prioActions = ACTION_BIT_BLOCKING or ACTION_BIT_BUILDING or ACTION_BIT_SHRUGGING or ACTION_BIT_BASHING or ACTION_BIT_MINING or ACTION_BIT_DIGGING or ACTION_BIT_OHNOING;
begin
  Result := 0;
  PrioritizedLemming := nil;
  NonPrioritizedLemming := nil;
  Lemming1 := nil;
  Lemming2 := nil;

  for L in fLemmingList do begin
    if L.IsRemoved then
      Continue;
    x := L.XPos + L.FrameLeftDx;
    y := L.YPos + L.FrameTopDy;
    if (x <= CP.X) and (CP.X <= x + 12) and (y <= CP.Y) and (CP.Y <= y + 12) then begin
      Inc(Result);
      if L.ActionIn(prioActions) then
        PrioritizedLemming := L
      else
        NonPrioritizedLemming := L;
    end;
  end; // for

  if NonPrioritizedLemming <> nil then
    fLastNonPrioritizedLemming := NonPrioritizedLemming; // save for RightClickGlitch emulation

  if (RightMouseButtonHeldDown and CheckRightMouseButton) or (PrioritizedLemming = nil)
  then Lemming1 := NonPrioritizedLemming
  else Lemming1 := PrioritizedLemming;

  Lemming2 := NonPrioritizedLemming;
end;

procedure TLemmingGame.HitTest;
var
  HitCount: Integer;
  Lemming1, Lemming2: TLemming;
  txt: string;
begin
  HitCount := PrioritizedHitTest(Lemming1, Lemming2, CursorPoint, True);
  if (HitCount > 0) and (Lemming1 <> nil) then
  begin
    // get highlight text
    if Lemming1.IsClimber and Lemming1.IsFloater then
      txt := gt.SAthlete
    else if Lemming1.IsClimber then
      txt := gt.SClimber
    else if Lemming1.IsFloater then
      txt := gt.SFloater
    else
      txt := gt.LemmingActionStrings[Lemming1.Action];

    fToolbar.SetInfoCursorLemming(txt, HitCount);
    fCurrentCursor := GAME_CURSOR_LEMMING;
  end
  // no hit
  else begin
    if fReplaying then begin
      if not fIsLastRecordedRecordReached or (fGlitchPauseIterations > 0) then
        fToolbar.SetInfoAlternative(gt.SGame_ToolBar_Replaying)
      else
        fToolbar.SetInfoAlternative(gt.SGame_ToolBar_Replayed);
    end
    else
      fToolbar.SetInfoCursorLemming(string.Empty, 0);
    fCurrentCursor := GAME_CURSOR_DEFAULT;
  end;
end;

function TLemmingGame.ProcessSkillAssignment(checkRegainControl: Boolean): TLemming;
var
  act: TLemmingAction;
  Lemming1, Lemming2: TLemming;
begin
  Result := nil;

  // convert selected skillpanel buttontype to action we have to assign
  act := SkillPanelButtonToAction[fSelectedSkill];
  {$ifdef paranoid} Assert(act <> TLemmingAction.None); {$endif}

  if PrioritizedHitTest(Lemming1, Lemming2, CursorPoint, True) > 0 then begin

    if TMechanic.RightClickGlitch in fMechanics then begin // this is the RightClickGlitch emulation
      if Lemming1 = nil then begin
        Lemming1 := fLastNonPrioritizedLemming;
        fAssignmentIsRightClickGlitch := True;
      end;
    end;

    if Lemming1 <> nil then
      if checkRegainControl and fReplaying then
        RegainControl;
      Result := AssignSkill(Lemming1, Lemming2, act);
  end;
end;

function TLemmingGame.ReplaySkillAssignment(item: TReplayItem): Boolean;
var
  storedLemming: TLemming;
  assignedAction: TLemmingAction;
  checkSel: TSkillPanelButton;
begin
  if (item.LemmingIndex < 0) or (item.LemmingIndex >= fLemmingList.Count) then begin
    RegainControl;
    Speak(TVoiceOption.ReplayFail, True);
    AddFeedbackMessage('fail lem at ' + CurrentIteration.ToString);
    Exit(False);
  end;

  storedLemming := fLemmingList[item.LemmingIndex];

  {$ifdef paranoid}
  Assert(item.AssignedSkill > 0);
  Assert(item.AssignedSkill <= 19);
  {$endif}
  assignedAction := TLemmingAction(item.AssignedSkill);

  if not (assignedAction in AssignableSkills) then begin
    RegainControl;
    Speak(TVoiceOption.ReplayFail, True);
    AddFeedbackMessage('fail skill at ' + CurrentIteration.ToString);
    Exit(False);
  end;

  if assignedAction in AssignableSkills then begin
    // for antiques but nice (and just to be sure the right skill is selected in the skillpanel)
    checkSel := ActionToSkillPanelButton[assignedAction];
    if checkSel <> fSelectedSkill then begin
      BtnInternalSelectOnly(checkSel);
    end;

    if item.Flags and rf_UseLemming2 = 0 then
      AssignSkill(storedLemming, nil, assignedAction)
    else
      AssignSkill(storedLemming, storedLemming, assignedAction);

    Inc(fReplayMessageCounter);
    if fReplayMessageCounter >= MESSAGE_COLOR_COUNT then
      fReplayMessageCounter := 0;
    if not HyperSpeed then begin
      // some very old replays (0.0.7.0 and before that) did not store cursorposition, and there is crap inside it

      if GameOptions.ShowReplayCursor then begin
        if item.HasValidCursorData then
          DrawReplayCursor(Point(item.CursorX, item.CursorY))
        else
          DrawReplayCursor(Point(storedlemming.XPos + storedLemming.FrameLeftDx + 7, storedlemming.YPos + storedLemming.FrameTopDy + 7)); // best guess position
      end;

      if GameOptions.ShowPhotoFlashReplayEffect then
        storedLemming.PhotoFlashForReplay := True;
      if item.Flags and rf_RightMouseGlitch = 0 then
        AddReplayMessage(storedLemming, assignedAction)
      else
        AddReplayMessage(storedLemming, assignedAction, 'glitch'); // show it was the glitch
    end;

    // check if the lemming is in sync with the replay command
    if (item.LemmingX > 0) and (item.LemmingY > 0)
    and ((item.LemmingX <> storedLemming.XPos) or (item.LemmingY <> storedLemming.YPos)) then begin
      RegainControl;
      Speak(TVoiceOption.ReplayFail, True);
      AddFeedbackMessage('fail pos at ' + CurrentIteration.ToString);
      Exit(False);
    end;
  end;
  Result := True;
end;

procedure TLemmingGame.ReplaySkillSelection(aReplayItem: TReplayItem);
var
  button: TSkillPanelButton;
begin
  button := TSkillPanelButton(aReplayItem.SelectedButton); // convert
  BtnInternalSelectOnly(button);
end;

procedure TLemmingGame.BtnSlower(minimize: Boolean);
// minimize only possible when paused
begin
  if not minimize then begin
    if fReleaseRateStatus <> TReleaseRateStatus.SlowingDown then // todo: maybe this check can disappear
      RecordReleaseRate(raf_StartDecreaseRR);
    fReleaseRateStatus := TReleaseRateStatus.SlowingDown;
  end
  else if IsPaused then begin
    if fReleaseRateStatus <> TReleaseRateStatus.SlowingDown then // todo: maybe this check can disappear
      RecordReleaseRate(raf_StartDecreaseRR);
    CurrReleaseRate := Level.Info.ReleaseRate;
    RecordReleaseRate(raf_StopChangingRR);
    if not HyperSpeed then fToolBar.DrawSkillCount(TSkillPanelButton.Faster, CurrReleaseRate);
    fReleaseRateStatus := TReleaseRateStatus.None;
  end;
end;

//procedure TLemmingGame.BtnSlowerStop;
//begin
//  if fReleaseRateStatus = TReleaseRateStatus.SlowingDown then begin
//    fReleaseRateStatus := TReleaseRateStatus.None;
//    RecordReleaseRate(raf_StopChangingRR);
//  end;
//end;

procedure TLemmingGame.BtnFaster(maximize: Boolean);
// maximize only possible when paused
begin
  if not maximize then begin
    if fReleaseRateStatus <> TReleaseRateStatus.SpeedingUp then // todo: maybe this check can disappear
      RecordReleaseRate(raf_StartIncreaseRR);
    fReleaseRateStatus := TReleaseRateStatus.SpeedingUp;
  end
  else if IsPaused then begin
    if fReleaseRateStatus <> TReleaseRateStatus.SpeedingUp then // todo: maybe this check can disappear
      RecordReleaseRate(raf_StartIncreaseRR);
    CurrReleaseRate := 99;
    RecordReleaseRate(raf_StopChangingRR);
    if not HyperSpeed then fToolBar.DrawSkillCount(TSkillPanelButton.Faster, CurrReleaseRate);
//    fToolBar.RefreshInfo;
    fReleaseRateStatus := TReleaseRateStatus.None;
  end;
end;

//procedure TLemmingGame.BtnFasterStop;
//begin
//  if fReleaseRateStatus = TReleaseRateStatus.SpeedingUp then begin
//    fReleaseRateStatus := TReleaseRateStatus.None;
//    RecordReleaseRate(raf_StopChangingRR);
//  end;
//end;

procedure TLemmingGame.BtnStopChangingReleaseRate;
begin
  if fReleaseRateStatus <> TReleaseRateStatus.None then begin
    fReleaseRateStatus := TReleaseRateStatus.None;
    RecordReleaseRate(raf_StopChangingRR);
  end;
end;

procedure TLemmingGame.BtnClimber;
begin
  if fSelectedSkill = TSkillPanelButton.Climber then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Climber);
  fSelectedSkill := TSkillPanelButton.Climber;
  RecordSkillSelection(TSkillPanelButton.Climber);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnUmbrella;
begin
  if fSelectedSkill = TSkillPanelButton.Umbrella then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Umbrella);
  fSelectedSkill := TSkillPanelButton.Umbrella;
  RecordSkillSelection(TSkillPanelButton.Climber);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnExplode;
begin
  if fSelectedSkill = TSkillPanelButton.Explode then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Explode);
  fSelectedSkill := TSkillPanelButton.Explode;
  RecordSkillSelection(TSkillPanelButton.Explode);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnBlocker;
begin
  if fSelectedSkill = TSkillPanelButton.Blocker then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Blocker);
  fSelectedSkill := TSkillPanelButton.Blocker;
  RecordSkillSelection(TSkillPanelButton.Blocker);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnBuilder;
begin
  if fSelectedSkill = TSkillPanelButton.Builder then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Builder);
  fSelectedSkill := TSkillPanelButton.Builder;
  RecordSkillSelection(TSkillPanelButton.Builder);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnBasher;
begin
  if fSelectedSkill = TSkillPanelButton.Basher then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Basher);
  fSelectedSkill := TSkillPanelButton.Basher;
  RecordSkillSelection(TSkillPanelButton.Basher);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnMiner;
begin
  if fSelectedSkill = TSkillPanelButton.Miner then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Miner);
  fSelectedSkill := TSkillPanelButton.Miner;
  RecordSkillSelection(TSkillPanelButton.Miner);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnDigger;
begin
  if fSelectedSkill = TSkillPanelButton.Digger then
    Exit;
  if not HyperSpeed then
    fToolbar.SwitchButtonSelector(fSelectedSkill, TSkillPanelButton.Digger);
  fSelectedSkill := TSkillPanelButton.Digger;
  RecordSkillSelection(TSkillPanelButton.Digger);
  CueSoundEffect(SoundData.SFX_SKILLBUTTON);
end;

procedure TLemmingGame.BtnPause(mode: TPauseCommandMode = TPauseCommandMode.None);
begin
  if IsPaused then
    Exit;

  fIsPaused := True;
  fIsPausedExt := mode = TPauseCommandMode.PauseKey;
  if GameOptions.HighlightedPauseButton then
    ToolBar.SetPauseHighlight(true);
  FastForward := False;
//  fReleaseRateStatus := TReleaseRateStatus.None; // todo: bug in progress

  if (CurrentIteration < 34) and (TMechanic.PauseGlitch in Mechanics) then begin
    // regain control if pausing *earlier* than the current replay (maybe optional)
    if fReplaying then begin
      var iteration: Integer := 34 - fRecorder.fRecordedGlitchPauseIterations;
      if CurrentIteration < iteration then begin
        fGlitchPauseIterations := 34 - CurrentIteration;
        fRecorder.fRecordedGlitchPauseIterations := fGlitchPauseIterations; // record it
        RegainControl;
      end;
    end
    // start pause keep track of glitch iterations
    else begin
      if fGlitchPauseIterations = 0 then begin
        fGlitchPauseIterations := 34 - CurrentIteration;
        fRecorder.fRecordedGlitchPauseIterations := fGlitchPauseIterations; // record it
      end;
    end;

  end;

  RecordStartPause;
end;

procedure TLemmingGame.BtnPauseStop;
begin
  if not fIsPaused then
    Exit;

  fIsPaused := False;
  fIsPausedExt := False;
  FastForward := False;

  if GameOptions.HighlightedPauseButton then
    ToolBar.SetPauseHighlight(false);

  RecordEndPause; // record
end;

procedure TLemmingGame.BtnTogglePause(mode: TPauseCommandMode = TPauseCommandMode.None);
begin
  if not fIsPaused then
    BtnPause(mode)
  else
    BtnPauseStop;
end;

procedure TLemmingGame.BtnNuke;
begin
  if fIsNukedByUser then
    Exit;
  // next line of code is the NUKE GLITCH. Changing MaxNumLemmings also allows IN % to be calculated and displayed in-game using the glitch calculation,
  // just like the actual game
  if TMechanic.NukeGlitch in fMechanics then
    MaxNumLemmings := LemmingsReleased;
  fIsNukedByUser := True;
  fIsExploderAssignInProgress := True;
  RecordNuke;
  CueSoundEffect(SoundData.SFX_NUKE);
end;

procedure TLemmingGame.CheckSpawnLemming;

    function CalculateNextLemmingCountdown: Integer;
    // ccexplore:
    // All I know is that in the DOS version, the formula is that for a given RR,
    // the number of frames from one release to the next is:
    // (99 - RR) / 2 + 4
    // Where the division is the standard truncating integer division
    // (so for example RR 99 and RR 98 acts identically).
    //
    // This means for example, at RR 99,
    // it'd be release, wait, wait, wait, release, wait, wait, wait, release,
    //
    // I don't know what the frame rate is though on the DOS version,
    // although to a large extent this mostly does not matter, since most aspects
    // of the game mechanics is based off of number of frames rather than absolute time.
    begin
      Result := 99 - currReleaseRate;
      if Result < 0 then
        Inc(Result, 256);
      Result := Result div 2 + 4;
    end;

var
  NewLemming: TLemming;
  ix, EntranceIndex: Integer;
begin
  if not EntrancesOpened or fIsNukedByUser then
    Exit;

  // NextLemmingCountdown is initialized to 20 before start of a level
  Dec(NextLemmingCountdown);

  if NextLemmingCountdown = 0 then begin
    NextLemmingCountdown := CalculateNextLemmingCountdown;
    if LemmingsReleased < MaxNumLemmings then begin
      if Entrances.Count = 0 then
        Exit; // prevent error when there are no entrances
      EntranceIndex := LemmingsReleased mod 4;
      ix := DosEntranceOrderTable[EntranceIndex];
      NewLemming := TLemming.Create;
      NewLemming.PixelCombine := CombineLemming;
      NewLemming.ListIndex := fLemmingList.Add(NewLemming);
      NewLemming.Born := CurrentIteration;
      Transition(NewLemming, TLemmingAction.Falling);
      NewLemming.XPos := Entrances[ix].Obj.Left + 24;
      // @Optional Game Mechanic
      if TMechanic.EntranceX25 in Mechanics then
        Inc(NewLemming.XPos);
      NewLemming.YPos := Entrances[ix].Obj.Top + 14;
      NewLemming.xDelta := 1;
      // these must be initialized to nothing
      NewLemming.ObjectInFront := DOM_NONE;
      NewLemming.ObjectBelow := DOM_NONE;
      Inc(LemmingsReleased);
      Inc(LemmingsOut);
    end;
  end;
end;

procedure TLemmingGame.CheckUpdateNuking;
var
  CurrentLemming: TLemming;
begin
  if fIsNukedByUser and fIsExploderAssignInProgress then begin
    // find first following non removed lemming
    while (fIndexOfLemmingToBeNuked < LemmingsReleased)
    and (fLemmingList[fIndexOfLemmingToBeNuked].IsRemoved) do
      Inc(fIndexOfLemmingToBeNuked);

    if (fIndexOfLemmingToBeNuked > LemmingsReleased - 1) then
      fIsExploderAssignInProgress := False
    else begin
      CurrentLemming := fLemmingList[fIndexOfLemmingToBeNuked];
      if (CurrentLemming.ExplosionTimer = 0) and not (CurrentLemming.ActionIn(ACTION_BIT_SPLATTING or ACTION_BIT_EXPLODING)) then
        CurrentLemming.ExplosionTimer := 79;
      Inc(fIndexOfLemmingToBeNuked);
    end;
  end;
end;

procedure TLemmingGame.DeveloperCreateLemmingAtCursorPoint;
// god modus, debugging procedure: click and create lemming
var
  NewLemming: TLemming;
begin
  if not EntrancesOpened or fIsNukedByUser then
    Exit;
  if LemmingsReleased < MaxNumLemmings then begin
    NewLemming := TLemming.Create;
    NewLemming.ListIndex := fLemmingList.Add(NewLemming);
    NewLemming.Born := CurrentIteration;
    NewLemming.PixelCombine := CombineLemming;
    Transition(NewLemming, TLemmingAction.Falling);
    NewLemming.XPos := CursorPoint.X;
    NewLemming.YPos := CursorPoint.Y;
    NewLemming.xDelta := 1;
    // these must be initialized to nothing
    NewLemming.ObjectInFront := DOM_NONE;
    NewLemming.ObjectBelow := DOM_NONE;
    Inc(LemmingsReleased);
    Inc(LemmingsOut);
  end;
end;

procedure TLemmingGame.Developer99Skills;
// god modus, all skills at maximum
begin
  CurrClimberCount  := 99;
  CurrFloaterCount  := 99;
  CurrBomberCount   := 99;
  CurrBlockerCount  := 99;
  CurrBuilderCount  := 99;
  CurrBasherCount   := 99;
  CurrMinerCount    := 99;
  CurrDiggerCount   := 99;
  fToolbar.DrawSkillCount(TSkillPanelButton.Climber, CurrClimberCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Umbrella, CurrFloaterCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Explode, CurrBomberCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Blocker, CurrBlockerCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Builder, CurrBuilderCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Basher, CurrBasherCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Miner, CurrMinerCount);
  fToolbar.DrawSkillCount(TSkillPanelButton.Digger, CurrDiggerCount);
end;

procedure TLemmingGame.CheckForPlaySoundEffect;
begin
  if HyperSpeed then
    Exit;
  if fSoundsToPlay.Count = 0 then
    Exit;
  for var i: integer in fSoundsToPlay do
    fSoundMgr.PlaySound(i);
  fSoundsToPlay.Clear;
end;

procedure TLemmingGame.CueSoundEffect(aSoundId: Integer);
// save last sound.
begin
  if (aSoundId < 0) or HyperSpeed or not Playing or not (TSoundOption.Sound in fSoundOpts) then
    Exit;
  if fSoundsToPlay.Count < 8 then // i think we have some balance with the soundlib (overlapping sounds)
    fSoundsToPlay.Add(aSoundId);
  if IsPaused then
    CheckForPlaySoundEffect;
end;

procedure TLemmingGame.AdjustReleaseRate(Delta: Integer);
var
  N: Integer;
begin
  N := CurrReleaseRate + Delta;
  Restrict(N, Level.Info.ReleaseRate, 99);
  if N <> currReleaseRate then begin
    currReleaseRate := N;
    if not HyperSpeed then fToolbar.DrawSkillCount(TSkillPanelButton.Faster, currReleaseRate);
  end;
end;

procedure TLemmingGame.AddReplayMessage(L: TLemming; aAssignedAction: TLemmingAction; const suffix: string = string.Empty);
begin
  if not GameOptions.ShowReplayMessages then
    Exit;
  if GameOptions.HighResolutionGameMessages then begin
   // x,y is converted to screen point, when high res
    var x: Integer := L.XPos;
    var y: Integer := L.YPos + L.FrameTopDy - 8;
    if aAssignedAction = TLemmingAction.Exploding then
      Dec(y, 8);
    var p: TPoint := fImg.BitmapToControl(Point(x, y));
    inc(fMessagesPlayedCount);
    HighResolutionLayer.AddMessage(p.X, p.Y, L.XDelta, - 1, 34, gt.LemmingReplayStrings[aAssignedAction] + suffix, SelectReplayMessageTextColor);
  end
  else begin
    var Msg: TLowResolutionMessage := TLowResolutionMessage.Create(Self);
    Msg.Duration := 32;
    Msg.fLocation := Point(L.GetLocationBounds.Left, L.GetLocationBounds.Top - 8);
    Msg.DeltaX := L.xDelta;
    Msg.SetText(gt.LemmingReplayStrings[aAssignedAction] + suffix, SelectReplayMessageTextColor);
    inc(fMessagesPlayedCount);
    MessageList.Add(Msg);
  end;
end;

procedure TLemmingGame.AddFeedbackMessage(const s: string);
begin
  if not GameOptions.ShowFeedbackMessages then
    Exit;
  if GameOptions.HighResolutionGameMessages then begin
    var x: Integer := CursorPoint.X;
    var y: Integer := GAME_BMPHEIGHT div 3;
    var p: TPoint := fImg.BitmapToControl(Point(x, y));
    inc(fMessagesPlayedCount);
    HighResolutionLayer.AddMessage(p.X, p.Y, 0, - 1, 34, s, SelectFeedbackMessageTextColor{(fMessagesPlayedCount)});
  end
  else begin
    var Msg: TLowResolutionMessage := TLowResolutionMessage.Create(Self);
    Msg.Duration := 32;
    Msg.DeltaX := 0;
    Msg.SetText(s, SelectFeedbackMessageTextColor{(fMessagesPlayedCount)});
    Msg.fLocation := Point(CursorPoint.X - Msg.fBuffer.Width div 2, 32);
    inc(fMessagesPlayedCount);
    MessageList.Add(Msg);
  end;
end;

procedure TLemmingGame.RecordStartPause;
{-------------------------------------------------------------------------------
  Records the start of a pause session.
  Just in case: when the previous record is raf_Pausing or raf_StartPause
  we do *not* record it.
-------------------------------------------------------------------------------}

    function PrevOk: Boolean;
    var
      last: TReplayItem;
    begin
       last := fRecorder.List.LastOrDefault;
       if not Assigned(last) then
         Result := True
       else
         Result := last.ActionFlags and (raf_Pausing or raf_StartPause) = 0;
    end;

var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying or not PrevOk then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := raf_StartPause;
  R.ReleaseRate := CurrReleaseRate;
end;

procedure TLemmingGame.RecordEndPause;
{-------------------------------------------------------------------------------
  Recording the end of a pause.
  Just in case: this is only allowed if there is a startpause-counterpart.
  if there is a previous record then this previous record has to
  have a flag raf_Pausing or raf_StartPause.
  In all other cases EndPause is *not* recorded.
-------------------------------------------------------------------------------}

    function PrevOk: Boolean;
    var
      last: TReplayItem;
    begin
      last := fRecorder.List.LastOrDefault;
      if not Assigned(last) then
        Result := True
      else
        Result := last.ActionFlags and (raf_Pausing or raf_StartPause) <> 0;
    end;

var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying or not PrevOk then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := raf_EndPause;
  R.ReleaseRate := currReleaseRate;
end;

procedure TLemmingGame.RecordNuke;
{-------------------------------------------------------------------------------
  Easy one: Record nuking. Always add new record.
-------------------------------------------------------------------------------}
var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := R.ActionFlags or raf_Nuke;
  // Just in case: nuking is normally not possible when pausing.
  // but it does no harm setting the raf_Pause flag
  if IsPaused then
    R.ActionFlags := R.ActionFlags or raf_Pausing;
  R.ReleaseRate := currReleaseRate;
end;

procedure TLemmingGame.RecordReleaseRate(aActionFlag: Byte);
{-------------------------------------------------------------------------------
  This is a tricky one. It can be done when pausing and when not pausing.
  Records a releaserate change command so the only valid parameters are:
    • raf_StartIncreaseRR,
    • raf_StartDecreaseRR,
    • raf_StopChangingRR
-------------------------------------------------------------------------------}
var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying then
    Exit;
  {$ifdef paranoid} Assert(aActionFlag in [raf_StartIncreaseRR, raf_StartDecreaseRR, raf_StopChangingRR]); {$endif}

  if fIsPaused then begin
    R := Recorder.List.LastOrDefault;
    // if empty add new reocrd
    // never overwrite a startpause, so create a new one as well
    if not Assigned(R) or (R.ActionFlags and raf_StartPause <> 0) then
      R := Recorder.add;
  end
  else
    // not paused, always create new record
    R := Recorder.Add;

  R.Iteration := CurrentIteration;
  R.ReleaseRate := CurrReleaseRate;
  R.ActionFlags := R.ActionFlags or aActionFlag;
  if IsPaused then
    R.ActionFlags := R.ActionFlags or raf_Pausing;
end;

procedure TLemmingGame.RecordSkillAssignment(L: TLemming; aSkill: TLemmingAction; usedLemming2, rightMouseGlitched: Boolean);
//  Always add new record.
var
  item: TReplayItem;
begin
  if not fPlaying or fReplaying then
    Exit;

  item := Recorder.Add;

  item.Iteration := CurrentIteration;
  item.ActionFlags := raf_SkillAssignment;

  // assignment is possible during pause, although we should refrain from it
  if IsPaused then
    item.ActionFlags := item.ActionFlags or raf_Pausing;

  item.LemmingIndex := L.ListIndex;
  item.ReleaseRate := CurrReleaseRate;
  item.AssignedSkill := Byte(aSkill); // the byte is "compatible" for now
  item.LemmingX := L.XPos;
  item.LemmingY := L.YPos;
  item.CursorX := CursorPoint.X;
  item.CursorY := CursorPoint.Y;
  if usedLemming2 then
    item.Flags := item.Flags or rf_UseLemming2;
  if rightMouseGlitched then
    item.Flags := item.Flags or rf_RightMouseGlitch;
end;

procedure TLemmingGame.RecordSkillSelection(aSkill: TSkillPanelButton);
var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying then
    Exit;
  {$ifdef paranoid}
  Assert(askill in [TSkillPanelButton.Climber, TSkillPanelButton.Umbrella, TSkillPanelButton.Explode, TSkillPanelButton.Blocker, TSkillPanelButton.Builder, TSkillPanelButton.Basher, TSkillPanelButton.Miner, TSkillPanelButton.Digger]);
  {$endif}

  if IsPaused then begin
    {$ifdef paranoid}  Assert(Recorder.List.Count > 0); {$endif}
    R := Recorder.List.LastOrDefault; //Items[Recorder.List.Count - 1];
    // never overwrite startpause
    if not Assigned(R) or (R.ActionFlags and raf_StartPause <> 0) then
      R := Recorder.Add;
  end
  else
    R := Recorder.Add;

  R.Iteration := CurrentIteration;
  R.ActionFlags := R.ActionFlags or raf_SkillSelection;
  if IsPaused then
    R.ActionFlags := R.ActionFlags or raf_Pausing;

  R.ReleaseRate := CurrReleaseRate;

  case aSkill of
    TSkillPanelButton.Climber  : R.SelectedButton := rsb_Climber;
    TSkillPanelButton.Umbrella : R.SelectedButton := rsb_Umbrella;
    TSkillPanelButton.Explode  : R.SelectedButton := rsb_Explode;
    TSkillPanelButton.Blocker  : R.SelectedButton := rsb_Stopper;
    TSkillPanelButton.Builder  : R.SelectedButton := rsb_Builder;
    TSkillPanelButton.Basher   : R.SelectedButton := rsb_Basher;
    TSkillPanelButton.Miner    : R.SelectedButton := rsb_Miner;
    TSkillPanelButton.Digger   : R.SelectedButton := rsb_Digger;
  end;

end;

procedure TLemmingGame.CheckForReplayAction;
// all records with the same iterationnumber must be handled here in one atomic moment.
var
  R: TReplayItem;
  Last: Integer;
begin
  if not fReplaying then
    Exit;
  Last := fRecorder.List.Count - 1;

  if fReplayIndex > Last then
    fIsLastRecordedRecordReached := True;

  // although it may not be possible to have 2 replay-actions at one iteration we use a while loop: it's the safest method
  // note: since 2.1.0 skill assignments are optionally possible during pause, so indeed we can have more than one action during a frame.
  while fReplayIndex <= Last do begin
    R := fRecorder.List[fReplayIndex];
    // break if we go beyond the current iteration
    if R.Iteration <> CurrentIteration then
      Break;

    if raf_Nuke and r.actionflags <> 0 then
      BtnNuke;
    if raf_skillassignment and r.actionflags <> 0 then
      if not ReplaySkillAssignment(R) then
        Exit;
    if raf_skillselection and r.actionflags <> 0 then
      ReplaySkillSelection(R);
    if raf_stopchangingRR and r.actionflags <> 0 then begin
      BtnStopChangingReleaseRate;
      if (R.ReleaseRate <> 0) and (R.ReleaseRate <> CurrReleaseRate) then
         AdjustReleaseRate(R.ReleaseRate - currReleaseRate);
    end
    else if raf_StartIncreaseRR and r.actionflags <> 0 then
      BtnFaster(False)
    else if raf_StartDecreaseRR and r.actionflags <> 0 then
      BtnSlower(False);

    // check for changes (on fail we regain control)
    if not fReplaying then
      Exit;

    // double check
    if (R.ReleaseRate > 0) and (R.ReleaseRate <> CurrReleaseRate) then
      AdjustReleaseRate(R.ReleaseRate - currReleaseRate);

    Inc(fReplayIndex);
  end; // while
end;

procedure TLemmingGame.CheckLemmings;
var
  CurrentLemming: TLemming;
  HandleInteractiveObjects: Boolean;
  CountDownReachedZero: Boolean;
begin
  if fLemmingList.IsEmpty then
    Exit;

  for CurrentLemming in fLemmingList do begin
    CountDownReachedZero := False;
    // @particles
    if CurrentLemming.ParticleTimer > 0 then begin
      Dec(CurrentLemming.ParticleTimer);
      if CurrentLemming.ParticleFrame < 50 then // #EL this solves the particle erase error
        Inc(CurrentLemming.ParticleFrame);
    end;
    if CurrentLemming.IsRemoved then
      Continue;
    if CurrentLemming.ExplosionTimer <> 0 then
      CountDownReachedZero := UpdateExplosionTimer(CurrentLemming);
    if CountDownReachedZero then
      Continue;
    HandleInteractiveObjects := HandleLemming(CurrentLemming);
    if HandleInteractiveObjects then
      CheckForInteractiveObjects(CurrentLemming);
  end;
end;

procedure TLemmingGame.SetGameResult;
{-------------------------------------------------------------------------------
  We will not, I repeat *NOT* simulate the original Nuke-error.
  #EL 2020: Ok ccexplore: here it is.
-------------------------------------------------------------------------------}
begin
  GameResultRec.Cheated := fIsCheated;

  if TMechanic.NukeGlitch in fMechanics
  then GameResultRec.LemmingCount := MaxNumLemmings // this is the glitch
  else GameResultRec.LemmingCount := Level.Info.LemmingsCount;

  GameResultRec.ToRescue := Level.Info.RescueCount;
  GameResultRec.Rescued := LemmingsSaved;

  if Level.Info.LemmingsCount = 0
  then GameResultRec.Target := 0
  else GameResultRec.Target := (GameResultRec.ToRescue * 100) div Level.Info.LemmingsCount;


  if GameResultRec.LemmingCount = 0
  then GameResultRec.Done := 0
  else GameResultRec.Done := (GameResultRec.Rescued * 100) div GameResultRec.LemmingCount;

  if GameResultRec.Cheated then begin
    GameResultRec.Done := 100;
    GameResultRec.Rescued := GameResultRec.LemmingCount;
  end;

  GameResultRec.Success := GameResultRec.Done >= GameResultRec.Target;
end;

procedure TLemmingGame.RegainControl;
//  Key routine. It jumps from replay into usercontrol.
begin
  if fReplaying then begin
    fReplaying := False;
    fRecorder.Truncate(fReplayIndex);
    // special case: if the game is paused and the control is regained we have to append the lost (truncated) startpause record.
    if IsPaused then
      RecordStartPause;
    fReplayIndex := 0;
  end;
end;

procedure TLemmingGame.HyperSpeedBegin;
begin
  if not fHyperSpeed then begin
    fHyperSpeed := True;
    FastForward := False;
    if not fTargetBitmapsUpdatingSet then begin
      fTargetBitmapsUpdatingSet := True;
      fTargetBitmap.BeginUpdate;
      fToolbar.BeginUpdateImg;
    end;
  end;
end;

procedure TLemmingGame.HyperSpeedEnd;
begin
  if fHyperSpeed then
   fHyperspeed := False;
end;

procedure TLemmingGame.UpdateInteractiveObjects;
{-------------------------------------------------------------------------------
  This method handles the updating of the moving interactive objects:
  • Entrances moving
  • Continuously moving objects like water
  • Triggered objects (traps)
  It does not handle the drawing
-------------------------------------------------------------------------------}
var
  Inf: TInteractiveObjectInfo;
begin

  // moving entrances?
  if not fEntranceAnimationCompleted and (CurrentIteration >= 35) then begin
    for Inf in Entrances do begin
      if Inf.Triggered then begin
        Inc(Inf.CurrentFrame);
        if Inf.CurrentFrame >= Inf.MetaObj.AnimationFrameCount then begin
          Inf.CurrentFrame := 0;
          Inf.Triggered := False;
          fEntranceAnimationCompleted := True;
        end;
      end;
    end;
  end;

  // other objects
  for Inf in ObjectInfos do begin
    if Inf.Triggered or (Inf.MetaObj.AnimationType = oat_Continuous) then
      Inc(Inf.CurrentFrame);
    if Inf.CurrentFrame >= Inf.MetaObj.AnimationFrameCount then begin
      Inf.CurrentFrame := 0;
      Inf.Triggered := False;
    end;
  end;
end;

procedure TLemmingGame.CheckAdjustReleaseRate;
begin
  case fReleaseRateStatus of
    TReleaseRateStatus.SlowingDown:
      begin
        AdjustReleaseRate(-1);
      end;
    TReleaseRateStatus.SpeedingUp:
      begin
        AdjustReleaseRate(1);
      end;
  end;
end;

procedure TLemmingGame.SetSoundOpts(const Value: TSoundOptions);
begin
  if fSoundOpts = Value then
    Exit;
  fSoundOpts := Value;
  if not (TSoundOption.Music in fSoundOpts) then
    fSoundMgr.StopMusic(MUSIC_INDEX)
  else
    fSoundMgr.PlayMusic(MUSIC_INDEX)
end;

procedure TLemmingGame.Terminate;
begin
  fIsFinished := True;
  fSoundMgr.StopMusic(MUSIC_INDEX);
  fSoundMgr.ClearMusics;
end;


procedure TLemmingGame.Finish;
begin
  Terminate;
  if Assigned(fOnFinish) then
    fOnFinish(Self);
end;

procedure TLemmingGame.ChangeMusicVolume(up: Boolean);
begin
  var curr: Single := fSoundMgr.GetMusicVolumne(MUSIC_INDEX);
  if up and (curr <= 0.9) then
    fSoundMgr.SetMusicVolume(MUSIC_INDEX, curr + 0.1)
  else if (curr >= 0.1) then
    fSoundMgr.SetMusicVolume(MUSIC_INDEX, curr - 0.1);
end;

procedure TLemmingGame.Cheat;
begin
  fIsCheated := True;
  Finish;
end;

procedure TLemmingGame.AutoSave;
begin
  InternalSave(True, True, False);
end;

procedure TLemmingGame.Save(includeGameResult: Boolean);
begin
  InternalSave(False, includeGameResult, True);
end;

procedure TLemmingGame.InternalSave(auto, includeGameResult, displayGameMessage: Boolean);
var
  path, levelname, basename, lrbname, txtname: string;
begin
  if not auto
  then path := Consts.PathToReplay + IncludeTrailingPathDelimiter(fLevelLoadingInfo.Style.Name)
  else path := Consts.PathToAutoSave + IncludeTrailingPathDelimiter(fLevelLoadingInfo.Style.Name);

  if not ForceDir(path) then
    Exit;

  levelname := StripInvalidFileChars(fLevelLoadingInfo.GetLevelTitle, False, True, True);
  if levelname.IsEmpty then
    levelname := 'noname';

  basename := path + levelname;
  lrbname := basename + '.lrb';
  txtname := basename + '.txt';

  if not auto and not MiscOptions.AlwaysOverwriteReplayFiles and FileExists(lrbname) then begin
    var d: TDateTime := Now;
    var t: string := FormatDateTime('yyyy-mm-dd hhNNss', d);
    lrbname := basename + ' (' + t + ').lrb';
    txtname := basename + ' (' + t + ').txt';
  end;

  var doCache: Boolean := not auto and MiscOptions.UpdateReplayCacheWhenSaving;

  Recorder.SaveToFile(lrbname, doCache);
  Recorder.SaveToTxt(txtname, includeGameResult);

  if not auto then
    Speak(TVoiceOption.GameSaved, False);

  if not auto and not fIsFinished and fPlaying and displayGameMessage then begin
    AddFeedbackMessage(gt.SGame_FeedbackMessage_GameSaved);
  end;
end;

{ TReplayItem }

procedure TLemmingGame.InitializeBrickColors(aBrickPixelColor: TColor32);
var
  i: Integer;
  aR, aG, aB: Integer;
  P: PColor32Entry;
begin
  BrickPixelColor := aBrickPixelColor;

  for i := 0 to 11 do
    BrickPixelColors[i] := aBrickPixelColor;

  if not GameOptions.GradientBridges then
    Exit;

  P := @BrickPixelColor;

  ar := P^.R;
  ag := P^.G;
  ab := P^.B;

  // lighter
  for i := 7 to 11 do begin
    P := @BrickPixelColors[i];
    if aR < 252  then inc(ar,4);
    if ag < 252 then inc(ag,4);
    if ab < 252 then inc(ab,4);
    P^.r := ar;
    P^.G := ag;
    P^.B := ab;
  end;


  P := @BrickPixelColor;

  ar:=P^.R;
  ag:=P^.G;
  ab:=P^.B;

  // darker
  for i := 5 downto 0 do begin
    P := @BrickPixelColors[i];
    if aR > 3 then dec(ar, 4);
    if ag > 3 then dec(ag, 4);
    if ab > 3 then dec(ab, 4);
    P^.r := ar;
    P^.G := ag;
    P^.B := ab;
  end;

end;

{ TRecorder }

constructor TRecorder.Create(aGame: TLemmingGame);
begin
  fGame := aGame;
  List := TFastObjectList<TReplayItem>.Create;
end;

destructor TRecorder.Destroy;
begin
  List.Free;
  inherited;
end;

function TRecorder.GetIsEmpty: Boolean;
begin
  Result := List.Count = 0;
end;

function TRecorder.Add: TReplayItem;
begin
  Result := TReplayItem.create;
  List.Add(Result);
end;

procedure TRecorder.Clear;
begin
  List.Clear;
  fWasLoaded := False;
  fWasSaved := False;
end;

function TRecorder.LoadFromFile(const aFileName: string; out error: string): Boolean;
var
  stream: TBufferedFileStream;
begin
  error := string.Empty;
  Result := False;
  stream := nil;
  try
    try
      stream := TBufferedFileStream.Create(aFileName, fmOpenRead);
      Result := LoadFromStream(stream, {out} error);
    except
      on E: Exception do begin
        error := E.Message;
     end;
  end;
  finally
    stream.Free;
  end;
end;

// todo: return false or true and errormessage, no exception
function TRecorder.LoadFromStream(stream: TStream; out error: string): Boolean;
const
  deftext = 'Replay loading error: ';
var
  header: TReplayFileHeaderRec;
  rec: TReplayRec;
  item: TReplayItem;
  Iter, RR, Parity: Integer;
begin
  Result := False;
  error := string.Empty;
  header.Clear;

  if stream.Read(header, SizeOf(header)) <> SizeOf(header) then begin
    error := deftext + 'Header read error size mismatch';
    Exit;
  end;

  if header.Signature <> 'LRB' then begin
    error := deftext + 'Invalid replay header signature. this must be "LRB"';
    Exit;
  end;

  if header.FileSize <> stream.Size then begin
    error := deftext + 'Invalid replay header filesize mismatch';
    Exit;
  end;

  if header.HeaderSize <> SizeOf(TReplayFileHeaderRec) then begin
    error := deftext + 'Invalid replay header headersize mismatch';
    exit;
  end;

  if (header.ReplayRecordCount > 0) and ((header.FirstRecordPos < header.HeaderSize) or (header.FirstRecordPos >= stream.Size)) then begin
    error := deftext + 'Invalid first record position';
    Exit;
  end;

  // reserved1 and reserved2 not handled

  if (header.Version < 1)  or (header.Version > LEMMIX_REPLAY_VERSION) then begin
    error := deftext + 'Invalid replay header version (' + IntToStr(header.Version) + ').';
    exit;
  end;

  fCurrentMechanics := header.Mechanics;

  if header.Version > 1 then begin
    Restrict(header.ReplayGlitchPauseIterations, 0, 33);
    fRecordedGlitchPauseIterations := header.ReplayGlitchPauseIterations;
  end;

  Parity := 0;
  List.Clear;
  List.Capacity := header.ReplayRecordCount;
  stream.Position := header.FirstRecordPos;

  while True do begin
    // check eof reached
    if stream.Read(rec, SizeOf(TReplayRec)) <> SizeOf(TReplayRec) then
      Break;
    if rec.Check <> 'R' then begin
      error := deftext + 'Replay record check error, must be equal to "R"';
      Exit;
    end;
    item := Add;
    item.Iteration := rec.Iteration;
    item.ActionFlags := rec.ActionFlags;
    item.AssignedSkill := rec.AssignedSkill;
    item.SelectedButton := rec.SelectedButton;
    item.ReleaseRate := rec.ReleaseRate;
    item.LemmingIndex := rec.LemmingIndex;
    item.LemmingX := rec.LemmingX;
    item.LemmingY := rec.LemmingY;
    item.CursorX := rec.CursorX;
    item.CursorY := rec.CursorY;

    // some very old replays (0.0.7.0 and before that) did not store cursorposition, and there is crap inside it
    item.HasValidCursorData :=
      (item.AssignedSkill <> 0)
      and (item.CursorX > item.LemmingX - 8)
      and (item.CursorX < item.LemmingX + 12)
      and (item.CursorY > item.LemmingY - 16)
      and (item.CursorY < item.LemmingY + 12);

    if not item.HasValidCursorData then begin
      item.CursorX := 0;
      item.CursorY := 0;
    end;

    if header.Version > 2 then
      item.Flags := rec.Flags; // glitchflags were added in replay version 3

    if rec.ActionFlags = raf_StartPause then
      Inc(Parity)
    else if rec.ActionFlags = raf_Endpause then
      Dec(Parity);

    RR := rec.Releaserate;
    Iter := rec.Iteration;

    // Add fake paused record if unpaired startpause. This happens when this file was saved in paused mode!
    if (parity = 1) and (List.Count = header.ReplayRecordCount) then begin
      item := Add;
      item.Iteration := Iter;
      item.ReleaseRate := RR;
      item.ActionFlags := raf_EndPause;
      Break;
    end;
    // todo: just delete the orphan beginpause record?

    if List.Count >= header.ReplayRecordCount then
      Break;
  end;

  fCurrentHeader := header;
  fWasLoaded := True;

  Result := True;
end;

procedure TRecorder.SaveToFile(const aFileName: string; updateCache: Boolean);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;

  if updateCache and Assigned(fGame.fReplayCache) then
    fGame.fReplayCache.AddOrReplace(aFilename, fCurrentHeader);
end;

procedure TRecorder.SaveToStream(S: TStream);
var
  header: TReplayFileHeaderRec;
  rec: TReplayRec;
  last: TReplayItem;
begin
  // if ending with pause we delete it
  last := List.LastOrDefault;
  if Assigned(last) and (last.ActionFlags = raf_StartPause) then begin
    List.Delete(List.Count - 1);
  end;

  header.Clear;

  header.Signature := 'LRB';
  header.Version := LEMMIX_REPLAY_VERSION;
  header.FileSize := SizeOf(TReplayFileHeaderRec) + SizeOf(TReplayRec) * List.Count;
  header.HeaderSize := SizeOf(TReplayFileHeaderRec);
  header.Mechanics := fGame.Mechanics + [TMechanic.Obsolete];
  header.FirstRecordPos := header.HeaderSize;
  //header.ReplayRecordSize := SizeOf(TReplayRec); // #EL 2021-01-16 wrong value was here in some previous versions
  header.ReplayRecordCount := List.Count;
  header.Hash := fGame.fLevelLoadingInfo.GetLevelHash;
  header.ReplayGlitchPauseIterations := fGame.GlitchPauseIterations;

  for var t := 1 to 32 do
    header.LevelTitle := ' ';
  for var i := 1 to fGame.Level.Info.Title.Length do
    header.LevelTitle[i - 1] := AnsiChar(fGame.Level.Info.Title[i]);

  S.WriteBuffer(header, SizeOf(TReplayFileHeaderRec));


  for var item: TReplayItem in List do begin
    rec.Clear;
    rec.Check := 'R';
    rec.Iteration := item.Iteration;
    rec.ActionFlags := item.ActionFlags;
    rec.AssignedSkill := item.AssignedSkill;
    rec.SelectedButton := item.SelectedButton;
    rec.ReleaseRate := item.ReleaseRate;
    rec.LemmingIndex := item.LemmingIndex;
    rec.LemmingX := item.LemmingX;
    rec.LemmingY := item.LemmingY;
    rec.CursorX := item.CursorX;
    rec.CursorY := item.CursorY;
    rec.Flags := item.Flags;
    S.WriteBuffer(rec, SizeOf(TReplayRec));
  end;

  fWasSaved := True;
  fCurrentHeader := header;
end;

procedure TRecorder.SaveToTxt(const aFileName: string; includeGameResult: Boolean);

var
  stringlist: TStringList;
  mech: TMechanics;
  hash: UInt64;
  descr: string;

const
  skillstrings: array[rla_none..rla_exploding] of string = (
    '-', 'Walk', 'Jump (not allowed)', 'Dig', 'Climb', 'Drown (not allowed)', 'Hoist (not allowed)',
    'Build', 'Bash', 'Mine', 'Fall (not allowed)', 'Float', 'Splat (not allowed)', 'Exit (not allowed)',
    'Vaporize (not allowed)', 'Block', 'Shrug (not allowed)', 'Ohno (not allowed)', 'Explode'
  );

const
  selstrings: array[rsb_none..rsb_nuke] of string = (
    '-', 'Slower', 'Faster', 'Climber', 'Umbrella', 'Explode', 'Stopper', 'Builder', 'Basher', 'Miner', 'Digger', 'Pause', 'Nuke'
  );

   procedure AddString(const s:string);
   begin
     stringlist.add(s);
   end;

   function PadR(const S: string; aLen: Integer): string; inline;
   begin
     Result := S.PadRight(aLen, ' ');
   end;

   function ActionStr(actionFlags: word): string;
   begin
     Result := '........';
     if raf_StartPause and actionFlags <> 0  then
       result[1] := 'B';
     if raf_EndPause and actionFlags <> 0  then
       result[2] := 'E';
     if raf_StartIncreaseRR and actionFlags <> 0  then
       result[3] := '+';
     if raf_StartDecreaseRR and actionFlags <> 0  then
       result[4] := '-';
     if raf_StopChangingRR and actionFlags <> 0  then
       result[5] := '*';
     if raf_SkillSelection and actionFlags <> 0  then
       result[6] := 'S';
     if raf_SkillAssignment and actionFlags <> 0  then
       result[7] := 'A';
     if raf_Nuke and actionFlags <> 0  then
       result[8] := 'N';
   end;

   function GetPrio(aFlags: Byte): Byte; inline;
   begin
     if aFlags and rf_UseLemming2 = 0 then
       Result := 0
     else
       Result := 1;
   end;


begin
  stringlist := TStringList.Create;
  mech := fGame.Mechanics;
  hash := fGame.fLevelLoadingInfo.GetLevelHash;

  descr := fGame.fLevelLoadingInfo.Style.StyleInformation.Description;
  if descr = fGame.fLevelLoadingInfo.Style.Name then
    descr := string.Empty
  else
    descr := ' (' + descr + ')';

  AddString(StringOfChar('-', 101));
  AddString('  Lemmix Replay Textfile recorded with ' + Consts.FullProgramName);
  AddString(StringOfChar('-', 101));
  AddString('• Title               : ' + Trim(fGame.Level.Info.Title));
  AddString('• Style               : ' + fGame.fLevelLoadingInfo.Style.Name + descr);
  AddString('• Section             : ' + fGame.fLevelLoadingInfo.Section.SectionName);
  AddString('• Level               : ' + Succ(fGame.fLevelLoadingInfo.LevelIndex).ToString);
  AddString('• Source              : ' + fGame.fLevelLoadingInfo.SourceFileName);
  AddString('• Replay fileversion  : ' + IntToStr(LEMMIX_REPLAY_VERSION));
  AddString('• Number of records   : ' + IntToStr(List.count));
  AddString('• Levelhash (decimal) : ' + hash.ToString);
  AddString('• Levelhash (hex)     : ' + IntToHex(hash, 16));
  AddString('• Saved               : ' + FormatDateTime('yyyy-mm-dd hh:NN:ss', Now));
  AddString('• Local computer      : ' + GetLocalComputerName);

  if includeGameResult then begin
    AddString(string.Empty);
    AddString(StringOfChar('-', 101));
    AddString('  Game Result');
    AddString(StringOfChar('-', 101));
    AddString('• Succes       : ' + YesNo(fGame.GameResultRec.Success));
    AddString('• Cheated      : ' + YesNo(fGame.GameResultRec.Cheated));
    AddString('• LemmingCount : ' + fGame.GameResultRec.LemmingCount.ToString);
    AddString('• To rescue    : ' + fGame.GameResultRec.ToRescue.ToString);
    AddString('• Rescued      : ' + fGame.GameResultRec.Rescued.ToString);
    AddString('• Target       : ' + fGame.GameResultRec.Target.ToString + '%');
    AddString('• Done         : ' + fGame.GameResultRec.Done.ToString + '%');
    AddString('• Time is up   : ' + YesNo(fGame.GameResultRec.TimeIsUp));
  end;

  AddString(string.Empty);
  AddString(StringOfChar('-', 101));
  AddString('  Mechanics');
  AddString(StringOfChar('-', 101));
  AddString(mech.AsText(True, True, False));
  AddString(string.Empty);
  AddString('• ReplayPauseGlitchIterations : ' + fRecordedGlitchPauseIterations.ToString);
  AddString(string.Empty);
  AddString(StringOfChar('-', 101));
  AddString(' Rec   Frame  Pausing Action        Skill     Button     RR  lem    x    y   mx   my prio MouseGlitch');
  AddString(StringOfChar('-', 101));

  var i: Integer := 0;
  for var item: TReplayItem in List do begin
    AddString(
      LeadZeroStr(i, 4) + '  ' +
      LeadZeroStr(item.Iteration, 6) + '  ' +
      PadR(YesNo(item.ActionFlags and raf_pausing <> 0), 7) + ' ' +
      PadR(actionstr(item.actionflags), 12) + '  ' +
      PadR(skillstrings[item.AssignedSkill], 8) + '  ' +
      PadR(selstrings[item.SelectedButton], 8) + '  ' +
      LeadZeroStr(item.ReleaseRate, 3) + ' ' +
      LeadZeroStr(item.LemmingIndex,4) + ' ' +
      LeadZeroStr(item.LemmingX, 4) + ' ' +
      LeadZeroStr(item.LemmingY, 4) + ' ' +
      LeadZeroStr(item.CursorX, 4) + ' ' +
      LeadZeroStr(item.CursorY, 4) + ' ' +
      PadR(GetPrio(item.Flags).ToString, 5) +
      YesNo(item.Flags and rf_RightMouseGlitch <> 0)
    );
    Inc(i);
  end;

  AddString(string.Empty);
  AddString(StringOfChar('-', 101));
  AddString('Legend');
  AddString(StringOfChar('-', 101));
  AddString('• B  = Begin pause');
  AddString('• E  = End pause');
  AddString('• +  = Start increasing release rate');
  AddString('• -  = Start decreasing release rate');
  AddString('• *  = Stop changing release rate');
  AddString('• S  = Skill button selection');
  AddString('• A  = Assign skill to lemming');
  AddString('• N  = Nuke');

  if ForceDir(aFileName) then
    stringlist.SaveToFile(aFileName);

  stringlist.free;
end;

procedure TRecorder.Truncate(aCount: Integer);
begin
  List.Count := aCount;
end;

class function TRecorder.LoadTitleHashVersionFromHeader(const aFileName: string; out aHash: UInt64; out aVersion: Byte; out aTitle: TLVLTitle): Boolean;
var
  H: TReplayFileHeaderRec;
  F: TBufferedFileStream;
begin
  aHash := 0;
  aTitle := EmptyLVLTitle;
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    if F.Read(H, SizeOf(H)) <> SizeOf(H) then
      Exit(False);
    if H.Signature <> 'LRB' then
      Exit(False);
    aTitle := H.LevelTitle;
    if H.Version > 1 then
      aHash := H.Hash;
    aVersion := H.Version;
    Result := True;
  finally
    F.Free;
  end;
end;

class function TRecorder.LoadHeader(const aFileName: string; out aHeader: TReplayFileHeaderRec): Boolean;
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    if F.Read(aHeader, SizeOf(aHeader)) <> SizeOf(aHeader) then
      Exit(False);
    if aHeader.Signature <> 'LRB' then
      Exit(False);
    if aHeader.Version <= 1 then
      aHeader.Hash := 0;
    Result := True;
  finally
    F.Free;
  end;
end;

procedure TLemmingGame.SaveCurrentFrameToPng;
var
  fileName: string;
begin
  filename := Consts.PathToScreenShots + GenFileNameWithoutExtension(fLevelLoadingInfo) + '.png';
  if ForceDir(filename) then begin
    var png: TPngImage := fTargetBitmap.ToPng(TPngMode.Opaque);
    try
      try
        png.SaveToFile(fileName);
        Clipboard.Assign(png);
      except
        AddFeedbackMessage(gt.SGame_FeedbackMessage_ScreenshotFail);// 'screenshot fail');
      end;
    finally
      png.Free;
    end;
    AddFeedbackMessage(gt.SGame_FeedbackMessage_Screenshot);// 'screenshot');
  end
  else
    AddFeedbackMessage(gt.SGame_FeedbackMessage_ScreenshotFail);// 'screenshot fail');
end;

procedure TLemmingGame.SaveToEngineFile(const aFileName: string);
// engine project code. the records here should have the exact implementation of the core lemmixengine (other project)
const
  WORLD_WIDTH      = 1584;
  WORLD_HEIGHT     = 160;

  WORLD_PIXEL_SIZE = WORLD_WIDTH * WORLD_HEIGHT;
  WORLD_BITS_SIZE  = WORLD_PIXEL_SIZE shr 3;

  MAX_TRAP_COUNT  = 32;
  MAX_STEEL_COUNT = 32;

type
  TLevelStaticsRec = packed record
  public
    HashKey: UInt64;
    Mechanics: Cardinal;
    ReleaseRate: Integer;
    LemmingsCount: Integer;
    RescueCount: Integer;
    TimeLimit: Integer;
    ClimberCount: Integer;
    FloaterCount: Integer;
    BomberCount: Integer;
    BlockerCount: Integer;
    BuilderCount: Integer;
    BasherCount: Integer;
    MinerCount: Integer;
    DiggerCount: Integer;
    Title: TLVLTitle;
  end;

  TLevelTrapRec = packed record
  public
    Id: Integer; // ID_TRAP_XXX
    Left: Integer;
    Top: Integer;
    TriggerLeft: Integer;
    TriggerTop: Integer;
    TriggerWidth: Integer;
    TriggerHeight: Integer;
    Effect: Integer; // TRAP_EFFECT_XXX
    FrameCount: Integer;
    AnimationType: Integer; // TRAP_ANIMATIONTYPE_XXX
  end;

  TLevelSteelRec = packed record
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TLevelMapRec = packed record
  public
    Terrain: array[0..WORLD_BITS_SIZE - 1] of Byte;
  end;

  // the complete file
  TLevelRec = packed record
    Statics: TLevelStaticsRec;
    TrapCount: Integer;
    Traps: array[0..MAX_TRAP_COUNT - 1] of TLevelTrapRec;
    SteelCount: Integer;
    Steels: array[0..MAX_STEEL_COUNT - 1] of TLevelSteelRec;
    MapWidth: Integer;
    MapHeight: Integer;
    Map: TLevelMapRec;
  end;

  PLevelRec = ^TLevelRec;

var
  lev: PLevelRec;
  metaObj: TMetaObject;
  ix: Integer;
  ptr: PByte;
  shift: Integer;
begin
  TempCursor.Activate;
  if (fWorld.Width <> WORLD_WIDTH) or (fWorld.Height <> WORLD_HEIGHT) then
    Throw('fWorld size mismatch');
  if fLevel.InteractiveObjects.Count > MAX_TRAP_COUNT then
    Throw('too many objects');
  if fLevel.Steels.Count > MAX_STEEL_COUNT then
    Throw('too much steel');

  New(lev);
  FillChar(lev^, SizeOf(TLevelRec), 0);

  // statics
  lev^.Statics.HashKey := fLevelLoadingInfo.GetLevelHash;
  lev^.Statics.Mechanics := Word(fMechanics);
  lev^.Statics.ReleaseRate := fLevel.info.ReleaseRate;
  lev^.Statics.LemmingsCount := fLevel.info.LemmingsCount;
  lev^.Statics.RescueCount := fLevel.info.RescueCount;
  lev^.Statics.TimeLimit := fLevel.info.TimeLimit;
  lev^.Statics.ClimberCount := fLevel.info.ClimberCount;
  lev^.Statics.FloaterCount := fLevel.info.FloaterCount;
  lev^.Statics.BomberCount := fLevel.info.BomberCount;
  lev^.Statics.BlockerCount := fLevel.info.BlockerCount;
  lev^.Statics.BuilderCount := fLevel.info.BuilderCount;
  lev^.Statics.BasherCount := fLevel.info.BasherCount;
  lev^.Statics.MinerCount := fLevel.info.MinerCount;
  lev^.Statics.DiggerCount := fLevel.info.DiggerCount;
  lev^.Statics.Title := fLevelLoadingInfo.GetRawLVLTitle;

  // traps
  ix := 0;
  lev^.TrapCount := fLevel.InteractiveObjects.Count;
  for var obj: TInteractiveObject in fLevel.InteractiveObjects do begin

    metaObj := fGraph.MetaObjectList[obj.Identifier];

    lev^.Traps[ix].Id := obj.Identifier;
    lev^.Traps[ix].Left := obj.Left;
    lev^.Traps[ix].Top := obj.Top;
    lev^.Traps[ix].Effect := metaObj.TriggerEffect;
    lev^.Traps[ix].AnimationType := metaObj.AnimationType;
    lev^.Traps[ix].FrameCount := metaObj.AnimationFrameCount;

    if obj.Identifier = DOS_OBJECT_ID_ENTRANCE then begin
      lev^.Traps[ix].TriggerLeft := 24; // home made trigger. the original is not meant to be as trigger area
      lev^.Traps[ix].TriggerTop := 14;
      lev^.Traps[ix].TriggerWidth := 1;
      lev^.Traps[ix].TriggerHeight := 1;
    end
    else begin
      lev^.Traps[ix].TriggerLeft := metaObj.TriggerLeft;
      lev^.Traps[ix].TriggerTop := metaObj.TriggerTop;
      lev^.Traps[ix].TriggerWidth := metaObj.TriggerWidth;
      lev^.Traps[ix].TriggerHeight := metaObj.TriggerHeight;
    end;

    Inc(ix);
  end;

  // steel
  lev^.SteelCount := fLevel.Steels.Count;
  ix := 0;
  for var steel: TSteel in fLevel.Steels do begin
    lev^.Steels[ix].Left := steel.Left;
    lev^.Steels[ix].Top := steel.Top;
    lev^.Steels[ix].Width := steel.Width;
    lev^.Steels[ix].Height := steel.Height;
    Inc(ix);
  end;

  // fWorld
  lev^.MapWidth := fWorld.Width;
  lev^.MapHeight := fWorld.Height;
  ptr := @lev^.Map.Terrain[0];
  for var y := 0 to fWorld.Height - 1 do begin
    shift := 0;
    for var x := 0 to fWorld.Width - 1 do begin
      if fWorld.Pixel[x, y] and ALPHA_TERRAIN <> 0 then
        ptr^ := ptr^ or (1 shl shift);
      Inc(shift);
      if shift > 7 then begin
        shift := 0;
        Inc(ptr);
      end;
    end;
  end;

  var stream: TBufferedFileStream := TBufferedFileStream.Create(aFileName, fmCreate);
  try
    stream.WriteData(lev^);
  finally
    stream.Free;
  end;

  Dispose(lev);

  Speak(TVoiceOption.BinLevelSaved, True);
end;

end.

