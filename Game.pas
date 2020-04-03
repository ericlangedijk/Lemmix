
unit Game;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Types, System.Classes, System.Contnrs, System.SysUtils, System.Math, System.TypInfo, System.Generics.Collections,
  Vcl.Forms, Vcl.Dialogs, Vcl.Controls, Vcl.Graphics, Vcl.ClipBrd, Vcl.Imaging.PngImage,
  GR32, GR32_OrdinalMaps, GR32_Layers,
  Base.Utils, Base.Bitmaps,
  Dos.Structures, Dos.Consts, Dos.Bitmaps, Prog.Strings,
  Meta.Structures,
  Level.Base,
  Styles.Base, Styles.Dos,
  Prog.Base, Prog.Types, Prog.Data,
  Game.Rendering, Game.SkillPanel, Game.Sound;

// we pass everything to the game preparation with this record
type
  TGameInfoRec = record
    Style                        : TStyle;
    Renderer                     : TRenderer;
    TargetBitmap                 : TBitmap32;
    Level                        : TLevel;
    LevelLoadingInfo             : TLevelLoadingInformation;
    GraphicSet                   : TGraphicSet;
    SoundOpts                    : TSoundOptions;
    UseParticles                 : Boolean;
    UseGradientBridges           : Boolean;
    ShowReplayMessages           : Boolean;
    ShowFeedbackMessages         : Boolean;
    ShowReplayCursor             : Boolean;
    EnableSkillButtonsWhenPaused : Boolean;
    UseShuffledMusic             : Boolean;
    UsePhotoFlashReplayEffect    : Boolean;
    OptionalMechanics            : TOptionalMechanics;
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

  TLemming = class
  private
    function GetLocationBounds: TRect;                      // paint rect in world
    function GetFrameBounds: TRect;                         // frame rect from animation bitmap
    function GetCountDownDigitBounds: TRect;                // painting rect countdown digits
    function GetRTL: Boolean; inline;
  public
    SavedMap                   : array[0..8] of Byte;        // saves part of ObjectMap of game when blocking
    RectToErase                : TRect;                      // the rectangle of the last drawaction (can include space for countdown digits)
    ListIndex                  : Integer;                    // index in the lemminglist
    xPos                       : Integer;                    // the "main" foot x position
    yPos                       : Integer;                    // the "main" foot y position
    xDelta                     : Integer;                    // x speed (1 if left to right, -1 if right to left)
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
  // easy property
    property RTL: Boolean read GetRTL;
  end;

  // internal object used by game
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

  // new experimental in-game message
  TGameMessage = class
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

  // todo: not implemented yet.
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
  TGameMessageList = class(TFastObjectList<TGameMessage>);

  // Replay stuff
  TReplayFileHeaderRec = packed record
    Signature                     : array[0..2] of AnsiChar;  //  3 bytes -  3
    Version                       : Byte;                     //  1 byte  -  4
    FileSize                      : Integer;                  //  4 bytes -  8
    HeaderSize                    : Word;                     //  2 bytes - 10
    Mechanics                     : TMechanics;               //  2 bytes - 12
    FirstRecordPos                : Integer;                  //  4 bytes - 16
    ReplayRecordSize              : Word;                     //  2 bytes - 18
    ReplayRecordCount             : Word;                     //  2 bytes - 20
    Hash                          : UInt64;                   //  8 bytes - 28  #EL 2020 added levelhash for accurate finding of level (replayversion 2)
    ReplayGlitchPauseIterations   : Integer;                  //  4 bytes - 32  #EL 2020: use last reserved int for glitch (replayversion 2)
    LevelTitle                    : TLVLTitle;                // 32 bytes - 64
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
    Flags              : Byte;         // 1 byte  - 30 #EL 2020: added for accurate replaying of RightClickGlitch (replayversion 3).
    Reserved2          : Byte;
    Reserved3          : Byte;         // 32 bytes
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
    fGame : TLemmingGame;
    List  : TFastObjectList<TReplayItem>;
    fCurrentMechanics: TMechanics;
    fRecordedGlitchPauseIterations: Integer;
    fCurrentHeader: TReplayFileHeaderRec;
  public
    constructor Create(aGame: TLemmingGame);
    destructor Destroy; override;
    function Add: TReplayItem; inline;
    procedure Clear; inline;
    procedure Truncate(aCount: Integer); inline;
    procedure SaveToFile(const aFileName: string);
    procedure SaveToStream(S: TStream);
    procedure SaveToTxt(const aFileName: string; includeGameResult: Boolean);
    function LoadFromFile(const aFileName: string; out error: string): Boolean;
    function LoadFromStream(stream: TStream; out error: string): Boolean;
    class function LoadTitleAndHashFromHeader(const aFileName: string; out aHash: UInt64; out aTitle: TLVLTitle): Boolean;
    property WasLoaded: Boolean read fWasLoaded;
    property WasSaved: Boolean read fWasSaved;
    property CurrentHeader: TReplayFileHeaderRec read fCurrentHeader;
  end;

  TLemmingMethod = function (L: TLemming): Boolean of object;
  TLemmingMethodArray = array[TLemmingAction] of TLemmingMethod;
  TSkillMethod = function (Lemming1, Lemming2: TLemming): TLemming of object;
  TSkillMethodArray = array[TLemmingAction] of TSkillMethod;

  TLemmingGame = class
  private
    fSelectedSkill               : TSkillPanelButton;          // currently selected skill restricted by F3-F9
    fMechanics                   : TMechanics;                 // mechanic options
    fParticles                   : TParticleTable;             // all particle offsets
    fParticleColors              : array[0..15] of TColor32;
  // internal objects
    LemmingList                  : TLemmingList;               // the list of lemmings
    World                        : TBitmap32;                  // actual bitmap that is changed by the lemmings
    ObjectMap                    : TByteMap;                   // dos compatible 4 pixel resolution map
    MiniMap                      : TBitmap32;                  // minimap of world
    fMinimapBuffer               : TBitmap32;                  // drawing buffer minimap
    fRecorder                    : TRecorder;
    MessageList                  : TGameMessageList;
    SoundMgr                     : TSoundMgr;
    ReplayCursor                 : TReplayCursor;
  // reference objects, mostly for easy access
    fTargetBitmap                : TBitmap32;                  // ref to the drawing bitmap on the gamewindow
    fRenderer                    : TRenderer;                  // ref to gameparams.renderer
    fInfoPainter                 : TSkillPanelToolbar;         // ref to skillpanel for drawing
    fLevel                       : TLevel;                     // ref to gameparams.level
    fLevelLoadingInfo            : TLevelLoadingInformation;   // ref to the loading information
    fStyle                       : TStyle;                     // ref to gameparams.style
    fGraph                       : TGraphicSet;                // ref to gameparams.graph
    fSoundOpts                   : TSoundOptions;
    CntDownBmp                   : TBitmap32;                  // ref to style.animationset.countdowndigits
    ExplodeMaskBmp               : TBitmap32;                  // ref to style.animationset.explosionmask
    BashMasks                    : array[Boolean] of TBitmap32;// ref to style.animationset.bashmasks (not RTL, RTL)
    MineMasks                    : array[Boolean] of TBitmap32;// ref to style.animationset.minemasks (not RTL, RTL)
  // vars
    fCurrentIteration            : Integer;
    fClockFrame                  : Integer;                    // 17 frames is one game-second
    fGlitchPauseIterations       : Integer;                    // pause glitch
    fStartPauseIteration         : Integer;                    // if pause glitch then 34 otherwise 0
    fReplayGlitchPauseIterations : Integer;
    LemmingsReleased             : Integer;                    // number of lemmings that were created
    LemmingsOut                  : Integer;                    // number of lemmings currently walking around
    LemmingsIn                   : integer;                    // number of lemmings that made it to heaven
    LemmingsRemoved              : Integer;                    // number of lemmings removed
    fCursorPoint                 : TPoint;
    fRightMouseButtonHeldDown    : Boolean;
    Minutes                      : Integer;                    // minutes left
    Seconds                      : Integer;                    // seconds left
    fPlaying                     : Boolean;                    // game in active playing mode?
    EntrancesOpened              : Boolean;
    LemmingMethods               : TLemmingMethodArray;        // a method for each basic lemming state
    SkillMethods                 : TSkillMethodArray;          // a method for assigning jobs (including dummies)
    ObjectInfos                  : TInteractiveObjectInfoList; // list of objects excluding entrances
    Entrances                    : TInteractiveObjectInfoList; // list of entrances
    DosEntranceOrderTable        : TArray<Integer>;            // table(0..3) for entrance release order
    fSlowingDownReleaseRate      : Boolean;
    fSpeedingUpReleaseRate       : Boolean;
    fPaused                      : Boolean;
    MaxNumLemmings               : Integer;
    CurrReleaseRate              : Integer;
    CurrClimberCount             : Integer;
    CurrFloaterCount             : Integer;
    CurrBomberCount              : Integer;
    CurrBlockerCount             : Integer;
    CurrBuilderCount             : Integer;
    CurrBasherCount              : Integer;
    CurrMinerCount               : Integer;
    CurrDiggerCount              : Integer;
    UserSetNuking                : Boolean;
    ExploderAssignInProgress     : Boolean;
    Index_LemmingToBeNuked       : Integer;
    fCurrentCursor               : Integer;                    // normal or highlight lemming
    BrickPixelColor              : TColor32;
    BrickPixelColors             : array[0..11] of TColor32;   // 12 gradient steps
    fGameFinished                : Boolean;
    fGameCheated                 : Boolean;
    NextLemmingCountDown         : Integer;
    fFastForward                 : Boolean;
    fReplaying                   : Boolean;
    fReplayIndex                 : Integer;
    fLastRecordedRecordReached   : Boolean;
    fLastCueSoundIteration       : Integer;
//    fSoundToPlay                 : Integer;
    fSoundsToPlay                : TList<Integer>;
    fMessagesPlayedCount         : Integer;
    fFading                      : Boolean;
    fTargetIteration             : Integer;                    // this is used in hyperspeed
    fHyperSpeedCounter           : Integer;                    // no screenoutput
    fHyperSpeed                  : Boolean;                    // we are at hyperspeed no targetbitmap output
    fLeavingHyperSpeed           : Boolean;                    // in between state (see UpdateLemmings)
    fEntranceAnimationCompleted  : Boolean;
    fStartupMusicAfterEntrance   : Boolean;
    fCurrentlyDrawnLemming       : TLemming;                   // needed for pixelcombining bridges in combinebuilderpixels
    fUseGradientBridges          : Boolean;
    fUseParticles                : Boolean;
    fUseShuffledMusic            : Boolean;
    fShowReplayMessages          : Boolean;
    fShowFeedbackMessages        : Boolean;
    fUsePhotoFlashEffect         : Boolean;
    fShowReplayCursor            : Boolean;
    fEnableSkillButtonsWhenPaused: Boolean;
    fExplodingPixelsUpdateNeeded : Boolean;
    fLastNonPrioritizedLemming   : TLemming;                   // RightClickGlitch emulation (user optional mechanic)
    fAssignmentIsRightClickGlitch: Boolean;                    // storage of the bug. global because we do not want extra parameters in AssignSkill
  // sound indices in soundmgr
    SFX_BUILDER_WARNING          : Integer;
    SFX_ASSIGN_SKILL             : Integer;
    SFX_YIPPEE                   : Integer;
    SFX_SPLAT                    : Integer;
    SFX_LETSGO                   : Integer;
    SFX_ENTRANCE                 : Integer;
    SFX_VAPORIZING               : Integer;
    SFX_DROWNING                 : Integer;
    SFX_EXPLOSION                : Integer;
    SFX_HITS_STEEL               : Integer;
    SFX_OHNO                     : Integer;
    SFX_SKILLBUTTON              : Integer;
    SFX_ROPETRAP                 : Integer;
    SFX_TENTON                   : Integer;
    SFX_BEARTRAP                 : Integer;
    SFX_ELECTROTRAP              : Integer;
    SFX_SPINNINGTRAP             : Integer;
    SFX_SQUISHINGTRAP            : Integer;
  // music index in soundmgr
    MUSIC_INDEX                  : Integer;                    // there is one music possible
    fParticleFinishTimer         : Integer;                    // extra frames to enable viewing of explosions
  // event
    fOnFinish                    : TNotifyEvent;
  // pixel combine eventhandlers
    procedure CombineDefaultPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineLemmingPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineBuilderPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineLemmingHighlight(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineMaskPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineMinimapWorldPixels(F: TColor32; var B: TColor32; M: TColor32);
  // internal methods
    procedure AddReplayMessage(L: TLemming; aAction: TLemmingAction; const Suffix: string = '');
    procedure AddCustomMessage(const s: string);
    procedure ApplyBashingMask(L: TLemming; MaskFrame: Integer);
    procedure ApplyExplosionMask(L: TLemming);
    procedure ApplyMinerMask(L: TLemming; MaskFrame, X, Y: Integer);
    function CalculateNextLemmingCountdown: Integer;
    procedure CheckAdjustReleaseRate;
    procedure CheckForGameFinished;
    procedure CheckForInteractiveObjects(L: TLemming);
    function CheckForLevelTopBoundary(L: TLemming; LocalFrameTopDy: Integer = 0): Boolean;
    function CheckForOverlappingField(L: TLemming): Boolean;
    procedure CheckForPlaySoundEffect;
    procedure CheckForReplayAction;
    procedure CheckLemmings;
    procedure CheckReleaseLemming;
    procedure CheckUpdateNuking;
    procedure CueSoundEffect(aSoundId: Integer);
    function DigOneRow(L: TLemming; Y: Integer): Boolean;
    procedure DrawAnimatedObjects;
    procedure DrawLemmings;
    procedure DrawParticles(L: TLemming);
    procedure DrawReplayCursorCheck;
    procedure DrawReplayCursor(const P: TPoint);
    procedure DrawInitialStatics;
    procedure DrawMessages;
    procedure EraseLemmings;
    procedure EraseMessages;
    procedure EraseParticles(L: TLemming);
    procedure EraseReplayCursor;
    function GetTrapSoundIndex(aDosSoundEffect: Integer): Integer;
    function HasPixelAt(X, Y: Integer): Boolean;
    function HasPixelAt_ClipY(X, Y, minY: Integer): Boolean;
    procedure IncrementIteration;
    procedure InitializeBrickColors(aBrickPixelColor: TColor32);
    procedure InitializeMiniMap;
    procedure InitializeObjectMap;
    procedure LayBrick(L: TLemming);
    function PrioritizedHitTest(out Lemming1, Lemming2: TLemming; const CP: TPoint; CheckRightMouseButton: Boolean): Integer;
    function ReadObjectMap(X, Y: Integer): Byte;
    procedure RecordStartPause;
    procedure RecordEndPause;
    procedure RecordNuke;
    procedure RecordReleaseRate(aActionFlag: Byte);
    procedure RecordSkillAssignment(L: TLemming; aSkill: TLemmingAction; usedLemming2, rightMouseGlitched: Boolean);
    procedure RecordSkillSelection(aSkill: TSkillPanelButton);
    procedure RemoveLemming(L: TLemming);
    procedure RemovePixelAt(X, Y: Integer);
    procedure ReplaySkillAssignment(item: TReplayItem);
    procedure ReplaySkillSelection(aReplayItem: TReplayItem);
    procedure RestoreMap(L: TLemming);
    procedure SaveMap(L: TLemming);
    procedure SetBlockerField(L: TLemming);
    procedure Transition(L: TLemming; aAction: TLemmingAction; DoTurn: Boolean = False);
    procedure TurnAround(L: TLemming);
    function UpdateExplosionTimer(L: TLemming): Boolean;
    procedure UpdateInteractiveObjects;
    procedure UpdateMessages;
    procedure WriteObjectMap(X, Y: Integer; aValue: Byte);
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
  public
    GameResultRec: TGameResultsRec;
    constructor Create;
    destructor Destroy; override;
  // callable
    procedure AdjustReleaseRate(Delta: Integer);
    procedure ChangeMusicVolume(up: Boolean);
    procedure Cheat;
    procedure CreateLemmingAtCursorPoint;
    procedure Finish;
    procedure Terminate;
    procedure HitTest;
    procedure HyperSpeedBegin;
    procedure HyperSpeedEnd;
    function ProcessSkillAssignment: TLemming;
    procedure Prepare(const aInfo: TGameInfoRec);
    procedure RegainControl;
    procedure Save(includeGameResult: Boolean);
    procedure SaveCurrentFrameToPng;
    procedure SetGameResult;
    procedure SetSelectedSkill(Value: TSkillPanelButton; MakeActive: Boolean = True);
    procedure Start(aReplay: Boolean = False);
    procedure UpdateLemmings;
  // properties
    property ClockFrame: Integer read fClockFrame;
    property CurrentCursor: Integer read fCurrentCursor;
    property CurrentIteration: Integer read fCurrentIteration;
    property CursorPoint: TPoint read fCursorPoint write fCursorPoint;
    property EnableSkillButtonsWhenPaused: Boolean read fEnableSkillButtonsWhenPaused;
    property Fading: Boolean read fFading;
    property FastForward: Boolean read fFastForward write fFastForward;
    property GameFinished: Boolean read fGameFinished;
    property HyperSpeed: Boolean read fHyperSpeed;
    property InfoPainter: TSkillPanelToolbar read fInfoPainter write fInfoPainter;
    property Level: TLevel read fLevel;
    property Mechanics: TMechanics read fMechanics;
    property MiniMapBuffer: TBitmap32 read fMiniMapBuffer;
    property Paused: Boolean read fPaused write fPaused;
    property GlitchPauseIterations: Integer read fGlitchPauseIterations;
    property ReplayGlitchPauseIterations: Integer read fReplayGlitchPauseIterations;
    property Playing: Boolean read fPlaying write fPlaying;
    property Recorder: TRecorder read fRecorder;
    property Renderer: TRenderer read fRenderer;
    property Replaying: Boolean read fReplaying;
    property RightMouseButtonHeldDown: Boolean read fRightMouseButtonHeldDown write fRightMouseButtonHeldDown;
    property SlowingDownReleaseRate: Boolean read fSlowingDownReleaseRate;
    property SoundOpts: TSoundOptions read fSoundOpts write SetSoundOpts;
    property SpeedingUpReleaseRate: Boolean read fSpeedingUpReleaseRate;
    property StartPauseIteration: Integer read fStartPauseIteration;
    property TargetIteration: Integer read fTargetIteration write fTargetIteration;
  // event
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
    So what's this: A table which describes what to do when floating.
    The floaters animation has 8 frames: 0..3 is opening the umbrella
    and 4..7 is the actual floating.
    This table "fakes" 16 frames of floating and what should happen with
    the Y-position of the lemming each frame. Frame zero is missing, because that's
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

function CheckRectCopy(const A, B: TRect): Boolean;
begin
  Result := (A.Width = B.Width) and (A.Height = B.Height);
end;

{ TLemming }

function TLemming.GetRTL: Boolean; // inline
begin
  Result := xDelta < 0;
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

{ TGameMessage }

constructor TGameMessage.Create(aGame: TLemmingGame);
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
  //fBuffer.DrawMode := dmBlend;//Transparent;
end;

destructor TGameMessage.Destroy;
begin
  fBuffer.Free;
  inherited;
end;

procedure TGameMessage.NextFrame;
begin
  Inc(fCurrentFrame);
  Inc(fLocation.Y, fDeltaY);
  Inc(fLocation.X, fDeltaX);
  if (fLocation.Y + fBuffer.Height <= 0)
  or (fLocation.X >= GAME_BMPWIDTH)
  or (fLocation.X + fBuffer.Width <= 0)
  or (fCurrentFrame > fDuration) then
    fEnded := True;
  //if fBuffer.MasterAlpha >= 8 then
//  fBuffer.MasterAlpha := fBuffer.MasterAlpha - 8;
end;

procedure TGameMessage.SetText(const Value: string; aColor: TColor32);
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

procedure TReplayCursor.Activate(const aCursorPos: TPoint);
begin
  // if reset we still need to erase the previous location
  if fErasable then
    fPreviousDrawRect := fDrawRect
  else
    fPreviousDrawRect := TRect.Empty;

  fCursorPos := aCursorPos;
  fPosition := Point(fCursorPos.X - 7, fCursorPos.Y - 7);
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
  if fFrames >= 0 then begin
    Dec(fFrames);
    fVisible := fFrames >= 0;
    fErasable := fFrames >= -1;
    Dec(fAlpha, 12);
    fBitmap.MasterAlpha := fAlpha;//ReplaceAlphaForAllNonZeroColors(fAlpha);
  end;
end;

{ TLemmingGame }

constructor TLemmingGame.Create;
begin
  inherited Create;

  Assert(SizeOf(TReplayFileHeaderRec) = 64);
  Assert(SizeOf(TMechanics) = 2); // mechanics set has to fit in correctly inside a replay record

  LemmingList    := TLemmingList.Create;
  World          := TBitmap32.Create;
  ObjectInfos    := TInteractiveObjectInfoList.Create;
  Entrances      := TInteractiveObjectInfoList.Create;
  ObjectMap      := TByteMap.Create;
  MiniMap        := TBitmap32.Create;
  fMinimapBuffer := TBitmap32.Create;
  fRecorder      := TRecorder.Create(Self);
  SoundMgr       := TSoundMgr.Create;
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

  // todo: maybe put the filenames in consts
  SFX_BUILDER_WARNING := SoundMgr.AddSoundFromFileName(TSoundEffect.BuilderWarning.AsFileName);
  SFX_ASSIGN_SKILL    := SoundMgr.AddSoundFromFileName(TSoundEffect.AssignSkill.AsFileName);
  SFX_YIPPEE          := SoundMgr.AddSoundFromFileName(TSoundEffect.Yippee.AsFileName);
  SFX_SPLAT           := SoundMgr.AddSoundFromFileName(TSoundEffect.Splat.AsFileName);
  SFX_LETSGO          := SoundMgr.AddSoundFromFileName(TSoundEffect.LetsGo.AsFileName);
  SFX_ENTRANCE        := SoundMgr.AddSoundFromFileName(TSoundEffect.EntranceOpening.AsFileName);
  SFX_VAPORIZING      := SoundMgr.AddSoundFromFileName(TSoundEffect.Vaporizing.AsFileName);
  SFX_DROWNING        := SoundMgr.AddSoundFromFileName(TSoundEffect.Drowning.AsFileName);
  SFX_EXPLOSION       := SoundMgr.AddSoundFromFileName('Explosion3.WAV'{TSoundEffect.Explosion.AsFileName});
  SFX_HITS_STEEL      := SoundMgr.AddSoundFromFileName(TSoundEffect.HitsSteel.AsFileName);
  SFX_OHNO            := SoundMgr.AddSoundFromFileName(TSoundEffect.Ohno.AsFileName);
  SFX_SKILLBUTTON     := SoundMgr.AddSoundFromFileName(TSoundEffect.SkillButtonSelect.AsFileName);
  SFX_ROPETRAP        := SoundMgr.AddSoundFromFileName(TSoundEffect.RopeTrap.AsFileName);
  SFX_TENTON          := SoundMgr.AddSoundFromFileName(TSoundEffect.TenTonTrap.AsFileName);
  SFX_BEARTRAP        := SoundMgr.AddSoundFromFileName(TSoundEffect.BearTrap.AsFileName);
  SFX_ELECTROTRAP     := SoundMgr.AddSoundFromFileName(TSoundEffect.ElectroTrap.AsFileName);
  SFX_SPINNINGTRAP    := SoundMgr.AddSoundFromFileName(TSoundEffect.SpinningTrap.AsFileName);
  SFX_SQUISHINGTRAP   := SoundMgr.AddSoundFromFileName(TSoundEffect.SquishingTrap.AsFileName);
end;

destructor TLemmingGame.Destroy;
begin
  LemmingList.Free;
  ObjectInfos.Free;
  World.Free;
  Entrances.Free;
  ObjectMap.Free;
  MiniMap.Free;
  fMinimapBuffer.Free;
  fRecorder.Free;
  SoundMgr.Free;
  MessageList.Free;
  ReplayCursor.Free;
  fSoundsToPlay.Free;
  inherited Destroy;
end;

procedure TLemmingGame.Prepare(const aInfo: TGameInfoRec);
// #EL 2009-04-02 added options (needed for replay)
// #EL 2020-02-23 decoupled some things: all info is passed to this method
var
  Ani: TLemmingAnimationSet;
  Bmp: TBitmap32;
begin
  // copy info param
  fMechanics := aInfo.Style.Mechanics;

  fSoundOpts := aInfo.SoundOpts;
  fRenderer := aInfo.Renderer;
  fTargetBitmap := aInfo.TargetBitmap;
  fLevel := aInfo.Level;
  fStyle := aInfo.Style;
  fGraph := aInfo.GraphicSet;
  fLevelLoadingInfo := aInfo.LevelLoadingInfo;
  fUseGradientBridges := aInfo.UseGradientBridges;
  fUseParticles := aInfo.UseParticles;
  fShowReplayMessages := aInfo.ShowReplayMessages;
  fShowFeedbackMessages := aInfo.ShowFeedbackMessages;
  fUseShuffledMusic := aInfo.UseShuffledMusic;
  fEnableSkillButtonsWhenPaused   := aInfo.EnableSkillButtonsWhenPaused;
  fUsePhotoFlashEffect := aInfo.UsePhotoFlashReplayEffect;
  fShowReplayCursor := aInfo.ShowReplayCursor;
  fLastNonPrioritizedLemming := nil;

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
  CntDownBmp.OnPixelCombine := CombineDefaultPixels;

  ExplodeMaskBmp := Ani.ExplosionMaskBitmap;
  ExplodeMaskBmp.DrawMode := dmCustom;
  ExplodeMaskBmp.OnPixelCombine := CombineMaskPixels;

  BashMasks[False] := Ani.BashMasksBitmap;
  BashMasks[False].DrawMode := dmCustom;
  BashMasks[False].OnPixelCombine := CombineMaskPixels;

  BashMasks[True] := Ani.BashMasksRTLBitmap;
  BashMasks[True].DrawMode := dmCustom;
  BashMasks[True].OnPixelCombine := CombineMaskPixels;

  MineMasks[False] := Ani.MineMasksBitmap;
  MineMasks[False].DrawMode := dmCustom;
  MineMasks[False].OnPixelCombine := CombineMaskPixels;

  MineMasks[True] := Ani.MineMasksRTLBitmap;
  MineMasks[True].DrawMode := dmCustom;
  MineMasks[True].OnPixelCombine := CombineMaskPixels;

  // prepare animationbitmaps for drawing (set pixelcombine eventhandlers)
  var ix: Integer := 0;
  for Bmp in Ani.LemmingBitmaps do begin
    Bmp.DrawMode := dmCustom;
    if ix in [TLemmingAnimationSet.BRICKLAYING, TLemmingAnimationSet.BRICKLAYING_RTL] then
      Bmp.OnPixelCombine := CombineBuilderPixels
    else
      Bmp.OnPixelCombine := CombineLemmingPixels;
    Inc(ix);
  end;

  World.SetSize(GAME_BMPWIDTH, 160);

  // load particle array
  var stream: TStream := TData.CreateDataStream(fStyle.Name, Consts.FilenameParticles, TDataType.Particles);
  try
    stream.Read(fParticles, stream.Size);
  finally
    stream.Free;
  end;

  if (fLevelLoadingInfo.MusicFileName <> '') then begin
    if not fUseShuffledMusic then
      MUSIC_INDEX := SoundMgr.AddMusicFromFileName(fLevelLoadingInfo.MusicFileName, fLevelLoadingInfo.MusicStreamType)
    else begin
      var info := fStyle.LevelSystem.SelectRandomLevel;
      if Assigned(info) and (info.MusicFileName <> '') then
        MUSIC_INDEX := SoundMgr.AddMusicFromFileName(info.MusicFileName, info.MusicStreamType)
    end;
  end
  else
    MUSIC_INDEX := -1;
end;

procedure TLemmingGame.Start(aReplay: Boolean = False);
var
  O: TInteractiveObject;
  MO: TMetaObject;
  Inf: TInteractiveObjectInfo;
begin
  Assert(InfoPainter <> nil);

  Playing := False;
  MessageList.Clear;
  fRenderer.RenderWorld(World, False);
  fTargetBitmap.Assign(World);

  // hyperspeed things
  fTargetIteration := 0;
  fHyperSpeedCounter := 0;
  fHyperSpeed := False;
  fLeavingHyperSpeed := False;
  fEntranceAnimationCompleted := False;

  fFastForward := False;
  fLastRecordedRecordReached := False;

  fGameFinished := False;
  fGameCheated := False;
  LemmingsReleased := 0;
  World.Assign(fTargetBitmap);
  World.OuterColor := 0;
  Minutes := Level.Info.TimeLimit;
  Seconds := 0;

  FillChar(GameResultRec, SizeOf(GameResultRec), 0);
  GameResultRec.LemmingCount  := Level.Info.LemmingsCount;
  GameResultRec.ToRescue := Level.Info.RescueCount;

  fReplayIndex := 0;
  LemmingsReleased := 0;
  LemmingsOut := 0;
  LemmingsIn := 0;
  LemmingsRemoved := 0;
  fRightMouseButtonHeldDown := False;
  fCurrentIteration := 0;

  fLastCueSoundIteration := 0;
  fClockFrame := 0;
  fFading := False;
  EntrancesOpened := False;
  ObjectInfos.Clear;
  Entrances.Clear;
  DosEntranceOrderTable := [0, 0, 0, 0];
  fSlowingDownReleaseRate := False;
  fSpeedingUpReleaseRate := False;
  fPaused := False;
  UserSetNuking := False;
  ExploderAssignInProgress := False;
  Index_LemmingToBeNuked := 0;
  fCurrentCursor := 0;
  fParticleFinishTimer := 0;
  LemmingList.Clear;
//  fSoundToPlay := -1;
  fSoundsToPlay.Clear;
  if not aReplay then
    fRecorder.Clear;
  fReplaying := aReplay;
  fExplodingPixelsUpdateNeeded := False;

  // replay pause glitch aware stuff
  if aReplay then
    fReplayGlitchPauseIterations := fGlitchPauseIterations // if not recorded then this is reset from the previously played game
  else
    fReplayGlitchPauseIterations := 0;
  fGlitchPauseIterations := 0;


  // if replaying then overwrite mechanics from the recorder
  if aReplay and (fRecorder.WasLoaded or fRecorder.WasSaved or not fRecorder.List.IsEmpty) then begin // not fRecorder.List.IsEmpty then begin
    fMechanics := fRecorder.fCurrentMechanics;
    if TMechanic.PauseGlitch in fMechanics then
      fReplayGlitchPauseIterations := fRecorder.fRecordedGlitchPauseIterations;
  end;

  if TMechanic.PauseGlitch in fMechanics then
    fStartPauseIteration := 34
  else
    fStartPauseIteration := 0;

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
      2: DosEntranceOrderTable := [0, 1, 1, 0]; // ABBA
      3: DosEntranceOrderTable := [0, 1, 2, 1]; // ABCB
      4: DosEntranceOrderTable := [0, 1, 2, 3]; // ABCD
    else
      DosEntranceOrderTable := [0, 0, 0, 0]; // AAAA
    end; // case
  end
  else begin
    case Entrances.Count of
      2: DosEntranceOrderTable := [0, 1, 0, 1]; // ABAB
      3: DosEntranceOrderTable := [0, 1, 2, 1]; // ABCB  #EL 2009-07-02 BUG solved: changed last index from 0 to 1. #EL 2020: that is why maybe some replay files fail
      4: DosEntranceOrderTable := [0, 1, 2, 3]; // ABCD
    else
      DosEntranceOrderTable := [0, 0, 0, 0]; // AAAA
    end
  end;

  InitializeBrickColors(fGraph.BrickColor);
  InitializeObjectMap;
  InitializeMiniMap;

  DrawAnimatedObjects; // first draw needed

  InfoPainter.SetInfoMinutes(Minutes);
  InfoPainter.SetInfoSeconds(Seconds);
  InfoPainter.SetInfoLemmingsOut(LemmingsOut);
  InfoPainter.SetInfoLemmingsIn(0, 1);
  InfoPainter.DrawButtonSelector(fSelectedSkill, False);

  fSelectedSkill := TSkillPanelButton.None; // to force update
  SetSelectedSkill(TSkillPanelButton.Climber, True); // default

  DrawInitialStatics;

  Playing := True;
end;

procedure TLemmingGame.CombineLemmingPixels(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then B := F;
end;

procedure TLemmingGame.CombineBuilderPixels(F: TColor32; var B: TColor32; M: TColor32);
// This trusts the CurrentlyDrawnLemming variable.
begin
  if F = BrickPixelColor then
    B := BrickPixelColors[12 - fCurrentlyDrawnLemming.NumberOfBricksLeft]
  else if F <> 0 then
    B := F;
end;

procedure TLemmingGame.CombineDefaultPixels(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then B := F;
end;

procedure TLemmingGame.CombineLemmingHighlight(F: TColor32; var B: TColor32; M: TColor32);
// photoflash
begin
  if F <> 0 then B := clBlack32 else B := clWhite32;
end;

procedure TLemmingGame.CombineMaskPixels(F: TColor32; var B: TColor32; M: TColor32);
// copy masks to world
begin
  if F <> 0 then B := 0;
end;

procedure TLemmingGame.CombineMinimapWorldPixels(F: TColor32; var B: TColor32; M: TColor32);
// copy world to minimap
begin
  if F <> 0 then B := BrickPixelColor;
end;

function TLemmingGame.HasPixelAt(X, Y: Integer): Boolean;
//  Read value from world. The function returns True when the value at (x, y) is terrain
begin
  Result := (X >= 0) and (Y >= 0) and (X < World.Width) and (Y < World.Height) and (World.Pixel[X, Y] and ALPHA_TERRAIN <> 0);
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
  World.PixelS[x, y] := 0;
end;

function TLemmingGame.ReadObjectMap(X, Y: Integer): Byte;
// original dos objectmap has a resolution of 4
begin
  // the "and not 3" ensures rounding down when operand is negative (eg. -0.25 -> -1)
  X := (X and not 3) div 4;
  Y := (Y and not 3) div 4;

  Inc(X, OBJMAPADD);
  Inc(Y, OBJMAPADD);

  if (X >= 0) and (X < ObjectMap.Width) and (Y >= 0) and (Y < ObjectMap.Height) then
    Result := ObjectMap.Bits^[X + Y * ObjectMap.Width]
  else
    Result := DOM_NONE; // whoops, important
end;

procedure TLemmingGame.WriteObjectMap(X, Y: Integer; aValue: Byte);
// original dos objectmap has a resolution of 4
begin
  // the "and not 3" ensures rounding down when operand is negative (eg. -0.25 -> -1)
  X := (X and not 3) div 4;
  Y := (Y and not 3) div 4;

  Inc(X, OBJMAPADD);
  Inc(Y, OBJMAPADD);

  if (X >= 0) and (X < ObjectMap.Width) and (Y >= 0) and (Y < ObjectMap.Height) then
    ObjectMap.Bits^[X + Y * ObjectMap.Width] := aValue;
end;

procedure TLemmingGame.SaveMap(L: TLemming);
begin
  L.SavedMap[0] := ReadObjectMap(L.xPos - 4, L.yPos - 6);
  L.SavedMap[1] := ReadObjectMap(L.xPos,     L.yPos - 6);
  L.SavedMap[2] := ReadObjectMap(L.xPos + 4, L.yPos - 6);
  L.SavedMap[3] := ReadObjectMap(L.xPos - 4, L.yPos - 2);
  L.SavedMap[4] := ReadObjectMap(L.xPos,     L.yPos - 2);
  L.SavedMap[5] := ReadObjectMap(L.xPos + 4, L.yPos - 2);
  L.SavedMap[6] := ReadObjectMap(L.xPos - 4, L.yPos + 2);
  L.SavedMap[7] := ReadObjectMap(L.xPos,     L.yPos + 2);
  L.SavedMap[8] := ReadObjectMap(L.xPos + 4, L.yPos + 2);
end;

procedure TLemmingGame.RestoreMap(L: TLemming);
begin
  WriteObjectMap(L.xPos - 4, L.yPos - 6, L.SavedMap[0]);
  WriteObjectMap(L.xPos,     L.yPos - 6, L.SavedMap[1]);
  WriteObjectMap(L.xPos + 4, L.yPos - 6, L.SavedMap[2]);
  WriteObjectMap(L.xPos - 4, L.yPos - 2, L.SavedMap[3]);
  WriteObjectMap(L.xPos,     L.yPos - 2, L.SavedMap[4]);
  WriteObjectMap(L.xPos + 4, L.yPos - 2, L.SavedMap[5]);
  WriteObjectMap(L.xPos - 4, L.yPos + 2, L.SavedMap[6]);
  WriteObjectMap(L.xPos,     L.yPos + 2, L.SavedMap[7]);
  WriteObjectMap(L.xPos + 4, L.yPos + 2, L.SavedMap[8]);
end;

procedure TLemmingGame.SetBlockerField(L: TLemming);
begin
  WriteObjectMap(L.xPos - 4, L.yPos - 6, DOM_FORCELEFT);
  WriteObjectMap(L.xPos,     L.yPos - 6, DOM_BLOCKER);
  WriteObjectMap(L.xPos + 4, L.yPos - 6, DOM_FORCERIGHT);
  WriteObjectMap(L.xPos - 4, L.yPos - 2, DOM_FORCELEFT);
  WriteObjectMap(L.xPos,     L.yPos - 2, DOM_BLOCKER);
  WriteObjectMap(L.xPos + 4, L.yPos - 2, DOM_FORCERIGHT);
  WriteObjectMap(L.xPos - 4, L.yPos + 2, DOM_FORCELEFT);
  WriteObjectMap(L.xPos,     L.yPos + 2, DOM_BLOCKER);
  WriteObjectMap(L.xPos + 4, L.yPos + 2, DOM_FORCERIGHT);
end;

function TLemmingGame.CheckForOverlappingField(L: TLemming): Boolean;
const
  BytesToCheck = [DOM_FORCELEFT, DOM_BLOCKER, DOM_FORCERIGHT];
begin
  Result := (ReadObjectMap(L.xPos - 4, L.yPos - 6) in BytesToCheck) or
            (ReadObjectMap(L.xPos,     L.yPos - 6) in BytesToCheck) or
            (ReadObjectMap(L.xPos + 4, L.yPos - 6) in BytesToCheck) or
            (ReadObjectMap(L.xPos - 4, L.yPos - 2) in BytesToCheck) or
            (ReadObjectMap(L.xPos,     L.yPos - 2) in BytesToCheck) or
            (ReadObjectMap(L.xPos + 4, L.yPos - 2) in BytesToCheck) or
            (ReadObjectMap(L.xPos - 4, L.yPos + 2) in BytesToCheck) or
            (ReadObjectMap(L.xPos,     L.yPos + 2) in BytesToCheck) or
            (ReadObjectMap(L.xPos + 4, L.yPos + 2) in BytesToCheck);
end;

procedure TLemmingGame.Transition(L: TLemming; aAction: TLemmingAction; DoTurn: Boolean = False);
//  Handling of a transition and/or turnaround
var
  ix: Integer;
begin
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
  L.Action := aAction;
  L.Frame := 0;
  L.EndOfAnimation := False;
  L.Fallen := 0;
  L.NumberOfBricksLeft := 0;

  // some things to do when entering state
  case L.Action of
    TLemmingAction.Splatting:
      begin
        L.ExplosionTimer := 0;
        L.xDelta := 0;
        CueSoundEffect(SFX_SPLAT)
      end;
    TLemmingAction.Blocking:
      begin
        L.IsBlocking := True;
        SaveMap(L);
        SetBlockerField(L);
      end;
    TLemmingAction.Exiting:
      CueSoundEffect(SFX_YIPPEE);
    TLemmingAction.Digging:
      L.IsNewDigger := True;
    TLemmingAction.Falling:
      if TMechanic.FallerStartsWith3 in Mechanics then
        L.Fallen := 3;
    TLemmingAction.Building:
      L.NumberOfBricksLeft := 12;
    TLemmingAction.Ohnoing:
      if not UserSetNuking then
        CueSoundEffect(SFX_OHNO);
    TLemmingAction.Exploding:
      CueSoundEffect(SFX_EXPLOSION);
    TLemmingAction.Floating:
      L.FloatParametersTableIndex := 0;
    TLemmingAction.Mining:
      Inc(L.yPos);
  end;
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
  Result := nil;
  Method := Skillmethods[aSkill];
  if Assigned(Method) then begin
    Result := Method(Lemming1, Lemming2);
    if Assigned(Result) then
      CueSoundEffect(SFX_ASSIGN_SKILL);
  end;
  fAssignmentIsRightClickGlitch := False;
end;

function TLemmingGame.AssignClimber(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (CurrClimberCount > 0)
  and not Lemming1.IsClimber
  and not (Lemming1.Action in [TLemmingAction.Blocking, TLemmingAction.Splatting, TLemmingAction.Exploding]) then begin
    Lemming1.IsClimber := True;
    Dec(CurrClimberCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Climber, currClimberCount);
    if TMechanic.AssignClimberShruggerActionBug in Mechanics then
      if (Lemming1.Action = TLemmingAction.Shrugging) then
        Lemming1.Action := TLemmingAction.Walking;
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Climbing, False, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignFloater(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (CurrFloaterCount > 0)
  and not Lemming1.IsFloater
  and not (Lemming1.Action in [TLemmingAction.Blocking, TLemmingAction.Splatting, TLemmingAction.Exploding]) then
  begin
    Lemming1.IsFloater := True;
    Dec(currFloaterCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Umbrella, currFloaterCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Floating, False, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignBomber(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (currBomberCount > 0)
  and (Lemming1.ExplosionTimer = 0)
  and not (lemming1.Action in [TLemmingAction.Ohnoing, TLemmingAction.Exploding, TLemmingAction.Vaporizing, TLemmingAction.Splatting]) then begin
    Lemming1.ExplosionTimer := 79;
    Dec(currBomberCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Explode, currBomberCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Exploding, False, fAssignmentIsRightClickGlitch);
  end
end;

function TLemmingGame.AssignBlocker(Lemming1, Lemming2: TLemming): TLemming;
begin
  Result := nil;
  if (currBlockerCount > 0)
  and (lemming1.Action in [TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Building, TLemmingAction.Bashing, TLemmingAction.Mining, TLemmingAction.Digging])
  and (CheckForOverlappingField(Lemming1) = False) then begin
    Dec(CurrBlockerCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Blocker, currBlockerCount);
    Transition(Lemming1, TLemmingAction.Blocking);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Blocking, False, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignBuilder(Lemming1, Lemming2: TLemming): TLemming;
const
  ActionSet = [TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Bashing, TLemmingAction.Mining, TLemmingAction.Digging];
begin
  Result := nil;

  if (currBuilderCount = 0) or (Lemming1.yPos + Lemming1.FrameTopdy < HEAD_MIN_Y) then
    Exit;

  if (Lemming1.Action in ActionSet) then begin
    Transition(Lemming1, TLemmingAction.Building);
    Dec(CurrBuilderCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Builder, currBuilderCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Building, False, fAssignmentIsRightClickGlitch);
  end
  else if (Lemming2 <> nil) and (Lemming2.Action in ActionSet) then begin
    Transition(Lemming2, TLemmingAction.Building);
    Dec(CurrBuilderCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Builder, currBuilderCount);
    Result := Lemming2;
    RecordSkillAssignment(Lemming2, TLemmingAction.Building, True, fAssignmentIsRightClickGlitch);
  end;

end;

function TLemmingGame.AssignBasher(Lemming1, Lemming2: TLemming): TLemming;
var
  SelectedLemming: TLemming;
const
  actionSet = [TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Building, TLemmingAction.Mining, TLemmingAction.Digging];
begin
  Result := nil;
  if (currBasherCount = 0) then
    Exit
  else if Lemming1.Action in actionSet then
    SelectedLemming := Lemming1
  else if (Lemming2 <> nil) and (Lemming2.Action in actionSet) then
    SelectedLemming := Lemming2
  else
    Exit;

  if (SelectedLemming.ObjectInFront = DOM_STEEL) then
  begin
    CueSoundEffect(SFX_HITS_STEEL);
    Exit;
  end
  else if ((SelectedLemming.ObjectInFront = DOM_ONEWAYLEFT) and (SelectedLemming.xDelta <> -1)) or
          ((SelectedLemming.ObjectInFront = DOM_ONEWAYRIGHT) and (SelectedLemming.xDelta <> 1)) then
    Exit
  else begin
    Transition(SelectedLemming, TLemmingAction.Bashing);
    Dec(CurrBasherCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Basher, CurrBasherCount);
    Result := SelectedLemming;
    RecordSkillAssignment(SelectedLemming, TLemmingAction.Bashing, SelectedLemming = Lemming2, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignMiner(Lemming1, Lemming2: TLemming): TLemming;
var
  SelectedLemming: TLemming;
const
  ActionSet = [TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Building, TLemmingAction.Bashing, TLemmingAction.Digging];
begin
  Result := nil;

  if (CurrMinerCount = 0) then
    Exit
  else if lemming1.Action in ActionSet then
    SelectedLemming := lemming1
  else if Assigned(Lemming2) and (lemming2.Action in ActionSet) then
    SelectedLemming := Lemming2
  else
    Exit;

  if (SelectedLemming.ObjectInFront = DOM_STEEL) then begin
    CueSoundEffect(SFX_HITS_STEEL);
    Exit;
  end
  else if (SelectedLemming.ObjectBelow = DOM_STEEL)
  or ((SelectedLemming.ObjectInFront = DOM_ONEWAYLEFT) and (SelectedLemming.xDelta <> -1))
  or ((SelectedLemming.ObjectInFront = DOM_ONEWAYRIGHT) and (SelectedLemming.xDelta <> 1)) then
    Exit
  else begin
    Transition(SelectedLemming, TLemmingAction.Mining);
    Dec(currMinerCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Miner, currMinerCount);
    Result := SelectedLemming;
    RecordSkillAssignment(SelectedLemming, TLemmingAction.Mining, SelectedLemming = Lemming2, fAssignmentIsRightClickGlitch);
  end;
end;

function TLemmingGame.AssignDigger(Lemming1, Lemming2: TLemming): TLemming;
const
  actionSet = [TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Building, TLemmingAction.Bashing, TLemmingAction.Mining];
begin
  Result := nil;
  if (CurrDiggerCount = 0) or (lemming1.ObjectBelow = DOM_STEEL) then
    Exit
  else if (lemming1.Action in actionSet) then begin//[TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Building, TLemmingAction.Bashing, TLemmingAction.Mining]) then begin
    Transition(lemming1, TLemmingAction.Digging);
    Dec(currDiggerCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Digger, currDiggerCount);
    Result := Lemming1;
    RecordSkillAssignment(Lemming1, TLemmingAction.Digging, False, fAssignmentIsRightClickGlitch);
  end
  else if Assigned(lemming2) and (lemming2.Action in actionSet) then begin //[TLemmingAction.Walking, TLemmingAction.Shrugging, TLemmingAction.Building, TLemmingAction.Bashing, TLemmingAction.Mining]) then begin
    Transition(lemming2, TLemmingAction.Digging);
    Dec(currDiggerCount);
    InfoPainter.DrawSkillCount(TSkillPanelButton.Digger, currDiggerCount);
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
    if L.Action in [TLemmingAction.Vaporizing, TLemmingAction.Drowning, TLemmingAction.Floating, TLemmingAction.Falling]
    then Transition(L, TLemmingAction.Exploding)
    else Transition(L, TLemmingAction.Ohnoing);
    Result := True;
  end;
end;

procedure TLemmingGame.CheckForGameFinished;
begin
  if fGameFinished then
    Exit;
  if fParticleFinishTimer > 0 then
    Exit;

  if (Minutes <= 0) and (Seconds <= 0) then begin
    GameResultRec.TimeIsUp := True;
    Finish;
    Exit;
  end;

  if (LemmingsIn >= MaxNumLemmings)
  or (LemmingsRemoved >= MaxNumLemmings)
  or (UserSetNuking and (LemmingsOut = 0)) then
    Finish;
end;

procedure TLemmingGame.InitializeObjectMap;
//  In one of the previous e-mails I said the DOS Lemmings object map has an
//  x range from -16 to 1647 and a y range from 0 to 159.
//  I think to provide better safety margins, let's extend the y range a bit,
//  say from -16 to 175 (I added 16 in both directions).
//  This is probably slightly on the excessive side but memory is cheap these days,
//  and you can always reduce the x range since DOS Lemmings
//  doesn't let you scroll to anywhere near x=1647
//  (I think the max visible x range is like 1580 or something).
var
  x, y: Integer;
  Inf : TInteractiveObjectInfo;
  Steel: TSteel;
  Effect, V: Byte;
  MaxO: Integer;
begin

  ObjectMap.SetSize((1647 + OBJMAPOFFSET) div 4, (175 + OBJMAPOFFSET) div 4);
  ObjectMap.Clear(DOM_NONE);

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
//  Put the terrainpixels in the minimap. Copy them (scaled) from the worldbitmap.
//  During the game the minimap will be updated like the world-bitmap gets updated.
//  The lemming-pixels are not drawn in the minimap: these are drawn directly in the
//  MiniMapBuffer.
var
  OldCombine: TPixelCombineEvent;
  OldMode: TDrawMode;
  SrcRect, DstRect: TRect;
begin
  Minimap.SetSize(DOS_MINIMAP_WIDTH, DOS_MINIMAP_HEIGHT);
  Minimap.Clear(0);
  OldCombine := World.OnPixelCombine;
  OldMode := World.DrawMode;
  World.DrawMode := dmCustom;
  World.OnPixelCombine := CombineMinimapWorldPixels;
  SrcRect := World.BoundsRect;
  DstRect := Rect(0, 0, World.Width div 16, World.Height div 8);
  World.DrawTo(Minimap, DstRect, SrcRect);
  World.OnPixelCombine := OldCombine;
  World.DrawMode := OldMode;
end;

function TLemmingGame.GetTrapSoundIndex(aDosSoundEffect: Integer): Integer;
begin
  case aDosSoundEffect of
    ose_RopeTrap             : Result := SFX_ROPETRAP;
    ose_SquishingTrap        : Result := SFX_SQUISHINGTRAP;
    ose_TenTonTrap           : Result := SFX_TENTON;
    ose_BearTrap             : Result := SFX_BEARTRAP;
    ose_ElectroTrap          : Result := SFX_ELECTROTRAP;
    ose_SpinningTrap         : Result := SFX_SPINNINGTRAP;
  else
    Result := -1;
  end;
end;

procedure TLemmingGame.CheckForInteractiveObjects(L: TLemming);
var
  Inf: TInteractiveObjectInfo;
begin
  L.ObjectBelow := ReadObjectMap(L.xPos, L.yPos);
  L.ObjectInFront := ReadObjectMap(L.xPos + 8 * L.xDelta, L.yPos - 8);

  case L.ObjectBelow of
    // DOM_NONE = 128 = nothing
    DOM_NONE:
      Exit;
    // 0..127 triggered objects
    0..127:
      begin
        Inf := ObjectInfos[L.ObjectBelow];
        if not Inf.Triggered then begin
          // trigger
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
          CueSoundEffect(SFX_YIPPEE);
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
        CueSoundEffect(SFX_DROWNING);
      end;
    DOM_FIRE:
      begin
        Transition(L, TLemmingAction.Vaporizing);
        CueSoundEffect(SFX_VAPORIZING);
      end;
  end;
end;

procedure TLemmingGame.ApplyExplosionMask(L: TLemming);
// dos explosion mask 16 x 22
var
  X, Y: Integer;
begin
  ExplodeMaskBmp.DrawTo(World, L.xPos - 8, L.yPos -14);
  if not HyperSpeed then
    ExplodeMaskBmp.DrawTo(fTargetBitmap, L.xPos - 8, L.yPos -14);

  // fake draw mask in minimap. this clears 4 pixels as windows programmers should know
  X := L.xPos div 16;
  Y := L.yPos div 8;
  Minimap.FillRectS(X - 1, Y - 1, X + 1, Y + 1, 0);
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
  D.Left := L.xPos + L.FrameLeftDx;
  D.Top := L.yPos + L.FrameTopDy;
  D.Right := D.Left + 16;
  D.Bottom := D.Top + 10;

  Assert(CheckRectCopy(D, S), 'bash rect error');

  Bmp.DrawTo(World, D, S);
  if not HyperSpeed then
    Bmp.DrawTo(fTargetBitmap, D, S);

  // fake draw mask in minimap
  X := L.xPos div 16;
  Y := L.yPos div 8;
  MiniMap.PixelS[X, Y] := 0;
end;

procedure TLemmingGame.ApplyMinerMask(L: TLemming; MaskFrame, X, Y: Integer);
// x,y is topleft
var
  Bmp: TBitmap32;
  S, D: TRect;
  aX, aY: Integer;
begin
  Assert((MaskFrame >= 0) and (MaskFrame <= 1), 'miner mask error');

  Bmp := MineMasks[L.RTL];

  S := Bmp.CalcFrameRect(2, MaskFrame);
  D.Create(X, Y, X + S.Width, Y + S.Height);

  Assert(CheckRectCopy(D, S), 'miner rect error ' + rectstr(d) + ' ' + rectstr(s)); // paranoid

  Bmp.DrawTo(World, D, S);
  if not HyperSpeed then
    Bmp.DrawTo(fTargetBitmap, D, S);

  // fake draw mask in minimap
  aX := L.xPos div 16;
  aY := L.yPos div 8;
  MiniMap.PixelS[aX, aY] := 0;
end;

procedure TLemmingGame.EraseParticles(L: TLemming);
//  Erase the previously drawn particles of an exploded lemming
var
  i, X, Y: Integer;
  Drawn: Boolean;
begin
  if not fUseParticles then
    Exit;

  Drawn := False;

  if L.ParticleFrame <= 50 then begin
    for i := 0 to 79 do begin
      X := fParticles[L.ParticleFrame][i].DX;
      Y := fParticles[L.ParticleFrame][i].DY;
      if (X <> -128) and (Y <> -128) then begin
        X := L.xPos + X;
        Y := L.yPos + Y;
        fTargetBitmap.PixelS[X, Y] := World.PixelS[X, Y];
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
  if not fUseParticles then
    Exit;

  Drawn := False;

  if L.ParticleFrame <= 50 then begin
    for i := 0 to 79 do begin
      X := fParticles[L.ParticleFrame][i].DX;
      Y := fParticles[L.ParticleFrame][i].DY;
      if (X <> -128) and (Y <> -128) then begin
        X := L.xPos + X;
        Y := L.yPos + Y;
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
    Renderer.EraseObject(fTargetBitmap, Inf.Obj, World);

  // erase other objects
  for Inf in ObjectInfos do
    Renderer.EraseObject(fTargetBitmap, Inf.Obj, World);

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
// Erase the lemming from the targetbitmap by copying its rect from the world bitmap.
var
  CurrentLemming: TLemming;
  DstRect: TRect;
begin
  if HyperSpeed or LemmingList.IsEmpty then
    Exit;

  for CurrentLemming in LemmingList do begin
    if not CurrentLemming.IsRemoved then begin
      DstRect := CurrentLemming.RectToErase;
      DstRect.Inflate(2, 2);
      // important to intersect the rects
      if GR32.IntersectRect(DstRect, DstRect, World.BoundsRect) then
        World.DrawTo(fTargetBitmap, DstRect, DstRect);
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
begin
  if HyperSpeed then
    Exit;

  fMinimapBuffer.Assign(Minimap);

  for L in LemmingList do begin
    if not L.IsRemoved then begin
      fCurrentlyDrawnLemming := L;
      SrcRect := L.GetFrameBounds;
      DstRect := L.GetLocationBounds;
      L.RectToErase := DstRect;

      fMinimapBuffer.PixelS[L.xPos div 16, L.yPos div 8] := Color32(0, 255, 000);

      if not L.PhotoFlashForReplay then
        L.LAB.DrawTo(fTargetBitmap, DstRect, SrcRect)
      else begin
        // replay assign job highlight fotoflash effect
        var OldCombine: TPixelCombineEvent := L.LAB.OnPixelCombine;
        L.LAB.OnPixelCombine := CombineLemmingHighlight;
        L.LAB.DrawTo(fTargetBitmap, DstRect, SrcRect);
        L.LAB.OnPixelCombine := OldCombine;
        L.PhotoFlashForReplay := False;
      end;

      // debugging foot pixel
      // if DrawLemmingPixel then
        // fTargetBitmap.FillRectS(L.xPos, L.yPos, L.xPos + 1, L.yPos + 1, clRed32);

      if L.ExplosionTimer > 0 then
      begin
        SrcRect := Rect(0, 0, 8, 8);
        DigRect := L.GetCountDownDigitBounds;
        L.RectToErase.Top := DigRect.Top;
        Assert(CheckRectCopy(SrcRect, DigRect), 'digit rect copy');

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

  HitTest;

  if InfoPainter <> nil then
    InfoPainter.SetInfoLemmingsOut(LemmingsOut);
end;

procedure TLemmingGame.EraseReplayCursor;
begin
  if HyperSpeed or not fReplaying or not ReplayCursor.Erasable then
    Exit;
  if not ReplayCursor.PreviousDrawRect.IsEmpty then begin
    World.DrawTo(fTargetBitmap, ReplayCursor.PreviousDrawRect, ReplayCursor.PreviousDrawRect);
    ReplayCursor.PreviousDrawRect := TRect.Empty;
  end;
  World.DrawTo(fTargetBitmap, ReplayCursor.DrawRect, ReplayCursor.DrawRect);
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
//  if ReplayCursor.Erasable then
  //  EraseReplayCursor;
  //log('draw CP ' + P.X.ToString + ':' + P.Y.ToString);
  ReplayCursor.Activate(P);
  ReplayCursor.Bitmap.DrawTo(fTargetBitmap, ReplayCursor.Position.X, ReplayCursor.Position.Y);
end;

procedure TLemmingGame.EraseMessages;
var
  DstRect: TRect;
  Msg: TGameMessage;
  i: Integer;
begin
  if HyperSpeed then
    Exit;
  if MessageList.IsEmpty then
    Exit;

  for Msg in MessageList do begin
    DstRect := Rect(Msg.fLocation.X, Msg.fLocation.Y, Msg.fLocation.X + Msg.fBuffer.Width, Msg.fLocation.Y + Msg.fBuffer.Height);
    if World.BoundsRect.IntersectsWith(DstRect) then
      World.DrawTo(fTargetBitmap, DstRect, DstRect);
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
  Msg: TGameMessage;
begin
  if HyperSpeed then
    Exit;

  for Msg in MessageList do begin
    if Msg.fEnded then
      Continue;
    DstRect := Rect(Msg.fLocation.X, Msg.fLocation.Y, Msg.fLocation.X + Msg.fBuffer.Width, Msg.fLocation.Y + Msg.fBuffer.Height);
    Msg.fBuffer.DrawTo(fTargetBitmap, Msg.fLocation.X, Msg.fLocation.Y);
  end;

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
  then x := L.xPos
  else x := L.xPos - 4;

  i := 12 - L.NumberOfBricksLeft;
  if i < 0 then i := 0;
  if i > 11 then i := 11;
  C := BrickPixelColors[i];
  C := C or ALPHA_TERRAIN;

  repeat
    if World.PixelS[x, L.yPos - 1] = 0 then
      World.PixelS[x, L.yPos - 1] := C;
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
  x := L.xPos - 4;
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

  // fake draw mask in minimap
  MiniMap.PixelS[L.xPos div 16, L.yPos div 8] := 0;
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
  // next frame (except floating and digging which are handled differently)
  if not (L.Action in [TLemmingAction.Floating, TLemmingAction.Digging]) then begin
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
  Result := False;

  Inc(L.xPos, L.xDelta);

  if (L.xPos >= LEMMING_MIN_X) and (L.xPos <= LEMMING_MAX_X) then begin
    if HasPixelAt_ClipY(L.xPos, L.yPos, 0) then begin
      // walk, jump, climb, or turn around
      dy := 0;
      NewY := L.yPos;
      while (dy <= 6) and HasPixelAt_ClipY(L.xPos, NewY - 1, -dy - 1) do begin
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
          NewY := L.yPos - 2;
        end;
        L.yPos := NewY;
        CheckForLevelTopBoundary(L);
        Exit(True);
      end
    end
    // no pixel at feet
    else begin
      // walk or fall downwards
      dy := 1;
      while dy <= 3 do begin
        Inc(L.yPos);
        if HasPixelAt_ClipY(L.xPos, L.yPos, dy) then
          Break;
        Inc(Dy);
      end;

      if dy > 3 then begin
        // in this case, lemming becomes a faller
        Inc(L.yPos);
        Transition(L, TLemmingAction.Falling);
      end;

      if L.yPos > LEMMING_MAX_Y then begin
        RemoveLemming(L);
        Exit;
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
  while (dy < 2) and HasPixelAt_ClipY(L.xPos, L.yPos - 1, -dy - 1) do begin
    Inc(Dy);
    Dec(L.yPos);
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
  Result := False;

  if L.IsNewDigger then begin
    DigOneRow(L, L.yPos - 2);
    DigOneRow(L, L.yPos - 1);
    L.IsNewDigger := False;
  end
  else begin
    Inc(L.Frame);
    if (L.Frame >= 16) then
      L.Frame := L.Frame - 16
  end;

  if L.Frame in [0, 8] then begin
    y := L.yPos;
    Inc(L.yPos);

    if (L.yPos > LEMMING_MAX_Y) then begin
      // #EL 2020-02-23: we changed L.IsRemoved with RemoveLemming(). This was a small bug. But we almost never get here so in 15 years nobody noticed.
      RemoveLemming(L);
      Exit;
    end;

    if (DigOneRow(L, y) = False) then
      Transition(L, TLemmingAction.Falling)
    else if (ReadObjectMap(L.xPos, L.yPos) = DOM_STEEL) then begin
      CueSoundEffect(SFX_HITS_STEEL);
      Transition(L, TLemmingAction.Walking);
    end;

    Exit(True);
  end
  else
    Exit;
end;

function TLemmingGame.HandleClimbing(L: TLemming): Boolean;
begin
  if (L.Frame <= 3) then begin
    // check if we approached the top
    if not HasPixelAt_ClipY(L.xPos, L.yPos - 7 - L.Frame, 0) then begin
      L.yPos := L.yPos - L.Frame + 2;
      Transition(L, TLemmingAction.Hoisting);
      CheckForLevelTopBoundary(L);
    end;
    Exit(True);
  end
  else begin
    Dec(L.yPos);
    // check for overhang or level top boundary
    if (L.yPos + L.FrameTopDy < HEAD_MIN_Y)
    or HasPixelAt_ClipY(L.xPos - L.xDelta, L.yPos - 8, -8) then begin
      Transition(L, TLemmingAction.Falling, True);
      Inc(L.xPos, L.xDelta * 2);
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
  else if not HasPixelAt(L.xPos + 8 * L.xDelta, L.yPos) then
    Inc(L.xPos, L.xDelta);
end;

function TLemmingGame.HandleHoisting(L: TLemming): Boolean;
begin
  Result := False;
  if L.Frame <= 4 then begin
    Dec(L.yPos, 2);
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
  Result := False;

  // sound
  if (L.Frame = 10) and (L.NumberOfBricksLeft <= 3) then
    CueSoundEffect(SFX_BUILDER_WARNING);

  // lay brick
  if (L.Frame = 9) or ( (L.Frame = 10) and (L.NumberOfBricksLeft = 9) ) then begin
    LayBrick(L);
    Exit;
  end
  else if (L.Frame = 0) then begin
    Inc(L.xPos, L.xDelta);
    Dec(L.yPos);
    if (L.xPos <= LEMMING_MIN_X) or (L.xPos > LEMMING_MAX_X)
    or HasPixelAt_ClipY(L.xPos, L.yPos - 1, -1) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      CheckForLevelTopBoundary(L);
      Exit(True);
    end;

    Inc(L.xPos, L.xDelta);
    if HasPixelAt_ClipY(L.xPos, L.yPos - 1, -1) then begin
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

    if HasPixelAt_ClipY(L.xPos + L.xDelta * 2, L.yPos - 9, -9)
    or (L.xPos <= LEMMING_MIN_X)
    or (L.xPos > LEMMING_MAX_X) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      CheckForLevelTopBoundary(L);
      Exit(True);
    end;

    {-------------------------------------------------------------------------------
      if builder too high he becomes a walker. will *not* turn around
      although it seems he should, but the CheckForLevelTop fails because
      of a changed FrameTopDy
    -------------------------------------------------------------------------------}
    if (L.yPos + L.FrameTopDy < HEAD_MIN_Y) then
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
  Result := False;

  index := L.Frame;
  if index >= 16 then
    Dec(index, 16);

  if (11 <= index) and (index <= 15) then begin
    Inc(L.xPos, L.xDelta);

    if (L.xPos < LEMMING_MIN_X) or (L.xPos > LEMMING_MAX_X) then begin
      // outside leftside or outside rightside?
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
    end
    else begin
      // check 3 pixels below the new position
      dy := 0;
      while (dy < 3) and not HasPixelAt_ClipY(L.xPos, L.yPos, dy) do begin
        Inc(dy);
        Inc(L.yPos);
      end;

      if dy = 3 then
        Transition(L, TLemmingAction.Falling)
      else begin
        // check steel or one way digging
        FrontObj := ReadObjectMap(L.xPos + L.xDelta * 8, L.yPos - 8);

        if (FrontObj = DOM_STEEL) then
          CueSoundEffect(SFX_HITS_STEEL);

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
      // special treatment frame 5 (see txt)
      if L.Frame = 5 then begin
        n := 0;
        x := L.xPos + L.xDelta * 8;
        y := L.yPos - 6;
        // here the use of HasPixelAt rather than HasPixelAt_ClipY is correct
        while (n < 4) and not HasPixelAt(x, y) do begin
          Inc(n);
          Inc(x, L.xDelta);
        end;
        if n = 4 then
          Transition(L, TLemmingAction.Walking);
      end;
    end;
  end;

end;

function TLemmingGame.HandleMining(L: TLemming): Boolean;
var
  BelowObj: Byte;
  Bug: Boolean;
begin
  Result := False;

  if L.Frame = 1 then begin
    ApplyMinerMask(L, 0, L.xPos + L.frameLeftdx, L.yPos + L.frameTopdy);
    Exit;
  end
  else if L.Frame = 2 then begin
    ApplyMinerMask(L, 1, L.xPos + L.xDelta + L.frameLeftdx, L.yPos + 1 + L.frameTopdy);
    Exit;
  end
  else if L.Frame in [3, 15] then begin
    Inc(L.xPos, L.xDelta);
    if (L.xPos < LEMMING_MIN_X) or (L.xPos > LEMMING_MAX_X) then begin
      Transition(L, TLemmingAction.Walking, True); // turn around as well
      Exit(True);
    end;

    Inc(L.xPos, L.xDelta);
    if (L.xPos < LEMMING_MIN_X) or (L.xPos > LEMMING_MAX_X) then begin
      Transition(L, TLemmingAction.Walking, True);  // turn around as well
      Exit(True);
    end;

    if (L.Frame = 3) then begin
      Inc(L.yPos);
      if (L.yPos > LEMMING_MAX_Y) then begin
        RemoveLemming(L);
        Exit;
      end;
    end;

    if not HasPixelAt_ClipY(L.xPos, L.yPos, 0) then begin
      Transition(L, TLemmingAction.Falling);
      Exit(True);
    end;

    belowObj := ReadObjectMap(L.xPos, L.yPos);
    if (belowObj = DOM_STEEL) then
      CueSoundEffect(SFX_HITS_STEEL);

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
      or (belowObj = DOM_ONEWAYRIGHT) then // missing check
        Transition(L, TLemmingAction.Walking, True);  // turn around as well
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
    Inc(L.yPos);
    if (L.yPos > LEMMING_MAX_Y) then begin
      RemoveLemming(L);
      Exit;
    end
    else
      Exit(True)
  end;
end;

function TLemmingGame.HandleFalling(L: TLemming): Boolean;
var
  dy: Integer;
begin
  Result := False;

  if (L.Fallen > 16) and L.IsFloater then begin
    Transition(L, TLemmingAction.Floating);
    Exit(True);
  end
  else begin
    dy := 0;
    while (dy < 3) and not HasPixelAt_ClipY(L.xPos, L.yPos, dy) do begin
      Inc(Dy);
      Inc(L.yPos);
      if (L.yPos > LEMMING_MAX_Y) then begin
        RemoveLemming(L);
        //CueSoundEffect(SFX_DIE); // todo: extended sounds?
        Exit;
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
  Result := False;
  L.Frame := FloatParametersTable[L.FloatParametersTableIndex].AnimationFrameIndex;
  dy := FloatParametersTable[L.FloatParametersTableIndex].dy;

  Inc(L.FloatParametersTableIndex);
  if L.FloatParametersTableIndex >= 16 then
    L.FloatParametersTableIndex := 8;

  if (dy <= 0) then
    Inc(L.yPos, dy)
  else begin
    minY := 0;
    while (dy > 0) do begin
      if HasPixelAt_ClipY(L.xPos, L.yPos, minY) then begin
        Transition(L, TLemmingAction.Walking);
        Exit(True)
      end
      else begin
        Inc(L.yPos);
        Dec(dy);
        Inc(minY);
      end;
    end; // while
  end;

  if (L.yPos > LEMMING_MAX_Y) then begin
    RemoveLemming(L);
    Exit;
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
    Inc(LemmingsIn);
    InfoPainter.SetInfoLemmingsOut(LemmingsOut);
    InfoPainter.SetInfoLemmingsIn(LemmingsIn, MaxNumLemmings);
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
  if not HasPixelAt_ClipY(L.xPos, L.yPos, 0) then begin
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
  Result := False;
  if L.EndOfAnimation then begin
    Transition(L, TLemmingAction.Exploding);
    Exit;
  end
  else begin
    dy := 0;
    while (dy < 3) and not HasPixelAt_ClipY(L.xPos, L.yPos, dy) do begin
      Inc(dy);
      Inc(L.yPos);
    end;

    if (L.yPos > LEMMING_MAX_Y) then begin
      RemoveLemming(L);
      Exit;
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
    if not (ReadObjectMap(L.xPos, L.yPos) in [DOM_STEEL, DOM_WATER]) then
      ApplyExplosionMask(L);
    RemoveLemming(L);
    L.IsExploded := True;
    L.ParticleTimer := PARTICLE_FRAMECOUNT;
    if fUseParticles then
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
  if (L.yPos + dy < HEAD_MIN_Y) then begin
    Result := True;
    L.yPos := HEAD_MIN_Y - 2 - dy;
    TurnAround(L);
    if L.Action = TLemmingAction.Jumping then
      Transition(L, TLemmingAction.Walking);
  end;
end;


procedure TLemmingGame.RemoveLemming(L: TLemming);
begin
  L.IsRemoved := True;
  Dec(LemmingsOut);
  Inc(LemmingsRemoved);
end;

procedure TLemmingGame.UpdateLemmings;
{-------------------------------------------------------------------------------
  The main method: handling a single frame of the game.
-------------------------------------------------------------------------------}
begin
  if fGameFinished then
    Exit;
  CheckForGameFinished;

  // do not move this!
  if not Paused then // paused is handled by the GUI
    CheckAdjustReleaseRate;

  // just as a warning: do *not* mess around with the order here
  IncrementIteration;
  EraseLemmings;
  EraseReplayCursor;
  EraseMessages;

  CheckReleaseLemming;
  CheckLemmings;
  CheckUpdateNuking;
  UpdateInteractiveObjects;
  UpdateMessages;

  // when hyperspeed is terminated then copy complete world back into targetbitmap
  if fLeavingHyperSpeed then begin
    fHyperSpeed := False;
    fLeavingHyperSpeed := False;
    fTargetBitmap.Assign(World);
  end;

  DrawAnimatedObjects;
  DrawLemmings;
  DrawReplayCursorCheck;
  DrawMessages;

  CheckForReplayAction;
  CheckForPlaySoundEffect;

  // force update if raw explosion pixels drawn
  if fExplodingPixelsUpdateNeeded then begin
    fTargetBitmap.Changed;
    fExplodingPixelsUpdateNeeded := False;
  end;

end;

procedure TLemmingGame.UpdateMessages;
var
  Msg: TGameMessage;
begin
  if HyperSpeed then
    Exit;
  if MessageList.IsEmpty then
    Exit;
  for Msg in MessageList do begin
    Msg.NextFrame;
  end;
end;

procedure TLemmingGame.IncrementIteration;
begin
  Inc(fCurrentIteration);

  if TMechanic.PauseGlitch in fMechanics then begin
    if fReplayGlitchPauseIterations > 0 then begin
      Dec(fReplayGlitchPauseIterations);
      Paused := fReplayGlitchPauseIterations > 0;
    end;
    if Paused and (fCurrentIteration <= fStartPauseIteration) then begin
      Inc(fGlitchPauseIterations);
      if not fReplaying then
        fRecorder.fRecordedGlitchPauseIterations := fGlitchPauseIterations; // keep track
    end
    else
      Inc(fClockFrame);
  end
  else
    Inc(fClockFrame);

  if fParticleFinishTimer > 0 then
    Dec(fParticleFinishTimer);

  if fClockFrame = 17 then begin
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
    if InfoPainter <> nil then begin
      InfoPainter.SetInfoMinutes(Minutes);
      InfoPainter.SetInfoSeconds(Seconds);
    end;
 end;


  // hard coded dos frame numbers
  case CurrentIteration of
    15: CueSoundEffect(SFX_LETSGO);
    34: CueSoundEffect(SFX_ENTRANCE);
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
          if (TSoundOption.Music in fSoundOpts) and not SoundMgr.MusicIsPlaying[MUSIC_INDEX] then
            SoundMgr.PlayMusic(MUSIC_INDEX);
        end;
      end;
  end;

end;

procedure TLemmingGame.DrawInitialStatics;
// draw the level info at startup
begin
  Assert(Assigned(InfoPainter));
  InfoPainter.DrawSkillCount(TSkillPanelButton.Slower, Level.Info.ReleaseRate);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Faster, Level.Info.ReleaseRate);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Climber, Level.Info.ClimberCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Umbrella, Level.Info.FloaterCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Explode, Level.Info.BomberCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Blocker, Level.Info.BlockerCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Builder, Level.Info.BuilderCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Basher, Level.Info.BasherCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Miner, Level.Info.MinerCount);
  InfoPainter.DrawSkillCount(TSkillPanelButton.Digger, Level.Info.DiggerCount);
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
  PrioActions = [TLemmingAction.Blocking, TLemmingAction.Building, TLemmingAction.Shrugging, TLemmingAction.Bashing, TLemmingAction.Mining, TLemmingAction.Digging, TLemmingAction.Ohnoing];
begin
  Result := 0;
  PrioritizedLemming := nil;
  NonPrioritizedLemming := nil;
  Lemming1 := nil;
  Lemming2 := nil;

  for L in LemmingList do begin
    if L.IsRemoved then
      Continue;
    x := L.xPos + L.FrameLeftDx;
    y := L.yPos + L.FrameTopDy;
    if (x <= CP.X) and (CP.X <= x + 12) and (y <= CP.Y) and (CP.Y <= y + 12) then begin
      Inc(Result);
      if L.Action in PrioActions then
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
  S: string;
begin
  HitCount := PrioritizedHitTest(Lemming1, Lemming2, CursorPoint, True);
  if (HitCount > 0) and (Lemming1 <> nil) then
  begin
    S := LemmingActionStrings[Lemming1.Action];
    // get highlight text
    if Lemming1.IsClimber and Lemming1.IsFloater then
      S := SAthlete
    else if Lemming1.IsClimber then
      S := SClimber
    else if Lemming1.IsFloater then
      S := SFloater
    else
      S := LemmingActionStrings[Lemming1.Action];

    InfoPainter.SetInfoCursorLemming(S, HitCount);
    fCurrentCursor := 2;
  end
  // no hit
  else begin
    if fReplaying then begin
      if not fLastRecordedRecordReached or (fReplayGlitchPauseIterations > 0) then
        InfoPainter.SetInfoAlternative('replaying')
      else
        InfoPainter.SetInfoAlternative('replayed');
    end
    else
      InfoPainter.SetInfoCursorLemming('', 0);
    fCurrentCursor := 1;
  end;
end;

function TLemmingGame.ProcessSkillAssignment: TLemming;
var
  act: TLemmingAction;
  Lemming1, Lemming2: TLemming;
begin
  Result := nil;

  // convert selected skillpanel buttontype to action we have to assign
  act := SkillPanelButtonToAction[fSelectedSkill];
  Assert(act <> TLemmingAction.None);

  if PrioritizedHitTest(Lemming1, Lemming2, CursorPoint, True) > 0 then begin

    if TMechanic.RightClickGlitch in fMechanics then begin // this is the RightClickGlitch emulation
      if Lemming1 = nil then begin
        Lemming1 := fLastNonPrioritizedLemming;
        fAssignmentIsRightClickGlitch := True;
      end;
    end;

    if Lemming1 <> nil then
      Result := AssignSkill(Lemming1, Lemming2, act);
  end;
end;

procedure TLemmingGame.ReplaySkillAssignment(item: TReplayItem);
// todo on any error regain control and show an ingame message. no exception raising
var
  storedLemming: TLemming;
  assignedAction: TLemmingAction;
begin
  if (item.LemmingIndex < 0) or (item.LemmingIndex >= LemmingList.Count) then begin
    RegainControl;
    AddCustomMessage('replay lemming error');
    Exit;
  end;

  storedLemming := LemmingList[item.LemmingIndex];

  Assert(item.AssignedSkill > 0);
  Assert(item.AssignedSkill <= 19);

  assignedAction := TLemmingAction(item.AssignedSkill);

  if not (assignedAction in AssignableSkills) then begin
    RegainControl;
    AddCustomMessage('replay skill error');
    Exit;
  end;

  if assignedAction in AssignableSkills then begin
    // for antiques but nice (and just to be sure the right skill is selected in the skillpanel)
    if ActionToSkillPanelButton[assignedAction] <> fSelectedSkill then
      SetSelectedSkill(ActionToSkillPanelButton[assignedAction], True);

    if item.Flags and rf_UseLemming2 = 0 then
      AssignSkill(storedLemming, nil, assignedAction)
    else
      AssignSkill(storedLemming, storedLemming, assignedAction);

    if not HyperSpeed then begin
      // some very old replays (0.0.7.0 and before that) did not store cursorposition, and there is crap inside it
      if fShowReplayCursor and item.HasValidCursorData then
        DrawReplayCursor(Point(item.CursorX, item.CursorY));
      if fUsePhotoFlashEffect then
        storedLemming.PhotoFlashForReplay := True;
      if item.Flags and rf_RightMouseGlitch = 0 then
        AddReplayMessage(storedLemming, assignedAction)
      else
        AddReplayMessage(storedLemming, assignedAction, ' glitch'); // show it was the glitch
    end;

    // check if the lemming is in sync with the replay command
    if (item.LemmingX > 0) and (item.LemmingY > 0)
    and ((item.LemmingX <> storedLemming.xPos) or (item.LemmingY <> storedLemming.yPos)) then begin
      RegainControl;
      AddCustomMessage('replayaction failed');
      Exit;
    end;
  end;

end;

procedure TLemmingGame.ReplaySkillSelection(aReplayItem: TReplayItem);
var
  bs: TSkillPanelButton;
begin
  bs := tSkillPanelButton(areplayitem.selectedbutton); // convert
    if bs in [
    TSkillPanelButton.Climber,
    TSkillPanelButton.Umbrella,
    TSkillPanelButton.Explode,
    TSkillPanelButton.Blocker,
    TSkillPanelButton.Builder,
    TSkillPanelButton.Basher,
    TSkillPanelButton.Miner,
    TSkillPanelButton.Digger] then
  setselectedskill(bs, true);
//  case aReplayItem
end;


procedure TLemmingGame.SetSelectedSkill(Value: TSkillPanelButton; MakeActive: Boolean = True);
begin
  case Value of
    TSkillPanelButton.Faster:
      begin
        if fSpeedingUpReleaseRate <> MakeActive then
          case MakeActive of
            False : RecordReleaseRate(raf_StopChangingRR);
            True  : RecordReleaseRate(raf_StartIncreaseRR);
          end;
        fSpeedingUpReleaseRate := MakeActive;
        if MakeActive then
          fSlowingDownReleaseRate := False;
      end;
    TSkillPanelButton.Slower:
      begin
        if fSlowingDownReleaseRate <> MakeActive then
          case MakeActive of
            False: RecordReleaseRate(raf_StopChangingRR);
            True: RecordReleaseRate(raf_StartDecreaseRR);
          end;
        fSlowingDownReleaseRate := MakeActive;
        if MakeActive then
          fSpeedingUpReleaseRate := False;
      end;
    TSkillPanelButton.Climber:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := TSkillPanelButton.Climber;
        InfoPainter.DrawButtonSelector(TSkillPanelButton.Climber, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Umbrella:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Explode:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Blocker:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Builder:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Basher:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Miner:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Digger:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    TSkillPanelButton.Pause:
      begin
        case Paused of
          False:
            begin
              Paused := True;
              FastForward := False;
              RecordStartPause;
            end;
          True:
            begin
              Paused := False;
              FastForward := False;
              RecordEndPause;
            end;
        end;
      end;
    TSkillPanelButton.Nuke:
      begin
        // next line of code is the NUKE GLITCH. Changing MaxNumLemmings also allows IN % to be calculated and displayed in-game using the glitch calculation,
        // just like the actual game
        if TMechanic.NukeGlitch in fMechanics then
          MaxNumLemmings := LemmingsReleased;
        UserSetNuking := True;
        ExploderAssignInProgress := True;
        RecordNuke;
      end;
  end;
end;

procedure TLemmingGame.CheckReleaseLemming;
var
  NewLemming: TLemming;
  ix, EntranceIndex: Integer;
begin
  if not EntrancesOpened or UserSetNuking then
    Exit;

  // NextLemmingCountdown is initialized to 20 before start of a level
  Dec(NextLemmingCountdown);

  if NextLemmingCountdown = 0 then begin
    NextLemmingCountdown := CalculateNextLemmingCountdown;
    if LemmingsReleased < MaxNumLemmings then begin
      EntranceIndex := LemmingsReleased mod 4;
      ix := DosEntranceOrderTable[EntranceIndex];
      NewLemming := TLemming.Create;
      NewLemming.ListIndex := LemmingList.Add(NewLemming);
      NewLemming.Born := CurrentIteration;
      Transition(NewLemming, TLemmingAction.Falling);
      NewLemming.xPos := Entrances[ix].Obj.Left + 24;
      // @Optional Game Mechanic
      if TMechanic.EntranceX25 in Mechanics then
        Inc(NewLemming.xPos);
      NewLemming.yPos := Entrances[ix].Obj.Top + 14;
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
  if UserSetNuking and ExploderAssignInProgress then begin
    // find first following non removed lemming
    while (Index_LemmingToBeNuked < LemmingsReleased)
    and (LemmingList[Index_LemmingToBeNuked].IsRemoved) do
      Inc(Index_LemmingToBeNuked);

    if (Index_LemmingToBeNuked > LemmingsReleased - 1) then
      ExploderAssignInProgress := False
    else begin
      CurrentLemming := LemmingList[Index_LemmingToBeNuked];
      if (CurrentLemming.ExplosionTimer = 0) and not (CurrentLemming.Action in [TLemmingAction.Splatting, TLemmingAction.Exploding]) then
        CurrentLemming.ExplosionTimer := 79;
      Inc(Index_LemmingToBeNuked);
    end;
  end;
end;

procedure TLemmingGame.CreateLemmingAtCursorPoint;
{-------------------------------------------------------------------------------
  debugging procedure: click and create lemming
-------------------------------------------------------------------------------}
var
  NewLemming: TLemming;
begin
  if not EntrancesOpened or UserSetNuking then
    Exit;
  if LemmingsReleased < MaxNumLemmings then begin
    NewLemming := TLemming.Create;
    NewLemming.ListIndex := LemmingList.Add(NewLemming);
    NewLemming.Born := CurrentIteration;
    Transition(NewLemming, TLemmingAction.Falling);
    NewLemming.xPos := CursorPoint.X;
    NewLemming.yPos := CursorPoint.Y;
    NewLemming.xDelta := 1;
    // these must be initialized to nothing
    NewLemming.ObjectInFront := DOM_NONE;
    NewLemming.ObjectBelow := DOM_NONE;
    Inc(LemmingsReleased);
    Inc(LemmingsOut);
  end;
end;


function TLemmingGame.CalculateNextLemmingCountdown: Integer;
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
  Result := Result div 2 + 4
end;

procedure TLemmingGame.CheckForPlaySoundEffect;
begin
  if HyperSpeed then
    Exit;

  if fSoundsToPlay.Count = 0 then
    Exit;

  for var i: integer in fSoundsToPlay do begin
    SoundMgr.PlaySound(i);
  end;

  fSoundsToPlay.Clear;
  Exit;

//  if fSoundToPlay <> -1 then begin
//    SoundMgr.PlaySound(fSoundToPlay);
//    fSoundToPlay := -1;
//  end;
end;

procedure TLemmingGame.CueSoundEffect(aSoundId: Integer);
// save last sound.
begin
  if HyperSpeed or not Playing or (aSoundId < 0) or not (TSoundOption.Sound in fSoundOpts) then
    Exit;
  //fSoundToPlay := aSoundId;
  if fSoundsToPlay.Count < 8 then // i think we have some balance with the soundlib (overlapping sounds)
    fSoundsToPlay.Add(aSoundId);
  if Paused then
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
    InfoPainter.DrawSkillCount(TSkillPanelButton.Faster, currReleaseRate);
  end;
end;

procedure TLemmingGame.AddReplayMessage(L: TLemming; aAction: TLemmingAction; const Suffix: string = '');
const
  countColors = 8;
  colors: array[0..countColors - 1] of TColor32 = (
    clBlue32, clRed32, clYellow32, clGreen32, clFuchsia32, clLime32, clLightGray32, clOrange32
  );
var
  Msg: TGameMessage;
begin
  if not fShowReplayMessages then
    Exit;
  Msg := TGameMessage.Create(Self);
  Msg.Duration := 32;
  Msg.fLocation := Point(L.GetLocationBounds.Left, L.GetLocationBounds.Top - 8);
  Msg.DeltaX := L.xDelta;
  Msg.SetText(LemmingReplayStrings[aAction] + Suffix, colors[fMessagesPlayedCount mod countColors]);
  inc(fMessagesPlayedCount);
  MessageList.Add(Msg);
end;

procedure TLemmingGame.AddCustomMessage(const s: string);
const
  countColors = 8;
  colors: array[0..countColors - 1] of TColor32 = (
    clBlue32, clRed32, clYellow32, clGreen32, clFuchsia32, clLime32, clLightGray32, clOrange32
  );
var
  Msg: TGameMessage;
begin
  if not fShowFeedbackMessages then
    Exit;
  Msg := TGameMessage.Create(Self);
  Msg.Duration := 32;
  Msg.DeltaX := 0;
  Msg.SetText(s, colors[fMessagesPlayedCount mod countColors]);
  Msg.fLocation := Point(CursorPoint.X - Msg.fBuffer.Width div 2, 32);
  inc(fMessagesPlayedCount);
  MessageList.Add(Msg);
end;

procedure TLemmingGame.RecordStartPause;
{-------------------------------------------------------------------------------
  Records the start of a pause session.
  Just in case: when the previous record is raf_Pausing or raf_StartPause
  we do *not* record it.
-------------------------------------------------------------------------------}

    function PrevOk: Boolean;
    var
      R: TReplayItem;
      last: Integer;
    begin
      Result := True;
      if fRecorder.List.IsEmpty then
         Exit;
       last := fRecorder.List.Count - 1;
       R := fRecorder.List[last];
       Result := R.ActionFlags and (raf_Pausing or raf_StartPause) = 0;
    end;

var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying or not PrevOk then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := raf_StartPause;
  R.ReleaseRate := currReleaseRate;
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
      R: TReplayItem;
      last: Integer;
    begin
      Result := False;
      if fRecorder.List.IsEmpty then
         Exit;
       last := fRecorder.List.Count - 1;
       R := fRecorder.List[last];
       Result := R.ActionFlags and (raf_Pausing or raf_StartPause) <> 0;
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
  if Paused then
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
  Assert(aActionFlag in [raf_StartIncreaseRR, raf_StartDecreaseRR, raf_StopChangingRR]);

  if Paused then begin
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
  if Paused then
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

  // this is possible in debugmode but don't know if I keep it that way
  if Paused then
    item.ActionFlags := item.ActionFlags or raf_Pausing;

  item.LemmingIndex := L.ListIndex;
  item.ReleaseRate := CurrReleaseRate;
  item.AssignedSkill := Byte(aSkill); // the byte is "compatible" for now
  item.LemmingX := L.xPos;
  item.LemmingY := L.yPos;
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
  Assert(askill in [TSkillPanelButton.Climber, TSkillPanelButton.Umbrella, TSkillPanelButton.Explode, TSkillPanelButton.Blocker, TSkillPanelButton.Builder, TSkillPanelButton.Basher, TSkillPanelButton.Miner, TSkillPanelButton.Digger]);

  if Paused then begin
    Assert(Recorder.List.Count > 0);
    R := Recorder.List.LastOrDefault; //Items[Recorder.List.Count - 1];
    // never overwrite startpause
    if not Assigned(R) or (R.ActionFlags and raf_StartPause <> 0) then
      R := Recorder.Add;
  end
  else
    R := Recorder.Add;

  R.Iteration := CurrentIteration;
  R.ActionFlags := R.ActionFlags or raf_SkillSelection;
  if Paused then
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
// this is bad code but works for now
// all records with the same iterationnumber must be
// handled here in one atomic moment
var
  R: TReplayItem;
  Last: Integer;
begin
  if not fReplaying then
    Exit;
  Last := fRecorder.List.Count - 1;

  if fReplayIndex > Last then
    fLastRecordedRecordReached := True;

  // although it may not be possible to have 2 replay-actions at one iteration we use a while loop: it's the safest method
  while fReplayIndex <= Last do begin
    R := fRecorder.List[fReplayIndex];
    // break if we go beyond the current iteration
    if R.Iteration <> CurrentIteration then
      Break;
    if raf_Nuke and r.actionflags <> 0 then
      SetSelectedSkill(TSkillPanelButton.Nuke, True);
    if raf_skillassignment and r.actionflags <> 0 then
      ReplaySkillAssignment(R);
    if raf_skillselection and r.actionflags <> 0 then
      ReplaySkillSelection(R);
    if raf_stopchangingRR and r.actionflags <> 0 then begin
      SetSelectedSkill(TSkillPanelButton.Faster, False);
      SetSelectedSkill(TSkillPanelButton.Slower, False);
      if (R.ReleaseRate <> 0) and (R.ReleaseRate <> CurrReleaseRate) then
         AdjustReleaseRate(R.ReleaseRate - currReleaseRate);
    end
    else if raf_startincreaserr and r.actionflags <> 0 then
      SetSelectedSkill(TSkillPanelButton.Faster, True)
    else if raf_startdecreaserr and r.actionflags <> 0 then
      SetSelectedSkill(TSkillPanelButton.Slower, True);

    // check for changes (error)
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
  if LemmingList.IsEmpty then
    Exit;

  for CurrentLemming in LemmingList do begin
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
  GameResultRec.Cheated := fGameCheated;

  if TMechanic.NukeGlitch in fMechanics
  then GameResultRec.LemmingCount := MaxNumLemmings // this is the glitch
  else GameResultRec.LemmingCount := Level.Info.LemmingsCount;

  GameResultRec.ToRescue := Level.Info.RescueCount;
  GameResultRec.Rescued := LemmingsIn;

  if Level.Info.LemmingsCount = 0
  then GameResultRec.Target := 0
  else GameResultRec.Target := (GameResultRec.ToRescue * 100) div Level.Info.LemmingsCount;


  if GameResultRec.LemmingCount = 0
  then GameResultRec.Done := 0
  else GameResultRec.Done := (GameResultRec.Rescued * 100) div GameResultRec.LemmingCount;

  if GameResultRec.Cheated then
    GameResultRec.Done := 100;

  GameResultRec.Success := GameResultRec.Done >= GameResultRec.Target;
end;

procedure TLemmingGame.RegainControl;
//  Key routine. It jumps from replay into usercontrol.
begin
  if fReplaying then begin
    fReplaying := False;
    fRecorder.Truncate(fReplayIndex);
    // special case: if the game is paused and the control is regained we have to insert a startpause record.
    if Paused then
      RecordStartPause;
    fReplayIndex := 0; // Recorder.List.Count - 1; ??? todo: CHECK THIS with the original
  end;
end;

procedure TLemmingGame.HyperSpeedBegin;
begin
  Inc(fHyperSpeedCounter);
  fHyperSpeed := True;
  FastForward := False;
end;

procedure TLemmingGame.HyperSpeedEnd;
begin
  if fHyperSpeedCounter > 0 then begin
    Dec(fHyperSpeedCounter);
    if fHyperSpeedCounter = 0 then
      fLeavingHyperSpeed := True;
  end;
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
// when paused, this is not called. we really should change the code
begin
  if SpeedingUpReleaseRate then
    AdjustReleaseRate(1)
  else if SlowingDownReleaseRate then
    AdjustReleaseRate(-1)
end;

procedure TLemmingGame.SetSoundOpts(const Value: TSoundOptions);
begin
  if fSoundOpts = Value then
    Exit;
  fSoundOpts := Value;
  if not (TSoundOption.Music in fSoundOpts) then
    SoundMgr.StopMusic(MUSIC_INDEX)
  else
    SoundMgr.PlayMusic(MUSIC_INDEX)
end;

procedure TLemmingGame.Terminate;
begin
  fGameFinished := True;
  SoundMgr.StopMusic(MUSIC_INDEX);
  SoundMgr.ClearMusics;
end;


procedure TLemmingGame.Finish;
begin
  Terminate;
  if Assigned(fOnFinish) then
    fOnFinish(Self);
end;

procedure TLemmingGame.ChangeMusicVolume(up: Boolean);
begin
  var curr: Single := SoundMgr.GetMusicVolumne(MUSIC_INDEX);
  if up and (curr <= 0.9) then
    SoundMgr.SetMusicVolume(MUSIC_INDEX, curr + 0.1)
  else if (curr >= 0.1) then
    SoundMgr.SetMusicVolume(MUSIC_INDEX, curr - 0.1)
end;

procedure TLemmingGame.Cheat;
begin
  fGameCheated := True;
  Finish;
end;

procedure TLemmingGame.Save(includeGameResult: Boolean);
var
  path: string;
  basename: string;
begin
  path := Consts.PathToReplay;
  if not ForceDir(path) then
    Exit;
  basename := path +  StripInvalidFileChars(fLevel.Info.Title, True, True, True);
  if basename.isEmpty then
    basename := '(nottile)';

  Recorder.SaveToFile(basename + '.lrb');
  Recorder.SaveToTxt(basename + '.txt', includeGameResult);

  AddCustomMessage('Game saved');
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

  if not fUseGradientBridges then
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
  error := '';
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
  error := '';

  FillChar(header, SizeOf(header), 0);

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

  if header.ReplayRecordSize <> SizeOf(TReplayFileHeaderRec) then begin
    error := deftext + 'Invalid replay header mismatch of replay-item record size';
    Exit;
  end;

  if (header.Version < 1)  or (header.Version > LEMMIX_REPLAY_VERSION) then begin
    error := deftext + 'Invalid replay header version (' + IntToStr(header.Version) + ').';
    exit;
  end;

  fCurrentMechanics := header.Mechanics;

  if header.Version > 1 then
    fRecordedGlitchPauseIterations := header.ReplayGlitchPauseIterations;

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
    if header.Version > 1 then
      item.HasValidCursorData := True
    else begin
      item.HasValidCursorData :=
        (item.CursorX > item.LemmingX - 8)
        and (item.CursorX < item.LemmingX + 8)
        and (item.CursorY > item.LemmingY - 8)
        and (item.CursorY < item.LemmingY + 8);
    end;

    if header.Version > 2 then
      item.Flags := rec.Flags;

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

    if List.Count >= header.ReplayRecordCount then
      Break;
  end;

  fWasLoaded := True;
  fCurrentHeader := header;

  Result := True;
end;

procedure TRecorder.SaveToFile(const aFileName: string);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TRecorder.SaveToStream(S: TStream);
var
  header: TReplayFileHeaderRec;
  rec: TReplayRec;
begin

  FillChar(header, SizeOf(TReplayFileHeaderRec), 0);

  header.Signature := 'LRB';
  header.Version := LEMMIX_REPLAY_VERSION;
  header.FileSize := SizeOf(TReplayFileHeaderRec) + SizeOf(TReplayRec) * List.Count;
  header.HeaderSize := SizeOf(TReplayFileHeaderRec);
  header.Mechanics := fGame.Mechanics + [TMechanic.Obsolete];
  header.FirstRecordPos := header.HeaderSize;
  header.ReplayRecordSize := SizeOf(TReplayFileHeaderRec);
  header.ReplayRecordCount := List.Count;
  header.Hash := fGame.fLevelLoadingInfo.GetLevelHash;
  header.ReplayGlitchPauseIterations := fGame.GlitchPauseIterations;

  for var t := 1 to 32 do
    header.LevelTitle := ' ';
  for var i := 1 to fGame.Level.Info.Title.Length do
    header.LevelTitle[i - 1] := AnsiChar(fGame.Level.Info.Title[i]);

  S.WriteBuffer(header, SizeOf(TReplayFileHeaderRec));

  for var item: TReplayItem in List do begin
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
  L: TStringList;
  mech: TMechanics;
  hash: UInt64;

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
     L.add(s);
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
  L := TStringList.Create;
  mech := fGame.Mechanics;
  hash := fGame.fLevelLoadingInfo.GetLevelHash;

  var s: string := 'Lemmix Replay Textfile recorded with ' + Consts.FullProgramName;
  AddString(s);
  AddString(StringOfChar('-', s.Length));
  AddString('Title: ' + Trim(fGame.Level.Info.Title));
  AddString('Replay fileversion: ' + IntToStr(LEMMIX_REPLAY_VERSION));
  AddString('Number of records: ' + IntToStr(List.count));
  AddString('Levelhash (decimal): ' + hash.ToString);
  AddString('Levelhash (hex): ' + IntToHex(hash, 16));

  if includeGameResult then begin
    AddString('');
    AddString('Game Result');
    AddString(StringOfChar('-', 64));
    AddString('Succes         : ' + YesNo(fGame.GameResultRec.Success));
    AddString('Cheated        : ' + YesNo(fGame.GameResultRec.Cheated));
    AddString('LemmingCount   : ' + fGame.GameResultRec.LemmingCount.ToString);
    AddString('To rescue      : ' + fGame.GameResultRec.ToRescue.ToString);
    AddString('Rescued        : ' + fGame.GameResultRec.Rescued.ToString);
    AddString('Target         : ' + fGame.GameResultRec.Target.ToString + '%');
    AddString('Done           : ' + fGame.GameResultRec.Done.ToString + '%');
    AddString('Time is up     : ' + YesNo(fGame.GameResultRec.TimeIsUp));
  end;

  AddString('');
  AddString('Mechanics');
  AddString(StringOfChar('-', 64));
  AddString(mech.AsText(True));
  AddString('ReplayPauseGlitchIterations         : ' + fRecordedGlitchPauseIterations.ToString);
  AddString('');

  AddString(' Rec   Frame  Pausing Action        Skill     Button     RR  lem    x    y   mx   my prio MouseGlitch');
  AddString(StringOfChar('-', 100));

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

  if ForceDir(aFileName) then
    L.SaveToFile(aFileName);

  L.free;
end;

procedure TRecorder.Truncate(aCount: Integer);
begin
  List.Count := aCount;
end;

class function TRecorder.LoadTitleAndHashFromHeader(const aFileName: string; out aHash: UInt64; out aTitle: TLVLTitle): Boolean;
var
  H: TReplayFileHeaderRec;
  F: TBufferedFileStream;
begin
  aHash := 0;
  F := TBufferedFileStream.Create(aFileName, fmOpenRead);
  try
    if F.Read(H, SizeOf(H)) <> SizeOf(H) then
      Exit(False);
    if H.Signature <> 'LRB' then
      Exit(False);
    aTitle := H.LevelTitle;
    if H.Version > 1 then
      aHash := H.Hash;
    Result := True;
  finally
    F.Free;
  end;
end;

procedure TLemmingGame.SaveCurrentFrameToPng;
// todo: do this async?
var
  fileName: string;
begin
  filename := Consts.PathToScreenShots + fStyle.Name + '_' + fLevelLoadingInfo.Section.SectionName + fLevelLoadingInfo.LevelIndex.ToString + '.png';
  if ForceDir(filename) then begin
    fTargetBitmap.SaveToPng(fileName);
    var png: TPngImage := fTargetBitmap.ToPng;
    try
      try
        png.SaveToFile(fileName);
        Clipboard.Assign(png);
        // todo: (option) save details about level (directory, style, levelloadinginfo)
      except
        AddCustomMessage('screenshot fail');
      end;
    finally
      png.Free;
    end;

    AddCustomMessage('screenshot');
  end
  else
    AddCustomMessage('screenshot fail');
end;

end.



