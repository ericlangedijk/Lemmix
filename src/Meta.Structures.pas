unit Meta.Structures;

{$include lem_directives.inc}

// translation of dos.structures for terrain and interactive objects

interface

uses
  Base.Utils,
  Dos.Structures;

const
  // Object Animation Types
  oat_None                     = 0;    // the object is not animated
  oat_Triggered                = 1;    // the object is triggered by a lemming
  oat_Continuous               = 2;    // the object is always moving
  oat_Once                     = 3;    // the object is animated once at the beginning (entrance only)

  // Object Trigger Effects: equal to the data inside the DOS files
  ote_None                     = 0;    // no effect (harmless)
  ote_Exit                     = 1;    // lemming reaches the exit
  ote_BlockerLeft              = 2;    // not used in DOS metadata, but reserved for 2D-objectmap
  ote_BlockerRight             = 3;    // not used in DOS metadata, but reserved for 2D-objectmap
  ote_TriggeredTrap            = 4;    // trap that animates once and kills a lemming
  ote_Drown                    = 5;    // used for watertype objects
  ote_Vaporize                 = 6;    // desintegration
  ote_OneWayWallLeft           = 7;    // bash en mine restriction (arrows)
  ote_OneWayWallRight          = 8;    // bash en mine restriction (arrows)
  ote_Steel                    = 9;    // not used by DOS metadata, but reserved for 2D-objectmap

  // Object Sound Effects: equal to the data inside the DOS files
  ose_None                     = 0;    // no sound effect
  ose_SkillSelect              = 1;    // the sound you get when you click on one of the skill icons at the bottom of the screen
  ose_Entrance                 = 2;    // entrance opening (sounds like "boing")
  ose_LevelIntro               = 3;    // level intro (the "let's go" sound)
  ose_SkillAssign              = 4;    // the sound you get when you assign a skill to lemming
  ose_OhNo                     = 5;    // the "oh no" sound when a lemming is about to explode
  ose_ElectroTrap              = 6;    // sound effect of the electrode trap and zap trap,
  ose_SquishingTrap            = 7;    // sound effect of the rock squishing trap, pillar squishing trap, and spikes trap
  ose_Splattering              = 8;    // the "aargh" sound when the lemming fall down too far and splatters
  ose_RopeTrap                 = 9;    // sound effect of the rope trap and slicer trap
  ose_HitsSteel                = 10;   // sound effect when a basher/miner/digger hits steel
  ose_Unknown                  = 11;   // ? (not sure where used in game)
  ose_Explosion                = 12;   // sound effect of a lemming explosion
  ose_SpinningTrap             = 13;   // sound effect of the spinning-trap-of-death, coal pits, and fire shooters (when a lemming touches the object and dies)
  ose_TenTonTrap               = 14;   // sound effect of the 10-ton trap
  ose_BearTrap                 = 15;   // sound effect of the bear trap
  ose_Exit                     = 16;   // sound effect of a lemming exiting
  ose_Drowning                 = 17;   // sound effect of a lemming dropping into water and drowning
  ose_BuilderWarning           = 18;   // sound effect for the last 3 bricks a builder is laying down


type
  TLemmingAnimationType = (
    Loop = 0,
    Once = 1
  );

  // This class is an abstract ancestor
  TAbstractMetaAnimation = class abstract
  strict private
    fDescription     : string;  // for fun, really
    fFrameCount      : Integer; // number of frames
    fWidth           : Integer; // width of a single frame picture
    fHeight          : Integer; // height of a single frame
    fImageLocation   : Integer; // DOS: data location in main.dat
    fBitsPerPixel    : Integer; // DOS bitmap
  public
    constructor Create(const aDescription: string; aFrameCount, aWidth, aHeight, aBitsPerPixel, aImageLocation: Integer);
    property Description      : string read fDescription;
    property FrameCount       : Integer read fFrameCount;
    property Width            : Integer read fWidth;
    property Height           : Integer read fHeight;
    property BitsPerPixel     : Integer read fBitsPerPixel;
    property ImageLocation    : Integer read fImageLocation;
  end;

  // This class describes lemming animation metadata
  TMetaLemmingAnimation = class(TAbstractMetaAnimation)
  strict private
    fAnimationType : TLemmingAnimationType;
    fFootX         : Integer;
    fFootY         : Integer;
  public
    constructor Create(const aDescription: string; aFrameCount, aWidth, aHeight, aBitsPerPixel, aImageLocation: Integer; aAnimationType: TLemmingAnimationType; aFootX, aFootY: Integer);
    property AnimationType : TLemmingAnimationType read fAnimationType;
    property FootX         : Integer read fFootX;
    property FootY         : Integer read fFootY;
  end;

  {-------------------------------------------------------------------------------
    This class describes extra animation metadata (masks and countdown digits)
  -------------------------------------------------------------------------------}
  TMetaExtraAnimation = class(TAbstractMetaAnimation);

  {-------------------------------------------------------------------------------
    This class describes terrain metadata
  -------------------------------------------------------------------------------}
  TMetaTerrain = class sealed
  private
    fWidth          : Integer; // the width of the bitmap
    fHeight         : Integer; // the height of the bitmap
    fImageLocation  : Integer; // DOS: data location of image in vgagr??.dat
  public
    procedure AssignFromDos(const ter: TDosMetaTerrain);
  // properties
    property Width         : Integer read fWidth write fWidth;
    property Height        : Integer read fHeight write fHeight;
    property ImageLocation : Integer read fImageLocation write fImageLocation;
  end;

  // This class describes interactive objects metadata
  TMetaObject = class sealed
  private
    fAnimationType                : Integer; // oat_xxxx
    fStartAnimationFrameIndex     : Integer; // frame with which the animation starts?
    fAnimationFrameCount          : Integer; // number of animations
    fWidth                        : Integer; // the width of the bitmap
    fHeight                       : Integer; // the height of the bitmap
    fAnimationFrameDataSize       : Integer; // DOS: the datasize in bytes of one frame
    fMaskOffsetFromImage          : Integer; // DOS: the datalocation of the mask-bitmap relative to the animation
    fTriggerLeft                  : Integer; // x-offset of triggerarea (if triggered animationtype)
    fTriggerTop                   : Integer; // y-offset of triggerarea (if triggered animationtype)
    fTriggerWidth                 : Integer; // width of triggerarea (if triggered animationtype)
    fTriggerHeight                : Integer; // height of triggerarea (if triggered animationtype)
    fTriggerEffect                : Integer; // ote_xxxx
    fAnimationFramesBaseLoc       : Integer; // DOS: data location of first frame in file (vgagr??.dat)
    fPreviewFrameIndex            : Integer; // index of preview (previewscreen)
    fSoundEffect                  : Integer; // ose_xxxx what sound to play
  public
    procedure AssignFromDos(const obj: TDosMetaObject);
  // properties
    property AnimationType            : Integer read fAnimationType;
    property StartAnimationFrameIndex : Integer read fStartAnimationFrameIndex;
    property AnimationFrameCount      : Integer read fAnimationFrameCount;
    property Width                    : Integer read fWidth;
    property Height                   : Integer read fHeight;
    property AnimationFrameDataSize   : Integer read fAnimationFrameDataSize;
    property MaskOffsetFromImage      : Integer read fMaskOffsetFromImage;
    property TriggerLeft              : Integer read fTriggerLeft;
    property TriggerTop               : Integer read fTriggerTop;
    property TriggerWidth             : Integer read fTriggerWidth;
    property TriggerHeight            : Integer read fTriggerHeight;
    property TriggerEffect            : Integer read fTriggerEffect;
    property AnimationFramesBaseLoc   : Integer read fAnimationFramesBaseLoc write fAnimationFramesBaseLoc;
    property PreviewFrameIndex        : Integer read fPreviewFrameIndex write fPreviewFrameIndex;
    property SoundEffect              : Integer read fSoundEffect write fSoundEffect;
  end;

  TMetaLemmingAnimationList = class(TFastObjectList<TMetaLemmingAnimation>);
  TMetaExtraAnimationList = class(TFastObjectList<TMetaExtraAnimation>);
  TMetaTerrainList = class(TFastObjectList<TMetaTerrain>);
  TMetaObjectList = class(TFastObjectList<TMetaObject>);

implementation

{ TAbstractMetaAnimation }

constructor TAbstractMetaAnimation.Create(const aDescription: string; aFrameCount, aWidth, aHeight, aBitsPerPixel, aImageLocation: Integer);
begin
  fDescription    := aDescription;
  fFrameCount     := aFrameCount;
  fWidth          := aWidth;
  fHeight         := aHeight;
  fImageLocation  := aImageLocation;
  fBitsPerPixel   := aBitsPerPixel;
end;

{ TMetaLemmingAnimation }

constructor TMetaLemmingAnimation.Create(const aDescription: string; aFrameCount, aWidth, aHeight, aBitsPerPixel, aImageLocation: Integer;
  aAnimationType: TLemmingAnimationType; aFootX, aFootY: Integer);
begin
  inherited Create(aDescription, aFrameCount, aWidth, aHeight, aBitsPerPixel, aImageLocation);
  fAnimationType := aAnimationType;
  fFootX := aFootX;
  fFootY := aFootY;
end;

{ TMetaTerrain }

procedure TMetaTerrain.AssignFromDos(const ter: TDosMetaTerrain);
begin
  fWidth         := ter.tWidth;
  fHeight        := ter.tHeight;
  fImageLocation := ter.tImage_loc;
end;

{ TMetaObject }

procedure TMetaObject.AssignFromDos(const obj: TDosMetaObject);
begin
  fAnimationType            := obj.oAnimation_flags;
  fStartAnimationFrameIndex := obj.oStart_animation_frame_index;
  fAnimationFrameCount      := obj.oAnimation_frame_count;
  fAnimationFrameDataSize   := obj.oAnimation_frame_data_size;
  fMaskOffsetFromImage      := obj.oMask_offset_from_image;
  fWidth                    := obj.oWidth;
  fHeight                   := obj.oHeight;
  fTriggerLeft              := obj.oTrigger_left * 4; // encoded
  fTriggerTop               := obj.oTrigger_top * 4 - 4; // encoded
  fTriggerWidth             := obj.oTrigger_width * 4; // encoded
  fTriggerHeight            := obj.oTrigger_height * 4; // encoded
  fTriggerEffect            := obj.oTrigger_effect_id;
  fAnimationFramesBaseLoc   := obj.oAnimation_frames_base_loc;
  fPreviewFrameIndex        := (obj.oPreview_image_location - obj.oAnimation_frames_base_loc) div obj.oAnimation_frame_data_size;
  fSoundEffect              := obj.oSound_effect_id;
end;

end.

