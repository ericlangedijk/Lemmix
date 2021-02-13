unit Dos.Consts;

{$include lem_directives.inc}

{--------------------------------------------------------------------------------------------------------------------------------------------------------------
  Some difficult glitch descriptions from https://www.lemmingsforums.net/index.php?topic=1382.0

  "Nuke glitch":
  When the nuke is activated, the game calculates the percentage saved
  based on how many lemmings have been released, not how many are available in the level.

  "Pausing for time":
  The entrance hatch begins opening a specific length of time after the level starts,
  at the same time the sound effect plays.
  If you pause the game before the hatch begins to open, the timer stops counting down while the game is paused,
  but the hatch still opens at the same time (or immediately after you unpause,
  if you wait until after the sound plays). This gives you about two seconds extra to complete the level.


  "Right-click bug":
  If you highlight a lemming inside the cursor, then mouse over a lemming that is performing a skill,
  and then left-click that lemming while holding the right mouse button, the previously highlighted lemming will perform whatever skill is currently selected,
  even if it is outside the cursor.

  If the blocker skill is selected, and highlighted lemming dies in a trap before the left-click,
  the blocker skill will still be used, and a blocker area will be placed over the trap.
--------------------------------------------------------------------------------------------------------------------------------------------------------------}

interface

uses
  System.Types, System.SysUtils,
  Base.Utils;

const
  DOS_MINIMAP_WIDTH  = 104;
  DOS_MINIMAP_HEIGHT = 20;

  DOS_FRAMES_PER_SECOND = 17;

const
  DosMiniMapCorners: TRect = (
    Left   : 208;
    Top    : 18;
    Right  : 311;
    Bottom : 37
  );

  // to draw
  DosMiniMapBounds: TRect = (
    Left   : 208;
    Top    : 18;
    Right  : 311 + 1;
    Bottom : 37 + 1
  );

const
  GAME_BMPWIDTH = 1584;
  GAME_BMPHEIGHT = 160;

  DOS_OBJECT_ID_EXIT  = 0; // interactive object id
  DOS_OBJECT_ID_ENTRANCE = 1; // interactive object id


{-----------------------------------------------------------------------------------
  we cannot get a nice minimapscale 1/16 so instead we chose the following:

  image width of game                 = 1584
  imagewidth of minimap               = 104
  width of white rectangle in minimap = 25

  width = 1664; // this is too far I think, but now it fits with the minimap!

  the original dos lemmings show pixels 0 through 1583 (so including pixel 1583)
  so the imagewidth should be 1584, which means 80 pixels less then 1664
-----------------------------------------------------------------------------------}

const
  clMask32  = $00FF00FF; // color used for "shape-only" masks

type
  TLemmingAction = (
    None,
    Walking,
    Jumping,
    Digging,
    Climbing,
    Drowning,
    Hoisting,
    Building,
    Bashing,
    Mining,
    Falling,
    Floating,
    Splatting,
    Exiting,
    Vaporizing,
    Blocking,
    Shrugging,
    Ohnoing,
    Exploding
  );

  TSkillPanelButton = (
    None,
    Slower,
    Faster,
    Climber,
    Umbrella,
    Explode,
    Blocker,
    Builder,
    Basher,
    Miner,
    Digger,
    Pause,
    Nuke
  );

const
  ACTION_BIT_WALKING     = 1 shl Ord(TLemmingAction.Walking);
  ACTION_BIT_JUMPING     = 1 shl Ord(TLemmingAction.Jumping);
  ACTION_BIT_DIGGING     = 1 shl Ord(TLemmingAction.Digging);
  ACTION_BIT_CLIMBING    = 1 shl Ord(TLemmingAction.Climbing);
  ACTION_BIT_DROWNING    = 1 shl Ord(TLemmingAction.Drowning);
  ACTION_BIT_HOISTING    = 1 shl Ord(TLemmingAction.Hoisting);
  ACTION_BIT_BUILDING    = 1 shl Ord(TLemmingAction.Building);
  ACTION_BIT_BASHING     = 1 shl Ord(TLemmingAction.Bashing);
  ACTION_BIT_MINING      = 1 shl Ord(TLemmingAction.Mining);
  ACTION_BIT_FALLING     = 1 shl Ord(TLemmingAction.Falling);
  ACTION_BIT_FLOATING    = 1 shl Ord(TLemmingAction.Floating);
  ACTION_BIT_SPLATTING   = 1 shl Ord(TLemmingAction.Splatting);
  ACTION_BIT_EXITING     = 1 shl Ord(TLemmingAction.Exiting);
  ACTION_BIT_VAPORIZING  = 1 shl Ord(TLemmingAction.Vaporizing);
  ACTION_BIT_BLOCKING    = 1 shl Ord(TLemmingAction.Blocking);
  ACTION_BIT_SHRUGGING   = 1 shl Ord(TLemmingAction.Shrugging);
  ACTION_BIT_OHNOING     = 1 shl Ord(TLemmingAction.Ohnoing);
  ACTION_BIT_EXPLODING   = 1 shl Ord(TLemmingAction.Exploding);

const
  AssignableSkills = [
    TLemmingAction.Digging,
    TLemmingAction.Climbing,
    TLemmingAction.Building,
    TLemmingAction.Bashing,
    TLemmingAction.Mining,
    TLemmingAction.Floating,
    TLemmingAction.Blocking,
    TLemmingAction.Exploding
  ];

const
  ActionToSkillPanelButton: array[TLemmingAction] of TSkillPanelButton = (
    TSkillPanelButton.None,
    TSkillPanelButton.None,
    TSkillPanelButton.None,
    TSkillPanelButton.Digger,
    TSkillPanelButton.Climber,
    TSkillPanelButton.None,
    TSkillPanelButton.None,
    TSkillPanelButton.Builder,
    TSkillPanelButton.Basher,
    TSkillPanelButton.Miner,
    TSkillPanelButton.None,
    TSkillPanelButton.Umbrella,
    TSkillPanelButton.None,
    TSkillPanelButton.None,
    TSkillPanelButton.None,
    TSkillPanelButton.Blocker,
    TSkillPanelButton.None,
    TSkillPanelButton.None,
    TSkillPanelButton.Explode
  );

const
  SkillPanelButtonToAction: array[TSkillPanelButton] of TLemmingAction = (
    TLemmingAction.None,
    TLemmingAction.None,
    TLemmingAction.None,
    TLemmingAction.Climbing,
    TLemmingAction.Floating,
    TLemmingAction.Exploding,
    TLemmingAction.Blocking,
    TLemmingAction.Building,
    TLemmingAction.Bashing,
    TLemmingAction.Mining,
    TLemmingAction.Digging,
    TLemmingAction.None,
    TLemmingAction.None
  );

type
  // NEVER change this. If items are added then AFTER RightClickGlitch.
  // It is stored in replayfiles (2 bytes. there is room for 3 more bits)
  TMechanic = (
    DisableObjectsAfter15,            // #0 objects with index higher than 15 will not work
    MinerOneWayRightBug,              // #1 the miner bug check
    Obsolete,                         // #2 deleted skillbutton option. never delete (replay stored).
    SplattingExitsBug,                // #3 ORIG only
    OldEntranceABBAOrder,             // #4 ORIG only
    EntranceX25,                      // #5 from OHNO
    FallerStartsWith3,                // #6 not in custlemm
    Max4EnabledEntrances,             // #7 always
    AssignClimberShruggerActionBug,   // #8 ORIG only
    TriggeredTrapLemmixBugSolved,     // #9 #EL 2020: bug in the old player solved. Trap started with frame #1 and should be #0
    NukeGlitch,                       // #10 #EL 2020: finally. see docs for details
    PauseGlitch,                      // #11 #EL 2020: finally. see docs for details
    RightClickGlitch                  // #11 #EL 2020: finally. see docs for details
  );
  TMechanics = set of TMechanic;

  TMechanicHelper = record helper for TMechanic
  public
    function AsString: string;
  end;

  TMechanicsHelper = record helper for TMechanics
  public
    const ALL = [Low(TMechanic)..High(TMechanic)];
  public
    function Get(const mech: TMechanic): Boolean; inline;
  public
    function AsText(align, useBullets, useTabs: Boolean): string;
  // easy access
    property DisableObjectsAfter15            : Boolean index TMechanic.DisableObjectsAfter15 read Get;
    property MinerOneWayRightBug              : Boolean index TMechanic.MinerOneWayRightBug read Get;
    property Obsolete                         : Boolean index TMechanic.Obsolete read Get;
    property SplattingExitsBug                : Boolean index TMechanic.SplattingExitsBug read Get;
    property OldEntranceABBAOrder             : Boolean index TMechanic.OldEntranceABBAOrder read Get;
    property EntranceX25                      : Boolean index TMechanic.EntranceX25 read Get;
    property FallerStartsWith3                : Boolean index TMechanic.FallerStartsWith3 read Get;
    property Max4EnabledEntrances             : Boolean index TMechanic.Max4EnabledEntrances read Get;
    property AssignClimberShruggerActionBug   : Boolean index TMechanic.AssignClimberShruggerActionBug read Get;
    property TriggeredTrapLemmixBugSolved     : Boolean index TMechanic.TriggeredTrapLemmixBugSolved read Get;
    property NukeGlitch                       : Boolean index TMechanic.NukeGlitch read Get;
    property PauseGlitch                      : Boolean index TMechanic.PauseGlitch read Get;
    property RightClickGlitch                 : Boolean index TMechanic.RightClickGlitch read Get;
  end;

const
  DOSORIG_MECHANICS = [
    TMechanic.DisableObjectsAfter15,
    TMechanic.MinerOneWayRightBug,
    TMechanic.Obsolete,                 // deleted skillbutton option. never delete (replay stored).
    TMechanic.SplattingExitsBug,
    TMechanic.OldEntranceABBAOrder,
    TMechanic.FallerStartsWith3,
    TMechanic.Max4EnabledEntrances,
    TMechanic.AssignClimberShruggerActionBug,
    TMechanic.TriggeredTrapLemmixBugSolved
  ];

  DOSOHNO_MECHANICS = [
    TMechanic.DisableObjectsAfter15,
    TMechanic.MinerOneWayRightBug,
    TMechanic.Obsolete,                 // deleted skillbutton option. never delete (replay stored).
    TMechanic.SplattingExitsBug,
    TMechanic.FallerStartsWith3,
    TMechanic.EntranceX25,
    TMechanic.Max4EnabledEntrances,
    TMechanic.TriggeredTrapLemmixBugSolved
  ];

  CUSTLEMM_MECHANICS = [
    TMechanic.DisableObjectsAfter15,
    TMechanic.MinerOneWayRightBug,
    TMechanic.Obsolete,                 // deleted skillbutton option. never delete (replay stored).
    TMechanic.SplattingExitsBug,
    TMechanic.EntranceX25,
    TMechanic.Max4EnabledEntrances,
    TMechanic.TriggeredTrapLemmixBugSolved
  ];

type
  TSoundEffect = (
    None,               // no sound effect
    BuilderWarning,     // last 3 steps of builder
    AssignSkill,        // click on lemming
    Yippee,             // lemming exits
    Splat,              // lemming falling on the ground
    LetsGo,             // lemming saying 'lets go' when the level starts
    EntranceOpening,    // entrance opening sound
    Vaporizing,         // lemming is vaporizing
    Drowning,           // lemming is drowing
    Explosion,          // lemming explodes
    HitsSteel,          // basher / miner / digger hits steel
    Ohno,               // just before lemming exploded (not when nuking)
    SkillButtonSelect,  // sound when we select a button in the skillpanel
    RopeTrap,           // trap sound
    TenTonTrap,         // trap sound
    BearTrap,           // trap sound
    ElectroTrap,        // trap sound
    SpinningTrap,       // trap sound
    SquishingTrap,      // trap sound
    Miner,              // custom sound
    Digger,             // custom sound
    Basher,             // custom sound
    OpenUmbrella,       // custom sound
    SilentDeath,        // custom sound
    Nuke                // custom sound
  );

  TSoundEffectHelper = record helper for TSoundEffect
  public
    function AsFileName(const ext: string): string;
    function IsCustom: Boolean; inline;
  end;

implementation

{ TSoundEffectHelper }

function TSoundEffectHelper.AsFileName(const ext: string): string;
begin
  // Result := Enum.AsString(Self) + ext;
  case Self of
    TSoundEffect.None              : Result := 'None';
    TSoundEffect.BuilderWarning    : Result := 'BuilderWarning';
    TSoundEffect.AssignSkill       : Result := 'AssignSkill';
    TSoundEffect.Yippee            : Result := 'Yippee';
    TSoundEffect.Splat             : Result := 'Splat';
    TSoundEffect.LetsGo            : Result := 'LetsGo';
    TSoundEffect.EntranceOpening   : Result := 'EntranceOpening';
    TSoundEffect.Vaporizing        : Result := 'Vaporizing';
    TSoundEffect.Drowning          : Result := 'Drowning';
    TSoundEffect.Explosion         : Result := 'Explosion';
    TSoundEffect.HitsSteel         : Result := 'HitsSteel';
    TSoundEffect.Ohno              : Result := 'Ohno';
    TSoundEffect.SkillButtonSelect : Result := 'SkillButtonSelect';
    TSoundEffect.RopeTrap          : Result := 'RopeTrap';
    TSoundEffect.TenTonTrap        : Result := 'TenTonTrap';
    TSoundEffect.BearTrap          : Result := 'BearTrap';
    TSoundEffect.ElectroTrap       : Result := 'ElectroTrap';
    TSoundEffect.SpinningTrap      : Result := 'SpinningTrap';
    TSoundEffect.SquishingTrap     : Result := 'SquishingTrap';
    TSoundEffect.Miner             : Result := 'Miner';
    TSoundEffect.Digger            : Result := 'Digger';
    TSoundEffect.Basher            : Result := 'Basher';
    TSoundEffect.OpenUmbrella      : Result := 'OpenUmbrella';
    TSoundEffect.SilentDeath       : Result := 'SilentDeath';
    TSoundEffect.Nuke              : Result := 'Nuke';
  else
    Result := 'SoundEffect' + Ord(Self).ToString;
  end;
  Result := Result + ext;
end;

function TSoundEffectHelper.IsCustom: Boolean;
begin
  Result := Self >= TSoundEffect.Miner;
end;

{ TMechanicHelper }

function TMechanicHelper.AsString: string;
begin
  case Self of
    TMechanic.DisableObjectsAfter15            : Result := 'Disabled objects after #15';
    TMechanic.MinerOneWayRightBug              : Result := 'Miner One Way Right Bug';
    TMechanic.Obsolete                         : Result := 'Obsolete';
    TMechanic.SplattingExitsBug                : Result := 'Splatting Exit Bug';
    TMechanic.OldEntranceABBAOrder             : Result := 'Old Entrance ABBA Order';
    TMechanic.EntranceX25                      : Result := 'Lemmings spawn at X=25';
    TMechanic.FallerStartsWith3                : Result := 'Faller starts with 3 Falling pixels';
    TMechanic.Max4EnabledEntrances             : Result := 'Max 4 Enabled Entrances';
    TMechanic.AssignClimberShruggerActionBug   : Result := 'Assign Climber Shrugger Action Bug';
    TMechanic.TriggeredTrapLemmixBugSolved     : Result := 'TriggeredTrap LemmixBug Solved';
    TMechanic.NukeGlitch                       : Result := 'Nuke Glitch';
    TMechanic.PauseGlitch                      : Result := 'Pause Glitch';
    TMechanic.RightClickGlitch                 : Result := 'Right Click Glitch';
    else
      Result := Ord(Self).ToString;
  end;
end;

{ TMechanicsHelper }

function TMechanicsHelper.Get(const mech: TMechanic): Boolean;
begin
  Result := mech in Self;
end;


function TMechanicsHelper.AsText(align, useBullets, useTabs: Boolean): string;
// used in replay.txt in info form
var
  bullet: string;
  sep: string;

    function MyYesNo(const b: Boolean): string;
    begin
      if b then Result := 'yes' else Result := 'no'; // do not localize
    end;

const
  // do not localize
  descriptions: array[TMechanic] of string = (
    'Disabled objects after #15',
    'Miner One Way Right Bug',
    'Obsolete (must be on)',
    'Splatting Exits Bug',
    'Old Entrance ABBA Order',
    'Lemmings spawn at X=25',
    'Faller starts with 3 Falling pixels', // 35 length
    'Max 4 Enabled Entrances',
    'Assign Climber Shrugger Action Bug',
    'TriggeredTrap LemmixBug Solved',
    'Optional Nuke Glitch',
    'Optional Pause Glitch',
    'Optional Right Click Glitch'
 );
begin
  if useBullets
  then bullet := '• '
  else bullet := string.Empty;

  if useTabs
  then sep := tab
  else sep := ' : ';

  Result := string.Empty;
  if not align then
    for var m: TMechanic in TMechanics.ALL do
      Result := Result + bullet + descriptions[m] + sep + MyYesNo(m in Self) + CRLF
  else
    for var m: TMechanic in TMechanics.ALL do
      Result := Result + bullet + descriptions[m].PadRight(35 + Length(bullet)) + sep + MyYesNo(m in Self) + CRLF;

  Result := Result.TrimRight;
end;

{$ifdef paranoid}
initialization
  Assert(Ord(High(TSoundEffect)) = 24, 'SoundEffect enum error');
{$endif}
end.


