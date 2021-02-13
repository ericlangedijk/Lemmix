unit Base.Types;

{$include lem_directives.inc}

// some shared types

interface

uses
  System.Types, System.SysUtils,
  Vcl.Forms,
  Base.Utils;

type
  TStyleFamily = (
    DOS,
    Lemmini
  );

  TStyleDef = (
    Orig, Ohno, H94, X91, X92, User
  );
  TStyleDefs = set of TStyleDef;

  TStyleDefHelper = record helper for TStyleDef
  public
    function Name: string; inline;
    class function IsDefaultName(const aName: string): Boolean; static;
  end;

const
  DefaultStyles = [TStyleDef.Orig, TStyleDef.Ohno, TStyleDef.H94, TStyleDef.X91, TStyleDef.X92];
  AllStyles = [Low(TStyleDef)..High(TStyleDef)];

  NAME_ORIG           = 'Orig';
  NAME_OHNO           = 'Ohno';
  NAME_H94            = 'H94';
  NAME_X91            = 'X91';
  NAME_X92            = 'X92';
  NAME_USER           = 'User';

  STYLE_NAMES: array[TStyleDef] of string = (NAME_ORIG, NAME_OHNO, NAME_H94, NAME_X91, NAME_X92, NAME_USER);

type
  TGameResultsRec = record
    LemmingCount       : Integer; // total number
    ToRescue           : Integer;
    Rescued            : Integer;
    Target             : Integer; // percentage
    Done               : Integer; // percentage
    Success            : Boolean; // level played successfully?
    Cheated            : Boolean; // level cheated?
    TimeIsUp           : Boolean;
  end;

  TGameScreenType = (
    Unknown      = 1000, // no result
    Menu         = 1001,
    Preview      = 1002,
    Play         = 1003,
    Postview     = 1004,
    LevelCode    = 1005,
    Interrupted  = 1006, // pseudo
    ExitProgram  = 1007, // pseudo
    Restart      = 1008  // pseudo
  );

  TMusicStreamType = (
    None,
    &MOD,
    MP3
  );

  TSoundOption = (
    Sound,
    Music
  );
  TSoundOptions = set of TSoundOption;

  TSoundOptionHelper = record helper for TSoundOption
  public
    function AsString: string;
  end;

  TSoundOptionsHelper = record helper for TSoundOptions
  public
    const ALL = [Low(TSoundOption)..High(TSoundOption)];
    const DEFAULT = [
      TSoundOption.Sound,
      TSoundOption.Music
    ];
  public
    function Get(opt: TSoundOption): Boolean; inline;
    procedure &Set(opt: TSoundOption; value: Boolean); inline;
  public
    property Sound: Boolean index TSoundOption.Sound read Get;
    property Music: Boolean index TSoundOption.Music read Get;
  end;

  TFormOption = (
    Opaque,
    FullScreen,
    FadeOut,
    ShowDefaultCursor
  );
  TFormOptions = set of TFormOption;

  TFormOptionHelper = record helper for TFormOption
  public
    function AsString: string;
  end;

  TFormOptionsHelper = record helper for TFormOptions
  public
    const ALL = [Low(TFormOption)..High(TFormOption)];
    const DEFAULT = [
      TFormOption.ShowDefaultCursor
    ];
  public
    function Get(opt: TFormOption): Boolean; inline;
    procedure &Set(opt: TFormOption; value: Boolean); inline;
  public
  // easy access
    property Opaque: Boolean index TFormOption.Opaque read Get write &Set;
    property FullScreen: Boolean index TFormOption.FullScreen read Get write &Set;
    property FadeOut: Boolean index TFormOption.FadeOut read Get write &Set;
    property ShowDefaultCursor: Boolean index TFormOption.ShowDefaultCursor read Get write &Set;
  end;

  TGameOption = (
    AlwaysRegainControlOnMouseClick,
    CheatKeyToSolveLevel,
    ColorizeLemmings,
    FullCPU,
    GradientBridges,
    HighResolutionGameMessages,
    HighlightedPauseButton,
    ShowFeedbackMessages,
    ShowParticles,
    ShowPhotoFlashReplayEffect,
    ShowReplayCursor,
    ShowReplayMessages,
    ShowReplayTextInToolBar,
    SkillAssignmentsEnabledWhenPaused,
    SkillButtonsEnabledWhenPaused
  );
  TGameOptions = set of TGameOption;

  TGameOptionHelper = record helper for TGameOption
  public
    function AsString: string;
  end;

  TGameOptionsHelper = record helper for TGameOptions
    const ALL = [Low(TGameOption)..High(TGameOption)];
    const DEFAULT = [
      TGameOption.AlwaysRegainControlOnMouseClick,
      TGameOption.GradientBridges,
      TGameOption.HighResolutionGameMessages,
      TGameOption.ShowFeedbackMessages,
      TGameOption.ShowParticles,
      TGameOption.ShowReplayCursor,
      TGameOption.ShowReplayMessages,
      TGameOption.ShowReplayTextInToolBar,
      TGameOption.SkillButtonsEnabledWhenPaused
    ];
  public
    function Get(opt: TGameOption): Boolean; inline;
    procedure &Set(opt: TGameOption; value: Boolean); inline;
  public
    property AlwaysRegainControlOnMouseClick    : Boolean index TGameOption.AlwaysRegainControlOnMouseClick read Get write &Set;
    property CheatKeyToSolveLevel               : Boolean index TGameOption.CheatKeyToSolveLevel read Get write &Set;
    property ColorizeLemmings                   : Boolean index TGameOption.ColorizeLemmings read Get write &Set;
    property FullCPU                            : Boolean index TGameOption.FullCPU read Get write &Set;
    property GradientBridges                    : Boolean index TGameOption.GradientBridges read Get write &Set;
    property HighResolutionGameMessages         : Boolean index TGameOption.HighResolutionGameMessages read Get write &Set;
    property HighlightedPauseButton             : Boolean index TGameOption.HighlightedPauseButton read Get write &Set;
    property ShowFeedbackMessages               : Boolean index TGameOption.ShowFeedbackMessages read Get write &Set;
    property ShowParticles                      : Boolean index TGameOption.ShowParticles read Get write &Set;
    property ShowPhotoFlashReplayEffect         : Boolean index TGameOption.ShowPhotoFlashReplayEffect read Get write &Set;
    property ShowReplayCursor                   : Boolean index TGameOption.ShowReplayCursor read Get write &Set;
    property ShowReplayMessages                 : Boolean index TGameOption.ShowReplayMessages read Get write &Set;
    property ShowReplayTextInToolBar            : Boolean index TGameOption.ShowReplayTextInToolBar read Get write &Set;
    property SkillAssignmentsEnabledWhenPaused  : Boolean index TGameOption.SkillAssignmentsEnabledWhenPaused read Get write &Set;
    property SkillButtonsEnabledWhenPaused      : Boolean index TGameOption.SkillButtonsEnabledWhenPaused read Get write &Set;
  end;

  TVoiceOption = (
    BinLevelSaved,
    BinLevelSavingOff,
    BinLevelSavingOn,
    Cheater,
    CurrentSection,
    CurrentStyle,
    GameSaved,
    ReplayFail,
    SoundFX,
    StartReplay,
    VoiceDisable,
    VoiceEnable
  );
  TVoiceOptions = set of TVoiceOption;

  TVoiceOptionHelper = record helper for TVoiceOption
  public
    function AsString: string;
  end;

  TVoiceOptionsHelper = record helper for TVoiceOptions
  public
    const ALL = [Low(TVoiceOption).. High(TVoiceOption)];
    const DEFAULT = ALL;
  public
    function Get(opt: TVoiceOption): Boolean; inline;
    procedure &Set(opt: TVoiceOption; value: Boolean); inline;
  public
    property BinLevelSaved        : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property BinLevelSavingOff    : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property BinLevelSavingOn     : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property Cheater              : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property CurrentSection       : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property CurrentStyle         : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property GameSaved            : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property ReplayFail           : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property SoundFX              : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property StartReplay          : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property VoiceDisable         : Boolean index TVoiceOption.StartReplay read Get write &Set;
    property VoiceEnable          : Boolean index TVoiceOption.StartReplay read Get write &Set;
  end;

  TMiscOption = (
    AdjustLogoInMenuScreen,
    AlwaysOverwriteReplayFiles,
    AutoSaveReplayFiles,
    CheatCodesInLevelCodeScreen,
    CheatScrollingInPreviewScreen,
    FullCPU,
    KeepLevelRatioInPreviewScreen,
    LemmingsPercentages,
    MessageAfterSaveInResultScreen,
    RepairCustomLevelErrors,
    ShuffledMusic,
    UpdateReplayCacheWhenSaving,
    Voice
  );
  TMiscOptions = set of TMiscOption;

  TMiscOptionHelper = record helper for TMiscOption
  public
    function AsString: string;
  end;

  TMiscOptionsHelper = record helper for TMiscOptions
  public
    const ALL = [Low(TMiscOption)..High(TMiscOption)];
    const DEFAULT: TMiscOptions = [
      TMiscOption.AdjustLogoInMenuScreen,
      TMiscOption.AlwaysOverwriteReplayFiles,
      TMiscOption.LemmingsPercentages,
      TMiscOption.MessageAfterSaveInResultScreen,
      TMiscOption.RepairCustomLevelErrors,
      TMiscOption.UpdateReplayCacheWhenSaving,
      TMiscOption.Voice
    ];
  public
    function Get(opt: TMiscOption): Boolean; inline;
    procedure &Set(opt: TMiscOption; value: Boolean); inline;
  public
    property AdjustLogoInMenuScreen               : Boolean index TMiscOption.AdjustLogoInMenuScreen read Get write &Set;
    property AlwaysOverwriteReplayFiles           : Boolean index TMiscOption.AlwaysOverwriteReplayFiles read Get write &Set;
    property AutoSaveReplayFiles                  : Boolean index TMiscOption.AutoSaveReplayFiles read Get write &Set;
    property CheatCodesInLevelCodeScreen          : Boolean index TMiscOption.CheatCodesInLevelCodeScreen read Get write &Set;
    property CheatScrollingInPreviewScreen        : Boolean index TMiscOption.CheatScrollingInPreviewScreen read Get write &Set;
    property FullCPU                              : Boolean index TMiscOption.FullCPU read Get write &Set;
    property KeepLevelRatioInPreviewScreen        : Boolean index TMiscOption.KeepLevelRatioInPreviewScreen read Get write &Set;
    property LemmingsPercentages                  : Boolean index TMiscOption.LemmingsPercentages read Get write &Set;
    property MessageAfterSaveInResultScreen       : Boolean index TMiscOption.MessageAfterSaveInResultScreen read Get write &Set;
    property RepairCustomLevelErrors              : Boolean index TMiscOption.RepairCustomLevelErrors read Get write &Set;
    property ShuffledMusic                        : Boolean index TMiscOption.ShuffledMusic read Get write &Set;
    property UpdateReplayCacheWhenSaving          : Boolean index TMiscOption.UpdateReplayCacheWhenSaving read Get write &Set;
    property Voice                                : Boolean index TMiscOption.Voice read Get write &Set;
  end;

  // this one is for optional mechanics, which are inserted into the mechanics if set
  TOptionalMechanic = (
    NukeGlitch,
    PauseGlitch,
    RighClickGlitch
  );
  TOptionalMechanics = set of TOptionalMechanic;

  TOptionalMechanicHelper = record helper for TOptionalMechanic
  public
    function AsString: string;
  end;

  TOptionalMechanicsHelper = record helper for TOptionalMechanics
  public
    const ALL = [Low(TOptionalMechanic)..High(TOptionalMechanic)];
    const DEFAULT: TOptionalMechanics = [];
  public
    function Get(opt: TOptionalMechanic): Boolean; inline;
    procedure &Set(opt: TOptionalMechanic; value: Boolean); inline;
  public
    property NukeGlitch       : Boolean index TOptionalMechanic.NukeGlitch read Get write &Set;
    property PauseGlitch      : Boolean index TOptionalMechanic.PauseGlitch read Get write &Set;
    property RighClickGlitch  : Boolean index TOptionalMechanic.RighClickGlitch read Get write &Set;
  end;

implementation

{ TStyleDefHelper }

function TStyleDefHelper.Name: string;
begin
  Result := STYLE_NAMES[Self];
end;

class function TStyleDefHelper.IsDefaultName(const aName: string): Boolean;
begin
  for var style: TStyleDef in DefaultStyles do
    if SameText(style.Name, aName) then
      Exit(True);
  Result := False;
end;

{ TFormOptionHelper }

function TFormOptionHelper.AsString: string;
begin
  case Self of
    TFormOption.Opaque            : Result := 'Opaque';
    TFormOption.FullScreen        : Result := 'FullScreen';
    TFormOption.FadeOut           : Result := 'FadeOut';
    TFormOption.ShowDefaultCursor : Result := 'ShowDefaultCursor';
    else
      Result := Ord(Self).ToString;
  end;
end;

{ TFormOptionsHelper }

function TFormOptionsHelper.Get(opt: TFormOption): Boolean; // inline
begin
  Result := opt in Self;
end;

procedure TFormOptionsHelper.&Set(opt: TFormOption; value: Boolean);
begin
  case value of
    False : Exclude(Self, opt);
    True  : Include(Self, opt);
  end;
end;

{ TGameOptionHelper }

function TGameOptionHelper.AsString: string;
begin
  case Self of
    TGameOption.AlwaysRegainControlOnMouseClick     : Result := 'AlwaysRegainControlOnMouseClick';
    TGameOption.CheatKeyToSolveLevel                : Result := 'CheatKeyToSolveLevel';
    TGameOption.ColorizeLemmings                    : Result := 'ColorizeLemmings';
    TGameOption.FullCPU                             : Result := 'FullCPU';
    TGameOption.GradientBridges                     : Result := 'GradientBridges';
    TGameOption.HighResolutionGameMessages          : Result := 'HighResolutionGameMessages';
    TGameOption.HighlightedPauseButton              : Result := 'HighlightedPauseButton';
    TGameOption.ShowFeedbackMessages                : Result := 'ShowFeedbackMessages';
    TGameOption.ShowParticles                       : Result := 'ShowParticles';
    TGameOption.ShowPhotoFlashReplayEffect          : Result := 'ShowPhotoFlashReplayEffect';
    TGameOption.ShowReplayCursor                    : Result := 'ShowReplayCursor';
    TGameOption.ShowReplayMessages                  : Result := 'ShowReplayMessages';
    TGameOption.ShowReplayTextInToolBar             : Result := 'ShowReplayTextInToolBar';
    TGameOption.SkillAssignmentsEnabledWhenPaused   : Result := 'SkillAssignmentsEnabledWhenPaused';
    TGameOption.SkillButtonsEnabledWhenPaused       : Result := 'SkillButtonsEnabledWhenPaused';
    else
      Result := Ord(Self).ToString;
  end;
end;

{ TGameOptionsHelper }

function TGameOptionsHelper.Get(opt: TGameOption): Boolean;
begin
  Result := opt in Self;
end;

procedure TGameOptionsHelper.&Set(opt: TGameOption; value: Boolean);
begin
  case value of
    False : Exclude(Self, opt);
    True  : Include(Self, opt);
  end;
end;

{ TSoundOptionHelper }

function TSoundOptionHelper.AsString: string;
begin
  case Self of
    TSoundOption.Sound : Result := 'Sound';
    TSoundOption.Music : Result := 'Music';
    else
      Result := Ord(Self).ToString;
  end;
end;

{ TSoundOptionsHelper }

function TSoundOptionsHelper.Get(opt: TSoundOption): Boolean;
begin
  Result := opt in Self;
end;

procedure TSoundOptionsHelper.&Set(opt: TSoundOption; value: Boolean);
begin
  case value of
    False : Exclude(Self, opt);
    True  : Include(Self, opt);
  end;
end;

{ TOptionalMechanicHelper }

function TOptionalMechanicHelper.AsString: string;
begin
  case Self of
    TOptionalMechanic.NukeGlitch       : Result := 'NukeGlitch';
    TOptionalMechanic.PauseGlitch      : Result := 'PauseGlitch';
    TOptionalMechanic.RighClickGlitch  : Result := 'RighClickGlitch';
    else
      Result := Ord(Self).ToString;
  end;
end;

{ TVoiceOptionHelper }

function TVoiceOptionHelper.AsString: string;
begin
  case Self of
    TVoiceOption.BinLevelSaved       : Result := 'BinLevelSaved';
    TVoiceOption.BinLevelSavingOff   : Result := 'BinLevelSavingOff';
    TVoiceOption.BinLevelSavingOn    : Result := 'BinLevelSavingOn';
    TVoiceOption.Cheater             : Result := 'Cheater';
    TVoiceOption.CurrentSection      : Result := 'CurrentSection';
    TVoiceOption.CurrentStyle        : Result := 'CurrentStyle';
    TVoiceOption.GameSaved           : Result := 'GameSaved';
    TVoiceOption.ReplayFail          : Result := 'ReplayFail';
    TVoiceOption.SoundFX             : Result := 'SoundFX';
    TVoiceOption.StartReplay         : Result := 'StartReplay';
    TVoiceOption.VoiceDisable        : Result := 'VoiceDisable';
    TVoiceOption.VoiceEnable         : Result := 'VoiceEnable';
   else
      Result := Ord(Self).ToString;
  end;
end;

{ TVoiceOptionsHelper }

function TVoiceOptionsHelper.Get(opt: TVoiceOption): Boolean;
begin
  Result := opt in Self;
end;

procedure TVoiceOptionsHelper.&Set(opt: TVoiceOption; value: Boolean);
begin
  case value of
    False : Exclude(Self, opt);
    True  : Include(Self, opt);
  end;
end;

{ TMiscOptionHelper }

function TMiscOptionHelper.AsString: string;
begin
  case Self of
    TMiscOption.AdjustLogoInMenuScreen               : Result := 'AdjustLogoInMenuScreen';
    TMiscOption.AlwaysOverwriteReplayFiles           : Result := 'AlwaysOverwriteReplayFiles';
    TMiscOption.AutoSaveReplayFiles                  : Result := 'AutoSaveReplayFiles';
    TMiscOption.CheatCodesInLevelCodeScreen          : Result := 'CheatCodesInLevelCodeScreen';
    TMiscOption.CheatScrollingInPreviewScreen        : Result := 'CheatScrollingInPreviewScreen';
    TMiscOption.FullCPU                              : Result := 'FullCPU';
    TMiscOption.KeepLevelRatioInPreviewScreen        : Result := 'KeepLevelRatioInPreviewScreen';
    TMiscOption.LemmingsPercentages                  : Result := 'LemmingsPercentages';
    TMiscOption.MessageAfterSaveInResultScreen       : Result := 'MessageAfterSaveInResultScreen';
    TMiscOption.RepairCustomLevelErrors              : Result := 'RepairCustomLevelErrors';
    TMiscOption.ShuffledMusic                        : Result := 'ShuffledMusic';
    TMiscOption.UpdateReplayCacheWhenSaving          : Result := 'UpdateReplayCacheWhenSaving';
    TMiscOption.Voice                                : Result := 'Voice';
    else
      Result := Ord(Self).ToString;
  end;
end;

{ TMiscOptionsHelper }

function TMiscOptionsHelper.Get(opt: TMiscOption): Boolean;
begin
  Result := opt in Self;
end;

procedure TMiscOptionsHelper.&Set(opt: TMiscOption; value: Boolean);
begin
  case value of
    False : Exclude(Self, opt);
    True  : Include(Self, opt);
  end;
end;

{ TOptionalMechanicsHelper }

function TOptionalMechanicsHelper.Get(opt: TOptionalMechanic): Boolean;
begin
  Result := opt in Self;
end;

procedure TOptionalMechanicsHelper.&Set(opt: TOptionalMechanic; value: Boolean);
begin
  case value of
    False : Exclude(Self, opt);
    True  : Include(Self, opt);
  end;
end;

// todo: write checkmethod paranoid debug on strings etc.

end.

