unit Prog.Types;

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
    Orig, Ohno, H93, H94, X91, X92, User
  );
  TStyleDefs = set of TStyleDef;


  TStyleDefHelper = record helper for TStyleDef
  public
    function Name: string; inline;
    class function IsDefaultName(const aName: string): Boolean; static;
  end;

const
  DefaultStyles = [TStyleDef.Orig, TStyleDef.Ohno, TStyleDef.H93, TStyleDef.H94, TStyleDef.X91, TStyleDef.X92];
  AllStyles = [Low(TStyleDef)..High(TStyleDef)];

  NAME_ORIG           = 'Orig';
  NAME_OHNO           = 'Ohno';
  NAME_H93            = 'H93';
  NAME_H94            = 'H94';
  NAME_X91            = 'X91';
  NAME_X92            = 'X92';
  NAME_USER           = 'User';

  STYLE_NAMES: array[TStyleDef] of string = (NAME_ORIG, NAME_OHNO, NAME_H93, NAME_H94, NAME_X91, NAME_X92, NAME_USER);

type
  TGameResultsRec = record
    Success            : Boolean; // level played successfully?
    Cheated            : Boolean; // level cheated?
    LemmingCount       : Integer; // number
    ToRescue           : Integer;
    Rescued            : Integer;
    Target             : Integer;
    Done               : Integer;
    TimeIsUp           : Boolean;
  end;

  TGameScreenType = (
    Unknown      = 1000,
    Menu         = 1001,
    Preview      = 1002,
    Play         = 1003,
    Postview     = 1004,
    LevelCode    = 1005,
    Options      = 1006,
    Finder       = 1007,
    Config       = 1008,
    Interrupted  = 1009, // pseudo
    ExitProgram  = 1010  // pseudo
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

  TMiscOption = (
    GradientBridges,
    UseFastForward, // not implemented yet
    UseSaveStates, // not implemented yet
    UseHyperJumps, // not implemented yet
    UseCheatCodes,
    ShowParticles,
    UseFadeOut,
    UseCheatScrollingInPreviewScreen,
    UseCheatKeyToSolveLevel,
    UseFinder,
    ShowReplayTextInSkillPanel,
    ShowReplayMessages,
    ShowFeedbackMessages,
    ShowDefaultCursor,
    ShowReplayCursor,
    EnableSkillButtonsWhenPaused,
    RepairCustomLevelErrors,
    UseShuffledMusic,
    UsePhotoFlashReplayEffect
  );
  TMiscOptions = set of TMiscOption;

  TMiscOptionsHelper = record helper for TMiscOptions
  public
    function Contains(opt: TMiscOption): Boolean; inline;
  end;

  // this one is for optional mechanics, which are inserted into the mechanics if set
  TOptionalMechanic = (
    NukeGlitch,
    PauseGlitch,
    RighClickGlitch
  );
  TOptionalMechanics = set of TOptionalMechanic;

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

{ TMiscOptionsHelper }

function TMiscOptionsHelper.Contains(opt: TMiscOption): Boolean;
begin
  Result := opt in Self;
end;

end.

