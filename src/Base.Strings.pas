unit Base.Strings;

{$include lem_directives.inc}

// the only unit of lemmix with rtti because of translations
{$rtti explicit fields([vcpublic])}

interface

uses
  Winapi.Windows,
  Vcl.Consts,
  Base.Utils, Base.Types,
  Dos.Consts;

type
  TGlobalTexts = class sealed
  strict private
    procedure InitArrays;
  private
    procedure Init;
  public
    procedure Load(const filename: string);
    procedure Save(const filename: string);
  public

  // Menu Screen -----------------------------------------------------------------------------------------------------------------------------------------------

  const
    SProgramName = 'Lemmix';
    SCheatCode = 'elangedijk';

    SByEricLangedijk = 'By Eric Langedijk';
    SProgramTexts : array[TStyleDef] of string = (
      'Original Lemmings',
      'Oh No More Lemmings!',
      'Holiday Lemmings 94' ,
      'XMas Lemmings 91',
      'XMas Lemmings 92',
      '%s'
    );

    // Note: Max size for string that fits in the scrolling reel = 34
    // 1234567890123456789012345678901234
    SCredits =
      'By Eric Langedijk' + CR +
      'Thanks to...' + CR +
      'DMA for the original game' + CR +
      'ccexplore for game-mechanics' + CR +
      'A. Denisov and others for Graphics32' + CR +
      'Un4seen Development for BASS at:' + CR +
      'un4seen.com' + CR +
      'The Lemmings Community at:' + CR +
      'lemmingsforums.net' + CR +
      'Volker Oth, ccexplore, Mindless, Namida, WilLEM, for sharing sourcecode, resources and technical information about lemmings' + CR +
      'Anna for support' + CR +
      'Milain for testing and ideas'  + CR +
      'Arjan for clone and licence text' + CR +
    // original credits
      'Original credits...'  + CR +
      'Lemmings By DMA Design' + CR +
      'Programming By Russell Kay' + CR +
      'Animation By Gary Timmons' + CR +
      'Graphics By Scott Johnston' + CR +
      'Music By Brian Johnston & Tim Wright' + CR +
      'PC Music By Tony Williams' + CR +
      'Copyright 1991 Psygnosis Ltd.';

  public

  // Game Screen -----------------------------------------------------------------------------------------------------------------------------------------------

    // texts toolbar
    SGame_ToolBar_TextTemplate                     : string;
    SGame_ToolBar_Replaying                        : string;
    SGame_ToolBar_Replayed                         : string;

    // lemming actions toolbar
    SNone                                          : string;
    SAthlete                                       : string;
    SWalker                                        : string;
    SJumper                                        : string;
    SDigger                                        : string;
    SClimber                                       : string;
    SDrowner                                       : string;
    SHoister                                       : string;
    SBuilder                                       : string;
    SBasher                                        : string;
    SMiner                                         : string;
    SFaller                                        : string;
    SFloater                                       : string;
    SSplatter                                      : string;
    SExiter                                        : string;
    SVaporizer                                     : string;
    SBlocker                                       : string;
    SShrugger                                      : string;
    SOhnoer                                        : string;
    SExploder                                      : string;

    // game messages
    SGame_FeedbackMessage_GameSaved                : string;
    SGame_FeedbackMessage_ScreenshotFail           : string;
    SGame_FeedbackMessage_Screenshot               : string;

    // replay messages
    SGame_ReplayMessage_None                       : string;
    SGame_ReplayMessage_Walk                       : string;
    SGame_ReplayMessage_Jump                       : string;
    SGame_ReplayMessage_Dig                        : string;
    SGame_ReplayMessage_Climb                      : string;
    SGame_ReplayMessage_Drown                      : string;
    SGame_ReplayMessage_Hoist                      : string;
    SGame_ReplayMessage_Build                      : string;
    SGame_ReplayMessage_Bash                       : string;
    SGame_ReplayMessage_Mine                       : string;
    SGame_ReplayMessage_Fall                       : string;
    SGame_ReplayMessage_Float                      : string;
    SGame_ReplayMessage_Splat                      : string;
    SGame_ReplayMessage_Exit                       : string;
    SGame_ReplayMessage_Vaporize                   : string;
    SGame_ReplayMessage_Block                      : string;
    SGame_ReplayMessage_Shrug                      : string;
    SGame_ReplayMessage_Ohno                       : string;
    SGame_ReplayMessage_Explode                    : string;

  // LevelCode Screen ------------------------------------------------------------------------------------------------------------------------------------------

    SLevelCodeScreen_EnterCode                     : string;
    SLevelCodeScreen_IncorrectCode                 : string;
    SLevelCodeScreen_CodeForSectionLevel_ss        : string; // section, levelindex

  // Preview Screen --------------------------------------------------------------------------------------------------------------------------------------------

    SPreviewScreen_Level_ss                        : string; // levelindex, title
    SPreviewScreen_NumberOfLemmings_s              : string; // lemmingcount
    SPreviewScreen_ToBeSaved_s                     : string; // lemmingcount or percentage
    SPreviewScreen_ReleaseRate_s                   : string; // releaserate
    SPreviewScreen_Time_s                          : string; // time in minutes
    SPreviewScreen_Rating_s                        : string; // section
    SPreviewScreen_Style_s                         : string; // stylename
    SPreviewScreen_PressMouseButtonToContinue      : string;

  // Postview Screen -------------------------------------------------------------------------------------------------------------------------------------------

    // upper part of screen
    SPostviewScreen_YourTimeIsUp                   : string;
    SPostviewScreen_AllLemmingsAccountedFor        : string;
    SPostviewScreen_YouRescued_s                   : string; // rescued by play
    SPostviewScreen_YouNeeded_s                    : string; // rescued needed

    // result texts original lemmings
    SPostviewScreen_Result0                        : string;
    SPostviewScreen_Result1                        : string;
    SPostviewScreen_Result2                        : string;
    SPostviewScreen_Result3                        : string;
    SPostviewScreen_Result4                        : string;
    SPostviewScreen_Result5                        : string;
    SPostviewScreen_Result6                        : string;
    SPostviewScreen_Result7                        : string;
    SPostviewScreen_Result8                        : string;
    SPostviewScreen_CongratulationOrig             : string;

    // result texts oh no more lemmings
    SPostviewScreen_ResultOhNo0                    : string;
    SPostviewScreen_ResultOhNo1                    : string;
    SPostviewScreen_ResultOhNo2                    : string;
    SPostviewScreen_ResultOhNo3                    : string;
    SPostviewScreen_ResultOhNo4                    : string;
    SPostviewScreen_ResultOhNo5                    : string;
    SPostviewScreen_ResultOhNo6                    : string;
    SPostviewScreen_ResultOhNo7                    : string;
    SPostviewScreen_ResultOhNo8                    : string;
    SPostviewScreen_CongratulationOhNo             : string;

    // lower part of screen
    SPostviewScreen_YourAccessCode_ss              : string;
    SPostviewScreen_PressLeftMouseForNextLevel     : string;
    SPostviewScreen_PressLeftMouseToRetryLevel     : string;
    SPostviewScreen_PressRightMouseForMenu         : string;
    SPostviewScreen_PressMouseToContinue           : string;

    SPostviewScreen_MessageGameIsSaved             : string;
    SPostviewScreen_YouCheater                     : string;
    SPostviewScreen_UnknownGameResultString        : string;


  // help menu screen ------------------------------------------------------------------------------------------------------------------------------------------

    SHelpMenuScreen_CloseLemmix                    : string;
    SHelpMenuScreen_PreviewScreen                  : string;
    SHelpMenuScreen_LevelCodeScreen                : string;
    SHelpMenuScreen_SelectSoundSetting             : string;
    SHelpMenuScreen_NextSection                    : string;
    SHelpMenuScreen_PrevSection                    : string;
    SHelpMenuScreen_OptionsScreen                  : string;
    SHelpMenuScreen_ConfigScreen                   : string;
    SHelpMenuScreen_LevelFinderScreen              : string;
    SHelpMenuScreen_PauseOrUnpauseCredits          : string;
    SHelpMenuScreen_ReplayFinderScreen             : string;
    SHelpMenuScreen_ReplaySystemDialog             : string;
    SHelpMenuScreen_ToggleVoiceOnOff               : string;
    SHelpPreviewScreen_ToggleDebugLayer            : string;

  // help preview screen ---------------------------------------------------------------------------------------------------------------------------------------

    SHelpPreviewScreen_Cancel                      : string;
    SHelpPreviewScreen_StartGame                   : string;
    SHelpPreviewScreen_FindMatchingReplays         : string;
    SHelpPreviewScreen_CheatScrollToNextLevel      : string;
    SHelpPreviewScreen_CheatScrollToPrevLevel      : string;
    SHelpPreviewScreen_CheatScrollMouseWheel       : string;
    SHelpPreviewScreen_BinLevelToggle              : string;

  // help game screen ------------------------------------------------------------------------------------------------------------------------------------------

    SHelpGameScreen_DecreaseReleaseRate            : string;
    SHelpGameScreen_IncreaseReleaseRate            : string;
    SHelpGameScreen_SelectClimberButton            : string;
    SHelpGameScreen_SelectUmbrellaButton           : string;
    SHelpGameScreen_SelectExploderButton           : string;
    SHelpGameScreen_SelectBlockerButton            : string;
    SHelpGameScreen_SelectBuilderButton            : string;
    SHelpGameScreen_SelectBasherButton             : string;
    SHelpGameScreen_SelectMinerButton              : string;
    SHelpGameScreen_SelectDiggerButton             : string;
    SHelpGameScreen_PauseOrUnpause                 : string;
    SHelpGameScreen_Nuke                           : string;
    SHelpGameScreen_SetMinimumReleaseRate          : string;
    SHelpGameScreen_SetMaximumReleaseRate          : string;
    SHelpGameScreen_FinishGame                     : string;
    SHelpGameScreen_SaveState                      : string;
    SHelpGameScreen_GotoSavedState                 : string;
    SHelpGameScreen_SkipOneSecond                  : string;
    SHelpGameScreen_RewindOneSecond                : string;
    SHelpGameScreen_SkipTenSeconds                 : string;
    SHelpGameScreen_RewindTenSeconds               : string;
    SHelpGameScreen_SkipOneMinute                  : string;
    SHelpGameScreen_RewindOneMinute                : string;
    SHelpGameScreen_SkipOneFrame                   : string;
    SHelpGameScreen_RewindOneFrame                 : string;
    SHelpGameScreen_SkipToEndOfGame                : string;
    SHelpGameScreen_ToggleSoundsOnOff              : string;
    SHelpGameScreen_ToggleMusicOnOff               : string;
    SHelpGameScreen_ToggleVoiceOnOff               : string;
    SHelpGameScreen_ToggleGameSpeed                : string;
    SHelpGameScreen_IncreaseMusicVolume            : string;
    SHelpGameScreen_DecreaseMusicVolume            : string;
    SHelpGameScreen_SaveCurrentFrameToPng          : string;
    SHelpGameScreen_ReplayGame                     : string;
    SHelpGameScreen_RestartGameWithoutReplay       : string;
    SHelpGameScreen_SaveGame                       : string;
    SHelpGameScreen_ShowMechanics                  : string;
    SHelpGameScreen_CheatGame                      : string;

  // help postview screen --------------------------------------------------------------------------------------------------------------------------------------

    SHelpPostViewScreen_MenuScreen                 : string;
    SHelpPostViewScreen_Previewcreen               : string;
    SHelpPostViewScreen_ReplayLastGame             : string;
    SHelpPostViewScreen_SaveReplay                 : string;
    SHelpPostViewScreen_CopyLevelCodeToClipBoard   : string;

  // help level finder screen ----------------------------------------------------------------------------------------------------------------------------------

    SHelpFinderScreen_CloseScreen                  : string;
    SHelpFinderScreen_StartSelectedLevel           : string;
    SHelpFinderScreen_EditFilterOfColumn           : string;
    SHelpFinderScreen_ClearFilterOfColumn          : string;
    SHelpFinderScreen_ClearAllFilters              : string;
    SHelpFinderScreen_CopyCellTextToClipboard      : string;
    SHelpFinderScreen_CopyContentsToClipboard      : string;

  // help replay finder screen ---------------------------------------------------------------------------------------------------------------------------------

    SHelpReplayFinderScreen_CloseScreen            : string;
    SHelpReplayFinderScreen_StartReplay            : string;
    SHelpReplayFinderScreen_EditFilterOfColumn     : string;
    SHelpReplayFinderScreen_ClearFilterOfColumn    : string;
    SHelpReplayFinderScreen_ClearAllFilters        : string;

  // screen names ----------------------------------------------------------------------------------------------------------------------------------------------

    SNameMenuScreen                                 : string;
    SNamePreviewScreen                              : string;
    SNameGameScreen                                 : string;
    SNamePostviewScreen                             : string;
    SNameFinderScreen                               : string;
    SNameReplayFinderScreen                         : string;

  // voice -----------------------------------------------------------------------------------------------------------------------------------------------------
    SVoice_BinLevelSaved                            : string;
    SVoice_BinLevelSavingOff                        : string;
    SVoice_BinLevelSavingOn                         : string;
    SVoice_Cheater                                  : string;
    SVoice_CurrentSection                           : string;
    SVoice_CurrentStyle                             : string;
    SVoice_GameSaved                                : string;
    SVoice_ReplayFail                               : string;
    SVoice_SoundFX                                  : string;
    SVoice_StartReplay                              : string;
    SVoice_VoiceDisable                             : string;
    SVoice_VoiceEnable                              : string;

  // generic ---------------------------------------------------------------------------------------------------------------------------------------------------
    SWordMouseWheel                                 : string;
    SWordLevels                                     : string;
    SWordGridNullCell                               : string;
    SWordReplay                                     : string;
    SWordMechanics                                  : string;
    SWordVersion                                    : string;
    SWordYes                                        : string;
    SWordNo                                         : string;

  // errors
    SErrorInvalidReplayFile_s                      : string; // filename
    SErrorCannotFindLevelFromReplayFile_ss         : string; // title, filename
    SErrorFileDoesNotExist_s                       : string; // filename
    SErrorCannotStartWithReplayFile_s              : string; // filename
    SErrorUnexpectedMismatchWhenLoading_s          : string; // filename
    SErrorThisLevelHasCountErrors_s                : string; // errorcount

  // reference arrays (initialized) ------------------------------------------------------------------------------------------------------------------------------
    LemmingActionStrings                           : array[TLemmingAction] of string;
    LemmingReplayStrings                           : array[TLemmingAction] of string;
    ResultStrings                                  : array[TStyleDef] of array[0..8] of string;
    SCongrats                                      : array[TStyleDef] of string;
    VoiceStrings                                   : array[TVoiceOption] of string;
  end;


function YesNo(const b: Boolean): string;

var
  gt: TGlobalTexts;

implementation

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.Rtti, System.TypInfo,
  Prog.Data; // circular

function YesNo(const b: Boolean): string;
begin
  if b then Result := gt.SWordYes else Result := gt.SWordNo;
end;

procedure TGlobalTexts.Init;
begin
  // texts toolbar
  SGame_ToolBar_TextTemplate                     := '..............OUT_.....IN_.....TIME_.-..';
  SGame_ToolBar_Replaying                        := 'replaying';
  SGame_ToolBar_Replayed                         := 'replayed';

  // lemming actions toolbar
  SNone                                          := string.Empty;
  SAthlete                                       := 'Athlete';
  SWalker                                        := 'Walker';
  SJumper                                        := 'Jumper';
  SDigger                                        := 'Digger';
  SClimber                                       := 'Climber';
  SDrowner                                       := 'Drowner';
  SHoister                                       := 'Hoister';
  SBuilder                                       := 'Builder';
  SBasher                                        := 'Basher';
  SMiner                                         := 'Miner';
  SFaller                                        := 'Faller';
  SFloater                                       := 'Floater';
  SSplatter                                      := 'Splatter';
  SExiter                                        := 'Exiter';
  SVaporizer                                     := 'Frier';
  SBlocker                                       := 'Blocker';
  SShrugger                                      := 'Shrugger';
  SOhnoer                                        := 'Ohnoer';
  SExploder                                      := 'Bomber';

  // game messages
  SGame_FeedbackMessage_GameSaved                := 'Game saved';
  SGame_FeedbackMessage_ScreenshotFail           := 'Screenshot fail';
  SGame_FeedbackMessage_Screenshot               := 'Screenshot';

  // replay messages
  SGame_ReplayMessage_None                       := string.Empty;
  SGame_ReplayMessage_Walk                       := 'Walk';
  SGame_ReplayMessage_Jump                       := 'Jump';
  SGame_ReplayMessage_Dig                        := 'Dig';
  SGame_ReplayMessage_Climb                      := 'Climb';
  SGame_ReplayMessage_Drown                      := 'Drown';
  SGame_ReplayMessage_Hoist                      := 'Hoist';
  SGame_ReplayMessage_Build                      := 'Build';
  SGame_ReplayMessage_Bash                       := 'Bash';
  SGame_ReplayMessage_Mine                       := 'Mine';
  SGame_ReplayMessage_Fall                       := 'Fall';
  SGame_ReplayMessage_Float                      := 'Float';
  SGame_ReplayMessage_Splat                      := 'Splat';
  SGame_ReplayMessage_Exit                       := 'Exit';
  SGame_ReplayMessage_Vaporize                   := 'Vaporize';
  SGame_ReplayMessage_Block                      := 'Block';
  SGame_ReplayMessage_Shrug                      := 'Shrug';
  SGame_ReplayMessage_Ohno                       := 'Ohno';
  SGame_ReplayMessage_Explode                    := 'Explode';

// LevelCode Screen --------------------------------------------------------------------------------------------------------------------------------------------

  SLevelCodeScreen_EnterCode                     := 'Enter Code';
  SLevelCodeScreen_IncorrectCode                 := 'INCORRECT CODE';
  SLevelCodeScreen_CodeForSectionLevel_ss        := 'Code for %s' + CR + 'Level %s';

// Preview Screen ----------------------------------------------------------------------------------------------------------------------------------------------

  SPreviewScreen_Level_ss                        := 'Level %s %s';
  SPreviewScreen_NumberOfLemmings_s              := 'Number of Lemmings %s';
  SPreviewScreen_ToBeSaved_s                     := '%s To Be Saved';
  SPreviewScreen_ReleaseRate_s                   := 'Release Rate %s';
  SPreviewScreen_Time_s                          := 'Time %s Minutes';
  SPreviewScreen_Rating_s                        := 'Rating %s';
  SPreviewScreen_Style_s                         := 'Style %s';
  SPreviewScreen_PressMouseButtonToContinue      := 'Press mouse button to continue';

// Postview Screen ---------------------------------------------------------------------------------------------------------------------------------------------

  // upper part of screen
  SPostviewScreen_YourTimeIsUp                   := 'Your time is up!';
  SPostviewScreen_AllLemmingsAccountedFor        := 'All lemmings accounted for.';
  SPostviewScreen_YouRescued_s                   := 'You rescued %s';
  SPostviewScreen_YouNeeded_s                    := 'You needed  %s';

  // result texts original lemmings
  SPostviewScreen_Result0                        := 'ROCK BOTTOM! I hope for your sake' + CR + 'that you nuked that level.';
  SPostviewScreen_Result1                        := 'Better rethink your strategy before' + CR + 'you try this level again!';
  SPostviewScreen_Result2                        := 'A little more practice on this level' + CR + 'is definitely recommended.';
  SPostviewScreen_Result3                        := 'You got pretty close that time.' + CR + 'Now try again for that few % extra.';
  SPostviewScreen_Result4                        := 'OH NO, So near and yet so far (teehee)' + CR + 'Maybe this time.....';
  SPostviewScreen_Result5                        := 'RIGHT ON. You can''t get much closer' + CR + 'than that. Let''s try the next...';
  SPostviewScreen_Result6                        := 'That level seemed no problem to you on' + CR + 'that attempt. Onto the next....';
  SPostviewScreen_Result7                        := 'You totally stormed that level!' + CR + 'Let''s see if you can storm the next...';
  SPostviewScreen_Result8                        := 'Superb! You rescued every lemmings on' + CR + 'that level. Can you do it again....';
  SPostviewScreen_CongratulationOrig             := CR + CR + 'Congratulations!' + CR + CR + CR + CR + CR +
                                                       'Everybody here at DMA Design salutes you' + CR +
                                                       'as a MASTER Lemmings player. Not many' + CR +
                                                       'people will complete the Mayhem levels,' + CR +
                                                       'you are definitely one of the elite' + CR +
                                                       CR + CR + CR + CR + CR + 'Now hold your breath for the data disk';

  // result texts oh no more lemmings (#6 seems a typo but it really is enough + space + point in the original exe)
  SPostviewScreen_ResultOhNo0                    := 'Oh dear, not even one poor Lemming' + CR + 'saved. Try a little harder next time.';
  SPostviewScreen_ResultOhNo1                    := 'Yes, well, err, erm, maybe that is' + CR + 'NOT the way to do this level.';
  SPostviewScreen_ResultOhNo2                    := 'We are not too impressed with your' + CR + 'attempt at that level!';
  SPostviewScreen_ResultOhNo3                    := 'Getting close. You are either pretty' + CR + 'good, or simply lucky.';
  SPostviewScreen_ResultOhNo4                    := 'Shame, You were short by a tiny amount.' + CR + 'Go for it this time.';
  SPostviewScreen_ResultOhNo5                    := 'Just made it by the skin of your' + CR + 'teeth. Time to progress..';
  SPostviewScreen_ResultOhNo6                    := 'More than enough .You have the makings' + CR + 'of a master Lemmings player.';
  SPostviewScreen_ResultOhNo7                    := 'What a fine display of Lemmings control.' + CR + 'Take a bow then carry on with the game.';
  SPostviewScreen_ResultOhNo8                    := 'WOW! You saved every Lemming.' + CR + 'TOTALLY EXCELLENT!';
  SPostviewScreen_CongratulationOhNo             := CR + CR + 'Congratulations!' + CR + CR + CR + CR + CR + CR +
                                                       'You are truly an Excellent' + CR + 'Lemmings player' + CR + CR +
                                                       'The Lemmings Saga continues at a' + CR + 'later date, watch this space';

  // lower part of screen
  SPostviewScreen_YourAccessCode_ss              := 'Your Access Code for Level %s' + CR + 'is %s';
  SPostviewScreen_PressLeftMouseForNextLevel     := 'Press left mouse button for next level';
  SPostviewScreen_PressLeftMouseToRetryLevel     := 'Press left mouse button to retry level';
  SPostviewScreen_PressRightMouseForMenu         := 'Press right mouse button for menu';
  SPostviewScreen_PressMouseToContinue           := 'Press mouse button to continue';

  SPostviewScreen_MessageGameIsSaved             := 'Game saved (including game result in .txt file)';
  SPostviewScreen_YouCheater                     := 'Cheating apparently allowed';
  SPostviewScreen_UnknownGameResultString        := 'WOW! We do not know' + CR + 'what happened in this level';


// help menu screen --------------------------------------------------------------------------------------------------------------------------------------------

  SHelpMenuScreen_CloseLemmix                    := 'Close Lemmix';
  SHelpMenuScreen_PreviewScreen                  := 'Preview screen';
  SHelpMenuScreen_LevelCodeScreen                := 'Level code screen';
  SHelpMenuScreen_SelectSoundSetting             := 'Select sound setting';
  SHelpMenuScreen_NextSection                    := 'Next section';
  SHelpMenuScreen_PrevSection                    := 'Prev section';
  SHelpMenuScreen_OptionsScreen                  := 'Options screen';
  SHelpMenuScreen_ConfigScreen                   := 'Configuration screen';
  SHelpMenuScreen_LevelFinderScreen              := 'Level finder screen';
  SHelpMenuScreen_PauseOrUnpauseCredits          := 'Pause or unpause credits animation';
  SHelpMenuScreen_ReplayFinderScreen             := 'Replay finder screen';
  SHelpMenuScreen_ReplaySystemDialog             := 'Replay system dialog';
  SHelpMenuScreen_ToggleVoiceOnOff               := 'Toggle voice on/off';


// help preview screen -----------------------------------------------------------------------------------------------------------------------------------------

  SHelpPreviewScreen_Cancel                      := 'Cancel';
  SHelpPreviewScreen_StartGame                   := 'Start game';
  SHelpPreviewScreen_FindMatchingReplays         := 'Find matching replays';
  SHelpPreviewScreen_CheatScrollToNextLevel      := 'Cheat scroll to next level';
  SHelpPreviewScreen_CheatScrollToPrevLevel      := 'Cheat scroll to previous level';
  SHelpPreviewScreen_CheatScrollMouseWheel       := 'Cheat scroll through levels';
  SHelpPreviewScreen_BinLevelToggle              := 'Toggle save level to binary file on/off when starting level';
  SHelpPreviewScreen_ToggleDebugLayer            := 'Toggle debug layer on/of';

// help game screen --------------------------------------------------------------------------------------------------------------------------------------------

  SHelpGameScreen_DecreaseReleaseRate            := 'Decrease release rate';
  SHelpGameScreen_IncreaseReleaseRate            := 'Increase release rate';
  SHelpGameScreen_SelectClimberButton            := 'Select climber button';
  SHelpGameScreen_SelectUmbrellaButton           := 'Select umbrella button';
  SHelpGameScreen_SelectExploderButton           := 'Select exploder button';
  SHelpGameScreen_SelectBlockerButton            := 'Select blocker button';
  SHelpGameScreen_SelectBuilderButton            := 'Select builder button';
  SHelpGameScreen_SelectBasherButton             := 'Select basher button';
  SHelpGameScreen_SelectMinerButton              := 'Select miner button';
  SHelpGameScreen_SelectDiggerButton             := 'Select digger button';
  SHelpGameScreen_PauseOrUnpause                 := 'Pause or unpause';
  SHelpGameScreen_Nuke                           := 'Nuke (press twice)';
  SHelpGameScreen_SetMinimumReleaseRate          := 'Set minimum release rate (when paused)';
  SHelpGameScreen_SetMaximumReleaseRate          := 'Set maximum release rate (when paused)';
  SHelpGameScreen_FinishGame                     := 'Finish game';
  SHelpGameScreen_SaveState                      := 'Save state current frame';
  SHelpGameScreen_GotoSavedState                 := 'Rewind to saved state';
  SHelpGameScreen_SkipOneSecond                  := 'Skip 1 second';
  SHelpGameScreen_RewindOneSecond                := 'Rewind 1 second';
  SHelpGameScreen_SkipTenSeconds                 := 'Skip 10 seconds';
  SHelpGameScreen_RewindTenSeconds               := 'Rewind 10 seconds';
  SHelpGameScreen_SkipOneMinute                  := 'Skip 1 minute';
  SHelpGameScreen_RewindOneMinute                := 'Rewind 1 minute';
  SHelpGameScreen_SkipOneFrame                   := 'Skip 1 frame (when paused)';
  SHelpGameScreen_RewindOneFrame                 := 'Rewind 1 frame (when paused)';
  SHelpGameScreen_SkipToEndOfGame                := 'Skip to the end of the game';
  SHelpGameScreen_ToggleSoundsOnOff              := 'Toggle sounds on/off';
  SHelpGameScreen_ToggleMusicOnOff               := 'Toggle music on/off';
  SHelpGameScreen_ToggleVoiceOnOff               := 'Toggle voice on/off';
  SHelpGameScreen_ToggleGameSpeed                := 'Toggle fast game-speed on/off';
  SHelpGameScreen_IncreaseMusicVolume            := 'Increase music volume';
  SHelpGameScreen_DecreaseMusicVolume            := 'Decrease music volume';
  SHelpGameScreen_SaveCurrentFrameToPng          := 'Save current frame to PNG and clipboard';
  SHelpGameScreen_ReplayGame                     := 'Replay game from the beginning';
  SHelpGameScreen_RestartGameWithoutReplay       := 'Restart game without replay';
  SHelpGameScreen_SaveGame                       := 'Save game to replayfile';
  SHelpGameScreen_ShowMechanics                  := 'Show mechanics information';
  SHelpGameScreen_CheatGame                      := 'Cheat game';

// help postview screen -----------------------------------------------------------------------------------------------------------------------------------------

  SHelpPostViewScreen_MenuScreen                 := 'Menu screen';
  SHelpPostViewScreen_Previewcreen               := 'Preview screen';
  SHelpPostViewScreen_ReplayLastGame             := 'Replay last game';
  SHelpPostViewScreen_SaveReplay                 := 'Preview screen';
  SHelpPostViewScreen_CopyLevelCodeToClipBoard   := 'Copy levelcode to clipboard';

// help level finder screen ------------------------------------------------------------------------------------------------------------------------------------

  SHelpFinderScreen_CloseScreen                  := 'Close screen';
  SHelpFinderScreen_StartSelectedLevel           := 'Start selected level (or double click)';
  SHelpFinderScreen_EditFilterOfColumn           := 'Edit filter of current column';
  SHelpFinderScreen_ClearFilterOfColumn          := 'Clear filter of current column';
  SHelpFinderScreen_ClearAllFilters              := 'Clear all filters';
  SHelpFinderScreen_CopyCellTextToClipboard      := 'Copy cell text to clipboard';
  SHelpFinderScreen_CopyContentsToClipboard      := 'Copy contents to clipboard';

// help replay finder screen -----------------------------------------------------------------------------------------------------------------------------------

  SHelpReplayFinderScreen_CloseScreen            := 'Close screen';
  SHelpReplayFinderScreen_StartReplay            := 'Start replay of selected replayfile (or double click)';
  SHelpReplayFinderScreen_EditFilterOfColumn     := 'Edit filter of current column';
  SHelpReplayFinderScreen_ClearFilterOfColumn    := 'Clear filter of current column';
  SHelpReplayFinderScreen_ClearAllFilters        := 'Clear all filters';

// screen names ------------------------------------------------------------------------------------------------------------------------------------------------

  SNameMenuScreen                                 := 'Menu Screen';
  SNamePreviewScreen                              := 'Preview Screen';
  SNameGameScreen                                 := 'Game Screen';
  SNamePostviewScreen                             := 'Result Screen';
  SNameFinderScreen                               := 'Level Finder Screen';
  SNameReplayFinderScreen                         := 'Replay Finder Screen';

// voice -------------------------------------------------------------------------------------------------------------------------------------------------------
  SVoice_BinLevelSaved                            := 'Binary level saved';
  SVoice_BinLevelSavingOff                        := 'Binary save off';
  SVoice_BinLevelSavingOn                         := 'Binary save on';
  SVoice_Cheater                                  := 'Cheater';
  SVoice_CurrentSection                           := 'Current section';
  SVoice_CurrentStyle                             := 'Current style';
  SVoice_GameSaved                                := 'Game saved';
  SVoice_ReplayFail                               := 'Fail';
  SVoice_SoundFX                                  := 'Current sound';
  SVoice_StartReplay                              := 'Start replay';
  SVoice_VoiceDisable                             := 'Mute';
  SVoice_VoiceEnable                              := 'I will speak';

// generic -----------------------------------------------------------------------------------------------------------------------------------------------------
  SWordMouseWheel                                 := 'Mousewheel';
  SWordLevels                                     := 'Levels';
  SWordGridNullCell                               := '<null>';
  SWordReplay                                     := 'Replay';
  SWordMechanics                                  := 'Mechanics';
  SWordVersion                                    := 'Version';
  SWordYes                                        := 'yes';
  SWordNo                                         := 'no';

// error
  SErrorInvalidReplayFile_s                       := 'Invalid replayfile %s.';
  SErrorCannotFindLevelFromReplayFile_ss          := 'Cannot find the level %s from replayfile %s.';
  SErrorFileDoesNotExist_s                        := 'File %s does not exist.';
  SErrorCannotStartWithReplayFile_s               := 'Cannot start program with replayfile %s.';
  SErrorUnexpectedMismatchWhenLoading_s           := 'Unexpected level information mismatch during loading of %s. Please report the error.';
  SErrorThisLevelHasCountErrors_s                 := 'This level contains %s errors.';

  InitArrays;
end;

procedure TGlobalTexts.InitArrays;
begin
  LemmingActionStrings[TLemmingAction.None]       := SNone;
  LemmingActionStrings[TLemmingAction.Walking]    := SWalker;
  LemmingActionStrings[TLemmingAction.Jumping]    := SJumper;
  LemmingActionStrings[TLemmingAction.Digging]    := SDigger;
  LemmingActionStrings[TLemmingAction.Climbing]   := SClimber;
  LemmingActionStrings[TLemmingAction.Drowning]   := SDrowner;
  LemmingActionStrings[TLemmingAction.Hoisting]   := SHoister;
  LemmingActionStrings[TLemmingAction.Building]   := SBuilder;
  LemmingActionStrings[TLemmingAction.Bashing]    := SBasher;
  LemmingActionStrings[TLemmingAction.Mining]     := SMiner;
  LemmingActionStrings[TLemmingAction.Falling]    := SFaller;
  LemmingActionStrings[TLemmingAction.Floating]   := SFloater;
  LemmingActionStrings[TLemmingAction.Splatting]  := SSplatter;
  LemmingActionStrings[TLemmingAction.Exiting]    := SExiter;
  LemmingActionStrings[TLemmingAction.Vaporizing] := SVaporizer;
  LemmingActionStrings[TLemmingAction.Blocking]   := SBlocker;
  LemmingActionStrings[TLemmingAction.Shrugging]  := SShrugger;
  LemmingActionStrings[TLemmingAction.Ohnoing]    := SOhnoer;
  LemmingActionStrings[TLemmingAction.Exploding]  := SExploder;

  LemmingReplayStrings[TLemmingAction.None]       := SGame_ReplayMessage_None;
  LemmingReplayStrings[TLemmingAction.Walking]    := SGame_ReplayMessage_Walk;
  LemmingReplayStrings[TLemmingAction.Jumping]    := SGame_ReplayMessage_Jump;
  LemmingReplayStrings[TLemmingAction.Digging]    := SGame_ReplayMessage_Dig;
  LemmingReplayStrings[TLemmingAction.Climbing]   := SGame_ReplayMessage_Climb;
  LemmingReplayStrings[TLemmingAction.Drowning]   := SGame_ReplayMessage_Drown;
  LemmingReplayStrings[TLemmingAction.Hoisting]   := SGame_ReplayMessage_Hoist;
  LemmingReplayStrings[TLemmingAction.Building]   := SGame_ReplayMessage_Build;
  LemmingReplayStrings[TLemmingAction.Bashing]    := SGame_ReplayMessage_Bash;
  LemmingReplayStrings[TLemmingAction.Mining]     := SGame_ReplayMessage_Mine;
  LemmingReplayStrings[TLemmingAction.Falling]    := SGame_ReplayMessage_Fall;
  LemmingReplayStrings[TLemmingAction.Floating]   := SGame_ReplayMessage_Float;
  LemmingReplayStrings[TLemmingAction.Splatting]  := SGame_ReplayMessage_Splat;
  LemmingReplayStrings[TLemmingAction.Exiting]    := SGame_ReplayMessage_Exit;
  LemmingReplayStrings[TLemmingAction.Vaporizing] := SGame_ReplayMessage_Vaporize;
  LemmingReplayStrings[TLemmingAction.Blocking]   := SGame_ReplayMessage_Block;
  LemmingReplayStrings[TLemmingAction.Shrugging]  := SGame_ReplayMessage_Shrug;
  LemmingReplayStrings[TLemmingAction.Ohnoing]    := SGame_ReplayMessage_Ohno;
  LemmingReplayStrings[TLemmingAction.Exploding]  := SGame_ReplayMessage_Explode;

  for var styleDef: TStyleDef := TStyleDef.Orig to TStyleDef.User do begin
    if styleDef in [TStyleDef.Orig, TStyleDef.User] then begin
      ResultStrings[styleDef][0] := SPostviewScreen_Result0;
      ResultStrings[styleDef][1] := SPostviewScreen_Result1;
      ResultStrings[styleDef][2] := SPostviewScreen_Result2;
      ResultStrings[styleDef][3] := SPostviewScreen_Result3;
      ResultStrings[styleDef][4] := SPostviewScreen_Result4;
      ResultStrings[styleDef][5] := SPostviewScreen_Result5;
      ResultStrings[styleDef][6] := SPostviewScreen_Result6;
      ResultStrings[styleDef][7] := SPostviewScreen_Result7;
      ResultStrings[styleDef][8] := SPostviewScreen_Result8;
      SCongrats[styleDef] := SPostviewScreen_CongratulationOrig;
    end
    else begin
      ResultStrings[styleDef][0] := SPostviewScreen_ResultOhNo0;
      ResultStrings[styleDef][1] := SPostviewScreen_ResultOhNo1;
      ResultStrings[styleDef][2] := SPostviewScreen_ResultOhNo2;
      ResultStrings[styleDef][3] := SPostviewScreen_ResultOhNo3;
      ResultStrings[styleDef][4] := SPostviewScreen_ResultOhNo4;
      ResultStrings[styleDef][5] := SPostviewScreen_ResultOhNo5;
      ResultStrings[styleDef][6] := SPostviewScreen_ResultOhNo6;
      ResultStrings[styleDef][7] := SPostviewScreen_ResultOhNo7;
      ResultStrings[styleDef][8] := SPostviewScreen_ResultOhNo8;
      SCongrats[styleDef] := SPostviewScreen_CongratulationOhNo;
    end;
  end;

  VoiceStrings[TVoiceOption.BinLevelSaved]       := SVoice_BinLevelSaved;
  VoiceStrings[TVoiceOption.BinLevelSavingOff]   := SVoice_BinLevelSavingOff;
  VoiceStrings[TVoiceOption.BinLevelSavingOn]    := SVoice_BinLevelSavingOn;
  VoiceStrings[TVoiceOption.Cheater]             := SVoice_Cheater;
  VoiceStrings[TVoiceOption.CurrentSection]      := SVoice_CurrentSection;
  VoiceStrings[TVoiceOption.CurrentStyle]        := SVoice_CurrentStyle;
  VoiceStrings[TVoiceOption.GameSaved]           := SVoice_GameSaved;
  VoiceStrings[TVoiceOption.ReplayFail]          := SVoice_ReplayFail;
  VoiceStrings[TVoiceOption.SoundFX]             := SVoice_SoundFX;
  VoiceStrings[TVoiceOption.StartReplay]         := SVoice_StartReplay;
  VoiceStrings[TVoiceOption.VoiceDisable]        := SVoice_VoiceDisable;
  VoiceStrings[TVoiceOption.VoiceEnable]         := SVoice_VoiceEnable;
end;

procedure HookResourceString(Res: PResStringRec; const NewStr: string); inline;
var
  oldprotect: Cardinal;
begin
  VirtualProtect(Res, SizeOf(Res^), PAGE_EXECUTE_READWRITE, @oldProtect);
  Res^.Identifier := NativeInt(PChar(NewStr));
  VirtualProtect(Res, SizeOf(Res^), oldProtect, @oldProtect);
end;

procedure TGlobalTexts.Load(const filename: string);
var
  ctx: TRttiContext;
  typ: TRttiType;
  list: TStringList;
  dict: TDictionary<string, string>;

    procedure Prepare;
    var
      line, key, value: string;
      p: Integer;
    begin
      for var s in list do begin
        line := s;
        p := Pos('=', line);
        if p < 1 then
          Continue;
        key := Copy(line, 1, p - 1).Trim;
        value := Copy(line, p + 1, Length(line)).Trim;
        value := value.Replace('[CRLF]', CRLF);
        dict.TryAdd(key, value);
      end;
    end;

    procedure Change(fld: TRttiField);
    var
      newValue: string;
    begin
      if not dict.TryGetValue(fld.Name, newValue) then begin
        {$ifdef debug} log('entry not found', fld.Name); {$endif}
        Exit;
      end;
      fld.SetValue(Self, newValue);
    end;

begin
  TempCursor.Activate;
  dict := nil;
  list := nil;
  try
    list := TData.CreateLanguageStringList(filename, False);
    dict := TDictionary<string, string>.Create;
    Prepare;
    ctx := TRttiContext.Create;
    try
      typ := ctx.GetType(TypeInfo(TGlobalTexts));
      for var fld: TRttiField in typ.GetDeclaredFields do
        if fld.FieldType is TRttiStringType then
          Change(fld);
    finally
      ctx.Free;
    end;

  finally
    list.Free;
    dict.Free;
  end;

  InitArrays;
end;

procedure TGlobalTexts.Save(const filename: string);
var
  ctx: TRttiContext;
  typ: TRttiType;
  list: TStringList;

    procedure Write(fld: TRttiField);
    begin
      var value: string := fld.GetValue(Self).ToString;
      value := value.Replace(CRLF, '[CRLF]');
      value := value.Replace(CR, '[CRLF]');
      value := value.Replace(LF, string.Empty);
      list.AddPair(fld.Name, value);
    end;

begin
  TempCursor.Activate;
  list := TStringList.Create;
  try
    ctx := TRttiContext.Create;
    try
      typ := ctx.GetType(TypeInfo(TGlobalTexts));
      for var fld: TRttiField in typ.GetDeclaredFields do
        if fld.FieldType is TRttiStringType then
          Write(fld);
    finally
      ctx.Free;
    end;

    list.SaveToFile(filename);

  finally
    list.Free;
  end;

end;

initialization
  gt := TGlobalTexts.Create;
  gt.Init;

finalization
  gt.Free;
end.

