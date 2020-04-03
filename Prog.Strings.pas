unit Prog.Strings;

{$include lem_directives.inc}

interface

uses
  Dos.Consts,
  Prog.Types;//Prog.Base;

const
  SCheatCode = 'elangedijk';
  SProgramName = 'Lemmix';
  SEmptyString = '';

  {-------------------------------------------------------------------------------
    DOS MenuScreen
  -------------------------------------------------------------------------------}
  SByEricLangedijk = 'By Eric Langedijk';
  SProgramTexts: array[TStyleDef] of string = (
    'Original Lemmings',
    'Oh No More Lemmings!',
    'Holiday Lemmings 93' ,
    'Holiday Lemmings 94' ,
    'XMas Lemmings 91',
    'XMas Lemmings 92',
    '%s'
  );

  // Note: Max size for string that fits in the scrolling reel = 34
  // 1234567890123456789012345678901234

  SOriginalCredits =
    'Original credits...'  + #13 +
    'Lemmings By DMA Design' + #13 +
    'Programming By Russell Kay' + #13 +
    'Animation By Gary Timmons' + #13 +
    'Graphics By Scott Johnston' + #13 +
    'Music By Brian Johnston & Tim Wright' + #13 +
    'PC Music By Tony Williams' + #13 +
    'Copyright 1991 Psygnosis Ltd.';

  SCredits =
//    SProgramName + ' ' + SLemmixVersion + {$if defined(cpux64)} ' 64 bits' {$else} ' 32 bits' {$endif} + #13 +
    'By Eric Langedijk' + #13 +
    'Thanks to...' + #13 +
    'DMA for the original game' + #13 +
    'ccexplore for game-mechanics' + #13 +
    'A. Denisov and others for Graphics32' + #13 +
    'Un4seen Development for BASS at:' + #13 +
    'un4seen.com' + #13 +
    'The Lemmings Community at:' + #13 +
    'lemmingsforums.net' + #13 +
    'Volker Oth, ccexplore, Mindless, Namida, WilLEM, for sharing sourcecode, resources and technical information about lemmings' + #13 +
    SOriginalCredits;

  {-------------------------------------------------------------------------------
    DOS LevelCodeScreen
  -------------------------------------------------------------------------------}
  SEnterCode =
   'Enter Code';

  SIncorrectCode =
    'INCORRECT CODE';

  SCodeForLevel_ssd =  // format
    'Code for ' + #13 +
    '%s' + #13 +
    '%s' + #13 +
    'Level %d';

  SCodeForLevel_sd = // format
    'Code for %s' + #13 +
    'Level %d';

  {-------------------------------------------------------------------------------
    DOS PreviewScreen
  -------------------------------------------------------------------------------}
  SPreviewString_dsdsss = // format
    'Level %d ' + '%s'                     + #13#13#13 +
    '          Number of Lemmings %d'      + #13#13 +
    '          %s To Be Saved'             + #13#13 +
    '          Release Rate %d'            + #13#13 +
    '          Time %d Minutes'            + #13#13 +
    '          Rating  %s'                 + #13#13 +
    '          Style  %s'                  + #13#13 + // todo: optional
    '     Press mouse button to continue';
//

  {-------------------------------------------------------------------------------
    DOS Game Screen SkillPanel: This one *must* be 40 characters
  -------------------------------------------------------------------------------}
  SSkillPanelTemplate =
    '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';

  {-------------------------------------------------------------------------------
    DOS Game screen SkillPanel: lemming mouse over text hints
  -------------------------------------------------------------------------------}
  SAthlete = 'Athlete';
  SWalker = 'Walker';
  SJumper = 'Jumper';
  SDigger = 'Digger';
  SClimber = 'Climber';
  SDrowner = 'Drowner';
  SHoister = 'Hoister';
  SBuilder = 'Builder';
  SBasher = 'Basher';
  SMiner = 'Miner';
  SFaller = 'Faller';
  SFloater = 'Floater';
  SSplatter = 'Splatter';
  SExiter = 'Exiter';
  SVaporizer = 'Frier';
  SBlocker = 'Blocker';
  SShrugger = 'Shrugger';
  SOhnoer = 'Ohnoer';
  SExploder = 'Bomber';

  {-------------------------------------------------------------------------------
    DOS Postview Screen
  -------------------------------------------------------------------------------}
  // top of screen
  SYourTimeIsUp =
    'Your time is up!';
  SAllLemmingsAccountedFor =
    'All lemmings accounted for.';
  SYouRescuedYouNeeded_ss =
     'You rescued %s' + #13 +
     'You needed  %s';

  // original lemmings
  SResult0 =
    'ROCK BOTTOM! I hope for your sake'      + #13 +
    'that you nuked that level.';
  SResult1 =
    'Better rethink your strategy before'    + #13 +
    'you try this level again!';
  SResult2 =
    'A little more practice on this level'   + #13 +
    'is definitely recommended.';
  SResult3 =
    'You got pretty close that time.'        + #13 +
    'Now try again for that few % extra.';
  SResult4 =
    'OH NO, So near and yet so far (teehee)' + #13 +
    'Maybe this time.....';
  SResult5 =
    'RIGHT ON. You can''t get much closer'   + #13 +
    'than that. Let''s try the next...';
  SResult6 =
    'That level seemed no problem to you on' + #13 +
    'that attempt. Onto the next....';
  SResult7 =
    'You totally stormed that level!'        + #13 +
    'Let''s see if you can storm the next...';
  SResult8 =
    'Superb! You rescued every lemmings on' + #13 +
    'that level. Can you do it again....';
  SCongratulationOrig = // when finished all levels
    #13 + #13 +
    'Congratulations!' +
    #13 + #13 + #13 + #13 + #13 +
    'Everybody here at DMA Design salutes you' + #13 +
    'as a MASTER Lemmings player. Not many' + #13 +
    'people will complete the Mayhem levels,' + #13 +
    'you are definitely one of the elite' + #13 +
    #13 + #13 + #13 + #13 + #13 +
    'Now hold your breath for the data disk';

  // oh no more lemmings
  SResultOhNo0 =
    'Oh dear, not even one poor Lemming'   + #13 +
    'saved. Try a little harder next time.';
  SResultOhNo1 =
    'Yes, well, err, erm, maybe that is' + #13 +
    'NOT the way to do this level.';
  SResultOhNo2 =
    'We are not too impressed with your' + #13 +
    'attempt at that level!';
  SResultOhNo3 =
    'Getting close. You are either pretty' + #13 +
    'good, or simply lucky.';
  SResultOhNo4 =
    'Shame, You were short by a tiny amount.' + #13 +
    'Go for it this time.';
  SResultOhNo5 =
    'Just made it by the skin of your' + #13 +
    'teeth. Time to progress..';
  SResultOhNo6 = // This seems a typo but it really is enough + space + point in the original exe
    'More than enough .You have the makings' + #13 +
    'of a master Lemmings player.';
  SResultOhNo7 =
    'What a fine display of Lemmings control.' + #13 +
    'Take a bow then carry on with the game.';
  SResultOhNo8 =
    'WOW! You saved every Lemming.' + #13 +
    'TOTALLY EXCELLENT!';
  SCongratulationOhNo = // when finished all levels
    #13 + #13 +
    'Congratulations!' + #13 +
    #13 + #13 + #13 + #13 + #13 +
    'You are truly an Excellent' + #13 +
    'Lemmings player' + #13 +
    #13 +
    'The Lemmings Saga continues at a' + #13 +
    'later date, watch this space';

  // lower part of screen
  SYourAccessCode_ds =
    'Your Access Code for Level %d' + #13 +
    'is %s';
  SPressLeftMouseForNextLevel =
    'Press left mouse button for next level';
  SPressLeftMouseToRetryLevel =
    'Press left mouse button to retry level';
  SPressRightMouseForMenu =
    'Press right mouse button for menu';
  SPressMouseToContinue =
    'Press mouse button to continue';

  LemmingActionStrings: array[TLemmingAction] of string = (
    SEmptyString, SWalker, SJumper, SDigger, SClimber, SDrowner, SHoister, SBuilder, SBasher, SMiner,
    SFaller, SFloater, SSplatter, SExiter, SVaporizer, SBlocker, SShrugger, SOhnoer, SExploder);

  LemmingReplayStrings: array[TLemmingAction] of string = (
    '', 'Walk', 'Jump', 'Dig', 'Climb', 'Drown', 'Hoist', 'Build', 'Bash', 'Mine',
    'Fall', 'Float', 'Splat', 'Exit', 'Vaporize', 'Block', 'Shrug', 'Ohno', 'Explode');

  ResultStrings: array[TStyleDef] of array[0..8] of string = (
    (SResult0, SResult1, SResult2, SResult3, SResult4, SResult5, SResult6, SResult7, SResult8),
    (SResultOhNo0, SResultOhNo1, SResultOhNo2, SResultOhNo3, SResultOhNo4, SResultOhNo5, SResultOhNo6, SResultOhNo7, SResultOhNo8),
    (SResultOhNo0, SResultOhNo1, SResultOhNo2, SResultOhNo3, SResultOhNo4, SResultOhNo5, SResultOhNo6, SResultOhNo7, SResultOhNo8),
    (SResultOhNo0, SResultOhNo1, SResultOhNo2, SResultOhNo3, SResultOhNo4, SResultOhNo5, SResultOhNo6, SResultOhNo7, SResultOhNo8),
    (SResultOhNo0, SResultOhNo1, SResultOhNo2, SResultOhNo3, SResultOhNo4, SResultOhNo5, SResultOhNo6, SResultOhNo7, SResultOhNo8),
    (SResultOhNo0, SResultOhNo1, SResultOhNo2, SResultOhNo3, SResultOhNo4, SResultOhNo5, SResultOhNo6, SResultOhNo7, SResultOhNo8),
    (SResult0, SResult1, SResult2, SResult3, SResult4, SResult5, SResult6, SResult7, SResult8)
  );

  SCongrats: array[TStyleDef] of string = (
    SCongratulationOrig,
    SCongratulationOhNo,
    SCongratulationOhNo,
    SCongratulationOhNo,
    SCongratulationOhNo,
    SCongratulationOhNo,
    SCongratulationOrig
  );

  {-------------------------------------------------------------------------------
    Lemmix additional postview screen texts
  -------------------------------------------------------------------------------}
  SYouCheater = 'Cheating apparently allowed';
  SUnknownGameResultString = 'WOW! We do not know' + #13 + 'what happened in this level';

implementation

end.


