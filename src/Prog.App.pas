unit Prog.App;

{$include lem_directives.inc}

{-------------------------------------------------------------------------------
  The TApp class is in fact just a global var which is used by all
  screens. not much more than a placeholder for the program / game logic.
  But at creation it load the configuration file and the style is set.
-------------------------------------------------------------------------------}

interface

uses
vcl.Dialogs,
  System.SysUtils, System.Generics.Collections,
  GR32,
  Base.Utils, Base.Types,
  Styles.Base,
  Level.Base,
  Prog.Base, Prog.Config, Prog.Cache, Prog.Voice,
  Game, Game.Sound, Game.Rendering;

type
  TApp = class sealed
  private
    fFormStack              : TList<IForm>;
    fCurrentForm            : IForm;                      // set by mainform
  public
    Config                 : TConfig;                     // loaded in contructor
    ReplayFileName         : string;                      // determined by mainform
    Style                  : TStyle;                      // constructed and initialized by mainform
    StyleCache             : TStyleCache;                 // constructed and initialized by mainform
    ReplayCache            : TReplayCache;                // constructore here. loaded on demand
    SoundMgr               : TSoundMgr;                   // constructed here
    VoiceMgr               : TVoiceMgr;                   // constructed here
    MainForm               : IMainForm;                   // set by mainform
    GlobalGame             : TLemmingGame;                // constructed and initialized by mainform, prepared in preview screen
    CurrentLevelInfo       : TLevelLoadingInformation;    // ref only, initizialized by mainform, changed all over the place
    ReplayCurrent          : Boolean;
    Level                  : TLevel;                      // constructed by mainform, loaded in preview screen
    GraphicSet             : TGraphicSet;                 // constructed by mainform, loaded in preview screen
    Renderer               : TRenderer;                   // constructed by mainform, prepared in preview screen
    TargetBitmap           : TBitmap32;                   // this is a reference only and filled in the player screen
    GameResult             : TGameResultsRec;             // this is retrieved from the game and copied by postview screen
    NewStyleName           : string;                      // can only be changed by options screen
    NewSectionIndex        : Integer;                     // quick and dirty
    NewLevelIndex          : Integer;                     // quick and dirty
    DebugLayerEnabled      : Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure FormBegin(aForm: IForm);
    procedure FormEnd(aForm: IForm);
    procedure Interrupt;
    procedure ToggleVoiceEnabled;
    property CurrentForm: IForm read fCurrentForm;
  end;

var
  App: TApp; // this is instantiated by the main form

implementation

uses
  Base.Strings,
  Form.Base;

{ TGameParams }

constructor TApp.Create;
begin
  inherited Create;
  Config.Load;

  Consts.Init(Config.PathToStyles, Config.PathToMusic, Config.PathToSounds, Config.PathToReplay);
  Consts.SetStyleName(Config.StyleName);
  NewStyleName := Consts.StyleName; // prevent change
  CurrentDisplay.MonitorIndex := Config.Monitor;

  NewSectionIndex := -1;
  NewLevelIndex := -1;

  fFormStack := TList<IForm>.Create;
  VoiceMgr := TVoiceMgr.Create(Config.VoiceOptions, Config.MiscOptions.Voice);
  ReplayCache := TReplayCache.Create;
  Level := Tlevel.Create;
  Renderer := TRenderer.Create;
  SoundMgr := TSoundMgr.Create;
  SoundData.Init(SoundMgr, Consts.PathToSounds);
  GlobalGame := TLemmingGame.Create;
end;

destructor TApp.Destroy;
// Note: we do *not* free the style, because it is pooled
begin

  Config.StyleName := Consts.StyleName;
  Config.Monitor := CurrentDisplay.MonitorIndex;
  Config.Save;

  FreeAndNil(GlobalGame);
  FreeAndNil(Renderer);
  FreeAndNil(Level);
  FreeAndNil(SoundMgr);
  FreeAndNil(GraphicSet);
  FreeAndNil(StyleCache);
  FreeAndNil(ReplayCache);
  FreeAndNil(VoiceMgr);
  FreeAndNil(fFormStack);

  Consts.Done;
  inherited Destroy;
end;

procedure TApp.FormBegin(aForm: IForm);
begin
  fFormStack.Add(aForm);
  fCurrentForm := aForm;
end;

procedure TApp.FormEnd(aForm: IForm);
begin
  fFormStack.Remove(aForm);
  if fFormStack.Count > 0 then
    fCurrentForm := fFormStack.Last
  else
    fCurrentForm := nil;
end;

procedure TApp.Interrupt;
var
  list: TList<IForm>;
begin
  list := TList<IForm>.Create;
  try
    list.AddRange(fFormStack);
    for var i := list.Count - 1 downto 0 do
      if list[i] is TAppForm then
        TAppForm(list[i]).CloseScreen(TGameScreenType.Interrupted)
  finally
    list.Free;
  end;
end;

procedure TApp.ToggleVoiceEnabled;
begin
  if VoiceMgr.Enabled then
    Speak(TVoiceOption.VoiceDisable, False);
  VoiceMgr.Enabled := not VoiceMgr.Enabled;
  Config.MiscOptions.Voice := VoiceMgr.Enabled;
  if VoiceMgr.Enabled then
    Speak(TVoiceOption.VoiceEnable, False);
end;

end.


