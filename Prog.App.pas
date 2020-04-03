unit Prog.App; // todo: rename to global or something

{$include lem_directives.inc}

{-------------------------------------------------------------------------------
  The TApp class is in fact just a global var which is used by all
  screens. not much more than a placeholder for the program / game logic.
  But at creation it load the configuration file and the style is set.
-------------------------------------------------------------------------------}

interface

uses
  System.SysUtils,
  GR32,
  Base.Utils,
  Styles.Base,
  Level.Base,
  Prog.Types, Prog.Base, Prog.Config, Prog.Cache,
  Game, Game.Rendering;

type
  TApp = class sealed
  public
    Config                 : TConfig;                     // loaded in contructor
    ReplayFileName         : string;                      // determined by mainform
    Style                  : TStyle;                      // constructed and initialized by mainform
    StyleCache             : TStyleCache;                 // constructed and initialized by mainform
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
    constructor Create;
    destructor Destroy; override;
  end;

var
  App: TApp; // this is instantiated by the main form

implementation

{ TGameParams }

constructor TApp.Create;
begin
  inherited Create;
  Config.Load;
  Consts.Init(Config.PathToStyles, Config.PathToMusic);
  Consts.SetStyleName(Config.StyleName);
  NewStyleName := Consts.StyleName; // prevent change
  CurrentDisplay.MonitorIndex := Config.Monitor;

  NewSectionIndex := -1;
  NewLevelIndex := -1;

  Level := Tlevel.Create;
  Renderer := TRenderer.Create;
  GlobalGame := TLemmingGame.Create;

end;

destructor TApp.Destroy;
begin
  Config.StyleName := Consts.StyleName;
  Config.Monitor := CurrentDisplay.MonitorIndex;
  Config.Save;

  FreeAndNil(GlobalGame);
  FreeAndNil(Renderer);
  FreeAndNil(Level);

  // we do not free the style, because it is pooled

  FreeAndNil(GraphicSet);
  FreeAndNil(StyleCache);

  Consts.Done;
  inherited Destroy;
end;

end.


