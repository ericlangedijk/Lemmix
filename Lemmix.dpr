program Lemmix;

{$include lem_directives.inc}
{$include lem_resources.inc}

uses
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Base.Bitmaps in 'Base.Bitmaps.pas',
  Base.Utils in 'Base.Utils.pas',
  Dos.Bitmaps in 'Dos.Bitmaps.pas',
  Dos.Compression in 'Dos.Compression.pas',
  Dos.Consts in 'Dos.Consts.pas',
  Dos.MainDat in 'Dos.MainDat.pas',
  Dos.Structures in 'Dos.Structures.pas',
  Form.Base in 'Form.Base.pas',
  Form.Main in 'Form.Main.pas' {FormMain},
  Form.Message in 'Form.Message.pas',
  Game.Rendering in 'Game.Rendering.pas',
  Game.SkillPanel in 'Game.SkillPanel.pas',
  Game.Sound in 'Game.Sound.pas',
  Game in 'Game.pas',
  GameScreen.Base in 'GameScreen.Base.pas',
  GameScreen.Config in 'GameScreen.Config.pas',
  GameScreen.Finder in 'GameScreen.Finder.pas',
  GameScreen.LevelCode in 'GameScreen.LevelCode.pas',
  GameScreen.Menu in 'GameScreen.Menu.pas',
  GameScreen.Options in 'GameScreen.Options.pas',
  GameScreen.Player in 'GameScreen.Player.pas',
  GameScreen.Postview in 'GameScreen.Postview.pas',
  GameScreen.Preview in 'GameScreen.Preview.pas',
  Level.Base in 'Level.Base.pas',
  Level.Hash in 'Level.Hash.pas',
  Level.Loader in 'Level.Loader.pas',
  Meta.Structures in 'Meta.Structures.pas',
  Prog.Base in 'Prog.Base.pas',
  Prog.App in 'Prog.App.pas',
  Prog.Config in 'Prog.Config.pas',
  Prog.Data in 'Prog.Data.pas',
  Prog.Cache in 'Prog.Cache.pas',
  Prog.Strings in 'Prog.Strings.pas',
  Prog.Types in 'Prog.Types.pas',
  Styles.Factory in 'Styles.Factory.pas',
  Styles.Base in 'Styles.Base.pas',
  Styles.Dos in 'Styles.Dos.pas',
  Styles.User in 'Styles.User.pas';

{$R *.res}

begin
  if not InitializeLemmix then
    Exit;

  {$ifdef debug}
  ReportMemoryLeaksOnShutdown := True;
  {$endif}

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.

