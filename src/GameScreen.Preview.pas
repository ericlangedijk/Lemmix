unit GameScreen.Preview;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Graphics,
  System.SysUtils,
  GR32, GR32_Image, GR32_Layers,
  Base.Utils,
  Dos.Consts, Dos.Structures,
  Prog.Types, Prog.App, Prog.Data, Prog.Strings,
  Game.Rendering,
  Styles.Base,
  Level.Base,
  Form.Message,
  GameScreen.Base, GameScreen.Player;

type
  TGamePreviewScreen = class(TGameBaseScreen)
  private
    fLevelErrorCount: Integer;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Form_MouseWheelDown(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
    procedure Form_MouseWheelUp(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ShowNextLevel(forwards: Boolean);
    function GetScreenText: string;
  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildScreen; override;
    procedure PrepareGameParams; override;
  end;

implementation

{ TGamePreviewScreen }

constructor TGamePreviewScreen.Create(aOwner: TComponent);
begin
  inherited;
  OnKeyDown := Form_KeyDown;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
  OnMouseWheelDown := Form_MouseWheelDown;
  OnMouseWheelUp := Form_MouseWheelUp;

//  MainPal := GetDosMainMenuPaletteColors32;
  InitializeImageSizeAndPosition(640, 350);
  ExtractBackGround;
  ExtractPurpleFont;
end;

procedure TGamePreviewScreen.BuildScreen;
var
  Mainpal: TArrayOfColor32;
  Temp, W: TBitmap32;
  DstRect: TRect;
begin
  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;

    // prepare the renderer, this is a little bit shaky (not sure if this is the right place)
    App.GraphicSet.Load(App.Level.Info.GraphicSet, App.Level.Info.GraphicSetEx);
    App.Renderer.Prepare(TRenderInfoRec.Create(App.Level, App.GraphicSet, TMiscOption.RepairCustomLevelErrors in App.Config.MiscOptions), fLevelErrorCount);

    if fLevelErrorCount > 0 then
      DlgWarning('This level contrains ' + fLevelErrorCount.ToString + ' errors.');

    Temp := TBitmap32.Create;
    W := TBitmap32.Create;
    try
      Temp.SetSize(640, 350);
      Temp.Clear(0);
      // draw level preview
      W.SetSize(GAME_BMPWIDTH, 160);
      W.Clear(0);
      App.Renderer.RenderWorld(W, True);
      DstRect := Rect(0, 0, 400, 40); // div 4
      DstRect.Offset(120, 20); // set location
      W.DrawTo(Temp, DstRect, W.BoundsRect);
      // draw background
      TileBackgroundBitmap(0, 78, Temp);
      // draw text
      DrawPurpleText(Temp, GetScreenText, 0, 80);
      ScreenImg.Bitmap.Assign(Temp);
    finally
      W.Free;
      Temp.Free;
    end;
  finally
    ScreenImg.EndUpdate;
    if Visible then begin
      ScreenImg.Changed;
      ScreenImg.Invalidate;
    end;
  end;
end;

destructor TGamePreviewScreen.Destroy;
begin
  inherited;
end;

procedure TGamePreviewScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE : CloseScreen(TGameScreenType.Menu);
    VK_RETURN : CloseScreen(TGameScreenType.Play);
    VK_UP     : ShowNextLevel(True);
    VK_DOWN   : ShowNextLevel(False);
  end;
end;

procedure TGamePreviewScreen.Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseScreen(TGameScreenType.Play);
end;

procedure TGamePreviewScreen.Form_MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  ShowNextLevel(False);
end;

procedure TGamePreviewScreen.Form_MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  ShowNextLevel(True);
end;

procedure TGamePreviewScreen.Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button = mbLeft then
    CloseScreen(TGameScreenType.Play);
end;

function TGamePreviewScreen.GetScreenText: string;
var
  PercentSaved: Integer;
  PercStr: string;
  ReplayString: string;
  StyleDecriptor: string;
begin
  Assert(App <> nil);

  if App.ReplayFileName <> '' then
    ReplayString := ' (Replay)'
  else
    ReplayString := '';

  StyleDecriptor := App.CurrentLevelInfo.Style.StyleInformation.Description;
  if StyleDecriptor.Length > 24 then
    StyleDecriptor := App.CurrentLevelInfo.Style.Name;

  PercentSaved := Percentage(App.Level.Info.LemmingsCount, App.Level.Info.RescueCount);
  PercStr := (PercentSaved.ToString + '%').PadRight(4);

  Result := Format(SPreviewString_dsdsss,
              [App.CurrentLevelInfo.LevelIndex + 1, // 1-based levelindex
               App.Level.Info.Title.Trim,
               App.Level.Info.LemmingsCount,
               PercStr,
               App.Level.Info.ReleaseRate,
               App.Level.Info.TimeLimit,
               App.CurrentLevelInfo.Section.SectionName + ReplayString,
               StyleDecriptor //App.CurrentLevelInfo.Style.Name // todo: optional
             ]);
end;

procedure TGamePreviewScreen.PrepareGameParams;
begin
  inherited PrepareGameParams;
  if App.CurrentLevelInfo = nil then
    App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
  if not Assigned( App.CurrentLevelInfo) then
    Throw('Level loading information is nil', 'Prepare');
  App.CurrentLevelInfo.LoadLevel(App.Level);
end;

procedure TGamePreviewScreen.ShowNextLevel(forwards: Boolean);
begin
  if not App.Config.UseCheatScrollingInPreviewScreen then
    Exit;
  if forwards then begin
    App.CurrentLevelInfo := App.CurrentLevelInfo.Next;
    if App.CurrentLevelInfo = nil then
      App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
    PrepareGameparams;
    BuildScreen;
  end
  else begin
    App.CurrentLevelInfo := App.CurrentLevelInfo.Prev;
    if App.CurrentLevelInfo = nil then
      App.CurrentLevelInfo := App.Style.LevelSystem.LastLevel;
    PrepareGameparams;
    BuildScreen;
  end;
end;

end.

