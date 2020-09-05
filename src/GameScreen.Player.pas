unit GameScreen.Player;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.MMSystem,
  System.Types, System.Classes, System.SysUtils, System.Math, System.Generics.Collections, System.UITypes,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms, Dialogs, Vcl.ExtCtrls, Vcl.Imaging.PngImage, Vcl.ClipBrd,
  GR32, GR32_Image, GR32_Backends, GR32_Layers,
  Base.Utils, Base.Bitmaps,
  Dos.Consts,
  Prog.Base, Prog.Types, Prog.App, Prog.Data,
  Level.Base,
  Game, Game.Rendering, Game.SkillPanel,
  GameScreen.Base,
  Form.Message;

type
  TGameScroll = (
    None,
    Right,
    Left
  );

  TGameScreenPlayer = class(TGameBaseScreen)
  private
    const InvalidPoint: TPoint = (X: -1; Y: -1);
  private
    const INTERVAL_FRAME = 58;
    const INTERVAL_FRAME_SUPERLEMMING = 20;
    const INTERVAL_FRAME_FASTFORWARD = 10;
    const INTERVAL_SCROLL = 58;
    const INTERVAL_NUKE_KEY = 250;
  private
  // game eventhandler
    procedure Game_Finished(Sender: TObject);
  // self eventhandlers
    procedure Form_Activate(Sender: TObject);
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Form_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  // app eventhandlers
    procedure Application_Idle(Sender: TObject; var Done: Boolean);
    procedure Application_Deactivate(Sender: TObject);
    procedure Application_Activate(Sender: TObject);
  // gameimage eventhandlers
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Img_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Img_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  // skillpanel eventhandlers
    procedure SkillPanel_MinimapClick(Sender: TObject; const P: TPoint);
    procedure SkillButtons_MouseDown(aButton: TSkillPanelButton; isDoubleClick: Boolean);
    procedure SkillButtons_MouseUp;
  // internal
    procedure DoLock; inline;
    procedure DoUnlock; inline;
    procedure DoFreeMouseClip; inline;
    procedure DoRestoreMouseClip; inline;
    procedure CheckResetCursor;
    procedure CheckScroll;
    procedure SaveState;
    procedure GotoSaveState;
    procedure GotoIteration(aTargetIteration: Integer);
    procedure CheckAdjustReleaseRate;
    procedure LoadReplay;
    procedure ShowMechanicsInformation;
    procedure SetAdjustedGameCursorPoint(BitmapPoint: TPoint);
    procedure StartReplay;
    procedure StartReplayFromFile(const aFileName: string);
    procedure InitializeCursor;
  private
    fGame                             : TLemmingGame;      // reference to App.Game
    Img                               : TImage32;          // the image in which the level is drawn (reference to inherited ScreenImg!)
    SkillPanel                        : TSkillPanelToolbar;// our good old dos skill panel
    MustForceUpdateOneFrame           : Boolean;           // used when paused
    FrameTimer                        : TTicker;
    ScrollTimer                       : TTicker;
    GameScroll                        : TGameScroll;       // scrollmode
    MouseScroll                       : Boolean;           // input scroll = mouse
    KeyBoardScroll                    : Boolean;           // input scroll = keyboard
    KeyBoardPageScroll                : Boolean;           // scroll a whole screen by keyboard
    MouseClipRect                     : TRect;             // we clip the mouse when there is more space
    fPlayLock                         : Integer;           // use in idle en set to false whenever we don't want to play
    fClipLock                         : Integer;           // used when showing a dialog
    HCursor1                          : HCURSOR;           // normal play cursor
    HCursor2                          : HCURSOR;           // highlight play cursor
    LemCursorIconInfo                 : TIconInfo;         // normal play cursor icon
    LemSelCursorIconInfo              : TIconInfo;         // highlight play cursor icon
    MaxDisplayScale                   : Integer;           // calculated in constructor
    DisplayScale                      : Integer;           // what's the zoomfactor (mostly 2, 3 or 4. in 2020 it is 5. highdpi monitor behaviour unknown yet)
    MinScroll                         : Single;            // scroll boundary for image
    MaxScroll                         : Single;            // scroll boundary for image
    fSaveStateIteration               : Integer;           // one savestate
//    fScrollSpeed                      : Integer;
    fLastShiftState                   : TShiftState;       // set in keyup en keydown events, used in keypress
    fMaximizingReleaseRate            : Boolean;           // only when pausing!
    fMinimizingReleaseRate            : Boolean;           // only when pausing!
    fLastNukeKeyTick                  : Int64;
    fApplicationWasDeactivated        : Boolean;
    fMousePosBeforeDeactivation       : TPoint;
    fScreenshotBuffer                 : TBitmap;
  // overridden
  protected
    procedure ScaleControlsForDpi(NewPPI: Integer); override;
    procedure PrepareGameParams; override;
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); override;
  // internal properties
    property Game: TLemmingGame read fGame;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses Form.Base;

{ TGameControllerForm }

procedure TGameScreenPlayer.Application_Idle(Sender: TObject; var Done: Boolean);
{---------------------------------------------------------------------------------------------
  • Main heartbeat of the player.
  • This method together with Game.UpdateLemmings() take care of most game-mechanics.
  • A bit problematic is the releaserate handling:
    if the game is paused it RR is handled here. if not it is handled by Game.UpdateLemmings.
  • #EL 2020-03-16: better handling of hyperspeed
----------------------------------------------------------------------------------------------}

var
  CurrTick: Int64;
  pause, ForceOne, TimeForFrame, TimeForScroll: Boolean;
begin
  if (fPlayLock > 0) or not Game.Playing or Game.GameFinished then begin
    Done := True;
    Exit;
  end;

  // this makes sure this method is called very often :-)
  Done := False;

  // handle hyperspeed jumps
  if Game.HyperSpeed then begin
    Game.UpdateLemmings;
    // check hyperspeed-end and repaint
    if Game.CurrentIteration >= Game.TargetIteration then begin
      Game.HyperSpeedEnd;
      SkillPanel.RefreshInfo;
      SkillPanel.DrawMinimap(Game.MinimapBuffer);
      CheckResetCursor;
    end;
    Exit;
  end;

  ForceOne := MustForceUpdateOneFrame;
  MustForceUpdateOneFrame := False;

  CurrTick := QueryTimer;

  TimeForFrame := FrameTimer.Check(CurrTick);
  TimeForScroll := ScrollTimer.Check(CurrTick);

  // relax CPU
  if not Game.FastForward then
    Sleep(1);

  pause := Game.Paused and (Game.CurrentIteration >= Game.StartPauseIteration);

  if ForceOne or TimeForFrame or TimeForScroll then begin

    // only in paused mode adjust RR. If not paused it's updated per frame.
    if Game.Paused then
      if (TimeForScroll and not Game.Replaying) or ForceOne then
        CheckAdjustReleaseRate;

    // handle scrolling
    if TimeForScroll then begin
      ScrollTimer.Reset(CurrTick);
      CheckScroll;
    end;

    // handle game mechanics
    if (TimeForFrame and not pause) or (Forceone and pause) then begin
      FrameTimer.Reset(CurrTick);
      Game.UpdateLemmings;
    end;

    SkillPanel.RefreshInfo;
    SkillPanel.DrawMinimap(Game.MinimapBuffer);
    CheckResetCursor;

  end;

end;

procedure TGameScreenPlayer.Application_Activate(Sender: TObject);
var
//  currentPos: TPoint;
  restoreMousePos: Boolean;
begin
  GameScroll := TGameScroll.None;
  restoreMousePos := fApplicationWasDeactivated and not MouseClipRect.Contains(Mouse.CursorPos);
  ClipCursor(@MouseClipRect);

  if restoreMousePos and (fMousePosBeforeDeactivation <> InvalidPoint) then begin
    var P: TPoint := fMousePosBeforeDeactivation;
    fMousePosBeforeDeactivation := InvalidPoint;
    SetCursorPos(P.X, P.Y);
    P := Img.ScreenToClient(P);
    if Img.BoundsRect.Contains(P) then begin
      P := Img.ControlToBitmap(P);
      SetAdjustedGameCursorPoint(P);
    end;
  end;

  fApplicationWasDeactivated := False;
end;

procedure TGameScreenPlayer.Application_Deactivate(Sender: TObject);
// release the cursor from its prison when another application is selected.
// when multiple monitors are active this is needed
begin
  fApplicationWasDeactivated := True;
  GameScroll := TGameScroll.None;
  fMousePosBeforeDeactivation := Mouse.CursorPos;
  ClipCursor(nil);
end;

procedure TGameScreenPlayer.DoLock;
begin
  Assert(fPlayLock >= 0);
  Inc(fPlayLock);
end;

procedure TGameScreenPlayer.DoUnlock;
begin
  Dec(fPlayLock);
  Assert(fPlayLock >= 0);
end;

procedure TGameScreenPlayer.DoFreeMouseClip;
begin
  Inc(fClipLock);
  if fClipLock = 1 then
    ClipCursor(nil);
end;

procedure TGameScreenPlayer.DoRestoreMouseClip;
begin
  Dec(fClipLock);
  if fClipLock = 0 then
    ClipCursor(@MouseClipRect);
end;

procedure TGameScreenPlayer.GotoIteration(aTargetIteration: Integer);
{-------------------------------------------------------------------------------
  Go in hyperspeed from the beginning to aTargetIteration
-------------------------------------------------------------------------------}
begin
  DoLock;
  try
    Game.Start(True);
    Game.HyperSpeedBegin;
    Game.TargetIteration := aTargetIteration;
    SkillPanel.RefreshInfo;
  finally
    DoUnlock;
  end;
end;

procedure TGameScreenPlayer.CheckResetCursor;
begin
  if Screen.Cursor <> Game.CurrentCursor then begin
    Img.Cursor := Game.CurrentCursor;
    Screen.Cursor := Game.CurrentCursor;
  end;
end;

procedure TGameScreenPlayer.CheckScroll;

    procedure CheckUpdateGameCursor;
    // #EL 2020-02-29. never noticed this all these years: when scrolling with keyboard the cursor was not updated.
    // And when the mouse was left or right at the border of the level the cursor was not updated as well.
    begin
      if MouseScroll or KeyBoardScroll then begin
        var P: TPoint := Mouse.CursorPos;
        P := Img.ScreenToClient(P);
        P := Img.ControlToBitmap(P);
        SetAdjustedGameCursorPoint(P);
      end;
    end;

const
  SCROLL_PIXELS_PRECISE = 1;
  SCROLL_PIXELS_NORMAL = 8;
  SCROLL_PIXELS_PAGE = 328;
var
  scrollDist: Integer;

begin
  // this is done when multiple monitors are active
  if not Application.Active then
    Exit;

  case GameScroll of
    TGameScroll.Right:
      begin
        scrollDist := SCROLL_PIXELS_NORMAL;
        if KeyBoardPageScroll then
          scrollDist := SCROLL_PIXELS_PAGE
        else if KeyBoardScroll and (ssCtrl in fLastShiftState) then
          scrollDist := SCROLL_PIXELS_PRECISE;
        Img.OffsetHorz := Max(MinScroll * DisplayScale, Img.OffSetHorz - DisplayScale * scrollDist{ * fScrollSpeed});
        CheckUpdateGameCursor;
      end;
    TGameScroll.Left:
      begin
        scrollDist := SCROLL_PIXELS_NORMAL;
        if KeyBoardPageScroll then
          scrollDist := SCROLL_PIXELS_PAGE
        else if KeyBoardScroll and (ssCtrl in fLastShiftState) then
          scrollDist := SCROLL_PIXELS_PRECISE;
        Img.OffsetHorz := Min(MaxScroll * DisplayScale, Img.OffSetHorz + DisplayScale * scrollDist{ * fScrollSpeed});
        CheckUpdateGameCursor;
      end;
  end;
end;

constructor TGameScreenPlayer.Create(aOwner: TComponent);
var
  HScale, VScale: Integer;
begin
  inherited Create(aOwner);

  fPlayLock := 1;

  // create game
  fGame := App.GlobalGame; // set ref to GlobalGame
  fGame.OnFinish := Game_Finished;
//  fScrollSpeed := 1;

  fSaveStateIteration := -1;

  Img := ScreenImg; // set ref to inherited screenimg (just for a short name)
  Img.RepaintMode := rmOptimizer;
  //Img.Color := clBlack;//None;
  Img.BitmapAlign := baCustom;
  Img.ScaleMode := smScale;

  // create toolbar
  SkillPanel := TSkillPanelToolbar.Create(Self);
  SkillPanel.Parent := Self;

  // calculate displayscale
  HScale := CurrentDisplay.BoundsRect.Width div 320;// Screen.Width div 320;
  VScale := CurrentDisplay.BoundsRect.Height div 200;// Screen.Height div 200;
  DisplayScale := Min(HScale, VScale);
  MaxDisplayScale := DisplayScale;

  Self.KeyPreview := True;

  // set eventhandlers
  Self.OnActivate := Form_Activate;
  Self.OnKeyDown := Form_KeyDown;
  Self.OnKeyUp := Form_KeyUp;
  Self.OnKeyPress := Form_KeyPress;
  Self.OnMouseMove := Form_MouseMove;
  Self.OnMouseUp := Form_MouseUp;

  Img.OnMouseDown := Img_MouseDown;
  Img.OnMouseMove := Img_MouseMove;
  Img.OnMouseUp := Img_MouseUp;

//  SkillPanel.Game := fGame; // this links the game to the infopainter interface as well
  SkillPanel.OnMinimapClick := SkillPanel_MinimapClick;
  SkillPanel.OnSkillButtonsMouseDown := SkillButtons_MouseDown;
  SkillPanel.OnSkillButtonsMouseUp := SkillButtons_MouseUp;

  Game.InfoPainter := SkillPanel;

  fMousePosBeforeDeactivation := InvalidPoint;

  fScreenshotBuffer := TBitmap.Create;

  Application.OnIdle := Application_Idle;
  Application.OnDeactivate := Application_Deactivate;
  Application.OnActivate := Application_Activate;

end;

destructor TGameScreenPlayer.Destroy;
begin
  DoLock;
  Application.OnIdle := nil;
  if HCursor1 <> 0 then
    DestroyIcon(HCursor1);
  if HCursor2 <> 0 then
    DestroyIcon(HCursor2);
  fScreenshotBuffer.Free;
  inherited Destroy;
end;

procedure TGameScreenPlayer.Form_Activate(Sender: TObject);
// activation eventhandler.
var
  F: string;
begin
  OnActivate := nil; // fire only once

  F := App.ReplayFileName;
  App.ReplayFileName := ''; // de-activate replaying

  if F = '' then begin
    if App.ReplayCurrent then // replay last game from postview screen which is still in memory
      StartReplay
    else
      Game.Start(False)
  end
  else
    StartReplayFromFile(F);

  App.ReplayCurrent := False;
  fPlayLock := 0;
end;

procedure TGameScreenPlayer.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fLastShiftState := Shift;

  if Key = VK_ESCAPE then
    Game.Finish; // OnFinish eventhandler does the rest

  if not Game.Playing then
    Exit;

  // this is quite important: no gamecontrol if going fast
  if (Key <> VK_F11) and (Game.HyperSpeed or Game.FastForward) then
     Exit;

  // #EL 2008-04-28 enable ctrl+f2 or ctrl+f1 changing releaserate immediatetely. *only* when pausing!
  // ctrl pressed
  if ssCtrl in Shift then begin
    case Key of
      VK_F1:
        if Game.Paused then begin
          Game.SetSelectedSkill(TSkillPanelButton.Slower, True);
          fMinimizingReleaseRate := True;
        end;
      VK_F2:
        if Game.Paused then begin
          Game.SetSelectedSkill(TSkillPanelButton.Faster, True);
          fMaximizingReleaseRate := True;
        end;
      VK_LEFT   :
        begin
          KeyBoardScroll := True;
          GameScroll := TGameScroll.Left;
        end;
      VK_RIGHT:
        begin
          KeyBoardScroll := True;
          GameScroll := TGameScroll.Right;
        end;
    end;
  end
  // no shiftstate
  else if Shift = [] then begin
    if (Key >= VK_F1) and (Key <= VK_F12) then begin
      if Key <> VK_F11 then
        Game.RegainControl;
      case Key of
        VK_F1 : Game.SetSelectedSkill(TSkillPanelButton.Slower, True);
        VK_F2 : Game.SetSelectedSkill(TSkillPanelButton.Faster, True);
        VK_F3 : Game.SetSelectedSkill(TSkillPanelButton.Climber, True);
        VK_F4 : Game.SetSelectedSkill(TSkillPanelButton.Umbrella, True);
        VK_F5 : Game.SetSelectedSkill(TSkillPanelButton.Explode, True);
        VK_F6 : Game.SetSelectedSkill(TSkillPanelButton.Blocker, True);
        VK_F7 : Game.SetSelectedSkill(TSkillPanelButton.Builder, True);
        VK_F8 : Game.SetSelectedSkill(TSkillPanelButton.Basher, True);
        VK_F9 : Game.SetSelectedSkill(TSkillPanelButton.Miner, True);
        VK_F10: Game.SetSelectedSkill(TSkillPanelButton.Digger, True);
        VK_F11: Game.SetSelectedSkill(TSkillPanelButton.Pause);
        VK_F12: begin
                  // double keypress needed to prevent accidently nuking
                  var currTick: Int64 := QueryTimer;
                  if MSBetween(currTick, fLastNukeKeyTick) < INTERVAL_NUKE_KEY
                  then Game.SetSelectedSkill(TSkillPanelButton.Nuke)
                  else fLastNukeKeyTick := currTick;
                end;
      end;
    end
    // other keys
    else begin
      case Key of
        VK_RETURN : SaveState;
        VK_BACK   : GotoSaveState;
        VK_LEFT   :
          begin
            KeyBoardScroll := True;
            GameScroll := TGameScroll.Left;
          end;
        VK_RIGHT:
          begin
            KeyBoardScroll := True;
            GameScroll := TGameScroll.Right;
          end;
//        VK_NEXT:
//          begin
//            KeyBoardScroll := True;
//            KeyBoardPageScroll := True;
//            GameScroll := TGameScroll.Right;
//          end;
//        VK_PRIOR:
//          begin
//            KeyBoardScroll := True;
//            KeyBoardPageScroll := True;
//            GameScroll := TGameScroll.Left;
//          end;
      end;
    end;
  end;
end;

procedure TGameScreenPlayer.Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fLastShiftState := Shift;

  if not Game.Playing then
    Exit;

  KeyBoardScroll := False;

  case Key of
    VK_F1:
      Game.SetSelectedSkill(TSkillPanelButton.Slower, False);
    VK_F2:
      Game.SetSelectedSkill(TSkillPanelButton.Faster, False);
    VK_LEFT, VK_RIGHT:
      GameScroll := TGameScroll.None;
//    VK_NEXT, VK_PRIOR:
//      begin
//        GameScroll := TGameScroll.None;
//        KeyBoardPageScroll := False;
//      end;
  end;
end;

procedure TGameScreenPlayer.SetAdjustedGameCursorPoint(BitmapPoint: TPoint);
{-------------------------------------------------------------------------------
  convert the normal hotspot to the hotspot the game uses (4,9 instead of 7,7)
-------------------------------------------------------------------------------}
begin
  Game.CursorPoint := Point(BitmapPoint.X - 3, BitmapPoint.Y + 2);
end;

procedure TGameScreenPlayer.Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
//  mouse handling of the game
var
  HandleClick: Boolean;
begin
  if Game.Playing and not Game.HyperSpeed then begin

    fMousePosBeforeDeactivation := InvalidPoint;

    Game.RegainControl;
    SetAdjustedGameCursorPoint(Img.ControlToBitmap(Point(X, Y)));

    // normal
    Game.RightMouseButtonHeldDown := ssRight in Shift;
    if Button = mbLeft then begin
      HandleClick := (not Game.Paused or Game.EnableSkillAssignmentsWhenPaused) and not Game.FastForward;
      if HandleClick then
        Game.ProcessSkillAssignment;
    end;
  end;
end;

procedure TGameScreenPlayer.Img_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Game.Playing and not Game.HyperSpeed then begin

    if Application.Active then
      fMousePosBeforeDeactivation := InvalidPoint;

    Game.RightMouseButtonHeldDown := ssRight in Shift;
    MouseScroll := False;

    SetAdjustedGameCursorPoint(Img.ControlToBitmap(Point(X, Y)));

    if Game.Paused then
      Game.HitTest; // maybe move to idle

    if X >= Img.Width - 1 then begin
      GameScroll := TGameScroll.Right;
      MouseScroll := True;
    end
    else if X <= 0 then begin
      GameScroll := TGameScroll.Left;
      MouseScroll := True;
    end
    else
      GameScroll := TGameScroll.None;
  end;

end;

procedure TGameScreenPlayer.Img_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  MouseScroll := False;
  Game.RightMouseButtonHeldDown := ssRight in Shift;
end;

procedure TGameScreenPlayer.InitializeCursor;
const
  PLAYCURSOR_DEFAULT = 1;
  PLAYCURSOR_LEMMING = 2;
var
  bmpMask : TBitmap;
  bmpColor : TBitmap;

    procedure ScaleBmp(bmp:tbitmap; ascale:integer);
    // bad code but it works for now
    var
      b: tbitmap32;
      src,dst:trect;

    begin
      if ascale = 1 then
        Exit;
      b := TBitmap32.create;
      src := Rect(0, 0, bmp.width, bmp.height);
      dst := Rect(0, 0, bmp.width * ascale, bmp.height * ascale);
      b.SetSize(bmp.width * ascale, bmp.height * ascale);
      b.Draw(dst, src, bmp.Canvas.Handle);
//      bmp.SetSize(b.Width, b.Height);
  //    b.DrawTo(bmp.Canvas.Handle, 0, 0);
      bmp.Assign(b);
      b.free;
    end;


begin
  // todo: cache cursor stuff?
  bmpColor := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorDefault);
  bmpMask  := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorDefaultMask);

  ScaleBmp(bmpMask, DisplayScale);
  ScaleBmp(bmpColor, DisplayScale);

  LemCursorIconInfo.fIcon := false;
  LemCursorIconInfo.xHotspot := 7 * DisplayScale;
  LemCursorIconInfo.yHotspot := 7 * DisplayScale;
  LemCursorIconInfo.hbmMask := bmpMask.Handle;
  LemCursorIconInfo.hbmColor := bmpColor.Handle;

  HCursor1 := CreateIconIndirect(LemCursorIconInfo);
  Screen.Cursors[PLAYCURSOR_DEFAULT] := HCursor1;

  img.Cursor := PLAYCURSOR_DEFAULT;
  SkillPanel.img.cursor := PLAYCURSOR_DEFAULT;
  Self.Cursor := PLAYCURSOR_DEFAULT;

  bmpMask.Free;
  bmpColor.Free;

  bmpColor := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHighlight);
  bmpMask  := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHighlightMask);

  scalebmp(bmpmask, DisplayScale);
  scalebmp(bmpcolor, DisplayScale);

  LemSelCursorIconInfo.fIcon := false;
  LemSelCursorIconInfo.xHotspot := 7 * DisplayScale;
  LemSelCursorIconInfo.yHotspot := 7 * DisplayScale;
  LemSelCursorIconInfo.hbmMask := bmpMask.Handle;
  LemSelCursorIconInfo.hbmColor := bmpColor.Handle;

  HCursor2 := CreateIconIndirect(LemSelCursorIconInfo);
  Screen.Cursors[PLAYCURSOR_LEMMING] := HCursor2;

  bmpMask.Free;
  bmpColor.Free;
end;

procedure TGameScreenPlayer.ScaleControlsForDpi(NewPPI: Integer);
// do nothing (!)
begin
  // dpi-awareness will scale the image32 controls (screenimg and skillpanel).
  // but we already place and size and scale them carefully ourselves, relative to the maximized form and currentdisplay.
  // we do not want any resizing from then on.
end;

procedure TGameScreenPlayer.PrepareGameParams;
{-------------------------------------------------------------------------------
  This method is called by the inherited ShowScreen
-------------------------------------------------------------------------------}
var
  Sca: Integer;
  ConfigScale, offsetX, offsetY: Integer; // scale as stored in ini-file
  GameInfo: TGameInfoRec;
begin
  ConfigScale := App.Config.ZoomFactor;

  // set the final displayscale
  Sca := MaxDisplayScale;
  if (ConfigScale > 0) and (ConfigScale <= MaxDisplayScale) then begin
     Sca := ConfigScale;
     DisplayScale := Sca;
  end;

  // repair wrong zoomfactor in config
  if (ConfigScale <> 0) and (ConfigScale > MaxDisplayScale) then
    App.Config.ZoomFactor := Sca;

  App.TargetBitmap := Img.Bitmap;

  // fill game info and prepare game
  GameInfo.Style                            := App.Style;
  GameInfo.Renderer                         := App.Renderer;
  GameInfo.TargetBitmap                     := App.TargetBitmap;
  GameInfo.SoundMgr                         := App.SoundMgr;
  GameInfo.Level                            := App.Level;
  GameInfo.LevelLoadingInfo                 := App.CurrentLevelInfo;
  GameInfo.GraphicSet                       := App.GraphicSet;
  GameInfo.SoundOpts                        := App.Config.SoundOptions;
  GameInfo.UseParticles                     := App.Config.ShowParticles;
  GameInfo.UseGradientBridges               := App.Config.GradientBridges;
  GameInfo.ShowReplayMessages               := TMiscOption.ShowReplayMessages in App.Config.MiscOptions;
  GameInfo.ShowFeedbackMessages             := TMiscOption.ShowFeedbackMessages in App.Config.MiscOptions;
  GameInfo.EnableSkillButtonsWhenPaused     := TMiscOption.EnableSkillButtonsWhenPaused in App.Config.MiscOptions;
  GameInfo.EnableSkillAssignmentsWhenPaused := TMiscOption.EnableSkillAssignmentsWhenPaused in App.Config.MiscOptions;
  GameInfo.UseShuffledMusic                 := TMiscOption.UseShuffledMusic in App.Config.MiscOptions;
  GameInfo.ShowReplayCursor                 := TMiscOption.ShowReplayCursor in App.Config.MiscOptions;
  GameInfo.UsePhotoFlashReplayEffect        := TMiscOption.UsePhotoFlashReplayEffect in App.Config.MiscOptions;
  GameInfo.OptionalMechanics                := App.Config.OptionalMechanics;

  fGame.Prepare(GameInfo);

  // init timers
  ScrollTimer.Reset(0);
  FrameTimer.Reset(0);
  ScrollTimer.Interval := INTERVAL_SCROLL;
  if Game.Level.Info.SuperLemming
  then FrameTimer.Interval := INTERVAL_FRAME_SUPERLEMMING
  else FrameTimer.Interval := INTERVAL_FRAME;

  // sizes
  Img.Width := 320 * Sca;
  Img.Height := 160 * Sca;
  Img.Scale := Sca;
  Img.OffsetHorz := -App.Level.Info.ScreenPosition * Sca;

  offsetX := (CurrentDisplay.BoundsRect.Width - Img.Width) div 2;
  offsetY := (CurrentDisplay.BoundsRect.Height - 200 * Sca) div 2; // including skillpanel
  Img.Left := offsetX;
  Img.Top := offsetY;


  SkillPanel.Top := Img.Top + Img.Height;
  SkillPanel.left := Img.Left;
  SkillPanel.Width := Img.Width;
  SkillPanel.Height := 40 * Sca;

  // almost invisible line around the game
  if (offsetY > 0) and (offsetY > 0) then begin
    var shape: TShape := TShape.Create(Self);
    shape.Parent := Self;
    shape.Brush.Style := bsClear;
    shape.Pen.Color := RGB(6,6,8);
    shape.SendToBack;
    var sr: TRect := Rect(img.Left, img.Top, img.Left + img.Width, SkillPanel.Top + SkillPanel.Height);
    sr.Inflate(1, 1);
    shape.BoundsRect := sr;
  end;

  MouseClipRect.Left := CurrentDisplay.BoundsRect.Left + offsetX;
  MouseClipRect.Top := CurrentDisplay.BoundsRect.Top + offsetY;
  MouseClipRect.Right := MouseClipRect.Left + img.Width;
  MouseClipRect.Bottom := MouseClipRect.Top + 200 * Sca;

  SkillPanel.SetStyleAndGraph(App.Style, App.GraphicSet, Sca);

  MinScroll := -(GAME_BMPWIDTH - 320);
  MaxScroll := 0;

  InitializeCursor;
  var P: TPoint := CurrentDisplay.BoundsRect.CenterPoint;
  ClipCursor(@MouseClipRect); // we first need to clip it. SetCursorPos acts strangely otherwise on multiple monitors
  SetCursorPos(P.X, P.Y);
end;

procedure TGameScreenPlayer.SkillPanel_MinimapClick(Sender: TObject; const P: TPoint);
{-------------------------------------------------------------------------------
  This method is an eventhandler (TSkillPanel.OnMiniMapClick),
  called when user clicks in the minimap-area of the skillpanel.
  Here we scroll the game-image.
-------------------------------------------------------------------------------}
var
  O: Single;
begin
  fMousePosBeforeDeactivation := InvalidPoint;
  O := -P.X * DisplayScale;
  O :=  O + Img.Width div 2;
  if O < MinScroll * DisplayScale then O := MinScroll * DisplayScale;
  if O > MaxScroll * DisplayScale then O := MaxScroll * DisplayScale;
  Img.OffSetHorz := O;
end;

procedure TGameScreenPlayer.SkillButtons_MouseDown(aButton: TSkillPanelButton; isDoubleClick: Boolean);
var
  Exec: Boolean;
begin
  if Game.HyperSpeed or Game.FastForward then
    Exit;

  if Application.Active then
    fMousePosBeforeDeactivation := InvalidPoint;

  if Game.EnableSkillButtonsWhenPaused
  then Exec := True
  else Exec := not Game.Paused or (aButton = TSkillPanelButton.Pause);

  // nuke requires doubleclick
  if Exec and (aButton = TSkillPanelButton.Nuke) then
    Exec := isDoubleClick;

  if Exec then begin
    if aButton <> TSkillPanelButton.Pause then
      Game.RegainControl;
    Game.SetSelectedSkill(aButton, True);
  end;

end;

procedure TGameScreenPlayer.SkillButtons_MouseUp;
begin
  Game.SetSelectedSkill(TSkillPanelButton.Slower, False);
  Game.SetSelectedSkill(TSkillPanelButton.Faster, False);
end;

procedure TGameScreenPlayer.Form_KeyPress(Sender: TObject; var Key: Char);
begin
  if Game.HyperSpeed then
    Exit;

  case Key of
    // --------- NUMBERS ----------------
    '1':
      begin
        // jump one second
        begin
          Game.HyperSpeedBegin;
          Game.TargetIteration := Game.CurrentIteration + 17;
        end
      end;
    '!':
      begin
        // rewind one second
        GotoIteration(Max(0, Game.CurrentIteration - 17));
        // todo: check if this can be handled with a simple targetiteration
      end;
    '5':
      begin
        if App.Config.UseCheatCodes then
          Game.Cheat;
      end;
    '+', '=': Game.ChangeMusicVolume(True);
    '-', '_': Game.ChangeMusicVolume(False);

    // --------- LOWERCASE ------------
    ' ':
      if Game.Playing and not Game.HyperSpeed then
      begin
        // go 10 game seconds further
        if not (ssShift in fLastShiftState) then
        begin
          Game.HyperSpeedBegin;
          Game.TargetIteration := Game.CurrentIteration + 17 * 10;
        end
        // go 10 game seconds back
        else begin
          GotoIteration(Max(0, Game.CurrentIteration - 17 * 10));
        end;
      end;

    'b':
      if Game.Paused then begin
        GotoIteration(Game.CurrentIteration - 2);
        Game.Paused := True;
      end;

    // test method ClipCursor
    'p': Game.SaveCurrentFrameToPng;

    // toggle fastforward
    'f': if not Game.Paused then begin
           Game.FastForward := not Game.FastForward;
           if Game.FastForward then
             FrameTimer.Interval := INTERVAL_FRAME_FASTFORWARD
           else begin
             if Game.Level.Info.SuperLemming
             then FrameTimer.Interval := INTERVAL_FRAME_SUPERLEMMING
             else FrameTimer.Interval := INTERVAL_FRAME;
           end;
         end;

    // show replay information
    'i': ShowMechanicsInformation;

    // load replay file
    'l', 'L': LoadReplay;

    // enable/disable music
    'm': if TSoundOption.Music in Game.SoundOpts
         then Game.SoundOpts := Game.SoundOpts - [TSoundOption.Music]
         else Game.SoundOpts := Game.SoundOpts + [TSoundOption.Music];

    // do next frame if paused
    'n':
      if Game.Paused then
        MustForceUpdateOneFrame := True;

    'o':
      begin
        // todo: convert / overwrite current replayfile to output
        // todo: convert / overwrite current replayfile direct!
      end;

    // start replay
    'r': StartReplay;

    // enable/disable sounds
    's': if TSoundOption.Sound in Game.SoundOpts
         then Game.SoundOpts := Game.SoundOpts - [TSoundOption.Sound]
         else  Game.SoundOpts := Game.SoundOpts + [TSoundOption.Sound];

    // save current game to .\Replay\<leveltitle>.lrb. in addition a more or less readable version is saved too with the extension ".txt"
    'u': Game.Save(False);

    // --------- UPPERCASE ------------
    'B':
      begin
        // todo: check if this can be done faster. on a high-res screen this takes some (visible) time.
        var bmp: TBitmap32 := TBitmap32.Create;
        try
          bmp.SetSize(CurrentDisplay.BoundsRect.Width, CurrentDisplay.BoundsRect.Height);
          Self.PaintTo(bmp.Canvas, 0, 0);
          var png: TPngImage := bmp.ToPng;
          try
            var t: string := StripInvalidFileChars(fGame.Level.Info.Title);
            if t.IsEmpty then
              t := 'screenshot';
            png.SaveToFile(Consts.PathToScreenShots + t + '.png');
            Clipboard.Assign(png);
          finally
            Png.Free;
          end;

        finally
          bmp.Free;
        end;
      end;
    'F':
      begin
        // go ahead one minute
        if Game.Playing then
        begin
          if not Game.HyperSpeed then begin
            Game.HyperSpeedBegin;
            Game.TargetIteration := Game.CurrentIteration + 17 * 60;
          end
          else begin
            Game.HyperSpeedEnd;
          end;
        end;
      end;

  end;

end;

procedure TGameScreenPlayer.Form_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
//  wit h MouseClipRect do
  if (Y >= Img.Top) and (Y <= Img.Top + Img.Height - 1) then begin
    if X <= Img.Left + DisplayScale then
      GameScroll := TGameScroll.Left
    else if X >= Img.Left + Img.Width - 1 + DisplayScale then
      GameScroll := TGameScroll.Right
    else
      GameScroll := TGameScroll.None;
  end
  else
    GameScroll := TGameScroll.None;
end;

procedure TGameScreenPlayer.Form_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GameScroll := TGameScroll.None;
end;

procedure TGameScreenPlayer.CheckAdjustReleaseRate;
{-------------------------------------------------------------------------------
  In the mainloop the decision is made if we really have to update
-------------------------------------------------------------------------------}
begin
  // faster
  if Game.SpeedingUpReleaseRate then begin
    if not fMaximizingReleaseRate then
      Game.AdjustReleaseRate(1)
    else
      Game.AdjustReleaseRate(99);
    fMaximizingReleaseRate := False;
    fMinimizingReleaseRate := False;
  end
  // slower
  else if Game.SlowingDownReleaseRate then begin
    if not fMinimizingReleaseRate then
      Game.AdjustReleaseRate(-1)
    else
      Game.AdjustReleaseRate(-99);
    fMaximizingReleaseRate := False;
    fMinimizingReleaseRate := False;
  end;
end;

procedure TGameScreenPlayer.StartReplay;
begin
  DoLock;
  try
    Game.SetGameResult;
    Game.Start(True);
    SkillPanel.RefreshInfo;
  finally
    DoUnlock;
  end;
end;

procedure TGameScreenPlayer.StartReplayFromFile(const aFileName: string);
var
  error: string;
begin
  DoLock;
  try
    if Game.Recorder.LoadFromFile(aFileName, {out} error) then begin
      Game.Start(True);
      SkillPanel.RefreshInfo;
    end
    else
      DlgInfo(error);
  finally
    DoUnlock;
  end;
end;


procedure TGameScreenPlayer.LoadReplay;
var
  Dlg : TOpenDialog;
  s: string;
begin
  DoLock;
  try
    s := '';
    ClipCursor(nil);
    dlg := TOpenDialog.Create(nil);
    try
      dlg.FileName := '*.lrb';
      if dlg.Execute then
        s := dlg.FileName;
      SetFocus;
    finally
      dlg.free;
      ClipCursor(@MouseClipRect);
    end;
    if s <> '' then begin
      StartReplayFromFile(s);
      exit;
    end;
  finally
    DoUnlock;
  end;
end;

procedure TGameScreenPlayer.ShowMechanicsInformation;
begin
  DoLock;
  DoFreeMouseClip;
    try
      if not fGame.Replaying or not (fGame.Recorder.WasLoaded or fGame.Recorder.WasSaved) then begin
        DlgInfo(
          'Mechanics' + sLineBreak + sLineBreak + fGame.Mechanics.AsText(True),
          'Courier New'
        )
      end
      else begin
        var header: TReplayFileHeaderRec := fgame.Recorder.CurrentHeader;
        DlgInfo(
          'Replay version = ' + header.Version.ToString + sLineBreak + sLineBreak +
          'Mechanics' + sLineBreak + sLineBreak + header.Mechanics.AsText(True),
          'Courier New'
        )
      end;
  finally
    DoRestoreMouseClip;
    DoUnlock;
  end;
end;

procedure TGameScreenPlayer.Game_Finished(Sender: TObject);
begin
  CloseScreen(TGameScreenType.Postview);
end;

procedure TGameScreenPlayer.BeforeCloseScreen(aNextScreen: TGameScreenType);
begin
  fPlayLock := 1;

  // this is in case we are reloaded
  if aNextScreen = TGameScreenType.Interrupted then
    fGame.Terminate;

  Application.OnIdle := nil;
  Application.OnActivate := nil;
  Application.OnDeactivate := nil;
  ClipCursor(nil);

  if aNextScreen <> TGameScreenType.Interrupted then begin
    Game.SetGameResult;
    App.GameResult := Game.GameResultRec;
  end;

  inherited BeforeCloseScreen(aNextScreen);
end;

procedure TGameScreenPlayer.SaveState;
begin
  fSaveStateIteration := Game.CurrentIteration
end;

procedure TGameScreenPlayer.GotoSaveState;
begin
  if fSaveStateIteration >= 0 then
    GotoIteration(fSaveStateIteration);
end;

end.

