unit GameScreen.Player;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.MMSystem,
  System.Types, System.Classes, System.SysUtils, System.Math, System.Generics.Collections, System.UITypes,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms, Dialogs, Vcl.ExtCtrls, Vcl.Imaging.PngImage, Vcl.ClipBrd,
  {$ifdef debug}
  Prog.Tools,
  {$endif}
  GR32, GR32_Image, GR32_Backends, GR32_Layers,
  Base.Utils, Base.Types, Base.Bitmaps, Base.Strings,
  Dos.Consts,
  Prog.Base, Prog.App, Prog.Data, Prog.Voice,
  Level.Base,
  Game, Game.Rendering, Game.SkillPanel,
  GameScreen.Base,  GameScreen.Help,
  Form.Message;
  //

type
  TGameScroll = (
    None,
    Right,
    Left
  );

  TGameScreenPlayer = class(TGameBaseScreen)
  private
    const InvalidPoint: TPoint = (X: -1; Y: -1);
    const INTERVAL_FRAME = 58;
    const INTERVAL_FRAME_SUPERLEMMING = 20;
    const INTERVAL_FRAME_FASTFORWARD = 10;
    const INTERVAL_SCROLL = 58;
    const INTERVAL_NUKE_KEY = 250;
    const SECOND = 17;
    const MINUTE = 17 * 60;
  private
    function GetHelpText: THelpString;
    procedure ShowHelpScreen;
    procedure ShowMechanicsScreen;
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
  // ToolBar eventhandlers
    procedure ToolBar_MinimapClick(Sender: TObject; const P: TPoint);
    procedure ToolBar_MouseDown(aButton: TSkillPanelButton; isDoubleClick: Boolean);
    procedure ToolBar_MouseUp;
    procedure ToolBar_MouseEnter(Sender: TObject);
  // internal
    procedure DoLock; inline;
    procedure DoUnlock; inline;
    procedure CheckResetCursor;
    procedure CheckScroll;
    procedure DragMap(var startX: Integer; currX: Integer);
    procedure GotoSaveState;
    procedure OtherScreenBegin(out screenCursorPoint: TPoint);
    procedure OtherScreenEnd(const restoreScreenCursorPoint: TPoint);
    procedure SaveState;
    procedure SelectReplayFile;
    procedure SetAdjustedGameCursorPoint(BitmapPoint: TPoint);
    procedure StartReplay;
    procedure StartNoReplay;
    procedure StartReplayFromFile(const aFileName: string);
    procedure InitializeCursor;
  private
    fIdle                             : TIdle;
    fGame                             : TLemmingGame;       // reference to App.Game
    Img                               : TImage32;           // the image in which the level is drawn (reference to inherited ScreenImg!)
    ToolBar                           : TSkillPanelToolBar; // our good old dos skill panel
    FrameTimer                        : TTicker;
    ScrollTimer                       : TTicker;
    GameScroll                        : TGameScroll;       // scrollmode
    MouseScroll                       : Boolean;           // input scroll = mouse
    KeyBoardScroll                    : Boolean;           // input scroll = keyboard
    KeyBoardPageScroll                : Boolean;           // scroll a whole screen by keyboard
    MouseClipRect                     : TRect;             // we clip the mouse when there is more space
    fPlayLock                         : Integer;           // use in idle en set to false whenever we don't want to play
    fDraggingMapStartX                : Integer;
    fDraggingMap                      : Boolean;
    HCursor1                          : HCURSOR;           // normal play cursor
    HCursor2                          : HCURSOR;           // highlight play cursor
    HCursor3                          : HCURSOR;           // drag map cursor
    MaxDisplayScale                   : Integer;           // calculated in constructor
    DisplayScale                      : Integer;           // what's the zoomfactor (mostly 2, 3 or 4. on highdpi even higher)
    MinScroll                         : Single;            // scroll boundary for image
    MaxScroll                         : Single;            // scroll boundary for image
    fSaveStateIteration               : Integer;           // one savestate
    fLastShiftState                   : TShiftState;       // set in keyup en keydown events, used in keypress
    fLastNukeKeyTick                  : Int64;
    fApplicationWasDeactivated        : Boolean;
    fMousePosBeforeDeactivation       : TPoint;
    fAlwaysRegainControlOnMouseClick  : Boolean;
    fFullCPU                          : Boolean;
    fKeyIsDown                        : Integer;
    fLastReplayFilename               : string;
    fHelpText                         : THelpString;
  // overridden
  protected
    procedure ScaleControlsForDpi(NewPPI: Integer); override;
    procedure BuildScreen; override;
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); override;
  // internal properties
    property Game: TLemmingGame read fGame;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Form.Base, GameScreen.ReplayFinder;

{ TGameScreenPlayer }

procedure TGameScreenPlayer.Application_Idle(Sender: TObject; var Done: Boolean);
{---------------------------------------------------------------------------------------------
  • Main heartbeat of the player.
  • This method together with Game.Update() take care of most game-mechanics.
----------------------------------------------------------------------------------------------}

var
  CurrTick: Int64;
  TimeForFrame, TimeForScroll: Boolean;

begin
  if (fPlayLock > 0) or not Game.Playing or Game.IsFinished or fDraggingMap then begin
    Done := True;
    Exit;
  end;

  // this makes sure this method is called very often :-)
  Done := False;

  CurrTick := QueryTimer;

  TimeForFrame := FrameTimer.Check(CurrTick);
  TimeForScroll := ScrollTimer.Check(CurrTick);

  // relax CPU
  if not Game.FastForward or fFullCPU then
    Sleep(1);

  if TimeForFrame or TimeForScroll then begin

    // handle scrolling
    if TimeForScroll then begin
      ScrollTimer.Reset(CurrTick);
      CheckScroll;
    end;

    // handle game mechanics
    if TimeForFrame then begin
      FrameTimer.Reset(CurrTick);
      Game.Update;
    end;

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
  fLastShiftState := [];
  fKeyIsDown := 0;
  GameScroll := TGameScroll.None;
  fMousePosBeforeDeactivation := Mouse.CursorPos;
  ClipCursor(nil);
end;

procedure TGameScreenPlayer.DoLock;
begin
  {$ifdef paranoid} Assert(fPlayLock >= 0); {$endif}
  Inc(fPlayLock);
end;

procedure TGameScreenPlayer.DoUnlock;
begin
  Dec(fPlayLock);
  {$ifdef paranoid} Assert(fPlayLock >= 0); {$endif}
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
  // this is done when multiple monitors are active (or the application has no focus)
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
        Img.OffsetHorz := Max(MinScroll * DisplayScale, Img.OffSetHorz - DisplayScale * scrollDist);
        CheckUpdateGameCursor;
      end;
    TGameScroll.Left:
      begin
        scrollDist := SCROLL_PIXELS_NORMAL;
        if KeyBoardPageScroll then
          scrollDist := SCROLL_PIXELS_PAGE
        else if KeyBoardScroll and (ssCtrl in fLastShiftState) then
          scrollDist := SCROLL_PIXELS_PRECISE;
        Img.OffsetHorz := Min(MaxScroll * DisplayScale, Img.OffSetHorz + DisplayScale * scrollDist);
        CheckUpdateGameCursor;
      end;
  end;
end;

procedure TGameScreenPlayer.DragMap(var startX: Integer; currX: Integer);
begin
  if currX < startX then begin
    Img.OffsetHorz := Max(MinScroll * DisplayScale, Img.OffSetHorz - {DisplayScale *} (startX - currX));
    startX := currX;
  end
  else if currX > startX then begin
    Img.OffsetHorz := Min(MaxScroll * DisplayScale, Img.OffSetHorz + {DisplayScale *} (currX - startX));
    startX := currX;
  end;
end;

constructor TGameScreenPlayer.Create(aOwner: TComponent);
var
  HScale, VScale: Integer;
begin
  inherited Create(aOwner);

  fPlayLock := 1;
  fIdle := TIdle.Create(Application_Idle, Application_Activate, Application_Deactivate);

  // create game
  fGame := App.GlobalGame; // set ref to GlobalGame
  fGame.OnFinish := Game_Finished;

  fSaveStateIteration := -1;

  Img := ScreenImg; // set ref to inherited screenimg (just for a short name)
  Img.RepaintMode := rmOptimizer;
  Img.BitmapAlign := baCustom;
  Img.ScaleMode := smScale;

  // create toolbar
  ToolBar := TSkillPanelToolbar.Create(Self);
  ToolBar.Parent := Self;

  // calculate displayscale
  HScale := CurrentDisplay.BoundsRect.Width div 320;
  VScale := CurrentDisplay.BoundsRect.Height div 200;
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

  ToolBar.OnMinimapClick := ToolBar_MinimapClick;
  ToolBar.OnSkillButtonsMouseDown := ToolBar_MouseDown;
  ToolBar.OnSkillButtonsMouseUp := ToolBar_MouseUp;
  ToolBar.Img.OnMouseEnter := ToolBar_MouseEnter;

  Game.Toolbar := Self.ToolBar;
  fMousePosBeforeDeactivation := InvalidPoint;
end;

destructor TGameScreenPlayer.Destroy;
begin
  DoLock;
  fIdle.Free;
  if HCursor1 <> 0 then
    DestroyIcon(HCursor1);
  if HCursor2 <> 0 then
    DestroyIcon(HCursor2);
  if HCursor3 <> 0 then
    DestroyIcon(HCursor3);
  inherited Destroy;
end;

procedure TGameScreenPlayer.Form_Activate(Sender: TObject);
// activation eventhandler.
var
  F: string;
begin
  OnActivate := nil; // fire only once

  F := App.ReplayFileName;
  App.ReplayFileName := string.Empty; // de-activate replaying

  if F = string.Empty then begin
    if App.ReplayCurrent then // replay last game from postview screen which is still in memory
      StartReplay
    else
      Game.Start(False)
  end
  else
    StartReplayFromFile(F);

  App.ReplayCurrent := False;
  fPlayLock := 0;
  fIdle.Active := True;
end;

procedure TGameScreenPlayer.ShowHelpScreen;
var
  cp: TPoint;
begin
  OtherScreenBegin(cp);
  try
    TGameScreenHelp.Execute(gt.SNameGameScreen, GetHelpText);
  finally
    OtherScreenEnd(cp);
  end;
end;

procedure TGameScreenPlayer.ShowMechanicsScreen;
var
  cp: TPoint;
  s: string;
  title: string;

    function IsOrig: Boolean;
    begin
      Result := fGame.Mechanics - [TMechanic.TriggeredTrapLemmixBugSolved] =  DOSORIG_MECHANICS - [TMechanic.TriggeredTrapLemmixBugSolved];
    end;

    function IsOhNo: Boolean;
    begin
      Result := fGame.Mechanics - [TMechanic.TriggeredTrapLemmixBugSolved] =  DOSOHNO_MECHANICS - [TMechanic.TriggeredTrapLemmixBugSolved];
    end;

begin
  if not fGame.Replaying or not (fGame.Recorder.WasLoaded or fGame.Recorder.WasSaved) then begin
    title := gt.SNameGameScreen + ' ' + gt.SWordMechanics;
    s := fGame.Mechanics.AsText(False, False, True);
  end
  else begin
    var header: TReplayFileHeaderRec := fGame.Recorder.CurrentHeader;
    title := gt.SNameGameScreen + ' ' + gt.SWordReplay + ' ' + gt.SWordMechanics;
    s := header.Mechanics.AsText(False, False, True) + CRLF + gt.SWordReplay + ' ' + gt.SWordVersion + tab + header.Version.ToString;
  end;

  if IsOrig then
    title := title + CRLF + CRLF + 'ORIG'
  else if IsOhNo then
    title := title + CRLF + CRLF + 'OHNO'
  else
      title := title + CRLF + CRLF + 'VAR';

  OtherScreenBegin(cp);
  try
    TGameScreenHelp.Execute(title, s);
  finally
    OtherScreenEnd(cp);
  end;
end;

procedure TGameScreenPlayer.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure IsHandled;
    begin
      Key := 0;
    end;

    procedure CheckNuke;
    begin
      if Game.IsNukedByUser then
        Exit;
      // double keypress needed to prevent accidently nuking
      var currTick: Int64 := QueryTimer;
      if MSBetween(currTick, fLastNukeKeyTick) < INTERVAL_NUKE_KEY then
        Game.BtnNuke
      else
        fLastNukeKeyTick := currTick;
    end;

begin
  fLastShiftState := Shift;
  Inc(fKeyIsDown);

  if Key = VK_ESCAPE then
    Game.Finish; // OnFinish eventhandler does the rest

  if not Game.Playing or Game.HyperSpeed then
    Exit;

  // no gamecontrol if going fast
  if not (Key in [VK_F11, VK_PAUSE]) and Game.FastForward then
    Exit;

  // #EL 2008-04-28 enable ctrl+f2 or ctrl+f1 changing releaserate immediatetely. *only* when pausing!
  // ctrl pressed
  if ssCtrl in Shift then begin
    case Key of
      VK_F1:
        begin
          if Game.IsPaused then
            Game.BtnSlower(True);
          IsHandled;
        end;
      VK_F2:
        begin
          if Game.IsPaused then
            Game.BtnFaster(True);
          IsHandled;
        end;
      VK_F12:
        begin
          if Consts.GLOBAL_DEVELOPER_MODUS and (ssShift in Shift) then begin
            Game.DebugLayerEnabled := not Game.DebugLayerEnabled;
            IsHandled;
          end;
        end;
      VK_LEFT:
        begin
          KeyBoardScroll := True;
          GameScroll := TGameScroll.Left;
          IsHandled;
        end;
      VK_RIGHT:
        begin
          KeyBoardScroll := True;
          GameScroll := TGameScroll.Right;
          IsHandled;
        end;
    end;
  end
  // no shiftstate
  else if Shift = [] then begin
    // game buttons
    if Key in [VK_PAUSE, VK_F1..VK_F12] then begin
      if not (Key in [VK_F11, VK_PAUSE]) then
        Game.RegainControl;
      case Key of
        VK_PAUSE : if fKeyIsDown < 2 then Game.BtnTogglePause(TPauseCommandMode.PauseKey);
        VK_F1    : Game.BtnSlower(False);
        VK_F2    : Game.BtnFaster(False);
        VK_F3    : Game.BtnClimber;
        VK_F4    : Game.BtnUmbrella;
        VK_F5    : Game.BtnExplode;
        VK_F6    : Game.BtnBlocker;
        VK_F7    : Game.BtnBuilder;
        VK_F8    : Game.BtnBasher;
        VK_F9    : Game.BtnMiner;
        VK_F10   : Game.BtnDigger;
        VK_F11   : if fKeyIsDown < 2 then Game.BtnTogglePause(TPauseCommandMode.F11);
        VK_F12   : if fKeyIsDown < 2 then CheckNuke;
        end;
      IsHandled;
    end
    // other keys than VK_F1..VK_F12, VK_PAUSE
    else begin
      case Key of
        VK_RETURN:
          begin
            SaveState;
            IsHandled;
          end;
        VK_BACK:
          begin
            GotoSaveState;
            IsHandled;
          end;
        VK_LEFT   :
          begin
            KeyBoardScroll := True;
            GameScroll := TGameScroll.Left;
            IsHandled;
          end;
        VK_RIGHT:
          begin
            KeyBoardScroll := True;
            GameScroll := TGameScroll.Right;
            IsHandled;
          end;
      end;
    end;
  end;
end;

procedure TGameScreenPlayer.Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fLastShiftState := Shift;
  fKeyIsDown := 0;

  if not Game.Playing then
    Exit;

  KeyBoardScroll := False;

  case Key of
    VK_F1:
      Game.BtnStopChangingReleaseRate;
    VK_F2:
      Game.BtnStopChangingReleaseRate;
    VK_LEFT, VK_RIGHT:
      GameScroll := TGameScroll.None;
//    VK_NEXT, VK_PRIOR: todo: future page scroll
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
  if not Game.Playing or Game.HyperSpeed then
    Exit;

  fMousePosBeforeDeactivation := InvalidPoint;

  if (Button = TMouseButton.mbLeft) and (ssAlt in Shift) and not (ssCtrl in Shift) then begin
    fDraggingMap := True;
    fDraggingMapStartX := X;
    Img.Cursor := GAME_CURSOR_DRAG;
    Screen.Cursor := GAME_CURSOR_DRAG;
    Exit;
  end;

  if fAlwaysRegainControlOnMouseClick or (ssCtrl in Shift) then
    Game.RegainControl;
  SetAdjustedGameCursorPoint(Img.ControlToBitmap(Point(X, Y)));

  if Consts.GLOBAL_DEVELOPER_MODUS and (ssAlt in Shift) and (ssCtrl in Shift) then begin
    if (Button = TMouseButton.mbLeft) then
      Game.DeveloperCreateLemmingAtCursorPoint
    else if (Button = TMouseButton.mbRight) then
      Game.Developer99Skills;
    Exit;
  end;

  // normal
  Game.RightMouseButtonHeldDown := ssRight in Shift;
  if Button = mbLeft then begin
    HandleClick := (not Game.IsPaused or Game.GameOptions.SkillAssignmentsEnabledWhenPaused) and not Game.FastForward;
    if HandleClick then
      Game.ProcessSkillAssignment(not fAlwaysRegainControlOnMouseClick);
  end;
end;

procedure TGameScreenPlayer.Img_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Game.Playing and not Game.HyperSpeed then begin

    if Application.Active then
      fMousePosBeforeDeactivation := InvalidPoint;

    Game.RightMouseButtonHeldDown := ssRight in Shift;
    MouseScroll := False;

    if fDraggingMap then begin
      DragMap(fDraggingMapStartX, X);
      Exit;
    end;

    SetAdjustedGameCursorPoint(Img.ControlToBitmap(Point(X, Y)));

    if Game.IsPaused then
      Game.HitTest;

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
  if fDraggingMap then begin
    fDraggingMap := False;
    CheckResetCursor;
  end;
  MouseScroll := False;
  Game.RightMouseButtonHeldDown := ssRight in Shift;
end;

procedure TGameScreenPlayer.InitializeCursor;
var
  bmpMask : TBitmap;
  bmpColor : TBitmap;
  iconInfo: TIconInfo;

    procedure ScaleBmp(bmp: tbitmap; ascale:integer);
    var
      b: tbitmap32;
      src, dst:trect;
    begin
      if ascale = 1 then
        Exit;
      b := TBitmap32.create;
      src := Rect(0, 0, bmp.Width, bmp.Height);
      dst := Rect(0, 0, bmp.Width * ascale, bmp.Height * ascale);
      b.SetSize(bmp.width * ascale, bmp.Height * ascale);
      b.Draw(dst, src, bmp.Canvas.Handle);
      bmp.Assign(b);
      b.free;
    end;

    procedure CopyCursor(bmp: TBitmap; cursorbmp: TBitmap32);
    begin
      cursorbmp.Assign(bmp);
      cursorbmp.ReplaceAlphaForNonZeroAndZeroColors
    end;

begin
  // default game cursor
  bmpColor := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorDefault);
  bmpMask  := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorDefaultMask);

  ScaleBmp(bmpMask, DisplayScale);
  ScaleBmp(bmpColor, DisplayScale);

  iconInfo.fIcon := false;
  iconInfo.xHotspot := 7 * DisplayScale;
  iconInfo.yHotspot := 7 * DisplayScale;
  iconInfo.hbmMask := bmpMask.Handle;
  iconInfo.hbmColor := bmpColor.Handle;

  HCursor1 := CreateIconIndirect(iconInfo);
  Screen.Cursors[GAME_CURSOR_DEFAULT] := HCursor1;

  img.Cursor := GAME_CURSOR_DEFAULT;
  ToolBar.img.cursor := GAME_CURSOR_DEFAULT;
  Self.Cursor := GAME_CURSOR_DEFAULT;

  bmpMask.Free;
  bmpColor.Free;

  // selection game cursor
  bmpColor := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHighlight);
  bmpMask  := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHighlightMask);

  scalebmp(bmpmask, DisplayScale);
  scalebmp(bmpcolor, DisplayScale);

  iconInfo.fIcon := false;
  iconInfo.xHotspot := 7 * DisplayScale;
  iconInfo.yHotspot := 7 * DisplayScale;
  iconInfo.hbmMask := bmpMask.Handle;
  iconInfo.hbmColor := bmpColor.Handle;

  HCursor2 := CreateIconIndirect(iconInfo);
  Screen.Cursors[GAME_CURSOR_LEMMING] := HCursor2;

  bmpMask.Free;
  bmpColor.Free;

  // drag map cursor
  bmpColor := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorDrag);
  bmpMask  := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorDragMask);

  scalebmp(bmpMask, DisplayScale);
  scalebmp(bmpColor, DisplayScale);

  iconInfo.fIcon := false;
  iconInfo.xHotspot := 7 * DisplayScale;
  iconInfo.yHotspot := 7 * DisplayScale;
  iconInfo.hbmMask := bmpMask.Handle;
  iconInfo.hbmColor := bmpColor.Handle;

  HCursor3 := CreateIconIndirect(iconInfo);
  Screen.Cursors[GAME_CURSOR_DRAG] := HCursor3;

  bmpMask.Free;
  bmpColor.Free;
end;

procedure TGameScreenPlayer.OtherScreenBegin(out screenCursorPoint: TPoint);
begin
  DoLock;
  fIdle.Active := False;
  screenCursorPoint := Mouse.CursorPos;
  fMousePosBeforeDeactivation := screenCursorPoint;
  ClipCursor(nil);
end;

procedure TGameScreenPlayer.OtherScreenEnd(const restoreScreenCursorPoint: TPoint);
begin
  SetCursorPos(restoreScreenCursorPoint.X, restoreScreenCursorPoint.Y);
  SetFocus;
  ClipCursor(@MouseClipRect);
  fIdle.Active := True;
  DoUnlock;
end;

procedure TGameScreenPlayer.ScaleControlsForDpi(NewPPI: Integer);
// do nothing (!)
begin
  // dpi-awareness will scale the image32 controls (screenimg and skillpanel).
  // but we already place and size and scale them carefully ourselves, relative to the maximized form and currentdisplay.
  // we do not want any resizing from then on.
end;

procedure TGameScreenPlayer.BuildScreen;
{-------------------------------------------------------------------------------
  This method is called by the inherited ShowScreen
-------------------------------------------------------------------------------}
var
  Sca: Integer;
  ConfigScale, offsetX, offsetY: Integer; // scale as stored in ini-file
  GameInfo: TGameInfoRec;
begin
  fFullCPU := TMiscOption.FullCPU in App.Config.MiscOptions;

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
  GameInfo.Style                := App.Style;
  GameInfo.Renderer             := App.Renderer;
  GameInfo.Img                  := Self.Img;
  GameInfo.TargetBitmap         := App.TargetBitmap;
  GameInfo.DisplayScale         := DisplayScale;
  GameInfo.SoundMgr             := App.SoundMgr;
  GameInfo.ReplayCache          := App.ReplayCache;
  GameInfo.Level                := App.Level;
  GameInfo.LevelLoadingInfo     := App.CurrentLevelInfo;
  GameInfo.GraphicSet           := App.GraphicSet;
  GameInfo.SoundOptions         := App.Config.SoundOptions;
  GameInfo.GameOptions          := App.Config.GameOptions;
  GameInfo.OptionalMechanics    := App.Config.OptionalMechanics;
  GameInfo.MiscOptions          := App.Config.MiscOptions;
  GameInfo.DebugLayerEnabled    := App.DebugLayerEnabled;

  fAlwaysRegainControlOnMouseClick := App.Config.GameOptions.AlwaysRegainControlOnMouseClick;

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


  ToolBar.Top := Img.Top + Img.Height;
  ToolBar.left := Img.Left;
  ToolBar.Width := Img.Width;
  ToolBar.Height := 40 * Sca;

  // almost invisible line around the game
  if (offsetY > 0) and (offsetY > 0) then begin
    var shape: TShape := TShape.Create(Self);
    shape.Parent := Self;
    shape.Brush.Style := bsClear;
    shape.Pen.Color := RGB(6,6,8);
    shape.SendToBack;
    var sr: TRect := Rect(img.Left, img.Top, img.Left + img.Width, ToolBar.Top + ToolBar.Height);
    sr.Inflate(1, 1);
    shape.BoundsRect := sr;
  end;

  MouseClipRect.Left := CurrentDisplay.BoundsRect.Left + offsetX;
  MouseClipRect.Top := CurrentDisplay.BoundsRect.Top + offsetY;
  MouseClipRect.Right := MouseClipRect.Left + img.Width;
  MouseClipRect.Bottom := MouseClipRect.Top + 200 * Sca;

  ToolBar.SetStyleAndGraph(App.Style, App.GraphicSet, Sca);

  MinScroll := -(GAME_BMPWIDTH - 320);
  MaxScroll := 0;

  InitializeCursor;
  var P: TPoint := CurrentDisplay.BoundsRect.CenterPoint;
  ClipCursor(@MouseClipRect); // we first need to clip it. SetCursorPos acts strangely otherwise on multiple monitors
  SetCursorPos(P.X, P.Y);
end;

procedure TGameScreenPlayer.ToolBar_MinimapClick(Sender: TObject; const P: TPoint);
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

procedure TGameScreenPlayer.ToolBar_MouseDown(aButton: TSkillPanelButton; isDoubleClick: Boolean);
var
  Exec: Boolean;
begin
  if Game.HyperSpeed or Game.FastForward then
    Exit;

  if Application.Active then
    fMousePosBeforeDeactivation := InvalidPoint;

  if Game.GameOptions.SkillButtonsEnabledWhenPaused
  then Exec := True
  else Exec := not Game.IsPaused or (aButton = TSkillPanelButton.Pause);

  // nuke requires doubleclick
  if Exec and (aButton = TSkillPanelButton.Nuke) then
    Exec := isDoubleClick;

  if Exec then begin
    if aButton <> TSkillPanelButton.Pause then
      Game.RegainControl;

    case aButton of
      TSkillPanelButton.Slower   : Game.BtnSlower(False);
      TSkillPanelButton.Faster   : Game.BtnFaster(False);
      TSkillPanelButton.Climber  : Game.BtnClimber;
      TSkillPanelButton.Umbrella : Game.BtnUmbrella;
      TSkillPanelButton.Explode  : Game.BtnExplode;
      TSkillPanelButton.Blocker  : Game.BtnBlocker;
      TSkillPanelButton.Builder  : Game.BtnBuilder;
      TSkillPanelButton.Basher   : Game.BtnBasher;
      TSkillPanelButton.Miner    : Game.BtnMiner;
      TSkillPanelButton.Digger   : Game.BtnDigger;
      TSkillPanelButton.Pause    : Game.BtnTogglePause;
      TSkillPanelButton.Nuke     : Game.BtnNuke;
    end;
  end;
end;

procedure TGameScreenPlayer.ToolBar_MouseUp;
begin
  Game.BtnStopChangingReleaseRate;
end;

procedure TGameScreenPlayer.ToolBar_MouseEnter(Sender: TObject);
begin
  if not KeyBoardScroll and MouseScroll then begin
    GameScroll := TGameScroll.None;
    MouseScroll := False;
  end;
end;

procedure TGameScreenPlayer.Form_KeyPress(Sender: TObject; var Key: Char);

    procedure IsHandled;
    begin
      Key := #0;
    end;

begin
  if Game.HyperSpeed or not Game.Playing then
    Exit;

  case Key of
    // signs / numbers -----------------------------------------------------------------------------------------------------------------------------------------

    '1': // skip one second
      begin
        Game.GotoIteration(Game.CurrentIteration + SECOND);
        IsHandled;
      end;
    '!': // rewind one second
      begin
        Game.GotoIteration(Game.CurrentIteration - SECOND);
        IsHandled;
      end;
    '5': // cheat
      begin
        if App.Config.GameOptions.CheatKeyToSolveLevel then
          Game.Cheat;
        IsHandled;
      end;
    '+', '=':
      begin
        Game.ChangeMusicVolume(True);
        IsHandled;
      end;
    '-', '_':
      begin
        Game.ChangeMusicVolume(False);
        IsHandled;
      end;
    '?':
      begin
        ShowHelpScreen;
        IsHandled;
      end;
    {$ifdef debug}
    ^E:
      begin
        ExportStyle(App.Style);
        IsHandled;
        DlgInfo('Exportstyle is ready');
      end;
    {$endif}

    // lowercase -----------------------------------------------------------------------------------------------------------------------------------------------

    ' ': // skip or rewind 10 seconds
      begin
        if not (ssShift in fLastShiftState) then
          Game.GotoIteration(Game.CurrentIteration + SECOND * 10)
        else
          Game.GotoIteration(Game.CurrentIteration - SECOND * 10);
        IsHandled;
      end;

    'b': // rewind one frame
      begin
        if Game.IsPaused then
          Game.GotoIteration(Game.CurrentIteration - 1);
        IsHandled;
      end;

    'f': // toggle fastforward
      begin
        if not Game.IsPaused then begin
           Game.FastForward := not Game.FastForward;
           if Game.FastForward then
             FrameTimer.Interval := INTERVAL_FRAME_FASTFORWARD
           else begin
             if Game.Level.Info.SuperLemming then
               FrameTimer.Interval := INTERVAL_FRAME_SUPERLEMMING
             else
               FrameTimer.Interval := INTERVAL_FRAME;
           end;
         end;
        IsHandled;
      end;

    'i': // show replay information
      begin
        ShowMechanicsScreen;
        IsHandled;
      end;

    // load replay file
    'l', 'L':
      begin
        SelectReplayFile;
        IsHandled;
      end;

    'm': // enable/disable music
      begin
        if TSoundOption.Music in Game.SoundOpts then
          Game.SoundOpts := Game.SoundOpts - [TSoundOption.Music]
        else
          Game.SoundOpts := Game.SoundOpts + [TSoundOption.Music];
        IsHandled;
      end;

    'n': // do next frame if paused
      begin
        if Game.IsPaused then
          Game.GotoIteration(Game.CurrentIteration + 1);
        IsHandled;
      end;
//    'o': // todo: convert current replay to latest
//      begin
//        if DlgConfirm('do you want to...') then
//          exit;
//        // todo: convert / overwrite current replayfile to output
//        // todo: convert / overwrite current replayfile direct!
//      end;

    'p':
      begin
        Game.SaveCurrentFrameToPng;
        IsHandled;
      end;

    'r': // start replay
      begin
        if fKeyIsDown < 2 then
          StartReplay;
        IsHandled;
      end;

    'R': // start replay without replay
      begin
        StartNoReplay;
        IsHandled;
      end;

    's': // enable/disable sounds
      begin
        if TSoundOption.Sound in Game.SoundOpts then
          Game.SoundOpts := Game.SoundOpts - [TSoundOption.Sound]
        else
          Game.SoundOpts := Game.SoundOpts + [TSoundOption.Sound];
        IsHandled;
      end;

    'u': // save replay
      begin
        if fKeyIsDown < 2 then
          Game.Save(False);
        IsHandled;
      end;

    'v', 'V': // voice
      begin
        if fKeyIsDown < 2 then
          App.ToggleVoiceEnabled;
        IsHandled;
      end;

    // run until the end
    'z':
      begin
        if fKeyIsDown < 2 then
          Game.GotoIteration(Game.CurrentIteration + MINUTE * 10); // let it overflow
        IsHandled;
      end;

    //  UPPERCASE ----------------------------------------------------------------------------------------------------------------------------------------------

    'D':
      begin
        Game.GoToIteration(Game.CurrentIteration - MINUTE); // rewind one minute
        IsHandled;
      end;
    'F':
      begin
        Game.GoToIteration(Game.CurrentIteration + MINUTE); // skip one minute
        IsHandled;
      end;
  end;
end;

procedure TGameScreenPlayer.Form_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
// safety when mouseclip is not correct
begin
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

procedure TGameScreenPlayer.StartReplay;
begin
  DoLock;
  try
    Game.SetGameResult; // todo: ???
    Game.Start(True);
  finally
    DoUnlock;
  end;
end;

procedure TGameScreenPlayer.StartNoReplay;
begin
  DoLock;
  try
    Game.SetGameResult;
    Game.Start(False);
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
    Speak(TVoiceOption.StartReplay, True);
    if Game.Recorder.LoadFromFile(aFileName, {out} error) then begin
      fLastReplayFilename := aFilename;
      Game.Start(True);
    end
    else
      DlgInfo(error);
  finally
    DoUnlock;
  end;
end;

procedure TGameScreenPlayer.SelectReplayFile;
var
  newFile: string;
  cp: TPoint;
begin
  OtherScreenBegin(cp);
  try
    newFile := TGameScreenReplayFinder.Execute(Game.LevelLoadingInfo);
  finally
    OtherScreenEnd(cp);
  end;
  if not newFile.IsEmpty then
    StartReplayFromFile(newFile);
end;

procedure TGameScreenPlayer.Game_Finished(Sender: TObject);
begin
  App.DebugLayerEnabled := Game.DebugLayerEnabled; // write back if game changed it
  CloseScreen(TGameScreenType.Postview);
end;

procedure TGameScreenPlayer.BeforeCloseScreen(aNextScreen: TGameScreenType);
begin
  fPlayLock := 1;

  // this is in case we are reloaded / triggered
  if aNextScreen = TGameScreenType.Interrupted then
    fGame.Terminate;

  fIdle.Active := False;
  ClipCursor(nil);

  if aNextScreen <> TGameScreenType.Interrupted then begin
    Game.SetGameResult;
    App.GameResult := Game.GameResultRec;

    if TMiscOption.AutoSaveReplayFiles in App.Config.MiscOptions then begin
      var allow: Boolean := not Game.Recorder.IsEmpty and not SameText(ExtractFilePath(fLastReplayFilename), Consts.PathToAutoSave);
      if allow then
        Game.AutoSave;
    end;
  end;

  inherited BeforeCloseScreen(aNextScreen);
end;

procedure TGameScreenPlayer.SaveState;
begin
  fSaveStateIteration := Game.CurrentIteration; // todo: initialize at -1 ?
end;

procedure TGameScreenPlayer.GotoSaveState;
begin
  if fSaveStateIteration >= 0 then
    Game.GotoIteration(fSaveStateIteration);
end;

function TGameScreenPlayer.GetHelpText: THelpString;
begin
  if not fHelpText.Text.IsEmpty then
    Exit(fHelpText);

  Result.Add(VK_F1, gt.SHelpGameScreen_DecreaseReleaseRate);
  Result.Add(VK_F2,  gt.SHelpGameScreen_IncreaseReleaseRate);
  Result.Add(VK_F3,  gt.SHelpGameScreen_SelectClimberButton);
  Result.Add(VK_F4,  gt.SHelpGameScreen_SelectUmbrellaButton);
  Result.Add(VK_F5,  gt.SHelpGameScreen_SelectExploderButton);
  Result.Add(VK_F6,  gt.SHelpGameScreen_SelectBlockerButton);
  Result.Add(VK_F7,  gt.SHelpGameScreen_SelectBuilderButton);
  Result.Add(VK_F8,  gt.SHelpGameScreen_SelectBasherButton);
  Result.Add(VK_F9,  gt.SHelpGameScreen_SelectMinerButton);
  Result.Add(VK_F10, gt.SHelpGameScreen_SelectDiggerButton);
  Result.Add(VK_F11, gt.SHelpGameScreen_PauseOrUnpause);
  Result.Add(VK_F12, gt.SHelpGameScreen_Nuke);
  Result.Add(VK_F1, [ssCtrl], gt.SHelpGameScreen_SetMinimumReleaseRate);
  Result.Add(VK_F2, [ssCtrl], gt.SHelpGameScreen_SetMaximumReleaseRate);
  Result.Add(VK_PAUSE, gt.SHelpGameScreen_PauseOrUnpause);
  Result.Add(VK_ESCAPE, gt.SHelpGameScreen_FinishGame);
  Result.Add(VK_RETURN, gt.SHelpGameScreen_SaveState);
  Result.Add(VK_BACK,  gt.SHelpGameScreen_GotoSavedState);
  Result.Add('1' ,  gt.SHelpGameScreen_SkipOneSecond);
  Result.Add('!' ,  gt.SHelpGameScreen_RewindOneSecond);
  Result.Add(VK_SPACE, gt.SHelpGameScreen_SkipTenSeconds);
  Result.Add(VK_SPACE, [ssShift], gt.SHelpGameScreen_RewindTenSeconds);
  Result.Add('F' , gt.SHelpGameScreen_SkipOneMinute);
  Result.Add('D' , gt.SHelpGameScreen_RewindOneMinute);
  Result.Add('n' , gt.SHelpGameScreen_SkipOneFrame);
  Result.Add('b' , gt.SHelpGameScreen_RewindOneFrame);
  Result.Add('z' , gt.SHelpGameScreen_SkipToEndOfGame);
  Result.Add('s' , gt.SHelpGameScreen_ToggleSoundsOnOff);
  Result.Add('m' , gt.SHelpGameScreen_ToggleSoundsOnOff);
  Result.Add('v' , gt.SHelpGameScreen_ToggleVoiceOnOff);
  Result.Add('f' , gt.SHelpGameScreen_ToggleGameSpeed);
  Result.Add('+ =' , gt.SHelpGameScreen_IncreaseMusicVolume);
  Result.Add('- _' , gt.SHelpGameScreen_DecreaseMusicVolume);
  Result.Add('p' , gt.SHelpGameScreen_SaveCurrentFrameToPng);
  Result.Add('r' , gt.SHelpGameScreen_ReplayGame);
  Result.Add('R' , gt.SHelpGameScreen_RestartGameWithoutReplay);
  Result.Add('u' , gt.SHelpGameScreen_SaveGame);
  Result.Add('i' , gt.SHelpGameScreen_ShowMechanics);

  if App.Config.GameOptions.CheatKeyToSolveLevel then
    Result.Add('5', gt.SHelpGameScreen_CheatGame);

  if Consts.GLOBAL_DEVELOPER_MODUS then begin
     Result.Add(VK_F12, [ssCtrl, ssShift], 'Toggle debug info on/off');
     Result.Add(MouseStr(TMouseButton.mbLeft, [ssCtrl, ssAlt]), 'Create lemming at cursor');
     Result.Add(MouseStr(TMouseButton.mbRight, [ssCtrl, ssAlt]), 'Set all skills to 99');
  end;
end;

end.


