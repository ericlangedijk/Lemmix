unit GameScreen.Preview;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, System.Classes, System.Math, Vcl.Controls, Vcl.Graphics,
  System.SysUtils,
  GR32, GR32_Image, GR32_Layers,
  Base.Utils, Base.Types, Base.Bitmaps, Base.Strings,
  Dos.Consts, Dos.Structures,
  Prog.Base, Prog.App, Prog.Data, Prog.Voice,
  Game.Rendering,
  Styles.Base,
  Level.Base,
  Form.Message,
  GameScreen.Base, GameScreen.Player, GameScreen.ReplayFinder, GameScreen.Help;

type
  TGamePreviewScreen = class(TGameBaseScreen)
  private
    type
      TStringArray16 = array[0..15] of string;
      TRecolorArray16 = array[0..15] of TFontRecolor;
  private
    fLevelErrorCount: Integer;
    fInitialLevelInfo: TLevelLoadingInformation;
    fSelectedReplayFilename: string;
    function GetHelpText: THelpString;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Form_MouseWheelDown(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
    procedure Form_MouseWheelUp(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ShowNextLevel(forwards: Boolean);
    procedure ToggleSaveToEngine;
    procedure GetScreenLinesAndColors(out aLines: TStringArray16; out aColors: TRecolorArray16);
  protected
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); override;
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TGamePreviewScreen }

constructor TGamePreviewScreen.Create(aOwner: TComponent);
begin
  inherited;
  OnKeyDown := Form_KeyDown;
  OnKeyUp := Form_KeyUp;
  OnKeyPress := Form_KeyPress;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
  OnMouseWheelDown := Form_MouseWheelDown;
  OnMouseWheelUp := Form_MouseWheelUp;

  InitializeImageSizeAndPosition(640, 350);
  ExtractBackGround;
  ExtractPurpleFont;
end;

procedure TGamePreviewScreen.BeforeCloseScreen(aNextScreen: TGameScreenType);
begin
  if aNextScreen = TGameScreenType.Play then begin
    if not fSelectedReplayFilename.IsEmpty then
      App.ReplayFileName := fSelectedReplayFilename
    else if (fInitialLevelInfo <> App.CurrentLevelInfo) and (not App.ReplayFileName.IsEmpty) then
      App.ReplayFileName := string.Empty;
  end;
  inherited BeforeCloseScreen(aNextScreen);
end;

procedure TGamePreviewScreen.BuildScreen;
var
  Mainpal: TArrayOfColor32;
  target, fullLevel: TBitmap32;
  DstRect: TRect;
  text: TStringArray16;
  colors: TRecolorArray16;
  previewLevelHeight, y: Integer;
begin
  if App.CurrentLevelInfo = nil then
    App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;

  if not Assigned(App.CurrentLevelInfo) then
    Throw('Level loading information is nil', 'TGamePreviewScreen.BuildScreen');

  if fInitialLevelInfo = nil then
    fInitialLevelInfo := App.CurrentLevelInfo;

  App.CurrentLevelInfo.LoadLevel(App.Level);

  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;

    // prepare the renderer, this is a little bit shaky (not sure if this is the right place)
    App.GraphicSet.Load(App.Level.Info.GraphicSet, App.Level.Info.GraphicSetEx);
    App.Renderer.Prepare(TRenderInfoRec.Create(App.Level, App.GraphicSet, TMiscOption.RepairCustomLevelErrors in App.Config.MiscOptions), fLevelErrorCount);

    if fLevelErrorCount > 0 then
      DlgWarning(FormatSimple(gt.SErrorThisLevelHasCountErrors_s, [fLevelErrorCount.ToString]));

    target := TBitmap32.Create;
    fullLevel := TBitmap32.Create;
    try
      target.SetSize(640, 350); // the original dos-screen size
      target.Clear(0);

      // draw level preview
      fullLevel.SetSize(GAME_BMPWIDTH, GAME_BMPHEIGHT);
      fullLevel.Clear(0);
      App.Renderer.RenderWorld(fullLevel, True);

      // make preview as big as possible at the top or keep ratio
      if TMiscOption.KeepLevelRatioInPreviewScreen in App.Config.MiscOptions
      then previewLevelHeight := round((target.Width/GAME_BMPWIDTH) * GAME_BMPHEIGHT) // keep ratio for 640/1584 * 160
      else previewLevelHeight := 80;

      DstRect := Rect(0, 0, target.width, previewLevelHeight);
      DstRect.Offset(0, (80 - previewLevelHeight));
      fullLevel.DrawTo(target, DstRect, fullLevel.BoundsRect);

      // draw background
      TileBackgroundBitmap(0, 80, target);

      // draw text
      GetScreenLinesAndColors(text, colors);
      y := 80 + 2;
      for var i := 0 to Length(text) - 1 do begin
        if i < 15 then
          DrawPurpleText(target, text[i], 0, y, colors[i])
        else
          DrawPurpleTextCentered(target, text[i], y);//, colors[i]);
        Inc(y, 16);
      end;

      ScreenImg.Bitmap.Assign(target);
    finally
      fullLevel.Free;
      target.Free;
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

function TGamePreviewScreen.GetHelpText: THelpString;
begin

  Result.Add(VK_ESCAPE, gt.SHelpPreviewScreen_Cancel);
  Result.Add(VK_RETURN, gt.SHelpPreviewScreen_StartGame);
  Result.Add('l', gt.SHelpPreviewScreen_FindMatchingReplays);

  if App.Config.MiscOptions.CheatScrollingInPreviewScreen then begin
    Result.Add(VK_UP, gt.SHelpPreviewScreen_CheatScrollToNextLevel);
    Result.Add(VK_DOWN, gt.SHelpPreviewScreen_CheatScrollToPrevLevel);
    Result.Add(gt.SWordMouseWheel, gt.SHelpPreviewScreen_CheatScrollMouseWheel);
  end;

  if Consts.GLOBAL_DEVELOPER_MODUS then begin
    Result.Add(VK_F10, [ssCtrl, ssShift], gt.SHelpPreviewScreen_BinLevelToggle);
    Result.Add(VK_F12, [ssCtrl, ssShift], gt.SHelpPreviewScreen_ToggleDebugLayer);
  end;
end;

procedure TGamePreviewScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_ESCAPE : CloseScreen(TGameScreenType.Menu);
      VK_RETURN : CloseScreen(TGameScreenType.Play);
      VK_UP     : ShowNextLevel(True);
      VK_DOWN   : ShowNextLevel(False);
    end;

end;

procedure TGamePreviewScreen.Form_KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'l':
        begin
          fSelectedReplayFilename := TGameScreenReplayFinder.Execute(App.CurrentLevelInfo);
          if not fSelectedReplayFilename.IsEmpty then
            CloseScreen(TGameScreenType.Play);
        end;
    '?':
       TGameScreenHelp.Execute(gt.SNamePreviewScreen, GetHelpText);
  end;
end;

procedure TGamePreviewScreen.Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not Consts.GLOBAL_DEVELOPER_MODUS then
    Exit;
  if Shift * [ssCtrl, ssShift] = [ssCtrl, ssShift] then begin
    case Key of
      VK_F10:
        ToggleSaveToEngine;
      VK_F12:
        begin
          App.DebugLayerEnabled := not App.DebugLayerEnabled;
          if App.DebugLayerEnabled then
            Speak('debug layer on', False)
          else
            Speak('debug layer off', False);
        end;
    end;
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

procedure TGamePreviewScreen.GetScreenLinesAndColors(out aLines: TStringArray16; out aColors: TRecolorArray16);
var
  PercentToBeSaved: Integer;
  PercStr: string;
  ReplayString: string;
  StyleDecriptor: string;
const
  IndentStr = '          '; // 10
begin
  {$ifdef paranoid} Assert(App <> nil); {$endif}

  if (not App.ReplayFileName.IsEmpty) and (fInitialLevelInfo = App.CurrentLevelInfo) then begin
    ReplayString := 'Replay';
  end
  else
    ReplayString := string.Empty;

  StyleDecriptor := App.CurrentLevelInfo.Style.StyleInformation.Description;
  if StyleDecriptor.Length > 24 then
    StyleDecriptor := App.CurrentLevelInfo.Style.Name;

  if App.Config.MiscOptions.LemmingsPercentages then begin
    PercentToBeSaved := Percentage(App.Level.Info.LemmingsCount, App.Level.Info.RescueCount);
    PercStr := (PercentToBeSaved.ToString + '%');
  end
  else begin
    PercStr := (App.Level.Info.RescueCount.ToString);
  end;


  aLines[00] := FormatSimple(gt.SPreviewScreen_Level_ss, [(App.CurrentLevelInfo.LevelIndex + 1).ToString, App.Level.Info.Title.Trim]);
  aLines[01] := string.Empty;
  aLines[02] := ReplayString;
  aLines[03] := IndentStr + FormatSimple(gt.SPreviewScreen_NumberOfLemmings_s, [App.Level.Info.LemmingsCount.ToString]);
  aLines[04] := string.Empty;
  aLines[05] := Indentstr + FormatSimple(gt.SPreviewScreen_ToBeSaved_s, [PercStr]);
  aLines[06] := string.Empty;
  aLines[07] := IndentStr + FormatSimple(gt.SPreviewScreen_ReleaseRate_s, [App.Level.Info.ReleaseRate.ToString]);
  aLines[08] := string.Empty;
  aLines[09] := IndentStr + FormatSimple(gt.SPreviewScreen_Time_s, [App.Level.Info.TimeLimit.ToString]);
  aLines[10] := string.Empty;
  aLines[11] := IndentStr + FormatSimple(gt.SPreviewScreen_Rating_s, [App.CurrentLevelInfo.Section.SectionName]);
  aLines[12] := string.Empty;
  aLines[13] := IndentStr + FormatSimple(gt.SPreviewScreen_Style_s, [StyleDecriptor]);
  aLines[14] := string.Empty;
  aLines[15] := gt.SPreviewScreen_PressMouseButtonToContinue; // this one is drawn centered

  aColors[00] := TFontRecolor.Red;
  aColors[01] := TFontRecolor.Purple;
  aColors[02] := TFontRecolor.Purple;
  aColors[03] := TFontRecolor.Blue;
  aColors[04] := TFontRecolor.Purple;
  aColors[05] := TFontRecolor.Green;
  aColors[06] := TFontRecolor.Purple;
  aColors[07] := TFontRecolor.Brown;
  aColors[08] := TFontRecolor.Purple;
  aColors[09] := TFontRecolor.Purple;
  aColors[10] := TFontRecolor.Purple;
  aColors[11] := TFontRecolor.Purple;
  aColors[12] := TFontRecolor.Purple;
  aColors[13] := TFontRecolor.Purple;
  aColors[14] := TFontRecolor.Purple;
  aColors[15] := TFontRecolor.Purple;

end;

procedure TGamePreviewScreen.ShowNextLevel(forwards: Boolean);
begin
  if not App.Config.MiscOptions.CheatScrollingInPreviewScreen then
    Exit;
  if forwards then begin
    App.CurrentLevelInfo := App.CurrentLevelInfo.Next;
    if App.CurrentLevelInfo = nil then
      App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
    BuildScreen;
  end
  else begin
    App.CurrentLevelInfo := App.CurrentLevelInfo.Prev;
    if App.CurrentLevelInfo = nil then
      App.CurrentLevelInfo := App.Style.LevelSystem.LastLevel;
    BuildScreen;
  end;
end;

procedure TGamePreviewScreen.ToggleSaveToEngine;
begin
  App.GlobalGame.SaveToEngineFileAtStartup := not App.GlobalGame.SaveToEngineFileAtStartup;
  if App.GlobalGame.SaveToEngineFileAtStartup then begin
    Winapi.Windows.Beep(880, 150);
    Speak(TVoiceOption.BinLevelSavingOn, False);
  end
  else begin
    Winapi.Windows.Beep(440, 150);
    Speak(TVoiceOption.BinLevelSavingOff, False)
  end;
end;

end.

