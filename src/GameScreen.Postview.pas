unit GameScreen.Postview;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.ClipBrd,
  Gr32, Gr32_Image, Gr32_Layers,
  Base.Utils, Base.Types, Base.Strings,
  Dos.Consts,
  Prog.Base, Prog.App, Prog.Data, Prog.Voice,
  Game,
  Styles.Base,
  Form.Message,
  GameScreen.Base, GameScreen.Help;

type
  // The dos postview screen, which shows you how you've played the game etc.
  TGamePostviewScreen = class(TGameBaseScreen)
  private
    fPlayedLevelInfo: TLevelLoadingInformation;
    fNextCode: string;
    fGameIsSaved: Boolean;
    function GetHelpText: THelpString;
    function GetScreenText: string;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseClick(Button: TMouseButton);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Forms;

{ TDosGamePreview }

procedure TGamePostviewScreen.BuildScreen;
var
  Temp: TBitmap32;
begin
  ScreenImg.BeginUpdate;
  Temp := TBitmap32.Create;
  try
    InitializeImageSizeAndPosition(640, 350);
    ExtractBackGround;
    ExtractPurpleFont;

    Temp.SetSize(640, 350);
    Temp.Clear(0);
    TileBackgroundBitmap(0, 0, Temp);
    DrawPurpleTextCentered(Temp, GetScreenText, 16);
    ScreenImg.Bitmap.Assign(Temp);
    if App.GameResult.Cheated then
      Speak(TVoiceOption.Cheater, True);
  finally
    ScreenImg.EndUpdate;
    Temp.Free;
  end;
end;

constructor TGamePostviewScreen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Stretched := True;
  OnKeyDown := Form_KeyDown;
  OnKeyPress := Form_KeyPress;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
end;

destructor TGamePostviewScreen.Destroy;
begin
  inherited Destroy;
end;

function TGamePostviewScreen.GetScreenText: string;
var
  STarget: string;
  SDone: string;
  H: string;

    procedure AddLine(const S: string);
    begin
      Result := Result + S + #13;
    end;

    procedure AddLineFeed(aCount: Integer);
    begin
      if aCount <= 0 then
        Exit;
      Result := Result + StringOfChar(#13, aCount);
    end;

    function GetResultText: string;
    var
      ix: Integer;
      r: TGameResultsRec;
    begin
      if App.GameResult.Cheated then
        Exit(gt.SPostviewScreen_YouCheater);

      ix := -1;
      r := App.GameResult; // shorten code
      // result text
      if r.Done = 100 then ix := 8 else
      if r.Done = 0 then ix := 0 else
      if r.Done < r.Target div 2 then ix := 1 else
      if r.Done < r.Target - 5 then ix := 2 else
      if r.Done < r.Target - 1 then ix := 3 else
      if r.Done = r.Target - 1 then ix := 4 else
      if r.Done = r.Target then ix := 5 else
      if r.Done < r.Target + 20 then ix := 6 else
      if r.Done >= r.Target + 20 then ix := 7;

      if (ix >= 0) and (ix < Length(gt.ResultStrings[Consts.StyleDef]))
      then Result := gt.ResultStrings[Consts.StyleDef][ix]
      else Result := gt.SPostviewScreen_UnknownGameResultString;
    end;

var
  NextInfo: TLevelLoadingInformation;
  NextAvail, Congrats: Boolean;
  r: TGameResultsRec;
begin
  {$ifdef paranoid} Assert(App.CurrentLevelInfo <> nil); {$endif}

  fPlayedLevelInfo := App.CurrentLevelInfo;

  Result := string.Empty;
  r := App.GameResult;
  NextInfo := App.CurrentLevelInfo.Next;
  NextAvail := NextInfo <> nil;
  Congrats := not NextAvail and r.Success;
  // next level earned
  if r.Success then
    App.CurrentLevelInfo := NextInfo;

  // all levels finished
  if Congrats then
    AddLine(gt.SCongrats[Consts.StyleDef])
  // default
  else begin
    // init some local strings

    // todo: align 2 lines when translated
    if App.Config.MiscOptions.LemmingsPercentages then begin
      STarget := (r.Target.ToString + '%');//.PadLeft(4);
      SDone := (r.Done.ToString + '%');//.PadLeft(4);
    end
    else begin
      STarget := (r.ToRescue.ToString);//.PadLeft(3);
      SDone := (r.Rescued.ToString);//.PadLeft(3);
    end;

    // top text
    if r.TimeIsUp
    then AddLine(gt.SPostviewScreen_YourTimeIsUp)
    else AddLine(gt.SPostviewScreen_AllLemmingsAccountedFor);

    AddLineFeed(1);
    AddLine(FormatSimple(gt.SPostviewScreen_YouRescued_s, [SDone]));
    AddLine(FormatSimple(gt.SPostviewScreen_YouNeeded_s, [STarget]));
    AddLineFeed(1);
    AddLine(GetResultText);
    AddLineFeed(6);

    if r.Success then begin
      H := NextInfo.GetLevelCode;
      fNextCode := H;
      AddLine(FormatSimple(gt.SPostviewScreen_YourAccessCode_ss, [(NextInfo.LevelIndex + 1).ToString, H]));
      AddLineFeed(3);
    end
    else
      AddLineFeed(5);
  end;

  // force bottomtext to a fixed position
  AddLineFeed(18 - Result.CountChar(CR));

  // check final screen
  if Congrats then
    AddLine(gt.SPostviewScreen_PressMouseToContinue)
  // default bottom text
  else begin
    if r.Success
    then AddLine(gt.SPostviewScreen_PressLeftMouseForNextLevel)
    else AddLine(gt.SPostviewScreen_PressLeftMouseToRetryLevel);
    AddLine(gt.SPostviewScreen_PressRightMouseForMenu);
  end;

end;

function TGamePostviewScreen.GetHelpText: THelpString;
begin
  Result.Add(VK_ESCAPE, gt.SHelpPostViewScreen_MenuScreen);
  Result.Add(VK_RETURN, gt.SHelpPostViewScreen_Previewcreen);
  Result.Add('r', gt.SHelpPostViewScreen_ReplayLastGame);
  Result.Add('u', gt.SHelpPostViewScreen_SaveReplay);
  Result.Add(Ord('C'), [ssCtrl], gt.SHelpPostViewScreen_CopyLevelCodeToClipBoard);
end;

procedure TGamePostviewScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: CloseScreen(TGameScreenType.Menu);
    VK_RETURN: CloseScreen(TGameScreenType.Preview);
  end;
end;

procedure TGamePostviewScreen.Form_KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'r':
       begin
         App.CurrentLevelInfo := fPlayedLevelInfo;
         App.ReplayCurrent := True;
         CloseScreen(TGameScreenType.Play);
       end;
    'u':
       begin
         if fGameIsSaved then
           Exit;
         fGameIsSaved := True;
         App.GlobalGame.Save(True);
         if App.Config.MiscOptions.MessageAfterSaveInResultScreen then
           DlgInfo(gt.SPostviewScreen_MessageGameIsSaved);
       end;
    '?':
       begin
         TGameScreenHelp.Execute(gt.SNamePostviewScreen, GetHelpText);
       end;
    ^C :
       begin
         if App.GlobalGame.GameResultRec.Success then
           ClipBoard.AsText := fNextCode;
       end;
  end;
end;

procedure TGamePostviewScreen.Form_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HandleMouseClick(Button);
end;

procedure TGamePostviewScreen.Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  HandleMouseClick(Button);
end;

procedure TGamePostviewScreen.HandleMouseClick(Button: TMouseButton);
begin
  if Button = mbLeft then
    CloseScreen(TGameScreenType.Preview)
  else if Button = mbRight then
    CloseScreen(TGameScreenType.Menu);
end;

end.
