unit GameScreen.Postview;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.ClipBrd,
  Gr32, Gr32_Image, Gr32_Layers,
  Base.Utils,
  Dos.Consts,
  Prog.Base, Prog.Types, Prog.App, Prog.Data, Prog.Strings,
  Game,
  Styles.Base,
  Form.Message,
  GameScreen.Base;

type
  // The dos postview screen, which shows you how you've played the game etc.
  TGamePostviewScreen = class(TGameBaseScreen)
  private
    fPlayedLevelInfo: TLevelLoadingInformation;
    fNextCode: string;
    function GetScreenText: string;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseClick(Button: TMouseButton);
  protected
    procedure PrepareGameParams; override;
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Forms;

{ TDosGamePreview }

procedure TGamePostviewScreen.PrepareGameParams;
begin
  inherited;
end;

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
        Exit(SYouCheater);

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

      if (ix >= 0) and (ix < Length(ResultStrings[Consts.StyleDef]))
      then Result := ResultStrings[Consts.StyleDef][ix]
      else Result := SUnknownGameResultString;
    end;

var
  NextInfo: TLevelLoadingInformation;
  NextAvail, Congrats: Boolean;
  r: TGameResultsRec;

  //Sys: TLevelSystem;
begin
  Assert(App.CurrentLevelInfo <> nil);

  fPlayedLevelInfo := App.CurrentLevelInfo;

  Result := '';
  r := App.GameResult;
  NextInfo := App.CurrentLevelInfo.Next;
  NextAvail := NextInfo <> nil;
  Congrats := not NextAvail and r.Success;
  // next level earned
  if r.Success then
    App.CurrentLevelInfo := NextInfo;

  // all levels finished
  if Congrats then
    AddLine(SCongrats[Consts.StyleDef])
  // default
  else begin
    // init some local strings
    STarget := (r.Target.ToString + '%').PadLeft(4);
    SDone := (r.Done.ToString + '%').PadLeft(4);

    // top text
    if r.TimeIsUp
    then AddLine(SYourTimeIsUp)
    else AddLine(SAllLemmingsAccountedFor);

    AddLineFeed(1);
    AddLine(Format(SYouRescuedYouNeeded_ss, [SDone, STarget]));
    AddLineFeed(1);
    AddLine(GetResultText);
    AddLineFeed(6);

    if r.Success then begin
      H := NextInfo.GetLevelCode;
      fNextCode := H;
      AddLine(Format(SYourAccessCode_ds, [NextInfo.LevelIndex + 1, H]));
      AddLineFeed(3);
    end
    else
      AddLineFeed(5);
  end;

  // force bottomtext to a fixed position
  AddLineFeed(18 - Result.CountChar(#13));

  // check final screen
  if Congrats then
    AddLine(SPressMouseToContinue)
  // default bottom text
  else begin
    if r.Success
    then AddLine(SPressLeftMouseForNextLevel)
    else AddLine(SPressLeftMouseToRetryLevel);
    AddLine(SPressRightMouseForMenu);
  end;

end;

procedure TGamePostviewScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: CloseScreen(TGameScreenType.Menu);
    VK_RETURN: CloseScreen(TGameScreenType.Preview);
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

procedure TGamePostviewScreen.Form_KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    ^C :
       begin
         if App.GlobalGame.GameResultRec.Success then
           ClipBoard.AsText := fNextCode;
       end;
    'u':
       begin
         App.GlobalGame.Save(True);
         DlgInfo('Game saved (including game result in .txt file)');
       end;
    'r':
       begin
         App.CurrentLevelInfo := fPlayedLevelInfo;
         App.ReplayCurrent := True;
         CloseScreen(TGameScreenType.Play);
       end;
  end;
end;

end.
