unit GameScreen.Menu;

{$include lem_directives.inc}


interface

uses
  Winapi.Windows, Winapi.MMSystem,
  System.Types, System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms,
  GR32, GR32_Image, GR32_Layers,
  Base.Utils, Base.Bitmaps,
  Dos.Structures,
  Styles.Base,
  Dos.Consts, Prog.Data, Prog.Strings, Game.Rendering, Level.Base,
  Prog.Base, Prog.Types, Prog.App, GameScreen.Base, GameScreen.Preview, GameScreen.LevelCode;

type
  //  The main menu dos screen.
  TGameMenuScreen = class(TGameBaseScreen)
  private
  // internal types
    type
      //  these are the images we need for the menuscreen.
      TGameMenuBitmap = (
        Logo,
        Play,         // 1st row, 1st button
        LevelCode,    // 1st row, 2nd button
        Music,        // 1st row, 3d button
        Section,      // 1st row, 4th button
        Exit,         // 2nd row, 1st button
        Navigation,   // 2nd row, 2nd button
        MusicNote,    // drawn in gmbMusic
        FXSound,      // drawn in gmbMusic
        GameSection1, // mayhem/havoc    drawn in gmbSection
        GameSection2, // taxing/wicked   drawn in gmbSection
        GameSection3, // tricky/wild     drawn in gmbSection
        GameSection4, // fun/crazy       drawn in gmbSection
        GameSection5 // .../ohno tame, only last one
      );

    const
       // Positions at which the images of the menuscreen are drawn
      GameMenuBitmapPositions: array[TGameMenuBitmap] of TPoint = (
        (X:8;    Y:10),                   // Logo
        (X:72;   Y:120),                  // Play
        (X:200;  Y:120),                  // LevelCode
        (X:328;  Y:120),                  // Music
        (X:456;  Y:120),                  // Section
        (X:200;  Y:196),                  // Exit
        (X:328;  Y:196),                  // Navigation
        (X:328 + 27;    Y:120 + 26),      // MusicNote
        (X:328 + 27;    Y:120 + 26),      // FXSign,
        (X:456 + 32;    Y:120 + 24),      // Section1
        (X:456 + 32;    Y:120 + 24),      // Section2
        (X:456 + 32;    Y:120 + 24),      // Section3
        (X:456 + 32;    Y:120 + 24),      // Section4
        (X:456 + 32;    Y:120 + 24)       // Section5 (ohno only)
      );

      YPos_ProgramText = 272;
      YPos_Credits = 350 - 16;

      Reel_Width = 34 * 16;
      Reel_Height = 16;

      Font_Width = 16;

  private
  // enumerated menu bitmap elements
    BitmapElements : array[TGameMenuBitmap] of TBitmap32;
  // section
    CurrentSection : Integer; // game section
    LastSection    : Integer; // last game section
  // credits
    LeftLemmingAnimation   : TBitmap32;
    RightLemmingAnimation  : TBitmap32;
    Reel                   : TBitmap32;
    ReelBuffer             : TBitmap32;
    CanAnimate             : Boolean;
  // credits animation counters
    FrameTimeMS            : Cardinal;
    PrevTime               : Cardinal;
    ReadingPauseMS         : Cardinal;
    ReelLetterBoxCount     : Integer; // the number of letterboxes on the reel (34)
    Pausing                : Boolean;
    UserPausing            : Boolean;
    PausingDone            : Boolean; // the current text has been paused
    CreditList             : TStringList;
    CreditIndex            : Integer;
    CreditString           : string;
    TextX                  : Integer;
    TextPauseX             : Integer; // if -1 then no pause
    TextGoneX              : Integer;
    CurrentFrame           : Integer;
    ReelShift              : Integer;
  // internal
    procedure DrawBitmapElement(aElement: TGameMenuBitmap);
    procedure SetSoundOptions(aOptions: TSoundOptions);
    procedure PaintCurrentSection;
    procedure NextSection(Forwards: Boolean);
    procedure NextSoundSetting;
    procedure DrawWorkerLemmings(aFrame: Integer);
    procedure DrawReel;
    procedure SetNextCredit;
  // eventhandlers
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Application_Idle(Sender: TObject; var Done: Boolean);
  protected
  // overrides
    procedure PrepareGameParams; override;
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Form.Base;

{ TGameMenuScreen }

procedure TGameMenuScreen.DrawBitmapElement(aElement: TGameMenuBitmap);
//  Draw bitmap at appropriate place on the screen.
var
  P: TPoint;
begin
  P := GameMenuBitmapPositions[aElement];
  BitmapElements[aElement].DrawTo(ScreenImg.Bitmap, P.X, P.Y);
end;

constructor TGameMenuScreen.Create(aOwner: TComponent);
var
  E: TGameMenuBitmap;
  Bmp: TBitmap32;
begin
  inherited Create(aOwner);

//  stretched := false;
  CurrentSection := 0;

  // create bitmaps
  for E := Low(TGameMenuBitmap) to High(TGameMenuBitmap) do
  begin
    Bmp := TBitmap32.Create;
    BitmapElements[E] := Bmp;
    if not (E in [TGameMenuBitmap.MusicNote, TGameMenuBitmap.FXSound, TGameMenuBitmap.GameSection1, TGameMenuBitmap.GameSection2, TGameMenuBitmap.GameSection3,
      TGameMenuBitmap.GameSection4])
    then Bmp.DrawMode := dmTransparent;
  end;

  LeftLemmingAnimation := TBitmap32.Create;
  LeftLemmingAnimation.DrawMode := dmTransparent;

  RightLemmingAnimation := TBitmap32.Create;
  RightLemmingAnimation.DrawMode := dmTransparent;

  Reel := TBitmap32.Create;
  ReelBuffer := TBitmap32.Create;
  CreditList := TStringList.Create;

  FrameTimeMS := 32;
  ReadingPauseMS := 1000;
  CreditList.Text := SCredits;
  CreditIndex := -1;
  ReelLetterBoxCount := 34;
  SetNextCredit;

  // set eventhandlers
  OnKeyDown := Form_KeyDown;
  OnKeyUp := Form_KeyUp;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
  Application.OnIdle := Application_Idle;
end;

destructor TGameMenuScreen.Destroy;
var
  E: TGameMenuBitmap;
begin
  Application.OnIdle := nil;
  for E := Low(TGameMenuBitmap) to High(TGameMenuBitmap) do
    BitmapElements[E].Free;

  LeftLemmingAnimation.Free;
  RightLemmingAnimation.Free;
  Reel.Free;
  ReelBuffer.Free;
  CreditList.Free;

  inherited Destroy;
end;

procedure TGameMenuScreen.BuildScreen;
//  extract bitmaps from the lemmingsdata and draw
var
  Mainpal: TArrayOfColor32;
  Tmp: TBitmap32;
  i: Integer;
begin
  Tmp := TBitmap32.Create;
  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;
    InitializeImageSizeAndPosition(640, 350);
    ExtractBackGround;
    ExtractPurpleFont;

    // todo: move to dedicated maindatextractor?
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.Logo], 3, $2080, 632, 94, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.Play], 3, $9488, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.LevelCode], 3, $A2D4, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.Music], 3, $B120, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.Section], 3, $BF6C, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.Exit], 3, $CDB8, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.Navigation], 3, $DC04, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.MusicNote], 3, $EA50, 64, 31, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.FXSound], 3, $EE30, 64, 31, 4, MainPal);

    MainDatExtractor.ExtractAnimation(LeftLemmingAnimation, 4, $2A00, 48, 16, 16, 4, MainPal);
    MainDatExtractor.ExtractAnimation(RightLemmingAnimation, 4, $4200, 48, 16, 16, 4, MainPal);

    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.GameSection1], 4, $5A80, 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.GameSection2], 4, $5E4C, 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.GameSection3], 4, $6218, 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.GameSection4], 4, $65E4, 72, 27, 4, MainPal);

    // the only style with 5 sections
    if Consts.StyleDef = TStyleDef.Ohno then
      MainDatExtractor.ExtractBitmap(BitmapElements[TGameMenuBitmap.GameSection5], 4, $65E4 + 972, 72, 27, 4, MainPal);

    // reel
    MainDatExtractor.ExtractBitmap(Tmp, 4, $5A00, 16, 16, 4, MainPal);

    // a little oversize
    Reel.SetSize(ReelLetterBoxCount * 16 + 32, 16);
    for i := 0 to ReelLetterBoxCount - 1 + 4 do
      Tmp.DrawTo(Reel, i * 16, 0);

    // make sure the reelbuffer is the right size
    ReelBuffer.SetSize(ReelLetterBoxCount * 16, 16);

    // background
    TileBackgroundBitmap(0, 0);
    BackBuffer.Assign(ScreenImg.Bitmap); // save it

    // menu elements
    DrawBitmapElement(TGameMenuBitmap.Logo);
    DrawBitmapElement(TGameMenuBitmap.Play);
    DrawBitmapElement(TGameMenuBitmap.LevelCode);
    DrawBitmapElement(TGameMenuBitmap.Music);
    DrawBitmapElement(TGameMenuBitmap.Section);
    DrawBitmapElement(TGameMenuBitmap.Exit);
    DrawBitmapElement(TGameMenuBitmap.Navigation);

    // program text
    if Consts.StyleDef <> TStyleDef.User then
      DrawPurpleTextCentered(ScreenImg.Bitmap, SProgramTexts[Consts.StyleDef] + sLineBreak + sLineBreak + Consts.FullProgramName, YPos_ProgramText)
    else begin
      var descriptor: string := App.Style.StyleInformation.Description;
      if descriptor.Length > 24 then
        descriptor := App.Style.Name;
      DrawPurpleTextCentered(ScreenImg.Bitmap, Format(SProgramTexts[TStyleDef.User] + sLineBreak + sLineBreak + Consts.FullProgramName, [descriptor]), YPos_ProgramText);
    end;

    // credits animation
    DrawWorkerLemmings(0);
    DrawReel;//(0, TextX);

    // a bit weird place, but here we know the bitmaps are loaded
    PaintCurrentSection;
    SetSoundOptions(App.Config.SoundOptions);

    CanAnimate := True;
  finally
    ScreenImg.EndUpdate;
    Tmp.Free;
  end;
end;

procedure TGameMenuScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then begin
    case Key of
      VK_RETURN : CloseScreen(TGameScreenType.Preview);
      VK_F1     : if TMiscOption.UseFinder in App.Config.MiscOptions then
                    CloseScreen(TGameScreenType.Finder);
      VK_F2     : CloseScreen(TGameScreenType.LevelCode);
      VK_F3     : NextSoundSetting;
      VK_F4     : CloseScreen(TGameScreenType.Options);
      // VK_F5     : CloseScreen(TGameScreenType.Config);
      VK_ESCAPE : CloseScreen(TGameScreenType.ExitProgram);
      VK_UP     : NextSection(True);
      VK_DOWN   : NextSection(False);
      VK_SPACE  : UserPausing := not UserPausing;
    end;
  end;
end;

procedure TGameMenuScreen.Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F12) and (Shift = []) then
    CurrentDisplay.MainForm.SwitchToNextMonitor;
end;

procedure TGameMenuScreen.Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseScreen(TGameScreenType.Preview);
end;

procedure TGameMenuScreen.Img_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Button = mbLeft then
    CloseScreen(TGameScreenType.Preview);
end;

procedure TGameMenuScreen.NextSection(Forwards: Boolean);

    procedure Change;
    begin
      var sect: TSection := App.Style.LevelSystem.SectionList[CurrentSection];
      App.CurrentLevelInfo := sect.LevelLoadingInformationList.First;
    end;

begin
  case Forwards of
    False:
      if CurrentSection > 0 then begin
        Dec(CurrentSection);
        PaintCurrentSection;
        Change;
      end;
    True:
      if CurrentSection < LastSection then begin
        Inc(CurrentSection);
        PaintCurrentSection;
        Change;
      end;
  end;
end;

procedure TGameMenuScreen.NextSoundSetting;
var
  Opt: TSoundOptions;
begin
  Opt := App.Config.SoundOptions;
  if Opt = [] then Include(Opt, TSoundOption.Sound)
  else if Opt = [TSoundOption.Sound] then Include(Opt, TSoundOption.Music)
  else if Opt = [TSoundOption.Music, TSoundOption.Sound] then Opt := [];
  SetSoundOptions(Opt);
end;

procedure TGameMenuScreen.PrepareGameParams;
begin
  if App.CurrentLevelInfo = nil then
    App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
  CurrentSection := App.CurrentLevelInfo.Section.SectionIndex;
  LastSection := App.Style.LevelSystem.SectionList.Last.SectionIndex;
end;

procedure TGameMenuScreen.SetSoundOptions(aOptions: TSoundOptions);
var
  Opt: TSoundOptions;
begin
  App.Config.SoundOptions := aOptions;
  Opt := App.COnfig.SoundOptions;
  if Opt = [] then DrawBitmapElement(TGameMenuBitmap.Music)
  else if Opt = [TSoundOption.Sound] then DrawBitmapElement(TGameMenuBitmap.FXSound)
  else if Opt = [TSoundOption.Music, TSoundOption.Sound] then DrawBitmapElement(TGameMenuBitmap.MusicNote);
end;

procedure TGameMenuScreen.PaintCurrentSection;
begin
  if Consts.StyleDef = TStyleDef.Ohno then begin
    case CurrentSection of
      0: DrawBitmapElement(TGameMenuBitmap.GameSection5);
      1: DrawBitmapElement(TGameMenuBitmap.GameSection4);
      2: DrawBitmapElement(TGameMenuBitmap.GameSection3);
      3: DrawBitmapElement(TGameMenuBitmap.GameSection2);
      4: DrawBitmapElement(TGameMenuBitmap.GameSection1);
    end;
  end
  else begin
    case CurrentSection of
      0: DrawBitmapElement(TGameMenuBitmap.GameSection4);
      1: DrawBitmapElement(TGameMenuBitmap.GameSection3);
      2: DrawBitmapElement(TGameMenuBitmap.GameSection2);
      3: DrawBitmapElement(TGameMenuBitmap.GameSection1);
    end;
  end;
end;

procedure TGameMenuScreen.DrawWorkerLemmings(aFrame: Integer);
var
  SrcRect, DstRect: TRect;
begin
  SrcRect := LeftLemmingAnimation.CalcFrameRect(16, aFrame);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  DstRect.Offset(0, YPos_Credits);
  BackBuffer.DrawTo(ScreenImg.Bitmap, DstRect, DstRect);
  LeftLemmingAnimation.DrawTo(ScreenImg.Bitmap, DstRect, SrcRect);

  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  DstRect.Offset(640 - 48, YPos_Credits);
  BackBuffer.DrawTo(ScreenImg.Bitmap, DstRect, DstRect);
  RightLemmingAnimation.DrawTo(ScreenImg.Bitmap, DstRect, SrcRect);
end;

procedure TGameMenuScreen.SetNextCredit;
var
  TextSize: Integer;
begin
  TextX := 33 * 16;

  if CreditList.Count = 0 then begin
    CreditString := '';
    Exit;
  end;

  Inc(CreditIndex);
  if CreditIndex > CreditList.Count - 1 then
    CreditIndex := 0;

  // set new string
  CreditString := CreditList[CreditIndex];
  Pausing := False;
  PausingDone := False;
  TextSize := Length(CreditString) * Font_Width;
  TextPauseX := (Reel_Width - TextSize) div 2;
  TextGoneX := -TextSize;
end;

procedure TGameMenuScreen.DrawReel;
//  Drawing of the moving credits. aShift = the reel shift which is wrapped every
//  other 16 pixels to zero.
var
  R: TRect;
begin
  R := Reel.BoundsRect;
  R.Left := ReelShift;
  R.Right := R.Left + ReelLetterBoxCount * 16 ;
  Reel.DrawTo(ReelBuffer, ReelShift, 0);
  DrawPurpleText(ReelBuffer, CreditString, TextX, 0);
  ReelBuffer.DrawTo(ScreenImg.Bitmap, 48, YPos_Credits);
end;

procedure TGameMenuScreen.Application_Idle(Sender: TObject; var Done: Boolean);
//  Animation of credits.
//  •  34 characters fit into the reel.
//  •  text scolls from right to left. when one line is centered into the reel, scrolling is paused for a while.
var
  CurrTime: Cardinal;
begin
  if not CanAnimate or ScreenIsClosing then
    Exit;
  Sleep(1);
  Done := False;
  CurrTime := TimeGetTime;
  if UserPausing then
    Exit;

  // check end reading pause
  if Pausing then begin
    if CurrTime > PrevTime + ReadingPauseMS then begin
      PrevTime := CurrTime;
      Pausing := False;
      PausingDone := True; // we only pause once per text
    end;
    Exit;
  end;

  // update frames
  if CurrTime > PrevTime + FrameTimeMS then
  begin
    PrevTime := CurrTime;

    // workerlemmings animation has 16 frames
    Inc(CurrentFrame);
    if CurrentFrame >= 15 then
      CurrentFrame := 0;

    // text + reel
    Dec(ReelShift, 4);
    if ReelShift <= - 16 then
      ReelShift := 0;

    Dec(TextX, 4);
    if TextX < TextGoneX then
      SetNextCredit;

    if not PausingDone then begin
      // if text can be centered then pause if we are there
      if TextPauseX >= 0 then
        if TextX <= TextPauseX then
          Pausing := True;
    end;

    DrawWorkerLemmings(CurrentFrame);
    DrawReel;
  end;
end;

end.

