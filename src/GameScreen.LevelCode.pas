unit GameScreen.LevelCode;

{$include lem_directives.inc}

interface
// todo: check working of cheatcodes (setting off is not possibel)
uses
  Windows, Classes, Controls, Graphics, MMSystem, Forms, ClipBrd,
  GR32, GR32_Image, GR32_Layers,
  Base.Utils, Base.Types, Base.Strings,
  Dos.Structures,
  Styles.Base,
  Prog.Base, Prog.Data, Prog.Cache, Prog.App,
  GameScreen.Base;

type
  TGameScreenLevelCode = class(TGameBaseScreen)
  private
    const INTERNAL_BLINK = 240;
  private
    BlinkTimer       : TTicker;
    LevelCode        : string;
    CursorPosition   : Integer;
    ValidLevelCode   : Boolean;
    YPositions       : array[0..3] of Integer;
    XPos             : Integer;
    Blinking         : Boolean;
    Typing           : Boolean;
    LastMessage      : string;
    LastCheatMessage : string;
  // internal events
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_Close(Sender: TObject; var Action: TCloseAction );
    procedure Application_Idle(Sender: TObject; var Done: Boolean);
  // internal methods
    function FindLevelByCode(const aCode: string): TLevelLoadingInformation;
    function CheckLevelCode: Boolean;
    function CheckCheatCode: Boolean;
    procedure DrawChar(aCursorPos: Integer; aBlink: Boolean = False);
    procedure DrawMessage(const S: string);
    procedure UpdateCheatMessage;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildScreen; override;
  end;

implementation

uses
  System.SysUtils, Form.Base;

{ TGameScreenLevelCode }

constructor TGameScreenLevelCode.Create(aOwner: TComponent);
begin
  inherited;
  LevelCode := '..........';
  CursorPosition := 1;
  ScreenImg.Enabled := False;

  OnKeyDown := Form_KeyDown;
  OnKeyPress := Form_KeyPress;
  OnClose := Form_Close;

  XPos := (640 - (10 * 16)) div 2;

  YPositions[0] := 120;
  YPositions[1] := 152;
  YPositions[2] := 184;
  YPositions[3] := 216;

  InitializeImageSizeAndPosition(640, 380);
  ExtractBackGround;
  ExtractPurpleFont;

  BlinkTimer.Reset(9);
  BlinkTimer.Interval := INTERNAL_BLINK;
end;

destructor TGameScreenLevelCode.Destroy;
begin
  Application.OnIdle := nil;
  inherited;
end;

procedure TGameScreenLevelCode.BuildScreen;
var
  Mainpal: TArrayOfColor32;
begin
  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;
    TileBackgroundBitmap(0, 0);
    BackBuffer.Assign(ScreenImg.Bitmap); // save background

    DrawPurpleText(ScreenImg.Bitmap, gt.SLevelCodeScreen_EnterCode, XPos, 120);
    DrawPurpleText(ScreenImg.Bitmap, LevelCode, XPos, YPositions[1]);

    UpdateCheatMessage;

    Application.OnIdle := Application_Idle;
  finally
    ScreenImg.EndUpdate;
  end;
end;

procedure TGameScreenLevelCode.Application_Idle(Sender: TObject; var Done: Boolean);
var
  CurrTick: Int64;
begin
  if ScreenIsClosing then
    Exit;
  if Typing then
    Exit;
  Done := False;
  Sleep(1); // relax CPU

  // todo: optional non blinking: then drawcursor beneath levelcode (_)

  CurrTick := QueryTimer;
  if BlinkTimer.Check(CurrTick) then begin
    BlinkTimer.Reset(CurrTick);
    Blinking := not Blinking;
    DrawChar(CursorPosition, Blinking);
  end;
end;

function TGameScreenLevelCode.CheckCheatCode: Boolean;
var
  S: string;
begin
  S := StringReplace(LowerCase(LevelCode), '.', string.Empty, [rfReplaceAll]);
  Result := SameText(S, gt.SCheatCode);
end;

function TGameScreenLevelCode.CheckLevelCode: Boolean;
var
  s: string;
  Txt: string;
  info: TLevelLoadingInformation;
begin
  Result := False;
  s := stringreplace(LevelCode, '.', string.Empty, [rfReplaceAll]);

  info := FindLevelByCode(s);

  if not Assigned(info) and App.Config.MiscOptions.CheatCodesInLevelCodeScreen then
    info := App.Style.LevelSystem.FindLevelBySectionNameAndNumber(LevelCode);

  if Assigned(info) then begin
    App.CurrentLevelInfo := info;
    Txt := FormatSimple(gt.SLevelCodeScreen_CodeForSectionLevel_ss, [App.CurrentLevelInfo.Section.SectionName, (App.CurrentLevelInfo.LevelIndex + 1).ToString]);
    DrawMessage(Txt);
    Exit(True);
  end
  else
    DrawMessage(gt.SLevelCodeScreen_IncorrectCode);
end;

procedure TGameScreenLevelCode.DrawChar(aCursorPos: Integer; aBlink: Boolean);
var
  C: Char;
begin
  if aBlink then
    C := '_'
  else
    C := LevelCode[CursorPosition];
  DrawPurpleText(ScreenImg.Bitmap, C, XPos + CursorPosition * 16 - 16, YPositions[1], TFontRecolor.Purple, BackBuffer);
end;

procedure TGameScreenLevelCode.DrawMessage(const S: string);
begin
  if not LastMessage.IsEmpty then
    DrawPurpleTextCentered(ScreenImg.Bitmap, LastMessage, YPositions[2], BackBuffer, True);

  LastMessage := S;

  if S.IsEmpty then
    Exit;

  DrawPurpleTextCentered(ScreenImg.Bitmap, S, YPositions[2]);
end;

procedure TGameScreenLevelCode.UpdateCheatMessage;
begin
  {$ifdef paranoid} Assert(App <> nil); {$endif}

  if not LastCheatMessage.IsEmpty then
    DrawPurpleTextCentered(ScreenImg.Bitmap, LastCheatMessage, 350- 20, BackBuffer, True);

  LastCheatMessage := string.Empty;

  if not App.Config.MiscOptions.CheatCodesInLevelCodeScreen then
    Exit;

  LastCheatMessage := 'Cheatcodes Enabled!';

  DrawPurpleTextCentered(ScreenImg.Bitmap, LastCheatMessage, 350 - 20);

end;

function TGameScreenLevelCode.FindLevelByCode(const aCode: string): TLevelLoadingInformation;
// look in cache and find the level
begin
  Result := nil;
  var items: TArray<TStyleCache.TLevelCacheItem> := App.StyleCache.FindLevelsByCode(aCode);
  if Length(items) = 0 then
    Exit;
  for var item: TStyleCache.TLevelCacheItem in items do begin
    if item.StyleName = Consts.StyleName then begin
      Result := App.Style.LevelSystem.FindLevelByIndex(item.SectionIndex, item.LevelIndex);
      Exit;
    end;
  end;
end;

procedure TGameScreenLevelCode.Form_Close(Sender: TObject; var Action: TCloseAction);
begin
  Application.OnIdle := nil;
end;

procedure TGameScreenLevelCode.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ScreenIsClosing then
    Exit;
  if Shift = [] then
  begin
    case Key of
      VK_ESCAPE: CloseScreen(TGameScreenType.Menu);
      VK_RETURN:
        begin
          if CheckCheatCode then
          begin
            // toggle cheat enabled
            App.Config.MiscOptions.CheatCodesInLevelCodeScreen := True;
            //App.Config.MiscOptions.UseCheatScrollingInPreviewScreen := True;
            UpdateCheatMessage;
            Exit;
          end;

          if not ValidLevelCode then
          begin
            ValidLevelCode := CheckLevelCode;
            if ValidLevelCode then
            begin
              CloseDelay := 1000;
              DrawChar(CursorPosition, False);
              CloseScreen(TGameScreenType.Menu);
            end;
//            closedelay:=1000;
  //          CloseScreen(gstMenu);

          end
          else
            CloseScreen(TGameScreenType.Menu);
//          if ValidLevelCode then
  //          App.WhichLevel := wlLevelCode;
        end;
    end;
  end;
end;

procedure TGameScreenLevelCode.Form_KeyPress(Sender: TObject; var Key: Char);
var
  OldC, C: Char;
  OldPos: Integer;
begin
  if ScreenIsClosing then
    Exit;

  Typing := True;
  try

    C := UpCase(Char(Key));

    case C of
      ^V:
        begin
          var s := Clipboard.AsText;
          s := s.Trim;
          if s.Length = 10 then begin
            LevelCode := s;
            for var i := 1 to 10 do begin
              CursorPosition := i;
              DrawChar(i, False);
            end;
          end;
          //beep;
        end;
      'A'..'Z', '0'..'9':
        begin
          DrawMessage(string.Empty);
          OldC := LevelCode[CursorPosition];
          OldPos := CursorPosition;
          LevelCode[CursorPosition] := C;

          if CursorPosition < 10 then
          begin
            // maybe blinking: repair
            DrawChar(CursorPosition, False);
            // next pos
            Inc(CursorPosition);
          end;

          if (OldPos <> CursorPosition) or (OldC <> C) then
          begin
            DrawChar(CursorPosition);
          end;

          ValidLevelCode := False;
        end;
      Chr(8):
        begin
          DrawMessage(string.Empty);
          // GoBack := True;
          if CursorPosition > 1 then
          begin
            LevelCode[CursorPosition] := '.';
            // maybe blinking: repair
            DrawChar(CursorPosition, False);
            if CursorPosition > 1 then
              Dec(CursorPosition);
            ValidLevelCode := False;
          end;
        end;
      end; // case C

  finally
    Typing := False;
  end;

end;

end.

