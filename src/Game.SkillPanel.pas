unit Game.SkillPanel;

{$include lem_directives.inc}

interface
uses
  System.Types, Classes, Controls, SysUtils, System.Math,
  GR32, GR32_Image, GR32_Layers,
  Dos.Compression,
  Dos.Structures,
  Base.Utils, Base.Bitmaps,
  Prog.Strings, Prog.Base, Prog.Data,
  Dos.Bitmaps,
  Dos.Consts,
  Level.Base,
  Styles.Base;

  {-------------------------------------------------------------------------------
    maybe this must be handled by lemgame (just bitmap writing)

    // info positions types above the toolbar
    // 1. BUILDER(23)             1/14
    // 2. OUT 28                  15/23
    // 3. IN 99%                  24/31
    // 4. TIME 2-31               32/40
  -------------------------------------------------------------------------------}
type
  TMinimapClickEvent = procedure(Sender: TObject; const P: TPoint) of object;
  TSkillButtonsMouseDownEvent = procedure(aButton: TSkillPanelButton; isDoubleClick: Boolean) of object;
  TSkillButtonsMouseUpEvent = procedure of object;

type
  TSkillPanelToolbar = class(TCustomControl)
  private
    fGraph                   : TGraphicSet;
    fImg                     : TImage32;
    fOriginal                : TBitmap32;
    fSkillFont               : array['0'..'9', 0..1] of TBitmap32; // todo make one bitmap
    fInfoFont                : array[0..37] of TBitmap32; {%} {0..9} {A..Z} // todo make one bitmap
    fButtonRects             : array[TSkillPanelButton] of TRect;
    fRectColor               : TColor32;
    fOnMinimapClick          : TMinimapClickEvent; // event handler for minimap
    fOnSkillButtonsMouseDown : TSkillButtonsMouseDownEvent;
    fOnSkillButtonsMouseUp   : TSkillButtonsMouseUpEvent;
  // imageview events
    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  protected
    procedure ReadBitmapFromStyle(aStyle: TStyle);
    procedure SetButtonRects;
  public
    fLastDrawnStr: string;
    fNewDrawStr: string;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawNewStr;
    property Img: TImage32 read fImg;
  // paint methods (called directly from game and gamescreen)
    procedure DrawSkillCount(aButton: TSkillPanelButton; aNumber: Integer);
    procedure DrawButtonSelector(aButton: TSkillPanelButton; Highlight: Boolean);
    procedure DrawMinimap(Map: TBitmap32);
    procedure SetInfoAlternative(const info: string);
    procedure SetInfoCursorLemming(const Lem: string; Num: Integer);
    procedure SetInfoLemmingsOut(Num: Integer);
    procedure SetInfoLemmingsIn(Num, Max: Integer);
    procedure SetInfoMinutes(Num: Integer);
    procedure SetInfoSeconds(Num: Integer);
    procedure SetStyleAndGraph(aStyle: TStyle; aGraph: TGraphicSet; aScale: Integer);
    procedure RefreshInfo;
  // events (for gamescreen, who is responsible for game instructions)
    property OnMinimapClick: TMinimapClickEvent read fOnMinimapClick write fOnMinimapClick;
    property OnSkillButtonsMouseDown: TSkillButtonsMouseDownEvent read fOnSkillButtonsMouseDown write fOnSkillButtonsMouseDown;
    property OnSkillButtonsMouseUp: TSkillButtonsMouseUpEvent read fOnSkillButtonsMouseUp write fOnSkillButtonsMouseUp;
  end;

implementation

uses
  GameScreen.Player;

function PtInRectEx(const Rect: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top) and (P.Y < Rect.Bottom);
end;

{ TSkillPanelToolbar }

constructor TSkillPanelToolbar.Create(aOwner: TComponent);
var
  c: Char;
  i: Integer;
begin
  inherited;
  fImg := TImage32.Create(Self);
  fImg.Parent := Self;
  fImg.RepaintMode := rmOptimizer;

  fImg.OnMouseDown := ImgMouseDown;
  fImg.OnMouseMove := ImgMouseMove;
  fImg.OnMouseUp := ImgMouseUp;

  fRectColor := DosVgaColorToColor32(DosInLevelPalettes[Consts.ChristmasPalette][3]);

  fOriginal := TBitmap32.Create;

  for i := 0 to 37 do begin
    fInfoFont[i] := TBitmap32.Create;
  end;



  for c := '0' to '9' do
    for i := 0 to 1 do
      fSkillFont[c, i] := TBitmap32.Create;


  // info positions types:
  // strings (40 characters) = cursor + out + in + time = 1,15,24,32
  // 1. BUILDER(23)             1/14               0..13      14
  // 2. OUT 28                  15/23              14..22      9
  // 3. IN 99%                  24/31              23..30      8
  // 4. TIME 2-31               32/40              31..39      9

  fLastDrawnStr := StringOfChar(' ', 40);
  fNewDrawStr := StringOfChar(' ', 40);
  fNewDrawStr := SSkillPanelTemplate;

  {$if defined(paranoid)}
  if fNewDrawStr.Length <> 40 then
    Throw('Create length error in infostring');
  {$ifend}
end;

destructor TSkillPanelToolbar.Destroy;
var
  c: Char;
  i: Integer;
begin
  for i := 0 to 37 do
    fInfoFont[i].Free;

  for c := '0' to '9' do
    for i := 0 to 1 do
      fSkillFont[c, i].Free;

  fOriginal.Free;
  inherited;
end;

procedure TSkillPanelToolbar.DrawButtonSelector(aButton: TSkillPanelButton; Highlight: Boolean);
var
  R: TRect;
  C: TColor32;
  A: TRect;
begin
  if aButton = TSkillPanelButton.None then
    Exit;
  case Highlight of
    False:
      begin
        R := fButtonRects[aButton];
        Inc(R.Right);
        Inc(R.Bottom, 2);

        // top
        A := R;
        A.Bottom := A.Top + 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

        // left
        A := R;
        A.Right := A.Left + 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

        // right
        A := R;
        A.Left := A.Right - 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

        // bottom
        A := R;
        A.Top := A.Bottom - 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);
      end;
    True:
      begin
        R := fButtonRects[aButton];
        Inc(R.Right);
        Inc(R.Bottom, 2);
        C := fRectColor;
        fImg.Bitmap.FrameRectS(R, C);
      end;
  end;
end;

procedure TSkillPanelToolbar.DrawNewStr;
var
  O, N: char;
  i, x, y, idx: integer;
begin
  // info positions types:
  // 1. BUILDER(23)             1/14               0..13
  // 2. OUT 28                  15/23              14..22
  // 3. IN 99%                  24/31              23..30
  // 4. TIME 2-31               32/40              31..39

  y := 0;
  x := 0;

  for i := 1 to 40 do begin
    idx := -1;

    O := UpCase(fLastDrawnStr[i]);
    N := UpCase(fNewDrawStr[i]);

    // only draw changed letters
    if O <> N then begin
      // get index of bitmap
      case N of
        '%'      : idx := 0;
        '0'..'9' : idx := ord(n) - ord('0') + 1;
        '-'      : idx := 11;
        'A'..'Z' : idx := ord(n) - ord('A') + 12;
      end;

      if (idx >= 0) and (idx <= 37)
      then fInfoFont[idx].DrawTo(fimg.Bitmap, x, 0)
      else fimg.Bitmap.FillRectS(x, y, x + 8, y + 16, 0); // empty
    end;
    Inc(x, 8);
  end;
end;


procedure TSkillPanelToolbar.DrawSkillCount(aButton: TSkillPanelButton; aNumber: Integer);
// draw the number of skills left in the top of the buttons
var
  S: string;
  LeftDigit, RightDigit: Char;
  BtnIdx: Integer;
  DstRect, SrcRect: TRect;
  c: TColor32;
const
  FontYPos = 17;
begin
  // x = 3, 19, 35 etc. are the "black holes" for the numbers inside the image
  // y = 17

  Restrict(aNumber, 0, 99);

  S := LeadZeroStr(aNumber, 2);
  LeftDigit := S[1];
  RightDigit := S[2];

  BtnIdx := Ord(aButton) - 1; // "ignore" the 'None' button enumeration

  // white nothingness if number is zero
  if aNumber = 0 then begin
    DstRect := Rect(BtnIdx * 16 + 4, 17, BtnIdx * 16 + 4 + 8, 17 + 8);
    c := Color32(60 * 4, 52 * 4, 52 * 4);
    fImg.Bitmap.FillRect(DstRect.Left, DstRect.Top, DstRect.Right, DstRect.Bottom, c);
    Exit;
  end;

  // left
  DstRect := Rect(BtnIdx * 16 + 4, 17, BtnIdx * 16 + 4 + 4, 17 + 8);
  SrcRect := Rect(0, 0, 4, 8);
  fSkillFont[LeftDigit, 1].DrawTo(fImg.Bitmap, DstRect, SrcRect); // 1 is left

  // right
  DstRect.Offset(4, 0);
  SrcRect := Rect(4, 0, 8, 8);
  fSkillFont[RightDigit, 0].DrawTo(fImg.Bitmap, DstRect, SrcRect); // 0 is right
end;

procedure TSkillPanelToolbar.RefreshInfo;
// called by game or gamescreen after a frame is finished
begin
  DrawNewStr;
  fLastDrawnStr := fNewDrawStr;
end;

procedure TSkillPanelToolbar.ImgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
{-------------------------------------------------------------------------------
  Mouse behaviour of toolbar.
  • Minimap scrolling
  • button clicks
-------------------------------------------------------------------------------}
var
  P: TPoint;
  btn: TSkillPanelButton;
  BtnRect: PRect;
begin
  P := Img.ControlToBitmap(Point(X, Y));

  // check minimap scroll
  if DosMiniMapCorners.Contains(P) then begin
    Dec(P.X, DosMinimapCorners.Left);
    Dec(P.Y, DosMiniMapCorners.Top);
    P.X := P.X * 16;
    P.Y := P.Y * 8;
    if Assigned(fOnMiniMapClick) then
      fOnMinimapClick(Self, P);
    Exit;
  end;

  // button click
  for btn := Succ(Low(TSkillPanelButton)) to High(TSkillPanelButton) do // "ignore" spbNone
  begin
    BtnRect := @fButtonRects[btn];
    if PtInRectEx(BtnRect^, P) then begin
      if Assigned(fOnSkillButtonsMouseDown) then
        fOnSkillButtonsMouseDown(btn, ssDouble in Shift);
      Exit;
    end;
  end;

end;

procedure TSkillPanelToolbar.ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
begin
  if ssLeft in Shift then begin
    P := Img.ControlToBitmap(Point(X, Y));
    if PtInRectEx(DosMiniMapCorners, P) then begin
      Dec(P.X, DosMinimapCorners.Left);
      Dec(P.Y, DosMiniMapCorners.Top);
      P.X := P.X * 16;
      P.Y := P.Y * 8;
      if Assigned(fOnMiniMapClick) then
        fOnMinimapClick(Self, P);
    end;
  end
end;

procedure TSkillPanelToolbar.ImgMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
// ment for gamescreen
begin
  if Assigned(fOnSkillButtonsMouseUp) then
    fOnSkillButtonsMouseUp;
end;

procedure TSkillPanelToolbar.ReadBitmapFromStyle(aStyle: TStyle);
var
  c: char; i: Integer;
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  DataStream: TStream;
  Fn: string;
  LemmixPal: TArrayOfColor32;
  HiPal: TArrayOfColor32;
begin
  // try and concat palettes
  LemmixPal := DosPaletteToArrayOfColor32(DosInLevelPalettes[Consts.ChristmasPalette]);
  HiPal := fGraph.PaletteCustom;

  {TODO: how o how is the palette constructed ??}
  Assert(Length(HiPal) = 8, 'hipal error');
  SetLength(LemmixPal, 16);
  for i := 8 to 15 do
    LemmixPal[i] := HiPal[i - 8];
  LemmixPal[7] := LemmixPal[8];

  Fn := aStyle.MainDatFileName;


  SetButtonRects; // todo: move (just once needed)

  Sections := TDosDatSectionList.Create;
  Decompressor := TDosDatDecompressor.Create;
  try

    DataStream := TData.CreateDataStream(aStyle.Name, Fn, TDataType.LemmingData);
    try
      Decompressor.LoadSectionList(DataStream, Sections, False);
    finally
      DataStream.Free;
    end;

    // skillpanel
    Decompressor.DecompressSection(Sections[6].CompressedData, Sections[6].DecompressedData);
    Sections[6].DecompressedData.seek(0, sofrombeginning);
    TDosPlanarBitmap.LoadFromStream(Sections[6].DecompressedData, fOriginal, 0, 320, 40, 4, LemmixPal);

    fImg.Bitmap.Assign(fOriginal);

    //fOriginal.SaveToPng(Consts.PathToDebugFiles + 'skillpanel.png');

    // info fonts
    Sections[6].DecompressedData.Seek($1900, soFromBeginning);
    for i := 0 to 37 do begin
      TDosPlanarBitmap.LoadFromStream(Sections[6].DecompressedData, fInfofont[i], -1, 8, 16, 3, LemmixPal);
      //replacecolor(fInfoFont[i], Color32(0,176,0), clred32);
    end;

    //for i := 0 to 37 do
    //fInfoFont[i].SaveToPng(AppPath + 'bmpdebug\skillfont' + i.ToString.PadLeft(2, '0') + '.png');
    //skillfont RGB = 0,176,0

    // skill fonts

    { TODO : christmas lemmings, fix it }
    //  pal := DosInLevelPalette;// fGraph.palettestandard; {DosInLevelPalette;}
    //    pal[1] := pal[3]; // WHITE
    LemmixPal[1] := LemmixPal[3]; // WHITE
    Decompressor.DecompressSection(Sections[2].CompressedData, Sections[2].DecompressedData);
    Sections[2].decompresseddata.seek($1900, sofrombeginning);
    for c := '0' to '9' do
      for i := 0 to 1 do
        TDosPlanarBitmap.LoadFromStream(sections[2].DecompressedData, fSkillFont[c, i], -1, 8, 8, 1, LemmixPal);
  finally
    Decompressor.free;
    sections.free;
  end;
end;

procedure TSkillPanelToolbar.SetButtonRects;
var
  Org, R: TRect;
  iButton: TSkillPanelButton;

begin
//  Sca := 3;
  Org := Rect(1, 16, 15, 38); // exact position of first button
  R := Org;
  {R.Left := R.Left * Sca;
  R.Right := R.Right * Sca;
  R.Top := R.Top * Sca;
  R.Bottom := R.Bottom * Sca; }

  for iButton := Succ(Low(TSkillPanelButton)) to High(TSkillPanelButton) do
  begin
    fButtonRects[iButton] := R;
    R.Offset(16, 0);
  end;

end;

procedure TSkillPanelToolbar.SetInfoCursorLemming(const Lem: string; Num: Integer);
var
  S: string;
begin
  if Lem <> '' then begin
    S := (Lem + ' ' + IntToStr(Num)).PadRight(14);
    for var i := 1 to 14 do
      fNewDrawStr[i] := S[i];
  end
  else begin
    for var i := 1 to 14 do
      fNewDrawStr[i] := ' ';
  end;
end;

procedure TSkillPanelToolbar.SetInfoAlternative(const info: string);
var
  S: string;
begin
  S := info.PadRight(14);
  for var i := 1 to 14 do
    fNewDrawStr[i] := S[i];
end;

procedure TSkillPanelToolbar.SetInfoLemmingsOut(Num: Integer); // todo: rename
var
  S: string;
begin
  S := Num.ToString.PadRight(5);
  for var i := 1 to 5 do
    fNewDrawStr[18 + i] := S[i]
end;

procedure TSkillPanelToolbar.SetInfoLemmingsIn(Num, Max: Integer); // todo: rename
var
  S: string;
begin
  S := Percentage(Max, Num).ToString + '%';
  // todo: optional real number lemstrings
  for var i := 1 to 5 do
    fNewDrawStr[26 + i] := S[i];
end;

procedure TSkillPanelToolbar.SetInfoMinutes(Num: Integer);
var
  S: string;
begin
  S := Num.ToString.PadLeft(2);
  for var i := 1 to 2 do
    fNewDrawStr[35 + i] := S[i];
end;

procedure TSkillPanelToolbar.SetInfoSeconds(Num: Integer);
var
  S: string;
begin
  S := Num.ToString.PadLeft(2, '0');
  for var i := 1 to 2 do
    fNewDrawStr[38 + i] := S[i];
end;

procedure TSkillPanelToolbar.SetStyleAndGraph(aStyle: TStyle; aGraph: TGraphicSet; aScale: Integer);
begin
  fImg.BeginUpdate;
  fGraph := aGraph;
  ReadBitmapFromStyle(aStyle);
  fImg.Scale := aScale;
  fImg.ScaleMode := smScale;
  fImg.Height := fOriginal.Height * aScale;
  fImg.Width := fOriginal.Width * aScale;
  Width := fImg.Width;
  Height := fImg.Height;
  fImg.EndUpdate;
  fImg.Changed;
  Invalidate;
end;

procedure TSkillPanelToolbar.DrawMinimap(Map: TBitmap32);
// o wow: todo: keep it simple
var
  X: Integer;
begin
  Map.DrawTo(Img.Bitmap, 208, 18);
  if Parent <> nil then
  begin
    X := -Round(TGameScreenPlayer(Parent).ScreenImg.OffsetHorz/(16 * fImg.Scale));
    Img.Bitmap.FrameRectS(208 + X, 18, 208 + X + 20 + 5, 38, fRectColor);
  end;
end;


end.

