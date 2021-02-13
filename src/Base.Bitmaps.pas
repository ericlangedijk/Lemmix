unit Base.Bitmaps;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Types, System.Classes, System.SysUtils, System.Generics.Collections, System.Math, System.Character,
  Vcl.Graphics, Vcl.Imaging.PngImage,
  GR32,
  Base.Utils;

type
  THSL = record
    H, S, L: Single;
  end;

  TColor32Helper = record helper for TColor32
  // speed was tested, it is as fast as working directly with TColor32Entry
  strict private
    function GetR: Byte; inline;
    function GetG: Byte; inline;
    function GetB: Byte; inline;
    function GetA: Byte; inline;
    procedure SetR(aValue: Byte); inline;
    procedure SetG(aValue: Byte); inline;
    procedure SetB(aValue: Byte); inline;
    procedure SetA(aValue: Byte); inline;
  public
    procedure Init(aR, aG, aB, aA: Byte); inline;
    function Average: Byte; inline;
    function AsGray: TColor32; inline;
    function AsGraySophisticated: TColor32; inline;
    function ToHSL: THSL; inline;
    procedure FromHSL(const HSL: THSL); inline;
  public
    property R: Byte read GetR write SetR;
    property G: Byte read GetG write SetG;
    property B: Byte read GetB write SetB;
    property A: Byte read GetA write SetA;
  end;

  TPngMode = (
    Opaque, // alpha = 255
    BlackIsTransparent, // alpha = 0 when RGB = 0,0,0 otherwise alpha = 255
    AsIs // 32 bits as is, including alpha
  );

  TBitmap32Helper = class helper for TBitmap32
  public
    function GetPixelCount: Integer; inline;
    function ToPng(mode: TPngMode): TPngImage;
    function ToWic: TWicImage;
    procedure FromPng(png: TPngImage);
    procedure SaveToPng(const aFileName: string; mode: TPngMode);
  // some manipulations
    procedure ReplaceColor(FromColor, ToColor: TColor32);
    procedure ReplaceAllNonZeroColors(ToColor: TColor32);
    procedure ReplaceAlphaForAllNonZeroColors(alpha: Byte);
    procedure ReplaceAlphaForNonZeroAndZeroColors;
    procedure MakeGray(sophisticated: Boolean);
    function CalcFrameRect(aFrameCount, aFrameIndex: Integer): TRect;
    function ToMaskText: string;
    function GetUpdateCount: Integer; inline;
  end;

  // special class for irregular chars (sizes are not equal)
  TBitmapFont = class(TBitmap32)
  private
    fFrameList: TList<TRect>;
    fCharList: TList<Char>;
    fTempList: TBitmaps;
    fMaxHeight: Integer;
    fAvgWidth: Integer;
    fAvgHeight: Integer;
    fLock: LONGBOOL;
    procedure CreateCompleteBitmap;
    class function GetFilledRect(bmp: TBitmap32): TRect;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FontBeginCreate;
    procedure FontEndCreate;
    procedure AddChar(C: Char; src: TBitmap32);
    function GetCharRect(C: Char): TRect;
    function GetCharSize(C: Char): TSize;
    function DrawChar(C: Char; dst: TBitmap32; x, y: Integer): TSize; overload;
    function DrawChar(C: Char; dst: TCanvas; x, y: Integer; aScale: Single): TSize; overload;
    function TextSize(const s: string): TSize;
    function DrawText(const s: string; dst: TBitmap32; x, y: Integer): TSize;
    property AvgHeight: Integer read fAvgHeight;
    property AvgWidth: Integer read fAvgWidth;
    property MaxHeight: Integer read fMaxHeight;
  end;

procedure MakeImageGrayscale(Image: TPngImage; Amount: Byte = 255);

implementation

{ TColor32Helper }

function TColor32Helper.GetR: Byte;
begin
  Result := TColor32Entry(Self).R;
end;

function TColor32Helper.GetG: Byte;
begin
  Result := TColor32Entry(Self).G;
end;

function TColor32Helper.GetB: Byte;
begin
  Result := TColor32Entry(Self).B;
end;

function TColor32Helper.GetA: Byte;
begin
  Result := TColor32Entry(Self).A;
end;

procedure TColor32Helper.SetR(aValue: Byte);
begin
  TColor32Entry(Self).R := aValue;
end;

procedure TColor32Helper.SetG(aValue: Byte);
begin
  TColor32Entry(Self).G := aValue;
end;

procedure TColor32Helper.SetB(aValue: Byte);
begin
  TColor32Entry(Self).B := aValue;
end;

procedure TColor32Helper.SetA(aValue: Byte);
begin
  TColor32Entry(Self).A := aValue;
end;

procedure TColor32Helper.Init(aR, aG, aB, aA: Byte);
begin
  R := aR;
  G := aG;
  B := aB;
  A := aA;
end;

(*
function TColor32Helper.Lightness: Single;
// 0.0 = black
// 1.0 = white
begin
  Result := (0.21 * R)/255 + (0.72 * G)/255 + (0.07 * B)/255;
end;
*)

function TColor32Helper.Average: Byte;
begin
  Result := Byte((Integer(R) + Integer(G) + Integer(B)) div 3);
end;

function TColor32Helper.AsGray: TColor32;
var
  avg: Byte;
begin
  avg := Average;
  Result.Init(avg, avg, avg, A);
end;

function TColor32Helper.AsGraySophisticated: TColor32;
// using formula Gray = Green * 0.59 + Blue * 0.30 + Red * 0.11;
var
  avg: Integer;
begin
  avg := Min(Round(R * 0.11) + Round(G * 0.58) + Round(B * 0.30), 255);
  Result.Init(avg, avg, avg, A);
end;

function TColor32Helper.ToHSL: THSL;
// get HSL from this color
begin
  RGBtoHSL(Self, Result.H, Result.S, Result.L);
end;

procedure TColor32Helper.FromHSL(const HSL: THSL);
// change color from HSL
begin
  Self := HSLtoRGB(HSL.H, HSL.S, HSL.L);
end;

{ TBitmap32Helper }

function TBitmap32Helper.GetPixelCount: Integer;
begin
  Result := Width * Height;
end;

procedure TBitmap32Helper.ReplaceAllNonZeroColors(ToColor: TColor32);
var
  P: PColor32;
  i: Integer;
begin
  if Width + Height = 0 then
    Exit;
  P := PixelPtr[0, 0];
  for i := 0 to Height * Width - 1 do begin
    if P^ <> 0 then
      P^ := ToColor;
    Inc(P);
  end;
end;

procedure TBitmap32Helper.ReplaceAlphaForAllNonZeroColors(alpha: Byte);
var
  P: PColor32;
  i: Integer;
begin
  if Width + Height = 0 then
    Exit;
  P := PixelPtr[0, 0];
  for i := 0 to Height * Width - 1 do begin
    if P^ <> 0 then
      P^.A := alpha;
    Inc(P);
  end;
end;

procedure TBitmap32Helper.ReplaceAlphaForNonZeroAndZeroColors;
// color = opaque, black = transparent
var
  P: PColor32;
  i: Integer;
begin
  if Width + Height = 0 then
    Exit;
  P := PixelPtr[0, 0];
  for i := 0 to Height * Width - 1 do begin
    if P^ and $FFFFFF00 = 0 then
      P^.A := 0
    else
      P^.A := 255;
    Inc(P);
  end;
end;

function TBitmap32Helper.CalcFrameRect(aFrameCount, aFrameIndex: Integer): TRect;
var
  Y, H: Integer;
begin
  H := Height div aFrameCount;
  Y := H * aFrameIndex;
  Result := Rect(0, Y, Width, Y + H);
end;

procedure TBitmap32Helper.ReplaceColor(FromColor, ToColor: TColor32);
var
  P: PColor32;
  i: Integer;
begin
  if Width + Height = 0 then
    Exit;
  P := PixelPtr[0, 0];
  for i := 0 to Height * Width - 1 do begin
    if P^ = FromColor then
      P^ := ToColor;
    Inc(P);
  end;
end;

procedure TBitmap32Helper.MakeGray(sophisticated: Boolean);
var
  P: PColor32;
  i: Integer;
begin
  if Width + Height = 0 then
    Exit;
  P := PixelPtr[0, 0];
  if not sophisticated then begin
    for i := 0 to Height * Width - 1 do begin
      P^ := P^.AsGray;
      Inc(P);
    end
  end
  else begin
    for i := 0 to Height * Width - 1 do begin
      P^ := P^.AsGraySophisticated;
      Inc(P);
    end
  end;
end;

function TBitmap32Helper.ToPng(mode: TPngMode): TPngImage;
type
  TRGB = packed record
    B,G,R: Byte;
  end;
  PRGB = ^TRGB;

var
  line: Pointer;
  alphaline: Pointer;
  rgbptr: PRGB;
  alphaptr: PByte;
  pix: PColor32;
begin
  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, Width, Height);

  if Width + Height <= 0 then
    Exit;

  pix := PixelPtr[0, 0];
  for var y: Integer := 0 to Height - 1 do begin
    line := Result.Scanline[y];
    rgbptr := line;
    alphaline := Result.AlphaScanline[y];
    alphaptr := alphaline;

    for var x := 0 to Width - 1 do begin
      rgbptr^.R := TColor32Entry(pix^).R;
      rgbptr^.G := TColor32Entry(pix^).G;
      rgbptr^.B := TColor32Entry(pix^).B;

      case mode of
        TPngMode.Opaque: alphaptr^ := 255;
        TPngMode.BlackIsTransparent: if pix^ and $FFFFFF00 = 0 then alphaptr^ := 0 else alphaptr^ := 255;
        TPngMode.AsIs: alphaptr^ := TColor32Entry(pix^).A;
      end;

      Inc(rgbptr);
      Inc(alphaptr);
      inc(pix);
    end;
  end;
end;

function TBitmap32Helper.ToWic: TWicImage;
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  Result := TWICImage.Create;
  try
    b.Assign(Self);
    Result.Assign(b);
  finally
    b.Free;
  end;
end;

procedure TBitmap32Helper.FromPng(png: TPngImage);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    b.Assign(png);
    Self.Assign(b);
  finally
    b.Free;
  end;
end;

procedure TBitmap32Helper.SaveToPng(const aFileName: string; mode: TPngMode);
var
  png: TPngImage;
begin
  png := nil;
  try
    png := ToPng(mode);
    png.SaveToFile(aFileName);
  finally
    png.Free;
  end;
end;

function TBitmap32Helper.ToMaskText: string;
// a little mask routine for debugging purposes
var
  mask: Cardinal;
  comment: string;
begin
  Result := string.Empty;

  for var y := 0 to Height - 1 do begin
    Mask := 0;
    comment := ' // ';
    for var x := 0 to Width - 1 do begin

      if PixelS[x,y] <> 0 then
        mask := mask or (1 shl x);

      if PixelS[x,y] = 0 then
        comment := comment + '0'
      else
        comment := comment + '1';
    end;
    Result := Result + '$' + IntToHex(mask) + ',' + comment;
    Result := Result + CRLF;
  end;
end;

function TBitmap32Helper.GetUpdateCount: Integer;
begin
  Result := UpdateCount;
end;

{ TBitmapFont }

constructor TBitmapFont.Create;
begin
  inherited Create;
  fFrameList := TList<TRect>.Create;
  fCharList := TList<Char>.Create;
end;

destructor TBitmapFont.Destroy;
begin
  fFrameList.Free;
  fCharList.Free;
  if Assigned(fTempList) then
    fTempList.Free;
  inherited;
end;

class function TBitmapFont.GetFilledRect(bmp: TBitmap32): TRect;
var
  c: TColor32;
  x, y: Integer;
  lineempty: Boolean;
  xLeft, xRight, yTop, yBottom: Integer;
begin
  if (bmp.Width = 0) or (bmp.Height = 0) then
    Exit(TRect.Empty);
  c := Bmp.PixelS[0, bmp.Height - 1];

  xLeft := 0;
  yTop := 0;
  xRight := bmp.Width - 1;
  yBottom := bmp.Height - 1;

  // top
  for y := 0 to bmp.Height - 1 do begin
    yTop := y;
    lineEmpty := True;
    for x := 0 to bmp.Width - 1 do begin
      if bmp.Pixel[x, y] <> c then begin
        lineEmpty := False;
        Break;
      end;
    end;
    if not lineEmpty then
      Break;
  end;

  // bottom
  for y := bmp.Height - 1 downto 0 do begin
    yBottom := y;
    lineEmpty := True;
    for x := 0 to bmp.Width - 1 do begin
      if bmp.Pixel[x, y] <> c then begin
        lineEmpty := False;
        Break;
      end;
    end;
    if not lineEmpty then
      Break;
  end;

  // left
  for x := 0 to bmp.Width - 1 do begin
    xLeft := x;
    lineEmpty := True;
    for y := 0 to bmp.Height - 1 do begin
      if bmp.Pixel[x, y] <> c then begin
        lineEmpty := False;
        Break;
      end;
    end;
    if not lineEmpty then
      Break;
  end;

  // right
  for x := bmp.Width - 1 downto 0 do begin
    xRight := x;
    lineEmpty := True;
    for y := 0 to bmp.Height - 1 do begin
      if bmp.Pixel[x, y] <> c then begin
        lineEmpty := False;
        Break;
      end;
    end;
    if not lineEmpty then
      Break;
  end;

  Result := Rect(xLeft, yTop, xRight + 1, yBottom + 1);
end;

procedure TBitmapFont.FontBeginCreate;
begin
  if not fLock then
    fTempList := TBitmaps.Create;
  Inc(fLock);
end;

procedure TBitmapFont.FontEndCreate;
begin
  if fLock then
    Dec(fLock);
  if not fLock then begin
    CreateCompleteBitmap;
    FreeAndNil(fTempList);
  end;
end;

procedure TBitmapFont.CreateCompleteBitmap;
var
  x, y: Integer;
begin
  {$ifdef paranoid}
  Assert(fTempList.Count = fFrameList.Count);
  Assert(fCharList.Count = fFrameList.Count);
  {$endif}

  fAvgWidth := 0;
  fAvgHeight := 0;
  fMaxHeight := 0;

  var neededWidth: Integer := 0;
  for var r: TRect in fFrameList do begin
    neededWidth := Max(neededWidth, r.Width);
  end;
  var neededHeight: Integer := fFrameList[fFrameList.Count - 1].Bottom;
  SetSize(neededWidth, neededHeight);
  Clear(0);
  for var ix := 0 to fCharList.Count - 1 do begin
    x := fFrameList[ix].Left;
    y := fFrameList[ix].Top;
    fTempList[ix].DrawTo(Self, x, y);
    fMaxHeight := Max(fMaxHeight, fFrameList[ix].Height);
    Inc(fAvgWidth, fFrameList[ix].Width);
    Inc(fAvgHeight, fFrameList[ix].Height);
  end;

  fAvgWidth := Round(fAvgWidth / fCharList.Count);
  fAvgHeight := Round(fAvgHeight / fCharList.Count);
end;

procedure TBitmapFont.AddChar(C: Char; src: TBitmap32);
var
  inf: TRect;
  tmp: TBitmap32;
  r: TRect;
begin
  if not fLock then
    Throw('Bitmapfont must be locked', 'AddChar');

  r := GetFilledRect(src);

//  if fFrameList.Count = 0 then begin
//    inf.Left := 0;
//    inf.Top := 0;
//    inf.Right := src.Width;
//    inf.Bottom := src.Height;
//  end
//  else begin
//    inf.Left := 0;
//    inf.Top := fFrameList.Last.Bottom;
//    inf.Right := src.Width;
//    inf.Bottom := inf.Top + src.Height;
//  end;

  if fFrameList.Count = 0 then begin
    inf.Left := 0;
    inf.Top := 0;
    inf.Right := r.Width;
    inf.Bottom := r.Height;
  end
  else begin
    inf.Left := 0;
    inf.Top := fFrameList.Last.Bottom;
    inf.Right := r.Width;
    inf.Bottom := inf.Top + r.Height;
  end;

  fCharList.Add(C);
  fFrameList.Add(inf);
  tmp := TBitmap32.Create;

  tmp.SetSize(r.Width, r.Height);
  src.DrawTo(tmp, 0, 0, r);

//  tmp.Assign(src);


  fTempList.Add(tmp);
end;

function TBitmapFont.GetCharRect(C: Char): TRect;
begin
  var ix: Integer := 0;
  for var ch: Char in fCharList do begin
    if ch = C then
      Exit(fFrameList[ix]);
    inc(ix);
  end;
  Result := TRect.Empty;
end;

function TBitmapFont.GetCharSize(C: Char): TSize;
begin
  var r: TRect := GetCharRect(c);
  Result.Create(r.Width, r.Height);
end;

function TBitmapFont.DrawChar(C: Char; dst: TBitmap32; x, y: Integer): TSize;
begin
  var R: TRect := GetCharRect(C);
  if R.IsEmpty then begin
    Result.cx := 0;
    Result.cy := 0;
  end
  else begin
    Self.DrawTo(dst, x, y);
    Result.cx := R.Width;
    Result.cy := R.Height;
  end;
end;

function TBitmapFont.DrawChar(C: Char; dst: TCanvas; x, y: Integer; aScale: Single): TSize;
var
  dstRect: TRect;
begin
  var R: TRect := GetCharRect(C);
  if R.IsEmpty then begin
    Result.cx := 0;
    Result.cy := 0;
  end
  else begin
    dstRect.Create(x, y, x + R.Width, y + R.Height);

//    dstRect.Offset(0, MaxHeight - R.Height); // todo: check

    if aScale <> 1.0 then begin
      dstRect.Width := Round(dstRect.Width * aScale);
      dstRect.Height := Round(dstRect.Height * aScale);
    end;
    Self.DrawTo(dst.Handle, dstRect, R);
    Result.cx := dstRect.Width;
    Result.cy := dstRect.Height;
  end;
end;

function TBitmapFont.TextSize(const s: string): TSize;
var
  C: Char;
  R: TRect;
begin
  Result.cx := 0;
  Result.cy := 0;
  for C in s do begin
    R := GetCharRect(C);
    Inc(Result.cx, R.Width);
    Result.cy := Max(Result.cy, R.Height);
  end;
end;

function TBitmapFont.DrawText(const s: string; dst: TBitmap32; x, y: Integer): TSize;
var
  C: Char;
  SrcRect, DstRect: TRect;
begin
  DstRect := Rect(X, Y, X, Y);
  for C in s do begin
    SrcRect := GetCharRect(C);

    DstRect.Width := SrcRect.Width;
    DstRect.Height := SrcRect.Height;

    Self.DrawTo(Dst, DstRect, SrcRect);
    Inc(DstRect.Left, SrcRect.Width);
  end;
end;

procedure MakeImageGrayscale(Image: TPngImage; Amount: Byte = 255);
// not used in lemmix

  procedure GrayscaleRGB(var R, G, B: Byte);
  var
    X: Byte;
  begin
    X := (R * 77 + G * 150 + B * 29) shr 8;
    R := ((R * (255 - Amount)) + (X * Amount) + 128) shr 8;
    G := ((G * (255 - Amount)) + (X * Amount) + 128) shr 8;
    B := ((B * (255 - Amount)) + (X * Amount) + 128) shr 8;
  end;

var
  X, Y, PalCount: Integer;
  Line: PRGBLine;
  PaletteHandle: HPalette;
  Palette: array[Byte] of TPaletteEntry;
begin
  //Don't do anything if the image is already a grayscaled one
  if not (Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_GRAYSCALEALPHA]) then begin
    if Image.Header.ColorType = COLOR_PALETTE then begin
      //Grayscale every palette entry
      PaletteHandle := Image.Palette;
      PalCount := GetPaletteEntries(PaletteHandle, 0, 256, Palette);
      for X := 0 to PalCount - 1 do
        GrayscaleRGB(Palette[X].peRed, Palette[X].peGreen, Palette[X].peBlue);
      SetPaletteEntries(PaletteHandle, 0, PalCount, Palette);
      Image.Palette := PaletteHandle;
    end
    else begin
      //Grayscale every pixel
      for Y := 0 to Image.Height - 1 do begin
        Line := Image.Scanline[Y];
        for X := 0 to Image.Width - 1 do
          GrayscaleRGB(Line[X].rgbtRed, Line[X].rgbtGreen, Line[X].rgbtBlue);
      end;
    end;
  end;
end;


end.

