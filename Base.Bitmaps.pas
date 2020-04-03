unit Base.Bitmaps;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Types, System.Classes, System.SysUtils, System.Generics.Collections, System.Math,
  Vcl.Graphics, Vcl.Imaging.PngImage,
  GR32,
  Base.Utils;

type
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
    //function Lightness: Single; inline;
    function AsGray: TColor32; inline;
    //function Recolor(newColor: TColor32): TColor32;
  public
    property R: Byte read GetR write SetR;
    property G: Byte read GetG write SetG;
    property B: Byte read GetB write SetB;
    property A: Byte read GetA write SetA;
  end;

  TBitmap32Helper = class helper for TBitmap32
  public
    function GetPixelCount: Integer; inline;
    function ToPng: TPngImage;
    function ToWic: TWicImage;
    procedure FromPng(png: TPngImage);
    procedure SaveToPng(const aFileName: string);
  // some manipulations
    procedure ReplaceColor(FromColor, ToColor: TColor32);
    procedure ReplaceAllNonZeroColors(ToColor: TColor32);
    procedure ReplaceAlphaForAllNonZeroColors(alpha: Byte);
    procedure MakeGray;
    //procedure Recolor(newColor: TColor32);
    function CalcFrameRect(aFrameCount, aFrameIndex: Integer): TRect;
  end;

  // special class for irregular chars (sizes are not equal)
  TBitmapFont = class(TBitmap32)
  private
    fFrameList: TList<TRect>;
    fCharList: TList<Char>;
    fTempList: TBitmaps;
    fLock: LONGBOOL;
    procedure CreateCompleteBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FontBeginCreate;
    procedure FontEndCreate;
    procedure AddChar(C: Char; src: TBitmap32);
    function GetCharRect(C: Char): TRect;
    function DrawChar(C: Char; dst: TBitmap32; x, y: Integer): TSize;
    function TextSize(const s: string): TSize;
    function DrawText(const s: string; dst: TBitmap32; x, y: Integer): TSize;
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
  Result.R := avg;
  Result.G := avg;
  Result.B := avg;
  Result.A := Self.A;
end;

(*
function TColor32Helper.Recolor(newColor: TColor32): TColor32;
// r = 0.21   g = 0.72   b = 0.07
//var
//  avg: Byte;
//  h,s,l: byte;
//  h2,s2,l2: byte;
begin
  Result := 0;
//  Result := newColor;//Self;
//  Result.A := 0;
//  if Result = 0 then
//    Exit;
//  avg := Average;
//
////  var a: single := self.Lightness;
////  if a = 0 then exit;
////  var b: single := newcolor.Lightness;
//
//
//  RGBtoHSL(Self, H, S, L);
//  RGBtoHSL(Result, H2, S2, L2);
//  Result := HSLtoRGB(H2, s, l, 0);

//  Result.R := Round(newColor.R * b/a * 0.21);
//  Result.G := Round(newColor.G * b/a * 0.72);
//  Result.B := Round(newColor.B * b/a * 0.07);


  //  Result.R := (Integer(newColor.R) * 255) div avg;
//  Result.G := (Integer(newColor.G) * 255) div avg;
//  Result.G := (Integer(newColor.B) * 255) div avg;
//  myLightness := Lightness;
////  redLightness := 0.21 * R;
////  Result.R := Round(newColor.R * myLightness * (1/0.21));
////  Result.G := Round(newColor.G * myLightness * (1/0.72));
////  Result.B := Round(newColor.B * myLightness * (1/0.07));
//  Result.R := Round(newColor.R * myLightness * (1/0.21));
//  Result.G := Round(newColor.G * myLightness * (1/0.72));
//  Result.B := Round(newColor.B * myLightness * (1/0.07));
//  Result.A := A;
end;
*)

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

procedure TBitmap32Helper.MakeGray;
var
  P: PColor32;
  i: Integer;
begin
  if Width + Height = 0 then
    Exit;
  P := PixelPtr[0, 0];
  for i := 0 to Height * Width - 1 do begin
    P^ := P^.AsGray;
    Inc(P);
  end;
end;

(*
procedure TBitmap32Helper.Recolor(newColor: TColor32);
//var
//  P: PColor32;  // Result := (0.21 * R) + (0.72 * G) + (0.07 * B);
//   i: Integer;
//  avg: Byte;
begin
//  if Width + Height = 0 then
//    Exit;
//  P := PixelPtr[0, 0];
//  for i := 0 to Height * Width - 1 do begin
//    P^ := P^.Recolor(newColor);
//    Inc(P);
//  end;
end;
*)

function TBitmap32Helper.ToPng: TPngImage;
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  Result := TPngImage.Create;
  try
    b.Assign(Self);
    Result.Assign(b);
  finally
    b.Free;
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

procedure TBitmap32Helper.SaveToPng(const aFileName: string);
var
  png: TPngImage;
begin
  png := nil;
  try
    png := ToPng;
    png.SaveToFile(aFileName);
  finally
    png.Free;
  end;
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
  Assert(fTempList.Count = fFrameList.Count);
  Assert(fCharList.Count = fFrameList.Count);

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
  end;
end;

procedure TBitmapFont.AddChar(C: Char; src: TBitmap32);
var
  inf: TRect;
  tmp: TBitmap32;
begin
  if not fLock then
    Throw('Bitmapfont must be locked', 'AddChar');
  if fFrameList.Count = 0 then begin
    inf.Left := 0;
    inf.Top := 0;
    inf.Right := src.Width;
    inf.Bottom := src.Height;
  end
  else begin
    inf.Left := 0;
    inf.Top := fFrameList.Last.Bottom;
    inf.Right := src.Width;
    inf.Bottom := inf.Top + src.Height;
  end;
  fCharList.Add(C);
  fFrameList.Add(inf);
  tmp := TBitmap32.Create;
  tmp.Assign(src);
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

