unit GameScreen.Base;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Forms, System.SysUtils, Vcl.Dialogs,
  GR32, GR32_Image, GR32_Layers,
  Dos.Structures,
  Base.Utils, Base.Types, Base.Bitmaps,
  Form.Base,
  Prog.Base, Prog.App,
  Dos.MainDat;

const
  DEF_STRETCHED = TRUE;

const
  PURPLEFONTCOUNT = ord('~') - ord('!') + 1;
  PurpleFontCharSet = ['!'..'~'];

type
  TFontRecolor = ( // not used yet
    Purple,
    Red, // hue = 100
    Blue, // hue = -24
    Brown, // hue = 140
    Green, // hue = -138
    Pink     // 0.85 (correct)
  );

  TPurpleFont = class(TComponent)
  private
    function GetBitmapOfChar(Ch: Char): TBitmap32;
    class procedure Combine(F: TColor32; var B: TColor32; M: TColor32); static;
  protected
  public
    fBitmaps: array[0..PURPLEFONTCOUNT - 1] of TBitmap32;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property BitmapOfChar[Ch: Char]: TBitmap32 read GetBitmapOfChar;
    procedure SaveBitmaps(const aDir: string);
  end;

  // This is the ancestor for all DOS-forms that are used in the program.
  // Using the Prog.App unit indicates that the global App can be used.
  TGameBaseScreen = class(TAppForm)
  private
    fMainDatExtractor    : TMainDatExtractor;
    fScreenImg           : TImage32;
    fBackGround          : TBitmap32;
    fBackBuffer          : TBitmap32; // general purpose buffer
    fPurpleFont          : TPurpleFont;
    fOriginalImageBounds : TRect;
    fStretched           : Boolean;
    fCloseDelay          : Integer;
    procedure SetStretched(const Value: Boolean);
    procedure AdjustImage;
    procedure MakeList(const S: string; aList: TStrings);
  protected
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); override;
    property MainDatExtractor: TMainDatExtractor read fMainDatExtractor;
    property CloseDelay: Integer read fCloseDelay write fCloseDelay;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure TileBackgroundBitmap(X, Y: Integer; Dst: TBitmap32 = nil);
    procedure ExtractBackGround;
    procedure ExtractPurpleFont;
    procedure DrawPurpleText(Dst: TBitmap32; const S: string; X, Y: Integer; recolor: TFontRecolor = TFontRecolor.Purple; aRestoreBuffer: TBitmap32 = nil);
    procedure DrawPurpleTextCentered(Dst: TBitmap32; const S: string; Y: Integer; aRestoreBuffer: TBitmap32 = nil; EraseOnly: Boolean = False);
    function CalcPurpleTextSize(const S: string): TRect;
    procedure FadeOut;
    function InitializeImageSizeAndPosition(aWidth, aHeight: Integer): TRect;
    property ScreenImg: TImage32 read fScreenImg;
    property BackGround: TBitmap32 read fBackGround;
    property BackBuffer: TBitmap32 read fBackBuffer;
    property Stretched: Boolean read fStretched write SetStretched default DEF_STRETCHED;
    property PurpleFont: TPurpleFont read fPurpleFont;
  end;

implementation

{$ifdef debug}
uses
  Prog.Tools;
{$endif}

{ TPurpleFont }

class procedure TPurpleFont.Combine(F: TColor32; var B: TColor32; M: TColor32);
// just show transparent
begin
  if F <> 0 then B := F;
end;

constructor TPurpleFont.Create(aOwner: TComponent);
//  The purple font has it's own internal pixelcombine.
//  I don't think this ever has to be different.
var
  i: Integer;
begin
  inherited;
  for i := 0 to PURPLEFONTCOUNT - 1 do begin
    fBitmaps[i] := TBitmap32.Create;
    fBitmaps[i].OnPixelCombine := Combine;
    fBitmaps[i].DrawMode := dmCustom;
  end;
end;

destructor TPurpleFont.Destroy;
var
  i: Integer;
begin
  for i := 0 to PURPLEFONTCOUNT - 1 do
    fBitmaps[i].Free;
  inherited;
end;

function TPurpleFont.GetBitmapOfChar(Ch: Char): TBitmap32;
var
  Idx: Integer;
begin
  {$ifdef paranoid} Assert(CharInSet(Ch, ['!'..'~'])); {$endif}
  Idx := Ord(Ch) - ord('!');
  Result := fBitmaps[Idx];
end;

procedure TPurpleFont.SaveBitmaps(const aDir: string);
var
  i: Byte;
begin
  ForceDirectories(aDir);
  for i := 0 to PURPLEFONTCOUNT - 1 do
  begin
    fBitmaps[i].SaveToFile(aDir + 'purplefont' + LeadZeroStr(i, 2) + '.bmp');
  end;
end;

{ TGameBaseScreen }

procedure TGameBaseScreen.AdjustImage;
begin
  case fStretched of
    False:
      begin
        fScreenImg.Align := alNone;
        fScreenImg.ScaleMode := smNormal;
      end;
    True:
      begin
        fScreenImg.Align := alClient;
        fScreenImg.ScaleMode := smResize;
        fScreenImg.BitmapAlign := baCenter;
      end;
  end;
end;

procedure TGameBaseScreen.BeforeCloseScreen(aNextScreen: TGameScreenType);
begin
  if aNextScreen <> TGameScreenType.Interrupted then begin
    if fCloseDelay > 0 then begin
      Repaint;
      Sleep(fCloseDelay);
    end;
    if App.Config.FormOptions.FadeOut then
      FadeOut;
  end;
end;

constructor TGameBaseScreen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fScreenImg := TImage32.Create(Self);
  fScreenImg.Parent := Self;
  fScreenImg.Color := clBlack;
  fPurpleFont := TPurpleFont.Create(nil);
  fBackGround := TBitmap32.Create;
  fBackBuffer := TBitmap32.Create;
  fMainDatExtractor := TMainDatExtractor.Create;
  fStretched := DEF_STRETCHED;
  if not App.Config.FormOptions.ShowDefaultCursor then begin
    Cursor := crNone;
    ScreenImg.Cursor := crNone;
  end;
  fMainDatExtractor.FileName := App.Style.MainDatFileName;
end;

destructor TGameBaseScreen.Destroy;
begin
  fBackGround.Free;
  fMainDatExtractor.Free;
  fPurpleFont.Free;
  fBackBuffer.Free;
  inherited Destroy;
end;

function TGameBaseScreen.CalcPurpleTextSize(const S: string): TRect;
// Linefeeds increment 16 pixels
// Spaces increment 16 pixels
var
  C: Char;
  CX, i: Integer;
begin
  CX := 0;
  FillChar(Result, SizeOf(Result), 0);
  if not S.IsEmpty then
    Result.Bottom := 16;
  for i := 1 to Length(S) do
  begin
    C := S[i];
    case C of
      #13:
        begin
          Inc(Result.Bottom, 16);
          CX := 0;
        end;
      '!'..'~', ' ':
        begin
          Inc(CX, 16);
          if CX > Result.Right then
            Result.Right := CX;
        end;
    end;
  end;
end;

procedure TGameBaseScreen.DrawPurpleText(Dst: TBitmap32; const S: string; X, Y: Integer; recolor: TFontRecolor = TFontRecolor.Purple; aRestoreBuffer: TBitmap32 = nil);
//  Linefeeds increment 16 pixels
//  Spaces increment 16 pixels
// TODO: colorizing fonts with HSL?
var
  C: Char;
  CX, CY, i: Integer;
  R: TRect;
  bmp, tmp: TBitmap32;
begin


  if S.IsEmpty then
    Exit;

  if aRestoreBuffer <> nil then
  begin
    R := CalcPurpleTextSize(S);
    R.OffSet(X, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect);
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

  tmp := TBitmap32.Create;

  CX := X;
  CY := Y;
  for i := 1 to Length(S) do begin
    C := S[i];
    case C of
      #13:
        begin
          Inc(CY, 16);
          CX := X;
        end;
      ' ':
        begin
          Inc(CX, 16);
        end;
      '!'..'~':
        begin
          bmp := fPurpleFont.BitmapOfChar[C];
          tmp.Assign(bmp);
          tmp.DrawMode := bmp.DrawMode;
          tmp.OnPixelCombine := bmp.OnPixelCombine;
          tmp.DrawTo(Dst, CX, CY);
          Inc(CX, 16);
        end;
    end;
  end;

  tmp.Free;
end;

procedure TGameBaseScreen.DrawPurpleTextCentered(Dst: TBitmap32; const S: string; Y: Integer; aRestoreBuffer: TBitmap32 = nil; EraseOnly: Boolean = False);
// Linefeeds increment 16 pixels
// Spaces increment 16 pixels
var
  X, i: Integer;
  R: TRect;
  List: TStringList;
  H: string;
begin
  List := TStringList.Create;
  MakeList(S, List);

  if aRestoreBuffer <> nil then begin
    R := CalcPurpleTextSize(S);
    R.Offset((Dst.Width - (R.Right - R.Left)) div 2, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect); // oops, again watch out for sourceretangle!
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

  if not EraseOnly then
    for i := 0 to List.Count - 1 do begin
      H := List[i]; // <= 40 characters!!!
      X := (Dst.Width - 16 * Length(H)) div 2;
      if H <> #13 then
        DrawPurpleText(Dst, H, X, Y)
      else
        Inc(Y, 16);
    end;

  List.Free;
end;

procedure TGameBaseScreen.ExtractBackGround;
begin
  fMainDatExtractor.ExtractBrownBackGround(fBackGround);
end;

procedure TGameBaseScreen.ExtractPurpleFont;
var
  Pal: TArrayOfColor32;
  i: Integer;
  PurpleFontPos: Integer;
begin
  Pal := GetDosMainMenuPaletteColors32;
  if (Consts.StyleDef = TStyleDef.Ohno) or (Consts.StyleInfo.MaindatOhNo)
  then PurpleFontPos := $69B0 + 972 // there are 5 signs in ohno stored before the purple font (5 sections)
  else PurpleFontPos := $69B0;
  // read first
  MainDatExtractor.ExtractBitmap(fPurpleFont.fBitmaps[0], 4, PurpleFontPos, 16, 16, 3, Pal);
  // the position is updated automatically by stream reading (-1 parameter)
  for i := 1 to PURPLEFONTCOUNT - 1 do
    MainDatExtractor.ExtractBitmap(fPurpleFont.fBitmaps[i], 4, -1, 16, 16, 3, Pal);
end;

function TGameBaseScreen.InitializeImageSizeAndPosition(aWidth, aHeight: Integer): TRect;
begin
  fScreenImg.Bitmap.SetSize(aWidth, aHeight);

  Result.Left := (Screen.Width - aWidth) div 2;
  Result.Top := (Screen.Height - aHeight) div 2;
  Result.Right := Result.Left + aWidth;
  Result.Bottom := Result.Top + aHeight;

  fOriginalImageBounds := Result;
  fScreenImg.BoundsRect := fOriginalImageBounds;

  AdjustImage;
end;

procedure TGameBaseScreen.SetStretched(const Value: Boolean);
begin
  fStretched := Value;
  AdjustImage;
end;

procedure TGameBaseScreen.TileBackgroundBitmap(X, Y: Integer; Dst: TBitmap32 = nil);
var
  aX, aY: Integer;
begin
  {$ifdef paranoid}
  Assert(fBackground.Width > 0);
  Assert(fBackground.Height > 0);
 {$endif}

  if Dst = nil then
    Dst := fScreenImg.Bitmap;

  // #EL 2020 changed the <= to <
  aY := Y;
  aX := X;
  while aY <{=} Dst.Height do begin
    while aX <{=} Dst.Width do begin
      fBackground.DrawTo(Dst, aX, aY);
      Inc(aX, fBackground.Width);
    end;
    Inc(aY, fBackground.Height);
    aX := X;
  end;

end;


procedure TGameBaseScreen.MakeList(const S: string; aList: TStrings);
var
  StartP, P: PChar;
  NewS: string;
begin
  StartP := PChar(S);
  P := StartP;
  repeat
    case P^ of
    #13 :
      begin
        if P >= StartP then begin
          SetString(NewS, StartP, P - StartP);
          aList.Add(NewS);

          while P^ = #13 do begin
            aList.Add(#13);
            Inc(P);
          end;
          if P^ = #0 then
            Break;
          StartP := P;
        end;
      end;

    #0:
      begin
        if P >= StartP then begin
          SetString(NewS, StartP, P - StartP);
          aList.Add(NewS);
          break;
        end;
      end;

    end; // case

    Inc(P);
    if P = #0 then Break;
  until False;

end;

procedure TGameBaseScreen.FadeOut;
var
  A: Integer;
begin
  AlphaBlend := True;
  A := AlphaBlendValue;
  Sleep(100);
  repeat
    Repaint;
    Sleep(20);
    Dec(A, 8);
    if A < 0 then
      Break;
    AlphaBlendValue := A;
  until False;
end;

end.

