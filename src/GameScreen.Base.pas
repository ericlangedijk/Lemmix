unit GameScreen.Base;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows,
  System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Forms, System.SysUtils, Vcl.Dialogs,
  GR32, GR32_Image, GR32_Layers,
  Dos.Structures,
  Base.Utils, Base.Bitmaps,
  Form.Base,
  Prog.Base, Prog.Types, Prog.App,
  Dos.MainDat;

const
  DEF_STRETCHED = TRUE;

const
  PURPLEFONTCOUNT = ord('~') - ord('!') + 1;
  PurpleFontCharSet = ['!'..'~'];

type
  TPurpleFont = class(TComponent)
  private
    function GetBitmapOfChar(Ch: Char): TBitmap32;
    procedure Combine(F: TColor32; var B: TColor32; M: TColor32);
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
  TGameBaseScreen = class(TBaseDosForm)
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
    procedure PrepareGameParams; override;
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); override;
    property MainDatExtractor: TMainDatExtractor read fMainDatExtractor;
    property CloseDelay: Integer read fCloseDelay write fCloseDelay;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure TileBackgroundBitmap(X, Y: Integer; Dst: TBitmap32 = nil);
    procedure ExtractBackGround;
    procedure ExtractPurpleFont;
    procedure DrawPurpleText(Dst: TBitmap32; const S: string; X, Y: Integer; aRestoreBuffer: TBitmap32 = nil);
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

{ TPurpleFont }

procedure TPurpleFont.Combine(F: TColor32; var B: TColor32; M: TColor32);
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
  Assert(CharInSet(Ch, ['!'..'~'])); // paranoid
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
    fBitmaps[i].SaveToFile(aDir + '' + 'purplefont' + LeadZeroStr(i, 2) + '.bmp');
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
      Update;
      Sleep(fCloseDelay);
    end;
    if App.Config.UseFadeOut then
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
  if not App.Config.ShowDefaultCursor then begin
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
  if S <> '' then
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

procedure TGameBaseScreen.DrawPurpleText(Dst: TBitmap32; const S: string; X, Y: Integer; aRestoreBuffer: TBitmap32 = nil);
//  Linefeeds increment 16 pixels
//  Spaces increment 16 pixels
var
  C: Char;
  CX, CY, i: Integer;
  R: TRect;
  bmp: TBitmap32;
begin

  if aRestoreBuffer <> nil then
  begin
    R := CalcPurpleTextSize(S);
    R.OffSet(X, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect); // oops, again watch out for sourceretangle!
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

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
          bmp.DrawTo(Dst, CX, CY);
          // todo: add optional recoloring
          Inc(CX, 16);
        end;
    end;
  end;

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
  if Consts.StyleDef = TStyleDef.Ohno
  then PurpleFontPos := $69B0 + 972 // there are 5 signs in ohno stored before the purple font (5 sections)
  else PurpleFontPos := $69B0;
  // read first
  MainDatExtractor.ExtractBitmap(fPurpleFont.fBitmaps[0], 4, PurpleFontPos, 16, 16, 3, Pal);
  // the position is now updated automatically by stream reading (-1 parameter)
  for i := 1 to PURPLEFONTCOUNT - 1 do
    MainDatExtractor.ExtractBitmap(fPurpleFont.fBitmaps[i], 4, -1, 16, 16, 3, Pal);

(*

  for i := 0 to Length(PurpleFont.fBitmaps) - 1 do begin
    var bmp: TBitmap32 := TBitmap32.Create;
    bmp.Assign(PurpleFont.BitmapOfChar['A']);
    bmp.Recolor(clRed32);
    var filename := Consts.PathToDebugFiles + 'Ared' + '.bmp';
    bmp.SaveToFile(filename);
    bmp.Assign(PurpleFont.BitmapOfChar['A']);
    bmp.Recolor(clMaroon32);
    filename := Consts.PathToDebugFiles + 'Amaroon' + '.bmp';
    bmp.SaveToFile(filename);
    bmp.Assign(PurpleFont.BitmapOfChar['A']);
    bmp.Recolor(clGreen32);
    filename := Consts.PathToDebugFiles + 'Agreen' + '.bmp';
    bmp.SaveToFile(filename);
    bmp.Assign(PurpleFont.BitmapOfChar['A']);
    bmp.Recolor(clLime32);
    filename := Consts.PathToDebugFiles + 'Alime' + '.bmp';
    bmp.SaveToFile(filename);
    bmp.Free;
  end;

*)

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

procedure TGameBaseScreen.PrepareGameParams;
begin
  inherited PrepareGameParams;
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

  Assert(fBackground.Width > 0);
  Assert(fBackground.Height > 0);

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
// todo: for the gamescreen this does not work. the skillpanel is not faded
var
  A: Integer;
begin
//  var bmp := TBitmap.Create;
//  bmp.SetSize(32, 32);
//  bmp.Canvas.Brush.Color := clBack;
//
//  while A >= 0 do begin
//    bmp.ScreenImg.Bitmap.ResetAlpha(Byte(A));
//    Sleep(20);
//    Dec(A, 8);
//  end;

  ScreenImg.Bitmap.DrawMode := dmBlend;
  ScreenImg.RepaintMode := rmDirect;
  // this is around 640 milliseconds - assuming a fast screenoutput which delays a little bit more we get around 750 ms
  A := 255;
  while A >= 0 do begin
    ScreenImg.Bitmap.ResetAlpha(Byte(A));
    Sleep(20);
    Dec(A, 8);
  end;
end;


end.

