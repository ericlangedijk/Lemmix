unit Form.Base;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Character, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
  Gr32,
  Base.Utils, Base.Bitmaps, Base.Types,
  Prog.Data, Prog.App;

type
  // abstract runtime black, fullscreen, ancestor form
  TBaseForm = class(TForm)
  public
    constructor Create(aOwner: TComponent); override;
    procedure AfterConstruction; override;
  end;

  // a lemmings form with some virtuals
  TAppForm = class(TBaseForm, IForm)
  private
    fScreenIsClosing: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override; final;
  protected
    procedure Input(param: TObject); virtual;
    procedure BuildScreen; virtual;
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    procedure CloseScreen(aNextScreen: TGameScreenType);
    function ShowScreen(param: TObject = nil): TGameScreenType;
    property ScreenIsClosing: Boolean read fScreenIsClosing;
  end;

  // non-maximized
  TToolForm = class(TAppForm)
  strict protected
    class var BmpFont: TBitmapFont;
    const FORM_COLOR = 1639942; // = GetShadowColor(clWebMidnightBlue, -50);
    const BORDER_COLOR = clGray;
    class procedure EnsureBmpFont;
    class function GetRedFontTextSize(const aText: string; multiLine, newLineOnSpace: Boolean): TSize; // todo: move drawing to font?
    class function DrawBitmapText(aCanvas: TCanvas; const aText: string; x, y: Integer; multiLine, newLineOnSpace: Boolean): TRect;
    class function CreateRedFontTextBitmap(const aText: string; multiLine, newLineOnSpace: Boolean): TBitmap;
  protected
    procedure Paint; override;
  public
    class destructor Destroy;
    constructor Create(aOwner: TComponent); override;
    procedure AfterConstruction; override;
  end;

  // controls that do not scale
  TLabelEx = class(TLabel)
  protected
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  end;

  TDrawGridEx = class(TDrawGrid)
  protected
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  public
    procedure InvalidateRow(aRow: Integer); inline;
  end;

  TEditEx = class(TEdit)
  protected
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  end;

  TPanelEx = class(TPanel)
  protected
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  end;

implementation

{ TBaseForm }

procedure TBaseForm.AfterConstruction;
begin
  inherited;
  ScaleForCurrentDpi;
end;

constructor TBaseForm.Create(aOwner: TComponent);
begin
  inherited CreateNew(aOwner);
  StyleElements := [];
  Caption := 'Lemmix';
  Color := clBlack;
  BorderStyle := bsNone;
  BorderIcons := [];
  Font.Name := 'Segoe UI';
  BoundsRect := CurrentDisplay.BoundsRect;
  DefaultMonitor := TDefaultMonitor.dmMainForm; // show on the same monitor as the mainform
end;

{ TAppForm }

constructor TAppForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  KeyPreview := True;
end;

procedure TAppForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_OWNDC; // maybe faster screen output
end;

procedure TAppForm.Input(param: TObject);
begin
 // do nothing
end;

procedure TAppForm.BuildScreen;
begin
  // do nothing
end;

procedure TAppForm.BeforeCloseScreen(aNextScreen: TGameScreenType);
begin
  // override for extra actions
end;

procedure TAppForm.CloseScreen(aNextScreen: TGameScreenType);
begin
  if fScreenIsClosing then
    Exit;
  fScreenIsClosing := True;

  Application.OnIdle := nil;
  OnKeyDown := nil;
  OnKeyPress := nil;
  OnKeyUp := nil;

  BeforeCloseScreen(aNextScreen);
  ModalResult := Ord(aNextScreen); // all screens are modal.

  App.FormEnd(Self);
end;

function TAppForm.ShowScreen(param: TObject = nil): TGameScreenType;
// if the screen closed with alt+f4 (for example) it bypasses our logic, so we remap to unknown screen
var
  res: TModalResult;
begin
  App.FormBegin(Self);

  Input(param);
  BuildScreen;

  res := ShowModal;

  if (res >= Ord(Low(TGameScreenType))) and (res <= Ord(High(TGameScreenType))) then
    Result := TGameScreenType(res)
  else
    Result := TGameScreenType.Unknown;

  App.FormEnd(Self);
end;

{ TToolForm }

constructor TToolForm.Create(aOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  if not App.Config.FormOptions.Opaque then begin
    AlphaBlend := True;
    AlphaBlendValue := 245;
  end;
  WindowState := wsNormal;
  Color := FORM_COLOR;
  Cursor := crDefault;
end;

procedure TToolForm.AfterConstruction;
var
  margin: Integer;
begin
  inherited;
  if not App.Config.FormOptions.FullScreen then
    margin := Scale(32)
  else
    margin := 0;
  Left := CurrentDisplay.BoundsRect.Left + margin;
  Top := CurrentDisplay.BoundsRect.Top + margin;
  Width := CurrentDisplay.BoundsRect.Width - margin * 2;
  Height := CurrentDisplay.BoundsRect.Height - margin * 2;
end;

procedure TToolForm.Paint;
begin
  inherited Paint;
  Canvas.Brush.Color := clGray;
  Canvas.FrameRect(ClientRect);
end;

class procedure TToolForm.EnsureBmpFont;
var
  bmp: TBitmap32;
  fromColor, toColor: TColor32;
begin
  if Assigned(BmpFont) then
    Exit;
  BmpFont := TBitmapFont.Create;
  BmpFont.FontBeginCreate;
  fromColor.Init(0, 0, 17, 255);
  toColor := Color32(FORM_COLOR);
  for var c: Char := 'a' to 'z' do begin
    bmp := TData.CreateAssetBitmap(c + '.bmp', True);
    bmp.ReplaceColor(fromColor, toColor);
    BmpFont.AddChar(c, bmp);
    bmp.Free;
  end;
  BmpFont.FontEndCreate;
end;

class destructor TToolForm.Destroy;
begin
  if Assigned(BmpFont) then
    FreeAndNil(BmpFont);
end;

class function TToolForm.GetRedFontTextSize(const aText: string; multiLine, newLineOnSpace: Boolean): TSize;
var
  size: TSize;
  x, y, h: Integer;
begin
  EnsureBmpFont;
  Result.Create(0, 0);
  x := 0;
  y := 0;
  h := 0;
  for var c: char in aText do begin
    if c.IsLetter then begin
      size := BmpFont.GetCharSize(c.ToLower);
      h := Max(h, size.cy);
      Inc(x, size.cx);
      Result.cx := Max(Result.cx, x);
      Result.cy := Max(Result.cy, size.cy);
    end
    else begin
      if ((c = ' ') and newLineOnSpace) or ((c = #13) and multiLine) then begin
        x := 0;
        Inc(y, h);
        Result.cy := Max(Result.cy, y);
        h := 0;
      end
      else if (c = ' ') then begin
        Inc(x, BmpFont.AvgWidth);
        Result.cx := Max(Result.cx, x);
      end;
    end;
   end;

  Result.cx := Scale(Result.cx);
  Result.cy := Scale(Result.cy);
end;

class function TToolForm.DrawBitmapText(aCanvas: TCanvas; const aText: string; x, y: Integer; multiLine, newLineOnSpace: Boolean): TRect;
var
  h, ax, ay: Integer;
  size: TSize;
begin
  Result.Create(x, y, x, y);
  EnsureBmpFont;
  ax := x;
  ay := y;
  h := 0;
  for var c: char in aText do begin
    if c.IsLetter then begin
      var delta := BmpFont.maxheight - BmpFont.GetCharSize(c.ToLower).cy;
      size := BmpFont.DrawChar(c.ToLower, aCanvas, ax, ay + delta div 2, CurrentDisplay.DpiScale);
      h := Max(h, size.cy);
      Inc(ax, size.cx);
      Result.Right := Max(Result.Right, ax);
      Result.Height := Max(Result.Height, size.cy);
    end
    else begin
      if ((c = ' ') and newLineOnSpace) or ((c = #13) and multiLine) then begin
        ax := x;
        if h > 0 then
          Inc(ay, h)
        else
          Inc(ay, BmpFont.AvgHeight);
        Result.Bottom := Max(Result.Bottom, ay);
        h := 0;
      end
      else if (c = ' ') then begin
        Inc(ax, BmpFont.AvgWidth);
        Result.Right := Max(Result.Right, ax);
      end;
    end;
   end;
//  end;
end;

class function TToolForm.CreateRedFontTextBitmap(const aText: string; multiLine, newLineOnSpace: Boolean): TBitmap;
begin
  Result := TBitmap.Create;
  var size := GetRedFontTextSize(aText, multiLine, newLineOnSpace);
  Result.SetSize(size.cx, size.cy);
  Result.Canvas.Brush.Color := FORM_COLOR;
  Result.canvas.FillRect(TRect.Create(0, 0, size.cx, size.cy));
  DrawBitmapText(Result.Canvas, aText, 0, 0, multiLine, newLineOnSpace);
end;


{ TLabelEx }

procedure TLabelEx.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  // do nothing
end;

{ TDrawGridEx }

procedure TDrawGridEx.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  // do nothing
end;

procedure TDrawGridEx.InvalidateRow(aRow: Integer);
begin
  inherited InvalidateRow(aRow);
end;

{ TEditEx }

procedure TEditEx.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  // do nothing
end;

{ TPanelEx }

procedure TPanelEx.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  // do nothing
end;

end.

