unit GameScreen.Help;

interface

uses
  Winapi.Windows,
  System.Types, System.Classes, System.SysUtils, System.Character, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Gr32,
  Base.Utils, Base.Types, Base.Bitmaps,
  Dos.Consts,
  Prog.Base, Prog.Data, Prog.Cache,
  Styles.Base,
  Prog.App,
  Form.Base;

type
  TGameScreenHelp = class(TToolForm)
  private
    fTitle: string;
    fText: THelpString;
    fKeys: TStringList;
    fCommands: TStringList;
    fPillarTop: TBitmap32;
    fPillar: TBitmap32;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Form_Paint(Sender: TObject);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class procedure Execute(const aTitle: string; const aText: THelpString); static;
  end;

implementation

{ TGameScreenHelp }

constructor TGameScreenHelp.Create(aOwner: TComponent);
begin
  inherited;
  fKeys := TStringList.Create;
  fCommands := TStringList.Create;

  // pillar images
  fPillarTop := TData.CreateAssetBitmap(Consts.FilenameAssetPillarTop);
  fPillar := TData.CreateAssetBitmap(Consts.FilenameAssetPillar);
  var c: TColor32;
  c.Init(0, 0, 17, 255);
  fPillarTop.ReplaceColor(c, Color32(Color));

  // events
  OnKeyDown := Form_KeyDown;
  OnMouseDown := Form_MouseDown;
  OnPaint := Form_Paint;
end;

destructor TGameScreenHelp.Destroy;
begin
  fPillar.Free;
  fPillarTop.Free;
  fKeys.Free;
  fCommands.Free;
  inherited;
end;

procedure TGameScreenHelp.BuildScreen;
begin
  fText.Split(fKeys, fCommands);
end;

procedure TGameScreenHelp.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Escape then
    CloseScreen(TGameScreenType.Unknown);
end;

procedure TGameScreenHelp.Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CloseScreen(TGameScreenType.Unknown);
end;

procedure TGameScreenHelp.Form_Paint(Sender: TObject);
var
  centerX: Integer;
  r: TRect;
  cw, ch, x, y, dist: Integer;
begin

  cw := ClientWidth;
  ch := ClientHeight;

  // draw images
  x := Scale(8);
  y := x;

  fPillarTop.DrawTo(Canvas.Handle, x, y);
  inc(y, fPillarTop.Height);

  while y < cw - 1 do begin
    fPillar.DrawTo(Canvas.Handle, x, y);
    Inc(y, fPillar.Height);
  end;

  x := cw - 1 - Scale(8) - fPillarTop.Width;
  y := Scale(8);

  fPillarTop.DrawTo(Canvas.Handle, x, y);
  inc(y, fPillarTop.Height);

  while y < ch - 1 do begin
    fPillar.DrawTo(Canvas.Handle, x, y);
    Inc(y, fPillar.Height);
  end;

  dist := Scale(8);
  centerX := cw div 2;

  // text
  SetBkMode(Canvas.Handle, TRANSPARENT);
  DrawBitmapText(Canvas, fTitle, fPillar.Width + Scale(32), Scale(32), True, True);

  Canvas.Font.Height := Scale(20);
  Canvas.Font.Color := clLime;

  r.Create(0, 0, centerX - dist, Canvas.TextHeight('q'));
  r.Offset(0, r.Height);

  for var key: string in fKeys do begin
    var s: string := key;
    if s <> '-' then begin
      Canvas.TextRect(r, s, [tfSingleLine, tfRight]);
      r.Offset(0, r.Height);
    end
    else begin
      r.Offset(0, r.Height); // just space
    end;
  end;

  r.Create(CenterX + dist, 0, cw - 1, Canvas.TextHeight('q'));
  r.Offset(0, r.Height);

  Canvas.Font.Color := clWebLightCoral;
  for var command: string in fCommands do begin
    var s: string := command;
    if s <> '-' then begin
      Canvas.TextRect(r, s, [tfSingleLine, tfLeft]);
      r.Offset(0, r.Height);
    end
    else
      r.Offset(0, r.Height);
  end;
end;

class procedure TGameScreenHelp.Execute(const aTitle: string; const aText: THelpString);
begin
  var f: TGameScreenHelp := TGameScreenHelp.Create(nil);
  try
    f.fTitle := aTitle;
    f.fText := aText;
    f.ShowScreen;
  finally
    f.Free;
  end;
end;

end.
