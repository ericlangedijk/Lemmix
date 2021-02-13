unit GameScreen.Options;

{$include lem_directives.inc}

interface

// todo: scaling check

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, System.UITypes, System.Contnrs,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.GraphUtil, Vcl.Grids,
  Base.Utils, Base.Types, Base.Bitmaps,
  Dos.Consts,
  Prog.Base, Prog.Data, Prog.Cache,
  Game,
  Prog.App,
  Form.Base;

type
  TGameScreenOptions = class(TToolForm)
  public
    const
      COLOR_DOS = clWebDarkSlateBlue;
      COLOR_CUSTOM = clWebDarkOliveGreen;
      COLOR_LEMMINI = clWebBrown;
  private
    fGrid: TDrawGridEx;
    fInfoList: Consts.TStyleInformationList;
    fNewStyleSelected: Boolean;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Grid_DrawCell(Sender: TObject; aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
    procedure Grid_CanSelectCell(Sender: TObject; aCol, aRow: Longint; var CanSelect: Boolean);
    procedure Grid_DblClick(Sender: TObject);
    function SelectedInfoToCell(info: Consts.TStyleInformation): TPoint;
    function CoordToLevelInfo(x, y: Integer): Consts.TStyleInformation;
    function GetCurrentInfo: Consts.TStyleInformation;
    procedure DoExitScreen(ok: Boolean);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute: Boolean;
  end;

implementation


{ TGameScreenOptions }

constructor TGameScreenOptions.Create(aOwner: TComponent);
var
  info: Consts.TStyleInformation;
begin
  inherited Create(aOwner);
  fInfoList := Consts.TStyleInformationList.Create(False);
  // sys
  for info in Consts.StyleInformationlist do
    if info.StyleDef <> TStyleDef.User then
      fInfoList.Add(info);
  // dos
  for info in Consts.StyleInformationlist do
    if (info.StyleDef = TStyleDef.User) and (info.Family = TStyleFamily.DOS) then
      fInfoList.Add(info);
  // lemmini
  for info in Consts.StyleInformationlist do
    if (info.StyleDef = TStyleDef.User) and (info.Family <> TStyleFamily.DOS) then
      fInfoList.Add(info);
end;

destructor TGameScreenOptions.Destroy;
begin
  fInfoList.Free;
  inherited Destroy;
end;

procedure TGameScreenOptions.DoExitScreen(ok: Boolean);
begin
  if not ok then
    //CloseScreen(TGameScreenType.Menu)
    CloseScreen(TGameScreenType.Unknown)
  else begin
    var info: Consts.TStyleInformation := GetCurrentInfo;
    if Assigned(info) and (App.Style.Name <> info.Name) then begin
      App.NewStyleName := info.Name;
      fNewStyleSelected := True;
    end;
    //CloseScreen(TGameScreenType.Menu);
    CloseScreen(TGameScreenType.Unknown);
  end;
end;

procedure TGameScreenOptions.BuildScreen;
begin
  fGrid := TDrawGridEx.Create(Self);
  fGrid.Parent := Self;
  fGrid.Align := alClient;
  fGrid.AlignWithMargins := True;
  fGrid.ColCount := (fGrid.Width - Scale(24)) div Scale(320);
  fGrid.ScrollBars := ssNone;
  fGrid.RowCount := fInfoList.Count div fGrid.ColCount;
  if fInfoList.Count mod fGrid.ColCount <> 0 then
    fGrid.RowCount := fGrid.RowCount + 1;
  fGrid.FixedRows := 0;
  fGrid.FixedCols := 0;
  fGrid.DefaultColWidth := Scale(320);
  fGrid.DefaultRowHeight := Scale(160);
  fGrid.Color := clBlack;
  fGrid.ParentColor := True;
  fGrid.DefaultDrawing := False;
  fGrid.DrawingStyle := TGridDrawingStyle.gdsClassic;
  fGrid.BorderStyle := bsNone;
  fGrid.Options := fGrid.Options - [TGridOption.goDrawFocusSelected, TGridOption.goVertLine, TGridOption.goHorzLine, TGridOption.goRangeSelect];

  var p: TPoint := SelectedInfoToCell(Consts.FindStyleInfo(App.Style.Name));
  fGrid.Col := p.X;
  fGrid.Row := p.Y;

  fGrid.Margins.SetBounds(Scale(8), Scale(8), Scale(8), Scale(24));
  fGrid.OnDrawCell := Grid_DrawCell;
  fGrid.OnSelectCell := Grid_CanSelectCell;
  fGrid.OnDblClick := Grid_DblClick;

  var lab: TLabelEx := TLabelEx.Create(Self);
  lab.Parent := Self;
  lab.AutoSize := False;
  lab.Height := Scale(20);
  lab.Align := alBottom;
  lab.Alignment := taCenter;
  lab.Font.Color := clgray;
  lab.Font.Height := lab.Height - Scale(2);
  lab.Caption := App.StyleCache.GetTotalLevelCount.ToString + ' levels';

  OnKeyDown := Form_KeyDown;
end;

procedure TGameScreenOptions.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    DoExitScreen(False)
  else if Key = VK_RETURN then
    DoExitScreen(True);
end;

function TGameScreenOptions.CoordToLevelInfo(x, y: Integer): Consts.TStyleInformation;
begin
  var ix: Integer := y * fGrid.ColCount + x;
  if not fInfoList.ValidIndex(ix) then
    Exit(nil);
  Result := fInfoList[ix];
end;

function TGameScreenOptions.SelectedInfoToCell(info: Consts.TStyleInformation): TPoint;
begin
  Result := Point(0, 0);
  var ix: Integer := fInfoList.IndexOf(info);
  Result.Y := ix div fGrid.ColCount;
  Result.X := ix mod fGrid.ColCount;
end;

function TGameScreenOptions.GetCurrentInfo: Consts.TStyleInformation;
begin
  Result := CoordToLevelInfo(fGrid.Col, fGrid.Row);
end;

procedure TGameScreenOptions.Grid_CanSelectCell(Sender: TObject; aCol, aRow: Longint; var CanSelect: Boolean);
begin
  CanSelect := Assigned(CoordToLevelInfo(aCol, aRow));
end;

procedure TGameScreenOptions.Grid_DblClick(Sender: TObject);
begin
  DoExitScreen(True);
end;

procedure TGameScreenOptions.Grid_DrawCell(Sender: TObject; aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
var
  canv: TCanvas;
  information: Consts.TStyleInformation;
  myColor, myShadowColor: TColor;
  th, numberOfLevels: Integer;
  plural, txt: string;
  txtRect: TRect;

begin
  // background ------------------------------------------------------------------------------------------------------------------------------------------------
  canv := fGrid.Canvas;
  canv.Brush.Color := FORM_COLOR;
  canv.FillRect(aRect);

  information := CoordToLevelInfo(aCol, aRow);

  if not Assigned(information) then
    myColor := FORM_COLOR
  else if information.StyleDef = TStyleDef.User then begin
    if information.Family = TStyleFamily.DOS then
      myColor := clWebDarkOliveGreen
    else
      myColor := clWebBrown;
  end
  else
    myColor := clWebDarkSlateBlue;

  aRect.Inflate(Scale(-4), Scale(-4));

  if not Assigned(information) then
    myShadowColor := myColor
  else if (gdSelected in aState) then
    myShadowColor := clYellow
  else
    myShadowColor := GetShadowColor(myColor, -20);

  for var i := 0 to Scale(2) - 1 do begin
   canv.Brush.Color := myShadowColor;
   canv.FrameRect(aRect);
   aRect.Inflate(-1, -1);
  end;

  canv.Brush.Color := myColor;
  if not Assigned(information) then
    canv.FillRect(aRect)
  else
    GradientFillCanvas(canv, myColor, GetShadowColor(myColor, -10), aRect, TGradientDirection.gdVertical);

  if not Assigned(information) then
    Exit;

  numberOfLevels := App.StyleCache.GetLevelCount(information.Name);
  if numberOfLevels = 1 then
    plural := string.Empty
  else
    plural := 's';

  Inc(aRect.Left, Scale(8));
  Dec(aRect.Right, Scale(8));
  txtRect := aRect;

  SetBkMode(canv.Handle, TRANSPARENT);

  // style -----------------------------------------------------------------------------------------------------------------------------------------------------
  canv.Font := Self.Font;
  canv.Font.Height := Scale(32);
  canv.Font.Color := clWhite;
  canv.Font.Style := [fsBold];

  th := canv.TextHeight('Wq');
  txtRect := aRect;
  Inc(txtRect.Top, Scale(8));
  txtRect.Height := th;

  if not information.Description.IsEmpty then
    txt := information.Description
  else
    txt := information.name;

  canv.TextRect(txtRect, txt, [tfSingleLine, tfTop, tfCenter]);

  // number of levels + author ---------------------------------------------------------------------------------------------------------------------------------
  canv.Font.Height := Scale(22);
  canv.Font.Style := [];
  th := canv.TextHeight('Wq');
  txtRect.Offset(0, txtRect.Height + Scale(2));
  txtRect.Height := th;

  if information.Author.IsEmpty then
    txt := numberOfLevels.ToString + ' level' + plural
  else
    txt := numberOfLevels.ToString + ' level' + plural + ' by ' + information.Author;
  canv.TextRect(txtRect, txt, [tfSingleLine, tfTop, tfCenter]);

  // location --------------------------------------------------------------------------------------------------------------------------------------------------
  canv.Font.Height := Scale(19);
  th := canv.TextHeight('Wq');
  txtRect.Offset(0, txtRect.Height);
  txtRect.Height := th;

  canv.Font.Color := GetShadowColor(myColor, 70);
  if information.StyleDef = TStyleDef.User then
    txt := ExcludeTrailingPathDelimiter(Consts.PathToStyle[information.Name])
  else
    txt := 'Resource';

  canv.TextRect(txtRect, txt, [tfSingleLine, tfTop, tfCenter, tfPathEllipsis]);

  // info ------------------------------------------------------------------------------------------------------------------------------------------------------
  canv.Font.Color := clWhite;

  txt := information.Info;
  if txt.IsEmpty then
    txt := 'No info available';

  txtRect.Create(0, 0, aRect.Width, 0);
  txtRect.Inflate(-Scale(2), 0);
  canv.TextRect(txtRect, txt, [tfTop, tfWordBreak, tfCalcRect]); // calc height

  txtRect.Top := aRect.Bottom - txtRect.Height - Scale(2);
  txtRect.Left := aRect.Left + Scale(2);
  txtRect.Right := aRect.Right - Scale(2);
  txtRect.Bottom := aRect.Bottom - Scale(2);

  canv.TextRect(txtRect, txt, [tfTop, tfWordBreak]);
end;

class function TGameScreenOptions.Execute: Boolean;
begin
  var f: TGameScreenOptions := TGameScreenOptions.Create(nil);
  try
    f.ShowScreen;
    Result := f.fNewStyleSelected;
  finally
    f.Free;
  end;
end;

end.

