unit GameScreen.Config;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.Character, System.Math, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.GraphUtil, Vcl.Imaging.pngimage,
  Gr32,
  Base.Utils, Base.Types, Base.Bitmaps,
  Dos.Consts,
  Prog.Base, Prog.Data, Prog.Cache,
  Styles.Base,
  Prog.Config, Prog.App, Prog.Voice,
  Form.Base, Form.Message;

type
  TGameScreenConfig = class(TToolForm)
  private
    Config: TConfig;
    MainPanel: TPanelEx;
    fBkColor: TColor;
    EditPathToStyles: TEdit;
    EditPathToSounds: TEdit;
    EditPathToMusic: TEdit;
    EditPathToReplay: TEdit;
    InfoLabel: TLabelEx;
    CheatClicks: Integer;
    function Save(out needsRestart: Boolean): Boolean;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetInfoLabel;
    procedure GameOption_Click(Sender: TObject);
    procedure FormOption_Click(Sender: TObject);
    procedure MiscOption_Click(Sender: TObject);
    procedure VoiceOption_Click(Sender: TObject);
    procedure MechanicOption_Click(Sender: TObject);
    procedure Img_DeveloperMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure BuildScreen; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute: TGameScreenType; static;
  end;

implementation

type
  TCheck = class(TCustomControl)
  strict private
    class var
      fImagesLoaded: Boolean;
      fPngChecked: TWicImage;
      fPngUnchecked: TWicImage;
      fPngCheckedTransparent: TWicImage;
      fPngUncheckedTransparent: TWicImage;
    class procedure CheckLoadImages; static;
  strict private
    fChecked: Boolean;
    fSpaceKeyPressed: Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; shift: TShiftState); override;
    procedure KeyUp(var Key: Word; shift: TShiftState); override;
  protected
    procedure Paint; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: Integer); override;
    procedure MouseUp(button: TMouseButton; shift: TShiftState; x, y: Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    class destructor Destroy;
    destructor Destroy; override;
    property Checked: Boolean read fChecked write SetChecked;
  end;

{ TCheck }

procedure TCheck.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  // nope
end;

procedure TCheck.Click;
begin
  fSpaceKeyPressed := False;
  fChecked := not fChecked;
  inherited Click;
  Invalidate;
end;

class procedure TCheck.CheckLoadImages;
begin
  if fImagesLoaded then
    Exit;
  fPngChecked := TData.CreateAssetWicImage(Consts.FilenameAssetCheckBox);
  fPngChecked.InterpolationMode := TWICImageInterpolationMode.wipmHighQualityCubic;

  fPngUnchecked := TData.CreateAssetWicImage(Consts.FilenameAssetCheckBoxGray);
  fPngUnchecked.InterpolationMode := TWICImageInterpolationMode.wipmHighQualityCubic;

  fPngCheckedTransparent := TData.CreateAssetWicImage(Consts.FilenameAssetCheckBoxTransparent);
  fPngCheckedTransparent.InterpolationMode := TWICImageInterpolationMode.wipmHighQualityCubic;

  fPngUncheckedTransparent := TData.CreateAssetWicImage(Consts.FilenameAssetCheckBoxGrayTransparent);
  fPngUncheckedTransparent.InterpolationMode := TWICImageInterpolationMode.wipmHighQualityCubic;

  fImagesLoaded := True;
end;

class destructor TCheck.Destroy;
begin
  if Assigned(fPngChecked) then FreeAndNil(fPngChecked);
  if Assigned(fPngUnchecked) then FreeAndNil(fPngUnchecked);
  if Assigned(fPngCheckedTransparent) then FreeAndNil(fPngCheckedTransparent);
  if Assigned(fPngUncheckedTransparent) then FreeAndNil(fPngUncheckedTransparent);
  fImagesLoaded := False;
end;

constructor TCheck.Create(aOwner: TComponent);
begin
  inherited;
  CheckLoadImages;
  TabStop := True;
  Width := 240;
  Height := 24;
end;

destructor TCheck.Destroy;
begin
  inherited;
end;

procedure TCheck.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
   Style := Style or WS_TABSTOP;
  WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCheck.KeyDown(var Key: Word; shift: TShiftState);
begin
  inherited;
  if fSpaceKeyPressed then
    Exit;
  fSpaceKeyPressed := False;
  if shift = [] then
    if Key = VK_SPACE then begin
      fSpaceKeyPressed := True;
      Invalidate;
    end;
end;

procedure TCheck.KeyUp(var Key: Word; shift: TShiftState);
begin
  inherited;
  if not fSpaceKeyPressed then
    Exit;
  if shift = [] then
    if Key = VK_SPACE then begin
      Key := 0;
      fSpaceKeyPressed := False;
      Click;
    end;
end;

procedure TCheck.MouseDown(button: TMouseButton; shift: TShiftState; x, y: Integer);
begin
  if not Focused and CanFocus then
    SetFocus;
  fSpaceKeyPressed := True;
  Invalidate;
  inherited;
end;

procedure TCheck.MouseUp(button: TMouseButton; shift: TShiftState; x, y: Integer);
begin
  fSpaceKeyPressed := False;
  Invalidate;
  inherited;
end;

procedure TCheck.Paint;
var
  src: TWicImage;
begin
  if fChecked then begin
    if fSpaceKeyPressed
    then src := fPngCheckedTransparent
    else src := fPngChecked;
  end
  else begin
    if fSpaceKeyPressed
    then src := fPngUncheckedTransparent
    else src := fPngUnchecked;
  end;

  if Assigned(src) then
    Canvas.StretchDraw(Rect(0, 0, Height, Height), src);

  Canvas.Font := Self.Font;
  if fChecked then
    Canvas.Font.Color := clWhite
  else
    Canvas.Font.Color := clGray;
  SetBkMode(Canvas.Handle, TRANSPARENT);

  var s: string := Caption;
  var r: TRect := TRect.Create(Height + Scale(8), 0, Width, Height);
  var tr:= r;

  Dec(tr.Right, Scale(2));
  DrawTextEx(Canvas.Handle, PChar(s), -1, tr, DT_LEFT or DT_SINGLELINE or DT_VCENTER, nil);

  if Focused then begin
    Canvas.Brush.Color := clGray;
    Canvas.FrameRect(Rect(Height + Scale(3), 0, Width, Height));
  end;
end;

procedure TCheck.SetChecked(const Value: Boolean);
begin
  if fChecked <> Value then begin
    fChecked := value;
    Invalidate;
  end;
end;

procedure TCheck.WMKillFocus(var Msg: TWMKillFocus);
begin
  fSpaceKeyPressed := False;
  inherited;
  Invalidate;
end;

procedure TCheck.WMSetFocus(var Msg: TWMSetFocus);
begin
  fSpaceKeyPressed := False;
  inherited;
  Invalidate;
end;

{ TGameScreenConfig }

constructor TGameScreenConfig.Create(aOwner: TComponent);
begin
  inherited;

  MainPanel := TPanelEx.Create(Self);
  MainPanel.Parent := Self;
  MainPanel.Color := FORM_COLOR;
  MainPanel.BevelInner := bvNone;
  MainPanel.BevelOuter := bvNone;

  fBkColor := GetShadowColor(FORM_COLOR, 30);

  OnKeyDown := Form_KeyDown;
end;

destructor TGameScreenConfig.Destroy;
begin
  Application.OnHint := nil;
  inherited;
end;

procedure TGameScreenConfig.AlignControls;
begin
  inherited;
  MainPanel.Left := ClientWidth div 2 - MainPanel.Width div 2;
end;

procedure TGameScreenConfig.BuildScreen;
var
  x, y: Integer;
  COLUMN_WIDTH, LINE_HEIGHT, TEXT_HEIGHT, LINE_DIST, INDENT: Integer;
  //img: TImage;

  function NewCheck(x: Integer; var y: Integer; aWidth: Integer): TCheck;
  begin
    Result := TCheck.Create(Self);
    Result.Parent := MainPanel;
    Result.SetBounds(x, y, aWidth, LINE_HEIGHT - Scale(2));
    Result.Font.Height := TEXT_HEIGHT;
    y := Result.top + LINE_HEIGHT + LINE_DIST;
  end;

  procedure NewFormOptionCheckBox(x: Integer; var y: Integer; aWidth: Integer; aOption: TFormOption);
  begin
    var cb: TCheck := NewCheck(x, y, aWidth);
    cb.Caption := SpaceCamelCase(aOption.AsString);
    cb.Checked := aOption in Config.FormOptions;
    cb.OnClick := FormOption_Click;
    cb.Tag := Ord(aOption);
  end;

  procedure NewGameOptionCheckBox(x: Integer; var y: Integer; aWidth: Integer; aOption: TGameOption);
  begin
    var cb: TCheck := NewCheck(x, y, aWidth);
    cb.Caption := SpaceCamelCase(aOption.AsString);
    cb.Checked := aOption in Config.GameOptions;
    cb.OnClick := GameOption_Click;
    cb.Tag := Ord(aOption);
  end;

  procedure NewMiscOptionCheckBox(x: Integer; var y: Integer; aWidth: Integer; aOption: TMiscOption);
  begin
    var cb: TCheck := NewCheck(x, y, aWidth);
    cb.Caption := SpaceCamelCase(aOption.AsString);
    cb.Checked := aOption in Config.MiscOptions;
    cb.OnClick := MiscOption_Click;
    cb.Tag := Ord(aOption);
  end;

  procedure NewVoiceOptionCheckBox(x: Integer; var y: Integer; aWidth: Integer; aOption: TVoiceOption);
  begin
    var cb: TCheck := NewCheck(x, y, aWidth);
    cb.Caption := SpaceCamelCase(aOption.AsString);
    cb.Checked := aOption in Config.VoiceOptions;
    cb.Width := cb.Width;
    cb.Tag := Ord(aOption);
    cb.OnClick := VoiceOption_Click;
  end;

  procedure NewMechanicOptionCheckBox(x: Integer; var y: Integer; aWidth: Integer; aOption: TOptionalMechanic);
  begin
    var cb: TCheck := NewCheck(x, y, aWidth);
    cb.Caption := SpaceCamelCase(aOption.AsString);
    cb.Checked := aOption in Config.OptionalMechanics;
    cb.Tag := Ord(aOption);
    cb.OnClick := MechanicOption_Click;
  end;

  function NewEdit(x: Integer; var y: Integer; const aText, aLabel: string): TEditEx;
  begin
    Result := TEditEx.Create(Self);
    Result.AutoSize := False;
    Result.Parent := MainPanel;
    Result.Text := aText;
    Result.Left := x;
    Result.Top := y;
    Result.BorderStyle := bsNone;
    Result.Font.Height := TEXT_HEIGHT;
    Result.Width := COLUMN_WIDTH;
    Result.Height := LINE_HEIGHT;
    Result.Color := fBkColor;
    Result.Font.Color := clWhite;
    Result.TextHint := aLabel;
    Result.MaxLength := 256;
    Result.OnKeyDown := Edit_KeyDown;
    y := Result.Top + Result.Height + LINE_DIST;
  end;

  function NewRedTextImg(const s: string; x: Integer; var y: Integer; aParent: TWinControl = nil): TImage;
  begin
    var bmp: TBitmap := CreateRedFontTextBitmap(s, False, False);
    Result := TImage.Create(Self);
    if aParent = nil
    then Result.Parent := MainPanel
    else Result.Parent := aParent;
    Result.Picture.Assign(bmp);
    Result.AutoSize := True;
    Result.Left := x;
    Result.Top := y;
    y := Result.Top + Scale(BmpFont.MaxHeight) + LINE_DIST;
    bmp.Free;
  end;

begin
  Config := App.Config;

  COLUMN_WIDTH := Scale(350);
  LINE_HEIGHT := Scale(21);
  TEXT_HEIGHT := Scale(19);
  LINE_DIST := Scale(4);
  INDENT := Scale(8);

  // column 1
  x := 0;
  y := 0;
  // game options ----------------------------------------------------------------------------------------------------------------------------------------------
  NewRedTextImg('Game', x, y);
  for var opt: TGameOption in TGameOptions.ALL do
    NewGameOptionCheckBox(x + INDENT, y, Scale(320), opt);

  // mechanics options -----------------------------------------------------------------------------------------------------------------------------------------
  Inc(y, LINE_HEIGHT);
  NewRedTextImg('Mech', x, y).OnMouseDown := Img_DeveloperMouseDown;
  for var opt: TOptionalMechanic := Low(TOptionalMechanic) to High(TOptionalMechanic) do
    NewMechanicOptionCheckBox(x + INDENT, y, Scale(320), opt);


  // column 2
  y := 0;
  Inc(x, COLUMN_WIDTH);
  // voice options ---------------------------------------------------------------------------------------------------------------------------------------------
  NewRedTextImg('Forms', x, y);
  for var opt: TFormOption in TFormOptions.ALL do
    NewFormOptionCheckBox(x + INDENT, y, Scale(320), opt);

  Inc(y, LINE_HEIGHT);
  NewRedTextImg('Voice', x, y);
  for var opt: TVoiceOption in TVoiceOptions.ALL do
    NewVoiceOptionCheckBox(x + INDENT, y, Scale(320), opt);

  // column 3
  y := 0;
  Inc(x, COLUMN_WIDTH);
  // paths -----------------------------------------------------------------------------------------------------------------------------------------------------
  NewRedTextImg('Misc', x, y);
  for var opt: TMiscOption in TMiscOptions.ALL do
    NewMiscOptionCheckBox(x + INDENT, y, Scale(320), opt);

  // paths -----------------------------------------------------------------------------------------------------------------------------------------------------
  Inc(y, LINE_HEIGHT);
  NewRedTextImg('Paths', x, y);
  EditPathToStyles := NewEdit(x + INDENT, y, Config.PathToStyles, 'Custom Path to Styles');
  EditPathToMusic := NewEdit(x + INDENT, y, Config.PathToMusic, 'Custom Path to Music');
  EditPathToSounds := NewEdit(x + INDENT, y, Config.PathToSounds, 'Custom Path to Sounds');
  EditPathToReplay := NewEdit(x + INDENT, y, Config.PathToReplay, 'Custom Path to Replays');

  MainPanel.AutoSize := True;
  MainPanel.Top := Scale(32);

  // info label------------------------------------------------------------------------------------------------------------------------------------------------
  InfoLabel := TLabelEx.Create(Self);
  InfoLabel.Parent := Self;
  InfoLabel.AutoSize := False;
  InfoLabel.Height := LINE_HEIGHT * 2;
  InfoLabel.Font.Height := TEXT_HEIGHT * 2;
  InfoLabel.Align := alBottom;
  InfoLabel.Alignment := taCenter;
  InfoLabel.Font.Color := clLime;
  SetInfoLabel;
end;

procedure TGameScreenConfig.Edit_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DOWN then begin
    Key := 0;
    SelectNext(TEdit(Sender), True, True);
  end
  else if Key = VK_UP then begin
    Key := 0;
    SelectNext(TEdit(Sender), False, True);
  end;
end;

class function TGameScreenConfig.Execute: TGameScreenType;
begin
  var
    f: TGameScreenConfig := TGameScreenConfig.Create(nil);
  try
    Result := f.ShowScreen;
  finally
    f.Free;
  end;
end;

procedure TGameScreenConfig.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  restart: Boolean;
begin
  if Key = VK_ESCAPE then begin
    CloseScreen(TGameScreenType.Unknown);
  end
  else if Key = VK_F2 then begin
    if not Save({out} restart) then
      Exit;

    if restart then begin
      restart := DlgConfirm('Restart is needed. Restart Lemmix now?');
    end;

    if not restart then
      CloseScreen(TGameScreenType.Unknown)
    else
      CloseScreen(TGameScreenType.Restart);
  end;
end;

procedure TGameScreenConfig.FormOption_Click(Sender: TObject);
var
  chk: TCheck absolute Sender;
begin
  if chk.Checked then
    Include(Config.FormOptions, TFormOption(chk.Tag))
  else
    Exclude(Config.FormOptions, TFormOption(chk.Tag));
end;

procedure TGameScreenConfig.GameOption_Click(Sender: TObject);
var
  chk: TCheck absolute Sender;
begin
  if chk.Checked then
    Include(Config.GameOptions, TGameOption(chk.Tag))
  else
    Exclude(Config.GameOptions, TGameOption(chk.Tag))
end;

procedure TGameScreenConfig.MiscOption_Click(Sender: TObject);
var
  chk: TCheck absolute Sender;
begin
  if chk.Checked then
    Include(Config.MiscOptions, TMiscOption(chk.Tag))
  else
    Exclude(Config.MiscOptions, TMiscOption(chk.Tag))
end;

procedure TGameScreenConfig.VoiceOption_Click(Sender: TObject);
var
  chk: TCheck absolute Sender;
begin
  if chk.Checked then
    Include(Config.VoiceOptions, TVoiceOption(chk.Tag))
  else
    Exclude(Config.VoiceOptions, TVoiceOption(chk.Tag))
end;

procedure TGameScreenConfig.MechanicOption_Click(Sender: TObject);
var
  chk: TCheck absolute Sender;
begin
  if chk.Checked then
    Include(Config.OptionalMechanics, TOptionalMechanic(chk.Tag))
  else
    Exclude(Config.OptionalMechanics, TOptionalMechanic(chk.Tag))
end;

function TGameScreenConfig.Save(out needsRestart: Boolean) : Boolean;
var
  paths: array[0..3] of string;
  oldpaths: array[0..3] of string;
begin
  needsRestart := False;
  Result := True;

  Config.PathToStyles := ExcludeTrailingPathDelimiter(Trim(EditPathToStyles.Text));
  Config.PathToMusic := ExcludeTrailingPathDelimiter(Trim(EditPathToMusic.Text));
  Config.PathToSounds := ExcludeTrailingPathDelimiter(Trim(EditPathToSounds.Text));
  Config.PathToReplay := ExcludeTrailingPathDelimiter(Trim(EditPathToReplay.Text));

  paths[0] := Config.PathToStyles;
  paths[1] := Config.PathToMusic;
  paths[2] := Config.PathToSounds;
  paths[3] := Config.PathToReplay;

  oldpaths[0] := ExcludeTrailingPathDelimiter(App.Config.PathToStyles);
  oldpaths[1] := ExcludeTrailingPathDelimiter(App.Config.PathToMusic);
  oldpaths[2] := ExcludeTrailingPathDelimiter(App.Config.PathToSounds);
  oldpaths[3] := ExcludeTrailingPathDelimiter(App.Config.PathToReplay);

  // check valid folders
  for var i := 0 to 3 do begin
    if not SameText(paths[i], oldpaths[i]) then begin
      if not paths[i].IsEmpty and not TDirectory.Exists(paths[i]) then begin
        DlgError('Folder ' + paths[i] + 'does not exist');
        Exit(False);
      end;
    end;
  end;

  // check some folder changed, then restart needed
  for var i := 0 to 3 do
    if not SameText(paths[i], oldpaths[i]) then begin
      needsRestart := True;
      Break;
    end;

  // apply
  App.Config := Config;
  App.VoiceMgr.Enabled := App.Config.MiscOptions.Voice;
  App.VoiceMgr.Options := App.Config.VoiceOptions;
end;

procedure TGameScreenConfig.SetInfoLabel;
begin
  if not Consts.GLOBAL_DEVELOPER_MODUS then
    InfoLabel.Caption := 'Press escape to cancel, F2 to save'
  else
    InfoLabel.Caption := 'Developer Modus. Press escape to cancel, F2 to save';
end;

procedure TGameScreenConfig.Img_DeveloperMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// god modus backdoor to create lemmings on the fly in gamescreen
// click 5 times on the 'Mech' image with right mousebutton to activate or deactivate.
begin
  if Button <> TMouseButton.mbRight then
    Exit;
  Inc(CheatClicks);
  if CheatClicks = 5 then begin
    Consts.GLOBAL_DEVELOPER_MODUS := not Consts.GLOBAL_DEVELOPER_MODUS;
    CheatClicks := 0;
    SetInfoLabel;
  end;
end;

end.
