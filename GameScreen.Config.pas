unit GameScreen.Config;

{$include lem_directives.inc}

interface

// IN THE MAKING. NOT USED YET

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, System.UITypes, System.Contnrs,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.GraphUtil, Vcl.WinXCtrls,
  Base.Utils, Base.Bitmaps,
  Dos.Consts,
  Prog.Types, Prog.Base, Prog.Data, Prog.Cache,
  Game,
  Prog.App,
  Form.Base;

type
  //  This is a 'normal' windows screen
  TGameScreenConfig = class(TBaseDosForm)
  private
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NewMechanicskControl(x: Integer; var y: Integer; const aCaption: string; mech: TOptionalMechanic);

    procedure Switch_Click(Sender: TObject);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{ TGameScreenConfig }

procedure TGameScreenConfig.BuildScreen;
var
  x, y: Integer;
begin
  x := 100;
  y := 100;
  NewMechanicskControl(x, y, 'Nuke glitch', TOptionalMechanic.NukeGlitch);
  Inc(y, 4);
  NewMechanicskControl(x, y, 'Pause glitch', TOptionalMechanic.PauseGlitch);
end;

constructor TGameScreenConfig.Create(aOwner: TComponent);
begin
  inherited;
  onKeyDown := Form_KeyDown;
end;

procedure TGameScreenConfig.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    CloseScreen(TGameScreenType.Menu);
end;

procedure TGameScreenConfig.NewMechanicskControl(x: Integer; var y: Integer; const aCaption: string; mech: TOptionalMechanic);
var
  lab: TLabel;
  switch: TToggleSwitch;
begin
  lab := TLabel.Create(Self);
  lab.Parent := Self;
  lab.AutoSize := False;
  lab.Font.Color := clWhite;
  lab.Caption := aCaption;
  lab.Left := x;
  lab.Top := y;
  lab.Height := 21;
  lab.Width := 120;
  lab.Layout := tlCenter;

  switch := TToggleSwitch.Create(Self);
  switch.Parent := Self;
  switch.OnClick := Switch_Click;
  switch.Left := lab.Left + lab.Width + 20;
  switch.Top := y;
  switch.StateCaptions.CaptionOff := 'No';
  switch.StateCaptions.CaptionOn := 'Yes';
  switch.Font.Color := lab.Font.Color;
//  switch.ThumbWidth := switch.SwitchWidth div 2 - 4;
  switch.ShowStateCaption := False;
  switch.Height := 21;
  switch.Color := RGB(10,10,10);//clDkGray;
  switch.FrameColor := clSilver;
//  switch.SwitchHeight := 25;
//  switch.


//  switch.
  if mech in App.Config.OptionalMechanics then switch.State := TToggleSwitchState.tssOn;

  Inc(y, 21);
end;

procedure TGameScreenConfig.Switch_Click(Sender: TObject);
var
  T: TToggleSwitch absolute Sender;
begin
  if T.State = TToggleSwitchState.tssOff then begin
    T.ThumbColor := clGray;
  end
  else begin
    T.ThumbColor := clLime;
  end;
end;

end.

