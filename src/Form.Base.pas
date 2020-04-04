unit Form.Base;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Gr32,
  Base.Utils,
  Prog.Types, Prog.App;

type
  //  abstract runtime black, fullscreen, ancestor form
  TBaseForm = class(TForm)
  protected
    procedure NormalForm;
  public
    constructor Create(aOwner: TComponent); override;
    procedure AfterConstruction; override; final;
  end;

  // a lemmings form with some virtuals
  TBaseDosForm = class(TBaseForm, IDosForm) // todo: (major rework) embed inside mainform
  private
    fScreenIsClosing: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override; final;
  protected
    procedure PrepareGameParams; virtual;
    procedure BuildScreen; virtual;
    procedure BeforeCloseScreen(aNextScreen: TGameScreenType); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    procedure CloseScreen(aNextScreen: TGameScreenType);
    function ShowScreen: TGameScreenType;
    property ScreenIsClosing: Boolean read fScreenIsClosing;
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
  //WindowState := wsMaximized;
  BoundsRect := CurrentDisplay.BoundsRect;
  DefaultMonitor := TDefaultMonitor.dmMainForm; // show on the same monitor as the mainform
end;

procedure TBaseForm.NormalForm;
begin
  Color := clWindow;
  BorderStyle := bsSizeable;
  BorderIcons := [biSystemMenu, biMinimize, biMaximize, biHelp];
  WindowState := wsNormal;
  Cursor := crDefault;
end;

{ TBaseDosForm }

procedure TBaseDosForm.BeforeCloseScreen(aNextScreen: TGameScreenType);
begin
  // override for extra actions
end;

procedure TBaseDosForm.CloseScreen(aNextScreen: TGameScreenType);
begin
  fScreenIsClosing := True;

  Application.OnIdle := nil;
  OnKeyDown := nil;
  OnKeyPress := nil;
  OnKeyUp := nil;

  BeforeCloseScreen(aNextScreen);
  ModalResult := Ord(aNextScreen); // all screens are modal. this closes the form
end;

constructor TBaseDosForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  KeyPreview := True;
end;

procedure TBaseDosForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_OWNDC; // maybe faster screen output
end;

procedure TBaseDosForm.PrepareGameParams;
begin
  // do nothing
end;

procedure TBaseDosForm.BuildScreen;
begin
  // do nothing
end;

function TBaseDosForm.ShowScreen: TGameScreenType;
// if the screen closed with alt+f4 (for example) it bypasses our logic, so we remap to unknown screen
var
  res: TModalResult;
begin
  PrepareGameParams;
  BuildScreen;

  res := ShowModal;

  if (res >= Ord(Low(TGameScreenType))) and (res <= Ord(High(TGameScreenType))) then
    Result := TGameScreenType(res)
  else
    Result := TGameScreenType.Unknown;
end;

end.

