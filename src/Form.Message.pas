unit Form.Message;

interface

{$include lem_directives.inc}

uses
  Winapi.Windows,
  System.Types, System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Imaging.PngImage, Vcl.Graphics,
  Base.Utils,
  Form.Base;

type
  // the current size calculations for high dpi are messy but working for now. todo: SmartAdjustSize not perfect on high dpi.
  TMessageType = (
    Info,
    Warning,
    Error
  );

  TFormMessage = class(TBaseForm)
  strict private
    procedure SmartAdjustSize(const Msg: string);
    procedure InternalInvoke(const s: string; aType: TMessageType; const aFontname: string = '');
  private
    fImage: TImage;
    fLabel: TLabel;
    fPanel: TPanel;
    fBtnOK: TButton;
    class procedure Invoke(const s: string; aType: TMessageType; const aFontname: string = '');
  public
    constructor Create(aOwner: TComponent); override;
  end;

procedure DlgError(const s: string);
procedure DlgWarning(const s: string);
procedure DlgInfo(const s: string; const aFontname: string = '');

implementation

{ TFormMessage }

constructor TFormMessage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  NormalForm;
  ClientWidth := 474;
  Position := poScreenCenter;

  fImage := TImage.Create(Self);
  fImage.Parent := Self;
  fImage.SetBounds(8, 8, 64 ,64);
  fImage.Stretch := True;

  fLabel := TLabel.Create(Self);
  fLabel.AutoSize := False;
  fLabel.Parent := Self;
  fLabel.Left := 80;
  fLabel.Top := 8;
  fLabel.Width := Self.ClientWidth - fLabel.Left - 8;
  fLabel.Anchors := [akLeft, akTop, akRight];
  fLabel.WordWrap := True;

  fPanel := TPanel.Create(Self);
  fPanel.Parent := Self;
  fPanel.ParentColor := False;
  fPanel.ParentBackground := False;
  fPanel.Color := clBtnFace;
  fPanel.BevelOuter := bvNone;
  fPanel.Align := alBottom;
  fPanel.Height := 40;

  fBtnOK := TButton.Create(Self);
  fBtnOK.Parent := fPanel;
  fBtnOK.Caption := 'OK';
  fBtnOK.Default := True;
  fBtnOk.ModalResult := mrOK;
  fBtnOk.Cancel := True;
  fBtnOK.Left := fPanel.ClientWidth - 8 - fBtnOK.Width;
  fBtnOk.Top := (fPanel.ClientHeight - fBtnOk.Height) div 2;
  fBtnOK.Anchors := [akRight, akTop];
end;

procedure TFormMessage.SmartAdjustSize(const Msg: string);
var
//  s: string;
  R: TRect;
  VDist: Integer;
  MinH: INteger;
  MaxLabelHeight: Integer;
  H: Integer;
begin
  R.Create(0, 0, fLabel.Width, 0);
//  fLabel.Width := w;
  canvas.Font := Self.Font;
  DrawText(Canvas.Handle, Msg, Length(Msg), R, DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK);

  // just in case of an exception and currentdisplay is empty
  if CurrentDisplay.BoundsRect.Height = 0 then
    MaxLabelHeight := Screen.Height - fPanel.Height
  else
    MaxLabelHeight := CurrentDisplay.BoundsRect.Height - fPanel.Height;

  if R.Height > MaxLabelHeight then
    R.Height := MaxLabelHeight;
  fLabel.Height := R.Height;

  VDist := 24;

  fLabel.Top := VDist;
  H := fPanel.Height + fLabel.Height + VDist * 2;

  MinH := Scale(80) + fPanel.Height;
  if H < MinH then
    H := MinH;

  ClientWidth := ClientWidth.Scale;
  ClientHeight := H;
end;

procedure TFormMessage.InternalInvoke(const s: string; aType: TMessageType; const aFontname: string = '');
var
  png: TPngImage;
begin
  try
    png := TPngImage.Create;
    try
      case aType of
        TMessageType.Info: png.LoadFromResourceName(HINSTANCE, 'INFO');
        TMessageType.Warning: png.LoadFromResourceName(HINSTANCE, 'WARNING');
        TMessageType.Error: png.LoadFromResourceName(HINSTANCE, 'ERROR');
      end;
      fImage.Picture.Assign(png);
    finally
      png.Free;
    end;
  except
    // eat it
  end;

  if not aFontname.IsEmpty then
    Font.Name := aFontname;

  var txt: string := s;
  if txt.Length > 1024 then
    txt := Copy(txt, 1, 1024);
  txt := WrapText(txt, 80);
  fLabel.Caption := txt;
  SmartAdjustSize(txt);
  ShowModal;
end;

class procedure TFormMessage.Invoke(const s: string; aType: TMessageType; const aFontname: string = '');
begin
  var f: TFormMessage := TFormMessage.Create(nil);
  try
    f.InternalInvoke(s, aType, aFontname);
  finally
    f.Free;
  end;
end;

procedure DlgError(const s: string);
begin
  TFormMessage.Invoke(s, TMessageType.Error);
end;

procedure DlgWarning(const s: string);
begin
  TFormMessage.Invoke(s, TMessageType.Warning);
end;

procedure DlgInfo(const s: string; const aFontname: string = '');
begin
  TFormMessage.Invoke(s, TMessageType.Info, aFontname);
end;

end.

