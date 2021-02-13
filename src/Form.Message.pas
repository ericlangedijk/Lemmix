unit Form.Message;

interface

{$include lem_directives.inc}

uses
  System.UITypes,
  Vcl.Dialogs,
  Base.Utils;

procedure DlgError(const s: string);
procedure DlgWarning(const s: string);
procedure DlgInfo(const s: string);
function DlgConfirm(const s: string): Boolean;

implementation

procedure DlgError(const s: string);
begin
  MessageDlg(CheckReformStringForDialog(s), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure DlgWarning(const s: string);
begin
  MessageDlg(CheckReformStringForDialog(s), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
end;

procedure DlgInfo(const s: string);
begin
  MessageDlg(CheckReformStringForDialog(s), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

function DlgConfirm(const s: string): Boolean;
begin
  Result := MessageDlg(CheckReformStringForDialog(s), TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes;
end;

end.

