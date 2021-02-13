unit Prog.Voice;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils,
  SpeechLib_TLB,
  Base.Utils, Base.Types,
  Base.Strings;

type
  TVoiceMgr = class
  private
    class var Inst: TVoiceMgr;
  strict private
    const TIMER_ID = 1;
  private
    fVoice: ISpeechVoice;
    fHandle: HWND;
    fCurrentString: string;
    fOptions: TVoiceOptions;
    fEnabled: Boolean;
    fInstalled: Boolean;
    procedure Cue(const s: string);
    procedure WndProc(var Message: TMessage);
    procedure DirectSpeak(const s: string);
    procedure TimedSpeak;
  private
    procedure Speak(const s: string; immediate: Boolean); overload;
    procedure Speak(aOption: TVoiceOption; immediate: Boolean); overload;
    procedure Speak(aOption: TVoiceOption; const customtext: string; immediate: Boolean); overload;
  public
    constructor Create(const aOptions: TVoiceOptions; aEnabled: Boolean);
    destructor Destroy; override;
    property Installed: Boolean read fInstalled;
    property Enabled: Boolean read fEnabled write fEnabled;
    property Options: TVoiceOptions read fOptions write fOptions;
  end;

procedure Speak(const s: string; immediate: Boolean); overload;
procedure Speak(aOption: TVoiceOption; immediate: Boolean); overload;
procedure Speak(aOption: TVoiceOption; const customtext: string; immediate: Boolean); overload;

procedure ForcedSpeak(aOption: TVoiceOption; const customtext: string = string.Empty);

implementation

{ TVoiceMgr }

constructor TVoiceMgr.Create(const aOptions: TVoiceOptions; aEnabled: Boolean);
begin
  Inst := Self;

  fInstalled := True;
  try
    fOptions := aOptions;
    fVoice := CoSpVoice.Create;
    fHandle := AllocateHWnd(WndProc);
    fEnabled := aEnabled;
  except
    fInstalled := False;
    fVoice := nil;
    DeallocateHWnd(fHandle);
    fHandle := 0;
  end;
end;

destructor TVoiceMgr.Destroy;
begin
  KillTimer(fHandle, TIMER_ID);
  if fHandle <> 0 then begin
    DeallocateHWnd(fHandle);
    fHandle := 0;
  end;
  Inst := nil;
  inherited;
end;

procedure TVoiceMgr.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_TIMER then begin
    KillTimer(fHandle, TIMER_ID);
    TimedSpeak;
    fCurrentString := string.Empty;
  end
  else
    Message.Result := DefWindowProc(fHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TVoiceMgr.DirectSpeak(const s: string);
begin
  if not s.IsEmpty then
    fVoice.Speak(s, SVSFlagsAsync or SVSFPurgeBeforeSpeak); // async + restart if new string
end;

procedure TVoiceMgr.TimedSpeak;
begin
  fVoice.Speak(fCurrentString, SVSFlagsAsync or SVSFPurgeBeforeSpeak); // async + restart if new string
end;

procedure TVoiceMgr.Cue(const s: string);
begin
  if s.IsEmpty then
    Exit;
  if fCurrentString = s then
    Exit;
  fCurrentString := s;
  KillTimer(fHandle, TIMER_ID);
  SetTimer(fHandle, TIMER_ID, 500, nil);
end;

procedure TVoiceMgr.Speak(const s: string; immediate: Boolean);
begin
  if Assigned(fVoice) and fEnabled then begin
    if immediate then
      DirectSpeak(s)
    else
      Cue(s);
  end;
end;

procedure TVoiceMgr.Speak(aOption: TVoiceOption; immediate: Boolean);
begin
  if aOption in fOptions then
    Self.Speak(gt.VoiceStrings[aOption], immediate);
end;

procedure TVoiceMgr.Speak(aOption: TVoiceOption; const customtext: string; immediate: Boolean);
begin
  if aOption in fOptions then
    Self.Speak(customtext, immediate);
end;

// procedures

procedure Speak(const s: string; immediate: Boolean); overload;
begin
  if Assigned(TVoiceMgr.Inst) then
    TVoiceMgr.Inst.Speak(s, immediate);
end;

procedure Speak(aOption: TVoiceOption; immediate: Boolean); overload;
begin
  if Assigned(TVoiceMgr.Inst) then
    TVoiceMgr.Inst.Speak(aOption, immediate);
end;

procedure Speak(aOption: TVoiceOption; const customtext: string; immediate: Boolean); overload;
begin
  if Assigned(TVoiceMgr.Inst) then
    TVoiceMgr.Inst.Speak(aOption, customtext, immediate);
end;

procedure ForcedSpeak(aOption: TVoiceOption; const customtext: string = string.Empty);
var
  oldOptions: TVoiceOptions;
  oldEnabled: Boolean;
begin
  if Assigned(TVoiceMgr.Inst) then begin
    oldOptions := TVoiceMgr.Inst.fOptions;
    oldEnabled := TVoiceMgr.Inst.fEnabled;
    Include(TVoiceMgr.Inst.fOptions, aOption);
    TVoiceMgr.Inst.fEnabled := True;

    if not customtext.IsEmpty then
      TVoiceMgr.Inst.Speak(aOption, customtext, True)
    else
      TVoiceMgr.Inst.Speak(aOption, True);

    TVoiceMgr.Inst.fOptions := oldOptions;
    TVoiceMgr.Inst.fEnabled := oldEnabled;
  end;
end;

end.
