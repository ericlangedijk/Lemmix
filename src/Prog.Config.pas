unit Prog.Config;

{$include lem_directives.inc}

interface

uses
  System.Classes, System.SysUtils,
  Base.Utils, Base.Types,
  Prog.Base;


type
  TConfig = record
  public
  // options
    FormOptions       : TFormOptions;
    GameOptions       : TGameOptions;
    OptionalMechanics : TOptionalMechanics;
    SoundOptions      : TSoundOptions;
    VoiceOptions      : TVoiceOptions;
    MiscOptions       : TMiscOptions;
  // other
    StyleName         : string;
    PathToStyles      : string;
    PathToMusic       : string;
    PathToSounds      : string;
    PathToReplay      : string;
    Language          : string;
    ZoomFactor        : Integer;
    Monitor           : Integer;
    procedure Load;
    procedure Save;
    function LanguageIsDefault: Boolean;
  end;

implementation

procedure TConfig.Load;
var
  list: TStringList;
  filename, value: string;
  v: Integer;

begin
  // set defaults
  FormOptions       := TFormOptions.DEFAULT;
  GameOptions       := TGameOptions.DEFAULT;
  SoundOptions      := TSoundOptions.DEFAULT;
  OptionalMechanics := TOptionalMechanics.DEFAULT;
  VoiceOptions      := TVoiceOptions.DEFAULT;
  MiscOptions       := TMiscOptions.DEFAULT;

  ZoomFactor := 0;
  StyleName := 'Orig';
  Monitor := 0;

  filename := ReplaceFileExt(Consts.AppName, '.config');

  // keep defaults if no configfile
  if not FileExists(filename) then
    Exit;

  list := TStringList.Create;
  try
    list.LoadFromFile(filename);
    // form options
    for var form: TFormOption in TFormOptions.ALL do begin
      value := list.Values['Forms.' + form.AsString].Trim;
      if value.Length = 1 then
        FormOptions.&Set(form, value.StartsWith('1'));
    end;
    // game options
    for var game: TGameOption in TGameOptions.ALL do begin
      value := list.Values['Game.' + game.AsString].Trim;
      if value.Length = 1 then
        GameOptions.&Set(game, value.StartsWith('1'));
    end;
    // optional mechanics
    for var mech: TOptionalMechanic in TOptionalMechanics.ALL do begin
      value := list.Values['Mechanics.' + mech.AsString].Trim;
      if value.Length = 1 then
        OptionalMechanics.&Set(mech, value.StartsWith('1'));
    end;
    // sound options
    for var snd: TSoundOption in TSoundOptions.ALL do begin
      value := list.Values['Sounds.' + snd.AsString].Trim;
      if value.Length = 1 then
        SoundOptions.&Set(snd, value.StartsWith('1'));
    end;
    // voice options
    for var voice: TVoiceOption in TVoiceOptions.ALL do begin
      value := list.Values['Voice.' + voice.AsString].Trim;
      if value.Length = 1 then
        VoiceOptions.&Set(voice, value.StartsWith('1'))
    end;
    // misc options
    for var misc: TMiscOption in TMiscOptions.ALL do begin
      value := list.Values['Misc.' + misc.AsString].Trim;
      if value.Length = 1 then
        MiscOptions.&Set(misc, value.StartsWith('1'));
    end;
    // zoom
    value := list.Values['Display.ZoomFactor'];
    if TryStrToInt(Value, v) and (v >= 0) and (v < 16) then
      ZoomFactor := v;
    // monitor
    value := list.Values['Display.Monitor'];
    if TryStrToInt(value, v) then
      Monitor := v;
    // style
    value := list.Values['Style'];
    if not value.IsEmpty then
      StyleName := value;
    // path to styles
    value := list.Values['PathToStyles'];
    if not value.IsEmpty then
      PathToStyles := value;
    // path to music
    value := list.Values['PathToMusic'];
    if not value.IsEmpty then
      PathToMusic := value;
    // path to sounds
    value := list.Values['PathToSounds'];
    if not value.IsEmpty then
      PathToSounds := value;
    // path to replay
    value := list.Values['PathToReplay'];
    if not value.IsEmpty then
      PathToReplay := value;
    // path to replay
    value := list.Values['Language'];
    if not value.IsEmpty then
      Language := value;
  finally
    list.Free;
  end;
end;

procedure TConfig.Save;
var
  list: TStringList;
  filename: string;

    function BoolEntry(const bool: Boolean): string; inline;
    begin
      if bool then Result := '1' else Result := '0';
    end;

begin
  filename := ReplaceFileExt(Consts.AppName, '.config');
  list := TStringList.Create;
  try
    // form options
    for var form: TFormOption in TFormOptions.ALL do
      list.Values['Forms.' + form.AsString] := BoolEntry(form in FormOptions);
    // game options
    for var game: TGameOption in TGameOptions.ALL do
      list.Values['Game.' + game.AsString] := BoolEntry(game in GameOptions);
    // optional mechanics
    for var mech: TOptionalMechanic in TOptionalMechanics.ALL do
      list.Values['Mechanics.' + mech.AsString] := BoolEntry(mech in OptionalMechanics);
    // sound options
    for var snd: TSoundOption in TSoundOptions.ALL do
      list.Values['Sounds.' + snd.AsString] := BoolEntry(snd in SoundOptions);
    // voice options
    for var voice: TVoiceOption in TVoiceOptions.ALL do
      list.Values['Voice.' + voice.AsString] := BoolEntry(voice in VoiceOptions);
    // misc options
    for var misc: TMiscOption in TMiscOptions.ALL do
      list.Values['Misc.' + misc.AsString] := BoolEntry(misc in MiscOptions);
    // display
    list.Values['Display.Monitor'] := Monitor.ToString;
    list.Values['Display.ZoomFactor'] := ZoomFactor.ToString;
    // style
    list.Values['Style'] := StyleName;
    // path
    if not PathToStyles.IsEmpty then
      list.Values['PathToStyles'] := PathToStyles;
    // path
    if not PathToStyles.IsEmpty then
      list.Values['PathToMusic'] := PathToMusic;
    // path
    if not PathToSounds.IsEmpty then
      list.Values['PathToSounds'] := PathToSounds;
    // path
    if not PathToReplay.IsEmpty then
      list.Values['PathToReplay'] := PathToReplay;
    // language
    if not Language.IsEmpty then
      list.Values['Language'] := Language;
    list.Sort;
    list.SaveToFile(filename);
  finally
    list.Free;
  end;
end;

function TConfig.LanguageIsDefault: Boolean;
begin
  Result := Language.IsEmpty or SameText(Language, 'Default');
end;

end.

