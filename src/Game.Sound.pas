unit Game.Sound;

{$include lem_directives.inc}

interface

uses
vcl.dialogs,
  Winapi.Windows, Winapi.MMSystem,
  System.Classes, System.Contnrs, System.SysUtils, System.Math, System.IOUtils,
  Base.Utils, Base.Types,
  Dos.Consts,
  Prog.Base, Prog.Data,
  BASS;

type
  TSound = class;
  TMusic = class;

  SoundLibrary = class
  strict private
    class var fLibEnabled : Boolean;
  private
  // sound handles
    class function CreateSoundHandle(aSound: TSound): DWORD; static;
    class procedure FreeSoundHandle(aSound: TSound); static;
  // music handles
    class function CreateMusicHandle(aMusic: TMusic; aType: TMusicStreamType): DWORD; static;
    class procedure FreeMusicHandle(aMusic: TMusic); static;
   // play
    class procedure PlaySound(aSound: TSound); static;
    class procedure StopSound(aSound: TSound); static;
    class procedure PlayMusic(aMusic: TMusic); static;
    class procedure StopMusic(aMusic: TMusic); static;
    class procedure SetMusicVolume(aMusic: TMusic; aVolume: Single); static;
    class procedure SetSoundVolume(aSound: TSound; aVolume: Single); static;
  public
    class procedure Init; static; // must be called at startup
    class procedure Done; static;
    class property LibEnabled: Boolean read fLibEnabled;
  end;

  TAbstractSound = class abstract
  strict protected
    fDataPtr  : Pointer; // owned by class
    fDataSize : Integer;
    fHandle   : DWORD; // owned by soundlibrary (BASS)
  public
  end;

  TSound = class sealed(TAbstractSound)
  private
    fVolume: Single;
    // fIsPlaying: Boolean;
    procedure SetVolume(aValue: Single);
  public
    constructor Create(const aFileName: string; const aVolume: Single = 1.0; disk: Boolean = False);
    destructor Destroy; override;
    procedure Play; inline;
    // procedure Stop; inline;
    property Volume: Single read fVolume write SetVolume;
    property DataPtr: Pointer read fDataPtr;
    property DataSize: Integer read fDataSize;
    property Handle: DWORD read fHandle;
  end;

  TMusic = class sealed(TAbstractSound)
  private
    fVolume: Single;
    fStreamType: TMusicStreamType;
    fIsPlaying: Boolean;
    procedure SetVolume(aValue: Single); inline;
  public
    constructor Create(const aFileName: string; aType: TMusicStreamType; aVolume: Single = 0.2);
    destructor Destroy; override;
    procedure Play; inline;
    procedure Stop; inline;
    property Volume: Single read fVolume write SetVolume;
    property DataPtr: Pointer read fDataPtr;
    property DataSize: Integer read fDataSize;
    property Handle: DWORD read fHandle;
    property IsPlaying: Boolean read fIsPlaying;
  end;

  TSoundList = class(TFastObjectList<TSound>);
  TMusicList = class(TFastObjectList<TMusic>);

  // we create, manage and play sounds from here (by index)
  TSoundMgr = class sealed
  private
    fSounds  : TSoundList;
    fMusics  : TMusicList;
    function GetMusicIsPlaying(index: Integer): Boolean; inline;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  // sounds
    function AddSoundFromFileName(const aFileName: string; const aVolume: Single = 1.0; disk: Boolean = False): Integer;
    procedure ClearSounds;
    procedure PlaySound(index: Integer);
    procedure StopSound(index: Integer);
    // procedure StopSounds;
  // musics
    function AddMusicFromFileName(const aFileName: string; aType: TMusicStreamType; aVolume: Single = 0.2): Integer;
    procedure ClearMusics;
    procedure PlayMusic(index: Integer);
    procedure StopMusic(index: Integer);
    function GetMusicVolumne(index: Integer): Single;
    procedure SetMusicVolume(index: Integer; const aVolume: Single);
    property MusicIsPlaying[index: Integer]: Boolean read GetMusicIsPlaying;
  end;

  SoundData = class sealed
  public
  class var
    SFX_BUILDER_WARNING  : Integer;
    SFX_ASSIGN_SKILL     : Integer;
    SFX_YIPPEE           : Integer;
    SFX_SPLAT            : Integer;
    SFX_LETSGO           : Integer;
    SFX_ENTRANCE         : Integer;
    SFX_VAPORIZING       : Integer;
    SFX_DROWNING         : Integer;
    SFX_EXPLOSION        : Integer;
    SFX_HITS_STEEL       : Integer;
    SFX_OHNO             : Integer;
    SFX_SKILLBUTTON      : Integer;
    SFX_ROPETRAP         : Integer;
    SFX_TENTON           : Integer;
    SFX_BEARTRAP         : Integer;
    SFX_ELECTROTRAP      : Integer;
    SFX_SPINNINGTRAP     : Integer;
    SFX_SQUISHINGTRAP    : Integer;
    SFX_MINER            : Integer;
    SFX_DIGGER           : Integer;
    SFX_BASHER           : Integer;
    SFX_FlOATER          : Integer;
    SFX_OPENUMBRELLA     : Integer;
    SFX_SILENTDEATH      : Integer;
    SFX_NUKE             : Integer;
  public
    class constructor Create;
    class procedure Init(aSoundMgr: TSoundMgr; const aPathToCustomSounds: string); static;
  end;

implementation

{ SoundLibrary }

class procedure SoundLibrary.Init;
begin
  // check version
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
    Exit;
  // Initialize audio: default device, 44100hz, stereo, 16 bits
  fLibEnabled := BASS_Init(-1, 44100, 0, 0, nil);
end;

class procedure SoundLibrary.Done;
begin
  if fLibEnabled then
    BASS_Free;
end;

class function SoundLibrary.CreateSoundHandle(aSound: TSound): DWORD;
// bass load from memory pointer. Note: bass owns the handle, TSound owns the memory
begin
  if not fLibEnabled or (aSound.DataPtr = nil) or (aSound.DataSize = 0) then
    Exit(0);
	Result := BASS_StreamCreateFile(True, aSound.DataPtr, 0, aSound.DataSize, BASS_UNICODE);
end;

class procedure SoundLibrary.FreeSoundHandle(aSound: TSound);
begin
  if not fLibEnabled or (aSound.Handle = 0) then
    Exit;
  BASS_StreamFree(aSound.Handle);
end;

class function SoundLibrary.CreateMusicHandle(aMusic: TMusic; aType: TMusicStreamType): DWORD;
// bass load from memory pointer. Note: bass owns the handle, TMusic owns the memory
begin
  Result := 0;
  if not fLibEnabled or (aMusic.DataPtr = nil) or (aMusic.DataSize = 0) then
    Exit;
  case aType of
    TMusicStreamType.&MOD:
     	Result := BASS_MusicLoad(True, aMusic.DataPtr, 0, aMusic.DataSize, BASS_MUSIC_RAMP or BASS_MUSIC_LOOP or BASS_MUSIC_SURROUND or BASS_UNICODE, 1);
     TMusicStreamType.MP3:
     	Result := BASS_StreamCreateFile(True, aMusic.DataPtr, 0, aMusic.DataSize, BASS_UNICODE);//BASS_MUSIC_RAMP or BASS_MUSIC_LOOP or BASS_MUSIC_SURROUND or BASS_UNICODE);
  end;
end;

class procedure SoundLibrary.FreeMusicHandle(aMusic: TMusic);
begin
  if not fLibEnabled or (aMusic.Handle = 0) then
    Exit;
  BASS_MusicFree(aMusic.Handle);

end;

class procedure SoundLibrary.PlaySound(aSound: TSound);
begin
  if (aSound.DataPtr = nil) or (aSound.DataSize <= 0) then
    Exit;
  if fLibEnabled then begin
    // overlapping sounds
    if (BASS_ChannelIsActive(aSound.Handle) = BASS_ACTIVE_PLAYING)
    and (BASS_ChannelGetPosition(aSound.Handle, BASS_POS_BYTE) >= BASS_ChannelSeconds2Bytes(aSound.Handle, 0.1)) then begin
      var tmpHandle: DWORD := BASS_StreamCreateFile(True, aSound.DataPtr, 0, aSound.DataSize, BASS_UNICODE or BASS_STREAM_AUTOFREE);
      BASS_ChannelSetAttribute(tmpHandle, BASS_ATTRIB_VOL,aSound.Volume);
    	BASS_ChannelPlay(tmpHandle, True);
      Exit;
    end;

  	BASS_ChannelPlay(aSound.Handle, True);
  end
  else
    Winapi.MMSystem.PlaySound(aSound.DataPtr, 0, SND_ASYNC or SND_MEMORY); // fallback for sounds on the winapi
end;

class procedure SoundLibrary.StopSound(aSound: TSound);
begin
  if (aSound.DataPtr = nil) or (aSound.DataSize <= 0) then
    Exit;
  if fLibEnabled then
  	BASS_ChannelStop(aSound.Handle)
  else
    Winapi.MMSystem.PlaySound(nil, 0, 0); // fallback for sounds on the winapi
end;

class procedure SoundLibrary.PlayMusic(aMusic: TMusic);
begin
  if fLibEnabled and (aMusic.DataPtr <> nil) and (aMusic.DataSize > 0) then begin
  	BASS_ChannelPlay(aMusic.Handle, True);
    BASS_ChannelSetAttribute(aMusic.Handle, BASS_ATTRIB_VOL, aMusic.Volume);
  end;
end;

class procedure SoundLibrary.StopMusic(aMusic: TMusic);
begin
  if fLibEnabled and (aMusic.DataPtr <> nil) and (aMusic.DataSize > 0) then begin
    BASS_ChannelStop(aMusic.Handle); // todo: try sliding down
  end;
end;

class procedure SoundLibrary.SetMusicVolume(aMusic: TMusic; aVolume: Single);
begin
  EnsureRange(aVolume, 0.0, 1.0);
  BASS_ChannelSetAttribute(aMusic.Handle, BASS_ATTRIB_VOL, aVolume);
end;

class procedure SoundLibrary.SetSoundVolume(aSound: TSound; aVolume: Single);
begin
  EnsureRange(aVolume, 0.0, 1.0);
  BASS_ChannelSetAttribute(aSound.Handle, BASS_ATTRIB_VOL, aVolume);
end;

{ TSound }

constructor TSound.Create(const aFileName: string; const aVolume: Single = 1.0; disk: Boolean = False);
begin
  inherited Create;
  fDataPtr := TData.CreatePointer(Consts.StyleName, aFileName, TDataType.Sound, {out}fDataSize, False, disk);
  fHandle := SoundLibrary.CreateSoundHandle(Self);
  fVolume := aVolume;
  SoundLibrary.SetSoundVolume(Self, fVolume);
end;

destructor TSound.Destroy;
begin
  SoundLibrary.FreeSoundHandle(Self);
  if Assigned(fDataPtr) then
    FreeMem(fDataPtr);
  inherited;
end;

procedure TSound.Play;
begin
  SoundLibrary.PlaySound(Self);
  //fIsPlaying := True;
end;

//procedure TSound.Stop;
//begin
//  if fIsPlaying then begin
//    SoundLibrary.StopSound(Self);
//    fIsPLaying := False;
//  end;
//end;

procedure TSound.SetVolume(aValue: Single);
begin
  Restrict(aValue, 0.0, 1.0);
  if fVolume = aValue then
    Exit;
  fVolume := aValue;
  SoundLibrary.SetSoundVolume(Self, fVolume);
end;

{ TMusic }

constructor TMusic.Create(const aFileName: string; aType: TMusicStreamType; aVolume: Single = 0.2);
begin
  inherited Create;
  fStreamType := aType;
  fDataPtr := TData.CreatePointer(Consts.StyleName, aFileName, TDataType.Music, {out} fDataSize, True); // do not cache these
  fHandle := SoundLibrary.CreateMusicHandle(Self, fStreamType);
  fVolume := aVolume;
end;

destructor TMusic.Destroy;
begin
  SoundLibrary.FreeMusicHandle(Self);
  if Assigned(fDataPtr) then
    FreeMem(fDataPtr);
  inherited;
end;

procedure TMusic.Play;
begin
  SoundLibrary.PlayMusic(Self);
  fIsPlaying := True;
end;

procedure TMusic.Stop;
begin
  SoundLibrary.StopMusic(Self);
  fIsPlaying := False;
end;

procedure TMusic.SetVolume(aValue: Single);
begin
  Restrict(aValue, 0.0, 1.0);
  if fVolume = aValue then
    Exit;
  fVolume := aValue;
  SoundLibrary.SetMusicVolume(Self, fVolume);
end;

{ TSoundMgr }

constructor TSoundMgr.Create;
begin
  inherited Create;
  fSounds := TSoundList.Create;
  fMusics := TMusicList.Create;
end;

destructor TSoundMgr.Destroy;
begin
  fSounds.Free;
  fMusics.Free;
  inherited Destroy;
end;

function TSoundMgr.AddSoundFromFileName(const aFileName: string; const aVolume: Single = 1.0; disk: Boolean = False): Integer;
begin
  var snd := TSound.Create(aFileName, aVolume, disk);
  Result := fSounds.Add(snd);
end;

function TSoundMgr.GetMusicIsPlaying(index: Integer): Boolean;
begin
  Result := fMusics.ValidIndex(index) and fMusics[index].IsPlaying;
end;

procedure TSoundMgr.PlaySound(index: Integer);
begin
  if fSounds.ValidIndex(index) then
    fSounds[index].Play;
end;

procedure TSoundMgr.StopSound(Index: Integer);
begin
  if fSounds.ValidIndex(index) then
    SoundLibrary.StopSound(fSounds[index]);
end;

//procedure TSoundMgr.StopSounds;
//begin
//  for var s: TSound in fSounds do
//    s.Stop;
//end;

procedure TSoundMgr.PlayMusic(index: Integer);
begin
  if fMusics.ValidIndex(index) then
    fMusics[index].Play;
end;

function TSoundMgr.GetMusicVolumne(index: Integer): Single;
begin
  if fMusics.ValidIndex(index) then
    Result := fMusics[index].Volume
  else
    Result := 0.0;
end;

procedure TSoundMgr.SetMusicVolume(index: Integer; const aVolume: Single);
begin
  if fMusics.ValidIndex(index) then
    fMusics[index].Volume := aVolume;
end;

procedure TSoundMgr.StopMusic(index: Integer);
begin
  if fMusics.ValidIndex(index) then
    fMusics[index].Stop;
end;

function TSoundMgr.AddMusicFromFileName(const aFileName: string; aType: TMusicStreamType; aVolume: Single = 0.2): Integer;
var
  M: TMusic;
begin
  M := TMusic.Create(aFileName, aType, aVolume);
  Result := fMusics.Add(M);
end;

procedure TSoundMgr.ClearMusics;
begin
  fMusics.Clear;
end;


procedure TSoundMgr.ClearSounds;
begin
  fSounds.Clear;
end;

{ TSoundData }

class constructor SoundData.Create;
begin
  SFX_BUILDER_WARNING  := -1;
  SFX_ASSIGN_SKILL     := -1;
  SFX_YIPPEE           := -1;
  SFX_SPLAT            := -1;
  SFX_LETSGO           := -1;
  SFX_ENTRANCE         := -1;
  SFX_VAPORIZING       := -1;
  SFX_DROWNING         := -1;
  SFX_EXPLOSION        := -1;
  SFX_HITS_STEEL       := -1;
  SFX_OHNO             := -1;
  SFX_SKILLBUTTON      := -1;
  SFX_ROPETRAP         := -1;
  SFX_TENTON           := -1;
  SFX_BEARTRAP         := -1;
  SFX_ELECTROTRAP      := -1;
  SFX_SPINNINGTRAP     := -1;
  SFX_SQUISHINGTRAP    := -1;
  SFX_MINER            := -1;
  SFX_DIGGER           := -1;
  SFX_BASHER           := -1;
  SFX_OPENUMBRELLA     := -1;
  SFX_SILENTDEATH      := -1;
end;

class procedure SoundData.Init(aSoundMgr: TSoundMgr; const aPathToCustomSounds: string);

var
  useCustom: Boolean;
  defaultNames: TStringList;
  customFileNames: TStringList;

    function Add(effect: TSoundEffect; const aVolume: Single = 1.0): Integer;
    const
      defaultExt = '.wav';
    var
      defaultfilename: string;
    begin
      Result := -1;
      defaultfilename := effect.AsFileName(defaultExt);
      // default resource sounds only
      if not useCustom then begin
        if not effect.IsCustom then
          Result := aSoundMgr.AddSoundFromFileName(defaultfilename, aVolume);
      end
      // check overwritten or custom or default
      else begin
        var mp3: string := ReplaceFileExt(defaultFilename, '.mp3');
        // check mp3
        if customFileNames.IndexOf(mp3) >= 0 then begin
          Result := aSoundMgr.AddSoundFromFileName(aPathToCustomSounds + mp3, aVolume, True);
        end
        // check wav
        else if customFileNames.IndexOf(defaultfilename) >= 0 then begin
          Result := aSoundMgr.AddSoundFromFileName(aPathToCustomSounds + defaultFilename, aVolume, True);
        end
        // else default
        else begin
          if not effect.IsCustom then
            Result := aSoundMgr.AddSoundFromFileName(defaultfilename, aVolume);
        end;
      end;
    end;


const
  ext = '.wav';
begin
  useCustom := not aPathToCustomSounds.IsEmpty and TDirectory.Exists(aPathToCustomSounds);
  defaultNames := TStringList.Create;
  customFileNames := TStringList.Create;
  try

    for var snd: TSoundEffect := Succ(Low(TSoundEffect)) to High(TSoundEffect) do
      defaultNames.Add(snd.AsFileName(string.empty).ToUpper);
    defaultNames.Sort;
    defaultNames.CaseSensitive := False;

    if useCustom then begin
      var files: TArray<string> := nil;
      var filter: TDirectory.TFilterPredicate :=
        function (const Path: string; const SearchRec: TSearchRec): Boolean
        begin
          var ext: string := ExtractFileExt(SearchRec.Name).ToUpper;
          var name: string := ReplaceFileExt(ExtractFileName(SearchRec.Name), string.empty).ToUpper;
          Result := (SearchRec.Size < 256 * 1024) and ((ext = '.WAV') or (ext = '.MP3')) and (defaultNames.IndexOf(name) >= 0);
        end;
      files := TDirectory.GetFiles(aPathToCustomSounds, '*.*', filter);
      for var s: string in files do
        customFileNames.Add(ExtractFileName(s));
      customFileNames.Sort;
      customFileNames.CaseSensitive := False;
    end;

    // initialize sounds
    SFX_BUILDER_WARNING := Add(TSoundEffect.BuilderWarning);
    SFX_ASSIGN_SKILL    := Add(TSoundEffect.AssignSkill);
    SFX_YIPPEE          := Add(TSoundEffect.Yippee);
    SFX_SPLAT           := Add(TSoundEffect.Splat);
    SFX_LETSGO          := Add(TSoundEffect.LetsGo);
    SFX_ENTRANCE        := Add(TSoundEffect.EntranceOpening);
    SFX_VAPORIZING      := Add(TSoundEffect.Vaporizing);
    SFX_DROWNING        := Add(TSoundEffect.Drowning);
    SFX_EXPLOSION       := Add(TSoundEffect.Explosion);
    SFX_HITS_STEEL      := Add(TSoundEffect.HitsSteel);
    SFX_OHNO            := Add(TSoundEffect.Ohno);
    SFX_SKILLBUTTON     := Add(TSoundEffect.SkillButtonSelect);
    SFX_ROPETRAP        := Add(TSoundEffect.RopeTrap);
    SFX_TENTON          := Add(TSoundEffect.TenTonTrap);
    SFX_BEARTRAP        := Add(TSoundEffect.BearTrap);
    SFX_ELECTROTRAP     := Add(TSoundEffect.ElectroTrap);
    SFX_SPINNINGTRAP    := Add(TSoundEffect.SpinningTrap);
    SFX_SQUISHINGTRAP   := Add(TSoundEffect.SquishingTrap);

    // custom
    SFX_MINER           := Add(TSoundEffect.Miner, 0.2);
    SFX_DIGGER          := Add(TSoundEffect.Digger, 0.2);
    SFX_BASHER          := Add(TSoundEffect.Basher, 0.2);
    SFX_OPENUMBRELLA    := Add(TSoundEffect.OpenUmbrella, 0.2);
    SFX_SILENTDEATH     := Add(TSoundEffect.SilentDeath, 0.2);
    SFX_NUKE            := Add(TSoundEffect.Nuke);
  finally
    defaultNames.Free;
    customFileNames.Free;
  end;

end;

end.


