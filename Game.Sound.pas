unit Game.Sound;

{$include lem_directives.inc}

interface

uses
vcl.dialogs,
  Winapi.Windows, Winapi.MMSystem,
  System.Classes, System.Contnrs, System.SysUtils, System.Math,
  Base.Utils,
  Prog.Types, Prog.Base, Prog.Data,
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
    // class procedure StopSound(aSound: TSound); static; not (yet) used so commented out interface and implementation
    class procedure PlayMusic(aMusic: TMusic); static;
    class procedure StopMusic(aMusic: TMusic); static;
    class procedure SetMusicVolume(aMusic: TMusic; aVolume: Single); static;
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
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    procedure Play; inline;
    property DataPtr: Pointer read fDataPtr;
    property DataSize: Integer read fDataSize;
    property Handle: DWORD read fHandle;
  end;

  TMusic = class(TAbstractSound)
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
  TSoundMgr = class
  private
    fSounds  : TSoundList;
    fMusics  : TMusicList;
    function GetMusicIsPlaying(index: Integer): Boolean; inline;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  // sounds
    function AddSoundFromFileName(const aFileName: string): Integer;
    procedure ClearSounds;
    procedure PlaySound(index: Integer);
    procedure StopSound(index: Integer);
  // musics
    function AddMusicFromFileName(const aFileName: string; aType: TMusicStreamType; aVolume: Single = 0.2): Integer;
    procedure ClearMusics;
    procedure PlayMusic(index: Integer);
    procedure StopMusic(index: Integer);
    function GetMusicVolumne(index: Integer): Single;
    procedure SetMusicVolume(index: Integer; const aVolume: Single);
    property MusicIsPlaying[index: Integer]: Boolean read GetMusicIsPlaying;
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
    	BASS_ChannelPlay(tmpHandle, True);
      Exit;
    end;

  	BASS_ChannelPlay(aSound.Handle, True);
  end
  else
    Winapi.MMSystem.PlaySound(aSound.DataPtr, 0, SND_ASYNC or SND_MEMORY); // fallback for sounds on the winapi
end;

//class procedure SoundLibrary.StopSound(aSound: TSound);
//begin
//  if (aSound.DataPtr = nil) or (aSound.DataSize <= 0) then
//    Exit;
//  if fEnabled then
//  	BASS_ChannelStop(aSound.Handle)
//  else
//    Winapi.MMSystem.PlaySound(nil, 0, 0); // fallback for sounds on the winapi
//end;

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

{ TSound }

constructor TSound.Create(const aFileName: string);
begin
  inherited Create;
  fDataPtr := TData.CreatePointer(Consts.StyleName, aFileName, TDataType.Sound, {out}fDataSize);
  fHandle := SoundLibrary.CreateSoundHandle(Self);
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

function TSoundMgr.AddSoundFromFileName(const aFileName: string): Integer;
begin
  var snd := TSound.Create(aFileName);
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
  if fMusics.ValidIndex(index) then
    SoundLibrary.StopMusic(fMusics[index]);
end;

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

end.


