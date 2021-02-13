// WARNING: do not perform 'class completion' on the Consts static class. It will produce nonsense.

unit Prog.Base;

{$include lem_directives.inc}

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.Contnrs,
  Dos.Consts,
  Base.Utils, Base.Types;

// levelmapping can be set in style.config file
type
  TLevelGraphicsMapping = (
    Default,      // mapping as is in the folder, including all .dat files   "DEFAULT" or empty
    Orig,         // levels use Orig graphics                                "ORIG"
    Ohno,         // levels use Ohno graphics                                "OHNO"
    Concat        // levels are remapped to ORIG+OHNO concatenated           "CONCAT"
  );

type
  TLevelSpecialGraphicsMapping = (
    Default,      // mapping as is in the folder
    Orig
  );

type
  Consts = class sealed
  public
    class var GLOBAL_DEVELOPER_MODUS : Boolean;
      // god modus
      // • Create lemmings on the fly at cursor (ctrl + alt + left mousebutton)
      // • Set all skills to 99 (ctrl + alt + right mousebutton)
      // • Use in-game debuglayer for game details (ctrl + shift + F12)
      // • save binary levels voor A.I. engine

    type
      TVersionRecord = record
        Major: Integer;
        Minor: Integer;
        Release: Integer;
        Build: Integer;
      end;

    type
      TStyleInformation = class
      private
        fStyleDef: TStyleDef;
        fFamily: TStyleFamily;
        fFullPath: string; // full path name to style
        fName: string; // must be same as folder name
        fDescription: string; // style description
        fInfo: string; // a longer line to give some info
        fAuthor: string;
        fUserMechanics: TMechanics; // only used in user styles (config)
        fUserGraphicsMapping: TLevelGraphicsMapping; // only used in user styles (config)
        fUserSpecialGraphicsMapping: TLevelSpecialGraphicsMapping; // only used in user styles (config)
        fMaindatOhNo: Boolean;
      public
        property StyleDef: TStyleDef read fStyleDef;
        property Family: TStyleFamily read fFamily;
        property FullPath: string read fFullPath;
        property Description: string read fDescription;
        property Name: string read fName;
        property Info: string read fInfo;
        property Author: string read fAuthor;
        property UserMechanics: TMechanics read fUserMechanics;
        property UserGraphicsMapping: TLevelGraphicsMapping read fUserGraphicsMapping;
        property UserSpecialGraphicsMapping: TLevelSpecialGraphicsMapping read fUserSpecialGraphicsMapping;
        property MaindatOhNo: Boolean read fMaindatOhNo;
      end;

    TStyleInformationList = class(TFastObjectList<TStyleInformation>);

  strict private
    const fGraphicSetNames: array[0..8] of string = ('Dirt', 'Fire', 'Marble', 'Pillar', 'Crystal', 'Brick', 'Rock', 'Snow', 'Bubble');
  // logos for options screen
    const fFilenameLogo_orig  = 'logo_orig.png';
    const fFilenameLogo_ohno  = 'logo_ohno.png';
    const fFilenameLogo_h94   = 'logo_h94.png';
    const fFilenameLogo_x91   = 'logo_x91.png';
    const fFilenameLogo_x92   = 'logo_x92.png';
    const fFilenamesLogo      : array[TStyleDef] of string = (fFilenameLogo_orig, fFilenameLogo_ohno, fFilenameLogo_h94, fFilenameLogo_x91, fFilenameLogo_x92, fFilenameLogo_orig);
  // const resource names
    const fResNameZippedLogos          : string = 'LOGOS';
    const fResNameZippedCursors        : string = 'CURSORS';
    const fResNameZippedSounds         : string = 'SOUNDS';
    const fResNameParticles            : string = 'PARTICLES';
    const fResNameCustom               : string = 'CUSTOM';
    const fResNameAssets               : string = 'ASSETS';
  // cursor
    const fFilenameCursorDefault       : string = 'CursorDefault.bmp';
    const fFilenameCursorDefaultMask   : string = 'CursorDefaultMask.bmp';
    const fFilenameCursorHighlight     : string = 'CursorHighlight.bmp';
    const fFilenameCursorHighlightMask : string = 'CursorHighlightMask.bmp';
    const fFilenameCursorDrag          : string = 'CursorDrag.bmp';
    const fFilenameCursorDragMask      : string = 'CursorDragMask.bmp';
    const fFilenameCursorHourglass     : string = 'CursorHourglass.bmp';
    const fFilenameCursorHourglassMask : string = 'CursorHourglassMask.bmp';
    const fFilenameParticles           : string = 'Particles.dat';
  // assets
    const fFilenameAssetPillar         : string = 'Pillar.bmp'; // mac bitmap for help screen. black = 0,0,17,255
    const fFilenameAssetPillarTop      : string = 'PillarTop.bmp'; // mac bitmap for help screen. black = 0,0,17,255
    const fFilenameAssetCheckBox       : string = 'Checkbox48.png';
    const fFilenameAssetCheckBoxGray   : string = 'Checkbox48gray.png';
    const fFilenameAssetCheckBoxTransparent  : string = 'checkbox48transparent.png';
    const fFilenameAssetCheckBoxGrayTransparent  : string = 'checkbox48graytransparent.png';
    const fFilenameAssetPlay           : string = 'play48.png';
  // language
    const fFilenameLanguageDutch       : string = 'Dutch.config';
  // internal
    class var fLemmixVersion           : string;
    class var fLemmixVersionRecord     : TVersionRecord;
    class var fIsBeta                  : Boolean;
    class var fFullProgramName         : string;
    class var fCachedAppName           : string;
    class var fCachedAppPath           : string;
    class var fPathToStyles            : string;
    class var fPathToMusic             : string;
    class var fPathToSounds            : string;
    class var fPathToReplay            : string;
    class var fInitialized             : Boolean;
    class var fDefined                 : Boolean;
  // current style
    class var fStyleDef                : TStyleDef; // changing
    class var fStyleName               : string; // changing
    class var fStyleInfo               : TStyleInformation; // changing
  // paths
    class var fSupportedStyleNames     : TArray<string>;
    class var fStyleInformationList    : TStyleInformationList;
  // misc
    class var fChristmasPalette        : Boolean; // changing
  // paths functions
    class function GetAppName: string; static; inline;
    class function GetAppPath: string; static; inline;
    class function GetPathToData: string; static; inline;
    class function GetPathToDebug: string; static; inline;
    class function GetPathToAssets: string; static; inline;
    class function GetPathToCursors: string; static; inline;
    class function GetPathToLanguage: string; static; inline;
    class function GetPathToErrorLogs: string; static; inline;
    class function GetPathToLogos: string; static; inline;
    class function GetPathToParticles: string; static; inline;
    class function GetPathToStyles: string; static; inline;
    class function GetPathToStyle(const aStylename: string): string; static; inline;
    class function GetPathToOutput: string; static; inline;
    class function GetPathToScreenShots: string; static; inline;
    class function GetPathToReplay: string; static; inline;
    class function GetPathToAutoSave: string; static; inline;
    class function GetPathToBin: string; static; inline;
    class function GetPathToTemp: string; static; inline;
    class function GetPathToCache: string; static; inline;
    class function GetPathToLemmings(const aStylename: string): string; static; inline;
    class function GetPathToMusics(const aStylename: string): string; static; inline;
    class function GetPathToSounds: string; static; inline;
    class function GetResourceNameZippedLemmings(const aStylename: string): string; static; inline;
    class function GetResourceNameZippedMusics(const aStylename: string): string; static; inline;
  // filename
    class function GetFilenameLogo(index: TStyleDef): string; static; inline;
  // internal method
    class function FindDefaultStyle(const aName: string; out aStyle: TStyleDef): Boolean; static;
    class procedure Check; static; inline;
  public
    class constructor Create;
    class procedure Init(const aPathToStyles, aPathToMusic, aPathToSounds, aPathToReplay: string); static; // must be called at startup
    class procedure Done; static;
    class procedure SetStyleName(const aName: string); static;
  // styledef
    class property StyleDef: TStyleDef read fStyleDef;
    class property StyleName: string read fStyleName;
    class property StyleInfo: TStyleInformation read fStyleInfo;
  // program
    class property LemmixVersion: string read fLemmixVersion;
    class property LemmixVersionRecord: TVersionRecord read fLemmixVersionRecord;
    class property FullProgramName: string read fFullProgramName;
    class property IsBeta: Boolean read fIsBeta;
  // paths
    class property AppName: string read GetAppName;
    class property AppPath: string read GetAppPath;
    class property PathToDebug: string read GetPathToDebug;
    class property PathToAssets: string read GetPathToAssets;
    class property PathToData: string read GetPathToData;
    class property PathToErrorLogs: string read GetPathToErrorLogs;
    class property PathToLogos: string read GetPathToLogos;
    class property PathToCursors: string read GetPathToCursors;
    class property PathToLanguage: string read GetPathToLanguage;
    class property PathToParticles: string read GetPathToParticles;
    class property PathToSounds: string read GetPathToSounds;
    class property PathToStyle[const aStylename: string]: string read GetPathToStyle;
    class property PathToStyles: string read GetPathToStyles;
    class property PathToOutput: string read GetPathToOutput;
    class property PathToScreenShots: string read GetPathToScreenShots;
    class property PathToReplay: string read GetPathToReplay;
    class property PathToAutoSave: string read GetPathToAutoSave;
    class property PathToBin: string read GetPathToBin;
    class property PathToTemp: string read GetPathToTemp;
    class property PathToCache: string read GetPathToCache;
    class property PathToLemmings[const aStylename: string]: string read GetPathToLemmings;
    class property PathToMusics[const aStylename: string]: string read GetPathToMusics;
  // resource and filenames
    class property ResNameZippedLogos: string read fResNameZippedLogos;
    class property ResNameZippedCursors: string read fResNameZippedCursors;
    class property ResNameZippedSounds: string read fResNameZippedSounds;
    class property ResNameParticles: string read fResNameParticles;
    class property ResNameCustom: string read fResNameCustom;
    class property ResNameAssets: string read fResNameAssets;
  // resource cursors
    class property FilenameCursorDefault: string read fFilenameCursorDefault;
    class property FilenameCursorDefaultMask: string read fFilenameCursorDefaultMask;
    class property FilenameCursorHighlight: string read fFilenameCursorHighlight;
    class property FilenameCursorHighlightMask: string read fFilenameCursorHighlightMask;
    class property FilenameCursorDrag: string read fFilenameCursorDrag;
    class property FilenameCursorDragMask: string read fFilenameCursorDragMask;
    class property FilenameCursorHourglass: string read fFilenameCursorHourglass;
    class property FilenameCursorHourglassMask: string read fFilenameCursorHourglassMask;
  // resource assets
    class property FilenameAssetPillar: string read fFilenameAssetPillar;
    class property FilenameAssetPillarTop: string read fFilenameAssetPillarTop;
    class property FilenameAssetCheckBox: string read fFilenameAssetCheckBox;
    class property FilenameAssetCheckBoxGray: string read fFilenameAssetCheckBoxGray;
    class property FilenameAssetCheckBoxTransparent: string read fFilenameAssetCheckBoxTransparent;
    class property FilenameAssetCheckBoxGrayTransparent: string read fFilenameAssetCheckBoxGrayTransparent;
    class property FilenameAssetPlay: string read fFilenameAssetPlay;
  // language
    class property FilenameLanguageDutch: string read fFilenameLanguageDutch;
  // resource lemming misc
    class property FilenameParticles: string read fFilenameParticles;
    class property ResourceNameZippedLemmings[const aStylename: string]: string read GetResourceNameZippedLemmings;
    class property ResourceNameZippedMusics[const aStylename: string]: string read GetResourceNameZippedMusics;
    class property FilenameLogo[index: TStyleDef]: string read GetFilenameLogo;
  // misc
    class property ChristmasPalette: Boolean read fChristmasPalette;
  // some style stuff
    class function IsUserStyle(const aName: string): Boolean; static;
    class function GetCustomStyleFolders: TArray<string>; static;
    class function FindStyleInfo(const aStyleName: string): TStyleInformation; static;
    class function GraphicSetNameToGraphicSet(const aGraphicSetname: string): Integer; static;
    class property StyleInformationlist: TStyleInformationList read fStyleInformationList;
  end;

implementation

{ Consts }

class procedure Consts.Check;
begin
  {$ifdef paranoid}
  if not fInitialized then
    raise EInvalidOpException.Create('Consts not initialized') at ReturnAddress;
  {$endif}
end;

class constructor Consts.Create;
var
  betaString: string;
begin
  {$ifdef debug}
  GLOBAL_DEVELOPER_MODUS := True;
  {$endif}
  fCachedAppName := ParamStr(0);
  fCachedAppPath := ExtractFilePath(ParamStr(0));
  fLemmixVersion := GetAppVersionString(fLemmixVersionRecord.Major, fLemmixVersionRecord.Minor, fLemmixVersionRecord.Release, fLemmixVersionRecord.Build);
  {$ifdef beta}
  fIsBeta := True;
  betaString := 'beta';
  {$else}
  fIsBeta := False;
  betaString := string.Empty;
  {$endif};
  fFullProgramName := 'Lemmix ' + betaString + ' ' + fLemmixVersion + ' - ' + {$if defined(cpux64)} '64 bits' {$else} '32 bits' {$endif};
  // set defaults
  fPathToStyles := fCachedAppPath + 'Data\Styles\';
  fPathToReplay := fCachedAppPath + 'Output\Replay\';
end;

class procedure Consts.Init(const aPathToStyles, aPathToMusic, aPathToSounds, aPathToReplay: string);
//
{ load the styles. style.config is parsed. properties are set in StyleInformation
  Possible entries:
  • Description     = MyText
  • Author          = MyName
  • Info            = MyInfo
  • Graphics        = DEFAULT or ORIG or OHNO or CONCAT
  • SpecialGraphics = ORIG or DEFAULT
  • Mechanics       = OHNO or ORIG
  • Maindat         = OHNO or ORIG
  • Family          = LEMMINI
}
const
  Texts: array[TStyleDef] of string = ('Original Lemmings', 'Oh No More Lemmings!', 'Holiday Lemmings 1994', 'Xmas Lemmings 1991', 'Xmas Lemmings 1992', 'User Lemmings');
var
  value: string;
  list: TStringList;
begin
  if not aPathToStyles.IsEmpty and TDirectory.Exists(aPathToStyles) then
    fPathToStyles := IncludeTrailingPathDelimiter(aPathToStyles);

  if not aPathToMusic.IsEmpty and TDirectory.Exists(aPathToMusic) then
    fPathToMusic := IncludeTrailingPathDelimiter(aPathToMusic);

  if not aPathToSounds.IsEmpty and TDirectory.Exists(aPathToSounds) then
    fPathToSounds := IncludeTrailingPathDelimiter(aPathToSounds);

  if not aPathToReplay.IsEmpty and TDirectory.Exists(aPathToReplay) then
    fPathToReplay := IncludeTrailingPathDelimiter(aPathToReplay);

  fStyleInformationList := TStyleInformationList.Create;

  fSupportedStyleNames := [TStyleDef.Orig.Name] + [TStyleDef.Ohno.Name] + [TStyleDef.H94.Name] + [TStyleDef.X91.Name] +
                          [TStyleDef.X92.Name] + GetCustomStyleFolders;

  // fill style information
  var ix: Integer :=0;
  for var s in fSupportedStyleNames do begin
    var info: TStyleInformation := TStyleInformation.Create;
    fStyleInformationList.Add(info);
    if ix < Ord(TStyleDef.User) then
      info.fStyleDef := TStyleDef(ix)
    else
      info.fStyleDef := TStyleDef.User;

    info.fFamily := TStyleFamily.DOS; // default
    info.fName := s;
    info.fDescription := s;
    info.fFullPath := PathToStyles + info.fName + '\';

    // only read values for user styles
    if info.fStyleDef = TStyleDef.User then begin
      if FileExists(info.fFullPath + 'Style.config') then begin
        list := TStringList.Create;
        try
          list.LoadFromFile(info.fFullPath + 'Style.config');

          info.fDescription := list.Values['description'];
          info.fAuthor := list.Values['author'];
          info.fInfo := list.Values['info'];

          // read family
          value := list.Values['family'].ToUpper.Trim;
          if value = 'LEMMINI' then
            info.fFamily := TStyleFamily.Lemmini;

          case info.Family of
            TStyleFamily.DOS:
              begin
                // graphics mapping
                value := list.Values['graphics'].ToUpper.Trim;
                if (value = 'ORIG') then info.fUserGraphicsMapping := TLevelGraphicsMapping.Orig
                else if (value = 'OHNO') then info.fUserGraphicsMapping := TLevelGraphicsMapping.Ohno
                else if (value = 'CONCAT') then info.fUserGraphicsMapping := TLevelGraphicsMapping.Concat
                else info.fUserGraphicsMapping := TLevelGraphicsMapping.Default;
                // vgaspec mapping
                value := list.Values['specialgraphics'].ToUpper.Trim;
                if (value = 'ORIG') then info.fUserSpecialGraphicsMapping := TLevelSpecialGraphicsMapping.Orig
                else info.fUserSpecialGraphicsMapping := TLevelSpecialGraphicsMapping.Default;
                // maindat read default or ohno (purple font is in different location because of section 5)
                value := list.Values['maindat'].ToUpper.Trim;
                if (value = 'OHNO') and (info.fUserGraphicsMapping = TLevelGraphicsMapping.Default) then info.fMaindatOhNo := True
                else info.fMaindatOhNo := False;
              end;
            TStyleFamily.Lemmini:
              begin
                info.fUserGraphicsMapping := TLevelGraphicsMapping.Concat;
                info.fUserSpecialGraphicsMapping := TLevelSpecialGraphicsMapping.Orig;
              end;
            end; // case family

            value := list.Values['mechanics'].ToUpper.Trim;
            if (value = 'ORIG') then info.fUserMechanics := DOSORIG_MECHANICS
            else info.fUserMechanics := DOSOHNO_MECHANICS;

          finally
            list.Free;
          end;
      end;
    end
    else begin
      info.fDescription := Texts[info.StyleDef];
      info.fInfo := 'The original DOS game';
      info.fAuthor := 'DMA';
    end;
    Inc(ix);
  end;

  fInitialized := True;
  SetStyleName(TStyleDef.Orig.Name);
end;

class procedure Consts.Done;
begin
  fStyleInformationList.Free;
end;

class function Consts.GetAppName: string;
begin
  Result := fCachedAppName;
end;

class function Consts.GetAppPath: string;
begin
  Result := fCachedAppPath;
end;

class function Consts.GetPathToStyles: string;
begin
  Result := fPathToStyles;
end;

class function Consts.GetPathToDebug: string;
begin
  Result := GetAppPath + '_debug\';
end;

class function Consts.GetPathToAssets: string;
begin
  Result := GetAppPath + 'Assets\';
end;

class function Consts.GetPathToCursors: string;
begin
  Result := GetAppPath + 'Cursors\';
end;

class function Consts.GetPathToErrorLogs: string;
begin
  Result := GetAppPath + 'Output\Logs\';
end;

class function Consts.GetPathToData: string;
begin
  Result := GetAppPath + 'Data\';
end;

class function Consts.GetPathToLogos: string;
begin
  Result := GetPathToData + 'Logos\';
end;

class function Consts.GetPathToParticles: string;
begin
  Result := GetPathToData + 'Particles\';
end;

class function Consts.GetPathToLanguage: string;
begin
  Result := GetPathToData + 'Language\';
end;

class function Consts.GetPathToStyle(const aStylename: string): string;
begin
  Result := GetPathToStyles + aStylename + '\';
end;

class function Consts.GetPathToOutput: string;
begin
  Result := GetAppPath + 'Output\';
end;

class function Consts.GetPathToScreenShots: string;
begin
  Result := GetAppPath + 'Output\ScreenShots\';
end;

class function Consts.GetPathToAutoSave: string;
begin
  Result := GetAppPath + 'Output\AutoSave\';
end;

class function Consts.GetPathToBin: string;
begin
  Result := GetAppPath + 'Output\Bin\';
end;

class function Consts.GetPathToTemp: string;
begin
  Result := GetAppPath + 'Output\Temp\';
end;

class function Consts.GetPathToCache: string;
begin
  Result := GetAppPath + 'Cache\';
end;

class function Consts.GetPathToLemmings(const aStylename: string): string;
begin
  Check;
  Result := PathToStyles + aStylename + '\';
end;

class function Consts.GetPathToMusics(const aStylename: string): string;
begin
  Check;
  if not fPathToMusic.IsEmpty and (IsUserStyle(aStyleName)) then
    Result := fPathToMusic
  else
    Result := GetPathToLemmings(aStyleName) + 'Music\';
end;

class function Consts.GetPathToSounds: string;
begin
  Check;
  if not fPathToSounds.IsEmpty then
    Result := fPathToSounds
  else
    Result := GetPathToData + 'Sounds\';
end;

class function Consts.GetPathToReplay: string;
begin
  Check;
  if not fPathToReplay.IsEmpty then
    Result := fPathToReplay
  else
    Result := GetPathToData + 'Output\Replay\';
end;

class function Consts.GetResourceNameZippedLemmings(const aStylename: string): string;
begin
  Check;
  Result := aStylename;
end;

class function Consts.GetResourceNameZippedMusics(const aStylename: string): string;
begin
  Check;
  Result := aStylename + '_music';
end;


class function Consts.GetFilenameLogo(index: TStyleDef): string;
begin
  Check;
  Result := fFilenamesLogo[index];
end;

class function Consts.FindDefaultStyle(const aName: string; out aStyle: TStyleDef): Boolean;
begin
  Check;
  for var s: TStyleDef in DefaultStyles do
    if SameText(STYLE_NAMES[s], aName) then begin
      aStyle := s;
      Exit(True);
    end;
  Result := False;
  aStyle := TStyleDef.Orig; // there is no null
end;

class procedure Consts.SetStyleName(const aName: string);
var
  found: TStyleDef;
  newName: string;
begin
  Check;
  if fDefined and SameText(fStyleName, aName) then
    Exit;

  newName := aName;
  var Exists: Boolean := False;
  for var s: string in fSupportedStyleNames do
    if SameText(aName, s) then begin
      Exists := True;
      Break;
    end;

  // fallback on default
  if not Exists then
    newName := TStyleDef.Orig.Name;

  for var info: TStyleInformation in fStyleInformationList do
    if SameText(newname, info.Name) then begin
      fStyleInfo := info;
      Break;
    end;

  if FindDefaultStyle(newName, found) then begin
    fStyleDef := found;
    fStyleName := STYLE_NAMES[fStyleDef];
  end
  else begin
    fStyleDef := TStyleDef.User;
    fStylename := newName;
  end;

  fChristmasPalette := fStyleDef in [TStyleDef.H94, TStyleDef.X91, TStyleDef.X92];
  fDefined := True;
end;

class function Consts.IsUserStyle(const aName: string): Boolean;
begin
  for var style: TStyleDef in DefaultStyles do
    if SameText(style.Name, aName) then
      Exit(False);
  Result := True;
end;

class function Consts.GetCustomStyleFolders: TArray<string>;
begin
  if not TDirectory.Exists(PathToStyles) then
    Exit(nil);

  // custom styles
  var Filter: TDirectory.TFilterPredicate :=
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
      for var style: TStyleDef in DefaultStyles do
        if SameText(SearchRec.Name, style.name) then
          Exit(False);
      Result := True;
    end;

  Result := TDirectory.GetDirectories(PathToStyles, Filter);

  for var i := 0 to Length(Result) - 1 do
    Result[i] := ExtractFileName(Result[i]);

end;

class function Consts.FindStyleInfo(const aStyleName: string): TStyleInformation;
begin
  Check;
  for Result in StyleInformationList do
    if SameText(aStyleName, Result.Name) then
      Exit;
  Result := nil;
end;

class function Consts.GraphicSetNameToGraphicSet(const aGraphicSetname: string): Integer;
begin
  for Result := 0 to 8 do
    if SameText(fGraphicSetNames[Result], aGraphicSetname) then
      Exit;
  Result := -1;
end;

end.

