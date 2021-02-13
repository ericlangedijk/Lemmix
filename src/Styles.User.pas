{------------------------------------------------------------------------------
  This unit contains classes for custom user styles inside the Styles folder.
-------------------------------------------------------------------------------}

unit Styles.User;

{$include lem_directives.inc}

interface

uses
vcl.dialogs,

  System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, System.Character,
  Base.Utils, Base.Types,
  Dos.Consts, Dos.Compression, Dos.Structures,
  Prog.Base,
  Styles.Base;

type
  TUserStyle = class(TStyle)
  private
  // these properties are decided by Style.config - see Prog.Base
    fFamily: TStyleFamily;
    fLevelGraphicsMapping: TLevelGraphicsMapping;
    fLevelSpecialGraphicsMapping: TLevelSpecialGraphicsMapping;
  protected
    function GetLevelSystemClass: TLevelSystemClass; override;
    function GetMechanics: TMechanics; override;
  public
    constructor Create(const aName: string); override;
    property Family: TStyleFamily read fFamily;
    property LevelGraphicsMapping: TLevelGraphicsMapping read fLevelGraphicsMapping;
    property LevelSpecialGraphicsMapping: TLevelSpecialGraphicsMapping read fLevelSpecialGraphicsMapping;
  end;

  TUserLevelSystem = class(TLevelSystem)
  strict private
    type
      TAnalyzedEntry = class
      public
        SectionIndex: Integer;
        LevelIndex: Integer;
        DosDatIndex: Integer;
        IsRawLVL: Boolean;
        FileName: string;
        constructor Create(aSectionIndex, aLevelIndex, aDosDatIndex: Integer; aRawLVL: Boolean; const aFileName: string);
      end;

    type
      TEntry = record
        SectionIndex: Integer;
        LevelIndex: Integer;
        constructor Create(aSection, aLevel: Integer);
      end;

  strict private
    fGroundFiles: TDictionary<Integer, string>;
    fGraphicFiles: TDictionary<Integer, string>;
    fGraphicExtFiles: TDictionary<Integer, string>;
    fLevelDATFiles: TStringList;
    fLevelLVLFiles: TStringList;
  protected
    procedure GetFileNamesForGraphicSet(aGraphicSetId, aGraphicSetIdExt: Integer; out aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName: string); override;
    function GetNumberOfLevelsFromLevelDATFiles(const path: string): Integer;
    procedure DoInitializeLevelSystem; override;
  public
    constructor Create(aStyle: TStyle); override;
    destructor Destroy; override;
  end;

  TLemminiLevelSystem = class(TLevelSystem)
  protected
    procedure DoInitializeLevelSystem; override;
  public
    constructor Create(aStyle: TStyle); override;
    destructor Destroy; override;
  end;

implementation

{ TUserStyle }

constructor TUserStyle.Create(const aName: string);
begin
  inherited Create(aName);
  fFamily := StyleInformation.Family;
  fLevelGraphicsMapping := StyleInformation.UserGraphicsMapping; // style.config indicated a mapping
  fLevelSpecialGraphicsMapping := StyleInformation.UserSpecialGraphicsMapping; // style.config indicated a mapping
end;

function TUserStyle.GetLevelSystemClass: TLevelSystemClass;
begin
//  Result := TUserLevelSystem;
//
//  var info: Consts.TStyleInformation := Consts.FindStyleInfo(Name);
//  if Assigned(info) and info.f then
//    Result := info.UserMechanics; // style.config indicated a mapping
  if fFamily = TStyleFamily.DOS then
    Result := TUserLevelSystem
  else
    Result := TLemminiLevelSystem;
end;

function TUserStyle.GetMechanics: TMechanics;
begin
  Result := DOSOHNO_MECHANICS;
  var info: Consts.TStyleInformation := Consts.FindStyleInfo(Name);
  if Assigned(info) then
    Result := info.UserMechanics; // style.config indicated a mapping
end;

{ TUserLevelSystem.TAnalyzedEntry }

constructor TUserLevelSystem.TAnalyzedEntry.Create(aSectionIndex, aLevelIndex, aDosDatIndex: Integer; aRawLVL: Boolean; const aFileName: string);
begin
  SectionIndex := aSectionIndex;
  LevelIndex := aLevelIndex;
  DosDatIndex := aDosDatIndex;
  IsRawLVL := aRawLVL;
  FileName := aFileName;
end;

{ TUserLevelSystem.TEntry }

constructor TUserLevelSystem.TEntry.Create(aSection, aLevel: Integer);
begin
  SectionIndex := aSection;
  LevelIndex := aLevel;
end;

{ TUserLevelSystem }

constructor TUserLevelSystem.Create(aStyle: TStyle);
begin
  if not (aStyle is TUserStyle) then
    Throw('Style owner type error', 'Create');
  inherited Create(aStyle);
  fGroundFiles := TDictionary<Integer, string>.Create;
  fGraphicFiles := TDictionary<Integer, string>.Create;
  fGraphicExtFiles := TDictionary<Integer, string>.Create;
  fLevelDATFiles := TStringList.Create;
  fLevelLVLFiles := TStringList.Create;
end;

destructor TUserLevelSystem.Destroy;
begin
  fGroundFiles.Free;
  fGraphicFiles.Free;
  fGraphicExtFiles.Free;
  fLevelDATFiles.Free;
  fLevelLVLFiles.Free;
  inherited;
end;

function TUserLevelSystem.GetNumberOfLevelsFromLevelDATFiles(const path: string): Integer;
const method = 'GetNumberOfLevelsFromLevelDATFiles';
// internal routine only called by DoInitializeLevelSystem: fLevelDATFiles must be filled.
// fills count as fake object to avoid dupliace calculations
var
  cmp: TDosDatDecompressor;
  cnt: Integer;
  LVLCheck: Boolean;
begin
  if fLevelDATFiles.Count = 0 then
    Exit(0);

  Result := 0;
  cmp := TDosDatDecompressor.Create;
  try
    var ix: Integer := 0;
    for var levelfile: string in fLevelDATFiles do begin
      cnt := cmp.GetNumberOfSectionsOnly(path + levelfile, {out} LVLCheck);
      if not LVLCheck or (cnt = 0) then
        Throw('Invalid levelfile encountered ' + path + levelfile, method);
      fLevelDATFiles.Objects[ix] := TObject(cnt); // store the nr of levels for this file
      Inc(Result, cnt);
      Inc(ix);
    end;
  finally
    cmp.Free;
  end;
end;

procedure TUserLevelSystem.DoInitializeLevelSystem;
const method = 'DoInitializeLevelSystem'; // system
{------------------------------------------------------------------------------
  • If there are no files with the mask "*vel*.dat" we assume all levels are
    seperate .LVL files.
  • If there are level.dat files the levelsystem counts and splits the sections
    and levels.
  • If there is
  • an eventual existing oddtable.dat is ignored
-------------------------------------------------------------------------------}

const
  sectionNames: array[0..3] of string = ('Fun', 'Tricky', 'Taxing', 'Mayhem'); // we use the original sectionnames

    function FindDigit(const aFilename: string; out filenameOnly: string): Integer;
    begin
      filenameOnly := ExtractFileName(aFileName);
      for var C: Char in filenameOnly do
        if C.IsDigit then
          Exit(StrToInt(C));
      Result := -1;
    end;

var
  path: string;
  musicpath: string;
  groundFilenames: TArray<string>;
  graphicFilenames: TArray<string>;
  leveldatFilenames: TArray<string>;
  leveldatDictionary: TDictionary<string, Integer>;
  customlevelFilenames: TArray<string>;
  musicFilenames: TArray<string>;
  mp3Filenames: TArray<string>;
  ix: Integer;
  filenameOnly: string;
  foundGroundIndices: set of Byte;
  foundGraphicIndices: set of Byte;
  currentSectionIndex: Integer;
  currentLevelIndex: Integer;
  section: TSection;
  levelInfo: TLevelLoadingInformation;
  templevelList: TFastObjectList<TAnalyzedEntry>;
  tempDictionary: TDictionary<TEntry, Boolean>;
  totalDATLevels: Integer;
  levelsPerSection: Integer;
  mapping: TLevelGraphicsMapping;
  graphicsmapped: Boolean;
  musicIndex: Integer;
  modulo: Integer;
begin

  mapping := Style.StyleInformation.UserGraphicsMapping;
  graphicsmapped := mapping <> TLevelGraphicsMapping.Default;
  musicFilenames := [];

  foundGroundIndices := [];
  foundGraphicIndices := [];

  path := Style.RootPath;
  musicpath := Consts.PathToMusics[style.Name];
  templevelList := TFastObjectList<TAnalyzedEntry>.Create;
  tempDictionary := TDictionary<TEntry, Boolean>.Create;
  leveldatDictionary := TDictionary<string, Integer>.Create;

  try
    // gather all files
    if not graphicsmapped then begin
      groundFilenames := TDirectory.GetFiles(path, 'ground*o.dat');
      graphicFilenames := TDirectory.GetFiles(path, 'vgagr*.dat');
    end;

    // gather dat level files
    leveldatFilenames := TDirectory.GetFiles(path, '*lev*.dat');

    // gather lvl level files
    var LVLFilter: TDirectory.TFilterPredicate :=
      function(const Path: string; const SearchRec: TSearchRec): Boolean
      begin
        Result := SearchRec.Size = LVL_SIZE;
      end;
    customlevelFilenames := TDirectory.GetFiles(path, '*.LVL', LVLFilter);

    if TDirectory.Exists(musicpath) then begin
      musicFilenames := TDirectory.GetFiles(musicpath, '*.MOD');
      mp3Filenames := TDirectory.GetFiles(musicpath, '*.MP3');
      musicFileNames := musicFilenames + mp3Filenames;
      TArray.Sort<string>(musicFileNames)
    end;

    // check if there are any levels
    if Length(leveldatFilenames) + Length(customlevelFilenames) = 0 then
      Throw('No level files found for style '+ Style.Name, method);

    // analyze ground filenames
    if not graphicsmapped then begin
      for var groundfile: string in groundFileNames do begin
        ix := FindDigit(groundfile, filenameOnly);
        if ix >= 0 then begin
          fGroundFiles.Add(ix, filenameOnly);
          Include(foundGroundIndices, Byte(ix));
        end;
      end;

      // analyze graphic filenames
      for var graphicfile: string in graphicFilenames do begin
        ix := FindDigit(graphicfile, filenameOnly);
        if ix >= 0 then begin
          fGraphicFiles.Add(ix, filenameOnly);
          Include(foundGraphicIndices, Byte(ix))
        end;
      end;
   end;

    // check matching ground and graphics
    if foundGroundIndices <> foundGraphicIndices then
      Throw('Mismatch in number of groundfiles and graphicfiles', method);

    // add level DAT filenames
    for var levelfile: string in leveldatFilenames do
      fLevelDATFiles.Add(ExtractFileName(levelfile));
    fLevelDATFiles.Sort; // we sort these dat-files by name

    // add level LVL filenames
    for var lvlfile: string in customlevelFilenames do
      fLevelLVLFiles.Add(ExtractFileName(lvlfile));
    fLevelLVLFiles.Sort;

    // determine the total number of levels from DAT and LVL
    totalDATLevels := GetNumberOfLevelsFromLevelDATFiles(Style.RootPath);
    if totalDATLevels + fLevelLVLFiles.Count = 0 then
      Throw('No DAT or LVL levels found for style '+ Style.Name, method);

    if totalDATLevels + fLevelLVLFiles.Count >= 40 then
      levelsPerSection := (totalDATLevels + fLevelLVLFiles.Count) div 4
    else
      levelsPerSection := 10;
    if levelsPerSection > (totalDATLevels + fLevelLVLFiles.Count) then
      levelsPerSection := (totalDATLevels + fLevelLVLFiles.Count);

    modulo := (totalDATLevels + fLevelLVLFiles.Count) mod levelsPerSection;

    // now distribute the level DAT files
    currentSectionIndex := 0;
    currentLevelIndex := 0;
    ix := 0;
    for var levelfile: string in fLevelDATFiles do begin
      var nrOfLevels := Integer(fLevelDatFiles.Objects[ix]); // retrieve nr of levels from stringlist
      for var i := 0 to nrOfLevels - 1 do begin
        templevelList.Add(TAnalyzedEntry.Create(currentSectionIndex, currentLevelIndex, i, False, levelfile));
        Inc(currentLevelIndex);
        if currentLevelIndex >= levelsPerSection then begin
          if modulo > 0 then
            Dec(modulo)
          else begin
            Inc(currentSectionIndex);
            currentLevelIndex := 0;
          end;
          if currentSectionIndex > 4 then
            Break;
        end;
      end;
      if currentSectionIndex >= 4 then
        Break;
      Inc(ix);
    end;

    // now distribute the raw LVL files.
    for var lvlFile in fLevelLVLFiles do begin
      templevelList.Add(TAnalyzedEntry.Create(currentSectionIndex, currentLevelIndex, 0, True, lvlFile));
      Inc(currentLevelIndex);
      if currentLevelIndex >= levelsPerSection then begin
        if modulo > 0 then
          Dec(modulo)
        else begin
          Inc(currentSectionIndex);
          currentLevelIndex := 0;
        end;
        if currentSectionIndex >= 4 then
          Break;
      end;
    end;

    // check if we found any levels stored
    if templevelList.Count = 0 then // levelCount + fCustomLVLFiles.Count = 0 then
      Throw('No levels found in style ' + Style.Name, method);

    musicIndex := 0;
    // now add sections and levels
    section := TSection.Create(Self); // first section
    section.SectionName := sectionNames[0];
    for var entry: TAnalyzedEntry in templevelList do begin

//      log([entry.SectionIndex, entry.LevelIndex, entry.DosDatIndex, entry.FileName, entry.IsRawLVL]);
      if entry.SectionIndex > SectionList.Last.SectionIndex then begin
        section := TSection.Create(Self);
        section.SectionName := sectionNames[section.SectionIndex];
      end;

      levelInfo := TLevelLoadingInformation.Create(section);
      levelinfo.SourceFileName := entry.FileName;
      levelinfo.SectionIndexInSourceFile := entry.DosDatIndex;
      levelinfo.IsRawLVLFile := entry.IsRawLVL;
      levelinfo.UseOddTable := False;
      levelinfo.OddTableIndex := -1;
      if musicFilenames.Length > 0 then begin
        levelinfo.MusicFileName := musicFilenames[musicIndex];
      end;
      Inc(musicIndex);
      if musicIndex >= musicFilenames.Length then
        musicIndex := 0;

    end;

  finally
    templevelList.Free;
    tempDictionary.Free;
    leveldatDictionary.Free;
  end;
end;

procedure TUserLevelSystem.GetFileNamesForGraphicSet(aGraphicSetId, aGraphicSetIdExt: Integer; out aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName: string);
const unknown = '$unknown';
const method = 'GetFileNamesForGraphicSet';
// we use our internal dictionaries for this if files are not mapped
begin
  aGraphicsFileName := string.Empty;
  aMetaDataFileName := string.Empty;
  aSpecialGraphicsFileName := string.Empty;

  inherited GetFileNamesForGraphicSet(aGraphicSetId, aGraphicSetIdExt, aMetaDataFileName, aGraphicsFileName, aSpecialGraphicsFileName);

  if TUserStyle(Style).LevelGraphicsMapping = TLevelGraphicsMapping.Default then begin
    if not fGroundFiles.TryGetValue(aGraphicSetId, aMetaDataFileName) then
      Throw('Cannot find ground file for graphicset (' + aGraphicSetId.ToString + ')', method);
    if not fGraphicFiles.TryGetValue(aGraphicSetId, aGraphicsFileName) then
      Throw('Cannot find graphics file for graphicset (' + aGraphicSetId.ToString + ')', method);
  end;

  if (aGraphicSetIdExt > 0) and (TUserStyle(Style).LevelSpecialGraphicsMapping = TLevelSpecialGraphicsMapping.Default) then
    aSpecialGraphicsFileName := 'vgaspec' + Pred(aGraphicSetIdExt).ToString + '.dat' // 1 maps to 0 for filename
end;

{ TLemminiLevelSystem }

constructor TLemminiLevelSystem.Create(aStyle: TStyle);
begin
  if not (aStyle is TUserStyle) or (TUserStyle(aStyle).Family <> TStyleFamily.Lemmini) then
    Throw('Style owner type error', 'Create');
  inherited Create(aStyle);
end;

destructor TLemminiLevelSystem.Destroy;
begin
  inherited Destroy;
end;

procedure TLemminiLevelSystem.DoInitializeLevelSystem;
const
  sectionNames: array[0..3] of string = ('Fun', 'Tricky', 'Taxing', 'Mayhem'); // we use the original sectionnames
var
  path, filename: string;
  iniFiles: TArray<string>;
  levelsPerSection: Integer;
  modulo: Integer;
  currentSectionIndex, currentLevelIndex: Integer;
  section: TSection;
  levelInfo: TLevelLoadingInformation;
begin
  path := Style.RootPath;

  var fileFilter: TDirectory.TFilterPredicate :=
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    begin
      Result := string(SearchRec.Name).ToLower <> 'levelpack.ini';
    end;

  iniFiles := TDirectory.GetFiles(path, '*.ini', fileFilter);
  iniFiles.Sort;
  if iniFiles.Length = 0 then
    Exit;

  modulo := iniFiles.Length mod 4;

  if iniFiles.Length >= 40 then
    levelsPerSection := iniFiles.Length div 4
  else
    levelsPerSection := 10;
  if levelsPerSection > iniFiles.Length then
    levelsPerSection := iniFiles.Length;

  section := TSection.Create(Self); // first section
  section.SectionName := sectionNames[0];

  currentSectionIndex := 0;
  currentLevelIndex := 0;
  for filename in iniFiles do begin
    levelInfo := TLevelLoadingInformation.Create(section);
    levelinfo.SourceFileName := ExtractFileName(filename);
    levelinfo.SectionIndexInSourceFile := 0;
    levelinfo.IsRawLVLFile := False;
    levelinfo.IsLemminiFile := True;
    levelinfo.UseOddTable := False;
    levelinfo.OddTableIndex := -1;
    Inc(currentLevelIndex);

    if currentLevelIndex >= levelsPerSection then begin
      if modulo > 0 then
        Dec(modulo)
      else begin
        Inc(currentSectionIndex);
        if currentSectionIndex > 3 then
          Break;
        section := TSection.Create(Self);
        section.SectionName := sectionNames[currentSectionIndex];
        currentLevelIndex := 0;
      end;
    end;


  end;
//    if musicFilenames.Length > 0 then begin
//      levelinfo.MusicFileName := musicFilenames[musicIndex];
//    end;
//    Inc(musicIndex);
//    if musicIndex >= musicFilenames.Length then
//      musicIndex := 0;

end;

end.

