unit Prog.Cache;

{$include lem_directives.inc}

interface

uses
  System.Classes, System.Contnrs, System.SysUtils, System.IOUtils, System.Generics.Collections, System.Character,
  Dos.Structures,
  Base.Utils,
  Level.Hash,
  Styles.Base, Styles.Factory,
  Prog.Types, Prog.Base;

type
  // the style cache builds lists of all styles with levelhashes + titles.
  // this is needed to be able to quickly find levelhashes, levelcodes, replayfiles
  TStyleCacheFeedbackProc = reference to procedure(const s: string);

  TStyleCache = class
  public
    const
      MAX_STYLENAME_LENGTH = 64;
      CACHE_ID = 'LCACHE';
      CACHE_VERSION = 8;
      LEVEL_ID = 'L';
    type
      // on disk
      THeaderRec = packed record
        Id: array[1..6] of Char; // = CACHE_ID; version 1 = 'LEMMIX' version 2 = 'LCACHE'
        LemmixVersion: Consts.TVersionRecord;
        CacheVersion: Integer;
        StyleName: array[1..MAX_STYLENAME_LENGTH] of Char;
        Family: TStyleFamily;
        FileSize: Integer;
        LevelCount: Integer; // total levelinfo records
        LastWriteTimeInSourceFolder: TDateTime;
        LastWriteTimeOfConfigFile: TDateTime;
        procedure Clear; inline;
        function GetStyleName: string;
        procedure SetStyleName(const s: string);
      end;

      TStatics = packed record
        ReleaseRate    : Byte;
        LemmingsCount  : Byte;
        RescueCount    : Byte;
        TimeLimit      : Byte;
        ClimberCount   : Byte;
        FloaterCount   : Byte;
        BomberCount    : Byte;
        BlockerCount   : Byte;
        BuilderCount   : Byte;
        BasherCount    : Byte;
        MinerCount     : Byte;
        DiggerCount    : Byte;
        GraphicSet     : Byte;
        GraphicSetEx   : Byte;
        SuperLemming   : Boolean;
        ObjectCount    : Byte;
        TerrainCount   : word;
        EntranceCount  : Byte;
        ExitCount      : Byte;
      end;

      // on disk
      TLevelRec = packed record
        Id: Char; // = LEVEL_ID; = 'L'
        SectionIndex: Integer;
        LevelIndex: Integer;
        LevelHash: UInt64;
        Statics: TStatics;
        LevelTitle: TLVLTitle;
        SourceFile: TArray<Char>;
        procedure Clear; inline;
        function GetTitle: string;
        function GetSourceFile: string;
        procedure SetSourceFile(const aValue: string);
      end;

    const
      LEVEL_REC_SIZE_UNTIL_SOURCE_FILE = SizeOf(Char) + SizeOf(Integer) * 2 + SizeOf(UInt64) + SizeOf(TStatics) + SizeOf(TLVLTitle);
      //                                 Id             section + level       hash             statics            title

    type
      // in memory
      TLevelCacheItem = class
      private
        fStyleName: string;
        //Family: TStyleFamily; todo
        fSectionIndex: Integer;
        fLevelIndex: Integer;
        fLevelHash: UInt64;
        fLevelCode: string;
        fLevelTitle: TLVLTitle;
        fStatics: TStatics;
        fSourceFile: string;
      public
        constructor Create(const aStyleName: string; const aLevelRec: TLevelRec);
        function GetTitleAsString: string;
        function MatchesWithLevelLoadingInformation(info: TLevelLoadingInformation): Boolean;
        property StyleName: string read fStyleName;
        property SectionIndex: Integer read fSectionIndex;
        property LevelIndex: Integer read fLevelIndex;
        property LevelHash: UInt64 read fLevelHash;
        property LevelCode: string read fLevelCode;
        property LevelTitle: TLVLTitle read fLevelTitle;
        property Statics: TStatics read fStatics;
        property SourceFile: string read fSourceFile;
      end;
      TLevelCacheList = TFastObjectList<TLevelCacheItem>;

//      TExactEntry = record
//        Style: string;
//        Section: Integer;
//        Level: Integer;
//        constructor Create(const aStyle: string; aSection, aLevel: Integer);
//      end;

  private
    fFlatList: TFastObjectList<TLevelCacheItem>; // todo: make this one the ultimate owner
    fCache: TObjectDictionary<string, TLevelCacheList>; // key=stylename, value=levels
    fHashCache: TObjectDictionary<UInt64, TLevelCacheList>; // key=levelhash, value=levels which have this hashcode
    fCodeCache: TObjectDictionary<string, TLevelCacheList>; // key=levelcode, value=levels which have this levelcode
    fTitleCache: TObjectDictionary<TLVLTitle, TLevelCacheList>; // key=leveltitle, value=levels which have this title
//    fExactCache : TObjectDictionary<TExactEntry, TLevelCacheItem>;
    fOnFeedback: TStyleCacheFeedbackProc; // simple feedback during load
    fLastState: string;
    procedure Feedback(const state: string);
    function ReadHeader(s: TStream; var header: THeaderRec): Boolean;
    function ReadLevelInfo(s: TStream; var info: TLevelRec): Boolean;
    procedure WriteLevelInfo(s: TStream; const info: TLevelRec);
    function RebuildNeeded(const aStyleName: string): Boolean;
    procedure BuildStyleCache(const aStyle: TStyle; aTargetList: TLevelCacheList);
    procedure LoadStyleCache(const aStyleName: string; aTargetlist: TLevelCacheList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const aFeedbackProc: TStyleCacheFeedbackProc);
    function GetCacheFilenames(fullName: Boolean = True): TArray<string>;
    function FindLevelsByHash(const aHash: UInt64): TArray<TLevelCacheItem>;
    function FindLevelsByCode(const aLevelCode: string): TArray<TLevelCacheItem>;
    function FindLevelsByTitle(const aLevelTitle: TLVLTitle): TArray<TLevelCacheItem>;
//    function FindExactLevel(const aStyleName: string; aSectionIndex, aLevelIndex: Integer): TLevelCacheItem;
    function GetLevelCount(const aStyleName: string): Integer;
    function GetTotalLevelCount: Integer;
    procedure ToCsv(const aFilename: string);
    property FlatList: TFastObjectList<TLevelCacheItem> read fFlatList;
  end;

implementation

{ TStyleCache.THeaderRec }

procedure TStyleCache.THeaderRec.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TStyleCache.THeaderRec.GetStyleName: string;
var
  C: Char;
begin
  Result := '';
  for var i := 1 to MAX_STYLENAME_LENGTH do begin
    C := StyleName[i];
    if C = #0 then
      Break;
    Result := Result + C;
  end;
end;

procedure TStyleCache.THeaderRec.SetStyleName(const s: string);
begin
  for var i := 1 to Length(s) do
    StyleName[i] := s[i];
end;

{ TStyleCache.TLevelRec }

function TStyleCache.TLevelRec.GetTitle: string;
var
  C: AnsiChar;
begin
  SetLength(Result, 31);
  for var i := 0 to 31 do begin
    C := LevelTitle[i];
    Result[i + 1] := Char(C);
  end;
end;

procedure TStyleCache.TLevelRec.Clear;
begin
  SetLength(SourceFile, 0);
  FillChar(Self, SizeOf(Self), 0);
end;

function TStyleCache.TLevelRec.GetSourceFile: string;
begin
  SetLength(Result, Length(SourceFile));
  for var i := 0 to Length(SourceFile) - 1 do
    Result[i + 1] := SourceFile[i];
end;

procedure TStyleCache.TLevelRec.SetSourceFile(const aValue: string);
begin
  SetLength(SourceFile, Length(aValue));
  for var i := 1 to Length(aValue) do
    SourceFile[i - 1] := aValue[i];
end;

{ TStyleCache.TLevelCacheItem }

constructor TStyleCache.TLevelCacheItem.Create(const aStyleName: string; const aLevelRec: TLevelRec);
begin
  fStylename := aStyleName;
  fSectionIndex := aLevelRec.SectionIndex;
  fLevelIndex := aLevelRec.LevelIndex;
  fLevelHash := aLevelRec.LevelHash;
  fLevelTitle := aLevelRec.LevelTitle;
  fLevelCode := TLevelHasher.GetLevelCode(fLevelHash);
  fStatics := aLevelRec.Statics;
  fSourceFile := aLevelRec.GetSourceFile;
end;

function TStyleCache.TLevelCacheItem.GetTitleAsString: string;
var
  C: AnsiChar;
begin
  SetLength(Result, 31);
  for var i := 0 to 31 do begin
    C := fLevelTitle[i];
    Result[i + 1] := Char(C);
  end;
end;

function TStyleCache.TLevelCacheItem.MatchesWithLevelLoadingInformation(info: TLevelLoadingInformation): Boolean;
begin
  Result :=
    Assigned(info) and
    (fStyleName = info.Style.Name) and
    (fSectionIndex = info.SectionIndex) and
    (fLevelIndex = info.LevelIndex) and
    (fSourceFile = info.SourceFileName);
end;
//
//{ TStyleCache.TExactEntry }
//
//constructor TStyleCache.TExactEntry.Create(const aStyle: string; aSection, aLevel: Integer);
//begin
//  Style := aStyle;
//  Section := aSection;
//  Level := aLevel;
//end;

{ TStyleCache }

constructor TStyleCache.Create;
begin
  fCache := TObjectDictionary<string, TLevelCacheList>.Create([doOwnsValues]);
  fHashCache := TObjectDictionary<UInt64, TLevelCacheList>.Create([doOwnsValues]);
  fCodeCache := TObjectDictionary<string, TLevelCacheList>.Create([doOwnsValues]);
  fTitleCache := TObjectDictionary<TLVLTitle, TLevelCacheList>.Create([doOwnsValues]);
  fFlatList := TFastObjectList<TLevelCacheItem>.Create(False);
//  fExactCache := TObjectDictionary<TExactEntry, TLevelCacheItem>.Create;
end;

destructor TStyleCache.Destroy;
begin
  fCache.Free;
  fHashCache.Free;
  fCodeCache.Free;
  fTitleCache.Free;
  //fExactCache.Free;
  fFlatList.Free;
  inherited;
end;

procedure TStyleCache.Feedback(const state: string);
begin
  if fLastState <> state then begin
    fLastState := state;
    if Assigned(fOnFeedback) then
      fOnFeedback(state);
  end;
end;

function TStyleCache.ReadHeader(s: TStream; var header: THeaderRec): Boolean;
// returns false if some error or version conflict
begin
  if s.ReadData(header) <> SizeOf(header) then
    Exit(False);
  if header.Id <> CACHE_ID then
    Exit(False);
  if header.CacheVersion <> CACHE_VERSION then
    Exit(False);
  Result := True;
end;

function TStyleCache.ReadLevelInfo(s: TStream; var info: TLevelRec): Boolean;
begin
//  if s.ReadData(info) <> SizeOf(info) then
//    Exit(False);
//  if info.Id <> 'L' then
//    Exit(False);
  info.Clear;

  if s.Read(info, LEVEL_REC_SIZE_UNTIL_SOURCE_FILE) <> LEVEL_REC_SIZE_UNTIL_SOURCE_FILE then
    Exit(False);
  if info.Id <> 'L' then
    Exit(False);

  var len: Integer := 0;
  s.ReadData(len);
  if (len < 0) or (len > 255) then
    Throw('Invalid sourcefilename length encountered (' + len.ToString + ')', 'ReadLevelInfo');
  if len > 0 then begin
    SetLength(info.SourceFile, len);
    s.ReadBuffer(info.SourceFile[0], len * SizeOf(Char));
  end;

  Result := True;
end;

procedure TStyleCache.WriteLevelInfo(s: TStream; const info: TLevelRec);
var
  len: Integer;
begin
  s.Write(info, LEVEL_REC_SIZE_UNTIL_SOURCE_FILE);
  len := Length(info.SourceFile);
  if (len < 0) or (len > 255) then
    Throw('Invalid sourcefilename length encountered (' + len.ToString + ')', 'WriteLevelInfo');
  s.WriteData(len);
  if len > 0 then
    s.WriteBuffer(info.SourceFile[0], len * SizeOf(Char));
end;

function TStyleCache.RebuildNeeded(const aStyleName: string): Boolean;
var
  filename, configfilename: string;
  sourcePath: string;
  header: THeaderRec;
  stream: TBufferedFileStream;
  isCustom: Boolean;
begin
  filename := Consts.PathToCache + aStyleName + '.cache';
  configfilename := Consts.PathToStyle[aStyleName] + 'Style.config';
  if not FileExists(filename) then
    Exit(True);
  isCustom := Consts.IsUserStyle(aStylename);
  sourcePath := Consts.PathToStyles + aStyleName;
  stream := TBufferedFileStream.Create(filename, fmOpenRead);
  try
    // check header
    if not ReadHeader(stream, header) then
      Exit(True);

    if isCustom then begin
      // check folder changes
      if TDirectory.GetLastWriteTime(sourcePath) <> header.LastWriteTimeInSourceFolder then
        Exit(True);
      // check config file change
      if FileExists(configfilename) then begin
        if header.LastWriteTimeOfConfigFile <> TFile.GetLastWriteTime(configfilename) then
          Exit(True);
      end
      else begin
        if header.LastWriteTimeOfConfigFile = 0 then
          Exit(True);
      end;
    end;

    Result := False;
  finally
    stream.Free;
  end;
end;

procedure TStyleCache.BuildStyleCache(const aStyle: TStyle; aTargetList: TLevelCacheList);
// write all levelinfo to file
var
  filename, configfilename: string;
  sourcePath: string;
  stream: TBytesStream;
  header: THeaderRec;
  rec: TLevelRec;
  lev: TLevelLoadingInformation;
  item: TLevelCacheItem;
  LVL: PLVLRec;
begin
  sourcePath := Consts.PathToStyle[aStyle.Name];
  filename := Consts.PathToCache + aStyle.Name + '.cache';
  configfilename := Consts.PathToStyle[aStyle.Name] + 'Style.config';

  if not ForceDir(filename) then
    Throw('Cannot create cache directory');

  // fill header
  header.Clear;
  header.Id := CACHE_ID;
  header.LemmixVersion := Consts.LemmixVersionRecord;
  header.CacheVersion := CACHE_VERSION;
  var ix: Integer := 1;
  for var C: Char in aStyle.Name do begin
    header.StyleName[ix] := C;
    Inc(ix);
    if ix > Length(header.StyleName) then
      Break;
  end;

  if TDirectory.Exists(sourcePath) then
    header.LastWriteTimeInSourceFolder := TDirectory.GetLastWriteTime(sourcePath)
  else
    header.LastWriteTimeInSourceFolder := 0;

  if FileExists(configfilename) then
    header.LastWriteTimeOfConfigFile := TFile.GetLastWriteTime(configfilename);

  stream := TBytesStream.Create;
  try
    // write header
    header.FileSize := SizeOf(header);
    stream.WriteData(header);

    // write levels
    lev := aStyle.LevelSystem.FirstLevelOrDefault;
    while lev <> nil do begin
      rec.Clear;
      rec.Id := 'L';
      rec.SectionIndex := lev.SectionIndex;

      rec.LevelIndex := lev.LevelIndex;
      rec.LevelHash := lev.GetLevelHash; // cachelvl triggered

      Assert(lev.IsCached);
      LVL := lev.CachedLVL;

      rec.Statics.ReleaseRate    := LVL^.SwapProp(LVL^.ReleaseRate);
      rec.Statics.LemmingsCount  := LVL^.SwapProp(LVL^.LemmingsCount);
      rec.Statics.RescueCount    := LVL^.SwapProp(LVL^.RescueCount);
      rec.Statics.TimeLimit      := LVL^.SwapProp(LVL^.TimeLimit);
      rec.Statics.ClimberCount   := LVL^.SwapProp(LVL^.ClimberCount);
      rec.Statics.FloaterCount   := LVL^.SwapProp(LVL^.FloaterCount);
      rec.Statics.BomberCount    := LVL^.SwapProp(LVL^.BomberCount);
      rec.Statics.BlockerCount   := LVL^.SwapProp(LVL^.BlockerCount);
      rec.Statics.BuilderCount   := LVL^.SwapProp(LVL^.BuilderCount);
      rec.Statics.BasherCount    := LVL^.SwapProp(LVL^.BasherCount);
      rec.Statics.MinerCount     := LVL^.SwapProp(LVL^.MinerCount);
      rec.Statics.DiggerCount    := Byte(LVL^.SwapProp(LVL^.DiggerCount));
      rec.Statics.GraphicSet     := Byte(LVL^.SwapProp(LVL^.GraphicSet));
      rec.Statics.GraphicSetEx   := LVL^.SwapProp(LVL^.GraphicSetEx);
      rec.Statics.SuperLemming   := LVL^.IsSuperLemming;

      var entranceCount, exitCount: Integer;

      rec.Statics.ObjectCount    := Byte(LVL^.GetObjectCount(entranceCount, exitCount));
      rec.Statics.TerrainCount   := Word(LVL^.GetTerrainCount);
      rec.Statics.EntranceCount  := Byte(entranceCount);
      rec.Statics.ExitCount      := Byte(exitCount);

      rec.LevelTitle := lev.GetRawLVLTitle;
      rec.SetSourceFile(lev.SourceFileName);

      inc(header.LevelCount);
      inc(header.FileSize, SizeOf(rec));
      item := TLevelCacheItem.Create(aStyle.Name, rec);
      aTargetList.Add(item);
      fFlatList.Add(item);
      WriteLevelInfo(stream, Rec);
      //stream.WriteData(rec);
      lev := lev.Next;
    end;
    stream.Position := 0;
    stream.WriteData(header);
    stream.SaveToFile(filename);
  finally
    stream.Free;
  end;


end;

procedure TStyleCache.LoadStyleCache(const aStyleName: string; aTargetlist: TLevelCacheList);
// load all levelinfo from file
var
  filename: string;
  stream: TBufferedFileStream;
  header: THeaderRec;
  rec: TLevelRec;
begin

  filename := Consts.PathToCache + aStyleName + '.cache';
  if not FileExists(filename) then
    Throw('Cannot find cache: ' + filename);
  stream := TBufferedFileStream.Create(filename, fmOpenRead);
  try
    if not ReadHeader(stream, header) then
      Throw('Invalid cache: ' + filename + sLineBreak + 'Please delete this file and restart'); // todo: rewrite cache is needed
    while ReadLevelInfo(stream, rec) do begin
      var item: TLevelCacheItem := TLevelCacheItem.Create(aStyleName, rec);
      aTargetList.Add(item);
      fFlatList.Add(item);
    end;
  finally
    stream.Free;
  end;
end;

procedure TStyleCache.Load(const aFeedbackProc: TStyleCacheFeedbackProc);
const
  method = 'Load';
var
  oldStyleName: string;
  style: TStyle;
  list: TLevelCacheList;
begin

  oldStyleName := Consts.StyleName;
  fOnFeedback := aFeedbackProc;
  try
    Feedback('Loading');
    // remove user style caches which are obsolete (not found in Styles folder)
    if TDirectory.Exists(Consts.PathToCache) then begin
      var cacheFiles: TArray<string> := TDirectory.GetFiles(Consts.PathToCache, '*.cache');
      for var cachefile in cacheFiles do begin
        var check: string := ReplaceFileExt(ExtractFileName(cachefile), '');
        if Consts.IsUserStyle(check) and not TDirectory.Exists(Consts.PathToStyles + check) then
          TFile.Delete(cachefile);
      end;
    end;

    // build
    for var styleinfo: Consts.TStyleInformation in Consts.StyleInformationlist do begin
      list := TLevelCacheList.Create;
      fCache.Add(styleinfo.Name, list);
      // load if cache is up to date
      if not RebuildNeeded(styleinfo.Name) then
        LoadStyleCache(styleinfo.Name, list)
      // otherwise build and load
      else begin
        Feedback('Cache ' + styleinfo.Name);
        // only one style can be loaded in the program so here we trick the system
        Consts.SetStyleName(styleinfo.Name);
        style := TStyleFactory.CreateStyle(True);
        try
          BuildStyleCache(style, list);
        finally
          style.Free;
        end;
      end
    end;

    Feedback('Loading');
    // now cache levelhash + levelcodes + leveltitles in dictionaries for fast searching
    for var key: string in fCache.Keys do begin

      if not fCache.TryGetValue(key, list) or (list = nil) then
        Throw('Fatal error dictionary key not found (' + key + ') or unassigned list during loading', method);

      for var info: TLevelCacheItem in list do begin

        // hash
        list := nil;
        if not fHashCache.TryGetValue(info.LevelHash, list) then begin
          list := TLevelCacheList.Create(False); // this list does not own the items. ref only
          list.Add(info);
          fHashCache.Add(info.LevelHash, list);
        end
        else begin
          list.Add(info);
        end;

        // code
        list := nil;
        if not fCodeCache.TryGetValue(info.LevelCode, list) then begin
          list := TLevelCacheList.Create(False); // this list does not own the items. ref only
          list.Add(info);
          fCodeCache.Add(info.LevelCode, list);
        end
        else begin
          list.Add(info);
        end;

        // title
        list := nil;
        if not fTitleCache.TryGetValue(info.LevelTitle, list) then begin
          list := TLevelCacheList.Create(False); // this list does not own the items. ref only
          list.Add(info);
          fTitleCache.Add(info.levelTitle, list);
        end
        else begin
          list.Add(info);
        end;

        // exact
        //fExactCache.Add(TExactEntry.Create(key, info.SectionIndex, info.LevelIndex), info);

//        if not Assigned(FindExactLevel(key, info.SectionIndex, info.LevelIndex)) then
//          dlg('kut');
      end;

    end;


  finally
    fOnFeedback := nil;
    Consts.SetStyleName(oldStyleName); // restore
  end;
end;

function TStyleCache.FindLevelsByHash(const aHash: UInt64): TArray<TLevelCacheItem>;
var
  list: TLevelCacheList;
begin
  Result := nil;
  if fHashCache.TryGetValue(aHash, list) then
    Result := list.ToArray;
end;

//function TStyleCache.FindExactLevel(const aStyleName: string; aSectionIndex, aLevelIndex: Integer): TLevelCacheItem;
//begin
//  //fExactCache.TryGetValue(TExactEntry.Create(aStyleName, aSectionIndex, aLevelIndex), Result);
//end;

function TStyleCache.FindLevelsByCode(const aLevelCode: string): TArray<TLevelCacheItem>;
var
  list: TLevelCacheList;
begin
  Result := nil;
  if fCodeCache.TryGetValue(aLevelCode, list) then
    Result := list.ToArray;
end;

function TStyleCache.FindLevelsByTitle(const aLevelTitle: TLVLTitle): TArray<TLevelCacheItem>;
var
  list: TLevelCacheList;
begin
  Result := nil;
  if fTitleCache.TryGetValue(aLevelTitle, list) then
    Result := list.ToArray;
end;

function TStyleCache.GetLevelCount(const aStyleName: string): Integer;
var
  list: TLevelCacheList;
begin
  if fCache.TryGetValue(aStyleName, list) then
    Result := list.Count
  else
    Result := 0;
end;

function TStyleCache.GetTotalLevelCount: Integer;
begin
  Result := fFlatList.Count;
end;

procedure TStyleCache.ToCsv(const aFilename: string);
begin
//    showmessage('titlecachecount = ' + fTitleCache.Count.ToString + sLineBreak + 'codecache count = ' + fCodeCache.Count.ToString);
//
    const tab = Chr(9);
    var l: tstringlist := tstringlist.Create;
    l.add('INDEX' + tab + 'STYLE' + tab + 'SECTION' + tab + 'LEVEL' + tab + 'HASH' + tab + 'HASHDUPS' + tab + 'LEVELCODE' + tab + 'TITLE' + tab +
          'TITLEDUPS' + tab + 'SOURCE' + tab +
          'RELEASERATE' + tab +
          'LEMMINGSCOUNT' + tab +
          'RESCUECOUNT' + tab +
          'TIMELIMIT' + tab +
          'CLIMBERCOUNT' + tab +
          'FLOATERCOUNT' + tab +
          'BOMBERCOUNT' + tab +
          'BLOCKERCOUNT' + tab +
          'BUILDERCOUNT' + tab +
          'BASHERCOUNT' + tab +
          'MINERCOUNT' + tab +
          'DIGGERCOUNT' + tab +
          'GRAPHICSET' + tab +
          'GRAPHICSETEX' + tab +
          'SUPERLEMMING' + tab +
          'TERRAINCOUNT' + tab +
          'OBJECTCOUNT' + tab +
          'ENTRANCECOUNT' + tab +
          'EXITCOUNT'
        );
    var ix: Integer := 0;
    for var info: TLevelCacheItem in fflatList do begin
      var tmp: TFastObjectList<TLevelCacheItem>;
      var hashdups: Integer := 0;
      var titledups: Integer := 0;
      if fHashCache.TryGetValue(info.fLevelHash, tmp) then
        hashdups := tmp.Count;
      if fTitleCache.TryGetValue(info.fLevelTitle, tmp) then
        titledups := tmp.Count;
      l.add(ix.tostring + tab + info.StyleName + tab + info.SectionIndex.ToString + tab + info.LevelIndex.ToString + tab + IntToHex(info.LevelHash, 16) + tab +
        hashdups.tostring + tab + info.LevelCode + tab + '[' + info.GetTitleAsString + ']' + tab + titledups.ToString + tab + info.fSourceFile + tab +

        info.fStatics.ReleaseRate.ToString + tab +
        info.fStatics.LemmingsCount.ToString + tab +
        info.fStatics.RescueCount.ToString + tab +
        info.fStatics.TimeLimit.ToString + tab +
        info.fStatics.ClimberCount.ToString + tab +
        info.fStatics.FloaterCount.ToString + tab +
        info.fStatics.BomberCount.ToString + tab +
        info.fStatics.BlockerCount.ToString + tab +
        info.fStatics.BuilderCount.ToString + tab +
        info.fStatics.BasherCount.ToString + tab +
        info.fStatics.MinerCount.ToString + tab +
        info.fStatics.DiggerCount.ToString + tab +
        info.fStatics.GraphicSet.ToString + tab +
        info.fStatics.GraphicSetEx.ToString + tab +
        Byte(info.fStatics.SuperLemming).ToString + tab +
        info.fStatics.TerrainCount.ToString + tab +
        info.fStatics.ObjectCount.ToString + tab +
        info.fStatics.EntranceCount.ToString + tab +
        info.fStatics.ExitCount.ToString);
      Inc(ix);
    end;
    l.SaveToFile(ReplaceFileExt(aFileName, '.csv'));
    l.Free;

end;

function TStyleCache.GetCacheFilenames(fullName: Boolean): TArray<string>;
begin
  SetLength(Result, fCache.Count);
  var ix: Integer := 0;
  for var key: string in fCache.Keys do begin
    if fullName then
      Result[ix] := Consts.PathToCache + key + '.cache'
    else
      Result[ix] := key + '.cache';
    Inc(ix);
  end;
end;

end.
