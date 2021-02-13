unit Prog.Tools;

{$ifndef debug}
Prog.Tools only for debugging purposes
{$endif}

{$include lem_directives.inc}

// unit with some tools

interface

uses
  Gr32,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Dos.Structures,
  Base.Utils,
  Base.Bitmaps,
  Prog.Base,
  Styles.Base,
  Dos.Consts,
  Level.Base,
  Level.Loader,
  Meta.Structures,
  GameScreen.Base;

procedure GluePurpleFontToOneBitmap(src: TGameBaseScreen; const aFileName: string);
procedure ExportStyle(aStyle: TStyle);
procedure ConvertStyleToLVLOnly(aStyle: TStyle);

implementation

procedure GluePurpleFontToOneBitmap(src: TGameBaseScreen; const aFileName: string);
begin
  var bmp: TBitmap32 := TBitmap32.Create;
  bmp.Width := src.PurpleFont.fBitmaps[0].Width;
  bmp.Height := src.PurpleFont.fBitmaps[0].Height * Length(src.PurpleFont.fBitmaps);
  bmp.Clear(0);
  var y: Integer := 0;
  for var b: TBitmap32 in src.PurpleFont.fBitmaps do begin
    b.DrawTo(bmp, 0, y);
    Inc(y, b.Height);
  end;


  var H, S, L: Single;
  var p: PColor32;
  p := bmp.PixelPtr[0, 0];
  for var i := 0 to bmp.Width * bmp.Height - 1 do begin
    RGBtoHSL(p^, H, S, L);
    H := 100;
    p^ := HSLtoRGB(H, S, L);
    Inc(p);
  end;

  bmp.SaveToFile(aFileName);
  bmp.Free;
end;

function Quoted(const s: string): string;
begin
  Result := AnsiQuotedStr(s, '"');
end;

function TestFlag(const x, flag: Byte): string;
begin
  if x and flag = flag then Result := 'true' else Result := 'false';
end;

type
  TEntry = record
    graphicset: Integer;
    dos_id: Integer;
  end;

  TEntryDictionary = TDictionary<TEntry, Integer>;

const
  graphicsetnames: array[0..4] of string = ('Dirt', 'Fire', 'Marble', 'Pillar', 'Crystal'); // orig

procedure GraphicSetTilesToJson(startid: Integer; aSet: TGraphicSet; list: TStringList; islastset: Boolean);
var
  comma: string;
begin

//  list.Add('  tiles:');
  //list.Add('    [');
  for var i := 0 to aSet.TerrainBitmaps.Count - 1 do begin
    if i < aSet.TerrainBitmaps.Count - 1 then comma := ',' else comma := string.Empty;
    if not islastset then comma := ',';
    list.Add( '      {' +
      Quoted('Id') + ': ' + startid.ToString + ', ' +
      Quoted('GraphicSetId') + ': ' + aSet.GraphicSetId.ToString + ', ' +
      Quoted('Resolution') + ': 1, ' +
      Quoted('Image') + ': ' + Quoted('tile_' + startid.ToString.PadLeft(4, '0') + '_' + graphicsetnames[aSet.GraphicSetId] + '.png') +
      '}' + comma
    );
    inc(startid);
  end;
  //list.Add('    ]');
end;

procedure GraphicSetObjectsToJson(startid: Integer; aSet: TGraphicSet; list: TStringList; islastset: Boolean);
var
  comma: string;
begin
  for var i := 0 to aSet.MetaObjectList.Count - 1 do begin
    var obj: TMetaObject := aSet.MetaObjectList[i];
    if (i < aSet.MetaObjectList.Count - 1) then comma := ',' else comma := string.Empty;
    if not islastset then comma := ',';
    list.Add( '      {' +
      Quoted('Id') + ': ' + startid.ToString + ', ' +
      Quoted('GraphicSetId') + ': ' + aSet.GraphicSetId.ToString + ', ' +
      Quoted('TriggerEffectId') + ': '  + obj.TriggerEffect.ToString + ',' +
      Quoted('SoundEffectId')  + ': ' + obj.SoundEffect.ToString + ', ' +

      Quoted('Resolution') + ': 1, ' +
      Quoted('AnimationType')  + ': ' + obj.AnimationType.ToString + ', ' +
      Quoted('FrameCount')  + ': ' + obj.AnimationFrameCount.ToString + ', ' +
      Quoted('StartAnimationFrameIndex')  + ': ' + obj.StartAnimationFrameIndex.ToString + ', ' +
      Quoted('PreviewFrameIndex')  + ': ' + obj.PreviewFrameIndex.ToString + ', ' +

//   fTriggerLeft              := obj.oTrigger_left * 4; // encoded
//  fTriggerTop               := obj.oTrigger_top * 4 - 4; // encoded
//  fTriggerWidth             := obj.oTrigger_width * 4; // encoded
//  fTriggerHeight            := obj.oTrigger_height * 4; // encoded

      Quoted('TriggerLeft')  + ': ' + (obj.TriggerLeft).ToString + ', ' +
      Quoted('TriggerTop')  + ': ' + (obj.TriggerTop).ToString + ', ' +
      Quoted('TriggerWidth')  + ': ' + (obj.TriggerWidth).ToString + ', ' +
      Quoted('TriggerHeight')  + ': ' + (obj.TriggerHeight).ToString + ', ' +


      Quoted('Image') + ': ' + Quoted('object_' + startid.ToString.PadLeft(4, '0') + '_' + graphicsetnames[aSet.GraphicSetId] + '.png') +
      '}' + comma
    );
    inc(startid);
  end;
  {"id": 0, "graphicset_id": 0, "resolution": 1, "effect_id": 1, "triggerleft": 20, "triggertop": 20, "triggerwidth": 20, "triggerheight": 20, "image": "object_0000_Dirt.png"}

  //list.Add('    ]');
end;

procedure LevelToJson(aLevel: TLevel; const aFilename: string);
var
  c: string;
begin

//  odf_OnlyOnTerrain = 1; // bit 0
//  odf_UpsideDown    = 2; // bit 1
//  odf_NoOverwrite   = 4; // bit 2

  var list: TStringList := tstringlist.Create;
  list.Add('{');
  list.Add('  ' + Quoted('Title') + ': ' + Quoted(aLevel.Info.Title.Trim) + ',');

  List.Add('  ' + Quoted('Width') + ': ' + 1584.ToString + ',');
  list.Add('  ' + Quoted('Height') + ': ' + 160.ToString + ',');
  list.Add('  ' + Quoted('Releaserate') + ': ' + aLevel.Info.ReleaseRate.ToString + ',');
  list.Add('  ' + Quoted('Seconds') + ': ' + (aLevel.Info.TimeLimit * 60).ToString + ',');
  list.Add('  ' + Quoted('Startx') + ': ' + aLevel.Info.ScreenPosition.ToString + ',');
  list.Add('  ' + Quoted('Starty') + ': 0,');
  list.Add('  ' + Quoted('Speed') + ': 1,');

  List.Add('  ' + Quoted('LemmingCount') + ': ' + aLevel.Info.LemmingsCount.ToString + ',');
  list.Add('  ' + Quoted('RescueCount') + ': ' + aLevel.Info.RescueCount.ToString + ',');
  list.Add('  ' + Quoted('ClimberCount') + ': ' + aLevel.Info.ClimberCount.ToString + ',');
  list.Add('  ' + Quoted('FloaterCount') + ': ' + aLevel.Info.FloaterCount.ToString + ',');
  list.Add('  ' + Quoted('BomberCount') + ': ' + aLevel.Info.BomberCount.ToString + ',');
  list.Add('  ' + Quoted('BlockerCount') + ': ' + aLevel.Info.BlockerCount.ToString + ',');
  list.Add('  ' + Quoted('BuilderCount') + ': ' + aLevel.Info.BuilderCount.ToString + ',');
  list.Add('  ' + Quoted('BasherCount') + ': ' + aLevel.Info.BasherCount.ToString + ',');
  list.Add('  ' + Quoted('MinerCount') + ': ' + aLevel.Info.MinerCount.ToString + ',');
  list.Add('  ' + Quoted('DiggerCount') + ': ' + aLevel.Info.DiggerCount.ToString + ',');

  list.Add('  ' + Quoted('Objects') + ': ');
  list.Add('    [');
  for var o: TInteractiveObject in aLevel.InteractiveObjects do begin
    if o = aLevel.InteractiveObjects.Last then c := string.Empty else c := ',';
    list.Add('      {' +
      Quoted('ObjectId') + ': ' + (aLevel.Info.GraphicSet * 1000 + o.Identifier).ToString + ', ' +
      Quoted('X') + ': ' + o.Left.ToString + ', ' +
      Quoted('Y') + ': ' + o.Top.ToString + ', ' +
      Quoted('Rotation') + ': 0,' +
      Quoted('OnlyOnTerrain') + ': ' + TestFlag(o.DrawingFlags, odf_OnlyOnTerrain) + ', ' +
      Quoted('NoOverwrite') +  ': ' + TestFlag(o.DrawingFlags, odf_NoOverwrite) + ', ' +
      Quoted('FlipVert') +  ': ' + TestFlag(o.DrawingFlags, odf_UpsideDown) + ', ' +
      Quoted('FlipHorz') + ': ' + TestFlag(0, 1) +
      '}' + c);
  end;
  list.Add('    ],');

  list.Add('  ' + Quoted('Terrains') + ': ');
  list.Add('    [');
  for var t: TTerrain in aLevel.Terrains do begin
    if t = aLevel.Terrains.Last then c := string.Empty else c := ',';
    list.Add('      {' +
      Quoted('TerrainId') + ': ' + (aLevel.Info.GraphicSet * 1000 + t.Identifier).ToString + ', ' +
      Quoted('X') + ': ' + t.Left.ToString + ', ' +
      Quoted('Y') + ': ' + t.Top.ToString + ', ' +
      Quoted('Rotation') + ': 0,' +
      Quoted('Erase') +  ': ' + TestFlag(t.DrawingFlags, tdf_Erase) + ', ' +
      Quoted('NoOverwrite') + ': ' +  TestFlag(t.DrawingFlags, tdf_NoOverwrite) + ', ' +
      Quoted('FlipVert') +  ': ' + TestFlag(t.DrawingFlags, tdf_Invert) + ', ' +
      Quoted('FlipHorz') + ': ' + TestFlag(0, 1) +
      '}' + c);
  end;
  list.Add('    ],');

  list.Add('  ' + Quoted('Steels') + ': ');
  list.Add('    [');
  for var s: TSteel in aLevel.Steels do begin
    if s = aLevel.Steels.Last then c := string.Empty else c := ',';
    list.Add('      {' +
      Quoted('X') + ': ' + s.Left.ToString + ', ' +
      Quoted('Y') + ': ' + s.Top.ToString + ', ' +
      Quoted('Width') + ': ' + s.Width.ToString + ', ' +
      Quoted('Height') + ': ' + s.Height.ToString +
      '}' + c);
  end;
  list.Add('    ]');

  list.Add('}');
  list.SaveToFile(aFileName);
  list.Free;
end;



procedure ExportStyle(aStyle: TStyle);
var
  globaltileoffsets: array[0..8] of Integer;
  tileoffset: Integer;
  path: string;
begin

  tobject.throw('ExportStyle not implemented yet');
  // todo: delete enum.asstring

  path := Consts.PathToTemp + IncludeTrailingPathDelimiter(aStyle.Name) + IncludeTrailingPathDelimiter('Export');
  ForceDir(path);

  var metalist: TStringList := TStringList.Create;
  metalist.Add('{');

  for var act: TLemmingAction := TLemmingAction.Walking to TLemmingAction.Exploding do begin
    for var dir := TLemmingAnimationSet.LTR to TLemmingAnimationSet.RTL do begin
      var ix: Integer := TLemmingAnimationSet.AnimationIndices[act, dir];
      var bmp := aStyle.LemmingAnimationSet.LemmingBitmaps[ix];
// todo: here
      var name: string := 'krak'; //act.AsString;// Enum.AsString(act);
      if dir = TLemmingAnimationSet.LTR then name := name + '_LTR.png' else name := name + '_RTL.png';
      bmp.SaveToPng(path + name, TPngMode.BlackIsTransparent);
    end;
  end;

  // save tile and object images
  fillchar(globaltileoffsets, Sizeof(globaltileoffsets), 0);
  tileoffset := 0;

  var grCount: Integer := 4;

  // images
  for var gid := 0 to grCount - 1 do begin // todo use count graphicsets
    var graphicset: TGraphicSet := TGraphicSet.Create(aStyle);
    graphicset.Load(gid, -1);
    globaltileoffsets[gid] := tileoffset;
    var ix: Integer := 0;
    for var bmp: TBitmap32 in graphicset.TerrainBitmaps do begin
      bmp.SaveToPng(path + 'tile_' + (gid * 1000 + ix).ToString.PadLeft(4, '0') + '_' + graphicsetnames[gid] +'.png', TPngMode.BlackIsTransparent);
      Inc(ix);
    end;
    ix := 0;
    for var bmp: TBitmap32 in graphicset.ObjectBitmaps do begin
      bmp.SaveToPng(path + 'object_' + (gid * 1000 + ix).ToString.PadLeft(4, '0') + '_' + graphicsetnames[gid] +'.png', TPngMode.BlackIsTransparent);
      Inc(ix);
    end;
    graphicset.Free;
  end;

  // metadata tiles json
  metalist.Add('  "Terrains":');
  metalist.Add('    [');
  for var gid := 0 to grCount - 1 do begin
    var graphicset: TGraphicSet := TGraphicSet.Create(aStyle);
    graphicset.Load(gid, -1);
    globaltileoffsets[gid] := tileoffset;
    GraphicSetTilesToJson(gid * 1000, graphicset, metalist, gid=4);
    graphicset.Free;
  end;
  metalist.Add('    ],');

  // metadata objects json
  metalist.Add('  "Objects":');
  metalist.Add('    [');
  for var gid := 0 to grCount - 1 do begin
    var graphicset: TGraphicSet := TGraphicSet.Create(aStyle);
    graphicset.Load(gid, -1);
    globaltileoffsets[gid] := tileoffset;
    GraphicSetObjectsToJson(gid * 1000, graphicset, metalist, gid=4);
    graphicset.Free;
  end;
  metalist.Add('    ]');


//  var levelinfo: TLevelLoadingInformation := aStyle.LevelSystem.FirstLevel;
  for var section: TSection in aStyle.LevelSystem.SectionList do begin
    for var levelinfo : TLevelLoadingInformation in section.LevelLoadingInformationList do begin
      var lev: TLevel := TLevel.Create;
      levelinfo.LoadLevel(lev);
      LevelToJson(lev, path + section.SectionName + '_' + levelinfo.LevelIndex.ToString + '.json');
      lev.Free;
    end;
  end;
  //  var level: TLevel

  metalist.Add('}');
  metalist.SaveToFile(path + 'metadata.json');
  metalist.Free;

end;

procedure ConvertStyleToLVLOnly(aStyle: TStyle);
var
  path: string;
begin
  path := Consts.PathToTemp + IncludeTrailingPathDelimiter(aStyle.Name) + IncludeTrailingPathDelimiter('LVL');
  if not ForceDir(path) then
    TObject.Throw('Cannot create path ' + path, 'ConvertStyleToLVLOnly');
  var ix: Integer := 0;
  for var section: TSection in aStyle.LevelSystem.SectionList do
    for var info: TLevelLoadingInformation in section.LevelLoadingInformationList do begin
      var LVL: TLVLRec := info.GetRawLVL;
      var ext := ExtractFileExt(info.SourceFileName).Replace('.', string.Empty);
      TLevelLoader.SaveLVLToFile(path + 'level_' + ix.ToString.PadLeft(4,'0') +'_' + ExtractFileName(info.SourceFileName) + '.lvl', LVL);
      Inc(ix);
    end;
end;

end.

