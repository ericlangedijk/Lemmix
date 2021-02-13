unit Level.Loader;

{$include lem_directives.inc}

interface

uses
  Classes, System.SysUtils, System.Contnrs, System.Math,
  Base.Utils,
  Dos.Consts, Dos.Structures,
  Prog.Base,
  Level.Base;

type
  TLevelLoader = class sealed
  private
    class function SwapWord(W: Word): Word; static; inline; // for translating
  public
    class procedure TranslateLevel(const LVL: TLVLRec; aLevel: TLevel); overload; static;
    class procedure TranslateLevel(aLevel: TLevel; var LVL: TLVLRec); overload; static;

    class procedure LoadLVLFromFile(const aFileName: string; var LVL: TLVLRec); static;
    class procedure LoadLVLFromStream(aStream: TStream; var LVL: TLVLRec); static;

    class procedure SaveLVLToFile(const aFilename: string; const LVL: TLVLRec); static;
    class procedure SaveLVLToStream(aStream: TStream; const LVL: TLVLRec); static;
  end;

  TLemminiLoader = class
  public
    class procedure LoadLVLFromFile(const aFileName: string; var LVL: TLVLRec); static;
  end;

implementation

{ TLevelLoader }

class function TLevelLoader.SwapWord(W: Word): Word;
begin
  Result := System.Swap(W);
end;

class procedure TLevelLoader.LoadLVLFromFile(const aFileName: string; var LVL: TLVLRec);
begin
  var f: TBufferedFileStream := TBufferedFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadLVLFromStream(f, LVL);
  finally
    f.Free;
  end;
end;

class procedure TLevelLoader.LoadLVLFromStream(aStream: TStream; var LVL: TLVLRec);
begin
  FillChar(LVL, SizeOf(LVL), 0);
  aStream.ReadBuffer(LVL, LVL_SIZE);
end;

class procedure TLevelLoader.SaveLVLToFile(const aFilename: string; const LVL: TLVLRec);
begin
  var f: TBufferedFileStream := TBufferedFileStream.Create(aFileName, fmCreate);
  try
    SaveLVLToStream(f, LVL);
  finally
    f.Free;
  end;
end;

class procedure TLevelLoader.SaveLVLToStream(aStream: TStream; const LVL: TLVLRec);
begin
  aStream.WriteData(LVL);
end;

class procedure TLevelLoader.TranslateLevel(const LVL: TLVLRec; aLevel: TLevel);
//  Translate a LVL file to a lemmix level and fill the lists. For decoding and technical details see documentation or read the code :)
var
  H, i: Integer;
  O: TLVLObject;
  T: TLVLTerrain;
  S: TLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;
begin
  aLevel.ClearLevel;
  {-------------------------------------------------------------------------------
    Get the statics. This is easy
  -------------------------------------------------------------------------------}
  aLevel.Info.ReleaseRate      := SwapWord(LVL.ReleaseRate);
  aLevel.Info.LemmingsCount    := SwapWord(LVL.LemmingsCount);
  aLevel.Info.RescueCount      := SwapWord(LVL.RescueCount);
  aLevel.Info.TimeLimit        := SwapWord(LVL.TimeLimit);
  aLevel.Info.ClimberCount     := SwapWord(LVL.ClimberCount);
  aLevel.Info.FloaterCount     := SwapWord(LVL.FloaterCount);
  aLevel.Info.BomberCount      := SwapWord(LVL.BomberCount);
  aLevel.Info.BlockerCount     := SwapWord(LVL.BlockerCount);
  aLevel.Info.BuilderCount     := SwapWord(LVL.BuilderCount);
  aLevel.Info.BasherCount      := SwapWord(LVL.BasherCount);
  aLevel.Info.MinerCount       := SwapWord(LVL.MinerCount);
  aLevel.Info.DiggerCount      := SwapWord(LVL.DiggerCount);
  aLevel.Info.ScreenPosition   := SwapWord(LVL.ScreenPosition);
  aLevel.Info.GraphicSet       := SwapWord(LVL.GraphicSet);
  aLevel.Info.GraphicSetEx     := SwapWord(LVL.GraphicSetEx);
  aLevel.Info.SuperLemming     := (LVL.Reserved = $FFFF);
  aLevel.Info.Title            := string(LVL.LevelName);

  {-------------------------------------------------------------------------------
    Get the objects
  -------------------------------------------------------------------------------}
  for i := 0 to LVL_MAXOBJECTCOUNT - 1 do begin
    O := LVL.Objects[i];
    { TODO : player skip empty objects but continue reading. editor: insert fake objects because of enabled or disabled }
    if O.AsInt64 = 0 then
      Continue;
    Obj := TInteractiveObject.Create;
    aLevel.InteractiveObjects.Add(Obj);
    Obj.Left := Integer(O.B0) shl 8 + Integer(O.B1) - 16;
    Obj.Top := Integer(O.B2) shl 8 + Integer(O.B3);
    Obj.Identifier := Integer(O.B5 and 15);
    if O.Modifier and $80 <> 0 then
      Obj.DrawingFlags := Obj.DrawingFlags or odf_NoOverwrite;
    if O.Modifier and $40 <> 0 then
      Obj.DrawingFlags := Obj.DrawingFlags or odf_OnlyOnTerrain;
    if O.DisplayMode = $8F then
      Obj.DrawingFlags := Obj.DrawingFlags or odf_UpsideDown;
  end;

  {-------------------------------------------------------------------------------
    Get the terrain.
  -------------------------------------------------------------------------------}
  for i := 0 to LVL_MAXTERRAINCOUNT - 1 do begin
    T := LVL.Terrain[i];
    if T.D0 = $FFFFFFFF then
      Continue;
    Ter := TTerrain.Create;
    aLevel.Terrains.Add(Ter);
    Ter.Left := Integer(T.B0 and 15) shl 8 + Integer(T.B1) - 16; // 9 bits
    Ter.DrawingFlags := T.B0 shr 5; // the bits are compatible with the Lemmix DrawingFlags
    H := Integer(T.B2) shl 1 + Integer(T.B3 and $80) shr 7;
    if H >= 256 then
      Dec(H, 512);
    Dec(H, 4);
    Ter.Top := H;
    Ter.Identifier := T.B3 and 63; // max = 63.  bit7 belongs to ypos
  end;

  {-------------------------------------------------------------------------------
    Get the steel.
  -------------------------------------------------------------------------------}
  for i := 0 to LVL_MAXSTEELCOUNT - 1 do begin
    S := LVL.Steel[i];
    if S.D0 = 0 then
      Continue;
    Steel := TSteel.Create;
    aLevel.Steels.Add(Steel);
    Steel.Left := ((Integer(S.B0) shl 1) + (Integer(S.B1 and Bit7) shr 7)) * 4 - 16;  // 9 bits
    Steel.Top := Integer(S.B1 and not Bit7) * 4;  // bit 7 belongs to steelx
    Steel.Width := Integer(S.B2 shr 4) * 4 + 4;  // first nibble bits 4..7 is width in units of 4 pixels (and then add 4)
    Steel.Height := Integer(S.B2 and $F) * 4 + 4;  // second nibble bits 0..3 is height in units of 4 pixels (and then add 4)
  end;

  // repair nonsense in case anybody messed around with LVL files or worse
  aLevel.Info.ReleaseRate      := EnsureRange(aLevel.Info.ReleaseRate, 1, 99);
  aLevel.Info.LemmingsCount    := EnsureRange(aLevel.Info.LemmingsCount, 1, 255); // ok ok
  aLevel.Info.RescueCount      := EnsureRange(aLevel.Info.RescueCount, 1, aLevel.Info.LemmingsCount);
  aLevel.Info.TimeLimit        := EnsureRange(aLevel.Info.TimeLimit, 1, 99); // #EL support for max 99 minutes
  aLevel.Info.ClimberCount     := EnsureRange(aLevel.Info.ClimberCount, 0, 99);
  aLevel.Info.FloaterCount     := EnsureRange(aLevel.Info.FloaterCount, 0, 99);
  aLevel.Info.BomberCount      := EnsureRange(aLevel.Info.BomberCount, 0, 99);
  aLevel.Info.BlockerCount     := EnsureRange(aLevel.Info.BlockerCount, 0, 99);
  aLevel.Info.BuilderCount     := EnsureRange(aLevel.Info.BuilderCount, 0, 99);
  aLevel.Info.BasherCount      := EnsureRange(aLevel.Info.BasherCount, 0, 99);
  aLevel.Info.MinerCount       := EnsureRange(aLevel.Info.MinerCount, 0, 99);
  aLevel.Info.DiggerCount      := EnsureRange(aLevel.Info.DiggerCount, 0, 99);
  aLevel.Info.ScreenPosition   := EnsureRange(aLevel.Info.ScreenPosition, -200, GAME_BMPWIDTH - 200);
  aLevel.Info.GraphicSet       := EnsureRange(aLevel.Info.GraphicSet, 0, 9);
  aLevel.Info.GraphicSetEx     := EnsureRange(aLevel.Info.GraphicSetEx, 0, 9);

  // todo: find the best solution for this
  if aLevel.Info.GraphicSetEx > 4 then
    aLevel.Info.GraphicSetEx := 1;
end;

class procedure TLevelLoader.TranslateLevel(aLevel: TLevel; var LVL: TLVLRec);
var
  Int16: SmallInt; Int32: Integer;
  H, i: Integer;
  M: Byte;
  W: Word;
  O: ^TLVLObject;
  T: ^TLVLTerrain;
  S: ^TLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;
begin

  FillChar(LVL, SizeOf(LVL), 0);
  FillChar(LVL.Terrain, Sizeof(LVL.Terrain), $FF);
  FillChar(LVL.LevelName, 32, ' ');

  {-------------------------------------------------------------------------------
    Set the statics.
  -------------------------------------------------------------------------------}
  LVL.ReleaseRate                    := aLevel.Info.ReleaseRate;
  LVL.LemmingsCount                  := aLevel.Info.LemmingsCount;
  LVL.RescueCount                    := aLevel.Info.RescueCount;
  LVL.TimeLimit                      := aLevel.Info.TimeLimit;
  LVL.ClimberCount                   := aLevel.Info.ClimberCount;
  LVL.FloaterCount                   := aLevel.Info.FloaterCount;
  LVL.BomberCount                    := aLevel.Info.BomberCount;
  LVL.BlockerCount                   := aLevel.Info.BlockerCount;
  LVL.BuilderCount                   := aLevel.Info.BuilderCount;
  LVL.BasherCount                    := aLevel.Info.BasherCount;
  LVL.MinerCount                     := aLevel.Info.MinerCount;
  LVL.DiggerCount                    := aLevel.Info.DiggerCount;
  LVL.ScreenPosition                 := aLevel.Info.ScreenPosition;
  LVL.GraphicSet                     := aLevel.Info.GraphicSet;
  LVL.GraphicSetEx                   := aLevel.Info.GraphicSetEx;
  // swap
  LVL.ReleaseRate                    := SwapWord(LVL.ReleaseRate);
  LVL.LemmingsCount                  := SwapWord(LVL.LemmingsCount);
  LVL.RescueCount                    := SwapWord(LVL.RescueCount);
  LVL.TimeLimit                      := SwapWord(LVL.TimeLimit);
  LVL.ClimberCount                   := SwapWord(LVL.ClimberCount);
  LVL.FloaterCount                   := SwapWord(LVL.FloaterCount);
  LVL.BomberCount                    := SwapWord(LVL.BomberCount);
  LVL.BlockerCount                   := SwapWord(LVL.BlockerCount);
  LVL.BuilderCount                   := SwapWord(LVL.BuilderCount);
  LVL.BasherCount                    := SwapWord(LVL.BasherCount);
  LVL.MinerCount                     := SwapWord(LVL.MinerCount);
  LVL.DiggerCount                    := SwapWord(LVL.DiggerCount);
  LVL.ScreenPosition                 := SwapWord(LVL.ScreenPosition);
  LVL.GraphicSet                     := SwapWord(LVL.GraphicSet);
  LVL.GraphicSetEx                   := SwapWord(LVL.GraphicSetEx);
  // encode superlemming
 if aLevel.Info.SuperLemming then
   LVL.Reserved := $FFFF;

  for i := 1 to aLevel.Info.Title.Length do begin
    var C: AnsiChar := AnsiChar(aLevel.Info.Title[i]);
    LVL.LevelName[i - 1] := C;
    if i > 31 then
      BReak;
  end;

  {-------------------------------------------------------------------------------
    Set the objects.
  -------------------------------------------------------------------------------}
  for i := 0 to aLevel.InteractiveObjects.Count - 1 do
  begin
    Obj := aLevel.InteractiveObjects[i];
    O := @LVL.Objects[i];
    // set xpos: revert the misery
    Int16 := Obj.Left;
    Inc(Int16, 16);
    W := Word(Int16);
    W := System.Swap(W);
    O^.XPos := W;
    // set ypos: revert the misery
    Int16 := Obj.Top;
    W := Word(Int16);
    W := System.Swap(W);
    O^.Ypos := W;
    // set object id
    WordRec(O^.ObjectID).Hi := Byte(Obj.Identifier);
    // set modifier
    if odf_NoOverwrite and Obj.DrawingFlags <> 0 then
      O^.Modifier := $80
    else if odf_OnlyOnTerrain and Obj.DrawingFlags <> 0 then
      O^.Modifier := $40;
    // set displaymode
    if odf_UpsideDown and Obj.DrawingFlags <> 0 then
      O^.DisplayMode := $8F
    else
      O^.DisplayMode := $0F; {default}
  end;

  {-------------------------------------------------------------------------------
    set the terrain
  -------------------------------------------------------------------------------}
  for i := 0 to aLevel.Terrains.Count - 1 do
  begin
    Ter := aLevel.Terrains[i];
    T := @LVL.Terrain[i];

    // GET: TerrainX := Integer(T.B0 and 15) shl 8 + Integer(T.B1) - 16;
    H := Ter.Left;
    Inc(H, 16);
    T.B0 := Byte(H shr 8);
    T.B1 := Byte(H);// + 16;

    // GET: TerrainDrawingFlags := TTerrainDrawingFlags(Byte(T.B0 shr 5));
    M := Byte(Ter.DrawingFlags) shl 5;
    T.B0 := T.B0 or M;
    // GET:
      (*
      H := Integer(T.B2) shl 1 + Integer(T.B3 and $80) shr 7;
      if H >= 256 then
        Dec(H, 512);
      Dec(H, 4);
      TerrainY := Map(H);
      *)
    H := Ter.Top;
    Inc(H, 4);
    if H < 0 then
      Inc(H, 512);
    T.B3 := Byte((H or Bit8) shl 7); // todo: still don't know if this is right this "or bit8" (although it worked from 2006-2020)
    // H := H and not Bit8;
    H := H shr 1;
    T.B2 := Byte(H);
    // GET: TerrainId := T.B3 and 63; // max = 63.
    T.B3 := T.B3 or (Byte(Ter.Identifier) and 63);

  end;

  {-------------------------------------------------------------------------------
    set the steel.
  -------------------------------------------------------------------------------}
  for i := 0 to aLevel.Steels.Count - 1 do
  begin
    Steel := aLevel.Steels[i];
    S := @LVL.Steel[i];
    // GET: SteelX := ((Integer(B0) shl 1) + (Integer(B1 and Bit7) shr 7)) * 4 - 16;
    Int32 := Steel.Left;
    Int32 := (Int32 + 16) div 4;
    S^.B1 := Byte((Int32 or Bit8) shl 7); // still don't know this "or bit8"
    // Int32 := Int32 and not Bit8; <-- I THINK THIS WAS WRONG!!
    Int32 := Int32 shr 1;
    S^.B0 := Byte(Int32);
    // GET: SteelY := Integer(S.B1 and not Bit7) * 4;
    Int32 := Steel.Top div 4;
    S^.B1 := S^.B1 or (Byte(Int32) and not Bit7);
    // GET: SteelWidth  := Integer(S.B2 shr 4) * 4 + 4;
    Int32 := (Steel.Width - 4) div 4;
    S^.B2 := Byte(Int32) shl 4;
    // GET: SteelHeight := Integer(S.B2 and $F) * 4 + 4;
    Int32 := (Steel.Height - 4) div 4;
    S^.B2 := S^.B2 or (Byte(Int32) and not $F0); // highest bit set already
  end;
end;

{ TLemminiLoader }

class procedure TLemminiLoader.LoadLVLFromFile(const aFileName: string; var LVL: TLVLRec);
const
  method = 'LoadLVLFromFile';
var
  level: TLevel;
  list: TStringList;
  line, key, value: string;
  equalsPos: Integer;
  lineResult: Boolean;

    procedure Error(const s: string);
    begin
      Throw('Error in Lemmini file: ' + CRLF + CRLF + s, method);
    end;

    function ValueToInts(count: Integer; var ar: TArray<Integer>): Boolean;
    // split comma-separated values
    begin
      var strings: TArray<string> := value.Split([',']);
      Result := strings.Length = count;
      if strings.Length <> count then
        Exit;
      SetLength(ar, count);
      for var i := 0 to strings.Length - 1 do begin
        var s := strings[i].Trim;
        if not TryStrToInt(s, ar[i]) then begin
          SetLength(ar, 0);
          Exit(False);
        end;
      end;
      Result := True;
    end;

    function ValueToInt(out i: Integer): Boolean;
    // get one value
    begin
      Result := TryStrToInt(value, i);
    end;

    function TileKeyNumber(out i: Integer): Boolean;
    // extract number from terrain_x, ojbect_x, steel_x
    var
      p: Integer;
    begin
      p := Pos('_', key);
      if p >= 0 then
        Result := TryStrToInt(Copy(key, P + 1, key.Length), i)
      else
        Result := False;
    end;

    function TryInfo: Boolean;
    var
      v: Integer;
    begin
      Result := False;
      if key.StartsWith('releaseRate', True) and ValueToInt(v) then begin level.Info.ReleaseRate := v; Exit(True); end;
      if key.StartsWith('numLemmings', True) and ValueToInt(v) then begin level.Info.LemmingsCount := v; Exit(True); end;
      if key.StartsWith('numToRescue', True) and ValueToInt(v) then begin level.Info.RescueCount := v; Exit(True); end;
      if key.StartsWith('timeLimit', True) and ValueToInt(v) then begin level.Info.TimeLimit := v; Exit(True); end;
      if key.StartsWith('numClimbers', True) and ValueToInt(v) then begin level.Info.ClimberCount := v; Exit(True); end;
      if key.StartsWith('numFloaters', True) and ValueToInt(v) then begin level.Info.FloaterCount := v; Exit(True); end;
      if key.StartsWith('numBombers', True) and ValueToInt(v) then begin level.Info.BomberCount := v; Exit(True); end;
      if key.StartsWith('numBlockers', True) and ValueToInt(v) then begin level.Info.BlockerCount := v; Exit(True); end;
      if key.StartsWith('numBuilders', True) and ValueToInt(v) then begin level.Info.BuilderCount := v; Exit(True); end;
      if key.StartsWith('numBashers', True) and ValueToInt(v) then begin level.Info.BasherCount := v; Exit(True); end;
      if key.StartsWith('numMiners', True) and ValueToInt(v) then begin level.Info.MinerCount := v; Exit(True); end;
      if key.StartsWith('numDiggers', True) and ValueToInt(v) then begin level.Info.DiggerCount := v; Exit(True); end;
      if key.StartsWith('xPos', True) and ValueToInt(v) then begin level.Info.ScreenPosition := v div 2; Exit(True); end;
      if key.StartsWith('style', True) then begin level.Info.GraphicSet := Consts.GraphicSetNameToGraphicSet(value); Exit(True); end;
    end;


    function TryTerrain: Boolean;
    // id, xpos, ypos, modifier
    // terrain_0 = 0, 1286, 260, 0
    // modifier: 8=NO_OVERWRITE, 4=UPSIDE_DOWN, 2=REMOVE (combining allowed, 0=FULL)
    var
      ar: TArray<Integer>;
      flags: Integer;
    begin
      Result := False;
      if level.Terrains.Count >= LVL_MAXTERRAINCOUNT then
        Exit;
      if key.StartsWith('terrain_', True) and ValueToInts(4, ar) then begin
        var ter := TTerrain.Create;
        level.Terrains.Add(ter);
        ter.Identifier := ar[0];
        ter.Left := ar[1] div 2;
        ter.Top := ar[2] div 2;
        ter.DrawingFlags := 0;
        flags := ar[3];
        if flags and 2 <> 0 then
          ter.DrawingFlags := ter.DrawingFlags or tdf_Erase;
        if flags and 4 <> 0 then
          ter.DrawingFlags := ter.DrawingFlags or tdf_Invert;
        if flags and 8 <> 0 then
          ter.DrawingFlags := ter.DrawingFlags or tdf_NoOverwrite;
        Result := True;
      end;
    end;

    function TryObject: Boolean;
    // id, xpos, ypos, paintmode, upsidedown
    // object_0 = 0, 1680, 216, 4, 0
    // modifier 8=VIS_ON_TERRAIN, 4=NO_OVERWRITE, 0=FULL (only one value possible)
    var
      ar: TArray<Integer>;
      flags: Integer;
    begin
      Result := False;
      if level.InteractiveObjects.Count >= LVL_MAXOBJECTCOUNT then
        Exit;
      if key.StartsWith('object_', True) and ValueToInts(5, ar) then begin
        var ix: Integer;
        if TileKeyNumber(ix) then begin
          var obj := TInteractiveObject.Create;
          level.InteractiveObjects.Add(obj);
          obj.Identifier := ar[0];
          obj.Left := (ar[1] div 2) and not 7; // dos-alignment
          obj.Top := ar[2] div 2;
          obj.DrawingFlags := 0;
          flags := ar[3];
          case flags of
            4: obj.DrawingFlags := obj.DrawingFlags or odf_NoOverwrite;
            8: obj.DrawingFlags := obj.DrawingFlags or odf_OnlyOnTerrain;
          end;
          flags := ar[4];
          if flags = 1 then
            obj.DrawingFlags := obj.DrawingFlags or odf_UpsideDown;
          Result := True;
        end;
      end;
    end;

    function TrySteel: Boolean;
    // id, xpos, ypos, width, height
    // steel_0 = 1568, 176, 128, 32
    var
      ar: TArray<Integer>;
    begin
      Result := False;
      if level.Steels.Count >= LVL_MAXSTEELCOUNT then
        Exit;
      if key.StartsWith('steel_', True) and ValueToInts(4, ar) then begin
        var ix: Integer;
        if TileKeyNumber(ix) and ValueToInts(4, ar) then begin
          var steel := TSteel.Create;
          level.Steels.Add(steel);
          steel.Left := (ar[0] div 2) and not 3; // dos-align
          steel.Top := (ar[1] div 2) and not 3; // dos-align
          steel.Width := Max((ar[2] div 2) and not 3, 4); // dos-align
          steel.Height := Max((ar[3] div 2) and not 3, 4); // dos-align
          Result := True;
        end;
      end;
    end;

    function TryName: Boolean;
    begin
      if key.StartsWith('name', True) then begin
        level.Info.Title := value;
        Result := True;
      end
      else
        Result := False;
    end;

    function TryIgnore: Boolean;
    begin
      Result := (key <> string.Empty) or (value <> string.Empty);
    end;

    procedure Validate;
    begin
      // a little bit of forgiveness
      if level.Info.ReleaseRate = 0 then
        level.Info.ReleaseRate := 1;

      if (level.Info.ReleaseRate <= 0) or (level.Info.ReleaseRate > 99) then Error('Invalid ReleaseRate ' + level.Info.ReleaseRate.ToString);
      if (level.Info.LemmingsCount <= 0) or(level.Info.LemmingsCount > 255) then Error('Invalid LemmingsCount' + level.Info.LemmingsCount.ToString);
      if (level.Info.RescueCount <= 0) or (level.Info.RescueCount > level.Info.LemmingsCount) then Error('Invalid RescueCount' + level.Info.RescueCount.ToString);
      if (level.Info.TimeLimit <= 0) or (level.Info.TimeLimit > 9) then Error('Invalid TimeLimit ' + level.Info.TimeLimit.ToString);
      if (level.Info.ClimberCount < 0) or (level.Info.ClimberCount > 99) then Error('Invalid ClimberCount ' + level.Info.ClimberCount.ToString);
      if (level.Info.FloaterCount < 0) or (level.Info.FloaterCount > 99) then Error('Invalid FloaterCount ' + level.Info.FloaterCount.ToString);
      if (level.Info.BomberCount < 0) or (level.Info.BomberCount > 99) then Error('Invalid BomberCount ' + level.Info.BlockerCount.ToString);
      if (level.Info.BlockerCount < 0) or (level.Info.BlockerCount > 99) then Error('Invalid BlockerCount ' + level.Info.BlockerCount.ToString);
      if (level.Info.BuilderCount < 0) or (level.Info.BuilderCount > 99) then Error('Invalid BuilderCount ' + level.Info.BuilderCount.ToString);
      if (level.Info.BasherCount < 0) or (level.Info.BasherCount > 99) then Error('Invalid BasherCount ' + level.Info.BasherCount.ToString);
      if (level.Info.MinerCount < 0) or (level.Info.MinerCount > 99) then Error('Invalid MinerCount ' + level.Info.MinerCount.ToString);
      if (level.Info.DiggerCount < 0) or (level.Info.DiggerCount > 99) then Error('Invalid DiggerCount ' + level.Info.DiggerCount.ToString);
      if (level.Info.GraphicSet < 0) then Error('GraphicSet');
//      if (level.Info.GraphicSetEx < 0) or (level.Info.GraphicSetE) then Error('ReleaseRate');
      //if (level.Info.SuperLemming   : Boolean read fSuperLemming write fSuperLemming;
      if (level.Info.ScreenPosition < 0) or (level.Info.ScreenPosition >= GAME_BMPWIDTH) then Error('Invalid ScreenPosition ' + level.Info.ScreenPosition.ToString);

      if level.Terrains.Count = 0 then Error('No terrain found');
      if level.InteractiveObjects.Count = 0 then Error('No objects found');
   end;



begin
  level := TLevel.Create;
  list := TStringList.Create;
  try
    list.LoadFromFile(aFilename);
    for var i := 0 to list.Count - 1 do begin
      line := list[i].Trim;
      if line.IsEmpty or line.StartsWith('#') then
        Continue;
      equalsPos := Pos('=', line);
      if equalsPos <= 0 then
        Continue;
      var ar: TArray<string> := line.Split(['=']);
      if ar.Length <> 2 then
        Continue;
      key := ar[0].Trim;
      value := ar[1].Trim;
      lineResult := TryInfo or TryObject or TryTerrain or TrySteel or TryName or TryIgnore;
      if not lineResult then
        Error('Invalid line: ' + line);
    end;

    Validate;
    TLevelLoader.TranslateLevel(level, LVL);

  finally
    list.Free;
    level.Free;
  end;
end;

end.

