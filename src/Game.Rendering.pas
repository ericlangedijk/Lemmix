unit Game.Rendering;

{$include lem_directives.inc}

{----------------------------------------------------------------------------------------
  Some notes on the rendering:

  Levels consist of terrains and objects.
  • Objects kan animate and terrain can be changed.
  • Lemmings only have collisions with terrain

  The alpha channel of the pixels is used to put information about the pixels
  in the bitmap:
  • Bit0 = there is terrain in this pixel
  • Bit1 = there is interactive object in this pixel (maybe this makes no sense)
----------------------------------------------------------------------------------------}

interface

uses
  System.Types, System.Classes, System.SysUtils, System.Contnrs, System.Generics.Collections,
  GR32, GR32_LowLevel,
  Base.Utils, Base.Types,
  Meta.Structures,
  Styles.Base,
  Level.Base;

type
  TPixelCombiner = class sealed
  private
    //class procedure Copy(var NewColor: TColor32; const stuff: Cardinal); static; inline;
  public
    class procedure TerrainDefault(F: TColor32; var B: TColor32; M: TColor32);
  end;


  TDrawItem = class
  private
  protected
    fOriginal: TBitmap32; // reference
  public
    constructor Create(aOriginal: TBitmap32);
    destructor Destroy; override;
    property Original: TBitmap32 read fOriginal;
  end;

  TDrawList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDrawItem;
  protected
  public
    function Add(Item: TDrawItem): Integer;
    procedure Insert(Index: Integer; Item: TDrawItem);
    property Items[Index: Integer]: TDrawItem read GetItem; default;
  end;

  TAnimation = class(TDrawItem)
  private
    {$ifdef paranoid}
    procedure Check;
    procedure CheckFrame(Bmp: TBitmap32);
    {$endif}
  protected
    fFrameHeight: Integer;
    fFrameCount: Integer;
    fFrameWidth: Integer;
  public
    constructor Create(aOriginal: TBitmap32; aFrameCount, aFrameWidth, aFrameHeight: Integer);
    function CalcFrameRect(aFrameIndex: Integer): TRect;
    function CalcTop(aFrameIndex: Integer): Integer;
    procedure InsertFrame(Bmp: TBitmap32; aFrameIndex: Integer);
    procedure GetFrame(Bmp: TBitmap32; aFrameIndex: Integer);
    property FrameCount: Integer read fFrameCount default 1;
    property FrameWidth: Integer read fFrameWidth;
    property FrameHeight: Integer read fFrameHeight;
  end;

  TObjectAnimation = class(TAnimation)
  private
  protected
    fInverted: TBitmap32; // copy of original
    procedure Flip;
  public
    constructor Create(aOriginal: TBitmap32; aFrameCount, aFrameWidth, aFrameHeight: Integer);
    destructor Destroy; override;
    property Inverted: TBitmap32 read fInverted;
  end;

  TRenderInfoRec = record
    Level        : TLevel;
    GraphicSet   : TGraphicSet;
    Repair       : Boolean;
    constructor Create(aLevel: TLevel; aGraphicSet: TGraphicSet; repairErrors: Boolean);
    procedure CheckOrRepair(out errorCount: Integer);
  end;

  TRenderer = class
  private
    TempBitmap         : TBitmap32;
    ObjectRenderList   : TDrawList; // list to accelerate object drawing
    Inf                : TRenderInfoRec;
    fWorld             : TBitmap32;
    class procedure CombineTerrainDefault(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineTerrainNoOverwrite(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineTerrainErase(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineObjectDefault(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineObjectNoOverwrite(F: TColor32; var B: TColor32; M: TColor32); static;
    class procedure CombineObjectOnlyOnTerrain(F: TColor32; var B: TColor32; M: TColor32); static;

    procedure PrepareTerrainBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
    procedure PrepareObjectBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Prepare(const aInfo: TRenderInfoRec; out errors: Integer);

    procedure DrawTerrain(Dst: TBitmap32; T: TTerrain);
    procedure DrawObject(Dst: TBitmap32; O: TInteractiveObject; aFrame: Integer; aOriginal: TBitmap32 = nil);
    procedure EraseObject(Dst: TBitmap32; O: TInteractiveObject; aOriginal: TBitmap32 = nil);
    procedure DrawSpecialBitmap(Dst, Spec: TBitmap32);

    function HasPixelAt(X, Y: Integer): Boolean;
    procedure RenderWorld(World: TBitmap32; DoObjects: Boolean);
    procedure Highlight(World: TBitmap32; M: TColor32);
  end;

const
  COLOR_MASK    = $80FFFFFF; // transparent black flag is included!
  ALPHA_MASK    = $FF000000;

  ALPHA_TERRAIN = $01000000;
  ALPHA_OBJECT  = $02000000; // not really needed, but used

  // to enable black terrain. bitmaps with transparent black should include this bit
  ALPHA_TRANSPARENTBLACK = $80000000;

implementation

{ TPixelCombiner }

class procedure TPixelCombiner.TerrainDefault(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then
    B := B and not COLOR_MASK or ALPHA_TERRAIN or F and COLOR_MASK;
end;

{ TDrawItem }

constructor TDrawItem.Create(aOriginal: TBitmap32);
begin
  inherited Create;
  fOriginal := aOriginal;
end;

destructor TDrawItem.Destroy;
begin
  inherited Destroy;
end;

{ TDrawList }

function TDrawList.Add(Item: TDrawItem): Integer;
begin
  Result := inherited Add(Item);
end;

function TDrawList.GetItem(Index: Integer): TDrawItem;
begin
  Result := inherited Get(Index);
end;

procedure TDrawList.Insert(Index: Integer; Item: TDrawItem);
begin
  inherited Insert(Index, Item);
end;

{ TAnimation }

{$ifdef paranoid}
procedure TAnimation.Check;
begin
  Assert(fFrameCount <> 0);
  Assert(Original.Width = fFrameWidth);
  Assert(fFrameHeight * fFrameCount = Original.Height);
end;

procedure TAnimation.CheckFrame(Bmp: TBitmap32);
begin
  Assert(Bmp.Width = Original.Width);
  Assert(Bmp.Height * fFrameCount = Original.Height);
end;
{$endif}

function TAnimation.CalcFrameRect(aFrameIndex: Integer): TRect;
begin
  Result.Left := 0;
  Result.Top := aFrameIndex * fFrameHeight;
  Result.Right := Result.Left + fFrameWidth;
  Result.Bottom := Result.Top + fFrameHeight;
end;

function TAnimation.CalcTop(aFrameIndex: Integer): Integer;
begin
  Result := aFrameIndex * fFrameHeight;
end;

constructor TAnimation.Create(aOriginal: TBitmap32; aFrameCount, aFrameWidth, aFrameHeight: Integer);
begin
  inherited Create(aOriginal);
  fFrameCount := aFrameCount;
  fFrameWidth := aFrameWidth;
  fFrameHeight := aFrameHeight;
  {$ifdef paranoid} Check; {$endif}
end;

procedure TAnimation.GetFrame(Bmp: TBitmap32; aFrameIndex: Integer);
// unsafe
var
  Y, W: Integer;
  SrcP, DstP: PColor32;
begin
  {$ifdef paranoid} Check; {$endif}
  Bmp.SetSize(fFrameWidth, fFrameHeight);
  DstP := Bmp.PixelPtr[0, 0];
  SrcP := Original.PixelPtr[0, CalcTop(aFrameIndex)];
  W := fFrameWidth;
  for Y := 0 to fFrameHeight - 1 do begin
    MoveLongWord(SrcP^, DstP^, W);
    Inc(SrcP, W);
    Inc(DstP, W);
  end;
end;

procedure TAnimation.InsertFrame(Bmp: TBitmap32; aFrameIndex: Integer);
// unsafe
var
  Y, W: Integer;
  SrcP, DstP: PColor32;
begin
  {$ifdef paranoid}
  Check;
  CheckFrame(Bmp);
  {$endif}

  SrcP := Bmp.PixelPtr[0, 0];
  DstP := Original.PixelPtr[0, CalcTop(aFrameIndex)];
  W := fFrameWidth;

  for Y := 0 to fFrameHeight - 1 do begin
    MoveLongWord(SrcP^, DstP^, W);
    Inc(SrcP, W);
    Inc(DstP, W);
  end;
end;

{ TObjectAnimation }

constructor TObjectAnimation.Create(aOriginal: TBitmap32; aFrameCount,  aFrameWidth, aFrameHeight: Integer);
begin
  inherited;
  fInverted := TBitmap32.Create;
  fInverted.Assign(aOriginal);
  Flip;
end;

destructor TObjectAnimation.Destroy;
begin
  fInverted.Free;
  inherited;
end;

procedure TObjectAnimation.Flip;
// unsafe, can be optimized by making a algorithm
var
  Temp: TBitmap32;
  i: Integer;

      procedure Ins(aFrameIndex: Integer);
      var
        Y, W: Integer;
        SrcP, DstP: PColor32;
      begin
        SrcP := Temp.PixelPtr[0, 0];
        DstP := Inverted.PixelPtr[0, CalcTop(aFrameIndex)];
        W := fFrameWidth;

        for Y := 0 to fFrameHeight - 1 do begin
          MoveLongWord(SrcP^, DstP^, W);
          Inc(SrcP, W);
          Inc(DstP, W);
        end;
      end;

begin
  if fFrameCount = 0 then
    Exit;
  Temp := TBitmap32.Create;
  try
    for i := 0 to fFrameCount - 1 do begin
      GetFrame(Temp, i);
      Temp.FlipVert;
      Ins(i);
    end;
  finally
    Temp.Free;
  end;
end;

{ TRenderInfoRec }

constructor TRenderInfoRec.Create(aLevel: TLevel; aGraphicSet: TGraphicSet; repairErrors: Boolean);
begin
  Level := aLevel;
  GraphicSet := aGraphicSet;
  Repair := repairErrors;
end;

procedure TRenderInfoRec.CheckOrRepair(out errorCount: Integer);
const
  method = 'TRenderInfoRec.Validate';
begin
  errorCount := 0;

  // first we check non-repairable stuff: this will raise an error
  if GraphicSet = nil then
    DoThrow('Graphicset not assigned', method);

  if Level = nil then
    DoThrow('Level not assigned', method);

  var levelTitle: string := Level.Info.Title.Trim;

  if GraphicSet.MetaObjectList.Count <> GraphicSet.ObjectBitmaps.Count then
    DoThrow('Graphicset metaobjects count mismatch with objects'
             + '. The error occurred in level ' + levelTitle, method);

  // we do not validate resourced levels
  if GraphicSet.Style.Def <> TStyleDef.User then
    Exit;

  if not Repair then begin
    for var o: TInteractiveObject in Level.InteractiveObjects do
     if (o.Identifier < 0) or (o.Identifier >= GraphicSet.ObjectBitmaps.Count) then
        DoThrow('Invalid object identifier (' + o.Identifier.ToString + '). '
                 + Pred(GraphicSet.ObjectBitmaps.Count).ToString
                 + '. The error occurred in level ' + levelTitle + CRLF
                 + 'Object listindex = ' + Level.InteractiveObjects.IndexOf(o).ToString, method);

    for var t: TTerrain in Level.Terrains do
      if (t.Identifier < 0) or (t.Identifier >= GraphicSet.TerrainBitmaps.Count) then
        DoThrow('Invalid terrain identifier (' + t.identifier.ToString + '). The maximum is ' + Pred(GraphicSet.TerrainBitmaps.Count).ToString
                + '. The error occurred in level ' + levelTitle + CRLF
                + 'Terrain listindex = ' + Level.Terrains.IndexOf(t).ToString, method);
  end
  else begin

    var terrainCheckList := TFastObjectList<TTerrain>.Create(False);
    var objectCheckList := TFastObjectList<TInteractiveObject>.Create(False);

    try
      for var o: TInteractiveObject in Level.InteractiveObjects do
        if (o.Identifier < 0) or (o.Identifier >= GraphicSet.ObjectBitmaps.Count) then
          objectCheckList.Add(o);

      for var t: TTerrain in Level.Terrains do
        if (t.Identifier < 0) or (t.Identifier >= GraphicSet.TerrainBitmaps.Count) then
          terrainCheckList.Add(t);

      for var o: TInteractiveObject in objectCheckList do
        Level.InteractiveObjects.Remove(o);
      for var t: TTerrain in terrainCheckList do
        Level.Terrains.Remove(t);

      errorCount := terrainCheckList.Count + objectCheckList.Count;

    finally
      terrainCheckList.free;
      objectCheckList.free;
    end;
  end;
end;

{ TRenderer }

class procedure TRenderer.CombineTerrainDefault(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then
  begin
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_TERRAIN; // put terrain bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

class procedure TRenderer.CombineTerrainNoOverwrite(F: TColor32; var B: TColor32; M: TColor32);
begin
  if (F <> 0) and (B and ALPHA_TERRAIN = 0) then
  begin
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_TERRAIN; // put terrain bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

class procedure TRenderer.CombineTerrainErase(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then
    B := 0;
end;

class procedure TRenderer.CombineObjectDefault(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then begin
    //B := F;
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_OBJECT; // put object bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

class procedure TRenderer.CombineObjectNoOverwrite(F: TColor32; var B: TColor32; M: TColor32);
begin
  if (F <> 0) and (B and ALPHA_MASK = 0) then begin
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_OBJECT; // put object bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

class procedure TRenderer.CombineObjectOnlyOnTerrain(F: TColor32; var B: TColor32; M: TColor32);
begin
  if (F <> 0) and (B and ALPHA_TERRAIN <> 0) then begin
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_OBJECT; // put object bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

procedure TRenderer.PrepareTerrainBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
begin
  if DrawingFlags and tdf_NoOverwrite <> 0 then begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineTerrainNoOverwrite;
  end
  else if DrawingFlags and tdf_Erase <> 0 then begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineTerrainErase;
  end
  else begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineTerrainDefault;
  end;
end;

procedure TRenderer.PrepareObjectBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
begin
  if DrawingFlags and odf_OnlyOnTerrain <> 0 then begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineObjectOnlyOnTerrain;
  end
  else if DrawingFlags and odf_NoOverwrite <> 0 then begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineObjectNoOverwrite;
  end
  else begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineObjectDefault;
  end;
end;

procedure TRenderer.DrawTerrain(Dst: TBitmap32; T: TTerrain);
var
  Src: TBitmap32;
begin
  Src := Inf.GraphicSet.TerrainBitmaps[T.Identifier];
  if T.DrawingFlags and tdf_Invert = 0 then begin
    PrepareTerrainBitmap(Src, T.DrawingFlags);
    Src.DrawTo(Dst, T.Left, T.Top)
  end
  else begin
    Src.FlipVert(TempBitmap);
    PrepareTerrainBitmap(TempBitmap, T.DrawingFlags);
    TempBitmap.DrawTo(Dst, T.Left, T.Top);
  end;
end;

procedure TRenderer.DrawSpecialBitmap(Dst, Spec: TBitmap32);
begin
  Spec.DrawMode := dmCustom;
  Spec.OnPixelCombine := CombineTerrainDefault;
  Spec.DrawTo(Dst, 304, 0);
end;

procedure TRenderer.DrawObject(Dst: TBitmap32; O: TInteractiveObject; aFrame: Integer; aOriginal: TBitmap32 = nil);
{-------------------------------------------------------------------------------
  Draws a interactive object
  • Dst = the targetbitmap
  • O = the object
  • aOriginal = if specified then first a part of this bitmap (world when playing)
    is copied to Dst to restore
-------------------------------------------------------------------------------}
var
  SrcRect, DstRect, R: TRect;
  Item: TObjectAnimation;
  Src: TBitmap32;
begin
  Item := TObjectAnimation(ObjectRenderList[O.Identifier]);

  if odf_UpsideDown and O.DrawingFlags = 0
  then Src := Item.Original
  else Src := Item.Inverted;

  PrepareObjectBitmap(Src, O.DrawingFlags);

  SrcRect := Item.CalcFrameRect(aFrame);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  GR32.OffsetRect(DstRect, O.Left, O.Top);

  if aOriginal <> nil then begin
    GR32.IntersectRect(R, DstRect, aOriginal.BoundsRect); // oops important!
    aOriginal.DrawTo(Dst, R, R);
  end;
  Src.DrawTo(Dst, DstRect, SrcRect);
end;

procedure TRenderer.EraseObject(Dst: TBitmap32; O: TInteractiveObject; aOriginal: TBitmap32);
{-------------------------------------------------------------------------------
  Draws a interactive object
  • Dst = the targetbitmap
  • O = the object
  • aOriginal = if specified then first a part of this bitmap (world when playing)
    is copied to Dst to restore
-------------------------------------------------------------------------------}
var
  SrcRect, DstRect, R: TRect;
  Item: TObjectAnimation;
begin
  if aOriginal = nil then
    Exit;

  Assert(ObjectRenderList[O.Identifier] is TObjectAnimation);
  Item := TObjectAnimation(ObjectRenderList[O.Identifier]);

  SrcRect := Item.CalcFrameRect(0);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  GR32.OffsetRect(DstRect, O.Left, O.Top);

  GR32.IntersectRect(R, DstRect, aOriginal.BoundsRect); // oops important!
  aOriginal.DrawTo(Dst, R, R);
end;


constructor TRenderer.Create;
begin
  inherited Create;
  TempBitmap := TBitmap32.Create;
  ObjectRenderList := TDrawList.Create;
end;

destructor TRenderer.Destroy;
begin
  TempBitmap.Free;
  ObjectRenderList.Free;
  inherited Destroy;
end;

procedure TRenderer.RenderWorld(World: TBitmap32; DoObjects: Boolean);
var
  i: Integer;
  Ter: TTerrain;
  Bmp: TBitmap32;
  Obj: TInteractiveObject;
  MO: TMetaObject;
begin
  World.Clear(0);

  if (Inf.level = nil) or (Inf.graphicset = nil) then
    Exit;

  if Inf.GraphicSet.GraphicSetIdExt > 0 then begin
    Bmp := Inf.GraphicSet.SpecialBitmap;
    DrawSpecialBitmap(World, Bmp);
  end
  else begin
    for i := 0 to Inf.Level.Terrains.Count - 1 do begin
      Ter := Inf.Level.Terrains.Items[i];
      DrawTerrain(World, Ter);
    end;
  end;

  if DoObjects then begin

    // draw only on terrain
    for i := 0 to Inf.Level.InteractiveObjects.Count - 1 do begin
      Obj := Inf.Level.InteractiveObjects.Items[i];
      MO := Inf.GraphicSet.MetaObjectList[obj.identifier];
      if odf_OnlyOnTerrain and Obj.DrawingFlags <> 0 then
        DrawObject(World, Obj, MO.PreviewFrameIndex);
    end;

    // draw *not* only on terrain
    for i := 0 to Inf.Level.InteractiveObjects.Count - 1 do begin
      Obj := Inf.Level.InteractiveObjects.Items[i];
      MO := Inf.GraphicSet.MetaObjectList[obj.identifier]; // ["I suppose that's an exception?" level]
      if odf_OnlyOnTerrain and Obj.DrawingFlags = 0 then
        DrawObject(World, Obj, MO.PreviewFrameIndex);
    end;

  end;

end;

procedure TRenderer.Prepare(const aInfo: TRenderInfoRec; out errors: Integer);
var
  i: Integer;
  Item: TObjectAnimation;
  Bmp: TBitmap32;
  MO: TMetaObject;
begin
  aInfo.CheckOrRepair({out} errors);

  Inf := aInfo; // copy

  // create cache to draw from
  ObjectRenderList.Clear;

  for i := 0 to Inf.GraphicSet.ObjectBitmaps.Count - 1 do begin
    MO := Inf.GraphicSet.MetaObjectList[i];
    Bmp := Inf.GraphicSet.ObjectBitmaps[i];
    Item := TObjectAnimation.Create(Bmp, MO.AnimationFrameCount, MO.Width, MO.Height);
    ObjectRenderList.Add(Item);
  end;

end;

procedure TRenderer.Highlight(World: TBitmap32; M: TColor32);
var
  i: Integer;
  P: PColor32;
begin
  P := World.PixelPtr[0, 0];
  for i := 0 to World.Width * World.Height - 1 do begin
    if P^ and M <> 0
    then P^ := clRed32
    else P^ := 0;
    Inc(P);
  end;
end;

function TRenderer.HasPixelAt(X, Y: Integer): Boolean;
begin
  Result := fWorld.PixelS[X, Y] and ALPHA_TERRAIN = 0;
end;

end.

