unit Level.Base;

{$include lem_directives.inc}

interface

uses
  Classes, SysUtils,
  Base.Utils;

const
  // Terrain Drawing Flags
  tdf_Erase         = 1;    // bit 0 use terrain bitmap as eraser
  tdf_Invert        = 2;    // bit 1 invert terrain bitmap
  tdf_NoOverwrite   = 4;    // bit 2 do not overwrite existing terrain pixels

const
  // Object Drawing Flags
  odf_OnlyOnTerrain = 1; // bit 0
  odf_UpsideDown    = 2; // bit 1
  odf_NoOverwrite   = 4; // bit 2

type
  TLevelInfo = class(TPersistent)
  protected
    fReleaseRate    : Integer;
    fLemmingsCount  : Integer;
    fRescueCount    : Integer;
    fTimeLimit      : Integer;
    fClimberCount   : Integer;
    fFloaterCount   : Integer;
    fBomberCount    : Integer;
    fBlockerCount   : Integer;
    fBuilderCount   : Integer;
    fBasherCount    : Integer;
    fMinerCount     : Integer;
    fDiggerCount    : Integer;
    fGraphicSet     : Integer;
    fGraphicSetEx   : Integer;
    fSuperLemming   : Boolean;
    fScreenPosition : Integer;
    fTitle          : string;
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
  public
    property ReleaseRate    : Integer read fReleaseRate write fReleaseRate;
    property LemmingsCount  : Integer read fLemmingsCount write fLemmingsCount;
    property RescueCount    : Integer read fRescueCount write fRescueCount;
    property TimeLimit      : Integer read fTimeLimit write fTimeLimit;
    property ClimberCount   : Integer read fClimberCount write fClimberCount;
    property FloaterCount   : Integer read fFloaterCount write fFloaterCount;
    property BomberCount    : Integer read fBomberCount write fBomberCount;
    property BlockerCount   : Integer read fBlockerCount write fBlockerCount;
    property BuilderCount   : Integer read fBuilderCount write fBuilderCount;
    property BasherCount    : Integer read fBasherCount write fBasherCount;
    property MinerCount     : Integer read fMinerCount write fMinerCount;
    property DiggerCount    : Integer read fDiggerCount write fDiggerCount;
    property GraphicSet     : Integer read fGraphicSet write fGraphicSet;
    property GraphicSetEx   : Integer read fGraphicSetEx write fGraphicSetEx;
    property SuperLemming   : Boolean read fSuperLemming write fSuperLemming;
    property ScreenPosition : Integer read fScreenPosition write fScreenPosition;
    property Title          : string read fTitle write fTitle;
  end;

  // abstract ancestor for object, terrain and steel
  TPiece = class
  protected
    fLeft : Integer;
    fTop  : Integer;
  public
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
  end;

  // basic ancestor for object and terrain
  TIdentifiedPiece = class(TPiece)
  protected
    fIdentifier: Integer;
  public
    property Identifier: Integer read fIdentifier write fIdentifier;
  end;

  TTerrain = class(TIdentifiedPiece)
  protected
    fDrawingFlags : Byte;
  public
    property DrawingFlags: Byte read fDrawingFlags write fDrawingFlags;
  end;

  TInteractiveObject = class(TIdentifiedPiece)
  protected
    fDrawingFlags: Byte; // odf_xxxx
  public
    property DrawingFlags: Byte read fDrawingFlags write fDrawingFlags;
  end;

  TSteel = class(TPiece)
  protected
    fHeight: Integer;
    fWidth: Integer;
  public
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
  end;

  TInteractiveObjects = class(TFastObjectList<TInteractiveObject>);
  TTerrains = class(TFastObjectList<TTerrain>);
  TSteels = class(TFastObjectList<TSteel>);

  TLevel = class(TPersistent)
  protected
    fLevelInfo          : TLevelInfo;
    fTerrains           : TTerrains;
    fInteractiveObjects : TInteractiveObjects;
    fSteels             : TSteels;
    function DoCreateLevelInfo: TLevelInfo; dynamic;
    function DoCreateTerrains: TTerrains; dynamic;
    function DoCreateInteractiveObjects: TInteractiveObjects; dynamic;
    function DoCreateSteels: TSteels; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const aFileName: string);
    procedure SaveToStream(S: TStream);
    procedure ClearLevel;
    property Info: TLevelInfo read fLevelInfo;
    property InteractiveObjects: TInteractiveObjects read fInteractiveObjects;
    property Terrains: TTerrains read fTerrains;
    property Steels: TSteels read fSteels;
  end;

implementation

{ TLevelInfo }

procedure TLevelInfo.Assign(Source: TPersistent);
var
  L: TLevelInfo absolute Source;
begin
  if Source is TLevelInfo then begin
    ReleaseRate    := L.ReleaseRate;
    LemmingsCount  := L.LemmingsCount;
    RescueCount    := L.RescueCount;
    TimeLimit      := L.TimeLimit;
    ClimberCount   := L.ClimberCount;
    FloaterCount   := L.FloaterCount;
    BomberCount    := L.BomberCount;
    BlockerCount   := L.BlockerCount;
    BuilderCount   := L.BuilderCount;
    BasherCount    := L.BasherCount;
    MinerCount     := L.MinerCount;
    DiggerCount    := L.DiggerCount;
    GraphicSet     := L.GraphicSet;
    GraphicSetEx   := L.GraphicSetEx;
    SuperLemming   := L.SuperLemming;
    ScreenPosition := L.ScreenPosition;
    Title          := L.Title;
  end
  else inherited Assign(Source);
end;

procedure TLevelInfo.Clear;
begin
  ReleaseRate    := 1;
  LemmingsCount  := 1;
  RescueCount    := 1;
  TimeLimit      := 1;
  ClimberCount   := 0;
  FloaterCount   := 0;
  BomberCount    := 0;
  BlockerCount   := 0;
  BuilderCount   := 0;
  BasherCount    := 0;
  MinerCount     := 0;
  DiggerCount    := 0;
  GraphicSet     := 0;
  GraphicSetEx   := 0;
  SuperLemming   := False;
  ScreenPosition := 0;
  Title          := '';
end;

constructor TLevelInfo.Create;
begin
  inherited Create;
  Clear;
end;

{ TLevel }

procedure TLevel.ClearLevel;
begin
  fInteractiveObjects.Clear;
  fTerrains.Clear;
  fSteels.Clear;
end;

constructor TLevel.Create;
begin
  inherited Create;
  fLevelInfo := DoCreateLevelInfo;
  fInteractiveObjects := DoCreateInteractiveObjects;
  fTerrains := DoCreateTerrains;
  fSteels := DoCreateSteels;
end;

destructor TLevel.Destroy;
begin
  fLevelInfo.Free;
  fInteractiveObjects.Free;
  fTerrains.Free;
  fSteels.Free;
  inherited Destroy;
end;

function TLevel.DoCreateInteractiveObjects: TInteractiveObjects;
begin
  Result := TInteractiveObjects.Create;
end;

function TLevel.DoCreateLevelInfo: TLevelInfo;
begin
  Result := TLevelInfo.Create;
end;

function TLevel.DoCreateSteels: TSteels;
begin
  Result := TSteels.Create;
end;

function TLevel.DoCreateTerrains: TTerrains;
begin
  Result := TTerrains.Create;
end;

procedure TLevel.SaveToFile(const aFileName: string);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(aFileName, fmCreate);
  try
     SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TLevel.SaveToStream(S: TStream);
begin
  Throw('SaveToStream not implemented', 'SaveToStream');
end;

end.

