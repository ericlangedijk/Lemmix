unit Dos.MainDat;

{$include lem_directives.inc}

interface

uses
  Classes, GR32,
  Base.Bitmaps,
  Dos.Compression, Dos.Structures,
  Prog.Base, Prog.Data, Dos.Bitmaps;

type
  // Tool to extract data from the dos main dat file
  TMainDatExtractor = class
  private
    fFileName: string;
    fDecompressor: TDosDatDecompressor;
    fSections: TDosDatSectionList;
    fPlanar: TDosPlanarBitmap;
    procedure EnsureLoaded;
    procedure EnsureDecompressed(aSection: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExtractBrownBackGround(Bmp: TBitmap32);
    procedure ExtractLogo(Bmp: TBitmap32);
    procedure ExtractBitmap(Bmp: TBitmap32; aSection, aPosition, aWidth, aHeight, BPP: Integer; const aPal: TArrayOfColor32);
    procedure ExtractAnimation(Bmp: TBitmap32; aSection, aPos, aWidth, aHeight, aFrameCount: Integer; BPP: Byte; const aPal: TArrayOfColor32);
    property FileName: string read fFileName write fFileName;
  end;

implementation

{ TMainDatExtractor }

constructor TMainDatExtractor.Create;
begin
  inherited Create;
  fSections := TDosDatSectionList.Create;
  fDecompressor := TDosDatDecompressor.Create;
end;

destructor TMainDatExtractor.Destroy;
begin
  fSections.Free;
  fDecompressor.Free;
  inherited;
end;

procedure TMainDatExtractor.EnsureDecompressed(aSection: Integer);
var
  Sec: TDosDatSection;
begin
  EnsureLoaded;
  Sec := fSections[aSection];
  if Sec.DecompressedData.Size = 0 then
    fDecompressor.DecompressSection(Sec.CompressedData, Sec.DecompressedData)
end;

procedure TMainDatExtractor.EnsureLoaded;
var
  DataStream: TStream;
begin
  if fSections.Count = 0 then
  begin
    DataStream := TData.CreateDataStream(Consts.StyleName, fFileName, TDataType.LemmingData);
    try
      fDecompressor.LoadSectionList(DataStream, fSections, False);
    finally
      DataStream.Free;
    end;
  end;
end;

procedure TMainDatExtractor.ExtractAnimation(Bmp: TBitmap32; aSection, aPos, aWidth, aHeight, aFrameCount: Integer; BPP: Byte; const aPal: TArrayOfColor32);
begin
  EnsureDecompressed(aSection);
  fPlanar.LoadAnimationFromStream(fSections[aSection].DecompressedData, Bmp, aPos, aWidth, aHeight, aFrameCount, BPP, aPal);
end;

procedure TMainDatExtractor.ExtractBitmap(Bmp: TBitmap32; aSection, aPosition, aWidth, aHeight, BPP: Integer; const aPal: TArrayOfColor32);
begin
  EnsureDecompressed(aSection);
  fPlanar.LoadFromStream(fSections[aSection].DecompressedData, Bmp, aPosition, aWidth, aHeight, BPP, aPal);
end;

procedure TMainDatExtractor.ExtractBrownBackGround(Bmp: TBitmap32);
//  Extract hte brown background, used in several screens
begin
  EnsureDecompressed(3);
  fPlanar.LoadFromStream(fSections[3].DecompressedData, Bmp, 0, 320, 104, 2,
    GetDosMainMenuPaletteColors32);
end;

procedure TMainDatExtractor.ExtractLogo(Bmp: TBitmap32);
//  Extract the LemmingLogo
begin
  EnsureDecompressed(3);
  fPlanar.LoadFromStream(fSections[3].DecompressedData, Bmp, $2080, 632, 94, 4,
    GetDosMainMenuPaletteColors32);
end;

end.

