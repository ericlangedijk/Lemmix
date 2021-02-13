unit GameScreen.Finder;

{$include lem_directives.inc}

interface

//todo: make grid component for grid screens
// todo: fontsize and distances in all screens the same

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, System.UITypes, System.Contnrs, System.Variants,
  System.Generics.Defaults,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.GraphUtil, Vcl.Grids, Vcl.ClipBrd,
  Base.Utils, Base.Types, Base.Bitmaps, Base.Strings,
  Dos.Consts,
  Prog.Base, Prog.Data, Prog.Cache,
  Styles.Base,
  Game,
  Prog.App,
  Form.Base, Form.Message, GameScreen.Help;

type
  TGameScreenFinder = class(TToolForm)
  private

    type
      TProp = record
        Value: Variant;
        Text: string;
      end;

    type
      TRecord = class
      private
        const
          property_count       = 28;

          prop_refindex        = 0;
          prop_sectionindex    = 1;
          prop_levelindex      = 2;
          prop_stylename       = 3;
          prop_leveltitle      = 4;
          prop_releaserate     = 5;
          prop_lemmingscount   = 6;
          prop_rescuecount     = 7;
          prop_timelimit       = 8;
          prop_climbercount    = 9;
          prop_floatercount    = 10;
          prop_bombercount     = 11;
          prop_blockercount    = 12;
          prop_buildercount    = 13;
          prop_bashercount     = 14;
          prop_minercount      = 15;
          prop_diggercount     = 16;
          prop_graphicset      = 17;
          prop_graphicsetex    = 18;
          prop_superlemming    = 19;
          prop_objectcount     = 20;
          prop_terraincount    = 21;
          prop_entrancecount   = 22;
          prop_exitcount       = 23;
          prop_levelhash       = 24;
          prop_levelcode       = 25;
          prop_duplicates      = 26;
          prop_sourcefile      = 27;

      private
        Ref: TStyleCache.TLevelCacheItem; // ref to original cache item in the style cache
        RefIndex: Integer; // the original index in the cache
        Duplicates: Integer; // calulated once during contruction
        MyStyle: (sOriginal, sCustom, sLemmini); // for colorizing
      public
        constructor Create(cacheIndex: Integer);
        function GetProp(index: Integer): TProp;
        function GetText(index: Integer): string;
        function Filter(index: Integer; const s: string): Boolean;
     end;

      TRecordList = class(TFastObjectList<TRecord>);

  private
    const
      Headers: array[0..TRecord.property_count - 1] of string = (
        'Index', 'Sect', 'Lev', 'Style', 'Title',
        'RR', 'Lem', 'Res', 'Tim', 'Cli', 'Flo', 'Bom', 'Blo', 'Bui', 'Bas', 'Min', 'Dig',
        'Gra', 'Spec', 'Fast', 'Obj', 'Ter', 'Entr', 'Ex', 'Hash', 'Code', 'Dup', 'Source'
      );

      ColumnColors : array[0..TRecord.property_count - 1] of TColor = (
        clSilver,                    // prop_refindex
        clWebRoyalBlue,              // prop_sectionindex
        clWebRoyalBlue,              // prop_levelindex
        clNone,                      // prop_stylename
        clWhite,                     // prop_leveltitle
        clFuchsia,                   // prop_releaserate
        clFuchsia,                   // prop_lemmingscount
        clFuchsia,                   // prop_rescuecount
        clFuchsia,                   // prop_timelimit
        clFuchsia,                   // prop_climbercount
        clFuchsia,                   // prop_floatercount
        clFuchsia,                   // prop_bombercount
        clFuchsia,                   // prop_blockercount
        clFuchsia,                   // prop_buildercount
        clFuchsia,                   // prop_bashercount
        clFuchsia,                   // prop_minercount
        clFuchsia,                   // prop_diggercount
        clWebOrangeRed,              // prop_graphicset
        clWebOrangeRed,              // prop_graphicsetex
        clWebCornFlowerBlue,         // prop_superlemming
        clWebOrangeRed,              // prop_objectcount
        clWebOrangeRed,              // prop_terraincount
        clWebOrangeRed,              // prop_entrancecount
        clWebOrangeRed,              // prop_exitcount
        clWebRoyalBlue,              // prop_levelhash
        clWebRoyalBlue,              // prop_levelcode
        clRed,                       // prop_duplicates
        clWebLightCoral              // prop_sourcefile
      );
    const
      DEF_ROWHEIGHT = 21;
      HEADER_ROWCOUNT = 3;
      FIRST_DATAROW = 3;
  private
    fFixedColor: TColor;
    fFixedShadowColor: TColor;
    fLockCount: Integer;
    fRecordList: TRecordList;
    fRefList: TList<Integer>;
    fGrid: TDrawGridEx;
    fSortColumn: Integer;
    fSortedAscending: Boolean;
    fLabelRecordCount: TLabelEx;
    fEdit: TEdit;
    fEditorCol: Integer;
    fLastSelectedRecord: TRecord;
    fLastRelativePosition: Integer;
    fApproxRowsOnScreen: Integer;
    fUserFilters: array[0..TRecord.property_count - 1] of string;
    fScreenResult: Boolean;
    function GetItem(aRow: Integer): TRecord; inline;
    function GetHelpText: THelpString;

    procedure Grid_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Grid_KeyPress(Sender: TObject; var Key: Char);
    procedure Grid_DblClick(Sender: TObject);
    procedure Grid_DrawCell(Sender: TObject; aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
    procedure Grid_SelectCell(Sender: TObject; aCol, aRow: Longint; var canSelect: Boolean);
    procedure Grid_FixedCellClick(Sender: TObject; aCol, aRow: Longint);
    procedure Editor_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Editor_KeyPress(Sender: TObject; var Key: Char);
    function EditorVisible: Boolean;
    procedure SetSelectedRecord(item: TRecord; aRelativePosition: Integer);
    procedure Lock;
    procedure Unlock;
    procedure UpdateLabel;
    procedure PaintArrowDown(const aRect: TRect; aColor: TColor);
    procedure PaintArrowUp(const aRect: TRect; aColor: TColor);
    procedure Sort(aCol: Integer; Asc: Boolean);
    procedure Filter;
    procedure ClearFilters;
    procedure TryCloseAndPlay;
    procedure CopyGridToClipBoard;

    procedure EditorStart(aCol: Integer; const key: Char= #0);
    procedure EditorEnd(accept: Boolean);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute: Boolean; static;
  end;


implementation

uses
  GameScreen.Options; // only for color consts GUI

{ TGameScreenFinder.TRecord }

constructor TGameScreenFinder.TRecord.Create(cacheIndex: Integer);
begin
  RefIndex := cacheIndex;
  Ref := App.StyleCache.FlatList[RefIndex];
  Duplicates := Length(App.StyleCache.FindLevelsByHash(Ref.LevelHash)) - 1;
  var info: Consts.TStyleInformation := Consts.FindStyleInfo(Ref.StyleName);
  if Assigned(info) then begin
    if info.StyleDef < TStyleDef.User then
      myStyle := sOriginal
    else begin
      if info.Family = TStyleFamily.DOS then
        myStyle := sCustom
      else
        myStyle := sLemmini;
    end;
  end;
end;

function TGameScreenFinder.TRecord.GetText(index: Integer): string;

    procedure SetResult(const s: string);
    begin
      Result := s;
    end;

begin
  case index of
    prop_refindex          : SetResult(RefIndex.ToString);
    prop_sectionindex      : SetResult(Ref.SectionIndex.ToString);
    prop_levelindex        : SetResult(Ref.LevelIndex.ToString);
    prop_stylename         : SetResult(Ref.StyleName);
    prop_leveltitle        : SetResult(Trim(string(Ref.LevelTitle)));
    prop_releaserate       : SetResult(Ref.Statics.ReleaseRate.ToString);
    prop_lemmingscount     : SetResult(Ref.Statics.LemmingsCount.ToString);
    prop_rescuecount       : SetResult(Ref.Statics.RescueCount.ToString);
    prop_timelimit         : SetResult(Ref.Statics.TimeLimit.ToString);
    prop_climbercount      : SetResult(Ref.Statics.ClimberCount.ToString);
    prop_floatercount      : SetResult(Ref.Statics.FloaterCount.ToString);
    prop_bombercount       : SetResult(Ref.Statics.BomberCount.ToString);
    prop_blockercount      : SetResult(Ref.Statics.BlockerCount.ToString);
    prop_buildercount      : SetResult(Ref.Statics.BuilderCount.ToString);
    prop_bashercount       : SetResult(Ref.Statics.BasherCount.ToString);
    prop_minercount        : SetResult(Ref.Statics.MinerCount.ToString);
    prop_diggercount       : SetResult(Ref.Statics.DiggerCount.ToString);
    prop_graphicset        : SetResult(Ref.Statics.GraphicSet.ToString);
    prop_graphicsetex      : SetResult(Ref.Statics.GraphicSetEx.ToString);
    prop_superlemming      : SetResult(Byte(Ref.Statics.SuperLemming).ToString);
    prop_objectcount       : SetResult(Ref.Statics.ObjectCount.ToString);
    prop_terraincount      : SetResult(Ref.Statics.TerrainCount.ToString);
    prop_entrancecount     : SetResult(Ref.Statics.EntranceCount.ToString);
    prop_exitcount         : SetResult(Ref.Statics.ExitCount.ToString);
    prop_levelhash         : SetResult(IntToHex(Ref.LevelHash, 16));
    prop_duplicates        : SetResult(Duplicates.ToString);
    prop_levelcode         : SetResult(Ref.LevelCode);
    prop_sourcefile        : SetResult(Ref.SourceFile);
  else
    SetResult(string.Empty);
  end;
end;

function TGameScreenFinder.TRecord.GetProp(index: Integer): TProp;

    procedure SetResult(const v: Variant; const s: string);
    begin
      Result.Value := v;
      Result.Text := s;
    end;

begin
  case index of
    prop_refindex         : SetResult(RefIndex, RefIndex.ToString);
    prop_sectionindex     : SetResult(Ref.SectionIndex, Ref.SectionIndex.ToString);
    prop_levelindex       : SetResult(Ref.LevelIndex, Ref.LevelIndex.ToString);
    prop_stylename        : SetResult(Ref.StyleName, Ref.StyleName);
    prop_leveltitle       : SetResult(string(Ref.LevelTitle), Trim(string(Ref.LevelTitle)));

    prop_releaserate      : SetResult(Ref.Statics.ReleaseRate, Ref.Statics.ReleaseRate.ToString);
    prop_lemmingscount    : SetResult(Ref.Statics.LemmingsCount, Ref.Statics.LemmingsCount.ToString);
    prop_rescuecount      : SetResult(Ref.Statics.RescueCount, Ref.Statics.RescueCount.ToString);
    prop_timelimit        : SetResult(Ref.Statics.TimeLimit, Ref.Statics.TimeLimit.ToString);
    prop_climbercount     : SetResult(Ref.Statics.ClimberCount, Ref.Statics.ClimberCount.ToString);
    prop_floatercount     : SetResult(Ref.Statics.FloaterCount, Ref.Statics.FloaterCount.ToString);
    prop_bombercount      : SetResult(Ref.Statics.BomberCount, Ref.Statics.BomberCount.ToString);
    prop_blockercount     : SetResult(Ref.Statics.BlockerCount, Ref.Statics.BlockerCount.ToString);
    prop_buildercount     : SetResult(Ref.Statics.BuilderCount, Ref.Statics.BuilderCount.ToString);
    prop_bashercount      : SetResult(Ref.Statics.BasherCount, Ref.Statics.BasherCount.ToString);
    prop_minercount       : SetResult(Ref.Statics.MinerCount, Ref.Statics.MinerCount.ToString);
    prop_diggercount      : SetResult(Ref.Statics.DiggerCount, Ref.Statics.DiggerCount.ToString);
    prop_graphicset       : SetResult(Ref.Statics.GraphicSet, Ref.Statics.GraphicSet.ToString);
    prop_graphicsetex     : SetResult(Ref.Statics.GraphicSetEx, Ref.Statics.GraphicSetEx.ToString);
    prop_superlemming     : SetResult(Ref.Statics.SuperLemming, Byte(Ref.Statics.SuperLemming).ToString);
    prop_objectcount      : SetResult(Ref.Statics.ObjectCount, Ref.Statics.ObjectCount.ToString);
    prop_terraincount     : SetResult(Ref.Statics.TerrainCount, Ref.Statics.TerrainCount.ToString);
    prop_entrancecount    : SetResult(Ref.Statics.EntranceCount, Ref.Statics.EntranceCount.ToString);
    prop_exitcount        : SetResult(Ref.Statics.ExitCount, Ref.Statics.ExitCount.ToString);

    prop_levelhash        : SetResult(Ref.LevelHash, IntToHex(Ref.LevelHash, 16));
    prop_duplicates       : SetResult(Duplicates, Duplicates.ToString);
    prop_levelcode        : SetResult(Ref.LevelCode, Ref.LevelCode);
    prop_sourcefile       : SetResult(Ref.SourceFile,Ref.SourceFile);
  else
    SetResult(RefIndex, RefIndex.ToString);
  end;
end;

function TGameScreenFinder.TRecord.Filter(index: Integer; const s: string): Boolean;
begin
  if index in [prop_stylename, prop_leveltitle, prop_levelhash, prop_levelcode, prop_sourcefile]
  then Result := GetText(index).ToUpper.Contains(s.ToUpper)
  else Result := GetText(index) = s;
end;

{ TGameScreenFinder }

constructor TGameScreenFinder.Create(aOwner: TComponent);
begin
  inherited;
  WindowState := wsNormal;
  Color := clBlack;
  fRecordList := TRecordList.Create(True);
  fRefList := TList<Integer>.Create;
  fGrid := TDrawGridEx.Create(Self);
  fFixedColor := RGB(0,32,64);
  fFixedShadowColor := RGB(0,48,80);
  //OnPaint := //Form_Paint;
end;

destructor TGameScreenFinder.Destroy;
begin
  fRecordList.Free;
  fRefList.Free;
  inherited;
end;

procedure TGameScreenFinder.BuildScreen;

   function TW(cnt: Integer): Integer;
   begin
     fGrid.Canvas.Font.Name := 'Segoe UI';
     var a: Integer := fGrid.Canvas.TextWidth(StringOfChar('W', cnt));
     var b: Integer := fGrid.Canvas.TextWidth(StringOfChar('i', cnt));
     Result := (a + b) div 2;
   end;

   function TWCourier(cnt: Integer): Integer;
   begin
     fGrid.Canvas.Font.Name := 'Courier New';
     Result:= fGrid.Canvas.TextWidth(StringOfChar('W', cnt));
   end;

var
  initiallySelectedRecord: TRecord;
  currLevel: TLevelLoadingInformation;
  cachedItem: TStyleCache.TLevelCacheItem;
  newRec: TRecord;
//  totalColWidths: Integer;
begin
//  inherited BuildScreen;
//  Left := CurrentDisplay.BoundsRect.Left + Scale(48);
//  Top := CurrentDisplay.BoundsRect.Top + Scale(48);
//  Width := CurrentDisplay.BoundsRect.Width - Scale(96);
//  Height := CurrentDisplay.BoundsRect.Height - Scale(96);

  initiallySelectedRecord := nil;
  currLevel := App.CurrentLevelInfo;

  // get all record refs
  fRecordList.Clear;
  for var i := 0 to App.StyleCache.FlatList.Count - 1 do begin

    newRec := TRecord.Create(i);
    fRecordList.Add(newRec);
    fRefList.Add(i);

    // initially selected item
    cachedItem := App.StyleCache.FlatList[i];
    if Assigned(currLevel)
    and not Assigned(initiallySelectedRecord)
    and (cachedItem.SectionIndex = currLevel.SectionIndex)
    and (cachedItem.LevelIndex = currLevel.LevelIndex)
    and (cachedItem.StyleName = currLevel.Style.Name) then
      initiallySelectedRecord := newRec;

  end;

  fGrid.Parent := Self;
  fGrid.Align := alClient;
  fGrid.AlignWithMargins := True;
  fGrid.Margins.SetBounds(Scale(8), Scale(8), Scale(8), Scale(8));
//  fGrid.ColCount := (fGrid.Width - Scale(24)) div Scale(320);
  fGrid.ScrollBars := ssNone;
  fGrid.RowCount := fRecordList.Count + 3; // todo: use header rowcount const. and fail safe when no items?
  fGrid.ColCount := TRecord.property_count;
  fGrid.FixedRows := 3; // 0 = filter, 1 = sort, 2 = title
  fGrid.FixedCols := 0;
  fGrid.DefaultColWidth := Scale(80);
  fGrid.DefaultRowHeight := Scale(DEF_ROWHEIGHT);
  fGrid.Color := clBlack;
  fGrid.ParentColor := True;
  fGrid.DefaultDrawing := False;
  fGrid.DrawingStyle := TGridDrawingStyle.gdsClassic;
  fGrid.BorderStyle := bsNone;
  fGrid.Options := fGrid.Options
                   - [TGridOption.goDrawFocusSelected, TGridOption.goVertLine, TGridOption.goHorzLine, TGridOption.goRangeSelect, TGridOption.goFixedVertLine, TGridOption.goFixedHorzLine]
                   + [TGridOption.goColSizing, TGridOption.goFixedRowClick];


//  fGrid.RowHeights[0] := fGrid.DefaultRowHeight + fGrid.DefaultRowHeight div 2;
  fGrid.Font.Name := 'Segoe UI'; // todo: delete?
  fGrid.Font.Height := fGrid.DefaultRowHeight - Scale(2);
  fGrid.Canvas.Font.Height := fGrid.DefaultRowHeight - Scale(2);
  const TW4 = TW(4);

  fGrid.ColWidths[TRecord.prop_refindex]      := TW(6);
  fGrid.ColWidths[TRecord.prop_sectionindex]  := TW4;
  fGrid.ColWidths[TRecord.prop_levelindex]    := TW4;
  fGrid.ColWidths[TRecord.prop_stylename]     := TW4 * 4;
  fGrid.ColWidths[TRecord.prop_leveltitle]    := TW4 * 8;
  fGrid.ColWidths[TRecord.prop_releaserate]   := TW4;
  fGrid.ColWidths[TRecord.prop_lemmingscount] := TW4;
  fGrid.ColWidths[TRecord.prop_rescuecount]   := TW4;
  fGrid.ColWidths[TRecord.prop_timelimit]     := TW4;
  fGrid.ColWidths[TRecord.prop_climbercount]  := TW4;
  fGrid.ColWidths[TRecord.prop_floatercount]  := TW4;
  fGrid.ColWidths[TRecord.prop_bombercount]   := TW4;
  fGrid.ColWidths[TRecord.prop_blockercount]  := TW4;
  fGrid.ColWidths[TRecord.prop_buildercount]  := TW4;
  fGrid.ColWidths[TRecord.prop_bashercount]   := TW4;
  fGrid.ColWidths[TRecord.prop_minercount]    := TW4;
  fGrid.ColWidths[TRecord.prop_diggercount]   := TW4;
  fGrid.ColWidths[TRecord.prop_graphicset]    := TW4;
  fGrid.ColWidths[TRecord.prop_graphicsetex]  := TW4;
  fGrid.ColWidths[TRecord.prop_superlemming]  := TW4;
  fGrid.ColWidths[TRecord.prop_objectcount]   := TW4;
  fGrid.ColWidths[TRecord.prop_terraincount]  := TW4;
  fGrid.ColWidths[TRecord.prop_entrancecount] := TW4;
  fGrid.ColWidths[TRecord.prop_exitcount]     := TW4;
  fGrid.ColWidths[TRecord.prop_levelhash]     := TWCourier(16 + 1); // TW4 * 4;
  fGrid.ColWidths[TRecord.prop_levelcode]     := TWCourier(10 + 1); //TW4 * 3;
  fGrid.ColWidths[TRecord.prop_duplicates]    := TW4;
  fGrid.ColWidths[TRecord.prop_sourcefile]    := TW4 * 8;

  // adjust last title column width
  var totalColWidths := 0;
  for var i := 0 to fgrid.ColCount - 2 do
    Inc(totalColWidths, fGrid.ColWidths[i]);
  if totalColWidths < fGrid.ClientWidth then
    fGrid.ColWidths[TRecord.prop_sourcefile] := fGrid.ClientWidth - totalColWidths;

  fGrid.OnDrawCell := Grid_DrawCell;
  fGrid.OnDblClick := Grid_DblClick;
  fGrid.OnSelectCell := Grid_SelectCell;
  fGrid.OnFixedCellClick := Grid_FixedCellClick;
  fGrid.OnKeyDown := Grid_KeyDown;
  fGrid.OnKeyPress := Grid_KeyPress;

  fLabelRecordCount := TLabelEx.Create(Self);
  fLabelRecordCount.Parent := Self;
  fLabelRecordCount.AutoSize := False;
  fLabelRecordCount.Height := Scale(DEF_ROWHEIGHT); // todo scaling stuff
  fLabelRecordCount.Font.Height := fLabelRecordCount.Height - Scale(2);
  fLabelRecordCount.Align := alBottom;
  fLabelRecordCount.Alignment := taCenter;
  fLabelRecordCount.Font.Color := clgray;

  fEdit := TEdit.Create(Self);
  fEdit.Parent := fGrid;
  fEdit.Visible := False;
  fEdit.Color := fFixedShadowColor;// clblack;
  fEdit.AutoSelect := False;
  fEdit.Font.Color := clYellow;
  fEdit.BorderStyle := bsNone;
  fEdit.OnKeyDown := Editor_KeyDown;
  fEdit.OnKeyPress := Editor_KeyPress;

  fSortColumn := TRecord.prop_refindex;
  fSortedAscending := True;
  UpdateLabel;

  fGrid.Col := TRecord.prop_leveltitle;

  fApproxRowsOnScreen := (fGrid.Height div Scale(DEF_ROWHEIGHT));

  if Assigned(initiallySelectedRecord) then
    SetSelectedRecord(initiallySelectedRecord, fApproxRowsOnScreen div 2);
end;


function TGameScreenFinder.GetItem(aRow: Integer): TRecord;
begin
  if (aRow >= FIRST_DATAROW) and (aRow -  HEADER_ROWCOUNT < fRefList.Count) then
    Result := fRecordList[fRefList[aRow - HEADER_ROWCOUNT]]
  else
    Result := nil;
end;

procedure TGameScreenFinder.PaintArrowDown(const aRect: TRect; aColor: TColor);
// sortmarker
var
  P: TPoint;
begin
  if aRect.Width < Scale(10) then
    Exit;
  P := Point(aRect.Left + Scale(2), aRect.Top + Scale(4));
  fGrid.Canvas.Pen.Color := aColor;
  DrawArrow(fGrid.Canvas, Vcl.GraphUtil.sdDown, P, Scale(5));
end;

procedure TGameScreenFinder.PaintArrowUp(const aRect: TRect; aColor: TColor);
// sortmarker
var
  P: TPoint;
begin
  if aRect.Width < Scale(10) then
    Exit;
  P := Point(aRect.Left + Scale(2), aRect.Top + Scale(4));
  fGrid.Canvas.Pen.Color := aColor;
  DrawArrow(fGrid.Canvas, Vcl.GraphUtil.sdUp, P, Scale(5));
end;

procedure TGameScreenFinder.SetSelectedRecord(item: TRecord; aRelativePosition: Integer);
var
  newTop: Integer;
  found: Boolean;
begin
  if (item = nil) and (fGrid.RowCount <= HEADER_ROWCOUNT) then
    Exit;
  found := False;
  for var i := FIRST_DATAROW to fGrid.RowCount - 1 do
    if GetItem(i) = item then begin
      fGrid.Row := i;
      found := True;
      Break;
    end;

  // not found
  if not found then begin
    if fGrid.RowCount > HEADER_ROWCOUNT then
      fGrid.Row := FIRST_DATAROW;
    Exit;
  end;


//  if fGrid.VisibleRowCount >

  exit;
  newTop := fGrid.Row - aRelativePosition;
  if (newTop >= FIRST_DATAROW) and (fGrid.Row > fApproxRowsOnScreen) and (fGrid.RowCount >= fApproxRowsOnScreen) then // (fGrid.RowCount > fGrid.ClientHeight div Scale(20)) then
    fGrid.TopRow := newTop
//  else
  //  fGrid.TopRow := FIRST_DATAROW
end;

procedure TGameScreenFinder.Sort(aCol: Integer; Asc: Boolean);
begin
  Lock;
  try
    fRefList.Sort(
       TComparer<Integer>.Construct(
         function(const a, b: Integer): Integer
         var
           r1, r2: TRecord;
           compareResult: TVariantRelationShip;
         begin
           r1 := fRecordList[a];
           r2 := fRecordList[b];

           if aCol in [TRecord.prop_stylename, TRecord.prop_leveltitle, TRecord.prop_levelcode, TRecord.prop_sourcefile] then
             Result := CompareText(r1.GetText(aCol), r2.GetText(aCol))
           else begin
               compareResult:= VarCompareValue(r1.GetProp(aCol).Value, r2.GetProp(aCol).Value);
             case compareResult of
               TVariantRelationship.vrLessThan: Result := -1;
               TVariantRelationship.vrGreaterThan: Result := 1;
             else
               Result := 0;
             end;
           end;

           if not Asc then Result := -Result;
         end)
    );
    fSortColumn := aCol;
    fSortedAscending := Asc;

  finally
    Unlock;
  end;
end;

procedure TGameScreenFinder.TryCloseAndPlay;
begin
  var item: TRecord := GetItem(fGrid.Row);
  if not Assigned(item) then
    Exit;
  fScreenResult := True;
  App.NewStyleName := item.Ref.StyleName;
  App.NewSectionIndex := Item.Ref.SectionIndex;
  App.NewLevelIndex := Item.Ref.LevelIndex;
  CloseScreen(TGameScreenType.Unknown);// TGameScreenType.Preview);
end;

procedure TGameScreenFinder.Lock;
begin
  Inc(fLockCount);
  if fLockCount = 1 then begin
    fLastSelectedRecord := GetItem(fGrid.Row);
    fLastRelativePosition := fGrid.Row - fGrid.TopRow;
  end;
end;

procedure TGameScreenFinder.Unlock;
begin
  if fLockCount = 1 then begin
    SetSelectedRecord(fLastSelectedRecord, fLastRelativePosition);
    fGrid.Invalidate;
  end;
  Dec(fLockCount);
end;

procedure TGameScreenFinder.UpdateLabel;
begin
  if fRefList.Count = fRecordList.Count then
    fLabelRecordCount.Caption := fRefList.Count.ToString + gt.SWordLevels
  else
    fLabelRecordCount.Caption := fRefList.Count.ToString + ' / ' + fRecordList.Count.ToString + gt.SWordLevels;
end;

procedure TGameScreenFinder.Filter;
begin
  Lock;
  try
    fRefList.Clear;

    var filterlist: TList<Integer> := TList<Integer>.Create;
    try

      for var i := 0 to TRecord.property_count - 1 do
        if not fUserFilters[i].IsEmpty then
          filterList.Add(i);

      // no filter at all, show complete list
      if filterlist.Count = 0 then begin
        for var ix := 0 to fRecordList.Count - 1 do
           fRefList.Add(ix);
      end
      // filtered
      else begin
        var ix: Integer := 0;
        for var rec: TRecord in fRecordList do begin

          var accept: Boolean := True;
          for var prop: integer in filterlist do begin
            accept := accept and rec.Filter(prop, fUserFilters[prop]);
            if not accept then
              break;
          end;
          if accept then
             fRefList.Add(ix);
          Inc(ix);
        end;
      end;

      if fRefList.Count > 0 then
        fGrid.RowCount := 3 + fRefList.Count
      else
        fGrid.RowCount := 4;
      Sort(fSortColumn, fSortedAscending);
      UpdateLabel;

    finally
      filterlist.Free;
    end;

  finally
    Unlock;
  end;
end;

//procedure TGameScreenFinder.Form_Paint(Sender: TObject);
//begin
//  Canvas.Brush.Color := clSilver;
//  Canvas.FrameRect(ClientRect);
//end;

procedure TGameScreenFinder.ClearFilters;
begin
  for var i := 0 to TRecord.property_count - 1 do
    fUserFilters[i] := string.Empty;
  Filter;
end;

procedure TGameScreenFinder.CopyGridToClipBoard;
var
  s: string;
begin
  var list: TStringList := TStringList.Create;

  s := string.Empty;
  for var p := 0 to TRecord.property_count - 1 do begin
    s := s + Headers[p];
    if p < TRecord.property_count - 1 then
      s := s + tab;
  end;
  list.Add(s);

  for var i: Integer in fReflist do begin
    var rec: TRecord := fRecordList[i];
    s := string.Empty;
    for var p := 0 to TRecord.property_count - 1 do begin
      s := s + rec.GetText(p);
      if p < TRecord.property_count - 1 then
        s := s + tab;
    end;
    list.Add(s);
  end;
  ClipBoard.AsText := list.Text;
  list.Free;
end;

procedure TGameScreenFinder.Grid_DblClick(Sender: TObject);
begin
  // prevent closing when dblclick on fixedrow
  var y := calcCursorPos.Y;
  var y2 := fGrid.CellRect(0, fGrid.TopRow).Top;
  if y < y2 then
    Exit;

  if fGrid.Row >= FIRST_DATAROW then
    TryCloseAndPlay;
end;

procedure TGameScreenFinder.Grid_DrawCell(Sender: TObject; aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
var
  item: TRecord;
  canv: TCanvas;
  txt: string;
  rowHighlight: Boolean;
begin
  if fLockCount <> 0 then
    Exit;

  txt := string.Empty;
  canv := fGrid.Canvas;
  canv.Font := fGrid.Font;
  item := GetItem(aRow);

  if (gdFixed in aState) then
    canv.Brush.Color := fFixedColor
  else
    canv.Brush.Color := clBlack;

  rowHighlight := (aRow >= FIRST_DATAROW) and (aRow = fGrid.Row) and not (aCol = fGrid.Col);
  if rowHighlight then
    canv.Brush.Color := RGB(16,20,16);

  canv.FillRect(aRect);

  if aRow = 1 then begin
    if aCol = fSortColumn then begin
      if fSortedAscending then PaintArrowUp(aRect, clLime) else PaintArrowDown(aRect, clLime)
    end
    else
      PaintArrowUp(aRect, fFixedShadowColor);
    Exit;
  end;

  // row #1 has no text
  if aRow <> 1 then begin

    if Assigned(item) then
      txt := item.GetText(aCol)
    else begin
      case aRow of
        0: txt := fUserFilters[aCol];
        2: txt := Headers[aCol];
      end;
    end;

    canv.Font.Color := ColumnColors[aCol];

    if Assigned(item) and (aCol = TRecord.prop_stylename) then begin
      case item.MyStyle of
        sOriginal : canv.Font.Color := TGameScreenOptions.COLOR_DOS;
        sCustom   : canv.Font.Color := TGameScreenOptions.COLOR_CUSTOM;
        sLemmini  : canv.Font.Color := TGameScreenOptions.COLOR_LEMMINI;
      end;
    end
    else  if Assigned(item) and (aCol = TRecord.prop_releaserate) then begin
      if not (item.Ref.Statics.ReleaseRate in [1..99]) then
        canv.Font.Color := clRed;
    end
    else
      if aRow < FIRST_DATAROW then begin
        if aRow = 0 then
          canv.Font.Color := clYellow // filter
        else
          canv.Font.Color := clLime; // header
      end;

    if (aCol in [TRecord.prop_lemmingscount..TRecord.prop_diggercount, TRecord.prop_graphicsetex, TRecord.prop_superlemming, TRecord.prop_duplicates])
    and (txt = '0') then begin
      txt := '-';
      canv.Font.Color := RGB(60,60,60);
    end;

    if aCol in [TRecord.prop_levelhash, TRecord.prop_levelcode] then
      canv.Font.Name := 'Courier New'; // Lucida Console

    var size: TSize;
    GetTextExtentPoint(canv.Handle, 'Wq', 2, size);
    var yDelta := (Scale(DEF_ROWHEIGHT) - size.cy) div 2;
    SetBkMode(canv.Handle, TRANSPARENT);

    // fake record when filtered empty
    if (item = nil) and (aRow >= FIRST_DATAROW) then begin
      canv.Font.Color := clGray;
      txt := '-';
      if aCol = TRecord.prop_leveltitle then txt := gt.SWordGridNullCell;
    end;

    canv.TextOut(aRect.Left + Scale(3), aRect.Top + yDelta{+ Scale(2)}, txt);

  end;

  // line
  canv.Pen.Color := RGB(20,20,20);
  if aRow < 3 then
    canv.Pen.Color := clSilver;
  if aRow <> 1 then begin
  canv.MoveTo(aRect.Left, aRect.Bottom - 1);
  canv.LineTo(aRect.Right, aRect.Bottom - 1); end;

  // selector
  if (gdSelected in aState) and (aRow > 2) then begin
    canv.Brush.Color := clLime;
    canv.FrameRect(aRect);
  end;

end;

procedure TGameScreenFinder.Grid_FixedCellClick(Sender: TObject; aCol, aRow: Longint);
begin
  if (aRow = 0) then begin
    fGrid.Col := aCol;
    EditorStart(aCol);
  end
  else if aRow in [1, 2] then begin
    if aCol = fSortColumn then
      Sort(aCol, not fSortedAscending)
    else
      Sort(aCol, True);
  end;
end;

function TGameScreenFinder.GetHelpText: THelpString;
begin
  Result.Add(VK_ESCAPE, gt.SHelpFinderScreen_CloseScreen);
  Result.Add(VK_RETURN, [ssCtrl], gt.SHelpFinderScreen_StartSelectedLevel);
  Result.Add(VK_F2, gt.SHelpFinderScreen_EditFilterOfColumn);
  Result.Add(VK_F2, [ssCtrl], gt.SHelpFinderScreen_ClearFilterOfColumn);
  Result.Add(VK_F3, gt.SHelpFinderScreen_ClearAllFilters);
  Result.Add(Ord('C'), [ssCtrl], gt.SHelpFinderScreen_CopyCellTextToClipboard);
  Result.Add(Ord('E'), [ssCtrl], gt.SHelpFinderScreen_CopyContentsToClipboard);
end;

procedure TGameScreenFinder.Grid_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EditorVisible then
    Exit;

  if Shift = [] then begin
    case Key of
      VK_ESCAPE:
        begin
          Key := 0;
          CloseScreen(TGameScreenType.Unknown);
        end;
      VK_F2:
        begin
          Key := 0;
          EditorStart(fGrid.Col);
        end;
      VK_F3:
        begin
          Key := 0;
          ClearFilters;
        end;
    end;
  end
  else if ssCtrl in Shift then begin
    case Key of
      VK_RETURN:
        if ssCtrl in Shift then TryCloseAndPlay;
      VK_F2:
        begin
          Key := 0;
          if not fUserFilters[fGrid.Col].IsEmpty then begin
            fUserFilters[fGrid.Col] := string.Empty;
            Filter;
          end;
        end;
      VK_END:
        begin
          Key := 0;
          if fGrid.RowCount > 3 then
            fGrid.Row := fGrid.RowCount - 1;
        end;
      VK_HOME:
        begin
          Key := 0;
          if fGrid.RowCount > 3 then
            fGrid.Row := 3;
        end;
    end;
  end;
end;

procedure TGameScreenFinder.Grid_KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '?':
       begin
         TGameScreenHelp.Execute(gt.SNameFinderScreen, GetHelpText); // vcl.forms
       end;
    'a'..'z', 'A'..'Z', '0'..'9', '!', '.':
      begin
        if not EditorVisible then begin
          EditorStart(fGrid.Col, key);
        end;
      end;
    ^C:
       begin
        var item: TRecord := GetItem(fGrid.Row);
        if Assigned(item) then begin
          var col: Integer := fGrid.Col;
          ClipBoard.AsText := item.GetText(col);
        end;
      end;
    ^E:
      begin
        CopyGridToClipBoard;
        key := #0;
      end;
  end;
end;

procedure TGameScreenFinder.Grid_SelectCell(Sender: TObject; aCol, aRow: Longint; var canSelect: Boolean);
begin
  EditorEnd(False);
  canSelect := aRow >= FIRST_DATAROW;
  if fLockCount = 0 then begin
    if fGrid.Row >= FIRST_DATAROW then
     fGrid.InvalidateRow(fGrid.Row);
    fGrid.InvalidateRow(aRow);
  end;
end;

function TGameScreenFinder.EditorVisible: Boolean;
begin
  Result := fEdit.Visible;
end;

procedure TGameScreenFinder.EditorEnd(accept: Boolean);
begin
  if EditorVisible then begin
    fEdit.Hide;
    if accept and (fEditorCol >= 0) then begin
      fUserFilters[fEditorCol] := fEdit.Text;
      fEditorCol := -1;
      Filter;
    end;
    if not fGrid.Focused then
      fGrid.SetFocus;
  end;
end;

procedure TGameScreenFinder.EditorStart(aCol: Integer; const key: Char= #0);
var
  r: TRect;
begin
  r := fGrid.CellRect(aCol, 0);
  r.Inflate(-1, -1);
  fEdit.BoundsRect := r;
  fEdit.Font.Height := Scale(DEF_ROWHEIGHT) - Scale(2);
  fEditorCol := aCol;
  if key = #0 then begin
    fEdit.Text := fUserFilters[aCol];
    fEdit.SelectAll;
  end
  else begin
    fEdit.Text := key;
    fEdit.SelStart := 2;
    fEdit.SelLength := 0;
  end;
  fEdit.Show;
  fEdit.SetFocus;
end;

procedure TGameScreenFinder.Editor_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin Key := 0; EditorEnd(True); end;
    VK_ESCAPE: begin Key := 0; EditorEnd(False); end;
  end;
end;

procedure TGameScreenFinder.Editor_KeyPress(Sender: TObject; var Key: Char);
// #$#$(% avoid Beep
begin
  if (Key = #13) or (Key = Chr(VK_ESCAPE)) then
    Key := #0;
end;

class function TGameScreenFinder.Execute: Boolean;
begin
  var f: TGameScreenFinder := TGameScreenFinder.Create(nil);
  try
    f.ShowScreen;
    Result := f.fScreenResult;
  finally
    f.Free;
  end;
end;

end.

