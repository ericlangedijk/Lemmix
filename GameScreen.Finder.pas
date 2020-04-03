unit GameScreen.Finder;

interface

// THIS SCREEN IS IN AN EXPERIMENTAL PHASE

// todo: do not use Grid DblClick: dblclick on header will PLAY the level and only god knows why. Row is not updated.
// todo: grid index out of range

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, System.UITypes, System.Contnrs, System.Variants,
  System.Generics.Defaults,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.GraphUtil, Vcl.Grids, Vcl.ClipBrd,
  Base.Utils, Base.Bitmaps,
  Dos.Consts,
  Prog.Types, Prog.Base, Prog.Data, Prog.Cache,
  Styles.Base,
  Game,
  Prog.App,
  Form.Base, Form.Message;

type
  //  This is a 'normal' windows screen
  TGameScreenFinder = class(TBaseDosForm)
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
          //prop_cachedate       = 28;

      private
        Ref: TStyleCache.TLevelCacheItem; // ref to original cache item in the style cache
        RefIndex: Integer; // the original index in the cache
        Duplicates: Integer; // calulated once during contruction
        MyStyle: (sOriginal, sCustom, sLemmini); // for colorizing
        //Cachedate: TDateTime;
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
        clSilver, clWebRoyalBlue, clWebRoyalBlue, clNone, clWhite,
        clFuchsia, clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,clFuchsia,
        clWebOrangeRed, clWebOrangeRed, clWebCornFlowerBlue, clWebOrangeRed, clWebOrangeRed, clWebOrangeRed, clWebOrangeRed, clWebRoyalBlue, clWebRoyalBlue, clRed, clWebLightCoral);
    const
      DEF_ROWHEIGHT = 21;
      HEADER_ROWCOUNT = 3;
      FIRST_DATAROW = 3;
  private
    fLockCount: Integer;
    fRecordList: TRecordList;
    fRefList: TList<Integer>;
    fGrid: TDrawGrid;
    fSortColumn: Integer;
    fSortedAscending: Boolean;
    fLabelRecordCount: TLabel;
    fEdit: TEdit;
    fEditorCol: Integer;
    fLastSelectedRecord: TRecord;
    fLastRelativePosition: Integer;
    fApproxRowsOnScreen: Integer;
    fUserFilters: array[0..TRecord.property_count - 1] of string;
    fHighlightBitmap: TBitmap;
    function GetItem(aRow: Integer): TRecord; inline;

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

    procedure EditorStart(aCol: Integer);
    procedure EditorEnd(accept: Boolean);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

uses
  GameScreen.Options; // only for color consts GUI

type
  TDrawGridHack = class(TDrawGrid);

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
    prop_refindex: SetResult(RefIndex.ToString);
    prop_sectionindex: SetResult(Ref.SectionIndex.ToString);
    prop_levelindex: SetResult(Ref.LevelIndex.ToString);
    prop_stylename: SetResult(Ref.StyleName);
    prop_leveltitle: SetResult(Trim(string(Ref.LevelTitle)));

    prop_releaserate: SetResult(Ref.Statics.ReleaseRate.ToString);
    prop_lemmingscount: SetResult(Ref.Statics.LemmingsCount.ToString);
    prop_rescuecount: SetResult(Ref.Statics.RescueCount.ToString);
    prop_timelimit: SetResult(Ref.Statics.TimeLimit.ToString);
    prop_climbercount: SetResult(Ref.Statics.ClimberCount.ToString);
    prop_floatercount: SetResult(Ref.Statics.FloaterCount.ToString);
    prop_bombercount: SetResult(Ref.Statics.BomberCount.ToString);
    prop_blockercount: SetResult(Ref.Statics.BlockerCount.ToString);
    prop_buildercount: SetResult(Ref.Statics.BuilderCount.ToString);
    prop_bashercount: SetResult(Ref.Statics.BasherCount.ToString);
    prop_minercount: SetResult(Ref.Statics.MinerCount.ToString);
    prop_diggercount: SetResult(Ref.Statics.DiggerCount.ToString);
    prop_graphicset: SetResult(Ref.Statics.GraphicSet.ToString);
    prop_graphicsetex: SetResult(Ref.Statics.GraphicSetEx.ToString);
    prop_superlemming: SetResult(Byte(Ref.Statics.SuperLemming).ToString);
    prop_objectcount: SetResult(Ref.Statics.ObjectCount.ToString);
    prop_terraincount: SetResult(Ref.Statics.TerrainCount.ToString);
    prop_entrancecount: SetResult(Ref.Statics.EntranceCount.ToString);
    prop_exitcount: SetResult(Ref.Statics.ExitCount.ToString);

    prop_levelhash: SetResult(IntToHex(Ref.LevelHash, 16));
    prop_duplicates: SetResult(Duplicates.ToString);
    prop_levelcode: SetResult(Ref.LevelCode);
    prop_sourcefile: SetResult(Ref.SourceFile);
    //prop_cachedate: SetResult('date');
  else
    SetResult('');
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
    //prop_cachedate        : SetResult(Cachedate, '1');
  else
    SetResult(RefIndex, RefIndex.ToString);
  end;
end;

function TGameScreenFinder.TRecord.Filter(index: Integer; const s: string): Boolean;
begin
  if index in [prop_stylename, prop_leveltitle, prop_levelhash, prop_levelcode, prop_sourcefile] then
    Result := GetText(index).ToUpper.Contains(s.ToUpper)
  else
    Result := GetText(index) = s;
end;

{ TGameScreenFinder }

constructor TGameScreenFinder.Create(aOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  Cursor := crDefault;
  fRecordList := TRecordList.Create(True);
  fRefList := TList<Integer>.Create;
  fGrid := TDrawGrid.Create(Self);
  fHighlightBitmap := TBitmap.Create;
  fHighlightBitmap.SetSize(32, 32);
  fHighlightBitmap.Canvas.Brush.Color := clLime;
  fHighlightBitmap.Canvas.FillRect(Rect(0, 0, 32, 32));
end;

destructor TGameScreenFinder.Destroy;
begin
  fRecordList.Free;
  fRefList.Free;
  fHighlightBitmap.Free;
  inherited;
end;

procedure TGameScreenFinder.BuildScreen;

   function TW(cnt: Integer): Integer;
   begin
     var a: Integer := fGrid.Canvas.TextWidth(StringOfChar('W', cnt));
     var b: Integer := fGrid.Canvas.TextWidth(StringOfChar('y', cnt));
     Result := (a + b) div 2;
   end;

var
  initiallySelectedRecord: TRecord;
  currLevel: TLevelLoadingInformation;
  cachedItem: TStyleCache.TLevelCacheItem;
  newRec: TRecord;
//  totalColWidths: Integer;
begin
  initiallySelectedRecord := nil;
  currLevel := App.CurrentLevelInfo;

//  for var cachename: string in App.StyleCache.GetCacheFilenames do begin
//
//  end;

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
  fGrid.Margins.SetBounds(8, 8, 8, 8);
  fGrid.ColCount := (fGrid.Width - Scale(24)) div Scale(320);
  fGrid.ScrollBars := ssNone;
  fGrid.RowCount := fRecordList.Count + 3;
  fGrid.ColCount := TRecord.property_count;
  fGrid.FixedRows := 3; // 0 = filter, 1 = sort, 2 = title
  fGrid.FixedCols := 0;
  fGrid.DefaultColWidth := 80;
  fGrid.DefaultRowHeight := DEF_ROWHEIGHT;
  fGrid.Color := clBlack;
  fGrid.ParentColor := True;
  fGrid.DefaultDrawing := False;
  fGrid.DrawingStyle := TGridDrawingStyle.gdsClassic;
  fGrid.BorderStyle := bsNone;
  fGrid.Options := fGrid.Options
                   - [TGridOption.goDrawFocusSelected, TGridOption.goVertLine, TGridOption.goHorzLine, TGridOption.goRangeSelect, TGridOption.goFixedVertLine, TGridOption.goFixedHorzLine]
                   + [TGridOption.goColSizing, TGridOption.goFixedRowClick];


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
  fGrid.ColWidths[TRecord.prop_levelhash]     := TW4 * 4;
  fGrid.ColWidths[TRecord.prop_levelcode]     := TW4 * 3;
  fGrid.ColWidths[TRecord.prop_duplicates]    := TW4;
  fGrid.ColWidths[TRecord.prop_sourcefile]    := TW4 * 8;

//  // todo: we have to find out what is going wrong here on High-Dpi
//  totalColWidths := 0;
//  for var i := 0 to fgrid.ColCount - 2 do
//    Inc(totalColWidths, fGrid.ColWidths[i]);
//
////  dlg(totalColWidths.ToString + ' ' + fGrid.ClientWidth.ToString);
//
//  //var w: integer := fGrid.ColWidths[TRecord.prop_sourcefile];
//  if totalColWidths < fGrid.ClientWidth then
//    fGrid.ColWidths[TRecord.prop_sourcefile] := fGrid.ClientWidth - totalColWidths;

//  fGrid.RowHeights[1] := 12;

  fGrid.OnDrawCell := Grid_DrawCell;
  fGrid.OnDblClick := Grid_DblClick;
  fGrid.OnSelectCell := Grid_SelectCell;
  fGrid.OnFixedCellClick := Grid_FixedCellClick;
  fGrid.OnKeyDown := Grid_KeyDown;
  fGrid.OnKeyPress := Grid_KeyPress;

  fLabelRecordCount := TLabel.Create(Self);
  fLabelRecordCount.Parent := Self;
  fLabelRecordCount.AutoSize := False;
  fLabelRecordCount.Height := DEF_ROWHEIGHT;
  fLabelRecordCount.Align := alBottom;
  fLabelRecordCount.Alignment := taCenter;
  fLabelRecordCount.Font.Color := clgray;

  fEdit := TEdit.Create(Self);
  fEdit.Parent := fGrid;
  fEdit.Visible := False;
  fEdit.Color := clblack;
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
  App.NewStyleName := item.Ref.StyleName;
  App.NewSectionIndex := Item.Ref.SectionIndex;
  App.NewLevelIndex := Item.Ref.LevelIndex;
  CloseScreen(TGameScreenType.Preview);
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
    fLabelRecordCount.Caption := fRefList.Count.ToString + ' levels'
  else
    fLabelRecordCount.Caption := fRefList.Count.ToString + ' / ' + fRecordList.Count.ToString + ' levels';
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

      if filterlist.Count = 0 then begin
        for var ix := 0 to fRecordList.Count - 1 do
           fRefList.Add(ix);
      end
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

      fGrid.RowCount := 3 + fRefList.Count;
      Sort(fSortColumn, fSortedAscending);
      UpdateLabel;

    finally
      filterlist.Free;
    end;

  finally
    Unlock;
  end;
end;

procedure TGameScreenFinder.ClearFilters;
begin
  for var i := 0 to TRecord.property_count - 1 do
    fUserFilters[i] := '';
  Filter;
end;

procedure TGameScreenFinder.CopyGridToClipBoard;
var
  s: string;
begin
  const tab: char = Chr(9);
  var list: TStringList := TStringList.Create;

  s := '';
  for var p := 0 to TRecord.property_count - 1 do begin
    s := s + Headers[p];
    if p < TRecord.property_count - 1 then
      s := s + tab;
  end;
  list.Add(s);

  for var i: Integer in fReflist do begin
    var rec: TRecord := fRecordList[i];
    s := '';
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
  if (fGrid.Row >= FIRST_DATAROW) then begin
    TryCloseAndPlay;
  end;
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

  txt := '';
  canv := fGrid.Canvas;
  canv.Font := fGrid.Font;

//  prop.value := 0;
//  prop.Text := '';

  item := GetItem(aRow);

  if (gdFixed in aState) then
    canv.Brush.Color := RGB(20,20,20)
  else
    canv.Brush.Color := clBlack;

  rowHighlight := (aRow >= FIRST_DATAROW) and (aRow = fGrid.Row) and not (aCol = fGrid.Col);
  if rowHighlight then
    canv.Brush.Color := RGB(16,20,16);

//  if aRow = 0 then
  //  canv.Brush.Color := clWebIndigo;
//  if aRow < FIRST_DATAROW then
//    canv.Brush.Color := RGB(40,40,40);

  canv.FillRect(aRect);

  if aRow = 1 then begin
    if aCol = fSortColumn then begin
      if fSortedAscending then PaintArrowUp(aRect, clLime) else PaintArrowDown(aRect, clLime)
    end
    else
      PaintArrowUp(aRect, RGB(60,60,60));
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
      canv.Font.Name := 'Lucida Console';

    var size: TSize;
    GetTextExtentPoint(canv.Handle, 'Wq', 2, size);
    var yDelta := (Scale(DEF_ROWHEIGHT) - size.cy) div 2;
    SetBkMode(canv.Handle, TRANSPARENT);
//    canv.TextFlags := ETO_CLIPPED;
    canv.TextOut(aRect.Left + Scale(3), aRect.Top + yDelta{+ Scale(2)}, txt);

  end;

//  if rowHighlight then
  //  DrawTransparentBitmap(fHighlightBitmap, canv, aRect, 20);

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
//    fUserFilters[TRecord.prop_leveltitle] := 'just a final test';
  //  Filter;
//    fGrid.Row := 0;
//    fGrid.EditorMode := True;
  end
  else if aRow in [1,2] then begin
    if aCol = fSortColumn then
      Sort(aCol, not fSortedAscending)
    else
      Sort(aCol, True);
  end;
end;

procedure TGameScreenFinder.Grid_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EditorVisible then
    Exit;

  if Shift = [] then begin
    case Key of
      VK_ESCAPE: CloseScreen(TGameScreenType.Menu);
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
          if fUserFilters[fGrid.Col] <> '' then begin
            fUserFilters[fGrid.Col] := '';
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
  if Key = ^C then begin
    var item: TRecord := GetItem(fGrid.Row);
    if Assigned(item) then begin
      var col: Integer := fGrid.Col;
      //if col <> TRecord.prop_sourcefile then
        ClipBoard.AsText := item.GetText(col)
      //else
        //ClipBoard.AsText := Consts.PathToStyle[item.GetText(TRecord.prop_stylename)] + item.GetText(col);
    end;
  end
  else if Key = ^E then begin
    CopyGridToClipBoard;
    key := #0;
  end;
end;

procedure TGameScreenFinder.Grid_SelectCell(Sender: TObject; aCol, aRow: Longint; var canSelect: Boolean);
begin
  EditorEnd(False);
  canSelect := aRow >= FIRST_DATAROW;
  if fLockCount = 0 then begin
    if fGrid.Row >= FIRST_DATAROW then
      TDrawGridHack(fGrid).InvalidateRow(fGrid.Row);
    TDrawGridHack(fGrid).InvalidateRow(aRow);
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

procedure TGameScreenFinder.EditorStart(aCol: Integer);
var
  r: TRect;
begin
  r := fGrid.CellRect(aCol, 0);
  r.Inflate(-1, -1);
  fEdit.BoundsRect := r;
  fEditorCol := aCol;
  fEdit.Text := fUserFilters[aCol];
  fEdit.SelectAll;
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

end.

