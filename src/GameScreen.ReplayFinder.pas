unit GameScreen.ReplayFinder;

{$include lem_directives.inc}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, System.UITypes, System.Contnrs, System.Variants, System.StrUtils,
  System.Generics.Defaults, System.Character,
  Vcl.Graphics, Vcl.Imaging.PngImage, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.GraphUtil, Vcl.Grids, Vcl.ClipBrd,
  Base.Utils, Base.Types, Base.Bitmaps, Base.Strings,
  Dos.Consts,
  Prog.App, Prog.Base, Prog.Data, Prog.Cache,
  Styles.Base,
  Game,
  Form.Base, Form.Message, GameScreen.Help;

type
  TGameScreenReplayFinder = class(TToolForm)
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
          property_count       = 6;

          prop_folder          = 0;
          prop_filename        = 1;
          prop_title           = 2;
          prop_version         = 3;
          prop_creationdate    = 4;
          prop_hash            = 5;
      private
        Ref: TReplayCache.TReplayCacheItem; // ref to original cache item in the style cache
        RefIndex: Integer; // the original index in the cache
      public
        constructor Create(cacheIndex: Integer);
        function GetProp(index: Integer): TProp;
        function GetText(index: Integer): string; inline;
        function Filter(index: Integer; const s: string): Boolean;
     end;

      TRecordList = class(TFastObjectList<TRecord>);
  private
    const
      DEF_ROWHEIGHT = 20;
      HEADER_ROWCOUNT = 3;
      FIRST_DATAROW = 3;

      ROW_FILTER = 0;
      ROW_SORT = 1;
      ROW_TITLE = 2;

      Headers: array[0..TRecord.property_count - 1] of string = (
        'Folder', 'Filename', 'Title', 'Version', 'Created' , 'Hash'
      );

      ColumnColors : array[0..TRecord.property_count - 1] of TColor = (
        clWebLightCoral,   // prop_filename
        clWebLightCoral,   // prop_filename
        clWhite,           // prop_title
        clFuchsia,         // prop_version
        clWhite,           // prop_creationdate
        clWebRoyalBlue     // prop_hash
      );
  private
    fRecordList: TRecordList;
    fRefList: TList<Integer>;
    fGrid: TDrawGridEx;
    fEdit: TEdit;
    fEditorCol: Integer;
    fFixedColor: TColor;
    fFixedShadowColor: TColor;
    fRowHighlightColor: TColor;
    fLockCount: Integer;
    fLastSelectedRecord : TRecord;
    fLastRelativePosition :Integer;
    fSortColumn: Integer;
    fSortedAscending: Boolean;
    fUserFilters: array[0..TRecord.property_count - 1] of string;
    fApproxRowsOnScreen: Integer;
    fResultFile: string;
    fLabelRecordCount: TLabelEx;
    function GetHelpText: THelpString;
    procedure TryCloseAndLoad;
    function EditorVisible: Boolean;
    procedure EditorStart(aCol: Integer; const key: Char = #0);
    procedure EditorEnd(accept: Boolean);
    procedure Filter;
    procedure ClearFilters;
    procedure UpdateLabel;
    procedure Lock;
    procedure Unlock;
    procedure SetInitialFilter(info: TLevelLoadingInformation);
    procedure Sort(aCol: Integer; Asc: Boolean);
    procedure SetSelectedRecord(item: TRecord; aRelativePosition: Integer);
    procedure PaintArrowDown(const aRect: TRect; aColor: TColor);
    procedure PaintArrowUp(const aRect: TRect; aColor: TColor);
    function GetItem(aRow: Integer): TRecord;
  // events
    procedure Grid_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Grid_KeyPress(Sender: TObject; var Key: Char);
    procedure Grid_DrawCell(Sender: TObject; aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
    procedure Grid_SelectCell(Sender: TObject; aCol, aRow: Longint; var canSelect: Boolean);
    procedure Grid_FixedCellClick(Sender: TObject; aCol, aRow: Longint);
    procedure Grid_DblClick(Sender: TObject);
    procedure Editor_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Editor_KeyPress(Sender: TObject; var Key: Char);
  protected
    procedure Input(param: TObject); override;
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(inputLoadingInformation: TLevelLoadingInformation): string; static;
  end;

implementation

{ TGameScreenReplayFinder.TRecord }

constructor TGameScreenReplayFinder.TRecord.Create(cacheIndex: Integer);
begin
  RefIndex := cacheIndex;
  Ref := App.ReplayCache.FlatList[RefIndex];
end;

{ TGameScreenReplayFinder }

procedure TGameScreenReplayFinder.TryCloseAndLoad;
begin
  var item: TRecord := GetItem(fGrid.Row);
  if not Assigned(item) then
    Exit;
  fResultFile := item.Ref.Filename;
  CloseScreen(TGameScreenType.Unknown);
end;

function TGameScreenReplayFinder.TRecord.Filter(index: Integer; const s: string): Boolean;
begin
  if s.IsEmpty then
    Exit(True);
  if index in [prop_folder, prop_filename, prop_title] then
    Result := GetText(index).ToUpper.Contains(s.ToUpper)
  else
    Result := GetText(index) = s;
end;

function TGameScreenReplayFinder.TRecord.GetProp(index: Integer): TProp;

    procedure SetResult(const v: Variant; const s: string);
    begin
      Result.Value := v;
      Result.Text := s;
    end;

    function Folder(const s: string): string; inline;
    begin
      Result := ExtractFilePath(s).Replace(Consts.PathToReplay, string.Empty);
    end;

    function Fn(const s: string): string; inline;
    begin
      Result := s.Replace(Consts.PathToReplay, string.Empty);
    end;

begin
  case index of
    prop_folder           : SetResult(Folder(Ref.Filename), Folder(Ref.Filename));
    prop_filename         : SetResult(ExtractFileName(Ref.Filename), ExtractFileName(Ref.FileName));
    prop_title            : SetResult(string(Ref.LevelTitle), Trim(string(Ref.LevelTitle)));
    prop_version          : SetResult(Ref.Version, Ref.version.ToString);
    prop_creationdate     : SetResult(Ref.CreationDate, FormatDateTime('yyyy-mm-dd hh:NN:ss', Ref.CreationDate));
    prop_hash             : SetResult(Ref.LevelHash, IfThen(Ref.LevelHash = 0, '0', IntToHex(Ref.LevelHash, 16)));
  else
    SetResult(RefIndex, RefIndex.ToString);
  end;
end;

function TGameScreenReplayFinder.TRecord.GetText(index: Integer): string;
begin
  Result := GetProp(index).Text;
end;

{ TGameScreenReplayFinder }

procedure TGameScreenReplayFinder.Filter;
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
        fGrid.RowCount := fRefList.Count + HEADER_ROWCOUNT
      else
        fGrid.RowCount := HEADER_ROWCOUNT + 1;
      Sort(fSortColumn, fSortedAscending);
      UpdateLabel;

    finally
      filterlist.Free;
    end;

  finally
    Unlock;
  end;
end;

procedure TGameScreenReplayFinder.ClearFilters;
begin
  for var i := 0 to TRecord.property_count - 1 do
    fUserFilters[i] := string.Empty;
  Filter;
end;

constructor TGameScreenReplayFinder.Create(aOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  Color := clBlack;
  fRecordList := TRecordList.Create(True);
  fRefList := TList<Integer>.Create;
  fGrid := TDrawGridEx.Create(Self);
  fEdit := TEdit.Create(Self);
  fLabelRecordCount := TLabelEx.Create(Self);
  fFixedColor := RGB(0,32,64);
  fRowHighlightColor := RGB(16,20,16);
  fFixedShadowColor := RGB(0,48,80);
end;

destructor TGameScreenReplayFinder.Destroy;
begin
  fRecordList.Free;
  fRefList.Free;
  inherited;
end;

procedure TGameScreenReplayFinder.EditorStart(aCol: Integer; const key: Char = #0);
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

procedure TGameScreenReplayFinder.EditorEnd(accept: Boolean);
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

function TGameScreenReplayFinder.EditorVisible: Boolean;
begin
  Result := fEdit.Visible;
end;

procedure TGameScreenReplayFinder.Editor_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin Key := 0; EditorEnd(True); end;
    VK_ESCAPE: begin Key := 0; EditorEnd(False); end;
  end;
end;

procedure TGameScreenReplayFinder.Editor_KeyPress(Sender: TObject; var Key: Char);
// #$#$(% avoid Beep
begin
  if (Key = #13) or (Key = Chr(VK_ESCAPE)) then
    Key := #0;
end;

function TGameScreenReplayFinder.GetItem(aRow: Integer): TRecord;
begin
  if (aRow >= FIRST_DATAROW) and (aRow -  HEADER_ROWCOUNT < fRefList.Count) then
    Result := fRecordList[fRefList[aRow - HEADER_ROWCOUNT]]
  else
    Result := nil;
end;

procedure TGameScreenReplayFinder.BuildScreen;

   function TW(cnt: Integer): Integer;
   begin
     var a: Integer := fGrid.Canvas.TextWidth(StringOfChar('W', cnt));
     var b: Integer := fGrid.Canvas.TextWidth(StringOfChar('y', cnt));
     Result := (a + b) div 2;
   end;

var
  newRec: TRecord;
begin
  fSortedAscending := True;
  App.ReplayCache.Load(nil);
  // fill record list
  fRecordList.Clear;
  for var i := 0 to App.ReplayCache.FlatList.Count - 1 do begin
    newRec := TRecord.Create(i);
    fRecordList.Add(newRec);
    fRefList.Add(i);
    // initially selected item
    //cachedItem := App.ReplayCache.FlatList[i];
  end;

  // init grid
  fGrid.Parent := Self;
  fGrid.Align := alClient;
  fGrid.AlignWithMargins := True;
  fGrid.Margins.SetBounds(Scale(8), Scale(8), Scale(8), Scale(8));
  fGrid.ScrollBars := ssNone;
  if fRecordList.Count > 0
  then fGrid.RowCount := fRecordList.Count + HEADER_ROWCOUNT
  else fGrid.RowCount := fRecordList.Count + HEADER_ROWCOUNT + 1;
  fGrid.ColCount := TRecord.property_count;
  fGrid.FixedRows := HEADER_ROWCOUNT;
  fGrid.FixedCols := 0;
  fGrid.DefaultColWidth := Scale(80);
  fGrid.DefaultRowHeight := Scale(DEF_ROWHEIGHT);
  fGrid.Color := clBlack;
  fGrid.ParentColor := False;
  fGrid.DefaultDrawing := False;
  fGrid.DrawingStyle := TGridDrawingStyle.gdsClassic;
  fGrid.BorderStyle := bsNone;
  fGrid.Options := fGrid.Options
                   - [TGridOption.goDrawFocusSelected, TGridOption.goVertLine, TGridOption.goHorzLine, TGridOption.goRangeSelect, TGridOption.goFixedVertLine, TGridOption.goFixedHorzLine]
                   + [TGridOption.goColSizing, TGridOption.goFixedRowClick];
  fGrid.Font.Name := 'Segoe UI'; // todo: delete?
  fGrid.Font.Height := fGrid.DefaultRowHeight - Scale(2);
  fGrid.Canvas.Font.Height := fGrid.DefaultRowHeight - Scale(2);

  fGrid.ColWidths[TRecord.prop_folder]       := TW(32);
  fGrid.ColWidths[TRecord.prop_filename]     := TW(40);
  fGrid.ColWidths[TRecord.prop_title]        := TW(32);
  fGrid.ColWidths[TRecord.prop_version]      := TW(8);
  fGrid.ColWidths[TRecord.prop_creationdate] := TW(16);
  fGrid.ColWidths[TRecord.prop_hash]         := TW(18);

  // adjust last title column width
  var totalColWidths := 0;
  for var i := 1 to fgrid.ColCount - 2 do
    Inc(totalColWidths, fGrid.ColWidths[i]);
  if totalColWidths < fGrid.ClientWidth then
    fGrid.ColWidths[TRecord.prop_hash] := fGrid.ClientWidth - totalColWidths;

  fGrid.OnDrawCell := Grid_DrawCell;
  fGrid.OnSelectCell := Grid_SelectCell;
  fGrid.OnKeyDown := Grid_KeyDown;
  fGrid.OnKeyPress := Grid_KeyPress;
  fGrid.OnFixedCellClick := Grid_FixedCellClick;
  fGrid.OnDblClick := Grid_DblClick;

  fEdit.Parent := fGrid;
  fEdit.Visible := False;
  fEdit.Color := fFixedShadowColor;
  fEdit.Font.Color := clYellow;
  fEdit.BorderStyle := bsNone;
  fEdit.OnKeyDown := Editor_KeyDown;
  fEdit.OnKeyPress := Editor_KeyPress;
  fEdit.AutoSelect := False;

  fLabelRecordCount.Parent := Self;
  fLabelRecordCount.AutoSize := False;
  fLabelRecordCount.Height := Scale(DEF_ROWHEIGHT);
  fLabelRecordCount.Font.Height := fLabelRecordCount.Height - Scale(2);
  fLabelRecordCount.Align := alBottom;
  fLabelRecordCount.Alignment := taCenter;
  fLabelRecordCount.Font.Color := clgray;

  fApproxRowsOnScreen := (fGrid.Height div fGrid.DefaultRowHeight);
  fGrid.Col := 1;
  Filter;
end;

procedure TGameScreenReplayFinder.Input(param: TObject);
begin
  if Assigned(param) and (param is TLevelLoadingInformation) then
    SetInitialFilter(TLevelLoadingInformation(param));
end;

procedure TGameScreenReplayFinder.Lock;
begin
  Inc(fLockCount);
  if fLockCount = 1 then begin
    fLastSelectedRecord := GetItem(fGrid.Row);
    fLastRelativePosition := fGrid.Row - fGrid.TopRow;
  end;
end;

procedure TGameScreenReplayFinder.PaintArrowDown(const aRect: TRect; aColor: TColor);
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

procedure TGameScreenReplayFinder.PaintArrowUp(const aRect: TRect; aColor: TColor);
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

procedure TGameScreenReplayFinder.Unlock;
begin
  if fLockCount = 1 then begin
    SetSelectedRecord(fLastSelectedRecord, fLastRelativePosition);
    fGrid.Invalidate;
  end;
  Dec(fLockCount);
end;

procedure TGameScreenReplayFinder.UpdateLabel;
begin
  if fRefList.Count = fRecordList.Count then
    fLabelRecordCount.Caption := fRefList.Count.ToString + ' files'
  else
    fLabelRecordCount.Caption := fRefList.Count.ToString + ' / ' + fRecordList.Count.ToString + ' files';
end;

procedure TGameScreenReplayFinder.SetInitialFilter(info: TLevelLoadingInformation);
begin
  if info = nil then
    Exit;
  fUserFilters[TRecord.prop_title] := info.GetLevelTitle;
end;

procedure TGameScreenReplayFinder.SetSelectedRecord(item: TRecord; aRelativePosition: Integer);
// todo: optimize and always correct position
var
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
end;

procedure TGameScreenReplayFinder.Sort(aCol: Integer; Asc: Boolean);
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

           if aCol in [TRecord.prop_filename, TRecord.prop_title] then
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

procedure TGameScreenReplayFinder.Grid_DblClick(Sender: TObject);
begin
  // prevent closing when dblclick on fixedrow
  var y := calcCursorPos.Y;
  var y2 := fGrid.CellRect(0, fGrid.TopRow).Top;
  if y < y2 then
    Exit;
  if fGrid.Row >= FIRST_DATAROW then
    TryCloseAndLoad;
end;

procedure TGameScreenReplayFinder.Grid_DrawCell(Sender: TObject; aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
var
  canv: TCanvas;
  rec: TRecord;
  txt: string;
  fillColor: TColor;
  textColor: TColor;
  rowHighlight: Boolean;
begin
  canv := fGrid.Canvas;

  if (aCol = TRecord.prop_hash) and (aRow >= FIRST_DATAROW) then
    canv.Font.Name := 'Courier New'
  else
    canv.Font.Name := 'Segoe UI';

  canv.Font.Height := Scale(DEF_ROWHEIGHT) - Scale(2);


  rec := GetItem(aRow);
  txt := string.Empty;
  fillColor := clBlack;
  textColor := clWhite;

  rowHighlight := (aRow >= FIRST_DATAROW) and (aRow = fGrid.Row) and not (aCol = fGrid.Col);
  if rowHighlight then
    fillColor := RGB(16,20,16);

  if aRow < FIRST_DATAROW then
    fillColor := fFixedColor
  else if aRow >= FIRST_DATAROW then
    textColor := ColumnColors[aCol];

  if (aRow >= FIRST_DATAROW) and Assigned(rec) then
    txt := rec.GetText(aCol)
  else begin
    case aRow of
      ROW_FILTER:
        begin
          txt := fUserFilters[aCol];
          textColor := clYellow;
        end;
      ROW_TITLE:
       begin
         txt := Headers[aCol];
         textColor := clLime;
       end;
    end;
  end;

  var size: TSize;
  GetTextExtentPoint(canv.Handle, 'Wq', 2, size);
  var yDelta := (Scale(DEF_ROWHEIGHT) - size.cy) div 2;

  canv.Brush.Color := fillColor;
  canv.FillRect(aRect);

  if aRow = ROW_SORT then begin
    if aCol = fSortColumn then begin
      if fSortedAscending then PaintArrowUp(aRect, clLime) else PaintArrowDown(aRect, clLime)
    end
    else
      PaintArrowUp(aRect, fFixedShadowColor);
    Exit;
  end
  else if aRow >= FIRST_DATAROW then begin
    // nil row
    if not Assigned(rec) then begin
      txt := '-';
      textColor := clGray;
    end;
  end;

  if not txt.IsEmpty then begin
    SetBkMode(canv.Handle, TRANSPARENT);
    canv.Font.Color := textColor;
    canv.TextOut(aRect.Left + Scale(3), aRect.Top + yDelta, txt);
  end;

  // line
  canv.Pen.Color := RGB(20,20,20);
  if aRow > 2 then begin
    canv.MoveTo(aRect.Left, aRect.Bottom - 1);
    canv.LineTo(aRect.Right, aRect.Bottom - 1);
  end;

  // selector
  if (gdSelected in aState) and (aRow > 2) then begin
    canv.Brush.Color := clLime;
    canv.FrameRect(aRect);
  end;
end;

procedure TGameScreenReplayFinder.Grid_FixedCellClick(Sender: TObject; aCol, aRow: Longint);
begin
  if (aRow = ROW_FILTER) then begin
    fGrid.Col := aCol;
    EditorStart(aCol);
  end
  else if aRow in [ROW_SORT, ROW_TITLE] then begin
    if aCol = fSortColumn then
      Sort(aCol, not fSortedAscending)
    else
      Sort(aCol, True);
  end;
end;

function TGameScreenReplayFinder.GetHelpText: THelpString;
begin
  Result.Add(VK_ESCAPE, gt.SHelpReplayFinderScreen_CloseScreen);
  Result.Add(VK_RETURN, [ssCtrl], gt.SHelpReplayFinderScreen_StartReplay);
  Result.Add(VK_F2, gt.SHelpReplayFinderScreen_EditFilterOfColumn);
  Result.Add(VK_F2, [ssCtrl], gt.SHelpReplayFinderScreen_ClearFilterOfColumn);
  Result.Add(VK_F3, gt.SHelpReplayFinderScreen_ClearAllFilters);
end;

procedure TGameScreenReplayFinder.Grid_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EditorVisible then
    Exit;
  if Shift = [] then begin
    case Key of
      VK_ESCAPE: CloseScreen(TGameScreenType.Unknown);
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
        TryCloseAndLoad;
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

procedure TGameScreenReplayFinder.Grid_KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '?':
      begin
        TGameScreenHelp.Execute(gt.SNameReplayFinderScreen, GetHelpText);
        Key := #0;
      end;
    'a'..'z', 'A'..'Z', '0'..'9', '!', '.':
      begin
        if not EditorVisible then begin
          EditorStart(fGrid.Col, key);
        end;
        Key := #0;
      end;
  end;
end;

procedure TGameScreenReplayFinder.Grid_SelectCell(Sender: TObject; aCol, aRow: Longint; var canSelect: Boolean);
begin
  EditorEnd(False);
  canSelect := aRow >= FIRST_DATAROW;
  if fLockCount = 0 then begin
    if fGrid.Row >= FIRST_DATAROW then
     fGrid.InvalidateRow(fGrid.Row);
    fGrid.InvalidateRow(aRow);
  end;
end;

class function TGameScreenReplayFinder.Execute(inputLoadingInformation: TLevelLoadingInformation): string;
begin
  var f: TGameScreenReplayFinder := TGameScreenReplayFinder.Create(nil);
  try
    f.SetInitialFilter(inputLoadingInformation);
    f.ShowScreen;
    Result := f.fResultFile;
  finally
    f.Free;
  end;
end;

end.
