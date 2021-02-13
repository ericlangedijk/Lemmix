unit Form.Main;

{$include lem_directives.inc}

{-------------------------------------------------------------------------------
  The main screen logic is handled in the 'Run' Method.
  it's a kind of simple statemachine, which shows the appropriate screens.
  These screens must change the App.NextScreen property, during CloseScreen().
  Furthermore there is code to handle a reload.
-------------------------------------------------------------------------------}

interface

uses
menus,


  Winapi.Windows, Winapi.Messages, Winapi.ShellApi,
  System.UITypes, System.SysUtils, System.Classes, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.PngImage, Vcl.StdCtrls,
  Base.Utils, Base.Types, Base.Strings, Base.Bitmaps,
  Form.Base, Form.Message,
  Dos.Compression, Dos.Structures,
  Styles.Base, Styles.Factory,
  Level.Base, Level.Hash, Level.Loader,
  Prog.Base, Prog.App, Prog.Data, Prog.Cache, Prog.Voice,
  Game, Game.Sound, Game.Rendering,
  GameScreen.Base, GameScreen.Menu, GameScreen.LevelCode, GameScreen.Preview, GameScreen.Postview, GameScreen.Options,
  GameScreen.Finder, GameScreen.ReplayFinder, GameScreen.Player;

type
  TFormMain = class(TBaseForm, IMainForm)
  private
    type
      TStartupFiletype = (
        None,
        Replay,
        LVL,
        DAT,
        Hashcode
      );
  private
    fHourGlassCursor: HCURSOR;
    fLoadingLabel: TLabel;
    fCurrentParamString: string;
    fInterruptingMessageEnabled: Boolean;
    procedure CreateLoadingLabel;
    procedure HideLoadingLabel;
    procedure LoadingFeedback(const state: string);
    procedure Form_Activate(Sender: TObject);
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LMStart(var Msg: TMessage); message LM_START;
    function InternalShowScreen<T: TAppForm>(param: TObject = nil): TGameScreenType;
    procedure InitDisplay;
    procedure CreateHourglassCursor;
    procedure SwitchToNextMonitor; // IMainForm support
    procedure SwitchToMonitor(index: Integer);
    function CheckLoadDAT(const aFilename: string): TStyleCache.TLevelCacheItem;
    function CheckLoadLVL(const aFilename: string): TStyleCache.TLevelCacheItem;
    function CheckLoadHashcode(const aHash: string): TStyleCache.TLevelCacheItem;
    function CheckLoadReplay(const aFilename: string): TStyleCache.TLevelCacheItem;
    function CheckLoadParam(const aFilename: string; out aType: TStartupFiletype): TStyleCache.TLevelCacheItem;
    procedure App_Message(var Msg: TMsg; var Handled: Boolean);
    procedure Run;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class procedure SaveError(const s: string);
    class procedure App_Exception(Sender: TObject; E: Exception);
  end;

var
  FormMain: TFormMain;

implementation

{ TFormMain }

constructor TFormMain.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FormatSettings.ThousandSeparator := '.';
  FormatSettings.DecimalSeparator := ',';
  Cursor := crDefault;
  CreateLoadingLabel;
  fCurrentParamString := ParamStr(1);
  KeyPreview := False;
  OnActivate := Form_Activate;
  OnKeyDown := Form_Keydown;
end;

class procedure TFormMain.SaveError(const s: string);
var
  t: TextFile;
  filename: string;
  isOpen: Boolean;
begin
  filename := Consts.PathToErrorLogs + 'Error.log';
  if not ForceDirectories(filename) then
    Exit;
  AssignFile(t, filename);
  isOpen := False;
  try
    try
      if FileExists(filename) then
        Append(t)
      else begin
        if not ForceDir(filename) then
          Exit;
        Rewrite(t);
      end;
      isOpen := True;
      WriteLn(t, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      WriteLn(t, s);
      WriteLn(t);
      Flush(t);
    finally
      if isOpen then
        CloseFile(t);
    end;
  except
  end;
end;

class procedure TFormMain.App_Exception(Sender: TObject; E: Exception);
// global fatal error exception handler initialized in the run method
var
  txt: string;
begin
  Application.OnIdle := nil;
  txt := E.message + CRLF + 'Exceptionclass: ' + E.Classname;
  SaveError(txt);
  DlgError(txt);
  Application.Terminate;
end;

procedure TFormMain.CreateLoadingLabel;
begin
  fLoadingLabel := TLabel.Create(Self);
  fLoadingLabel.Parent := Self;
  fLoadingLabel.AutoSize := false;
  fLoadingLabel.Font.Color := clLime;
  fLoadingLabel.Font.Size := 16;

  fLoadingLabel.Align := alClient;
  fLoadingLabel.WordWrap := False;
  fLoadingLabel.Layout := tlCenter;
  fLoadingLabel.Alignment := taCenter;
end;

destructor TFormMain.Destroy;
begin
  if PROGAM_CURSOR_HOURGLASS <> 0 then
    DestroyIcon(PROGAM_CURSOR_HOURGLASS);
  inherited;
end;

procedure TFormMain.LoadingFeedback(const state: string);
begin
  if not fLoadingLabel.Visible then
    fLoadingLabel.Show;
  fLoadingLabel.Caption := state;
  fLoadingLabel.Update;
end;

procedure TFormMain.HideLoadingLabel;
begin
  fLoadingLabel.Hide;
end;

procedure TFormMain.LMStart(var Msg: TMessage);
begin
  Msg.Result := 1;
  Run;
end;

procedure TFormMain.Form_Activate(Sender: TObject);
begin
  OnActivate := nil;
  Application.OnMessage := App_Message;
  PostMessage(Handle, LM_START, 0, 0);
end;

procedure TFormMain.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // issue #12 closing screen when error occurred
  if Key = VK_ESCAPE then
    Close;
end;

function TFormMain.InternalShowScreen<T>(param: TObject = nil): TGameScreenType;
var
  F: T;
begin
  F := T.Create(nil);
  try
    Result := F.ShowScreen(param);
  finally
    F.Free;
  end;
end;

function TFormMain.CheckLoadDAT(const aFilename: string): TStyleCache.TLevelCacheItem;
// try to find the first level in a DAT file
var
  LVLCheck: Boolean;
  sectionCount: Integer;
  dosSections: TDosDatSectionList;
  LVL: TLVLRec;
  hash: UInt64;
begin
  Result := nil;
  var cmp: TDosDatDecompressor := TDosDatDecompressor.Create;
  try
    sectionCount := cmp.GetNumberOfSectionsOnly(aFileName, {out} LVLCheck);
    // check these are levels
    if (sectionCount < 1) or not LVLCheck then
      Exit;
    // from here on this should always work
    dosSections := TDosDatSectionList.Create;
    try
      cmp.LoadSectionListFromFile(aFileName, dosSections, False);
      cmp.DecompressSection(dosSections[0].CompressedData, dosSections[0].DecompressedData);
      dosSections[0].DecompressedData.Position := 0;
      TLevelLoader.LoadLVLFromStream(dosSections[0].DecompressedData, LVL);
      hash := TLevelHasher.ShortHash(LVL);
      var itemArray: TArray<TStyleCache.TLevelCacheItem> := App.StyleCache.FindLevelsByHash(hash);
      if Length(itemArray) = 0 then
        Exit;
      Result := itemArray[0];
    finally
      dosSections.Free;
    end;
  finally
    cmp.Free;
  end;
end;

function TFormMain.CheckLoadLVL(const aFilename: string): TStyleCache.TLevelCacheItem;
// try find LVL by hash
var
  LVL: TLVLRec;
begin
  Result := nil;
  var stream: TBytesStream := TBytesStream.Create;
  try
    stream.LoadFromFile(aFilename);
    if stream.Size <> LVL_SIZE then
      Exit(nil);
    stream.Position := 0;
    TLevelLoader.LoadLVLFromStream(stream, LVL);
    var hash: UInt64 := TLevelHasher.ShortHash(LVL);
    var itemArray: TArray<TStyleCache.TLevelCacheItem> := App.StyleCache.FindLevelsByHash(hash);
    if Length(itemArray) = 0 then
      Exit;
    Result := itemArray[0];
  finally
    stream.Free;
  end;
end;

function TFormMain.CheckLoadHashcode(const aHash: string): TStyleCache.TLevelCacheItem;
var
  hash: UInt64;
begin
  Result := nil;
  if Length(aHash) <> 16 then
    Exit;
  if not TryStrToUInt64('$' + aHash, hash) then
    Exit;
  var itemArray: TArray<TStyleCache.TLevelCacheItem> := App.StyleCache.FindLevelsByHash(hash);
  if Length(itemArray) = 0 then
    Exit;
  Result := itemArray[0];
end;

function TFormMain.CheckLoadReplay(const aFilename: string): TStyleCache.TLevelCacheItem;
// try load replay
var
  hash: UInt64;
  version: Byte;
  title: TLVLTitle;
begin
  // search in levelcache
  if not TRecorder.LoadTitleHashVersionFromHeader(aFileName, hash, version, title) then begin
    DlgWarning(FormatSimple(gt.SErrorInvalidReplayFile_s, [aFilename]));
    Exit(nil);
  end;

  if App.StyleCache.SelectBestMatchingLevel(title, hash, Result) = 0 then begin
    DlgWarning(FormatSimple(gt.SErrorCannotFindLevelFromReplayFile_ss, [Trim(string(title)), aFilename]));
    Exit(nil);
  end;
end;

function TFormMain.CheckLoadParam(const aFilename: string; out aType: TStartupFiletype): TStyleCache.TLevelCacheItem;
var
  ext: string;
begin
  Result := nil;
  aType := TStartupFiletype.None;

  if aFilename.Trim.IsEmpty then
    Exit;

  ext := ExtractFileExt(aFilename).ToUpper;
  if ext.IsEmpty then begin
    Result := CheckLoadHashcode(aFilename);
    if Assigned(Result) then
      aType := TStartupFiletype.Hashcode;
    Exit;
  end;

  if not FileExists(aFilename) then begin
    MessageDlg(aFilename + ' does not exist', mtInformation, [mbOK], 0);
    Exit;
  end;

  if ext = '.LRB' then begin
    Result := CheckLoadReplay(aFilename);
    if Assigned(Result) then
      aType := TStartupFiletype.Replay;
  end
  else if ext = '.LVL' then begin
    Result := CheckLoadLVL(aFilename);
    if Assigned(Result) then
      aType := TStartupFiletype.LVL;
  end
  else if ext = '.DAT' then begin
    Result := CheckLoadDAT(aFilename);
    if Assigned(Result) then
      aType := TStartupFiletype.DAT;
  end;
end;

procedure TFormMain.SwitchToNextMonitor;
// only triggered from the menuscreen by the IMainForm interface
begin
  if Screen.MonitorCount <= 1 then
    Exit;
  if not (App.CurrentForm is TGameMenuScreen) then
    Exit;
  var current: Integer := CurrentDisplay.MonitorIndex;
  Inc(current);
  if current >= Screen.MonitorCount then
    current := 0;
  SwitchToMonitor(current);
end;

procedure TFormMain.SwitchToMonitor(index: Integer);
// change boundsrect to monitor
var
  appForm: TAppForm;
begin
  if (index >= Screen.MonitorCount) or (index < 0) then
    index := 0;
  CurrentDisplay.MonitorIndex := index;
  BoundsRect := CurrentDisplay.BoundsRect;
  if Assigned(App.CurrentForm) and (App.CurrentForm is TAppForm) then begin
    appForm := TAppForm(App.CurrentForm);
    appForm.BoundsRect := CurrentDisplay.BoundsRect;
  end;
end;

procedure TFormMain.InitDisplay;
begin
  App.MainForm := Self;
  SwitchToMonitor(App.Config.Monitor);
end;

procedure TFormMain.CreateHourglassCursor;
var
  bmpColor, bmpMask: TBitmap;
  info: TIconInfo;
begin
  // default game cursor
  bmpColor := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHourglass);
  bmpMask  := TData.CreateCursorBitmap(Consts.StyleName, Consts.FilenameCursorHourglassMask);

  info.fIcon := false;
  info.xHotspot := bmpColor.Width div 2;
  info.yHotspot := bmpColor.Height div 2;
  info.hbmMask := bmpMask.Handle;
  info.hbmColor := bmpColor.Handle;

  fHourGlassCursor := CreateIconIndirect(info);
  Screen.Cursors[PROGAM_CURSOR_HOURGLASS] := fHourGlassCursor;

  bmpColor.Free;
  bmpMask.Free;
end;

procedure TFormMain.App_Message(var Msg: TMsg; var Handled: Boolean);
// this LM_RESTART message is send to this application, when a second lemmix is started.
// see Base.Utils.InitializeLemmix and Lemmix.dpr for the magic
begin
  if Msg.message = LM_RESTART then begin
    Handled := True;
    if fInterruptingMessageEnabled
    and Assigned(App.CurrentForm)
    and (App.CurrentForm is TAppForm)
    and (Assigned(_LemmixMemoryMappedRecord)) then begin
      fCurrentParamString := _LemmixMemoryMappedRecord.GetAsString;
      if not fCurrentParamString.IsEmpty then
        App.Interrupt; // close top-down all screens
    end;
  end;
end;

procedure TFormMain.Run;

    procedure SpeakStyle(info: Consts.TStyleInformation);
    var
      s: string;
    begin
      if not (TVoiceOption.CurrentStyle in App.Config.VoiceOptions) then
        Exit;
      s := info.Description;
      if s.IsEmpty then
        s := info.Name;
      // honoring correct ccexplore pronouncation :-)
      if s.ToLower.Contains('ccexplore') then
        s := s.Replace('ccexplore', 'c c explore');
      Speak(s, True);
    end;

    procedure DoRecreateStyle(const aName: string);
    begin
      TempCursor.Activate;
      // we do *not* free the current style, because it is pooled
      FreeAndNil(App.GraphicSet);
      FreeAndNil(App.Level);
      Consts.SetStyleName(aName);
      App.Style := TStyleFactory.CreateStyle(False);
      App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
      App.Level := Tlevel.Create;
      App.GraphicSet := TGraphicSet.Create(App.Style);
      App.NewStyleName := Consts.StyleName;
      HideLoadingLabel;
      SpeakStyle(App.Style.StyleInformation);
    end;

    procedure CheckRecreateStyle(const newName: string);
    begin
      if not Assigned(App.Style) or not SameText(App.Style.Name, newName) then
        DoRecreateStyle(newName);
    end;

    procedure CheckGotoLevel;
    begin
      if (App.NewSectionIndex >= 0) and (App.NewLevelIndex >= 0) then begin
        App.CurrentLevelInfo := App.Style.LevelSystem.FindLevelByIndex(App.NewSectionIndex, App.NewLevelIndex);
      end;
      App.NewSectionIndex := -1;
      App.NewLevelIndex := -1;
    end;

    function CheckLoad(isInterrupted: Boolean): TGameScreenType;
    var
      startupFile: string;
      startupFileType: TStartupFiletype;
      cacheItem: TStyleCache.TLevelCacheItem;
    begin
      Result := TGameScreenType.Menu;
      fInterruptingMessageEnabled := False; // this will be reset just before showing the next screen
      startupFileType := TStartupFiletype.None;

      // check opening with LVL or LRB
      startupFile := fCurrentParamString;
      cacheItem := CheckLoadParam(startupFile, {out} startupFiletype);

      if (not startupFile.IsEmpty) and (cacheItem = nil) then
        DlgWarning(FormatSimple(gt.SErrorCannotStartWithReplayFile_s, [startupfile]));

      if not Assigned(cacheItem) then
        startupFile := string.Empty
      else
        Consts.SetStyleName(cacheItem.StyleName);

      // now we know which style to load
      if not isInterrupted then
        DoRecreateStyle(Consts.StyleName)
      else
        CheckRecreateStyle(Consts.StyleName);

      // and now load the levelinformation from the current levelsystem
      if Assigned(cacheItem) and (startupFileType <> TStartupFiletype.None) then begin
        if startupFileType = TStartupFiletype.Replay then begin
          App.CurrentLevelInfo := App.Style.LevelSystem.FindLevelByIndex(cacheItem.SectionIndex, cacheItem.LevelIndex);
          App.ReplayFileName := startupFile;
          App.NewStyleName := Consts.StyleName;
          Result := TGameScreenType.Preview;
        end
        else if startupFileType in [TStartupFiletype.LVL, TStartupFiletype.DAT, TStartupFileType.Hashcode] then begin
          App.CurrentLevelInfo := App.Style.LevelSystem.FindLevelByIndex(cacheItem.SectionIndex, cacheItem.LevelIndex);
          App.NewStyleName := Consts.StyleName;
          Result := TGameScreenType.Preview;
        end;
        // validation
        if not Assigned (App.CurrentLevelInfo) or not cacheItem.MatchesWithLevelLoadingInformation(App.CurrentLevelInfo) then
          DlgWarning(FormatSimple(gt.SErrorUnexpectedMismatchWhenLoading_s, [startupFile]));
      end
      // and otherwise just start
      else begin
        App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
        Result := TGameScreenType.Menu;
      end;
    end;

    procedure CheckLoadLanguage;
    begin
      gt.Save('Default.config');
      if App.Config.LanguageIsDefault then
        Exit;
      // pre-check
      var filename: string := Consts.PathToLanguage + App.Config.Language + '.config'; // do not localize
      if FileExists(filename) then
        gt.Load(App.Config.Language + '.config'); // do not localize
    end;

var
  NewScreen: TGameScreenType;
begin
  Application.OnException := App_Exception;
  if Assigned(_LemmixMemoryMappedRecord) then
    _LemmixMemoryMappedRecord^.ApplicationHandle := Application.Handle;

  App := nil;

  CreateHourglassCursor;
  SoundLibrary.Init;
  TStyleFactory.Init;
  try
    App := TApp.Create;
    CheckLoadLanguage;
    App.NewSectionIndex := -1;
    App.NewLevelIndex := -1;
    InitDisplay;
    App.StyleCache := TStyleCache.Create;
    App.StyleCache.Load(LoadingFeedback);

    {$ifdef debug}
    // when debugging parameters set fCurrentParamString *right down this line*
    // fCurrentParamString := 'myreplay.lrb';
    {$endif}

    NewScreen := CheckLoad(False);
    repeat
      CheckRecreateStyle(App.NewStyleName); // recreate style if options screen -> menu screen changed it
      CheckGotoLevel;

      fInterruptingMessageEnabled := True;
      case NewScreen of
        TGameScreenType.Menu          : NewScreen := InternalShowScreen<TGameMenuScreen>;
        TGameScreenType.Preview       : NewScreen := InternalShowScreen<TGamePreviewScreen>;
        TGameScreenType.Play          : NewScreen := InternalShowScreen<TGameScreenPlayer>;
        TGameScreenType.Postview      : NewScreen := InternalShowScreen<TGamePostviewScreen>;
        TGameScreenType.LevelCode     : NewScreen := InternalShowScreen<TGameScreenLevelCode>;
        TGameScreenType.Interrupted   : NewScreen := CheckLoad(True);
        else
          Break; // unknown, exitprogram
      end;
    until False;

  finally
    SoundLibrary.Done;
    App.Free;
    TStyleFactory.Done;
    Application.OnException := nil;
  end;

  Close;

  // triggered if config screen -> menu screen changed paths. we need to restart the exe
  if NewScreen = TGameScreenType.Restart then
    ShellExecute(0, 'open', PChar(Application.ExeName), nil, nil, SW_SHOWNORMAL);
end;

end.


