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
  Winapi.Windows, Winapi.Messages,
  System.UITypes, System.SysUtils, System.Classes, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.PngImage, Vcl.StdCtrls,
  Base.Utils, Base.Bitmaps,
  Form.Base, Form.Message,
  Dos.Compression, Dos.Structures,
  Styles.Base, Styles.Factory,
  Level.Base, Level.Hash, Level.Loader,
  Prog.Types, Prog.Base, Prog.App, Prog.Data, Prog.Cache,
  Game, Game.Sound, Game.Rendering,
  GameScreen.Base, GameScreen.Menu, GameScreen.LevelCode, GameScreen.Preview, GameScreen.Postview, GameScreen.Options,
  GameScreen.Finder, GameScreen.Config, GameScreen.Player;

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
    fLoadingLabel: TLabel;
    fCurrentRunningScreen: TBaseDosForm;
    fCurrentParamString: string;
    fInterruptingMessageEnabled: Boolean;
    procedure CreateLoadingLabel;
    procedure HideLoadingLabel;
    procedure LoadingFeedback(const state: string);
    procedure Form_Activate(Sender: TObject);
    procedure LMStart(var Msg: TMessage); message LM_START;
    function ShowScreen<T: TBaseDosForm>: TGameScreenType;
    procedure InitDisplay;
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
  Cursor := crDefault;
  CreateLoadingLabel;
  fCurrentParamString := ParamStr(1);
  OnActivate := Form_Activate;
end;

class procedure TFormMain.SaveError(const s: string);
var
  t: TextFile;
  filename: string;
  isOpen: Boolean;
begin
  filename := ExtractFilePath(ParamStr(0)) + 'Output\Logs\Error.log';
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
  txt := E.message + sLineBreak + 'Exceptionclass: ' + E.Classname;
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

function TFormMain.ShowScreen<T>: TGameScreenType;
var
  F: T;
begin
  F := T.Create(nil);
  try
    CurrentDisplay.CurrentForm := F;
    fCurrentRunningScreen := F;
    Result := F.ShowScreen;
  finally
    CurrentDisplay.CurrentForm := nil;
    fCurrentRunningScreen := nil;
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
  title: TLVLTitle;
  ResultArray: TArray<TStyleCache.TLevelCacheItem>;
begin
  Result := nil;

  // search in levelcache
  if not TRecorder.LoadTitleAndHashFromHeader(aFileName, hash, title) then begin
    MessageDlg(aFilename + ' is an invalid replayfile', mtInformation, [mbOK], 0);
    Exit;
  end;

  if hash <> 0 then
    ResultArray := App.StyleCache.FindLevelsByHash(hash)
  else
    ResultArray := App.StyleCache.FindLevelsByTitle(title);
  if Length(ResultArray) = 0 then begin
    MessageDlg(aFilename + ': cannot find the level (' + Trim(string(title)) + ')', mtInformation, [mbOK], 0);
    Exit;
  end;

  Result := ResultArray[0];
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
  if ext = '' then begin
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
  if not (CurrentDisplay.CurrentForm is TGameMenuScreen) then
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
  dosForm: TBaseDosForm;
begin
  if (index >= Screen.MonitorCount) or (index < 0) then
    index := 0;
  CurrentDisplay.MonitorIndex := index;
  BoundsRect := CurrentDisplay.BoundsRect;
  if Assigned(CurrentDisplay.CurrentForm) and (CurrentDisplay.CurrentForm is TBaseDosForm) then begin
    dosForm := TBaseDosForm(CurrentDisplay.CurrentForm);
    dosForm.BoundsRect := CurrentDisplay.BoundsRect;
  end;
end;

procedure TFormMain.InitDisplay;
begin
  CurrentDisplay.MainForm := Self;
  SwitchToMonitor(App.Config.Monitor);
end;

procedure TFormMain.App_Message(var Msg: TMsg; var Handled: Boolean);
// this message is send to this application, when a second lemmix is started (see Base.Utils.InitializeLemmix and .dpr for the magic)
begin
  if Msg.message = LM_RESTART then begin
    Handled := True;
    if fInterruptingMessageEnabled
    and Assigned(fCurrentRunningScreen)
    and (Assigned(_LemmixMemoryMappedRecord)) then begin
      fCurrentParamString := _LemmixMemoryMappedRecord.GetAsString;
      if fCurrentParamString <> '' then
        fCurrentRunningScreen.CloseScreen(TGameScreenType.Interrupted); // this will be catched in the run method, so we leave this method immediately
    end;
  end;
end;

procedure TFormMain.Run;

    procedure DoRecreateStyle(const aName: string);
    begin
      LoadingFeedback('Loading');
      // we do *not* free the style, because it is pooled
      FreeAndNil(App.GraphicSet);
      FreeAndNil(App.Level);
      Consts.SetStyleName(aName);
      App.Style := TStyleFactory.CreateStyle(False);
      App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
      App.Level := Tlevel.Create;
      App.GraphicSet := TGraphicSet.Create(App.Style);
      App.NewStyleName := Consts.StyleName;
      HideLoadingLabel;
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

      if (startupFile <> '') and (cacheItem = nil) then
        DlgWarning('Could not load ' +  startupFile);

      if not Assigned(cacheItem) then
        startupFile := ''
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
          DlgWarning('Strange mismatch during loading of ' + startupFile + '. Please report the error.');
      end
      // and otherwise just start
      else begin
        App.CurrentLevelInfo := App.Style.LevelSystem.FirstLevel;
        Result := TGameScreenType.Menu;
      end;
    end;

var
  NewScreen: TGameScreenType;

begin
  Application.OnException := App_Exception;
  if Assigned(_LemmixMemoryMappedRecord) then
    _LemmixMemoryMappedRecord^.ApplicationHandle := Application.Handle;
  App := nil;
  TData.Init;
  SoundLibrary.Init;
  TStyleFactory.Init;
  try

    App := TApp.Create;
    App.NewSectionIndex := -1;
    App.NewLevelIndex := -1;

    InitDisplay;
    App.StyleCache := TStyleCache.Create;
    App.StyleCache.Load(LoadingFeedback);

    // when debugging parameters set fCurrentParamString *right down this line*

    NewScreen := CheckLoad(False);
    repeat
      CheckRecreateStyle(App.NewStyleName); // recreate style if options screen changed it
      CheckGotoLevel;

      fInterruptingMessageEnabled := True;
      case NewScreen of
        TGameScreenType.Menu        : NewScreen := ShowScreen<TGameMenuScreen>;
        TGameScreenType.Preview     : NewScreen := ShowScreen<TGamePreviewScreen>;
        TGameScreenType.Play        : NewScreen := ShowScreen<TGameScreenPlayer>;
        TGameScreenType.Postview    : NewScreen := ShowScreen<TGamePostviewScreen>;
        TGameScreenType.LevelCode   : NewScreen := ShowScreen<TGameScreenLevelCode>;
        TGameScreenType.Options     : NewScreen := ShowScreen<TGameScreenOptions>;
        TGameScreenType.Finder      : NewScreen := ShowScreen<TGameScreenFinder>;
        TGameScreenType.Config      : NewScreen := ShowScreen<TGameScreenConfig>;
        TGameScreenType.Interrupted : NewScreen := CheckLoad(True);
        else
          Break;
      end;
    until False;

  finally
    TData.Done;
    SoundLibrary.Done;
    App.Free;
    TStyleFactory.Done;
    Application.OnException := nil;
  end;

  Close;

end;

end.


