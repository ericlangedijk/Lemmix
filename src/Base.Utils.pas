unit Base.Utils;

{$include lem_directives.inc}

interface

// Base.Utils must be without any lemmix program specific references.
// Some basic stuff here as well as the important initialization of the executable.

uses
  {$ifdef debug}
  System.Rtti, System.TypInfo, // debug mode logging using TValue
  {$endif}
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.IniFiles, System.Math, System.Contnrs, System.Generics.Collections, System.Character,
  System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  GR32;

const
  // cursors
  GAME_CURSOR_DEFAULT     = 1;
  GAME_CURSOR_LEMMING     = 2;
  GAME_CURSOR_DRAG        = 3;
  PROGAM_CURSOR_HOURGLASS = 4;

  // chars
  CR = #13;
  LF = #10;
  CRLF = sLineBreak;
  tab = Chr(9);
  bulletright = Chr($2023);
  bullet = Chr($25CF);

  // bitnumbers
  Bit0  = 1;         // 1
  Bit1  = 1 shl 1;   // 2
  Bit2  = 1 shl 2;   // 4
  Bit3  = 1 shl 3;   // 8
  Bit4  = 1 shl 4;   // 16
  Bit5  = 1 shl 5;   // 32
  Bit6  = 1 shl 6;   // 64
  Bit7  = 1 shl 7;   // 128
  Bit8  = 1 shl 8;   // 256
  Bit9  = 1 shl 9;   // 512
  Bit10 = 1 shl 10;  // 1024
  Bit11 = 1 shl 11;  // 2048
  Bit12 = 1 shl 12;  // 4096
  Bit13 = 1 shl 13;  // 8192
  Bit14 = 1 shl 14;  // 16384
  Bit15 = 1 shl 15;  // 32768

function InitializeLemmix: Boolean;
procedure DoThrow(const msg: string; const proc: string = string.Empty);

type
//  TStringFeedbackProc = reference to procedure(const s: string);
  TFeedbackProc = reference to procedure (const msg: string);

type
  TObjectHelper = class helper for TObject
  public
    class procedure Throw(const msg: string; const method: string = string.Empty);
  end;

  TIdle = class sealed
  private
    fActive: Boolean;
    fIdleEvent: TIdleEvent;
    fActivateEvent: TNotifyEvent;
    fDeactivateEvent: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(aIdleEvent: TIdleEvent; aActivateEvent: TNotifyEvent = nil; aDeactivateEvent: TNotifyEvent = nil);
    destructor Destroy; override;
    property Active: Boolean read fActive write SetActive;
  end;

  ITempCursor = interface(IInterface)
    ['{495ADE0F-EFBE-4A0E-BF37-F1ACCACCE03D}']
  end;

  TempCursor = class(TInterfacedObject, ITempCursor)
  strict private
    fCursor: TCursor;
  public
    constructor Create(const aCursor: TCursor);
    destructor Destroy; override;
    class function Activate(const aCursor: TCursor = PROGAM_CURSOR_HOURGLASS): ITempCursor;
  end;

  // dedicated simple string helper for creating a tab seperated string for help screen
  THelpString = record
  private
    fText: string;
  public
    class operator Initialize(out h: THelpString);
    class operator Implicit(const s: string): THelpString; inline;
    class operator Implicit(const h: THelpString): string; inline;
    procedure Add(const key, value: string); overload; inline;
    procedure Add(const key: Word; const value: string); overload; inline;
    procedure Add(const key: Word; shift: TShiftState; const value: string); overload; inline;
    procedure Split(keys, values: TStringList); overload;
    procedure Split(out keys: TArray<string>; out values: TArray<string>); overload;
    property Text: string read fText;
  end;


// file stuff
function ForceDir(const aFileName: string): Boolean;
function GetFileSize(const aFilename: string): Int64;
function SelectFileDlg(const ext: string): string;

// string stuff
function FormatSimple(const s: string; const args: array of string): string;
function LeadZeroStr(const i, zeros: Integer): string; inline;
function ThousandStr(Int: Integer): string; overload; inline;
function ThousandStr(Int: Cardinal): string; overload; inline;
function CutLeft(const S: string; aLen: integer): string; inline;
function CutRight(const S: string; aLen: integer): string; inline;
function ReplaceFileExt(const aFileName, aNewExt: string): string;
function StripInvalidFileChars(const S: string; removeDots: Boolean = True; removeDoubleSpaces: Boolean = True; trimAccess: Boolean = True): string;
function CheckReformStringForDialog(const s: string): string;
function SpaceCamelCase(const s: string): string;
function StringFromFile(const aFilename: string): string;

// conversion string stuff
function RectStr(const R: TRect): string;
function PointStr(const P: TPoint): string;
function BytesToHex(const bytes: TBytes): string;

// rectangle stuff
function ZeroTopLeftRect(const r: TRect): TRect; inline;

// int stuff
procedure Restrict(var i: Integer; aMin, aMax: Integer); overload; inline;
procedure Restrict(var s: Single; const aMin, aMax: Single); overload; inline;
function Percentage(Max, N: integer): integer;

// win stuff
function GetLocalComputerName: string;

// low level system messagebox
procedure Dlg(const s: string);

// shortcut string
function KeyStr(key: Word; shift: TShiftState = []): string;
function MouseStr(button: TMouseButton; shift: TShiftState = []): string;

// debug
{$ifdef debug}
procedure Log(const v: TValue); overload;
procedure Log(const v1, v2: TValue); overload;
procedure Log(const v1, v2, v3: TValue); overload;
procedure Log(const v1, v2, v3, v4: TValue); overload;
procedure Log(const v1, v2, v3, v4, v5: TValue); overload;
procedure Log(const v1, v2, v3, v4, v5, v6: TValue); overload;
procedure ClearLog;
{$endif debug}

// version
function GetAppVersionString(out major, minor, release, build: Integer): string;

// timing stuff
function QueryTimer: Int64; overload; inline;
function MSBetween(const T1, T2: Int64): Int64; inline;
function MSBetweenD(const T1, T2: Int64): Double; inline;

// simple timer mechanism
type
  TTicker = record
  strict private
    fLastTick     : Int64; // ticks
    fCurrentTick  : Int64; // ticks
    fInterval     : Cardinal;  // MS
  public
    procedure Reset(const aLastTick: Int64); inline;
    function Check(const aCurrentTick: Int64): Boolean; inline;
    property LastTick: Int64 read fLastTick;
    property CurrentTick: Int64 read fCurrentTick;
    property Interval: Cardinal read fInterval write fInterval;
  end;

// monitor and dpi stuff
function Scale(const i: Integer): Integer; overload; inline;
function Scale(const r: TRect): TRect; overload; inline;

type
  TIntHelper = record helper for Integer
  public
    function ToString: string; inline;
    function ToThousandString: string; inline;
    function Scale: Integer; inline;
  end;

  // just useless stub interface to prevent circular references
  IForm = interface
    ['{24535447-B742-4EB2-B688-825A1AD69349}']
  end;

  IMainForm = interface
    ['{24535447-B742-4EB2-B688-825A1AD69349}']
    procedure SwitchToNextMonitor;
  end;

  TDisplayInfo = record
  private
    fGeneralPurposeBitmap: TBitmap;
    procedure FreeBitmap;
  private
    fDpi: Integer; // the dpi of the monitor
    fMonitorIndex: Integer; // the current monitor index
    fBoundsRect: TRect; // the boundsrect of the current display
    fDpiScale: Single; // scale relative = Dpi/96
    procedure SetMonitorIndex(aValue: Integer);
  public
    function CalcTextExtent(aFont: TFont; const s: string): TSize; overload;
    function CalcTextExtent(const aFontname: string; aFontHeight: Integer; const s: string): TSize; overload;
    property MonitorIndex: Integer read fMonitorIndex write SetMonitorIndex;
    property Dpi: Integer read fDpi;
    property DpiScale: Single read fDpiScale;
    property BoundsRect: TRect read fBoundsRect;
  end;

type
  TFastObjectList<T: class> = class;

  // fastest possible 'for in' loop support
  TEnumeratorForFastList<T: class> = record
  private
    fIndex: Integer;
    fList: TFastObjectList<T>;
  public
    function MoveNext: Boolean; inline;
    function GetCurrent: T; inline;
    property Current: T read GetCurrent;
  end;

  // fast typed object list
  TFastObjectList<T: class> = class(TObjectList)
  strict private
  private
  public
    function GetEnumerator: TEnumeratorForFastList<T>; inline;
    function ValidIndex(ix: Integer): Boolean; inline;
    procedure CheckIndex(aIndex: Integer);
    function GetItem(aIndex: Integer): T; inline;
    function First: T; inline;
    function Last: T; inline;
    function FirstOrDefault: T; inline;
    function LastOrDefault: T; inline;
    function HasItems: Boolean; inline;
    function IsEmpty: Boolean; inline;
    function ToArray: TArray<T>;
    property Items[aIndex: Integer]: T read GetItem; default;
  end;

  // a very basic helper
  TStringArray = TArray<string>;
  TStringArrayHelper = record helper for TStringArray
  public
    function IndexOf(const s: string; caseSensitive: Boolean = True): Integer;
    function Contains(const s: string; caseSensitive: Boolean = True): Boolean; inline;
    function Length: Integer; inline;
    procedure Sort;
  end;

  TBitmaps = class(TFastObjectList<TBitmap32>);

  TGenericStringFeedbackProc = reference to procedure(const s: string);

// lemmix messages
const
  LM_START   = WM_USER + 1;
  LM_RESTART = WM_USER + 2;

type
  PLemmixMemoryMappedRecord = ^TLemmixMemoryMappedRecord;
  TLemmixMemoryMappedRecord = packed record
  public
    ApplicationHandle: NativeUInt;
    ParamChars: array[1..256] of Char;
    procedure Clear;
    function GetAsString: string;
    procedure SetAsString(const s: string);
  end;

// lowlevel program globals
var
  _UniqueName: string = 'Lemmix-1965-05-21';
  _UniqueMemoryMappedFileName: string = 'Lemmix-1965-05-21-memfile';
  _Mutex: THandle; // lemmix can only have 1 instance
  _Freq: Int64; // for timer
  _MemoryMappedFileHandle: THandle; // for restarting lemmix with new parameter
  _LemmixMemoryMappedRecord: PLemmixMemoryMappedRecord; // the pointer
  CurrentDisplay: TDisplayInfo; // info on monitor and dpi

implementation

var
  GlobalDlg: TOpenDialog;

procedure DoThrow(const msg: string; const proc: string = string.Empty);
// generic invalid operation exception for procedues
begin
  var txt: string := msg;
  if proc.IsEmpty then
    txt := txt + CRLF + 'Proc: ' + proc;
  raise EInvalidOperation.Create(txt) at ReturnAddress;
end;

class procedure TObjectHelper.Throw(const msg: string; const method: string = string.Empty);
// generic object helper for invalid operation exception from method of whichever object
var
  classString: string;
begin
  classString := ClassName;
  if classString.StartsWith('T') then
    classString := Copy(classString, 2, Length(ClassString));
  var txt: string := msg + CRLF + CRLF + 'Error from: ' + classString + CRLF + 'Unit: ' + Unitname;
  if method.isEmpty then
    txt := txt + CRLF + 'Method: ' + method;
  raise EInvalidOperation.Create(txt) at ReturnAddress;
end;

{ TIdle }

constructor TIdle.Create(aIdleEvent: TIdleEvent; aActivateEvent: TNotifyEvent = nil; aDeactivateEvent: TNotifyEvent = nil);
begin
  inherited Create;
  fIdleEvent := aIdleEvent;
  fActivateEvent := aActivateEvent;
  fDeactivateEvent := aDeactivateEvent;
end;

destructor TIdle.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TIdle.SetActive(const Value: Boolean);
begin
  if fActive = Value then
    Exit;
  fActive := Value;
  case fActive of
    False:
      begin
        Application.OnIdle := nil;
        Application.OnActivate := nil;
        Application.OnDeactivate := nil;
      end;
    True:
      begin
        Application.OnIdle := fIdleEvent;
        Application.OnActivate := fActivateEvent;
        Application.OnDeactivate := fDeactivateEvent;
      end;
  end;
end;

{ TempCursor }

constructor TempCursor.Create(const aCursor: TCursor);
begin
  inherited Create;
  fCursor := Screen.Cursor;
  Screen.Cursor := aCursor;
end;

destructor TempCursor.Destroy;
begin
  if Assigned(Screen) then
    Screen.Cursor := fCursor;
  inherited Destroy;
end;

class function TempCursor.Activate(const aCursor: TCursor = PROGAM_CURSOR_HOURGLASS): ITempCursor;
begin
  Result := TempCursor.Create(aCursor);
end;

function ForceDir(const aFileName: string): Boolean;
// force path for filename
begin
  var path: string := ExtractFilePath(aFileName);
  Result := ForceDirectories(path);
end;

function GetFileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;
  if not GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    Exit;
  result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

function SelectFileDlg(const ext: string): string;
begin
  if not Assigned(GlobalDlg) then begin
    GlobalDlg := TOpenDialog.Create(Application);
  end;

  GlobalDlg.FileName := ext;
  if GlobalDlg.Execute
  then Result := GlobalDlg.FileName
  else Result := string.Empty;
end;

function FormatSimple(const s: string; const args: array of string): string;
begin
  Result := s;
  for var i := 0 to Length(args) - 1 do
    Result := Result.Replace('%s', args[i], []);
end;

function LeadZeroStr(const i, zeros: Integer): string; inline;
begin
  Result := i.ToString.PadLeft(zeros, '0');
end;

function ThousandStr(Int: Integer): string;
begin
  Result := FloatToStrF(Int / 1, ffNumber, 15, 0);
end;

function ThousandStr(Int: Cardinal): string;
begin
  Result := FloatToStrF(Int / 1, ffNumber, 15, 0);
end;

function CutLeft(const S: string; aLen: integer): string;
begin
  Result := Copy(S, aLen + 1, Length(S));
end;

function CutRight(const S: string; aLen: integer): string;
begin
  Result := Copy(S, 1, Length(S) - aLen);
end;

function ZeroTopLeftRect(const r: TRect): TRect;
begin
  Result := r;
  Result.Offset(-Result.Left, -Result.Top);
end;

procedure Restrict(var i: Integer; aMin, aMax: Integer);
begin
  i := EnsureRange(i, aMin, aMax);
end;

procedure Restrict(var s: Single; const aMin, aMax: Single); overload;
begin
  s := EnsureRange(s, aMin, aMax);
end;

function Percentage(Max, N: integer): integer;
begin
  if Max = 0 then
    Result := 0
  else
    Result := Trunc((N/Max) * 100);
end;

function GetLocalComputerName: string;
var
  c1: dword;
  arrCh : array [0..MAX_PATH] of char;
begin
  c1 := MAX_PATH;
  GetComputerName(arrCh, c1);
  if c1 > 0 then
    result := arrCh
  else
    result := string.Empty;
end;

function StringFromFile(const aFilename: string): string;
begin
  var list: TStringList := TStringList.Create;
  try
    list.LoadFromFile(aFilename);
    Result := list.Text;
  finally
    list.Free;
  end;
end;

function ReplaceFileExt(const aFileName, aNewExt: string): string;
var
  Ext, NewExt: string;
begin
  Ext := ExtractFileExt(aFileName);
  NewExt := aNewExt;
  if not NewExt.IsEmpty and not NewExt.StartsWith('.') then
    NewExt := '.' + NewExt;
  if Ext <> string.Empty then
    Result := Copy(aFileName, 1, Length(aFileName) - Length(Ext)) + NewExt
  else
    Result := aFilename + NewExt;
end;

function StripInvalidFileChars(const S: string; removeDots: Boolean = True; removeDoubleSpaces: Boolean = True; trimAccess: Boolean = True): string;
begin
  Result := string.Empty;
  for var C: Char in S do
    if TPath.IsValidFileNameChar(C) then
      Result := Result + C;

  if removeDoubleSpaces then
    while Pos('  ', Result) > 0 do
      Result := Result.Replace('  ', ' ');

  if removeDots then
    Result := Result.Replace('.', string.Empty);

  if trimAccess then
    Result := Result.Trim;
end;

function CheckReformStringForDialog(const s: string): string;
begin
  if not s.Contains(':\') then
    Result := s
  else
    Result := s.Replace('\', ' ' + bulletright + ' ');
end;

function SpaceCamelCase(const s: string): string;
var
  first, upper, prevupper: Boolean;
begin
  Result := string.Empty;
  first := True;
  prevupper := False;
  for var c: Char in s do begin
    upper := c.IsUpper;
    if not first and upper and not prevupper then
      Result := Result + ' ' + c
    else
      Result := Result + c;
    prevupper := upper;
    first := False;
  end;
end;

function RectStr(const R: TRect): string;
begin
  Result := '(' + R.Left.ToString + ',' + R.Top.ToString + ',' +R.Right.ToString + ',' +R.Bottom.ToString + ')';
end;

function PointStr(const P: TPoint): string;
begin
  Result := '(' + P.X.ToString + ',' + P.Y.ToString + ')';
end;

function BytesToHex(const bytes: TBytes): string;
begin
  SetLength(Result, Length(Bytes) * 2);
  var ix: Integer := 1;
  for var i := 0 to Length(Bytes) - 1 do begin
    var h: string := IntToHex(bytes[i], 2);
    Move(h[1], Result[ix], SizeOf(Char) * 2);
    Inc(ix, 2);
  end;
end;

procedure Dlg(const s: string);
begin
  MessageBox(0, Pchar(s), 'Lemmix', 0); // do not use when active: this messes with focus controls.
end;

function KeyStr(key: Word; shift: TShiftState = []): string;
begin
  if key = VK_PAUSE then
    Result := 'Pause/Break'
  else
    Result := ShortCutToText(ShortCut(key, shift))
end;

function MouseStr(button: TMouseButton; shift: TShiftState = []): string;
const
  ar: array[TMouseButton] of string = ('Left Click', 'Right Click', 'Middle Click'); // todo: localize
begin
  Result := string.Empty;
  if ssShift in shift then Result := Result + 'Shift';//MenuKeyCaps[mkcShift];
  if ssCtrl in shift then begin if not Result.IsEmpty then Result := Result + '+'; Result := Result + 'Ctrl'; end; //MenuKeyCaps[mkcCtrl];
  if ssAlt in shift then begin if not Result.IsEmpty then Result := Result + '+'; Result := Result + 'Alt'; end; //MenuKeyCaps[mkcAlt];
  if not Result.IsEmpty then Result := Result + '+';
  Result := Result + ar[button];
end;

{$ifdef debug}
var
  LogCalls: Integer;

procedure Log(const v: TValue);
var
  txtFile: TextFile;
  path: string;
begin
  path := ExtractFilePath(ParamStr(0));
  Assign(txtFile, path + 'log.txt');
  if not FileExists(path + 'log.txt') or (LogCalls = 0) then
    Rewrite(txtFile)
  else
    Append(txtFile);
  WriteLn(txtFile, v.ToString);
  CloseFile(txtFile);
  Inc(LogCalls);
end;

procedure Log(const v1, v2: TValue); overload;
begin
  Log(v1.ToString + ', ' + v2.ToString);
end;

procedure Log(const v1, v2, v3: TValue); overload;
begin
  Log(v1.ToString + ', ' + v2.ToString + ', ' + v3.ToString);
end;

procedure Log(const v1, v2, v3, v4: TValue); overload;
begin
  Log(v1.ToString + ', ' + v2.ToString + ', ' + v3.ToString + ', ' + v4.ToString);
end;

procedure Log(const v1, v2, v3, v4, v5: TValue); overload;
begin
  Log(v1.ToString + ', ' + v2.ToString + ', ' + v3.ToString + ', ' + v4.ToString + ', ' + v5.ToString);
end;

procedure Log(const v1, v2, v3, v4, v5, v6: TValue); overload;
begin
  Log(v1.ToString + ', ' + v2.ToString + ', ' + v3.ToString + ', ' + v4.ToString + ', ' + v5.ToString + ', ' + v6.ToString);
end;

procedure ClearLog;
begin
  DeleteFile('log.txt');
end;
{$endif debug}

function GetAppVersionString(out major, minor, release, build: Integer): string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Result := '?';

  major := 0;
  minor := 0;
  release := 0;
  build := 0;

  Exe := ParamStr(0);

  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    Exit;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    Exit;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    Exit;

  major := LongRec(FixedPtr.dwFileVersionMS).Hi;
  minor := LongRec(FixedPtr.dwFileVersionMS).Lo;
  release := LongRec(FixedPtr.dwFileVersionLS).Hi;
  build := LongRec(FixedPtr.dwFileVersionLS).Lo;

  Result := Format('%d.%d.%d', [major, minor, release]); // we do not use the build number
end;

function QueryTimer: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function MSBetween(const T1, T2: Int64): Int64;
// returns the difference in milliseconds between 2 values, calculated with QueryTimer
begin
  if T2 > T1 then
    Result := Trunc(1000 * ((T2 - T1) / _Freq))
  else
    Result := Trunc(1000 * ((T1 - T2) / _Freq))
end;

function MSBetweenD(const T1, T2: Int64): Double;
// returns the difference in milliseconds between 2 values, calculated with QueryTimer
begin
  if T2 > T1 then
    Result := 1000 * ((T2 - T1) / _Freq)
  else
    Result := 1000 * ((T1 - T2) / _Freq)
end;

{ TTicker }

procedure TTicker.Reset(const aLastTick: Int64);
begin
  fLastTick := aLastTick;
  fCurrentTick := aLastTick;
end;

function TTicker.Check(const aCurrentTick: Int64): Boolean;
begin
  Result := (aCurrentTick > fLastTick) and (Trunc(1000 * ((aCurrentTick - fLastTick) / _Freq)) >= fInterval);
end;

class operator THelpString.Initialize(out h: THelpString);
begin
  h.fText := string.Empty;
end;

class operator THelpString.Implicit(const s: string): THelpString;
begin
  Result.fText := s;
end;

class operator THelpString.Implicit(const h: THelpString): string;
begin
  Result := h.fText;
end;

procedure THelpString.Add(const key, value: string);
begin
  fText := fText + key + tab + value + CRLF;
end;

procedure THelpString.Add(const key: Word; const value: string);
begin
  Add(KeyStr(key), value);
end;

procedure THelpString.Add(const key: Word; shift: TShiftState; const value: string);
begin
  Add(KeyStr(key, shift), value);
end;

procedure THelpString.Split(out keys: TArray<string>; out values: TArray<string>);
var
  lines: TArray<string>;
  s: string;
  KeyValue: TArray<string>;
begin
  lines := fText.Split([CRLF]);
  SetLength(keys, Length(lines));
  SetLength(values, Length(lines));
  for var i := 0 to Length(lines) - 1 do begin
    s := lines[i];
    KeyValue := s.Split([tab]);
    if Length(KeyValue) = 2 then begin
      keys[i] := KeyValue[0];
      values[i] := KeyValue[1];
    end
    else begin
      keys[i] := string.Empty;
      values[i] := s;
    end;
  end;
end;

procedure THelpString.Split(keys, values: TStringList);
var
  lines: TArray<string>;
  s: string;
  KeyValue: TArray<string>;
begin
  keys.Clear;
  values.Clear;
  lines := fText.Split([CRLF]);
  for var i := 0 to Length(lines) - 1 do begin
    s := lines[i];
    KeyValue := s.Split([tab]);
    if Length(KeyValue) = 2 then begin
      keys.add(KeyValue[0]);
      values.Add(KeyValue[1]);
    end
    else begin
      keys.Add(string.Empty);
      values.Add(s);
    end;
  end;
end;

{ TDisplayInfo }

procedure TDisplayInfo.FreeBitmap;
begin
  if Assigned(fGeneralPurposeBitmap) then
    FreeAndNil(fGeneralPurposeBitmap);
end;

procedure TDisplayInfo.SetMonitorIndex(aValue: Integer);
var
  monitor: TMonitor;
begin
  if (aValue < 0) or (aValue >= Screen.MonitorCount) then
    aValue := 0;
  fMonitorIndex := aValue;
  monitor := Screen.Monitors[fMonitorIndex];
  fDpi := monitor.PixelsPerInch;
  fDpiScale := fDpi / 96;  // 96 or 72 ??? vcl.forms
  fBoundsRect := monitor.BoundsRect;
end;

function TDisplayInfo.CalcTextExtent(aFont: TFont; const s: string): TSize;
begin
  if not Assigned(fGeneralPurposeBitmap) then begin
    fGeneralPurposeBitmap := TBitmap.Create;
    fGeneralPurposeBitmap.SetSize(16, 16);
  end;
  fGeneralPurposeBitmap.Canvas.Font := aFont;
  Result := fGeneralPurposeBitmap.Canvas.TextExtent(s);
end;

function TDisplayInfo.CalcTextExtent(const aFontname: string; aFontHeight: Integer; const s: string): TSize;
begin
  if not Assigned(fGeneralPurposeBitmap) then begin
    fGeneralPurposeBitmap := TBitmap.Create;
    fGeneralPurposeBitmap.SetSize(16, 16);
  end;
  fGeneralPurposeBitmap.Canvas.Font.Name := aFontName;
  fGeneralPurposeBitmap.Canvas.Font.Height := aFontHeight;
  Result := fGeneralPurposeBitmap.Canvas.TextExtent(s);
end;

{ TEnumeratorForFastList<T> }

function TEnumeratorForFastList<T>.GetCurrent: T;
begin
  Result := fList.List[fIndex];
end;

function TEnumeratorForFastList<T>.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < fList.Count;
end;

{ TFastObjectList<T> }

function TFastObjectList<T>.ValidIndex(ix: Integer): Boolean;
begin
  Result := (ix >= 0) and (ix < Count);
end;

procedure TFastObjectList<T>.CheckIndex(aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex >= Count) then begin
    Throw('TFastObjectList (' + T.ClassName + ') index error (' + aIndex.ToString + ')');
  end;
end;

function TFastObjectList<T>.GetItem(aIndex: Integer): T;
begin
  {$ifdef paranoid} CheckIndex(aIndex); {$endif}
  Result := List[aIndex];
end;

function TFastObjectList<T>.First: T;
begin
  {$ifdef paranoid} CheckIndex(0); {$endif}
  Result := GetItem(0);
end;

function TFastObjectList<T>.Last: T;
begin
  {$ifdef paranoid} CheckIndex(0); {$endif}
  Result := GetItem(Count - 1);
end;

function TFastObjectList<T>.FirstOrDefault: T;
begin
  if Count > 0
  then Result := GetItem(0)
  else Result := nil;
end;

function TFastObjectList<T>.LastOrDefault: T;
begin
  if Count > 0
  then Result := GetItem(Count - 1)
  else Result := nil;
end;

function TFastObjectList<T>.HasItems: Boolean;
begin
  Result := Count > 0;
end;

function TFastObjectList<T>.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TFastObjectList<T>.GetEnumerator: TEnumeratorForFastList<T>;
begin
  // we do not use a record constructor. this is faster
  Result.fIndex := -1;
  Result.fList := Self;
end;

function TFastObjectList<T>.ToArray: TArray<T>;
begin
  SetLength(Result, Count);
  if Count > 0 then
    System.Move(List[0], Result[0], Count * SizeOf(T));
end;

function Scale(const i: Integer): Integer;
begin
  Result := Round(i * CurrentDisplay.DpiScale);
end;

function Scale(const r: TRect): TRect;
begin
  Result.Left := Scale(r.Left);
  Result.Top := Scale(r.Top);
  Result.Right := Scale(r.Right);
  Result.Bottom := Scale(r.Bottom);
end;

{ TIntHelper }

function TIntHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

function TIntHelper.ToThousandString: string;
begin
  Result := FloatToStrF(Self / 1, ffNumber, 15, 0);
end;

function TIntHelper.Scale: Integer;
begin
  Result := Base.utils.Scale(Self);
end;

{ TStringArrayHelper }

function TStringArrayHelper.IndexOf(const s: string; caseSensitive: Boolean = True): Integer;
begin
  var ix: Integer := 0;
  if caseSensitive then begin
    for var tmp: string in Self do begin
      if s = tmp then
        Exit(ix);
      Inc(ix);
    end;
  end
  else begin
    for var tmp: string in Self do begin
      if SameText(s, tmp) then
        Exit(ix);
      Inc(ix);
    end;
  end;
  Result := -1;
end;

function TStringArrayHelper.Contains(const s: string; caseSensitive: Boolean = True): Boolean;
begin
  Result := IndexOf(s, caseSensitive) >= 0;
end;

function TStringArrayHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

procedure TStringArrayHelper.Sort;
begin
  TArray.Sort<string>(Self);
end;

{ TLemmixMemoryMappedRecord }

procedure TLemmixMemoryMappedRecord.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TLemmixMemoryMappedRecord.GetAsString: string;
begin
  SetLength(Result, Length(ParamChars));
  for var i := 1 to 255 do begin
    var C := ParamChars[i];
    if C = #0 then begin
      SetLength(Result, i - 1);
      Break;
    end;
    Result[i] := ParamChars[i];
  end;
end;

procedure TLemmixMemoryMappedRecord.SetAsString(const s: string);
begin
  FillChar(ParamChars, SizeOf(ParamChars), 0);
  for var i := 1 to Copy(s, 1, 255).Length do
    ParamChars[i] := s[i];
end;

procedure TryNotifyRunningLemmixInstance;
var
  param: string;
  mapHandle: THandle;
  tempRecord: PLemmixMemoryMappedRecord;
begin
  param := ParamStr(1);
  if param.Trim.IsEmpty then  begin
    // no messagebox because of 2x taskbar
    Exit;
  end;

  mapHandle := 0;

  try
    // get handle to memory mapped file
    mapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(_UniqueMemoryMappedFileName));
    if mapHandle = 0 then
      Exit;

    tempRecord := MapViewOfFile(mapHandle, FILE_MAP_ALL_ACCESS, 0, 0, SizeOf(TLemmixMemoryMappedRecord));

    if (tempRecord = nil) or (tempRecord^.ApplicationHandle = 0) then
      Exit;

    tempRecord^.SetAsString(ParamStr(1));

    // send new param to existing lemmix app. the message is handled in Form.Main and will reload with this param
    PostMessage(tempRecord^.ApplicationHandle, LM_RESTART, 0, 0);

   finally
     if mapHandle <> 0 then
       CloseHandle(mapHandle);
   end;
end;

function InitializeLemmix: Boolean;
begin
  // check already running
  var existingMutex := OpenMutex(SYNCHRONIZE, False, PChar(_UniqueName));
  if existingMutex <> 0 then begin
    CloseHandle(existingMutex);
    TryNotifyRunningLemmixInstance;
    Exit(False);
  end;

  // if not running create mutex
  _Mutex := CreateMutex(nil, True, PChar(_UniqueName));
  if GetLastError = ERROR_ALREADY_EXISTS then begin
    _Mutex := 0;
    Dlg('Lemmix failed to create mutex');
    Exit(False);
  end;

  // if ok create memory mapped file
  _MemoryMappedFileHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, SizeOf(TLemmixMemoryMappedRecord), PChar(_UniqueMemoryMappedFileName));
  _LemmixMemoryMappedRecord := MapViewOfFile(_MemoryMappedFileHandle, FILE_MAP_ALL_ACCESS, 0, 0, SizeOf(TLemmixMemoryMappedRecord));
  _LemmixMemoryMappedRecord^.Clear;

  Result := True;
end;

procedure FinalizeLemmix;
begin
  if _MemoryMappedFileHandle <> 0 then
    CloseHandle(_MemoryMappedFileHandle);
  if _Mutex <> 0 then
    CloseHandle(_Mutex);
end;

initialization
  CurrentDisplay.fDpiScale := 1.0;
  QueryPerformanceFrequency(_Freq);
finalization
  FinalizeLemmix;
  CurrentDisplay.FreeBitmap;
end.


// faster string starts with
(*
function MyStartsWith(const SearchText, Text: string): Boolean;
var
  Index, SearchTextLen: Integer;
begin
  SearchTextLen := Length(SearchText);
  if SearchTextLen>Length(Text) then
  begin
    Result := False;
    Exit;
  end;
  for Index := 1 to SearchTextLen do
    if Text[Index]<>SearchText[Index] then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;
*)

