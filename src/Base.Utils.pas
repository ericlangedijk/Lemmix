unit Base.Utils;

{$include lem_directives.inc}

interface

// Base.Utils must be without any lemmix program specific references.
// Some globals here, som baseclasses as well, and some basic stuff.

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.Classes, System.SysUtils, System.TypInfo, System.IniFiles, System.Math, System.Contnrs, System.Generics.Collections,
  System.Rtti, System.IOUtils,
  Vcl.Forms, Vcl.Imaging.PngImage, Vcl.Graphics, Vcl.Controls,
  GR32, GR32_LowLevel;

function InitializeLemmix: Boolean;

procedure DoThrow(const msg: string; const proc: string = '');

type
  TObjectHelper = class helper for TObject
  public
    class procedure Throw(const msg: string; const method: string = '');
  end;

const
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


// file stuff
function ForceDir(const aFileName: string): Boolean;
function GetFileSize(const aFilename: string): Int64;

// string stuff
function LeadZeroStr(const i, zeros: Integer): string; inline;
function ThousandStr(Int: Integer): string; overload;
function ThousandStr(Int: Cardinal): string; overload;
function CutLeft(const S: string; aLen: integer): string;
function CutRight(const S: string; aLen: integer): string;
function ReplaceFileExt(const aFileName, aNewExt: string): string;
function StripInvalidFileChars(const S: string; removeDots: Boolean = True; removeDoubleSpaces: Boolean = True; trimAccess: Boolean = True): string;

// conversion string stuff
function RectStr(const R: TRect): string;
function PointStr(const P: TPoint): string;
function BytesToHex(const bytes: TBytes): string;
function YesNo(const b: Boolean): string; inline;

// rectangle stuff
function ZeroTopLeftRect(const r: TRect): TRect; inline;
procedure RectMove(var r: TRect; x, y: Integer); inline;

// int stuff
procedure Restrict(var i: Integer; aMin, aMax: Integer); overload; inline;
procedure Restrict(var s: Single; const aMin, aMax: Single); overload; inline;
function Percentage(Max, N: integer): integer;

// debug
procedure Dlg(const s: string);
{$ifdef debug}
procedure Log(const v: TValue); overload;
procedure Log(const v1, v2: TValue); overload;
procedure ClearLog;
{$endif debug}

// version
function GetAppVersionString(out major, minor, release, build: Integer): string;

// timing stuff
function QueryTimer: Int64; overload; inline;
function MSBetween(const T1, T2: Int64): Int64; inline;

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
    function Scale: Integer; inline;
  end;

  // just useless stub interfaces to prevent circular references
  IDosForm = interface
    ['{24535447-B742-4EB2-B688-825A1AD69349}']
  end;

  IMainForm = interface
  ['{24535447-B742-4EB2-B688-825A1AD69349}']
    procedure SwitchToNextMonitor;
    //procedure Interrupt;
  end;

  TDisplayInfo = record
  private
    fMainForm: IMainForm;
    fCurrentForm: IDosForm;
    fDpi: Integer; // the dpi of the monitor
    fMonitorIndex: Integer; // the current monitor index
    fBoundsRect: TRect; // the boundsrect of the current display
    fDpiScale: Single; // scale relative = Dpi/96
    procedure SetMonitorIndex(aValue: Integer);
  public
    property MainForm: IMainForm read fMainForm write fMainForm;
    property CurrentForm: IDosForm read fCurrentForm write fCurrentForm;
    property MonitorIndex: Integer read fMonitorIndex write SetMonitorIndex;

    property Dpi: Integer read fDpi;
    property DpiScale: Single read fDpiScale;
    property BoundsRect: TRect read fBoundsRect;
  end;


type
  Enum = class sealed
  public
    // enum to string of set to string
    class function AsString<T>(const aValue: T): string; static; inline;
  end;

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
  CurrentDisplay: TDisplayInfo; // info on monitor and currently active form

implementation

procedure DoThrow(const msg: string; const proc: string = '');
// generic invalid operation exception for procedues
begin
  var txt: string := msg;
  if proc <> '' then
    txt := txt + sLineBreak + 'Proc: ' + proc;
  raise EInvalidOperation.Create(txt) at ReturnAddress;
end;

class procedure TObjectHelper.Throw(const msg: string; const method: string = '');
// generic object helper for invalid operation exception from method of whichever object
var
  classString: string;
begin
  classString := ClassName;
  if classString.StartsWith('T') then
    classString := Copy(classString, 2, Length(ClassString));
  var txt: string := msg + sLineBreak + sLineBreak + 'Error from: ' + classString + sLineBreak + 'Unit: ' + Unitname;
  if method <> '' then
    txt := txt + sLineBreak + 'Method: ' + method;
  raise EInvalidOperation.Create(txt) at ReturnAddress;
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

procedure RectMove(var r: TRect; x, y: Integer);
begin
  r.Offset(x, y);
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

function ReplaceFileExt(const aFileName, aNewExt: string): string;
var
  Ext, NewExt: string;
begin
  Ext := ExtractFileExt(aFileName);
  NewExt := aNewExt;
  if (NewExt <> '') and not NewExt.StartsWith('.') then
    NewExt := '.' + NewExt;
  if Ext <> '' then
    Result := Copy(aFileName, 1, Length(aFileName) - Length(Ext)) + NewExt
  else
    Result := aFilename + NewExt;
end;

function StripInvalidFileChars(const S: string; removeDots: Boolean = True; removeDoubleSpaces: Boolean = True; trimAccess: Boolean = True): string;
begin
  Result := '';
  for var C: Char in S do
    if TPath.IsValidFileNameChar(C) then
      Result := Result + C;

  if removeDoubleSpaces then
    while Pos('  ', Result) > 0 do
      Result := Result.Replace('  ', '');

  if removeDots then
    while Pos('.', Result) > 0 do
      Result := Result.Replace('.', '');

  if trimAccess then
    Result := Result.Trim;
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

function YesNo(const b: Boolean): string; inline;
begin
  if b then Result := 'yes' else Result := 'no';
end;

procedure Dlg(const s: string);
begin
  MessageBox(0, Pchar(s), 'Lemmix', 0); // todo: this messes with focus controls. not restored ok (test out in Finder screen)
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

  Result := Format('%d.%d.%d', [major, minor, release]);
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

{ TDisplayInfo }

procedure TDisplayInfo.SetMonitorIndex(aValue: Integer);
var
  monitor: TMonitor;
begin
  if (aValue < 0) or (aValue >= Screen.MonitorCount) then
    aValue := 0;
  fMonitorIndex := aValue;
  monitor := Screen.Monitors[fMonitorIndex];
  fDpi := monitor.PixelsPerInch;
  fDpiScale := fDpi / 96;
  fBoundsRect := monitor.BoundsRect;
end;

{ Enum }

class function Enum.AsString<T>(const aValue: T): string;
// alleen aanroepen voor enum of set met typeinfo
begin
  Result := TValue.From<T>(aValue).ToString;
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
  if param.Trim.IsEmpty then   begin
    //MessageBox(GetDesktopWindow, 'Lemmix is already running', 0, MB_SYSTEMMODAL);// todo: damn this shows a second taskbar item. we ignore this for now
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
    Dlg('Failed to create mutex');
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
end.


