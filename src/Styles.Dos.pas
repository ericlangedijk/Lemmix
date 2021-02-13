unit Styles.Dos;

{$include lem_directives.inc}

interface

uses
  System.Classes, System.SysUtils,
  Base.Utils,
  Prog.Base, Prog.Data,
  Dos.Consts,
  Styles.Base;

//  this unit contains the classes for the 6 original dos styles

type
  TDosOrigStyle = class sealed(TStyle)
  protected
    function GetLevelSystemClass: TLevelSystemClass; override;
    function GetMechanics: TMechanics; override;
  end;

  TDosOrigLevelSystem = class(TLevelSystem)
  strict private
    type
      TDosOrigLevelOrderTable = array[1..4, 1..30] of Byte;  // fun..mayhem, 1..30 encoded indices (note: the levels are 1-based)
    const
      SectionTable: TDosOrigLevelOrderTable = (
        (
           147, 155, 157, 149, 151, 153, 159,  14,  22,  54,
            70,  16,  29,  32,  38,  42,  48,  72,  84, 104,
           138,  23,  68,  96,  98, 116,  78, 100, 108, 134
        ),
        (
             1,  30,  36,  50,  52,  56,  58,  80, 102, 120,
           128, 130, 136,   5, 148, 152, 154, 156, 160,   7,
            11,  13,  15,  17,  19,  21,  25,  27,  33,  31
        ),
        (
           37, 39, 41, 43, 45, 47, 49, 51, 53, 55,
           57, 59, 61, 63,  3, 65, 67, 69, 71, 73,
           75, 77, 79, 81, 83, 85, 87, 89, 35,111
        ),
        (
            91,  93,  95,  97,  99, 101, 103, 105, 107, 109,
           112, 113, 115, 117, 119, 121, 123, 125, 127, 150,
           129,   9, 131, 133, 135, 137, 139, 141, 143, 145
        )
      );
    procedure GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
  protected
    procedure DoInitializeLevelSystem; override;
  public
    // all 6 built in dos styles use the same filename pattern
  end;


  TDosOhNoStyle = class sealed(TStyle)
  protected
    function GetLevelSystemClass: TLevelSystemClass; override;
    function GetMechanics: TMechanics; override;
  end;

  TDosOhNoLevelSystem = class(TLevelSystem)
  private
    type
      TDosOhNoLevelOrderTable = array[1..5, 1..20] of Byte;  {tame..havoc, 1..20} // raw order
    const
    {-------------------------------------------------------------------------------
        My own little system for OhNo: div 10 = file, mod 10 is index.
        So 117 means file 11, index 7.
        How did I find out this ??? I think I just compared and compared.
        Ohno original does about the same trick based on div 8 I think.
      -------------------------------------------------------------------------------}
      OhNoTable: TDosOhNoLevelOrderTable = (
        // tame 1..8     : 10.0, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6, 10.7
        // tame 9..16    : 11.0, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7
        // tame 17..20   : 12.0, 12.1, 12.2, 12.3 (missing in windows)
        (
           100, 101, 102, 103, 104, 105, 106, 107,
           110, 111, 112, 113, 114, 115, 116, 117,
           120, 121, 122, 123
        ),

        // crazy 1..8    : 0.1, 1.0, 1.4, 2.0, 2.1, 3.0, 3.1, 3.5 (repaired typo crazy 1: 0.0 to the correct 0.1)
        // crazy 9..16   : 5.1, 5.4, 5.7, 6.7, 0.4, 1.5, 1.6, 2.6
        // crazy 17..20  : 3.4, 3.7, 4.3, 6,4
        (
             1,  10,  14,  20,  21,  30,  31, 35,
            51,  54,  57,  67,   4,  15,  16, 26,
            34,  37,  43,  64
        ),

        // wild 1..8     : 9.2, 7.0, 7.1, 7.2, 7.3, 7.5, 8.6, 0.7
        // wild 9..16    : 2.2, 2.5, 3.3, 3.6, 4.0, 4.2, 4.4, 5.0 (repaired typo wild 15: 4.5 to the correct 4.4)
        // wild 17..20   : 6.3, 6.5, 6.6, 4.7
        (
          92,70,71,72,73,75,86,7,
          22,25,33,36,40,42,44,50,
          63,65,66,47
        ),

        // wicked 1..8   : 9.6, 4.6, 9.0, 9.1, 0.5, 9.4, 6.1, 0.6
        // wicked 9..16  : 1.2, 8.7, 4.1, 5.5, 6.0, 6.2, 7.7, 8.1,
        // wicked 17..20 : 9.7, 9.3, 8.0, 7.6
        (
          96,46,90,91,5,94,61,6,
          12,87,41,55,60,62,77,81,
          97,93,80,76
        ),

        // havoc 1..8    : 7.4, 5.3, 3.2, 2.7, 2.4, 2.3, 5.2, 1.7
        // havoc 9..16   : 1.1, 0.3, 0.2, 0.0, 4.5, 8.5, 1.3, 8.2
        // havoc 17..20  : 8.3, 5.6, 8.4, 9.5
        (
          74,53,32,27,24,23,52,17,
          11,3,2,0,45,85,13,82,
          83,56,84,95
        )
      );

  protected
    procedure GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
    procedure DoInitializeLevelSystem; override;
  end;

  TDosH94Style = class(TStyle)
  protected
    function GetLevelSystemClass: TLevelSystemClass; override;
    function GetMechanics: TMechanics; override;
  end;

  TDosH94LevelSystem = class(TLevelSystem)
  private
    procedure GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
  protected
    procedure DoInitializeLevelSystem; override;
  end;

  TDosX91Style = class(TStyle)
  protected
    function GetLevelSystemClass: TLevelSystemClass; override;
    function GetMechanics: TMechanics; override;
  end;

  TDosX91LevelSystem = class(TLevelSystem)
  private
    class procedure GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer); static;
  protected
    procedure DoInitializeLevelSystem; override;
  end;

  TDosX92Style = class(TStyle)
  protected
    function GetLevelSystemClass: TLevelSystemClass; override;
    function GetMechanics: TMechanics; override;
  end;

  TDosX92LevelSystem = class(TLevelSystem)
  private
    class procedure GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer); static;
  protected
    procedure DoInitializeLevelSystem; override;
  end;


implementation

{ TDosOrigStyle }

function TDosOrigStyle.GetLevelSystemClass: TLevelSystemClass;
begin
  Result := TDosOrigLevelSystem;
end;

function TDosOrigStyle.GetMechanics: TMechanics;
begin
  Result := DOSORIG_MECHANICS;
end;

{ TDosOrigLevelSystem }

procedure TDosOrigLevelSystem.GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
{-------------------------------------------------------------------------------
  I think the numbers in the table are subtracted by 1 before being used in the manner Mike described.
  For example, for Fun 1 the number is 147.
  Divide by 2 (dropping lowest bit first) and you get 73.  "Just Dig" is the 2nd
  level in set level009.dat.  Using 0-based counting, 73 would indeed be the correct number.
  On the other hand, for Fun 8 the number is 14.  Divide by 2 and you get 7.
  Yet "Turn Around Young Lemmings!" is the 7th
  level in set level000.dat.  The correct number should be 6 if 0-based counting is used.
  This inconsistency goes away if all the numbers are subtracted by 1 first.  For example, the 147 becomes 146.  Divide by 2
  and you still get 73.  But for Fun 9, subtract by 1 and you get 13.  Divide by 2 and now you get 6, the correct number when
  counting from 0.  Alternatively, you can add 1 to all numbers for 1-based counting, so that the 147 becomes 148 and 73
  becomes 74.  But either way, the parity (odd/even) changes when you add/subtract 1, and after that, Mike's description
  regarding the bottom bit works correctly.
-------------------------------------------------------------------------------}
var
  B: Byte;
  Fx: Integer;
begin
  B := SectionTable[aSection + 1, aLevel + 1];
  Dec(B);
  IsOddTable := Odd(B);
  B := B div 2;
  Fx := (B) div 8;
  aFileName := 'LEVEL' + Fx.ToString.PadLeft(3, '0') + '.DAT';
  aFileIndex := B mod 8;
  aOddIndex := -1;
  if IsOddTable then
    aOddIndex := B;
end;

procedure TDosOrigLevelSystem.DoInitializeLevelSystem;
const
  sectionNames: array[0..3] of string = ('Fun', 'Tricky', 'Taxing', 'Mayhem');
var
  section: TSection;
  level: TLevelLoadingInformation;
  levelFileName: string;
  levelFileIndex: Integer;
  isOdd: Boolean;
  oddIndex: Integer;
  iMusic: Integer;
begin
  fOddTableFileName := 'oddtable.dat';
  iMusic := 1;
  for var iSection := 0 to 3 do begin
    section := TSection.Create(Self);
    section.SectionName := sectionNames[iSection];
    for var iLevel: Integer := 0 to 29 do begin
      level := TLevelLoadingInformation.Create(section);
      GetEntry(section.SectionIndex, level.LevelIndex, {out} levelFileName, {out} levelFileIndex, {out} isOdd, {out} oddIndex);
      level.SourceFileName := levelFileName;
      level.SectionIndexInSourceFile := levelFileIndex;
      level.UseOddTable := isOdd;
      level.OddTableIndex := oddIndex;
      // tracks 01..21
      level.MusicFileName := 'Track_' + LeadZeroStr(iMusic, 2) + '.mod';
      Inc(iMusic);
      if iMusic > 21 then
        iMusic := 1;
    end;
  end;
end;

{ TDosOhNoStyle }

function TDosOhNoStyle.GetLevelSystemClass: TLevelSystemClass;
begin
  Result := TDosOhNoLevelSystem;
end;

function TDosOhNoStyle.GetMechanics: TMechanics;
begin
  Result := DOSOHNO_MECHANICS;
end;

{ TDosOhNoLevelSystem }

procedure TDosOhNoLevelSystem.GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
var
  H, Sec, Lev: Integer;
begin
  H := OhnoTable[aSection + 1, aLevel + 1];
  Sec := H div 10;
  Lev := H mod 10;
  aFilename := 'Dlvel' + LeadZeroStr(Sec, 3) + '.dat';
  aFileIndex := Lev;

  IsOddTable := False;
  aOddIndex := -1;
end;

procedure TDosOhNoLevelSystem.DoInitializeLevelSystem;
const
  sectionNames: array[0..4] of string = ('Tame','Crazy','Wild','Wicked','Havoc');
var
  section: TSection;
  level: TLevelLoadingInformation;
  levelFileName: string;
  levelFileIndex: Integer;
  isOdd: Boolean;
  oddIndex: Integer;
  iMusic: Integer;
begin
  fOddTableFileName := string.Empty;
  iMusic := 1;
  for var iSection := 0 to 4 do begin
    section := TSection.Create(Self);
    section.SectionName := sectionNames[iSection];
    for var iLevel: Integer := 0 to 19 do begin
      level := TLevelLoadingInformation.Create(section);
      GetEntry(section.SectionIndex, level.LevelIndex, {out} levelFileName, {out} levelFileIndex, {out} isOdd, {out} oddIndex);
      level.SourceFileName := levelFileName;
      level.SectionIndexInSourceFile := levelFileIndex;
      level.UseOddTable := isOdd;
      level.OddTableIndex := oddIndex;
      // tracks 01..06
      level.MusicFileName := 'Track_' + LeadZeroStr(iMusic, 2) + '.mod';
      Inc(iMusic);
      if iMusic > 6 then
        iMusic := 1;
    end;
  end;
end;

{ TDosH94Style }

function TDosH94Style.GetLevelSystemClass: TLevelSystemClass;
begin
  Result := TDosH94LevelSystem;
end;

function TDosH94Style.GetMechanics: TMechanics;
begin
  Result := DOSOHNO_MECHANICS;
end;

{ TDosH94LevelSystem }

procedure TDosH94LevelSystem.GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
// I know this can be a oneliner, but this shows better which level is where
begin

  IsOddTable := False;
  aOddIndex := -1;

  Inc(aSection);
  Inc(aLevel);

  case aSection of
    1:
      case aLevel of
        1..8:
          begin
            aFileName := 'LEVEL000.DAT';
            aFileIndex := aLevel - 1;
          end;
        9..16 :
          begin
            aFileName := 'LEVEL001.DAT';
            aFileIndex := aLevel - 9;
          end;
      end;
    2:
      case aLevel of
        1..8  :
          begin
            aFileName := 'LEVEL002.DAT';
            aFileIndex := aLevel - 1;
          end;
        9..16 :
          begin
            aFileName := 'LEVEL003.DAT';
            aFileIndex := aLevel - 9;
          end;
      end;
    3:
      case aLevel of
        1..8  :
          begin
            aFileName := 'LEVEL004.DAT';
            aFileIndex := aLevel - 1;
          end;
        9..16 :
          begin
            aFileName := 'LEVEL005.DAT';
            aFileIndex := aLevel - 9;
          end;
      end;
    4:
      case aLevel of
        1..8  :
          begin
            aFileName := 'LEVEL006.DAT';
            aFileIndex := aLevel - 1;
          end;
        9..16 :
          begin
            aFileName := 'LEVEL007.DAT';
            aFileIndex := aLevel - 9;
          end;
      end;
  end;

end;

procedure TDosH94LevelSystem.DoInitializeLevelSystem;
// 2 sections with each 16 levels
const
  sectionNames: array[0..3] of string = ('Frost ''94','Hail ''94', 'Flurry ''93', 'Blitz ''93');
var
  section: TSection;
  level: TLevelLoadingInformation;
  levelFileName: string;
  levelFileIndex: Integer;
  isOdd: Boolean;
  oddIndex: Integer;
  iMusic: Integer;
begin
  fOddTableFileName := string.Empty;
  iMusic := 1;
  for var iSection := 0 to 3 do begin
    section := TSection.Create(Self);
    section.SectionName := sectionNames[iSection];
    for var iLevel: Integer := 0 to 15 do begin
      level := TLevelLoadingInformation.Create(section);
      GetEntry(section.SectionIndex, level.LevelIndex, {out} levelFileName, {out} levelFileIndex, {out} isOdd, {out} oddIndex);
      level.SourceFileName := levelFileName;
      level.SectionIndexInSourceFile := levelFileIndex;
      level.UseOddTable := isOdd;
      level.OddTableIndex := oddIndex;
      // tracks 01..02
      level.MusicFileName := 'Track_' + LeadZeroStr(iMusic, 2) + '.mod';
      Inc(iMusic);
      if iMusic > 2 then
        iMusic := 1;
    end;
  end;
end;



{ TDosX91Style }

function TDosX91Style.GetLevelSystemClass: TLevelSystemClass;
begin
  Result := TDosX91LevelSystem;
end;

function TDosX91Style.GetMechanics: TMechanics;
begin
  Result := DOSOHNO_MECHANICS;
end;

{ TDosX91LevelSystem }

class procedure TDosX91LevelSystem.GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
begin
  aFileName := 'LEVEL000.DAT';
  aFileIndex := aLevel;
  IsOddTable := False;
  aOddIndex := -1;
end;

procedure TDosX91LevelSystem.DoInitializeLevelSystem;
// one section with 4 levels
var
  section: TSection;
  level: TLevelLoadingInformation;
  levelFileName: string;
  levelFileIndex: Integer;
  isOdd: Boolean;
  oddIndex: Integer;
  iMusic: Integer;
begin
  fOddTableFileName := string.Empty;
  iMusic := 1;
  section := TSection.Create(Self);
  section.SectionName := 'XMas';
  for var iLevel: Integer := 0 to 3 do begin
    level := TLevelLoadingInformation.Create(section);
    GetEntry(section.SectionIndex, level.LevelIndex, {out} levelFileName, {out} levelFileIndex, {out} isOdd, {out} oddIndex);
    level.SourceFileName := levelFileName;
    level.SectionIndexInSourceFile := levelFileIndex;
    level.UseOddTable := isOdd;
    level.OddTableIndex := oddIndex;
    // not tracks yet
    level.MusicFileName := string.Empty;
    Inc(iMusic);
    if iMusic > 6 then
      iMusic := 1;
  end;

end;

{ TDosX92Style }

function TDosX92Style.GetLevelSystemClass: TLevelSystemClass;
begin
  Result := TDosX92LevelSystem;
end;

function TDosX92Style.GetMechanics: TMechanics;
begin
  Result := DOSOHNO_MECHANICS;
end;

{ TDosX92LevelSystem }

class procedure TDosX92LevelSystem.GetEntry(aSection, aLevel: Integer; out aFileName: string; out aFileIndex: Integer; out IsOddTable: Boolean; out aOddIndex: Integer);
begin
  aFileName := 'LEVEL000.DAT';
  aFileIndex := aLevel;
  IsOddTable := False;
  aOddIndex := -1;
end;

procedure TDosX92LevelSystem.DoInitializeLevelSystem;
// one section with 4 levels
var
  section: TSection;
  level: TLevelLoadingInformation;
  levelFileName: string;
  levelFileIndex: Integer;
  isOdd: Boolean;
  oddIndex: Integer;
  iMusic: Integer;
begin
  fOddTableFileName := string.Empty;
  iMusic := 1;
  section := TSection.Create(Self);
  section.SectionName := 'XMas';
  for var iLevel: Integer := 0 to 3 do begin
    level := TLevelLoadingInformation.Create(section);
    GetEntry(section.SectionIndex, level.LevelIndex, {out} levelFileName, {out} levelFileIndex, {out} isOdd, {out} oddIndex);
    level.SourceFileName := levelFileName;
    level.SectionIndexInSourceFile := levelFileIndex;
    level.UseOddTable := isOdd;
    level.OddTableIndex := oddIndex;
    // not tracks yet
    level.MusicFileName := string.Empty;
    Inc(iMusic);
    if iMusic > 6 then
      iMusic := 1;
  end;

end;


end.



