unit Level.Hash;

{$include lem_directives.inc}

interface

uses
  System.SysUtils, System.Hash,
  Base.Utils,
  Dos.Structures;

// Note: after release never change the hashing of levelcodes
type
  TLevelHasher = class sealed
  private
    type
      TShortHash = array[0..9] of Byte;
  public
    class function LongHash(const LVL: TLVLRec): TBytes; static; inline; // 16
    class function ShortHash(const LVL: TLVLRec): UInt64; static; // 8
    class function GetLevelCode(const hash: UInt64): string; overload; static; // 10
    class function GetLevelCode(const LVL: TLVLRec): string; overload; static; // 10
  end;

implementation

{ TLevelHasher }

class function TLevelHasher.LongHash(const LVL: TLVLRec): TBytes;
begin
  var H: THashMD5 := THashMD5.Create;
  H.Update(LVL, SizeOf(LVL));
  Result := H.HashAsBytes;
  {$if defined(paranoid)}
  if Length(Result) <> 16 then Throw('TLevelHasher.LongHash length error');
  {$ifend}
end;

class function TLevelHasher.ShortHash(const LVL: TLVLRec): UInt64;
var
  U: Int64Rec absolute Result;
begin
  var hash := LongHash(LVL);
  for var i := 0 to 7 do
    U.Bytes[i] := hash[i] xor hash[i + 8];
end;

class function TLevelHasher.GetLevelCode(const hash: UInt64): string;
const
  Vowels: array[0..4] of Char = ('A','E','I','O','U');
  NonVowels: array[0..19] of Char = ('B','C','D','F','G','H','J','K','L','M','N','P', 'R', 'S','T','V','W','X','Y','Z'); // Q is omitted on purpose
var
  U: Int64Rec absolute hash;
  Sum: Integer;
  Vowel: Boolean;
  b: Byte;
begin
  Sum := 0;
  for var i := 0 to 7 do
    Inc(Sum, Integer(U.Bytes[i]));
  Vowel := Odd(Sum);
  SetLength(Result, 10);
  for var i := 0 to 7 do begin
    if Vowel
    then Result[i + 1] := Vowels[U.Bytes[i] mod 5]
    else Result[i + 1] := NonVowels[U.Bytes[i] mod 20];
    Vowel := not Vowel;
  end;

  if Odd(Sum)
  then b := U.Bytes[0] xor U.Bytes[1]
  else b := U.Bytes[6] xor U.Bytes[7];

  if Vowel
  then Result[9] := Vowels[b mod 5]
  else Result[9] := NonVowels[b mod 20];
  Vowel := not Vowel;

  if Odd(Sum)
  then b := U.Bytes[2] xor U.Bytes[3]
  else b := U.Bytes[4] xor U.Bytes[5];

  if Vowel
  then Result[10] := Vowels[b mod 5]
  else Result[10] := NonVowels[b mod 20];
end;

class function TLevelHasher.GetLevelCode(const LVL: TLVLRec): string;
begin
  Result := GetLevelCode(ShortHash(LVL));
end;

end.

