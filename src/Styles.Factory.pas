unit Styles.Factory;
{$include lem_directives.inc}

interface

uses
  System.Generics.Collections,
  Base.Utils,
  Prog.Types, Prog.Base,
  Styles.Base, Styles.Dos, Styles.User;

type
  TStyleFactory = class sealed
  private
    class var StylePool: TObjectDictionary<string, TStyle>;
  public
    class procedure Init; static;
    class procedure Done; static;
    class function CreateStyle(preventPooling: Boolean): TStyle; static;
  end;


implementation

{ TStyleFactory }

class procedure TStyleFactory.Init;
begin
  StylePool := TObjectDictionary<string, TStyle>.Create([doOwnsValues]);
end;

class procedure TStyleFactory.Done;
begin
  for var s: TStyle in StylePool.Values do
    s.IsPooledByFactory := False;
  StylePool.Free;
end;

class function TStyleFactory.CreateStyle(preventPooling: Boolean): TStyle;
// preventPooling is needed when we are - for example - building the cache
begin
  Result := nil;

  if not preventPooling and StylePool.TryGetValue(Consts.StyleName, Result) then
    Exit;

  case Consts.StyleDef of
    TStyleDef.Orig: Result := TDosOrigStyle.Create(Consts.StyleName);
    TStyleDef.Ohno: Result := TDosOhNoStyle.Create(Consts.StyleName);
    TStyleDef.H93 : Result := TDosH93Style.Create(Consts.StyleName);
    TStyleDef.H94 : Result := TDosH94Style.Create(Consts.StyleName);
    TStyleDef.X91 : Result := TDosX91Style.Create(Consts.StyleName);
    TStyleDef.X92 : Result := TDosX92Style.Create(Consts.StyleName);
    TStyleDef.User: Result := TUserStyle.Create(Consts.StyleName);
  else
    Throw('Unhandled style definition', 'CreateStyle');
  end;

  if not preventPooling then begin
    StylePool.Add(Consts.StyleName, Result);
    Result.IsPooledByFactory := True;
  end;
end;


end.

