unit Game.Logger;

{$ifndef debug}
Only in debugmode
{$endif}

{$include lem_directives.inc}

interface

uses
  System.Types, System.SysUtils, System.Rtti,
  Base.Utils,
  Dos.Consts, Game;

type
  TGameLogger = class
  private
    const
      LemmingActionNames: array[TLemmingAction] of string = (
        'NONE',
        'WALKING',
        'JUMPING',
        'DIGGING',
        'CLIMBING',
        'DROWNING',
        'HOISTING',
        'BUILDING',
        'BASHING',
        'MINING',
        'FALLING',
        'FLOATING',
        'SPLATTING',
        'EXITING',
        'VAPORIZING',
        'BLOCKING',
        'SHRUGGING',
        'OHNOING',
        'EXPLODING'
      );

      CommandNames: array[TSkillPanelButton] of string = (
        'NONE',
        'RR_SLOWDOWN',
        'RR_SPEEDUP',
        'SELECT_CLIMBER',
        'SELECT_UMBRELLA',
        'SELECT_EXPLODE',
        'SELECT_BLOCKER',
        'SELECT_BUILDER',
        'SELECT_BASHER',
        'SELECT_MINER',
        'SELECT_DIGGER',
        'PAUSE',
        'NUKE'
      );
  class var
    _Txt: ^TextFile;
  private
    class procedure Add(const s: string); static;
    class function BoolStr(b: Boolean): string; static;
    class function IterationStr(e: TLemmingGame): string; static;
    class function LemmingIndexStr(L: TLemming): string; static;
  public
    class destructor Destroy;
    class procedure Log(const s: string); static;
    class procedure LogLemming(e: TLemmingGame; L: TLemming); static;
    class procedure LogRemove(e: TLemmingGame; Lemming: TLemming); static;
    class procedure LogTransition(e: TLemmingGame; Lemming: TLemming; oldAction, newAction: TLemmingAction; turn: Boolean); static;
    class procedure LogTurn(e: TLemmingGame; Lemming: TLemming); static;
    class procedure LogCommand(e: TLemmingGame; command: TSkillPanelButton; activate: Boolean); static;
    class procedure LogAssignment(e: TLemmingGame; Lemming1, Lemming2: TLemming; skill: TLemmingAction; const cursorpos: TPoint); static;
    class procedure LogTrapTriggering(e: TLemmingGame; trap: TInteractiveObjectInfo); static;
  end;

implementation

{ TGameLogger }

class destructor TGameLogger.Destroy;
begin
  if Assigned(_Txt) then begin
    CloseFile(_Txt^);
    Dispose(_Txt);
    _Txt := nil;
  end;
end;


class procedure TGameLogger.Add(const s: string);
begin
  if _Txt = nil then begin
    New(_Txt);
    AssignFile(_Txt^, 'game.log');
    Rewrite(_Txt^);
  end;
  WriteLn(_Txt^, s);
  Flush(_Txt^);
end;

class procedure TGameLogger.Log(const s: string);
begin
  Add(s);
end;

class function TGameLogger.IterationStr(e: TLemmingGame): string;
begin
  Result := e.CurrentIteration.ToString.PadLeft(6, '0');
end;

class procedure TGameLogger.LogLemming(e: TLemmingGame; L: TLemming);
begin
  Add(
    e.CurrentIteration.tostring.PadLeft(6, '0') +
    ' ix=' + l.ListIndex.ToString.PadLeft(3) +
    ' x=' +  l.XPos.ToString +
    ' y=' + L.YPos.ToString +
    ' act=' + LemmingActionNames[L.Action] +
    ' fr=' + l.Frame.ToString +
    ' bricks=' + l.NumberOfBricksLeft.tostring +
    ' dir=' + l.xdelta.tostring);
end;

class function TGameLogger.LemmingIndexStr(L: TLemming): string;
begin
  if L <> nil then
    Result := L.ListIndex.ToString
  else
    Result := 'nil';
end;

class function TGameLogger.BoolStr(b: Boolean): string;
begin
  if b then Result := 'true' else Result := 'false';
end;

class procedure TGameLogger.LogRemove(e: TLemmingGame; Lemming: TLemming);
begin
  var s: string :=
    IterationStr(e) +
    ' REMOVE: lemming ' + LemmingIndexStr(Lemming) + ' with action ' + LemmingActionNames[Lemming.Action];
  Add(s);
end;

class procedure TGameLogger.LogTransition(e: TLemmingGame; Lemming: TLemming; oldAction, newAction: TLemmingAction; turn: Boolean);
begin
  var s: string :=
    IterationStr(e) +
    ' TRANSITION: lemming ' + LemmingIndexStr(Lemming) + ' from ' + LemmingActionNames[oldAction] + ' to ' + LemmingActionNames[newAction] + ' turn = ' + BoolStr(turn);
  Add(s);
end;

class procedure TGameLogger.LogTurn(e: TLemmingGame; Lemming: TLemming);
begin
  var s: string :=
    IterationStr(e) +
    ' TURN: lemming ' + LemmingIndexStr(Lemming);
  Add(s);
end;

class procedure TGameLogger.LogCommand(e: TLemmingGame; command: TSkillPanelButton; activate: Boolean);
begin

  var s: string := Iterationstr(e) + ' COMMAND ' + CommandNames[command] + ' (' + BoolStr(activate) + ')';
  Add(s);
end;

class procedure TGameLogger.LogAssignment(e: TLemmingGame; Lemming1, Lemming2: TLemming; skill: TLemmingAction; const cursorpos: TPoint);
begin
  var s: string := IterationStr(e) + ' ASSIGN ' + LemmingActionNames[skill] + ' Lemming1 ' + LemmingIndexStr(Lemming1) + ' Lemming2 ' + LemmingIndexStr(Lemming2)
    + ' cursor ' + cursorpos.x.tostring + ':' + cursorpos.y.tostring;
  if e.Replaying then s := 'R:' + s else s := 'A:' + s;
  Add(s);
end;

class procedure TGameLogger.LogTrapTriggering(e: TLemmingGame; trap: TInteractiveObjectInfo);
begin
  var s: string := Iterationstr(e) + ' TRAPTRIGGERING ' + 'frame =' + trap.CurrentFrame.ToString + ' framecount=' + trap.MetaObj.AnimationFrameCount.ToString;
  Add(s);
end;

end.

