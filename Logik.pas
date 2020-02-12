unit Logik;

interface

uses
  ExtCtrls, OffLists;

const
  GameFieldColumns = 10;
  GameFieldRows = 20;

type
  TGameColor = (CoBlue,CoRed,CoYellow,CoGreen,CoPurple,CoPink,CoOrange);
  TDirection = (DiLeft,DiRight);
  TFallTimerInterval = (TINormal,TISpeed);
  TLost = procedure of Object;

  TFieldCoordinates = record
    x: Byte;
    y: Byte;
  end;

  TField = class;
  TBlock = class;
  TSpawn = class;

  TGame = class(TObject)
  private
    ClearedRows: Integer;
    NormalIntervall: Integer;
    SpeedIntervall: Integer;
    DropRows: Boolean;
    FallTimer: TTimer;
    MoveTimer: TTimer;
    DisapearTimer: TTimer;
    FLost: TLost;
    FLevel: Integer;
    procedure OnFallTimer(Sender: TObject);
    procedure OnMoveTimerLeft(Sender: TObject);
    procedure OnMoveTimerRight(Sender: TObject);
    procedure OnDisapearTimer(Sender: TObject);
    procedure SetLevel(const Value: Integer);
  public
    Points: Integer;
    GameField: array[0..GameFieldColumns-1,0..GameFieldRows-1] of TField;
    NewBlockField: array[0..5,0..3] of TField;
    ActiveBlock: TBlock;
    Spawn: TSpawn;
    constructor Create;
    destructor Destroy; override;
    procedure SetFallTimerInterval(const aInterval: TFallTimerInterval);
    procedure BeforeSpawn;
    procedure StartMove(const Direction: TDirection);
    procedure StopMove;
    procedure Pause;
    procedure StopPause;
    property Lost: TLost read FLost write FLost;
    property Level: Integer read FLevel write SetLevel;
  end;

  TField = class(TObject)
  public
    Visible: Boolean;
    Disapearing: Boolean;
    Color: TGameColor;
    constructor Create(const aColor: TGameColor);
  end;

  TBlock = class(TObject)
  private
    Color: TGameColor;
    TurnNumber: Byte;
    FieldCoordinates: array[0..3] of TFieldCoordinates;
    function UnknowBlock(const x,y: Integer): Boolean;
    procedure ChangeCoordinates(const x1,y1,x2,y2,x3,y3,x4,y4: Integer);
  public
    Disapering: Boolean;
    procedure Turn;
    function Down: Boolean;
    procedure MoveLeft;
    procedure MoveRight;
    procedure FullDown;
    constructor Create(const aColor: TGameColor);
  end;

  TSpawn = class(TObject)
  private
    BlockList: TIDList;
    NextBlockColor: TGameColor;
    procedure FillList;
    procedure GenerateNewBlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SpawnNewBlock;
  end;

var
  Game: TGame;

implementation

{ TGame }

constructor TGame.Create;

  procedure ClearFields;
  var
    x,y: Integer;
  begin
    for x:=0 to GameFieldColumns-1 do begin
      for y:=0 to GameFieldColumns-1 do begin
        GameField[x,y]:=nil;
      end;
    end;
    for x:=0 to 4 do begin
      for y:=0 to 3 do begin
        NewBlockField[x,y]:=nil;
      end;
    end;
  end;

  procedure CrateFallTimer;
  begin
    FallTimer:=TTimer.Create(nil);
    NormalIntervall:=500;
    SpeedIntervall:=50;
    FallTimer.Interval:=NormalIntervall;
    FallTimer.OnTimer:=OnFallTimer;
    FallTimer.Enabled:=True;
  end;

  procedure CreateMoveTimer;
  begin
    MoveTimer:=TTimer.Create(nil);
    MoveTimer.Interval:=400;
    MoveTimer.Enabled:=False;
  end;

  procedure CrateDisapearTimer;
  begin
    DisapearTimer:=TTimer.Create(nil);
    DisapearTimer.Interval:=70;
    DisapearTimer.OnTimer:=OnDisapearTimer;
    DisapearTimer.Enabled:=False;
  end;

begin
  ClearFields;
  CrateFallTimer;
  CreateMoveTimer;
  CrateDisapearTimer;
  ClearedRows:= 0;
  Level:=1;
  Points:=0;
end;

destructor TGame.Destroy;

  procedure ClearFields;
  var
    x,y: Integer;
  begin
    for x:=0 to GameFieldColumns-1 do begin
      for y:=0 to GameFieldColumns-1 do begin
        GameField[x][y]:= nil;
      end;
    end;
    for x:=0 to 4 do begin
      for y:=0 to 3 do begin
        NewBlockField[x][y]:= nil;
      end;
    end;
  end;

begin
  ClearFields;
  FallTimer.Free;
  MoveTimer.Free;
  DisapearTimer.Free;
  ActiveBlock.Free;
  Spawn.Free;
  inherited;
end;

procedure TGame.OnFallTimer(Sender: TObject);
begin
  if not DisapearTimer.Enabled then
    ActiveBlock.Down
end;

procedure TGame.SetFallTimerInterval(const aInterval: TFallTimerInterval);
begin
  if aInterval = TINormal then
    FallTimer.Interval:=NormalIntervall
  else
    FallTimer.Interval:=SpeedIntervall;
end;

procedure TGame.SetLevel(const Value: Integer);
begin
  FLevel:=Value;
  NormalIntervall:=500-(FLevel*25);
  FallTimer.Interval:=NormalIntervall;
  SpeedIntervall:=50-(FLevel*1);
end;

procedure TGame.Pause;
begin
  FallTimer.Enabled:=False;
end;

procedure TGame.StartMove(const Direction: TDirection);
begin
  case Direction of
    DiLeft: begin
      MoveTimer.OnTimer:=OnMoveTimerLeft;
      ActiveBlock.MoveLeft;
    end;
    DiRight: begin
      MoveTimer.OnTimer:=OnMoveTimerRight;
      ActiveBlock.MoveRight;
    end;
  end;
  MoveTimer.Enabled:=True;
end;

procedure TGame.StopMove;
begin
  MoveTimer.Enabled:=False;
  MoveTimer.Interval:=400;
  MoveTimer.OnTimer:=nil;
end;

procedure TGame.OnMoveTimerLeft(Sender: TObject);
begin
  MoveTimer.Interval:=150;
  ActiveBlock.MoveLeft;
end;

procedure TGame.OnMoveTimerRight(Sender: TObject);
begin
  MoveTimer.Interval:=150;
  ActiveBlock.MoveRight;
end;

procedure TGame.StopPause;
begin
  FallTimer.Enabled:=True;
end;

procedure TGame.BeforeSpawn;

  function GetClearableRows(var Rows: TIDList): Boolean;
  var
    x,y: Integer;
  begin
    Rows.Clear;
    for y:=0 to 19 do begin
      for x:=0 to 9 do begin
        if GameField[x,y] = nil then
          Break;
        if x = 9 then
          Rows.Add(y);
      end;
    end;
    Result:=Rows.Count<>0;
  end;

var
  Rows: TIDList;
  I: Integer;
  x: Integer;
begin
  Rows:=TIDList.Create;
  try
    if GetClearableRows(Rows) then begin
      for I:=0 to Rows.Count - 1 do
        for x:=0 to 9 do
          GameField[x,Rows[I]].Disapearing:=True;
      Inc(ClearedRows,Rows.Count);
      case Rows.Count of
        1: Inc(Points,40*Level);
        2: Inc(Points,100*Level);
        3: Inc(Points,300*Level);
        4: Inc(Points,1200*Level);
      end;
      if (ClearedRows>=10) and (Level<=15) then begin
        Level:= Level+1;
        Dec(ClearedRows,10);
      end;
      ActiveBlock.Disapering:=True;
      DisapearTimer.Tag:=0;
      DisapearTimer.Enabled:=True;
    end else
      Spawn.SpawnNewBlock;
  finally
    Rows.Free;
  end;
end;

procedure TGame.OnDisapearTimer(Sender: TObject);
var
  x: Integer;
  y: Integer;
  I: Integer;
begin
  DisapearTimer.Tag:=DisapearTimer.Tag+1;
  for x:=0 to 9 do begin
    for y:=0 to 19 do begin
      if (GameField[x,y]<>nil) and (GameField[x,y].Disapearing) then begin
        GameField[x,y].Visible:=(DisapearTimer.Tag mod 2) = 1;
        if DisapearTimer.Tag = 10 then begin
          for I:=y downto 1 do
            GameField[x,I]:=GameField[x,I-1];
          GameField[x,0]:=nil;
        end;
      end;
    end;
  end;
  if DisapearTimer.Tag = 10 then begin
    DisapearTimer.Enabled:=False;
    Spawn.SpawnNewBlock;
  end;
end;

{ TField }

constructor TField.Create(const aColor: TGameColor);
begin
  Color:=aColor;
  Visible:=True;
  Disapearing:=False;
end;

{ TBlock }

constructor TBlock.Create(const aColor: TGameColor);
begin
  Disapering:=False;
  TurnNumber:=1;
  Color:= aColor;
  case Color of
    CoBlue: begin
      if (Game.GameField[3,0] <> nil) or (Game.GameField[4,0] <> nil) or
         (Game.GameField[5,0] <> nil) or (Game.GameField[6,0] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,0]:= TField.Create(CoBlue);
      Game.GameField[4,0]:= TField.Create(CoBlue);
      Game.GameField[5,0]:= TField.Create(CoBlue);
      Game.GameField[6,0]:= TField.Create(CoBlue);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=0;
      FieldCoordinates[1].x:=4; FieldCoordinates[1].y:=0;
      FieldCoordinates[2].x:=5; FieldCoordinates[2].y:=0;
      FieldCoordinates[3].x:=6; FieldCoordinates[3].y:=0;
    end;
    CoRed: begin
      if (Game.GameField[3,0] <> nil) or (Game.GameField[4,0] <> nil) or
         (Game.GameField[4,1] <> nil) or (Game.GameField[5,0] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,0]:= TField.Create(CoRed);
      Game.GameField[4,0]:= TField.Create(CoRed);
      Game.GameField[4,1]:= TField.Create(CoRed);
      Game.GameField[5,0]:= TField.Create(CoRed);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=0;
      FieldCoordinates[1].x:=4; FieldCoordinates[1].y:=0;
      FieldCoordinates[2].x:=4; FieldCoordinates[2].y:=1;
      FieldCoordinates[3].x:=5; FieldCoordinates[3].y:=0;
    end;
    CoYellow: begin
      if (Game.GameField[3,0] <> nil) or (Game.GameField[4,0] <> nil) or
         (Game.GameField[4,1] <> nil) or (Game.GameField[5,1] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,0]:= TField.Create(CoYellow);
      Game.GameField[4,0]:= TField.Create(CoYellow);
      Game.GameField[4,1]:= TField.Create(CoYellow);
      Game.GameField[5,1]:= TField.Create(CoYellow);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=0;
      FieldCoordinates[1].x:=4; FieldCoordinates[1].y:=0;
      FieldCoordinates[2].x:=4; FieldCoordinates[2].y:=1;
      FieldCoordinates[3].x:=5; FieldCoordinates[3].y:=1;
    end;
    CoGreen: begin
      if (Game.GameField[3,1] <> nil) or (Game.GameField[4,0] <> nil) or
         (Game.GameField[4,1] <> nil) or (Game.GameField[5,0] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,1]:= TField.Create(CoGreen);
      Game.GameField[4,0]:= TField.Create(CoGreen);
      Game.GameField[4,1]:= TField.Create(CoGreen);
      Game.GameField[5,0]:= TField.Create(CoGreen);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=1;
      FieldCoordinates[1].x:=4; FieldCoordinates[1].y:=0;
      FieldCoordinates[2].x:=4; FieldCoordinates[2].y:=1;
      FieldCoordinates[3].x:=5; FieldCoordinates[3].y:=0;
    end;
    CoPurple: begin
      if (Game.GameField[3,0] <> nil) or (Game.GameField[4,0] <> nil) or
         (Game.GameField[5,0] <> nil) or (Game.GameField[3,1] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,0]:= TField.Create(CoPurple);
      Game.GameField[4,0]:= TField.Create(CoPurple);
      Game.GameField[5,0]:= TField.Create(CoPurple);
      Game.GameField[3,1]:= TField.Create(CoPurple);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=0;
      FieldCoordinates[1].x:=4; FieldCoordinates[1].y:=0;
      FieldCoordinates[2].x:=5; FieldCoordinates[2].y:=0;
      FieldCoordinates[3].x:=3; FieldCoordinates[3].y:=1;
    end;
    CoPink: begin
      if (Game.GameField[3,0] <> nil) or (Game.GameField[4,0] <> nil) or
         (Game.GameField[5,0] <> nil) or (Game.GameField[5,1] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,0]:= TField.Create(CoPink);
      Game.GameField[4,0]:= TField.Create(CoPink);
      Game.GameField[5,0]:= TField.Create(CoPink);
      Game.GameField[5,1]:= TField.Create(CoPink);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=0;
      FieldCoordinates[1].x:=4; FieldCoordinates[1].y:=0;
      FieldCoordinates[2].x:=5; FieldCoordinates[2].y:=0;
      FieldCoordinates[3].x:=5; FieldCoordinates[3].y:=1;
    end;
    CoOrange: begin
      if (Game.GameField[3,0] <> nil) or (Game.GameField[3,1] <> nil) or
         (Game.GameField[4,0] <> nil) or (Game.GameField[4,1] <> nil) then begin
        Game.Lost;
        Exit;
      end;
      Game.GameField[3,0]:= TField.Create(CoOrange);
      Game.GameField[3,1]:= TField.Create(CoOrange);
      Game.GameField[4,0]:= TField.Create(CoOrange);
      Game.GameField[4,1]:= TField.Create(CoOrange);
      FieldCoordinates[0].x:=3; FieldCoordinates[0].y:=0;
      FieldCoordinates[1].x:=3; FieldCoordinates[1].y:=1;
      FieldCoordinates[2].x:=4; FieldCoordinates[2].y:=0;
      FieldCoordinates[3].x:=4; FieldCoordinates[3].y:=1;
    end;
  end;
end;

function TBlock.Down: Boolean;
var
  I: Integer;
begin
  Result:=False;
  if Disapering then
    Exit;
  for I:=0 to 3 do begin
    if UnknowBlock(FieldCoordinates[I].x,FieldCoordinates[I].y+1) then begin
      Game.BeforeSpawn;
      Result:=True;
      Exit;
    end;
  end;
  ChangeCoordinates(FieldCoordinates[0].x,FieldCoordinates[0].y+1,
                    FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                    FieldCoordinates[2].x,FieldCoordinates[2].y+1,
                    FieldCoordinates[3].x,FieldCoordinates[3].y+1);
end;

procedure TBlock.MoveLeft;
var
  I: Integer;
begin
  if Disapering then
    Exit;
  for I:=0 to 3 do begin
    if UnknowBlock(FieldCoordinates[I].x-1,FieldCoordinates[I].y) then begin
      Exit;
    end;
  end;
  ChangeCoordinates(FieldCoordinates[0].x-1,FieldCoordinates[0].y,
                    FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                    FieldCoordinates[2].x-1,FieldCoordinates[2].y,
                    FieldCoordinates[3].x-1,FieldCoordinates[3].y);
end;

procedure TBlock.MoveRight;
var
  I: Integer;
begin
  if Disapering then
    Exit;
  for I:=0 to 3 do begin
    if UnknowBlock(FieldCoordinates[I].x+1,FieldCoordinates[I].y) then begin
      Exit;
    end;
  end;
  ChangeCoordinates(FieldCoordinates[0].x+1,FieldCoordinates[0].y,
                    FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                    FieldCoordinates[2].x+1,FieldCoordinates[2].y,
                    FieldCoordinates[3].x+1,FieldCoordinates[3].y);
end;

procedure TBlock.Turn;
begin
  if Disapering then
    Exit;
  case Color of
    CoBlue:
      {$REGION 'CoBLue'}
      case TurnNumber of
        1: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+2)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+2);
          TurnNumber:=2;
        end;
        2: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+2,FieldCoordinates[1].y)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+2,FieldCoordinates[1].y);
          TurnNumber:=1;
        end;
      end;
      {$ENDREGION}
    CoRed:
      {$REGION 'CoRed'}
      case TurnNumber of
        1: begin
          if UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1);
          TurnNumber:=2;
        end;
        2: begin
          if UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y);
          TurnNumber:=3;
        end;
        3: begin
          if UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y-1);
          TurnNumber:=4;
        end;
        4: begin
          if UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y);
          TurnNumber:=1;
        end;
      end;
      {$ENDREGION}
    CoYellow:
      {$REGION 'CoYellow'}
      case TurnNumber of
        1: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y+1);
          TurnNumber:=2;
        end;
        2: begin
          if (UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y-1);
          TurnNumber:=3;
        end;
        3: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y)or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y-1);
          TurnNumber:=4;
        end;
        4: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y+1);
          TurnNumber:=1;
        end;
      end;
      {$ENDREGION}
    CoGreen:
      {$REGION 'CoGreen'}
      case TurnNumber of
        1: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1);
          TurnNumber:=2;
        end;
        2: begin
          if (UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x+1,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y);
          TurnNumber:=3;
        end;
        3: begin
          if (UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x+1,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y-1);
          TurnNumber:=4;
        end;
        4: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y);
          TurnNumber:=1;
        end;
      end;
      {$ENDREGION}
    CoPurple:
      {$REGION 'CoPurple'}
      case TurnNumber of
        1: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y-1);
          TurnNumber:=2;
        end;
        2: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y-1);
          TurnNumber:=3;
        end;
        3: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y+1);
          TurnNumber:=4;
        end;
        4: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y+1);
          TurnNumber:=1;
        end;
      end;
      {$ENDREGION}
    CoPink:
     {$REGION 'CoPink'}
      case TurnNumber of
        1: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y+1);
          TurnNumber:=2;
        end;
        2: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x-1,FieldCoordinates[1].y-1);
          TurnNumber:=3;
        end;
        3: begin
          if (UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y-1) or
              UnknowBlock(FieldCoordinates[1].x,FieldCoordinates[1].y+1) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y-1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x,FieldCoordinates[1].y-1,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y+1,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y-1);
          TurnNumber:=4;
        end;
        4: begin
          if (UnknowBlock(FieldCoordinates[1].x-1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y) or
              UnknowBlock(FieldCoordinates[1].x+1,FieldCoordinates[1].y+1)) then
            Exit;
          ChangeCoordinates(FieldCoordinates[1].x-1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y,
                            FieldCoordinates[1].x+1,FieldCoordinates[1].y+1);
          TurnNumber:=1;
        end;
      end;
      {$ENDREGION}
  end;
end;

procedure TBlock.FullDown;
begin
  repeat
  until (Down);
end;

function TBlock.UnknowBlock(const x, y: Integer): Boolean;

  function HitFieldCoordinates(const x,y: Integer; FieldCoordinates: TFieldCoordinates): Boolean;
  begin
    Result:=((x=FieldCoordinates.x)and(y=FieldCoordinates.y));
  end;

var
  I: Integer;
begin
  Result:=False;
  if (x < 0) or (x > 9) or (y < 0) or (y > 19) then begin
    Result:= True;
    Exit;
  end;
  for I:=0 to 3 do
    if HitFieldCoordinates(x,y,FieldCoordinates[I]) then
      Exit;
  if Game.GameField[x,y] <> nil then
    Result:=True;
end;

procedure TBlock.ChangeCoordinates(const x1, y1, x2, y2, x3, y3, x4, y4: Integer);
var
  I: Integer;
begin
  for I:=0 to 3 do
    Game.GameField[FieldCoordinates[I].x,FieldCoordinates[I].y]:=nil;
  Game.GameField[x1,y1]:= TField.Create(Color);
  Game.GameField[x2,y2]:= TField.Create(Color);
  Game.GameField[x3,y3]:= TField.Create(Color);
  Game.GameField[x4,y4]:= TField.Create(Color);
  FieldCoordinates[0].x:=x1; FieldCoordinates[0].y:=y1;
  FieldCoordinates[1].x:=x2; FieldCoordinates[1].y:=y2;
  FieldCoordinates[2].x:=x3; FieldCoordinates[2].y:=y3;
  FieldCoordinates[3].x:=x4; FieldCoordinates[3].y:=y4;
end;

{ TSpawn }

constructor TSpawn.Create;
begin
  BlockList:=TIDList.Create;
  FillList;
  GenerateNewBlock;
end;

destructor TSpawn.Destroy;
begin
  BlockList.Free;
  inherited;
end;

procedure TSpawn.SpawnNewBlock;
begin
  Game.ActiveBlock:=TBlock.Create(NextBlockColor);
  GenerateNewBlock;
end;

procedure TSpawn.FillList;
var
  I: Integer;
begin
  BlockList.Clear;
  for I:=0 to 6 do
    BlockList.Add(I);
end;

procedure TSpawn.GenerateNewBlock;

  procedure ClearNewBlockField;
  var
    x,y: Integer;
  begin
    for x:=0 to 5 do
      for y:=0 to 3 do
        Game.NewBlockField[x,y]:=nil;
  end;

  procedure SetBlockInNewBlock(const aColor: TGameColor);
  begin
    case aColor of
      CoBlue: begin
        Game.NewBlockField[1,1]:= TField.Create(CoBlue);
        Game.NewBlockField[2,1]:= TField.Create(CoBlue);
        Game.NewBlockField[3,1]:= TField.Create(CoBlue);
        Game.NewBlockField[4,1]:= TField.Create(CoBlue);
      end;
      CoRed: begin
        Game.NewBlockField[1,1]:= TField.Create(CoRed);
        Game.NewBlockField[2,1]:= TField.Create(CoRed);
        Game.NewBlockField[2,2]:= TField.Create(CoRed);
        Game.NewBlockField[3,1]:= TField.Create(CoRed);
      end;
      CoYellow: begin
        Game.NewBlockField[1,1]:= TField.Create(CoYellow);
        Game.NewBlockField[2,1]:= TField.Create(CoYellow);
        Game.NewBlockField[2,2]:= TField.Create(CoYellow);
        Game.NewBlockField[3,2]:= TField.Create(CoYellow);
      end;
      CoGreen: begin
        Game.NewBlockField[1,2]:= TField.Create(CoGreen);
        Game.NewBlockField[2,1]:= TField.Create(CoGreen);
        Game.NewBlockField[2,2]:= TField.Create(CoGreen);
        Game.NewBlockField[3,1]:= TField.Create(CoGreen);
      end;
      CoPurple: begin
        Game.NewBlockField[1,1]:= TField.Create(CoPurple);
        Game.NewBlockField[2,1]:= TField.Create(CoPurple);
        Game.NewBlockField[3,1]:= TField.Create(CoPurple);
        Game.NewBlockField[1,2]:= TField.Create(CoPurple);
      end;
      CoPink: begin
        Game.NewBlockField[1,1]:= TField.Create(CoPink);
        Game.NewBlockField[2,1]:= TField.Create(CoPink);
        Game.NewBlockField[3,1]:= TField.Create(CoPink);
        Game.NewBlockField[3,2]:= TField.Create(CoPink);
      end;
      CoOrange: begin
        Game.NewBlockField[2,1]:= TField.Create(CoOrange);
        Game.NewBlockField[2,2]:= TField.Create(CoOrange);
        Game.NewBlockField[3,1]:= TField.Create(CoOrange);
        Game.NewBlockField[3,2]:= TField.Create(CoOrange);
      end;
    end;
  end;

var
  RandomNumber: Integer;
begin
  if BlockList.Count = 0 then
    FillList;
  RandomNumber:=Random(BlockList.Count);
  NextBlockColor:=TGameColor(BlockList[RandomNumber]);
  ClearNewBlockField;
  SetBlockInNewBlock(NextBlockColor);
  BlockList.Delete(RandomNumber);
end;

end.
