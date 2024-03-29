unit ScoreBoard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, Contnrs;

type
  TPlayerScoreList = class;

  TFrmScoreBoard = class(TForm)
    Grid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  private
    PlayerScoreList: TPlayerScoreList;
    procedure FillPlayerSoreList;
    procedure FillGrid;
  public
    { Public-Deklarationen }
  end;

  TPlayerScore = class(TObject)
  public
    Name: string;
    Score: Integer;
    constructor Create(const aName: string; const aScore: Integer);
  end;

  TPlayerScoreList = class(TObjectList)
  private
    function Get(Index: Integer): TPlayerScore;
  public
    procedure Sort;
    function Add(aPlayerScore: TPlayerScore): Integer;
    property Items[Index: Integer]: TPlayerScore read Get; default;
  end;

var
  FrmScoreBoard: TFrmScoreBoard;

implementation

uses
  OffLists, StrUtils;

{$R *.dfm}

{ TFrmScoreBoard }

procedure TFrmScoreBoard.FormCreate(Sender: TObject);
begin
  FillPlayerSoreList;
  FillGrid;
end;

procedure TFrmScoreBoard.FormDestroy(Sender: TObject);
begin
  PlayerScoreList.Free;
end;

procedure TFrmScoreBoard.GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
begin
  Grid.Canvas.Font:=Grid.Font;
  Grid.Canvas.Brush.Color:= Grid.Color;
  if (ACol=0) or (ARow=0) then begin
    Grid.Canvas.Font.Color:=clwhite;
    Grid.Canvas.Brush.Color:=Grid.FixedColor;
  end;
  Grid.Canvas.TextRect(Rect,Rect.Left+(Rect.Right-Rect.Left-Grid.Canvas.TextWidth(Grid.Cells[ACol,ARow]))div 2,
                            Rect.Top+(Rect.Bottom-Rect.Top-Grid.Canvas.TextHeight(Grid.Cells[ACol,ARow]))div 2,
                            Grid.Cells[ACol, ARow]);
end;

procedure TFrmScoreBoard.FillPlayerSoreList;
var
  Text: TStringList;
  Split: TStringList;
  I: Integer;
begin
  PlayerScoreList:=TPlayerScoreList.Create;
  Text:=TStringList.Create;
  try
    Text.LoadFromFile(ExtractFilePath(Application.ExeName)+'Score.txt');
    Split:=TStringList.Create;
    Split.Delimiter:=':';
    try
      for I:=0 to Text.Count-1 do begin
        Split.DelimitedText:=Text[I];
        PlayerScoreList.Add(TPlayerScore.Create(Split[0],StrToInt(Split[1])));
      end;
    finally
      Split.Free;
    end;
  finally
    Text.Free;
  end;
  PlayerScoreList.Sort;
end;

procedure TFrmScoreBoard.FillGrid;
var
  I: Integer;
begin
  Grid.RowCount:=PlayerScoreList.Count+1;
  Grid.Cells[0,0]:='Place';
  Grid.Cells[1,0]:='Player';
  Grid.Cells[2,0]:='Points';
  for I:=0 to PlayerScoreList.Count - 1 do begin
    Grid.Cells[0,I+1]:=IntToStr(I+1);
    Grid.Cells[1,I+1]:=PlayerScoreList[I].Name;
    Grid.Cells[2,I+1]:=IntToStr(PlayerScoreList[I].Score);
  end;
end;

{ TPlayerScore }

constructor TPlayerScore.Create(const aName: string; const aScore: Integer);
begin
  Name:=aName;
  Score:=aScore;
end;

{ TPlayerScoreList }

function TPlayerScoreList.Add(aPlayerScore: TPlayerScore): Integer;
begin
  Result:=inherited Add(aPlayerScore);
end;

function TPlayerScoreList.Get(Index: Integer): TPlayerScore;
begin
  Result:=TPlayerScore(inherited Get(Index));
end;

procedure TPlayerScoreList.Sort;

  procedure DeleteDoubleNames;
  var
    I: Integer;
    Players: string;
  begin
    i:=0;
    Players:='';
    while (I < Count) do begin
      if AnsiContainsStr(Players,Items[I].Name) then
        Delete(I)
      else begin
        Players:=Players+Items[I].Name+'|';
        Inc(I);
      end;
    end;
  end;

var
  I,J: Integer;
  InsertionValue: TPlayerScore;
begin
  for I:=0 to Count-1 do begin
    InsertionValue:=TPlayerScore.Create(Items[I].Name,Items[I].Score);
    J:=I;
    while ((J>0)and(Items[J-1].Score < InsertionValue.Score)) do begin
      SetItem(J,TPlayerScore.Create(Items[J-1].Name,Items[J-1].Score));
      Dec(J);
    end;
    SetItem(J,InsertionValue);
  end;
  DeleteDoubleNames;
end;

end.
