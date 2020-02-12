unit TetrisAnzeige;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Logik, ExtCtrls, ImgList, StdCtrls, ActnList;

type
  TBacktoStartScreen = procedure of object;

  TPaint = class;

  TFrmTetrisAnzeige = class(TForm)
    ImageList: TImageList;
    FPS: TTimer;
    BtnRestart: TButton;
    BtnStartScreen: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FPSTimer(Sender: TObject);
    procedure BtnRestartClick(Sender: TObject);
    procedure BtnStartScreenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    StartLevel: Integer;
    Player: string;
    Pausing: Boolean;
    Finished: Boolean;
    Paint: TPaint;
    FBacktoStartScreen: TBacktoStartScreen;
    procedure OnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure StopPause;
    procedure Lost;
  public
    procedure SetLevel(const Level: Integer);
    procedure SetPlayer(const aPlayer: string);
    procedure Pause;
    property BacktoStartScreen: TBacktoStartScreen read FBacktoStartScreen write FBacktoStartScreen;
  end;

  TPaint = class(TObject)
  private
    grayBlock: TGraphic;
    blueBlock: TGraphic;
    redBlock: TGraphic;
    yellowBlock: TGraphic;
    greenBlock: TGraphic;
    purpleBlock: TGraphic;
    pinkBlock: TGraphic;
    orangeBlock: TGraphic;
    Img1: TImage;
    Img2: TImage;
    Img1AtFront: Boolean;
    FFrmTetrisAnzeige: TFrmTetrisAnzeige;
    procedure Paint(Image: TImage);
  public
    procedure Initialize(const ImageList: TImageList);
    procedure Repaint;
    property FrmTetrisAnzeige: TFrmTetrisAnzeige read FFrmTetrisAnzeige;
  end;

var
  FrmTetrisAnzeige: TFrmTetrisAnzeige;

implementation

{$R *.dfm}

{ TFrmTetrisAnzeige }

procedure TFrmTetrisAnzeige.FormCreate(Sender: TObject);
begin
  Game:=TGame.Create;
  Game.Spawn:=TSpawn.Create;
  Game.Spawn.SpawnNewBlock;
  Game.Lost:= Lost;
  Application.OnMessage:=OnMessage;
  Paint:=TPaint.Create;
  Paint.FFrmTetrisAnzeige:=Self;
  Paint.Initialize(ImageList);
end;

procedure TFrmTetrisAnzeige.FormDestroy(Sender: TObject);
begin
  Game.Free;
  Paint.Free;
end;

procedure TFrmTetrisAnzeige.SetLevel(const Level: Integer);
begin
  StartLevel:=Level;
  Game.Level:=Level;
end;

procedure TFrmTetrisAnzeige.SetPlayer(const aPlayer: string);
begin
  Player:=aPlayer;
end;

procedure TFrmTetrisAnzeige.FPSTimer(Sender: TObject);
begin
  Paint.Repaint;
end;

procedure TFrmTetrisAnzeige.BtnRestartClick(Sender: TObject);
begin
  Game.Free;
  Game:=TGame.Create;
  Game.Spawn:=TSpawn.Create;
  Game.Spawn.SpawnNewBlock;
  Game.Lost:=Lost;
  Game.Level:=StartLevel;
  Finished:=False;
  Pausing:=False;
end;

procedure TFrmTetrisAnzeige.BtnStartScreenClick(Sender: TObject);
begin
  BacktoStartScreen;
end;

procedure TFrmTetrisAnzeige.OnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Visible then begin
    if (Msg.message = Messages.WM_KEYDOWN) and (not Pausing) then begin
      case Msg.wParam of
        VK_LEFT: Game.StartMove(DiLeft);
        VK_RIGHT: Game.StartMove(DiRight);
        VK_DOWN: Game.SetFallTimerInterval(TISpeed);
      end;
    end;
    if (Msg.message = Messages.WM_KEYUP) and (not Pausing) then begin
      case Msg.wParam of
        VK_UP: Game.ActiveBlock.Turn;
        VK_LEFT: Game.StopMove;
        VK_RIGHT: Game.StopMove;
        VK_DOWN: Game.SetFallTimerInterval(TINormal);
        VK_SPACE: Game.ActiveBlock.FullDown;
      end;
    end;
    if (Msg.message = Messages.WM_LBUTTONUP) and (not Finished) then
      if not Pausing then
        Pause
      else
        StopPause;  
  end;
end;

procedure TFrmTetrisAnzeige.Pause;
begin
  Game.Pause;
  Pausing:=True;
end;

procedure TFrmTetrisAnzeige.StopPause;
begin
  Game.StopPause;
  Pausing:=False;
end;

procedure TFrmTetrisAnzeige.Lost;
var
  FileName: string;
  Text: TStringList;
begin
  FileName:=ExtractFilePath(Application.ExeName)+'Score.txt';
  Pause;
  Finished:=True;
  if Player <> '' then begin
    Text:=TStringList.Create;
    try
      Text.LoadFromFile(FileName);
      Text.Add(Player+':'+IntToStr(Game.Points));
      FileSetAttr(FileName,0);
      Text.SaveToFile(FileName);
      FileSetAttr(FileName,0);
      FileSetAttr(ExtractFilePath(Application.ExeName)+'Score.txt', faHidden+faReadOnly+faSysFile);
    finally
      Text.Free;
    end;
  end;
end;

{ TPaint }

procedure TPaint.Initialize(const ImageList: TImageList);
var
  BMP: TBitmap;
begin
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(0,BMP);
  blueBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(1,BMP);
  redBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(2,BMP);
  yellowBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(3,BMP);
  greenBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(4,BMP);
  purpleBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(5,BMP);
  pinkBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(6,BMP);
  orangeBlock:=TGraphic(BMP);
  BMP:=TBitmap.Create;
  ImageList.GetBitmap(7,BMP);
  grayBlock:=TGraphic(BMP);
  Img1:=TImage.Create(FrmTetrisAnzeige);
  Img1.Parent:=FrmTetrisAnzeige;
  Img1.Align:=alClient;
  Img2:=TImage.Create(FrmTetrisAnzeige);
  Img2.Parent:=FrmTetrisAnzeige;
  Img2.Align:=alClient;
  Img1.BringToFront;
  Img1AtFront:=True;
end;

procedure TPaint.Paint(Image: TImage);

  function GetGameFieldGraphic(x,y: Integer): TGraphic;
  begin
    if (Game.GameField[x,y] = nil) or not (Game.GameField[x,y].Visible) then begin
      Result:=nil;
      Exit;
    end;
    case Game.GameField[x,y].Color of
      CoBlue: Result:=blueBlock;
      CoRed: Result:=redBlock;
      CoYellow: Result:=yellowBlock;
      CoGreen: Result:=greenBlock;
      CoPurple: Result:=purpleBlock;
      CoPink: Result:=pinkBlock;
      CoOrange: Result:=orangeBlock;
    end;
  end;

  function GetNewBlockGraphic(x,y: Integer): TGraphic;
  begin
    if Game.NewBlockField[x,y] = nil then begin
      Result:=nil;
      Exit;
    end;
    case Game.NewBlockField[x,y].Color of
      CoBlue: Result:=blueBlock;
      CoRed: Result:=redBlock;
      CoYellow: Result:=yellowBlock;
      CoGreen: Result:=greenBlock;
      CoPurple: Result:=purpleBlock;
      CoPink: Result:=pinkBlock;
      CoOrange: Result:=orangeBlock;
    end;
  end;

var
  x: Integer;
  y: Integer;
begin
  Image.Canvas.Brush.Color:=clBlack;
  Image.Canvas.Rectangle(0,0,840,840);
  for x:=0 to 20 do begin
    for y:=0 to 20 do begin
      if (((x=0)or(x>=11))or(y=20)) and
         (not((x>=13)and(x<=18)and(y>=2)and(y<=5))) and
         (not((x>=13)and(x<=18)and(y>=8)and(y<=9))) then begin
        Image.Canvas.Draw(40*x,40*y,grayBlock);
      end;
    end;
  end;
  for x:=0 to 9 do
    for y:=0 to 19 do
      Image.Canvas.Draw(40*(x+1),40*y,GetGameFieldGraphic(x,y));
  for x:=0 to 5 do
    for y:=0 to 3 do
      Image.Canvas.Draw(40*(x+13),40*(y+2),GetNewBlockGraphic(x,y));
  Image.Canvas.Font.Color:= clWhite;
  Image.Canvas.Font.size:=15;
  Image.Canvas.TextOut(530,326,'Level:');
  Image.Canvas.TextOut(736-Image.Canvas.TextWidth(IntToStr(Game.Level)),326,IntToStr(Game.Level));
  Image.Canvas.TextOut(530,366,'Punkte:');
  Image.Canvas.TextOut(736-Image.Canvas.TextWidth(IntToStr(Game.Points)),366,IntToStr(Game.Points));
  if FrmTetrisAnzeige.Pausing then begin
    Image.Canvas.Font.size:=35;
    Image.Canvas.Brush.Style:=bsClear;
    if FrmTetrisAnzeige.Finished then
      Image.Canvas.TextOut(150,370,'Verloren')
    else
      Image.Canvas.TextOut(160,370,'Pause');
    FrmTetrisAnzeige.BtnRestart.Visible:=True;
    FrmTetrisAnzeige.BtnStartScreen.Visible:=True;
  end else begin
    FrmTetrisAnzeige.BtnRestart.Visible:=False;
    FrmTetrisAnzeige.BtnStartScreen.Visible:=False;
  end;
end;

procedure TPaint.Repaint;
begin
  if Img1AtFront then begin
    Paint(Img2);
    Img2.BringToFront;
    Img1AtFront:=False;
  end else begin
    Paint(Img1);
    Img1.BringToFront;
    Img1AtFront:=True;
  end;
end;

end.
