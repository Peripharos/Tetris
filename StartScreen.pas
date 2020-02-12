unit StartScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TCreateGame = procedure(const Level: Integer; const Player: string) of object;

  TFrmStartScreen = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LblCreator: TLabel;
    LblLevelStatic: TLabel;
    LblLevel: TLabel;
    BtnLevel: TUpDown;
    BtnGo: TButton;
    EdtPlayer: TEdit;
    LblPlayer: TLabel;
    BtnScoreBoard: TButton;
    procedure BtnLevelChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure BtnGoClick(Sender: TObject);
    procedure BtnScoreBoardClick(Sender: TObject);
  private
    FCreateGame: TCreateGame;
  public
    property CreateGame: TCreateGame read FCreateGame write FCreateGame;
  end;

var
  FrmStartScreen: TFrmStartScreen;

implementation

uses
  ScoreBoard;

{$R *.dfm}

procedure TFrmStartScreen.BtnGoClick(Sender: TObject);
begin
  CreateGame(BtnLevel.Position,EdtPlayer.Text);
end;

procedure TFrmStartScreen.BtnLevelChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  if (NewValue>=1) and (NewValue<=15) then
    LblLevel.Caption:=IntToStr(NewValue);
end;

procedure TFrmStartScreen.BtnScoreBoardClick(Sender: TObject);
begin
  if not Assigned(FrmScoreBoard) then begin
    Application.CreateForm(TFrmScoreBoard,FrmScoreBoard);
    FrmScoreBoard.Left:=Application.MainForm.Left+Width+10;
    FrmScoreBoard.Top:=Application.MainForm.Top;
    FrmScoreBoard.Height:=Application.MainForm.Height;
  end else
    FrmScoreBoard.Visible:=not FrmScoreBoard.Visible;
end;

end.
