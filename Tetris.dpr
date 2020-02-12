program Tetris;

uses
  Forms,
  TetrisAnzeige in 'TetrisAnzeige.pas' {FrmTetrisAnzeige},
  Main in 'Main.pas' {FrmMain},
  StartScreen in 'StartScreen.pas' {FrmStartScreen},
  Logik in 'Logik.pas',
  ScoreBoard in 'ScoreBoard.pas' {FrmScoreBoard};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
//  Application.CreateForm(TFrmScoreBoard, FrmScoreBoard);
  Application.Run;
end.
