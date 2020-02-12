unit ScoreBoard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids;

type
  TFrmScoreBoard = class(TForm)
    Grid: TStringGrid;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FrmScoreBoard: TFrmScoreBoard;

implementation

{$R *.dfm}

end.
