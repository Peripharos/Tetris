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
    procedure BtnLevelChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure BtnGoClick(Sender: TObject);
  private
    FCreateGame: TCreateGame;
  public
    property CreateGame: TCreateGame read FCreateGame write FCreateGame;
  end;

var
  FrmStartScreen: TFrmStartScreen;

implementation

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

end.
