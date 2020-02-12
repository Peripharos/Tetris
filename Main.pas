unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TetrisAnzeige, StartScreen;

type
  TFrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FrmStartScreen: TFrmStartScreen;
    FrmTetrisAnzeige: TFrmTetrisAnzeige;
    procedure CreateGame(const Level: Integer; const Player: string);
    procedure BacktoStartScreen;
    procedure AppDeactivate(Sender: TObject);
  public
    { Public-Deklarationen }
  end;

var
  FrmMain: TFrmMain;

implementation



{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);

  procedure CreateScoreFile;
  var
    f: TEXTfile;
  begin
    if not FileExists(ExtractFilePath(Application.ExeName)+'Score.txt') then begin
      assignfile(f, ExtractFilePath(Application.ExeName)+'Score.txt');
      rewrite(f);
      closefile(f);
      FileSetAttr(ExtractFilePath(Application.ExeName)+'Score.txt', faHidden+faReadOnly+faSysFile);
    end;
  end;

begin
  CreateScoreFile;
  Application.OnDeactivate:=AppDeactivate;
  FrmStartScreen:=TFrmStartScreen.Create(Self);
  FrmStartScreen.Parent:=self;
  FrmStartScreen.Align:=alClient;
  FrmStartScreen.Show;
  FrmStartScreen.CreateGame:=CreateGame;
end;

procedure TFrmMain.CreateGame(const Level: Integer; const Player: string);
begin
  FrmStartScreen.Free;
  Randomize;
  FrmTetrisAnzeige:=TFrmTetrisAnzeige.Create(Self);
  FrmTetrisAnzeige.SetLevel(Level);
  FrmTetrisAnzeige.SetPlayer(Player);
  FrmTetrisAnzeige.Parent:=self;
  FrmTetrisAnzeige.Align:=alClient;
  FrmTetrisAnzeige.Show;
  FrmTetrisAnzeige.BacktoStartScreen:=BacktoStartScreen;
end;

procedure TFrmMain.BacktoStartScreen;
begin
  FrmTetrisAnzeige.Free;
  FrmStartScreen:=TFrmStartScreen.Create(Self);
  FrmStartScreen.Parent:=self;
  FrmStartScreen.Align:=alClient;
  FrmStartScreen.Show;
  FrmStartScreen.CreateGame:=CreateGame;
end;

procedure TFrmMain.AppDeactivate(Sender: TObject);
begin
  if FrmTetrisAnzeige <> nil then
    FrmTetrisAnzeige.Pause;
end;

end.
