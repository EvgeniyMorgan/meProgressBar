unit udemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  meProgressBarLoading, meProgressBarEx;

type

  { TForm1 }

  TForm1 = class(TForm)
    meProgressBarEx1: TmeProgressBarEx;
    meProgressBarEx2: TmeProgressBarEx;
    meProgressBarEx3: TmeProgressBarEx;
    meProgressBarLoading1: TmeProgressBarLoading;
    meProgressBarLoading2: TmeProgressBarLoading;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1Click(Sender: TObject);
    procedure TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  meProgressBarLoading1.Progress:=TrackBar1.Position;
  meProgressBarLoading2.Progress:=TrackBar1.Position;
  meProgressBarEx1.Progress:=TrackBar1.Position;
  meProgressBarEx2.Progress:=TrackBar1.Position;
  meProgressBarEx3.Progress:=TrackBar1.Position;
end;

procedure TForm1.TrackBar1Click(Sender: TObject);
begin

end;

procedure TForm1.TrackBar1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
  begin
    TrackBar1.Position:=50;
    TrackBar1Change(Self);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1Change(Self);
end;

end.

